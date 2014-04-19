#include <err.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "erlcmd.h"

//#define DEBUG
#ifdef DEBUG
#define debug(...) fprintf(stderr, __VA_ARGS__)
#else
#define debug(...)
#endif

/*
 * GPIO handling definitions and prototypes
 */
enum gpio_state {
    GPIO_OUTPUT,
    GPIO_INPUT,
    GPIO_INPUT_WITH_INTERRUPTS
};

struct gpio {
    enum gpio_state state;
    int fd;
    int pin_number;
};

/**
 * @brief write a string to a sysfs file
 * @return returns 0 on failure, >0 on success
 */
int sysfs_write_file(const char *pathname, const char *value)
{
    int fd = open(pathname, O_WRONLY);
    if (fd < 0) {
	warn("Error opening %s", pathname);
	return 0;
    }

    size_t count = strlen(value);
    ssize_t written = write(fd, value, count);
    close(fd);

    if (written != count) {
	warn("Error writing '%s' to %s", value, pathname);
	return 0;
    }

    return written;
}

// GPIO functions

/**
 * @brief	Open and configure a GPIO
 *
 * @param	pin           The pin structure
 * @param	pin_number    The GPIO pin
 * @param       dir           Direction of pin (input or output)
 *
 * @return 	1 for success, -1 for failure
 */
int gpio_init(struct gpio *pin, unsigned int pin_number, enum gpio_state dir)
{
    /* Initialize the pin structure. */
    pin->state = dir;
    pin->fd = -1;
    pin->pin_number = pin_number;

    /* Construct the gpio control file paths */
    char direction_path[64];
    sprintf(direction_path, "/sys/class/gpio/gpio%d/direction", pin_number);

    char value_path[64];
    sprintf(value_path, "/sys/class/gpio/gpio%d/value", pin_number);

    /* Check if the gpio has been exported already. */
    if (access(value_path, F_OK) == -1) {
	/* Nope. Export it. */
	char pinstr[64];
	sprintf(pinstr, "%d", pin_number);
	if (!sysfs_write_file("/sys/class/gpio/export", pinstr))
	    return -1;
    }

    /* The direction file may not exist if the pin only works one way.
       It is ok if the direction file doesn't exist, but if it does
       exist, we must be able to write it.
    */
    if (access(direction_path, F_OK) != -1) {
	if (!sysfs_write_file(direction_path, dir == GPIO_OUTPUT ? "out" : "in"))
	    return -1;
    }

    pin->pin_number = pin_number;

    /* Open the value file for quick access later */
    pin->fd = open(value_path, pin->state == GPIO_OUTPUT ? O_RDWR : O_RDONLY);
    if (pin->fd < 0)
	return -1;

    return 1;
}

/**
 * @brief	Set pin with the value "0" or "1"
 *
 * @param	pin           The pin structure
 * @param       value         Value to set (0 or 1)
 *
 * @return 	1 for success, -1 for failure
 */
int gpio_write(struct gpio *pin, unsigned int val)
{
    if (pin->state != GPIO_OUTPUT)
	return -1;

    char buf = val ? '1' : '0';
    ssize_t amount_written = pwrite(pin->fd, &buf, sizeof(buf), 0);
    if (amount_written < sizeof(buf))
	err(EXIT_FAILURE, "pwrite");

    return 1;
}

/**
* @brief	Read the value of the pin
*
* @param	pin            The GPIO pin
*
* @return 	The pin value if success, -1 for failure
*/
int gpio_read(struct gpio *pin)
{
    char buf;
    ssize_t amount_read = pread(pin->fd, &buf, sizeof(buf), 0);
    if (amount_read < sizeof(buf))
	err(EXIT_FAILURE, "pread");

    return buf == '1' ? 1 : 0;
}

/**
 * Set isr as the interrupt service routine (ISR) for the pin. Mode
 * should be one of the strings "rising", "falling" or "both" to
 * indicate which edge(s) the ISR is to be triggered on. The function
 * isr is called whenever the edge specified occurs, receiving as
 * argument the number of the pin which triggered the interrupt.
 *
 * @param   pin	Pin number to attach interrupt to
 * @param   mode	Interrupt mode
 *
 * @return  Returns 1 on success.
 */
int gpio_set_int(struct gpio *pin, const char *mode)
{
    char path[64];
    sprintf(path, "/sys/class/gpio/gpio%d/edge", pin->pin_number);
    if (!sysfs_write_file(path, mode))
	return -1;

    pin->state = GPIO_INPUT_WITH_INTERRUPTS;
    return 1;
}

/**
 * Called after poll() returns when the GPIO sysfs file indicates
 * a status change.
 *
 * @param pin which pin to check
 */
void gpio_process(struct gpio *pin)
{
    int value = gpio_read(pin);

    ETERM *resp;
    if (value)
	resp = erl_format("{gpio_interrupt, rising}");
    else
	resp = erl_format("{gpio_interrupt, falling}");

    erlcmd_send(resp);
    erl_free_term(resp);
}

void gpio_handle_request(ETERM *emsg, void *cookie)
{
    struct gpio *pin = (struct gpio *) cookie;

    // Commands are of the form {Command, Arguments}:
    // { atom(), [term()] }

    ETERM *cmd = erl_element(1, emsg);
    ETERM *args = erl_element(2, emsg);
    if (cmd == NULL || args == NULL)
	errx(EXIT_FAILURE, "Expecting { cmd, args }");

    debug("gpio_request_handler: %s\n", ERL_ATOM_PTR(cmd));

    ETERM *resp;
    if (strcmp(ERL_ATOM_PTR(cmd), "read") == 0) {
	int value = gpio_read(pin);
	if (value !=-1)
	    resp = erl_format("~i", value);
	else
	    resp = erl_format("{error, gpio_read_failed}");
    } else if (strcmp(ERL_ATOM_PTR(cmd), "write") == 0) {
	ETERM *evalue = erl_hd(args);
	if (evalue == NULL)
	    errx(EXIT_FAILURE, "write: didn't get value to write");

	int value = ERL_INT_VALUE(evalue);

	if(gpio_write(pin, value))
	    resp = erl_format("ok");
	else
	    resp = erl_format("{error, gpio_write_failed}");
	erl_free_term(evalue);
    } else {
	resp = erl_format("error");
    }
    erlcmd_send(resp);

    erl_free_term(resp);
    erl_free_term(cmd);
    erl_free_term(args);
}

int main(int argc, char *argv[])
{
    if (argc != 3)
	errx(EXIT_FAILURE, "%s <pin#> <input|output>", argv[0]);

    int pin_number = strtol(argv[1], NULL, 0);
    enum gpio_state initial_state;
    if (strcmp(argv[2], "input") == 0)
	initial_state = GPIO_INPUT;
    else if (strcmp(argv[2], "output") == 0)
	initial_state = GPIO_OUTPUT;
    else
	errx(EXIT_FAILURE, "Specify 'input' or 'output'");

    struct gpio pin;
    gpio_init(&pin, pin_number, initial_state);

    struct erlcmd handler;
    erlcmd_init(&handler, gpio_handle_request, &pin);

    for (;;) {
	struct pollfd fdset[2];

	fdset[0].fd = STDIN_FILENO;
	fdset[0].events = POLLIN;
	fdset[0].revents = 0;

	fdset[1].fd = pin.fd;
	fdset[1].events = POLLPRI;
	fdset[1].revents = 0;

	/* Always fill out the fdset structure, but only have poll() monitor
	 * the sysfs file if interrupts are enabled.
	 */
	int rc = poll(fdset, pin.state == GPIO_INPUT_WITH_INTERRUPTS ? 2 : 1, -1);
	if (rc < 0) {
	    // Retry if EINTR
	    if (errno == EINTR)
		continue;

	    err(EXIT_FAILURE, "poll");
	}

	if (fdset[0].revents & (POLLIN | POLLHUP))
	    erlcmd_process(&handler);

	if (fdset[1].revents & POLLPRI)
	    gpio_process(&pin);
    }

    return 0;
}
