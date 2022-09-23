This is a folder of backup Drone_roject I started with on the ESP8266 that is labeled "Ai Cloud Inside" from KeyeStudio.com in 2pcs.
Also I was using a visual basic 6.0 app to communicate to the drone, and there are some advantages, and some disadvantages between
the ESP Basic firmware and using AT 100% duty cycle.  Either way, the ESP8266 ever since has been labaled "ESP-01" and I switched
from Micro's to Nano's and haven't been able to correctly communicate form the serial to the module so I went with ESP Basic.

The VisualBasic 6. code is a small mess I didn't deal with to tidy up, but these two projects did function,
VB with a USB to TTL to ESP, and Ardunio Micro to ESP.  Successfully, the project immediately reconnects with
a IP ack, or starts at reset going through the phases of for both until the IP ack, so startup is immediate
verses the 30 second wait time of the ESP8266 Basic firmware, least the resets in play, that only become
nessisary for debigging.  YOu may have to place the _DEF in the first mode AT command for both, and set to 3.
I toggle between dedicating specifically each's function and not flashing but I heard rumors that I don't
have test resulted in found reasoning, on reset and/or the time at which the mode retrieves or flashes.