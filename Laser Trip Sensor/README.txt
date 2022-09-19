This project is a bit different then the Security System project, but essentially same application.
The Security System allows for basic terminal readings and can handle a photo resistor for a laser,
but then requires the power for the laser too, thus four wires per trip.  My aim of this project is
to have only a pair of wires for any laser trip, and I was successful!  I'll put in my mount prints
too, as well as where I got my lasers, and mirrors.  Finally some pictures and the schematic layout.

Here is the Ardunio Nano that I am using;

https://www.amazon.com/gp/product/B07G99NNXL/ref=ppx_yo_dt_b_search_asin_title?ie=UTF8&psc=1

Here are the lasers I am using;

https://www.amazon.com/gp/product/B071FT9HSV/ref=ppx_yo_dt_b_search_asin_image?ie=UTF8&psc=1 

2N2222A Transistors that are needed;

https://www.amazon.com/gp/product/B09VC3WFBJ/ref=ppx_yo_dt_b_search_asin_image?ie=UTF8&psc=1

5539 Photoresistors that are needed;

https://www.amazon.com/20PCS-Light-Dependent-Resistor-Photoresistor/dp/B0B3VKL2XL/ref=sr_1_1?keywords=5539+photoresistor&qid=1663529225&sr=8-1

Here are the mirrors I am using;

https://www.amazon.com/Mirror-Traveling-Framing-Decoration-Projects/dp/B07HFHTLSR/ref=sr_1_1?crid=2GFYR8RDVI02V&keywords=3%2F4th+inch+round+mirrors&qid=1663526976&sprefix=3%2F4th+inch+round+mirrors%2Caps%2C88&sr=8-1

And here are the resistors I'm using;

https://www.amazon.com/California-JOS-Resistors-Tolerance-Electronics/dp/B0BDHLKQLD/ref=sr_1_18?crid=32JGI28G2NNEZ&keywords=1k+resistor&qid=1663529456&sprefix=1k+resistor%2Caps%2C99&sr=8-18

This is optional, but it is how I keep my Nano powered;

https://www.amazon.com/gp/product/B00GNIKFLW/ref=ppx_yo_dt_b_search_asin_image?ie=UTF8&psc=1


There are two different mount 3D prints, one for the mirror and one for a laster & photoresistor pair,
you'll need two screws, two nuts, and two washers for each mount (a local hardware store like True Value,
or Ace Hardware has nuts, bolts and washers, you'll just have to work with fitting them correct sizes).

Pictures of the mounts can be found in "Arduino Workshop\Security System," however they're fitted to the
four wire per trip design, that this folder outlines aim to make each trip only two wires per trip and
sharing groung therefore three wires total for two trips.  One difference to this project is that it
requires a digital and analog pin per trip, in order to have multiple trips operating correctly.

Possible advantages are the low level lighting of the laser during when the trip is tripped, both for
secuirty as it is possibly less likely to be seen, and general light polution if the use is inactive.
