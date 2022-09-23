
//two IR receivers (transparent), one Ir compliment emitter (dark blue)
//One Infrared emitter (transparent) and receiver (dark green), one red
//laser, 2 x 120 ohm resistor, 2 x 200 ohm resistor, 1 x 330 ohm resistor

//infrared receiver's positive should go to the 330ohm, which goes
//to _INP_.  each IR receivers positive should hook direct to a _INP_
//then also a jumper using the 120 ohm to a positive Arduino power 5v.
//the IR emitter, and the Infrared emitter should should hook directly
//to the 120 ohm resister and then to a digital _OUT_ pin, and the laser
//hooks directly to the 5v arduino power source, arrange the laser and
//one IR receiver on one side aiming to the other side where the setup
//is ifrared emitter, next to the IR emitter, then a small wall, and
//the other IR receiver next to the ifrared receiver, aiming at laster

//Now we are ready to clock invisible red (IR) light using a laser
//in opposed direction (invisible red has a very faint red glow on
//the LED, but does project onto anything, or illuminate the path)
//and bounce back detection (infrared, which aim the same ways and
//detects object distance with no glow) to make placement in smoke
//as a line of rate pixels in a row we can individually pint point

//I think this would be great with a green and blue laser on say
//x and y axis, and the red on z axis to make a rgb spectrum 256

//that's the goal, but I would be happy with a toy light saber
//like real ones that stop short like in the star wars movie.

#define IR1_OUT_PIN 2 //IR
#define IR2_OUT_PIN 4 //laser
#define IR3_OUT_PIN 6 //infrared

#define IR1_INP_PIN A3//3 //IR2
#define IR2_INP_PIN A1//5 //ifrared
#define IR3_INP_PIN A2//6 //IR1

void setup() {
  
  pinMode(IR1_OUT_PIN, OUTPUT);
  digitalWrite(IR1_OUT_PIN, LOW);

  pinMode(IR2_OUT_PIN, OUTPUT);
  digitalWrite(IR2_OUT_PIN, LOW);

  pinMode(IR3_OUT_PIN, OUTPUT);
  digitalWrite(IR3_OUT_PIN, LOW);

  pinMode(IR1_INP_PIN,INPUT);
  pinMode(IR2_INP_PIN,INPUT);
  pinMode(IR3_INP_PIN,INPUT);
  
  Serial.begin(256000);
}

void loop() {
  if (Serial.available()) {
    char ch = Serial.read();
    switch (ch) {
      case '1':
        digitalWrite(IR1_OUT_PIN, !digitalRead(IR1_OUT_PIN));
        break;
      case '2':
        digitalWrite(IR2_OUT_PIN, !digitalRead(IR2_OUT_PIN));
        break;
      case '3':
        digitalWrite(IR3_OUT_PIN, !digitalRead(IR3_OUT_PIN));
        break;
    }
  }

  Serial.print('[');
  Serial.print(digitalRead(IR1_OUT_PIN));
  Serial.print(' ');
  Serial.print(analogRead(IR1_INP_PIN));
  Serial.print(' ');
  Serial.print(digitalRead(IR2_OUT_PIN));
  Serial.print(' ');
  Serial.print(analogRead(IR2_INP_PIN));
  Serial.print(' ');
  Serial.print(digitalRead(IR3_OUT_PIN));
  Serial.print(' ');
  Serial.print(analogRead(IR3_INP_PIN));
  Serial.print(']');
  Serial.println();
}
