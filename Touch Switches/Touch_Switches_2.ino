
//touch switch sensor example
//
//this method is somewhat slow, but it doesn't require an extra ditigal pin for a sensor.
//jumper the sensor pin with a 10Megaohm resistor to +3.3v power, the examples 3 sensors.
//I am  using a normal breadboard jumper wire as thr touch metal, direct to the digitals.

#define pin1 4
#define pin2 5
#define pin3 6

bool sensor(int btn) {
  for (int i = 0 ; i < 8; i++)
  {
    pinMode(btn, INPUT);
    if(digitalRead(btn)==LOW) return true;
    pinMode(btn, OUTPUT);
    digitalWrite(btn,LOW);
    pinMode(btn, INPUT);
    delay(1);    
  }
  return false;  
}

void setup() {  
  Serial.begin(115200);
}

void loop() {
  Serial.print(sensor(pin1));  
  Serial.print(' ');
  Serial.print(sensor(pin2));
  Serial.print(' ');
  Serial.println(sensor(pin3));
}
