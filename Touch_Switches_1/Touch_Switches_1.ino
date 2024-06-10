//Touch switch code similar to CapacitiveSensor.h
//Each pin requires a 10Megaohm resistor jumper
//from the touch sensor pin to a set pin, example
//here is using 4 as the set pin and two switches
//at 5 and 6, I am using basic breadboard jumpers
//as the touch metal, really long ones to contact

void setup()
{
  Serial.begin(115200);
}

int sensor(byte set, byte pin) {
  pinMode(set, OUTPUT);
  pinMode(pin, INPUT);
  digitalWrite(set,LOW);
  long val2=0;  
  static bool val1;
  static bool toggle;
  toggle=val1;
  for (int i = 0 ; i<200; i++) {
    val1=digitalRead(pin);
    if (val1!=toggle) {
      toggle=val1;
      digitalWrite(set, !val1);
      digitalWrite(pin, LOW);
      val2++;
    } else {
      digitalWrite(pin, LOW);
      digitalWrite(set, !toggle);      
    }
  }
  return (!(val2>45));  
}
void loop()
{
  Serial.print( sensor( 4, 5));
  Serial.print(' ');
  Serial.println( sensor(4,  6));
}
