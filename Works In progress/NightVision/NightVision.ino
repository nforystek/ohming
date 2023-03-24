byte writes[8] = {6, 7, 8, 9, 10, 11, 12, 13 };
byte reader[4] = {A0, A2, A3, A4 };

#define Delay_In_Microseconds true

#define Use_Delay_During_Emitter true
#define Delay_During_Emitter 8

#define Use_Delay_Between_Emitters false
#define Delay_Between_Emitters 8

#define Use_Delay_Radius_Multiplier true
#define Delay_Radius_Multiplier 8

/*
 *   LED Setup
 *   
 *   @=receiver/reader
 *   O=emitter/writes
 * 
 *             Indexed      Pin Out
 *     O O       0 1         06 07
 *   O @ @ O   7 0 1 2    13 A0 A1 08
 *   O @ @ O   6 3 2 3    12 A3 A2 09
 *     O O       5 4         11 10
 */

byte data[4*23];

void setup() {

  for (int i=0;i<18; i++) {
    pinMode(writes[i], OUTPUT);
    digitalWrite(writes[i], LOW);
  }
  for (int i=0;i<4*23; i++) {
    data[i]=0;
  }
  Serial.begin(256000);  
}

void do_Delay_During_Emitter() {
#ifdef Use_Delay_During_Emitter
  #ifdef Delay_In_Microseconds
    delayMicroseconds(Delay_During_Emitter);
  #else
    delay(Delay_During_Emitter);
  #endif
#endif
}

void do_Delay_Between_Emitters() {
#ifdef Use_Delay_Between_Emitters
  #ifdef Delay_In_Microseconds
    delayMicroseconds(Delay_Between_Emitters);
  #else
    delay(Delay_Between_Emitters);
  #endif
#endif
}
void do_Delay_Radius_Multiplier(int i) {
#ifdef Use_Delay_Radius_Multiplier
  #ifdef Delay_In_Microseconds
     delayMicroseconds((i+1)*Delay_Radius_Multiplier);
  #else
    delay((i+1)*Delay_Radius_Multiplier); 
  #endif
#else
  #ifdef Use_Delay_During_Emitter
    #ifdef Delay_In_Microseconds
      delayMicroseconds((i+1)*Delay_During_Emitter);
    #else
      delay((i+1)*Delay_During_Emitter);
    #endif
  #else
    #ifdef Delay_In_Microseconds
       delayMicroseconds((i+1));
    #else
      delay((i+1)); 
    #endif
  #endif
#endif
}
byte analogByteRead(int pin) {
    return (255*((float)(((float)analogRead(pin))/1024)));
}

void GatherSet(int c) {
  //c=cycle or set around the circle, 1 to 4 directions
  //for each set we gather the following information:

  //an emitter and reader pair next to each other
  //along the same lines as the current direction
  //this acts as the intregal expiriment direction 
  digitalWrite(writes[(c*2)], HIGH);  
  do_Delay_During_Emitter();
  data[c*23]=analogByteRead(reader[c]);
  digitalWrite(writes[(c*2)], LOW);
  do_Delay_Between_Emitters();

  //the same reader with a emitter diagnal to it
  //or the next emitter forth clockwise like a set
  //this will act as a folcum in ratios controlled
  digitalWrite(writes[(c*2)+1], HIGH);  
  do_Delay_During_Emitter();
  data[c*23+1]=analogByteRead(reader[c]);
  digitalWrite(writes[(c*2)+1], LOW);
  do_Delay_Between_Emitters();

  //according to the prior diagnal, also gather
  //the same signals of it's pair to the next reader
  //along the same lines as the current direction
  //this will act as the factor to direction tests
  digitalWrite(writes[(c*2)+1], HIGH);  
  do_Delay_During_Emitter();
  data[c*23+2]=analogByteRead(reader[c+1]);
  digitalWrite(writes[(c*2)+1], LOW);
  do_Delay_Between_Emitters();

  //using the first emitter left on, and the second
  //pulsing, read from the second reader used above
  //for the duration of 20 repititions as our radius
  //these will act as texts to the ratio directions
  digitalWrite(writes[(c*2)], HIGH);
  for (int i = 0; i<20;i++) {
    digitalWrite(writes[(c*2)+1], HIGH);    
    do_Delay_Radius_Multiplier(i);
    data[c*23+3+i]=analogByteRead(reader[c+1]);
    digitalWrite(writes[(c*2)+1], LOW);    
    do_Delay_Between_Emitters();
  }  
  digitalWrite(writes[(c*2)], LOW);  

}

void loop() {

  GatherSet(0);

  GatherSet(1);

  GatherSet(2);

  GatherSet(3);
  
  if (Serial) {
    Serial.write(data, 4*23);
  }
  
}
