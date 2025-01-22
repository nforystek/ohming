byte writes[8] = {2, 3, 4, 5, 6, 7, 8, 9};
byte reader[4] = {A0, A1, A2, A3 };

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
byte data[4*2];

void setup() {

  for (int i=0;i<8; i++) {
    pinMode(writes[i], OUTPUT);
    digitalWrite(writes[i], LOW);
  }
  for (int i=0;i<4*2; i++) {
    data[i]=0;
  }
  Serial.begin(256000); 
}

byte analogByteRead(int pin) {
     return map(analogRead(pin),0,1024,0,255);
}

void GatherSet(int c) {

  /* using two readers, in a total of 4 reads 2 each
  that per pair of emitters with the same readers */

 /* so it looks like this per each
  * four read starting north reads
  *  
  *     O x  
  *   x @ x x
  *   x x x x
  *     x x  
  *     
  *     O O  
  *   x @ x x
  *   x x x x
  *     x x  
  *     
  *     O O  
  *   x x @ x
  *   x x x x
  *     x x  
  *     
  *     x O  
  *   x x @ x
  *   x x x x
  *     x x  
  *     
  * the same pattern per set for east, south,
  * and west emitters but using same readers
  */

  digitalWrite(writes[(c*2)+0], HIGH);
  data[(c*2)+0]=analogByteRead(reader[c]);  
  digitalWrite(writes[(c*2)+0], LOW);
  digitalWrite(writes[(c*2)+1], HIGH);
  data[(c*2)+1]=analogByteRead(reader[c]);
  digitalWrite(writes[(c*2)+1], LOW);
   
}

void loop() {

  GatherSet(0); //reader one

  GatherSet(1); //reader two

  GatherSet(2); //reader three

  GatherSet(3); //reader four

  if (Serial) {
    Serial.write(data, 4*2);
  }
  
}
