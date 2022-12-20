byte writes[8] = {5, 6, 7, 8, 9, 10, 11, 12 };
byte reader[4] = {A0, A1, A2, A3 };

byte data[16];

void setup() {

  for (int i=0;i<8; i++) {
    pinMode(writes[i], OUTPUT);
    digitalWrite(writes[i], LOW);
  }
  for (int i=0;i<16; i++) {
    data[i]=0;
  }
  Serial.begin(115200);  
}

void GatherSet(int idx, int in1, int out1) {
  //digitalWrite(writes[out1], LOW);
  data[idx] = analogRead(reader[in1]);
  //digitalWrite(writes[out1], HIGH);
}

void loop() {

  GatherSet(0, 0, 0);
  GatherSet(1, 0, 1);
  GatherSet(2, 0, 2);
  GatherSet(3, 0, 3);   

  GatherSet(4, 1, 2);
  GatherSet(5, 1, 3);
  GatherSet(6, 1, 4);
  GatherSet(7, 1, 5);

  GatherSet(8, 2, 4);
  GatherSet(9, 2, 5);
  GatherSet(10, 2, 6);
  GatherSet(11, 2, 7);  

  GatherSet(12, 3, 6);
  GatherSet(13, 3, 7);
  GatherSet(14, 3, 0);
  GatherSet(15, 3, 1);   

  if (Serial) {
    Serial.flush();
    Serial.write(data, 16);
    
  }
}
