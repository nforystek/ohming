byte writes[8] = {5, 6, 7, 8, 9, 10, 11, 12 };
byte reader[4] = {A0, A1, A2, A3 };

int width=20;
int height=20;

byte pixel[20][20];

void setup() {

  for (int i=0;i<8; i++) {
    pinMode(writes[i], OUTPUT);
    digitalWrite(writes[i], HIGH);
  }
  for (int x=0;x<width; x++) {
    for (int y=0;y<height; y++) {
      pixel[x][y]=0;
    }
  }
  Serial.begin(256000);  
}
float delayValue(int axis, int bound) {
  if (axis<(bound/2)) {
    return (((bound-axis)-(bound/2))/(bound/2));
  } else {
    return ((axis-(bound/2))  /(bound/2));
  }
}

void GatherSet(int x, int y) {

  float reads[4];

  digitalWrite(writes[0], LOW);
  digitalWrite(writes[1], LOW);

  digitalWrite(writes[4], LOW);
  digitalWrite(writes[5], LOW);
  
  reads[0] =((analogRead(reader[0])+ analogRead(reader[2]))/2);
  
  delayMicroseconds(delayValue(x,width));
  
  reads[1] = ((analogRead(reader[1])+ analogRead(reader[3]))/2);

  digitalWrite(writes[0], HIGH);
  digitalWrite(writes[1], HIGH);

  digitalWrite(writes[4], HIGH);
  digitalWrite(writes[5], HIGH);  
 
  digitalWrite(writes[2], LOW);
  digitalWrite(writes[3], LOW);

  digitalWrite(writes[6], LOW);
  digitalWrite(writes[7], LOW);

  reads[2] =((analogRead(reader[0])+analogRead(reader[1]))/2);
  
  delayMicroseconds(delayValue(y,height));
  
  reads[3] =  ((analogRead(reader[2])+analogRead(reader[3]))/2);

  digitalWrite(writes[2], HIGH);
  digitalWrite(writes[3], HIGH);

  digitalWrite(writes[6], HIGH);
  digitalWrite(writes[7], HIGH);

  pixel[x][y]=(byte)((reads[0]+reads[1]+reads[2]+reads[3])/4);

}

void loop() {


  for (int x=0;x<width; x++) {
    for (int y=0;y<height; y++) {
      GatherSet(x,y);
    }
  }

  if (Serial) {
    Serial.flush();
    Serial.write(*pixel, width*height);
    
  }
}
