byte writes[8] = {5, 6, 7, 8, 9, 10, 11, 12 };
byte reader[4] = {A0, A1, A2, A3 };


/*
 *   LED Setup
 *   
 *   @=receiver/reader
 *   O=emitter/writes
 * 
 *     O O        0 1
 *   O @ @ O    7 0 1 2
 *   O @ @ O    6 3 2 3
 *     O O        5 4
 */



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

float delayValue(int axis, int bound, bool leftorright) {
  float half = (((float)bound)/2);
  float faxis=((float)axis);
  if (faxis<half) {
    if (leftorright) {
      return ((((((-faxis-half)+((float)bound))/half)*((float)bound)))*10);
    } else {
      return 0;
    }
  } else if (faxis>half) {
     if (!leftorright) {
      return (((((faxis-half) / half)*((float)bound)))*10);
     } else {
      return 0;
     }
  }
}

void GatherSet(int x, int y) {

  float reads[4];  

  digitalWrite(writes[7], LOW);
  digitalWrite(writes[6], LOW);

  delayMicroseconds(delayValue(x,width,true));
  
  reads[0] = (((float)(analogRead(reader[3])+analogRead(reader[0])))/2);

  digitalWrite(writes[7], HIGH);
  digitalWrite(writes[6], HIGH);

  digitalWrite(writes[3], LOW);
  digitalWrite(writes[2], LOW);
  
  delayMicroseconds(delayValue(x,width,false));
  
  reads[1] = (((float)(analogRead(reader[1])+analogRead(reader[2])))/2);
  
  digitalWrite(writes[3], HIGH);
  digitalWrite(writes[2], HIGH); 
 
  digitalWrite(writes[0], LOW);
  digitalWrite(writes[1], LOW);

  
  delayMicroseconds(delayValue(y,height,true));
  
  reads[2] = (((float)(analogRead(reader[0])+analogRead(reader[1])))/2);


  digitalWrite(writes[0], HIGH);
  digitalWrite(writes[1], HIGH);
 
  digitalWrite(writes[4], LOW);
  digitalWrite(writes[5], LOW);
  
  delayMicroseconds(delayValue(y,height,false));
  
  reads[3] = (((float)(analogRead(reader[2])+analogRead(reader[3])))/2);


  digitalWrite(writes[4], HIGH);
  digitalWrite(writes[5], HIGH);

  pixel[x][y]=((byte)((reads[0]+reads[1]+reads[2]+reads[3])/4));

}

void loop() {

  byte hi=0;
  byte lo=255;
  float avg=0;
  for (int x=0;x<width; x++) {
    for (int y=0;y<height; y++) {
      GatherSet(x,y);
      if (pixel[x][y]>hi) hi = pixel[x][y];
      if (pixel[x][y]<lo) lo = pixel[x][y];
      avg+=pixel[x][y];
    }
  }
  avg = (avg / ((float)(width*height)));
  for (int x=0;x<width; x++) {
    for (int y=0;y<height; y++) {
      if (pixel[x][y]>avg) {
        pixel[x][y] +=(byte)(((((float)hi)-pixel[x][y])/256)*((float)(256-pixel[x][y])));
      } else if (pixel[x][y]<avg) {
        pixel[x][y] -=(byte)(((pixel[x][y]-((float)lo))/256)*((float)pixel[x][y]));
      }
    }
  }

  if (Serial) {

    Serial.write(*pixel, width*height);
    
  }
}
