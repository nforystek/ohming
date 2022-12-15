const byte MaxPacketSize=200;
bool doblink=false;


byte stateLen=1;
byte stateDat[0];

void setup() {
  pinMode(LED_BUILTIN, OUTPUT);
  Serial.begin(115200);
}

void loop() {
  ProcessSerial(); 
  Blinker();
}

void Blinker() {
  static unsigned long elapsed;
  if (millis()-elapsed>100) {
    elapsed=millis();
    static bool isOn;
    if ((!isOn)&&doblink)
      digitalWrite(LED_BUILTIN, HIGH); 
    else
      digitalWrite(LED_BUILTIN, LOW);
    isOn=!isOn;
  }
}

String NextWord(String data) {
  return ((data.indexOf(" ")>-1)?data.substring(0,data.indexOf(" ")):data);
}

String DataPack() {
  String ret="";
  ret += ((char)(stateLen+1));
  for (int i=0;i<stateLen;i++) {
    ret += ((char)stateDat[i]);
  }
  return ret + ((char)0x00);
}
void DataRead(String data) {
  String cmd=NextWord(data);
  String var;  
  if (cmd=="echo") {
      if (data.length()>5) DataSend(data.substring(5));      
  } else  if (cmd=="hello") {
      DataSend("Arduino ready, send help for information.\r\n");
  } else  if (cmd=="help") {
      DataSend("Commands\r\n");
      DataSend(" hello - Service ping, if a message is returned.\r\n");
      DataSend(" help - Displays this help, a list of commands.\r\n");
      DataSend(" echo <text> - Causes the Arduino to loopback.\r\n");
      DataSend(" noop - Does nothing, not even an error message.\r\n");
      DataSend(" toggle led - Toggles the Arduino LED on or off.\r\n");
      DataSend(" state led - Returns the state of the LED blink.\r\n");
  } else if (cmd=="toggle") {
    data = data.substring(7);
    var = NextWord(data); 
    if (var=="led") {
      doblink=!doblink;
    } else DataSend("Toggler unkown for " + var + "!\r\n");    
  } else if (cmd=="state") {    
    data = data.substring(6);
    var = NextWord(data); 
    if (var=="led") {
       DataSend("State for: " + var + " is " + (doblink?"blinking":"off") + "\r\n");
    } else DataSend("Toggler unkown for " + var + "!\r\n");    
  } else if (cmd=="noop") {
  } else {
    DataSend("Command not understood!\r\n");
  }
}

String DataSend(String text) {
  static String out;
  if (text!="") {
      while (text!="") {
        if (text.length() > MaxPacketSize) {
          out += ((char)MaxPacketSize) + text.substring(0,(MaxPacketSize-1));
          text=text.substring(MaxPacketSize);
        } else {
          out += ((char)text.length()) + text;
          text = "";
        }
      }
      return "";
  } else if (out != "") {
    String ret=out.substring(0,1);
    out = out.substring(1);
    return ret;
  } else return ""; 
}

void ProcessSerial() {
  static String data;
  byte bl;
  if (Serial.available())  data += ((char)Serial.read());    
  switch ((data=="")?'0':data.charAt(0))
  {
    case '0':
      Serial.print(DataSend(""));
      break;
    default:
      bl=data.charAt(0);
      if (((data.length()>=(bl+1))&&(bl>0))) {           
        data = data.substring(1);
        DataRead(data.substring(0,bl));
        if (data.length()>bl)
          data = data.substring(bl);
        else
          data = "";          
      } else if (Serial.available()) data += ((char)Serial.read());
      break;
  }
}
