
bool doblink=false;
static unsigned long elapsed;
int Handshake = 1;

void setup() {
  pinMode(LED_BUILTIN, OUTPUT);
  Serial.begin(115200);
  elapsed=millis();
  Handshake=1;
}

void Blinker() {
  static bool isOn;
  if ((!isOn)&&doblink)
    digitalWrite(LED_BUILTIN, HIGH); 
  else
    digitalWrite(LED_BUILTIN, LOW);
  isOn=!isOn;
}

String NextWord(String data) {
  return ((data.indexOf(" ")>-1)?data.substring(0,data.indexOf(" ")):data);
}

void DataRead(String data) {
  String cmd=NextWord(data);
  String var;  
  if (cmd=="echo") {
      if (data.length()>5) DataSend(data.substring(5));      
  } else  if (cmd=="hello") {
      DataSend("Arduino Uno, send help for information.\r\n");
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
  } else DataSend("Command not understood!\r\n");
}

String DataSend(String text) {
  static String out;
  if (text!="") {
      while (text!="") {
        if (text.length() > 255) {
          out += ((char)255) + text.substring(0,254);
          text=text.substring(255);
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

void ProcessPackets() {
  static String data;
  byte bl;
  if (Serial.available())  data += ((char)Serial.read());    
  switch ((data=="")?'0':data.charAt(0))
  {
    case '1':
      Handshake=1; //the routine wont
      //handle single byte packets now
      //with handshake, they must be 2+
      break;
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
      } else if (Serial.available()) {
        data += ((char)Serial.read());
      } else {
        //TODO: timeout waiting and reset handhsake
        //to tell the remote serial to repeat packet
      }     
      break;
  }
}

int Toggler(int val) {
  switch (val)
    {
      case 1:
        return 0;
      case 0:
        return -1;
      default:
        return 1;
    }
}

void ProcessSerial() {  
  char inc=' ';  
  switch (Handshake)
  {
    case 0:
    case 1:
      if (Serial.available()) {
        inc=(char)Serial.read();
        Serial.print(String(fabs(Handshake)).charAt(0));
        Handshake = Toggler(Handshake);
      }      
      break;
    case -1:
       ProcessPackets();
       break;
  }
}

void loop() {
  ProcessSerial();
}
