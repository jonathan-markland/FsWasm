
void ClearScreen()
{
  for(int i=0xB8000; i<0xBA000; i += 2)
  {
    *(unsigned short *) i = 0x1F20;
  }
}


void WriteText(int x, int y, const char *s)
{
  int a = y * 160 + x + 0xB8000;
  while (*s)
  {
    *(char *)a = *s;
    *(char *)(a+1) = 0x2F;
    a += 2;
    ++s;
  }
}


void Write1()
{
  WriteText(20,10, "Write1()");
}


void Write2()
{
  WriteText(20,10, "Write2()");
}


void WriteN(int y)
{
  WriteText(20,y,"NNNNNNNNN");
}

void WriteM(int y)
{
  WriteText(20,y,"MMMMMMMMM");
}



int WaitKey()
{
  return *(char *) 0xC0000;  // Fake address
}


int main() 
{ 
  ClearScreen();
  
  WriteText(10, 2, "Hello");
  WriteText(0,24, "This is the bottom line of the screen");
  
  for (int y=4; y<14; y++)
  {
    WriteText(20,y,"**********");
  }
  
  void (*func)() = &Write1;
  void (*funcN)() = &WriteN;
  
  for(;;)
  {
    int w = WaitKey();
    if(w == 27) break;
    for (int y=4; y<14; y++)
    {
      switch(w)
      {
        case 65:  WriteText(20,y,"AAAAAAAAAA"); func=&Write1; break;
        case 66:  WriteText(20,y,"AAAAAAAAAA"); func=&Write2; break;
        case 67:  WriteText(20,y,"AAAAAAAAAA"); break;
        case 68:  WriteText(20,y,"AAAAAAAAAA"); break;
        case 69:  WriteText(20,y,"AAAAAAAAAA"); break;
        case 70:  WriteText(20,y,"AAAAAAAAAA"); break;
        case 71:  WriteText(20,y,"AAAAAAAAAA"); funcN=&WriteN; break;
        case 72:  WriteText(20,y,"AAAAAAAAAA"); funcN=&WriteM; break;
        case 73:  WriteText(20,y,"AAAAAAAAAA"); break;
        case 74:  WriteText(20,y,"AAAAAAAAAA"); break;
        case 75:  WriteText(20,y,"AAAAAAAAAA"); break;
        case 76:  WriteText(20,y,"AAAAAAAAAA"); break;
        case 77:  WriteText(20,y,"AAAAAAAAAA"); break;
        case 78:  WriteText(20,y,"AAAAAAAAAA"); break;
        case 79:  WriteText(20,y,"AAAAAAAAAA"); break;
        case 80:  WriteText(20,y,"AAAAAAAAAA"); break;
      }
      (*func)();
      (*funcN)();
    }
  }
  
  return 0;
}
