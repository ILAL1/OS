package processing.test.final_emoji;

import processing.core.*; 
import processing.data.*; 
import processing.event.*; 
import processing.opengl.*; 

import ketai.ui.*; 
import android.view.MotionEvent; 
import android.view.inputmethod.InputMethodManager; 
import android.content.Context; 

import java.util.HashMap; 
import java.util.ArrayList; 
import java.io.File; 
import java.io.BufferedReader; 
import java.io.PrintWriter; 
import java.io.InputStream; 
import java.io.OutputStream; 
import java.io.IOException; 

public class final_Emoji extends PApplet {






enum Mode 
{
  START, 
    STOP_POSTURE, 
    MOTION_POSTURE, 
    ADD_MOTION_EXPRESSION, 
    SECOND_MOTION, 
    MOTION_CHECKING, 
    ADD_EXPRESSION, 
    ADD_TEXT, 
    ADD_TEXT_MOTION,
    SAVE,
    SAVE_MOTION
};

Emoji emoji;
PFont inter_font;
PFont text_font;
KetaiGesture gesture;

public void setup() {
  
  gesture = new KetaiGesture(this);
  emoji = new Emoji();
  inter_font = loadFont("HelveticaNeue-100.vlw");
  text_font = loadFont("_Daisy_s_Delights__-100.vlw");
}

public void draw() {
  background(0);
  emoji.start();
}

public void mousePressed() {
  emoji.mousePressed();
}

public void mouseDragged() {
  emoji.mouseDragged();
}

public void mouseReleased() {
  emoji.mouseReleased();
}

public void keyPressed () {
  emoji.keyPressed();
}

class Emoji {
  protected Mode mode;
  RectButton stopButton;
  RectButton motionButton;
  RectButton saveButton;
  ArrowButton arrowButton;
  eDisplay eDisplay;
  cStorage cStorage;
  Shadow shadowB;
  Shadow shadowCS;
  CharacterFactory characterFactory;
  ArrayList<Character> allCharacters;
  ExpressionFactory expressionFactory;
  ArrayList<Expression> allExpressions;
  EmoticonFactory emoticonFactory;
  ArrayList<Emoticon> allEmoticons;
  ArrayList<Emoticon> emoticonsDraw;
  Text emoticonText;
  PImage mask;
  PGraphics pg;
  ArrayList<Emoticon> startEmoticonsDraw;
  ArrayList<Emoticon> endEmoticonsDraw;
  Animation ani;

  Emoji() {
    mode = Mode.START;
    stopButton = new RectButton(width/2, 1*9*height/10/3+height/10, PApplet.parseInt(width/2), width/9, color(232, 110, 107), "Stop Emoticon");
    motionButton = new RectButton(width/2, 2*9*height/10/3+height/10, PApplet.parseInt(width/2), width/9, color(232, 110, 107), "Motion Emoticon");
    saveButton = new RectButton(width/2, 2*9*height/10/3+height/10, PApplet.parseInt(width/2), width/9, color(232, 110, 107), "Save");
    arrowButton = new ArrowButton(9*width/10, height/20, height/30, height/15);
    eDisplay = new eDisplay(width/2, 13*height/40, width, height);
    cStorage = new cStorage();
    shadowB = new Shadow(0, height/10, width, height/100, color(0, 51), color(0, 0), 1);
    shadowCS = new Shadow(0, 11*height/20, width, height/100, color(0, 51), color(0, 0), 1);
    characterFactory = new CharacterFactory();
    characterFactory.loadXMLfile("characters.xml");
    allCharacters = characterFactory.getCharacters();
    expressionFactory = new ExpressionFactory();
    expressionFactory.loadXMLfile("expressions.xml");
    allExpressions = expressionFactory.getExpressions();
    emoticonFactory = new EmoticonFactory();
    emoticonFactory.loadXMLfile("emoticons.xml");
    allEmoticons = emoticonFactory.getEmoticons();
    emoticonsDraw = new ArrayList<Emoticon>();
    emoticonText = new Text();
    mask = loadImage("mask.png");
    pg = createGraphics(width, height, P2D);
    startEmoticonsDraw = new ArrayList<Emoticon>();
    endEmoticonsDraw = new ArrayList<Emoticon>();
    ani = new Animation(startEmoticonsDraw, endEmoticonsDraw);
  }

  public void start() {
    //background
    noStroke();
    rectMode(CORNER);
    fill(224, 139, 118);
    rect(0, 0, width, height);

    switch (mode) {
    case START:
      stopButton.draw();
      motionButton.draw();
      break;
    case STOP_POSTURE:
      //emoticon display
      eDisplay.draw(); 

      //emoticon display
      if (emoticonsDraw != null) {
        for (Emoticon e : emoticonsDraw) {
          e.draw();
        }
      } 

      imageMode(CENTER);
      image(mask, width/2, height/2, width, height);

      //character storage display
      cStorage.draw();

      //shadow on cStorage
      shadowCS.draw();

      //character display
      if (allCharacters != null) {
        for (Character c : allCharacters) {
          c.draw();
        }
      }
      break;
    case MOTION_POSTURE:
      //emoticon display
      eDisplay.draw(); 

      //emoticon display
      if (emoticonsDraw != null) {
        for (Emoticon e : emoticonsDraw) {
          e.draw();
        }
      }

      imageMode(CENTER);
      image(mask, width/2, height/2, width, height);

      //character storage display
      cStorage.draw();

      //shadow on cStorage
      shadowCS.draw();

      //character display
      if (allCharacters != null) {
        for (Character c : allCharacters) {
          c.draw();
        }
      }
      break;
    case ADD_MOTION_EXPRESSION:
      //emoticon display
      eDisplay.draw(); 

      //emoticon display
      if (emoticonsDraw != null) {
        for (Emoticon e : emoticonsDraw) {
          e.draw();
          if (e.expImg != null) {
            e.drawExpression();
          }
        }
      }

      imageMode(CENTER);
      image(mask, width/2, height/2, width, height);

      //character storage display
      cStorage.draw();

      //shadow on cStorage
      shadowCS.draw();

      //character display
      if (allExpressions != null) {
        for (Expression ex : allExpressions) {
          ex.draw();
        }
      }
      break;
    case SECOND_MOTION:
      //emoticon display
      eDisplay.draw(); 

      if (emoticonsDraw != null) {
        for (Emoticon e : emoticonsDraw) {
          e.draw();
          if (e.expImg != null) {
            e.drawExpression();
          }
        }
      } 

      imageMode(CENTER);
      image(mask, width/2, height/2, width, height);

      break;
    case MOTION_CHECKING:
      //emoticon display
      eDisplay.draw();

      ani.draw();

      imageMode(CENTER);
      image(mask, width/2, height/2, width, height);
      break;
    case ADD_EXPRESSION:
      //emoticon display
      eDisplay.draw(); 

      //emoticon display
      if (emoticonsDraw != null) {
        for (Emoticon e : emoticonsDraw) {
          e.draw();
          if (e.expImg != null) {
            e.drawExpression();
          }
        }
      }

      imageMode(CENTER);
      image(mask, width/2, height/2, width, height);

      //character storage display
      cStorage.draw();

      //shadow on cStorage
      shadowCS.draw();

      //character display
      if (allExpressions != null) {
        for (Expression ex : allExpressions) {
          ex.draw();
        }
      }
      break;
    case ADD_TEXT:
      //emoticon display
      eDisplay.draw(); 

      //emoticon display
      if (emoticonsDraw != null) {
        for (Emoticon e : emoticonsDraw) {
          e.draw();
          if (e.expImg != null) {
            e.drawExpression();
          }
        }
      }

      if (emoticonText != null && emoticonText.text.length()>0) {
        emoticonText.drawText();
      }

      imageMode(CENTER);
      image(mask, width/2, height/2, width, height);
      break;
    case ADD_TEXT_MOTION:
      //emoticon display
      eDisplay.draw(); 

      //emoticon display
      if (emoticonsDraw != null) {
        for (Emoticon e : emoticonsDraw) {
          e.draw();
          if (e.expImg != null) {
            e.drawExpression();
          }
        }
      }

      if (emoticonText != null && emoticonText.text.length()>0) {
        emoticonText.drawText();
      }

      imageMode(CENTER);
      image(mask, width/2, height/2, width, height);
      break;
    case SAVE:
      //emoticon display
      pg.beginDraw();
      eDisplay.pgDraw(pg); 
      if (emoticonsDraw != null) {
        for (Emoticon e : emoticonsDraw) {
          e.pgDraw(pg);
          if (e.expImg != null) {
            e.pgDrawExpression(pg);
          }
        }
      }

      if (emoticonText != null && emoticonText.text.length()>0) {
        emoticonText.pgDrawText(pg);
      }
      pg.endDraw();

      imageMode(CENTER);
      image(pg, width/2, height/2);

      imageMode(CENTER);
      image(mask, width/2, height/2, width, height);
      break;
    case  SAVE_MOTION:
      //emoticon display
      eDisplay.draw();

      ani.draw();
      
      if (emoticonText != null && emoticonText.text.length()>0) {
        emoticonText.drawText();
      }

      imageMode(CENTER);
      image(mask, width/2, height/2, width, height);

      break;
    default:
      break;
    }

    //status bar
    rectMode(CORNER);
    noStroke();
    fill(240, 114, 168);
    rect(0, 0, width, height/10);

    //emoji text in status bar
    textAlign(CENTER, CENTER);
    textFont(inter_font, height/20);
    fill(255);
    text("EMOJI", width/2, height/20);

    if (emoticonsDraw.size() != 0 && mode != Mode.SAVE && mode != Mode.SAVE) {
      arrowButton.draw();
    }

    if (mode == Mode.SAVE || mode == Mode.SAVE_MOTION) {
      saveButton.draw();
    }

    //shadow under status bar
    shadowB.draw();
  }

  public void mousePressed() {
    switch (mode) {
    case START:
      if (stopButton.isOver(mouseX, mouseY)) {
        mode = Mode.STOP_POSTURE;
      } else if (motionButton.isOver(mouseX, mouseY)) {
        mode = Mode.MOTION_POSTURE;
      }
      break;
    case STOP_POSTURE:
      for (Emoticon e : emoticonsDraw) {
        for (int i = 0; i<4; i++) {
          if (eDisplay.isOver(mouseX, mouseY)) {
            if (e.isOver(mouseX, mouseY)) {
              e.transPivotPos = new PVector(mouseX, mouseY);
              e.isTrans = true;
            }
            if (e.isOverPivot(mouseX, mouseY, i)) {
              e.maniPivotPos = new PVector(mouseX, mouseY);
              e.isMani[i] = true;
              e.isTrans = false;
              break;
            }
          }
        }
      }
      if (arrowButton.isOver(mouseX, mouseY) && emoticonsDraw.size() != 0) {
        mode = Mode.ADD_EXPRESSION;
      }
      break;
    case MOTION_POSTURE:
      for (Emoticon e : emoticonsDraw) {
        for (int i = 0; i<4; i++) {
          if (eDisplay.isOver(mouseX, mouseY)) {
            if (e.isOver(mouseX, mouseY)) {
              e.transPivotPos = new PVector(mouseX, mouseY);
              e.isTrans = true;
            }
            if (e.isOverPivot(mouseX, mouseY, i)) {
              e.maniPivotPos = new PVector(mouseX, mouseY);
              e.isMani[i] = true;
              e.isTrans = false;
              break;
            }
          }
        }
      }
      if (arrowButton.isOver(mouseX, mouseY) && emoticonsDraw.size() != 0) {
        mode = Mode.ADD_MOTION_EXPRESSION;
      }
      break;
    case ADD_MOTION_EXPRESSION:
      int exp_count = 0;
      int e_count = 0;
      for (Emoticon e : emoticonsDraw) {
        if (eDisplay.isOver(mouseX, mouseY)) {
          if (e.isOver(mouseX, mouseY)) {
            e.transPivotPos = new PVector(mouseX, mouseY);
            e.isTrans = true;
          }
        }
        e_count++;
        if (e.expImg != null) {
          exp_count++;
        }
      }
      if (arrowButton.isOver(mouseX, mouseY) && e_count == exp_count) {
        for (Emoticon e : emoticonsDraw) {
          startEmoticonsDraw.add(new Emoticon(e));
        }
        mode = Mode.SECOND_MOTION;
      }
      break;
    case SECOND_MOTION:
      for (Emoticon e : emoticonsDraw) {
        for (int i = 0; i<4; i++) {
          if (eDisplay.isOver(mouseX, mouseY)) {
            if (e.isOver(mouseX, mouseY)) {
              e.transPivotPos = new PVector(mouseX, mouseY);
              e.isTrans = true;
            }
            if (e.isOverPivot(mouseX, mouseY, i)) {
              e.maniPivotPos = new PVector(mouseX, mouseY);
              e.isMani[i] = true;
              e.isTrans = false;
              break;
            }
          }
        }
      }
      if (arrowButton.isOver(mouseX, mouseY) && emoticonsDraw.size() != 0) {
        for (Emoticon e : emoticonsDraw) {
          endEmoticonsDraw.add(new Emoticon(e));
        }
        mode = Mode.MOTION_CHECKING;
      }
      break;
    case MOTION_CHECKING:
      if (arrowButton.isOver(mouseX, mouseY)) {
        mode = Mode.ADD_TEXT_MOTION;
      }
      break;
    case ADD_EXPRESSION:
      exp_count = 0;
      e_count = 0;
      for (Emoticon e : emoticonsDraw) {
        if (eDisplay.isOver(mouseX, mouseY)) {
          if (e.isOver(mouseX, mouseY)) {
            e.transPivotPos = new PVector(mouseX, mouseY);
            e.isTrans = true;
          }
        }
        e_count++;
        if (e.expImg != null) {
          exp_count++;
        }
      }
      if (arrowButton.isOver(mouseX, mouseY) && e_count == exp_count) {
        mode = Mode.ADD_TEXT;
      }
      break;
    case ADD_TEXT:
      for (Emoticon e : emoticonsDraw) {
        if (eDisplay.isOver(mouseX, mouseY)) {
          if (e.isOver(mouseX, mouseY)) {
            e.transPivotPos = new PVector(mouseX, mouseY);
            e.isTrans = true;
          }
        }
      }
      if (eDisplay.isOver(mouseX, mouseY) && !emoticonText.isOver(mouseX, mouseY)) {  
        int isEmoticonSelected = 0;
        for (Emoticon e : emoticonsDraw) {
          if (e.isTrans) {
            isEmoticonSelected++;
          }
        }
        if (isEmoticonSelected == 0) {
          showVirtualKeyboard();
        }
      }
      if (emoticonText.text.length() == 0) {
        emoticonText.setTextPos(mouseX, mouseY);
      }

      if (emoticonText.isOver(mouseX, mouseY)) {
        emoticonText.isTrans = true;
      }

      if (arrowButton.isOver(mouseX, mouseY)) {
        mode = Mode.SAVE;
      }
      break;
    case ADD_TEXT_MOTION:

      if (eDisplay.isOver(mouseX, mouseY) && !emoticonText.isOver(mouseX, mouseY)) {  
          showVirtualKeyboard();
      }
      
      if (emoticonText.text.length() == 0) {
        emoticonText.setTextPos(mouseX, mouseY);
      }

      if (emoticonText.isOver(mouseX, mouseY)) {
        emoticonText.isTrans = true;
      }

      if (arrowButton.isOver(mouseX, mouseY)) {
        mode = Mode.SAVE_MOTION;
      }
      break;
    case SAVE:
      if (saveButton.isOver(mouseX, mouseY)) {
        pg.save("/sdcard/DCIM/Camera/abc.png");
        mode = Mode.START;
        reset();
      }
      break;
    case SAVE_MOTION:
      if (saveButton.isOver(mouseX, mouseY)) {
        mode = Mode.START;
        reset();
      }
      break;
    default:
      break;
    }
  }

  public void mouseDragged() {
    switch (mode) {
    case START:
      break;
    case STOP_POSTURE:
      if (allCharacters == null) return;
      for (Character c : allCharacters) {
        if (c.isOver(mouseX, mouseY)) {
          c.x = mouseX; 
          c.y = mouseY;
          if (eDisplay.isOver(mouseX, mouseY))
          {
            Emoticon emoticon = null;
            for (Emoticon e : allEmoticons) {
              if (e.name.equals(c.name)) {
                emoticon = e;
              }
            }
            if (emoticon != null) {
              Emoticon check = null;
              for (Emoticon e : emoticonsDraw) {
                if (emoticon.name.equals(e.name)) {
                  check = e;
                }
              }
              if (check == null) {
                emoticonsDraw.add(emoticon);
              }
            }
          }
          return;
        }
      }
      for (Emoticon e : emoticonsDraw) {
        for (int i= 0; i<4; i++) {
          if (e.isMani[i]) {
            e.manipulate(mouseX, mouseY, i);
            return;
          }
          if (eDisplay.isOver(mouseX, mouseY)) {
            if (e.isTrans) {
              e.translate(mouseX, mouseY);
              e.transPivotPos = new PVector(mouseX, mouseY);
              return;
            }
          }
        }
      }
      break;
    case MOTION_POSTURE:
      if (allCharacters == null) return;
      for (Character c : allCharacters) {
        if (c.isOver(mouseX, mouseY)) {
          c.x = mouseX; 
          c.y = mouseY;
          if (eDisplay.isOver(mouseX, mouseY))
          {
            Emoticon emoticon = null;
            for (Emoticon e : allEmoticons) {
              if (e.name.equals(c.name)) {
                emoticon = e;
              }
            }
            if (emoticon != null) {
              Emoticon check = null;
              for (Emoticon e : emoticonsDraw) {
                if (emoticon.name.equals(e.name)) {
                  check = e;
                }
              }
              if (check == null) {
                emoticonsDraw.add(emoticon);
              }
            }
          }
          return;
        }
      }
      for (Emoticon e : emoticonsDraw) {
        for (int i= 0; i<4; i++) {
          if (e.isMani[i]) {
            e.manipulate(mouseX, mouseY, i);
            return;
          }
          if (eDisplay.isOver(mouseX, mouseY)) {
            if (e.isTrans) {
              e.translate(mouseX, mouseY);
              e.transPivotPos = new PVector(mouseX, mouseY);
              return;
            }
          }
        }
      }
      break;
    case ADD_MOTION_EXPRESSION:
      if (allExpressions == null) return;
      for (Expression ex : allExpressions) {
        if (ex.isOver(mouseX, mouseY)) {
          ex.x = mouseX; 
          ex.y = mouseY;
          for (Emoticon e : emoticonsDraw) {
            if (e.isOver(mouseX, mouseY)) {
              e.setExpression(ex.filename);
            }
          }
          return;
        }
      }
      for (Emoticon e : emoticonsDraw) {
        for (int i= 0; i<4; i++) {
          if (eDisplay.isOver(mouseX, mouseY)) {
            if (e.isTrans) {
              e.translate(mouseX, mouseY);
              e.transPivotPos = new PVector(mouseX, mouseY);
              return;
            }
          }
        }
      }
      break;
    case SECOND_MOTION:
      for (Emoticon e : emoticonsDraw) {
        for (int i= 0; i<4; i++) {
          if (e.isMani[i]) {
            e.manipulate(mouseX, mouseY, i);
            return;
          }
          if (eDisplay.isOver(mouseX, mouseY)) {
            if (e.isTrans) {
              e.translate(mouseX, mouseY);
              e.transPivotPos = new PVector(mouseX, mouseY);
              return;
            }
          }
        }
      }
      break;
    case MOTION_CHECKING:
      break;
    case ADD_EXPRESSION:
      if (allExpressions == null) return;
      for (Expression ex : allExpressions) {
        if (ex.isOver(mouseX, mouseY)) {
          ex.x = mouseX; 
          ex.y = mouseY;
          for (Emoticon e : emoticonsDraw) {
            if (e.isOver(mouseX, mouseY)) {
              e.setExpression(ex.filename);
            }
          }
          return;
        }
      }
      for (Emoticon e : emoticonsDraw) {
        for (int i= 0; i<4; i++) {
          if (eDisplay.isOver(mouseX, mouseY)) {
            if (e.isTrans) {
              e.translate(mouseX, mouseY);
              e.transPivotPos = new PVector(mouseX, mouseY);
              return;
            }
          }
        }
      }
      break;
    case ADD_TEXT:
      if (emoticonText.isOver(mouseX, mouseY) && emoticonText.isTrans) {
        emoticonText.setTextPos(mouseX, mouseY);
        return;
      }
      for (Emoticon e : emoticonsDraw) {
        for (int i= 0; i<4; i++) {
          if (eDisplay.isOver(mouseX, mouseY)) {
            if (e.isTrans) {
              e.translate(mouseX, mouseY);
              e.transPivotPos = new PVector(mouseX, mouseY);
              return;
            }
          }
        }
      }
      break;
    case ADD_TEXT_MOTION:
      if (emoticonText.isOver(mouseX, mouseY) && emoticonText.isTrans) {
        emoticonText.setTextPos(mouseX, mouseY);
      }
      break;
    case SAVE:
      break;
    case  SAVE_MOTION:
      break;
    default:
      break;
    }
  }

  public void mouseReleased() {
    switch (mode) {
    case START:
      break;
    case STOP_POSTURE:
      if (allCharacters == null) return;
      for (Character c : allCharacters) {
        c.x = c.initX;
        c.y = c.initY;
      }
      for (Emoticon e : emoticonsDraw) {
        for (int i = 0; i<4; i++) {
          e.isMani[i] = false;
        }
        e.isTrans = false;
      }
      break;
    case MOTION_POSTURE:
      if (allCharacters == null) return;
      for (Character c : allCharacters) {
        c.x = c.initX;
        c.y = c.initY;
      }
      for (Emoticon e : emoticonsDraw) {
        for (int i = 0; i<4; i++) {
          e.isMani[i] = false;
        }
        e.isTrans = false;
      }
      break;
    case ADD_MOTION_EXPRESSION:
      if (allExpressions == null) return;
      for (Expression ex : allExpressions) {
        ex.x = ex.initX;
        ex.y = ex.initY;
      }
      for (Emoticon e : emoticonsDraw) {
        e.isTrans = false;
      }
      break;
    case SECOND_MOTION:
      for (Emoticon e : emoticonsDraw) {
        for (int i = 0; i<4; i++) {
          e.isMani[i] = false;
        }
        e.isTrans = false;
      }
      break;
    case MOTION_CHECKING:
      break;
    case ADD_EXPRESSION:
      if (allExpressions == null) return;
      for (Expression ex : allExpressions) {
        ex.x = ex.initX;
        ex.y = ex.initY;
      }
      for (Emoticon e : emoticonsDraw) {
        e.isTrans = false;
      }
      break;
    case ADD_TEXT:
      for (Emoticon e : emoticonsDraw) {
        e.isTrans = false;
      }
      emoticonText.isTrans = false;
      break;
    case SAVE:
      break;
    case  SAVE_MOTION:
      break;
    default:
      break;
    }
  }

  public void keyPressed() {
    if (mode == Mode.ADD_TEXT || mode == Mode.ADD_TEXT_MOTION) {
      if (key == ENTER || key == RETURN) {
        hideVirtualKeyboard();
      } else if ((int) key == 65535 && keyCode == 67) {
        if (emoticonText.text.length()>1) {
          emoticonText.setText(emoticonText.text.substring(0, emoticonText.text.length()-1));
        } else {
          emoticonText.setText("");
        }
      } else {
        emoticonText.setText(emoticonText.text+key);
      }
    }
  }

  public void reset () {
    mode = Mode.START;
    stopButton = new RectButton(width/2, 1*9*height/10/3+height/10, PApplet.parseInt(width/2), width/9, color(232, 110, 107), "Stop Emoticon");
    motionButton = new RectButton(width/2, 2*9*height/10/3+height/10, PApplet.parseInt(width/2), width/9, color(232, 110, 107), "Motion Emoticon");
    saveButton = new RectButton(width/2, 2*9*height/10/3+height/10, PApplet.parseInt(width/2), width/9, color(232, 110, 107), "Save");
    arrowButton = new ArrowButton(9*width/10, height/20, height/30, height/15);
    eDisplay = new eDisplay(width/2, 13*height/40, width, height);
    cStorage = new cStorage();
    shadowB = new Shadow(0, height/10, width, height/100, color(0, 51), color(0, 0), 1);
    shadowCS = new Shadow(0, 11*height/20, width, height/100, color(0, 51), color(0, 0), 1);
    characterFactory = new CharacterFactory();
    characterFactory.loadXMLfile("characters.xml");
    allCharacters = characterFactory.getCharacters();
    expressionFactory = new ExpressionFactory();
    expressionFactory.loadXMLfile("expressions.xml");
    allExpressions = expressionFactory.getExpressions();
    emoticonFactory = new EmoticonFactory();
    emoticonFactory.loadXMLfile("emoticons.xml");
    allEmoticons = emoticonFactory.getEmoticons();
    emoticonsDraw = new ArrayList<Emoticon>();
    emoticonText = new Text();
    mask = loadImage("mask.png");
    pg = createGraphics(width, height, P2D);
    startEmoticonsDraw = new ArrayList<Emoticon>();
    endEmoticonsDraw = new ArrayList<Emoticon>();
    ani = new Animation(startEmoticonsDraw, endEmoticonsDraw);
  }
}

class Animation {
  ArrayList<Emoticon> startEmoticonsDraw, endEmoticonsDraw;
  int totalFrame;
  int frame;
  int flow;
  ArrayList<Emoticon> firstEmoticons;


  Animation (ArrayList<Emoticon> startEmoticonsDraw, ArrayList<Emoticon> endEmoticonsDraw) {
    this.startEmoticonsDraw = startEmoticonsDraw;
    this.endEmoticonsDraw = endEmoticonsDraw;
    totalFrame = 10;
    frame = 1;
    flow = 1;
    firstEmoticons = new ArrayList<Emoticon>();
  }

  public void draw() {
    for (int i =0; i<startEmoticonsDraw.size(); i++) {
      Emoticon e1 = startEmoticonsDraw.get(i);
      Emoticon e2 = endEmoticonsDraw.get(i);

      for (int j = 0; j<4; j++) {
        e1.isMani[j] = true;
        e1.isTrans = true;
      }
      Emoticon e3 = new Emoticon (e1);
      animate(e3, e2);
      e3.draw();
      e3.drawExpression();
    }
  }

  public void animate(Emoticon e1, Emoticon e2) {
    if (frame>0) {
      e1.transPivotPos = new PVector(0, 0);
      e1.translate((e2.x-e1.x)/totalFrame*frame, (e2.y-e1.y)/totalFrame*frame);
      e1.scale(1+(e2.ratio/e1.ratio-1)*frame/totalFrame);

      for (int i= 0; i<4; i++) {
        e1.maniPivotPos = new PVector(e1.endPivotPos.get(i).x+(e2.endPivotPos.get(i).x-e1.endPivotPos.get(i).x)/totalFrame*(frame-flow), 
          e1.endPivotPos.get(i).y+(e2.endPivotPos.get(i).y-e1.endPivotPos.get(i).y)/totalFrame*(frame-flow));
        if (e2.endPivotPos.get(i).x-e1.endPivotPos.get(i).x != 0 && e2.endPivotPos.get(i).y-e1.endPivotPos.get(i).y != 0) {
          e1.manipulateInverse(e1.endPivotPos.get(i).x+(e2.endPivotPos.get(i).x-e1.endPivotPos.get(i).x)/totalFrame*frame, 
            e1.endPivotPos.get(i).y+(e2.endPivotPos.get(i).y-e1.endPivotPos.get(i).y)/totalFrame*frame, i);
        }
      }
    }
    if (frame > totalFrame || frame < 1) {
      flow *= -1;
      frame += flow;
    } else {
      frame += flow;
    }
  }

  public void resetFrame() {
    this.frame = 0;
  }
}




class Text {
  private String text;
  private int x, y;
  private float ratio;
  private boolean isTrans;

  Text() {
    this.text = "";
    this.ratio = 14*min(width, height)/20/3/3;
    this.isTrans = false;
  }
  public void setText(String text) {
    this.text= text;
  }
  public void setTextPos(int mx, int my) {
    this.x = mx;
    this.y = my;
  }

  public void drawText() {
    textAlign(CENTER, CENTER);
    textFont(text_font, ratio);
    fill(0);
    text(text, x, y);
  }

  public void pgDrawText(PGraphics pg) {
    pg.textAlign(CENTER, CENTER);
    pg.textFont(text_font, ratio);
    pg.fill(0);
    pg.text(text, x, y);
  }

  public boolean isOver(int mx, int my) {
    if (mx<x-ratio/1.5f*text.length() || mx>x+ratio/1.5f*text.length()) return false;
    if (my<y-ratio/1.5f || my>y+ratio/1.5f) return false;
    return true;
  }
}

class EmoticonFactory {
  private ArrayList<Emoticon> emoticons;
  private XML xml;

  EmoticonFactory () {
    emoticons = new ArrayList<Emoticon>();
  }

  public void loadXMLfile(String fileName) {
    xml = loadXML(fileName);
    XML[] children = xml.getChildren("emoticon");
    for (int i =0; i < children.length; i++) {
      String name = children[i].getString("name");
      String filename = children[i].getString("filename");
      float w = children[i].getChild("width").getFloatContent();
      float h = children[i].getChild("height").getFloatContent();

      String[] posArray = {"lLegPivotPos", "rLegPivotPos", "lArmPivotPos", "rArmPivotPos", 
        "bodyPivotPos", "facePivotPos"};
      ArrayList<PVector> posPVectorArray = new ArrayList<PVector>();
      for (String pos : posArray) {
        float x = children[i].getChild(pos+"X").getFloatContent();
        float y = children[i].getChild(pos+"Y").getFloatContent();
        PVector posPVector = new PVector(x, y);
        posPVectorArray.add(posPVector);
      }
      emoticons.add(new Emoticon(name, filename, w, h, posPVectorArray));
    }
  }

  public ArrayList<Emoticon> getEmoticons() {
    return emoticons;
  }
}

class Emoticon {
  //lLegPivotPos, rLegPivotPos, lArmPivotPos, rArmPivotPos
  private ArrayList<PVector> startPivotPos;
  private ArrayList<PVector> middlePivotPos;
  private ArrayList<PVector> endPivotPos;
  private PVector bodyPivotPos;
  private PVector facePivotPos;
  private PVector maniPivotPos;
  private PVector transPivotPos;
  private float w, h;
  private int x, y;
  private String name;
  private float ratio;
  private String filename;
  private PImage img;
  private boolean[] isMani;
  private boolean isTrans;
  private String expFilename;
  private PImage expImg;


  Emoticon (String name, String filename, float w, float h, ArrayList<PVector> startPivotPos) {
    this.name = name; 
    this.filename = filename;
    this.img = loadImage(this.filename);
    this.w = w;
    this.h = h;
    this.ratio = 14*min(width, height)/20/3;
    this.x = width/2;
    this.y = 13*height/40;
    this.startPivotPos = new ArrayList<PVector>();
    for (int i=0; i<4; i++) {
      this.startPivotPos.add(new PVector(x+startPivotPos.get(i).x/w*ratio, y+startPivotPos.get(i).y/w*ratio));
    }
    this.bodyPivotPos = new PVector(x+startPivotPos.get(4).x/w*ratio, y+startPivotPos.get(4).y/w*ratio);
    this.facePivotPos = new PVector(x+startPivotPos.get(5).x/w*ratio, y+startPivotPos.get(5).y/w*ratio);
    this.middlePivotPos = new ArrayList<PVector>();
    this.middlePivotPos.add(new PVector(this.startPivotPos.get(0).x, this.startPivotPos.get(0).y+ratio/5));
    this.middlePivotPos.add(new PVector(this.startPivotPos.get(1).x, this.startPivotPos.get(1).y+ratio/5));
    this.middlePivotPos.add(new PVector(this.startPivotPos.get(2).x+ratio/5, this.startPivotPos.get(2).y+ratio/5));
    this.middlePivotPos.add(new PVector(this.startPivotPos.get(3).x-ratio/5, this.startPivotPos.get(3).y+ratio/5));
    this.endPivotPos = new ArrayList<PVector>();
    this.endPivotPos.add(new PVector(this.startPivotPos.get(0).x, this.startPivotPos.get(0).y+ratio/2.5f));
    this.endPivotPos.add(new PVector(this.startPivotPos.get(1).x, this.startPivotPos.get(1).y+ratio/2.5f));
    this.endPivotPos.add(new PVector(this.startPivotPos.get(2).x+ratio/2.5f, this.startPivotPos.get(2).y+ratio/2.5f));
    this.endPivotPos.add(new PVector(this.startPivotPos.get(3).x-ratio/2.5f, this.startPivotPos.get(3).y+ratio/2.5f));
    isMani = new boolean[4];
    for (int i = 0; i<4; i++) {
      isMani[i] = false;
    }
    isTrans = false;
  }

  Emoticon (Emoticon other) {
    this.startPivotPos = new ArrayList<PVector>(other.startPivotPos);
    this.middlePivotPos = new ArrayList<PVector>(other.middlePivotPos);
    this.endPivotPos = new ArrayList<PVector>(other.endPivotPos);
    this.bodyPivotPos = new PVector(other.bodyPivotPos.x, other.bodyPivotPos.y);
    this.facePivotPos = new PVector(other.facePivotPos.x, other.facePivotPos.y);
    this.w = other.w;
    this.h = other.h;
    this.x = other.x;
    this.y = other.y;
    this.name = other.name;
    this.ratio = other.ratio;
    this.filename = other.filename;
    this.img = loadImage(filename);
    this.isMani = other.isMani;
    this.isTrans = other.isTrans;
    this.expFilename = other.expFilename;
    this.expImg = loadImage(expFilename);
  }

  public void draw() {
    imageMode(CENTER);
    image(img, x, y, ratio, h/w*ratio);
    for (int i=0; i<4; i++) {
      noFill();
      stroke(0);
      strokeWeight(4);
      beginShape();
      vertex(startPivotPos.get(i).x, startPivotPos.get(i).y);
      quadraticVertex(middlePivotPos.get(i).x, middlePivotPos.get(i).y, endPivotPos.get(i).x, endPivotPos.get(i).y);
      endShape();  
      fill(0);
      ellipse(endPivotPos.get(i).x, endPivotPos.get(i).y, ratio/15, ratio/15);
    }
  }

  public void pgDraw(PGraphics pg) {
    pg.imageMode(CENTER);
    pg.image(img, x, y, ratio, h/w*ratio);
    for (int i=0; i<4; i++) {
      pg.noFill();
      pg.stroke(0);
      pg.strokeWeight(4);
      pg.beginShape();
      pg.vertex(startPivotPos.get(i).x, startPivotPos.get(i).y);
      pg.quadraticVertex(middlePivotPos.get(i).x, middlePivotPos.get(i).y, endPivotPos.get(i).x, endPivotPos.get(i).y);
      pg.endShape();  
      pg.fill(0);
      pg.ellipse(endPivotPos.get(i).x, endPivotPos.get(i).y, ratio/15, ratio/15);
    }
  }

  public void setExpression(String filename) {
    this.expFilename = filename;
    this.expImg = loadImage(filename);
  }

  public void drawExpression() {
    imageMode(CENTER);
    image(expImg, facePivotPos.x, facePivotPos.y, ratio/3, h/w*ratio/3);
  }

  public void pgDrawExpression(PGraphics pg) {
    pg.imageMode(CENTER);
    pg.image(expImg, facePivotPos.x, facePivotPos.y, ratio/3, h/w*ratio/3);
  }

  public void manipulate(float mx, float my, int i) {
    float dx = mx-maniPivotPos.x;
    float dy = my-maniPivotPos.y;

    float distance = sqrt(dx*dx+dy*dy);

    float a= ratio/5;
    float b = ratio/5;
    float c = min(distance, a+b);

    float b1 = (b*b-a*a-c*c)/(-2*a*c);
    float c1 = (c*c-a*a-b*b)/(-2*a*b);

    if (b1>1.0f) {
      b1=1.0f;
    } else if (b1<-1.0f) {
      b1=-1.0f;
    }

    if (c1>1.0f) {
      c1=1.0f;
    } else if (c1<-1.0f) {
      c1=-1.0f;
    }

    float B = (1-2*(i%2))*acos(b1);
    float C = (1-2*(i%2))*acos(c1);
    float D = atan2(dy, dx);
    float E = D+B+C+PI;

    float ex = (cos(E) * a) + startPivotPos.get(i).x;
    float ey = (sin(E) * a) + startPivotPos.get(i).y;
    //print("UpperArm Angle=  "+degrees(E)+"    ");

    float hx = (cos(D+B) * b) + ex;
    float hy = (sin(D+B) * b) + ey;
    //println("LowerArm Angle=  "+degrees((D+B)));

    middlePivotPos.set(i, new PVector(ex, ey));
    endPivotPos.set(i, new PVector(hx, hy));
  }

  public void manipulateInverse(float hx, float hy, int i) {
    float dx = hx-maniPivotPos.x;
    float dy = hy-maniPivotPos.y;

    float distance = sqrt(dx*dx+dy*dy);

    float a= ratio/5;
    float b = ratio/5;
    float c = min(distance, a+b);

    float b1 = (b*b-a*a-c*c)/(-2*a*c);
    float c1 = (c*c-a*a-b*b)/(-2*a*b);

    if (b1>1.0f) {
      b1=1.0f;
    } else if (b1<-1.0f) {
      b1=-1.0f;
    }

    if (c1>1.0f) {
      c1=1.0f;
    } else if (c1<-1.0f) {
      c1=-1.0f;
    }

    float B = (1-2*(i%2))*acos(b1);
    float C = (1-2*(i%2))*acos(c1);
    float D = atan2(dy, dx);
    float E = D+B+C+PI;

    float ex = (cos(E) * a) + startPivotPos.get(i).x;
    float ey = (sin(E) * a) + startPivotPos.get(i).y;
    //print("UpperArm Angle=  "+degrees(E)+"    ");

    middlePivotPos.set(i, new PVector(ex, ey));
    endPivotPos.set(i, new PVector(hx, hy));
  }

  public void scale(float s) {
    this.ratio = this.ratio*s;

    facePivotPos = new PVector((facePivotPos.x-x)*s+x, (facePivotPos.y-y)*s+y);

    for (int i=0; i<4; i++) {
      this.startPivotPos.set(i, new PVector((startPivotPos.get(i).x-x)*s+x, (startPivotPos.get(i).y-y)*s+y));
    }

    this.middlePivotPos.set(0, new PVector((this.middlePivotPos.get(0).x-x)*s+x, (this.middlePivotPos.get(0).y-y)*s+y));
    this.middlePivotPos.set(1, new PVector((this.middlePivotPos.get(1).x-x)*s+x, (this.middlePivotPos.get(1).y-y)*s+y));
    this.middlePivotPos.set(2, new PVector((this.middlePivotPos.get(2).x-x)*s+x, (this.middlePivotPos.get(2).y-y)*s+y));
    this.middlePivotPos.set(3, new PVector((this.middlePivotPos.get(3).x-x)*s+x, (this.middlePivotPos.get(3).y-y)*s+y));

    this.endPivotPos.set(0, new PVector((this.endPivotPos.get(0).x-x)*s+x, (this.endPivotPos.get(0).y-y)*s+y));
    this.endPivotPos.set(1, new PVector((this.endPivotPos.get(1).x-x)*s+x, (this.endPivotPos.get(1).y-y)*s+y));
    this.endPivotPos.set(2, new PVector((this.endPivotPos.get(2).x-x)*s+x, (this.endPivotPos.get(2).y-y)*s+y));
    this.endPivotPos.set(3, new PVector((this.endPivotPos.get(3).x-x)*s+x, (this.endPivotPos.get(3).y-y)*s+y));
  }

  public void translate(int mx, int my) {
    int dx = PApplet.parseInt(mx-transPivotPos.x);
    int dy = PApplet.parseInt(my-transPivotPos.y);

    this.x = this.x+dx;
    this.y = this.y+dy;

    facePivotPos = new PVector(facePivotPos.x+dx, facePivotPos.y+dy);

    for (int i=0; i<4; i++) {
      this.startPivotPos.set(i, new PVector(startPivotPos.get(i).x+dx, startPivotPos.get(i).y+dy));
    }

    this.middlePivotPos.set(0, new PVector(this.middlePivotPos.get(0).x+dx, this.middlePivotPos.get(0).y+dy));
    this.middlePivotPos.set(1, new PVector(this.middlePivotPos.get(1).x+dx, this.middlePivotPos.get(1).y+dy));
    this.middlePivotPos.set(2, new PVector(this.middlePivotPos.get(2).x+dx, this.middlePivotPos.get(2).y+dy));
    this.middlePivotPos.set(3, new PVector(this.middlePivotPos.get(3).x+dx, this.middlePivotPos.get(3).y+dy));

    this.endPivotPos.set(0, new PVector(this.endPivotPos.get(0).x+dx, this.endPivotPos.get(0).y+dy));
    this.endPivotPos.set(1, new PVector(this.endPivotPos.get(1).x+dx, this.endPivotPos.get(1).y+dy));
    this.endPivotPos.set(2, new PVector(this.endPivotPos.get(2).x+dx, this.endPivotPos.get(2).y+dy));
    this.endPivotPos.set(3, new PVector(this.endPivotPos.get(3).x+dx, this.endPivotPos.get(3).y+dy));
  }

  public boolean isOverPivot(int mx, int my, int i) {
    if (mx<endPivotPos.get(i).x-14*min(width, height)/20/3/5 || mx>endPivotPos.get(i).x+14*min(width, height)/20/3/5) return false;
    if (my<endPivotPos.get(i).y-14*min(width, height)/20/3/5 || my>endPivotPos.get(i).y+14*min(width, height)/20/3/5) return false;
    return true;
  }

  public boolean isOver(int mx, int my) {
    if (mx<x-ratio/2 || mx>x+ratio/2) return false;
    if (my<y-h/w*ratio/2 || my>y+h/w*ratio/2) return false;
    return true;
  }
}

class eDisplay {
  Emoticon emoticon;
  int w, h; 
  int left, right, top, bottom;
  int x, y;


  eDisplay(int x, int y, int w, int h) {
    this.w = 17*w/20;
    this.h = 3*h/8;
    this.x = x;
    this.y = y;
  }

  public void draw() {
    noStroke();
    fill(255);
    rectMode(CENTER);
    rect(x, y, w, h, w/20);
  }

  public void pgDraw(PGraphics pg) {
    pg.noStroke();
    pg.fill(255);
    pg.rectMode(CENTER);
    pg.rect(x, y, w, h, w/20);
  }

  public boolean isOver(int mx, int my) {
    if (mx<x-w/2 || mx>x+w/2) return false;
    if (my<y-h/2 || my>y+h/2) return false;
    return true;
  }
}

class ExpressionFactory {
  private ArrayList<Expression> expressions;
  private XML xml;

  ExpressionFactory () {
    expressions = new ArrayList<Expression>();
  }

  public void loadXMLfile(String fileName) {
    xml = loadXML(fileName);
    XML[] children = xml.getChildren("expression");
    for (int i =0; i < children.length; i++) {
      String name = children[i].getString("name");
      String filename = children[i].getString("filename");
      float w = children[i].getChild("width").getFloatContent();
      float h = children[i].getChild("height").getFloatContent();

      expressions.add(new Expression(name, filename, w, h, 
        (min(width, height)-3*min(width, height)/20)/3*(i%3)+3*width/40+14*width/20/3/2, 
        7*height/10+PApplet.parseInt((i%6)/3)*3*height/20, 14*min(width, height)/20/3));
    }
  }

  public ArrayList<Expression> getExpressions() {
    return expressions;
  }
}

class Expression {
  private String name;
  private String filename;
  private PImage img;
  private float w, h;
  private int x, y;
  private int initX, initY;
  private float ratio;

  Expression (String name, String filename, float w, float h, int x, int y, float ratio) {
    this.name = name; 
    this.filename = filename;
    this.img = loadImage(filename);
    this.w = w;
    this.h = h;
    this.x = x;
    this.initX = x;
    this.y = y;
    this.initY = y;
    this.ratio = ratio;
  }

  public void draw() {
    imageMode(CENTER);
    image(img, x, y, ratio, h/w*ratio);
  }

  public boolean isOver(int mx, int my) {
    if (mx<x-ratio/2 || mx>x+ratio/2) return false;
    if (my<y-h/w*ratio/2 || my>y+h/w*ratio/2) return false;
    return true;
  }
}

class CharacterFactory {
  private ArrayList<Character> characters;
  private XML xml;

  CharacterFactory () {
    characters = new ArrayList<Character>();
  }

  public void loadXMLfile(String fileName) {
    xml = loadXML(fileName);
    XML[] children = xml.getChildren("character");
    for (int i =0; i < children.length; i++) {
      String name = children[i].getString("name");
      String filename = children[i].getString("filename");
      float w = children[i].getChild("width").getFloatContent();
      float h = children[i].getChild("height").getFloatContent();

      characters.add(new Character(name, filename, w, h, 
        (min(width, height)-3*min(width, height)/20)/3*(i%3)+3*width/40+14*width/20/3/2, 
        7*height/10+PApplet.parseInt((i%6)/3)*3*height/20, 14*min(width, height)/20/3));
    }
  }

  public ArrayList<Character> getCharacters() {
    return characters;
  }
}

class Character {
  private String name;
  private String filename;
  private PImage img;
  private float w, h;
  private int x, y;
  private int initX, initY;
  private float ratio;

  Character(String name, String filename, float w, float h, int x, int y, float ratio) {
    this.name = name; 
    this.filename = filename;
    this.img = loadImage(filename);
    this.w = w;
    this.h = h;
    this.x = x;
    this.initX = x;
    this.y = y;
    this.initY = y;
    this.ratio = ratio;
  }

  public void draw() {
    imageMode(CENTER);
    image(img, x, y, ratio, h/w*ratio);
  }

  public boolean isOver(int mx, int my) {
    if (mx<x-ratio/2 || mx>x+ratio/2) return false;
    if (my<y-h/w*ratio/2 || my>y+h/w*ratio/2) return false;
    return true;
  }
}

class cStorage {
  cStorage() {
  }

  public void draw() {
    noStroke();
    fill(232, 110, 107);
    rectMode(CORNER);
    rect(0, 11*height/20, width, 9*height/20);
  }
}

abstract class Button {
  protected int x, y;
  protected int col;


  Button (int x, int y, int col) {
    this.x = x;
    this.y = y;
    this.col = col;
  }

  Button (int x, int y) {
    this.x = x;
    this.y = y;
  }

  public abstract void draw();
  public abstract boolean isOver(int mx, int my);
}

class RectButton extends Button {
  private int w, h;
  private String name;

  RectButton(int x, int y, int w, int h, int col, String name) {
    super(x, y, col);
    this.w = w;
    this.h = h;
    this.name = name;
  }

  public void draw() {
    noStroke();
    fill(super.col);
    rectMode(CENTER);
    rect(super.x, super.y, w, h, w/20);
    textAlign(CENTER, CENTER);
    textFont(inter_font, h/2);
    fill(255);
    text(name, x, y);
  }

  public boolean isOver(int mx, int my) {
    if (mx<super.x-w/2 || mx>super.x+w/2) return false;
    if (my<super.y-h/2 || my>super.y+h/2) return false;
    return true;
  }
}

class ArrowButton extends Button {
  private int w, h;
  private String filename;
  private PImage arrow;

  ArrowButton(int x, int y, int w, int h) {
    super(x, y);
    this.w = w;
    this.h = h;
    this.filename = "arrow.png";
    this.arrow = loadImage(filename);
  }

  public void draw() {
    imageMode(CENTER);
    image(arrow, x, y, w, h);
  }

  public boolean isOver(int mx, int my) {
    if (mx<super.x-w/2 || mx>super.x+w/2) return false;
    if (my<super.y-h/2 || my>super.y+h/2) return false;
    return true;
  }
}

class Shadow {
  int Y_AXIS = 1;
  int X_AXIS = 2;
  int x, y;
  float w, h;
  int c1, c2;
  int axis;

  Shadow (int x, int y, float w, float h, int c1, int c2, int axis ) {
    this.x = x;
    this.y = y;
    this.w = w;
    this.h = h;
    this.c1 = c1;
    this.c2 = c2;
    this.axis = axis;
  }

  public void draw() {
    noFill();
    if (axis == Y_AXIS) {  // Top to bottom gradient
      for (int i = y; i <= y+h-1; i++) {
        float inter = map(i, y, y+h, 0, 1);
        int c = lerpColor(c1, c2, inter);
        stroke(c);
        line(x, i, x+w, i);
      }
    } else if (axis == X_AXIS) {  // Left to right gradient
      for (int i = x; i <= x+w-1; i++) {
        float inter = map(i, x, x+w, 0, 1);
        int c = lerpColor(c1, c2, inter);
        stroke(c);
        line(i, y, i, y+h);
      }
    }
  }
}

public boolean surfaceTouchEvent(MotionEvent event) {
  super.surfaceTouchEvent(event);
  return gesture.surfaceTouchEvent(event);
}

public void onPinch(float x, float y, float d)
{  
  if (emoji.mode != Mode.START && emoji.mode != Mode.ADD_TEXT && emoji.mode != Mode.SAVE) {
    if (d>10) { 
      for (Emoticon e : emoji.emoticonsDraw) {
        if (e.isOver(PApplet.parseInt(x), PApplet.parseInt(y))) {
          e.isTrans = false;
          float scale = map(d, 0, width, 1, 2);
          e.scale(scale);
        }
      }
    } else if (d<=-10) {
      for (Emoticon e : emoji.emoticonsDraw) {
        if (e.isOver(PApplet.parseInt(x), PApplet.parseInt(y))) {
          e.isTrans = false;
          float scale = map(d, -width, 0, 0, 1);
          e.scale(scale);
        }
      }
    }
  }
}

public void showVirtualKeyboard()
{
  InputMethodManager imm = (InputMethodManager) getActivity().getSystemService(Context.INPUT_METHOD_SERVICE);
  imm.toggleSoftInput(InputMethodManager.SHOW_FORCED, 0);
}

public void hideVirtualKeyboard()
{
  InputMethodManager imm = (InputMethodManager) getActivity().getSystemService(Context.INPUT_METHOD_SERVICE);
  imm.toggleSoftInput(InputMethodManager.HIDE_IMPLICIT_ONLY, 0);
}
  public void settings() {  fullScreen(P2D); }
  static public void main(String[] passedArgs) {
    String[] appletArgs = new String[] { "final_Emoji" };
    if (passedArgs != null) {
      PApplet.main(concat(appletArgs, passedArgs));
    } else {
      PApplet.main(appletArgs);
    }
  }
}
