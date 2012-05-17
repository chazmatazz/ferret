
#ifndef H_FERRET
#define H_FERRET

#ifdef __AVR__
# define AVR_GCC TRUE
#else
# define GNU_GCC TRUE
#endif

#include <stdlib.h>
#include <stdio.h>

#ifdef GNU_GCC
#include <iostream>
#include <sstream>
#endif

#ifdef AVR_GCC
#include "Arduino.h"
#endif

//
// Compiler Specific
//

#ifdef AVR_GCC
extern "C" void __cxa_pure_virtual(void);
void __cxa_pure_virtual(void) {};

static FILE uartout = {0};

static int uart_putchar (char c, FILE *stream){
  Serial.write(c);
  return 0 ;
}

#define OUTPUT_STREAM &uartout

#define INIT_ENV                                                        \
  init();                                                               \
  Serial.begin(9600);                                                   \
  fdev_setup_stream (&uartout, uart_putchar, NULL, _FDEV_SETUP_WRITE);  \

#endif

#ifdef GNU_GCC
#define OUTPUT_STREAM stdout
#define INIT_ENV
#endif


#define VAR ferret::var

//
// Casting
//

#define OBJECT(v) static_cast<ferret::Object*>(v.get())
#define POINTER(v) static_cast<ferret::Pointer*>(v.get())
#define INTEGER(v) static_cast<ferret::Integer*>(v.get())
#define FLOAT(v) static_cast<ferret::Float*>(v.get())
#define BOOLEAN(v) static_cast<ferret::Boolean*>(v.get())
#define KEYWORD(v) static_cast<ferret::Keyword*>(v.get())
#define CHARACTER(v) static_cast<ferret::Character*>(v.get())
#define SEQUENCE(v) static_cast<ferret::Sequence*>(v.get())
#define CELL(v) static_cast<ferret::Cell*>(v.get())
#define LAMBDA(v) static_cast<ferret::Lambda*>(v.get())

#define GETFLOAT(arg) (OBJECT(arg)->getType() == INTEGER_TYPE ? INTEGER(arg)->floatValue() : FLOAT(arg)->floatValue())

//
// Function Invocation
//

#define VA_ARGS(...) , ##__VA_ARGS__
#define INVOKE(f,...) LAMBDA(f)->invoke((ferret::var(new ferret::Sequence()) VA_ARGS(__VA_ARGS__)))
#define FN(f,...) ferret::var(new ferret::f(__VA_ARGS__))

namespace ferret{

  //
  // Objects
  //

  class var;

  enum TYPE {CONS_TYPE, LIST_TYPE, LAMBDA_TYPE, BOOLEAN_TYPE, KEYWORD_TYPE,
             POINTER_TYPE, INTEGER_TYPE, FLOAT_TYPE, CHARACTER_TYPE};

class Object{
    public:
      Object() : refCount(0) {}
      virtual ~Object() {};

      virtual int getType() = 0;
      virtual var toOutputStream() = 0;
      virtual var equals(var o) = 0;

      void addRef() { refCount++; }
      bool subRef() { return (--refCount <= 0); }


      void* operator new(size_t size){ 
        return malloc(size); 
      } 

      void  operator delete(void * ptr){ 
        free(ptr); 
      }

      void* operator new[](size_t size){ 
        return malloc(size); 
      }

      void  operator delete[](void * ptr){ 
        free(ptr); 
      }

    private:
      int refCount;
    };
    
    class var{
public:
  var(Object* ptr=0) : m_ptr(ptr) { addRef(); }

  var(const var& p) : m_ptr(p.m_ptr) { addRef(); }

  ~var() { subRef(); }

  var& operator= (const var& p){
    return *this = p.m_ptr;
  }

  var& operator= (Object* ptr){
    if (m_ptr != ptr){
      subRef();
      m_ptr=ptr;
      addRef();
    }
    return *this;
  }

  var(int i);
  var(float f);
  var(bool b);
  var(char b);

  var& operator, (const var& m);
  var toOutputStream() {
    if (m_ptr != NULL )
      m_ptr->toOutputStream();
    else
      fprintf(OUTPUT_STREAM, "nil");
  }

  Object* get() { return m_ptr; }

private:
  void addRef(){
    // Only change if non-null
    if (m_ptr) m_ptr->addRef();
  }

  void subRef(){
    // Only change if non-null
    if (m_ptr){
      // Subtract and test if this was the last pointer.
      if (m_ptr->subRef()){
        delete m_ptr;
        m_ptr=0;
      }
    }
  }

  Object* m_ptr;
};

  class Pointer : public Object {
  public:
    void* ptr;
    Pointer(void* p){ptr = p;}

    int getType(){ return POINTER_TYPE;}
    var equals(var o){ return ptr = POINTER(o)->ptr; }

    var toOutputStream(){
      fprintf(OUTPUT_STREAM, "Pointer");
      return var();
    }
  };
  
  class Boolean : public Object { 
public:
  Boolean(bool b){value = b;}
  int getType(){ return BOOLEAN_TYPE;}

  bool asBool() { return value; }

  var equals(var o){
    if (OBJECT(o)->getType() != BOOLEAN_TYPE)
      return false;

    return (value == BOOLEAN(o)->asBool());
  }

  var toOutputStream(){ 
    if (value)
      fprintf(OUTPUT_STREAM, "true"); 
    else
      fprintf(OUTPUT_STREAM, "false"); 

    return var();
  }
private:
  bool value;
};


class Lambda : public Object{ 
public:
  virtual var invoke(var args) = 0;
};

  class Integer : public Object{
  public:
    Integer(int x){value = x;}
    int getType(){ return INTEGER_TYPE;}
    var toOutputStream(){ fprintf(OUTPUT_STREAM, "%d", value); return var();};

    int intValue(){
      return value;
    }

    float floatValue(){
      return (float)value;
    }

    var equals(var o);

  private:
    int value;
  };

  class Float : public Object{
  public:
    Float(float x){value = x;}
    int getType(){ return FLOAT_TYPE;}
    var toOutputStream(){ fprintf(OUTPUT_STREAM, "%f", value); return var();};

    int intValue(){
      return (int)value;
    }

    float floatValue(){
      return value;
    }

    var equals(var o){

      switch(OBJECT(o)->getType()) {
      case INTEGER_TYPE:
        return (value == INTEGER(o)->floatValue());
      case FLOAT_TYPE:
        return (value == FLOAT(o)->floatValue());
      }

      return false;
    }
  private:
    float value;
  };

  var Integer::equals(var o){

    switch(OBJECT(o)->getType()) {
    case INTEGER_TYPE:
      return (value == INTEGER(o)->intValue());
    case FLOAT_TYPE:
      return (value == FLOAT(o)->intValue());
    }

    return false;
  }


  class Keyword : public Object {
  public:
    int id;

    Keyword(int b){id = b;}
    int getType(){ return KEYWORD_TYPE;}

    var equals(var o){
      if (OBJECT(o)->getType() != KEYWORD_TYPE)
        return false;

      return (id == KEYWORD(o)->id);
    }

    var toOutputStream(){ fprintf(OUTPUT_STREAM, "%d", id); return var();};
  };

  class Character : public Object {
  public:
    char value;
    Character(char c){value = c;}
    int getType(){ return CHARACTER_TYPE;}

    var equals(var o){
      if (OBJECT(o)->getType() != CHARACTER_TYPE)
        return false;

      return (value == CHARACTER(o)->value);
    }

    var toOutputStream(){
      fprintf(OUTPUT_STREAM, "%c",value);
      return var();
    }
  };


  class Cell : public Object{
  public:
    var data;
    var next;

    var equals(var o){
      if (OBJECT(o)->getType() != CONS_TYPE)
        return false;

      return OBJECT(data)->equals(o);
    }

    int getType(){ return CONS_TYPE;}
    var toOutputStream(){ OBJECT(data)->toOutputStream(); return var();};
  };

  class Sequence : public Object{
    var head;
  public:
    Sequence(){
      head = NULL;
    }

    Sequence(var h){
      head = h;
    }

    void cons(var x){
      var v = var(new Cell());
      CELL(v)->data = x;
      CELL(v)->next = head;
      head = v;
    }

    var first(){
      if (head.get() == NULL )
        return var();
      else
        return CELL(head)->data;
    }

    var rest(){
      if ( head.get() == NULL || CELL(head)->next.get() == NULL )
        return var(new Sequence());
      else
        return var(new Sequence(CELL(head)->next));
    }

    var nth(var i){
      var it = head;
      int index = INTEGER(i)->intValue();

      for(int i = 0 ; i < index; i++){
        if ((CELL(it)->next).get() == NULL )
          return VAR();

        it = CELL(it)->next;
      }

      return CELL(it)->data;
    }

    bool isEmpty(){
      if (head.get() == NULL)
        return true;

      return false;
    }

    var toOutputStream(){
      fprintf(OUTPUT_STREAM, "( ");

      for(var it = head; it.get() != NULL ; it = CELL(it)->next){
        OBJECT(CELL(it)->data)->toOutputStream();
        fprintf(OUTPUT_STREAM, " ");
      }

      fprintf(OUTPUT_STREAM, ")");
      return var();
    }

    var equals(var o){

      if (OBJECT(o)->getType() != LIST_TYPE)
        return false;

      var itOther = o;
      for(var it = this; !SEQUENCE(it)->isEmpty(); it = SEQUENCE(it)->rest()){
        if (SEQUENCE(itOther)->isEmpty() ||
            BOOLEAN(OBJECT(SEQUENCE(it)->first())->equals(SEQUENCE(itOther)->first()))->asBool() == false)
          return false;

        itOther = SEQUENCE(itOther)->rest();
      }

      return true;
    }

    var clone() { return var(new Sequence(head));}
    int getType(){ return LIST_TYPE;}

    var reduce(var f){
      var acc = INVOKE(f,CELL(head)->data,CELL(CELL(head)->next)->data);

      for(var it = CELL(CELL(head)->next)->next; it.get() != NULL ; it = CELL(it)->next)
        acc = INVOKE(f, CELL(it)->data, acc);

      return acc;
    }

    var reduce(var f, var acc){
      for(var it = head; it.get() != NULL ; it = CELL(it)->next)
        acc = INVOKE(f, CELL(it)->data, acc);

      return acc;
    }
  };

  var::var(int i){
    m_ptr = new Integer(i);
    addRef();
  }

  var::var(float f){
    m_ptr = new Float(f);
    addRef();
  }

  var::var(bool b){
    m_ptr = new Boolean(b);
    addRef();
  }

  var::var(char b){
    m_ptr = new Character(b);
    addRef();
  }

  var& var::operator, (const var& m){
    static_cast<Sequence*>(m_ptr)->cons(m);
    return *this;
  }

#ifdef GNU_GCC
  std::string toCppString(var s){
    std::stringstream ss;

    for(var it = s; !SEQUENCE(it)->isEmpty(); it = SEQUENCE(it)->rest())
      ss << CHARACTER(SEQUENCE(it)->first())->value;

    return ss.str();
  }
#endif
}
#endif
