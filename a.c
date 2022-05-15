#include <stdio.h>
#include <stdlib.h>

typedef struct Object Object;
typedef Object* (Func)  (Object**, Object**);
typedef struct Clos {Func* func; Object** free_vars;} Clos;


typedef union ObjCont
{
    int i;
    float f;
    Object * obj_ptr;
    Clos clos;
}
ObjCont;

typedef struct Object
{
    char *type;
    ObjCont cont;
}
Object;

Object* add(Object** args, Object** free_vars){
    Object *result = malloc(sizeof(Object));
    (result->type) = "int";
    (result->cont).i = (args[0]->cont).i + (args[1]->cont).i;  
    printf("%d", (result->cont).i);
    return result;
  }

int main(void)
{
    Object *add_b78e8kc8 = malloc(sizeof(Object));
    (add_b78e8kc8->type) = "closure";
    (add_b78e8kc8->cont).clos.func = add;
    (add_b78e8kc8->cont).clos.free_vars = 0;
  
    Object *sym0 = malloc(sizeof(Object));
    (sym0->type) = "int";
    (sym0->cont).i = 12;
    Object *sym1 = malloc(sizeof(Object));
    (sym1->type) = "int";
    (sym1->cont).i = 7;
    
    Object* sym2[2];
    sym2[0] = sym0;
    sym2[1] = sym1;
    add(sym2, 0);
    return 0;
}