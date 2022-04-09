#include "lib.c"



typedef struct LL {
    int head;
    struct LL * next;
} LL;

void main(void){
    ref_hash_table = (RefHashTableItem*) malloc(sizeof(RefHashTableItem)* ref_hash_table_size);

    for (unsigned long long i=0;i<ref_hash_table_size;i++){
        ref_hash_table[i].is_used = false;
    }/*

    RefHashTableItem a;
    a.is_marked = false;
    a.is_used = false;
    a.refer_address = 20000;
    a.refee_address = 0;

    RefHashTableItem b;
    b.is_marked = false;
    b.is_used = false;
    b.refer_address = 20000;
    b.refee_address = 8964;
    
    RefHashTableItem c;
    c.is_marked = false;
    c.is_used = false;
    c.refer_address = 20001;
    c.refee_address = 20000;

    RefHashTableItem d;
    d.is_marked = false;
    d.is_used = false;
    d.refer_address = 1895;
    d.refee_address = 0;

    ref_hash_table = (RefHashTableItem*) add_item(ref_hash_table, a);
    ref_hash_table = (RefHashTableItem*) add_item(ref_hash_table, b);
    ref_hash_table = (RefHashTableItem*) delete_items(ref_hash_table, b);
    ref_hash_table = (RefHashTableItem*) add_item(ref_hash_table, c);
    ref_hash_table = (RefHashTableItem*) add_item(ref_hash_table, d);
    ref_hash_table = (RefHashTableItem*) add_item(ref_hash_table, b); */

    LL * a = malloc(sizeof(LL));
    a->next = 0;
    a->head = 12;
    RefHashTableItem a_;
    a_.is_used = true;
    a_.is_marked = UNMARKED;
    a_.refer_address = (long long unsigned) a;
    a_.refee_address = (long long unsigned) a->next;

    ref_hash_table = (RefHashTableItem*) add_item(ref_hash_table, a_);

    LL * b = malloc(sizeof(LL));
    b->next = 0;
    b->head = 88;
    RefHashTableItem b_;
    b_.is_used = true;
    b_.is_marked = UNMARKED;
    b_.refer_address = (long long unsigned) b;
    b_.refee_address = (long long unsigned) b->next;

    ref_hash_table = (RefHashTableItem*) add_item(ref_hash_table, b_);

    LL * c = malloc(sizeof(LL));
    c->next = b;
    c->head = 99;

    RefHashTableItem c1_;
    c1_.is_used = true;
    c1_.is_marked = UNMARKED;
    c1_.refer_address = (long long unsigned) c;
    c1_.refee_address = (long long unsigned) c->next;

    RefHashTableItem c2_;
    c2_.is_used = true;
    c2_.is_marked = UNMARKED;
    c2_.refer_address = (long long unsigned) c;
    c2_.refee_address = 0;

    ref_hash_table = (RefHashTableItem*) add_item(ref_hash_table, c1_);
    ref_hash_table = (RefHashTableItem*) add_item(ref_hash_table, c2_);

    ref_hash_table = (RefHashTableItem*) unreachize_item(ref_hash_table, b_.refer_address);

    ref_hash_table = (RefHashTableItem*) unreachize_item(ref_hash_table, a_.refer_address);

    ref_hash_table = mark(ref_hash_table);
    ref_hash_table = sweep(ref_hash_table);
    


}