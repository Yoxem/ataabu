#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

typedef struct LL LL;

typedef enum {MARKED, UNMARKED, UNREACHABLE} MarkingStatus;

typedef struct{
    unsigned long long refer_address;
    unsigned long long refee_address;
    MarkingStatus is_marked;
    bool is_used; 

} RefHashTableItem;

unsigned long long ref_hash_table_size = 2;
unsigned long long number_of_item = 0;

RefHashTableItem *ref_hash_table;

RefHashTableItem* add_item(RefHashTableItem* table, RefHashTableItem item);
RefHashTableItem* delete_items(RefHashTableItem* table, RefHashTableItem item);

void mark_specific(RefHashTableItem* hash_table, unsigned long long addr);

RefHashTableItem* mark(RefHashTableItem* hash_table){
    for (unsigned long long i =0; i<ref_hash_table_size; i++){
        if (hash_table[i].is_used == true && hash_table[i].refee_address == 0 &&  hash_table[i].is_marked != UNREACHABLE){
            hash_table[i].is_marked = MARKED;
            unsigned long long address = hash_table[i].refer_address;
            mark_specific(hash_table, address);
        }
    }

    return hash_table;
}

void mark_specific(RefHashTableItem* hash_table, unsigned long long addr){
    for (unsigned long long i =0; i<ref_hash_table_size; i++){
        if (hash_table[i].refer_address == addr){
            if (hash_table[i].refee_address == 0){
                hash_table[i].is_marked = MARKED;
            }else{
                mark_specific(hash_table, hash_table[i].refee_address);
            }
        }
    }

}

RefHashTableItem* sweep(RefHashTableItem* hash_table){
    for (unsigned long long i =0; i<ref_hash_table_size; i++){
        if (hash_table[i].is_marked == UNREACHABLE || hash_table[i].is_marked == UNMARKED){
            

            if (hash_table[i].refee_address == 0){
                free((void*)(hash_table[i].refer_address));
                hash_table = delete_items(hash_table, hash_table[i]);
            }
        }
    }

    for (unsigned long long i=0; i<ref_hash_table_size; i++){
        if ((hash_table[i].is_marked) != UNMARKED){
        hash_table[i].is_marked = UNMARKED;
        }
    }

    return hash_table;
}



RefHashTableItem* resize_ref_table(RefHashTableItem* hash_table){
    ref_hash_table_size = ref_hash_table_size * 2;
    RefHashTableItem* new_hash_table = malloc(sizeof (RefHashTableItem)* ref_hash_table_size);
    number_of_item = 0;

    for (unsigned long long i=0;i<ref_hash_table_size;i++){
        new_hash_table[i].is_used = false;
    }

    for (unsigned long long i=0 ; i < (ref_hash_table_size / 2); i++){
        if(hash_table[i].is_used == true){
            add_item(new_hash_table, hash_table[i]);
        }

    }

    RefHashTableItem* old_hash_table = hash_table;
    hash_table = new_hash_table;
    free(old_hash_table);
    return hash_table;
}

RefHashTableItem* add_item(RefHashTableItem* table, RefHashTableItem item){
    if (number_of_item >= ref_hash_table_size){
        table = resize_ref_table(table);
    }

    unsigned long long main_addr = item.refer_address;
    unsigned long long key = main_addr % ref_hash_table_size;
    while (table[key].is_used  == true){
        key = (key + 1) % ref_hash_table_size;
    }

    table[key].refer_address = item.refer_address;
    table[key].refee_address = item.refee_address;
    table[key].is_marked = item.is_marked;
    table[key].is_used = true;

    number_of_item += 1;

    return table;
}

RefHashTableItem* unreachize_item(RefHashTableItem* table, unsigned long long addr){

    unsigned long long key = addr % ref_hash_table_size;
    while (table[key].refer_address  != addr || table[key].refee_address != 0){
        key = (key + 1) % ref_hash_table_size;
    }

    table[key].is_marked = UNREACHABLE ;

    return table;
}

RefHashTableItem* delete_items(RefHashTableItem* table, RefHashTableItem item){

    for (unsigned i=0;i<ref_hash_table_size;i++){
        if (table[i].refer_address == item.refer_address){
            table[i].is_used = false;
            number_of_item -= 1;
        }
    }

    return table;
}