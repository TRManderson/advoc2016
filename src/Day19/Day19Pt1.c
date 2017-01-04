#include <stdio.h>
#include <stdlib.h>

struct ll_node {
  int n;
  struct ll_node * next;
};

struct ll_node * build_list(int n){
  struct ll_node * head = malloc(sizeof(struct ll_node));
  struct ll_node * current = head;
  for (int i=1; i < n; i++){
    current->n = i;
    current->next = malloc(sizeof(struct ll_node));
    current = current->next;
  }
  current->n = n;
  current->next = head;
  return head;
}

struct ll_node * run_through(struct ll_node * current){
  struct ll_node * next_item = 0;
  while(current->next != current){
    next_item = current->next;
    current->next = next_item->next;
    current = current->next;
    free(next_item);
  }
  return current;
}

int main(){
  struct ll_node * head = build_list(3001330);
  head = run_through(head);
  printf("%d\n", head->n);
  fflush(stdout);
  free(head);
  return 0;
}