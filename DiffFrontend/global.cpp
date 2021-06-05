#include "global.h"

Global* Global::global_ = nullptr;

Global *Global::getInstance()
{
    if(global_ ==nullptr){
        global_ = new Global();
    }
    return global_;
}

int Global::getSelectedErrorLexId()
{
    if(currentTextVersion == 1){
        return selected_error_lex_id1;
    } else if(currentTextVersion == 2) {
        return selected_error_lex_id2;
    }
}

int Global::getSelectedErrorNodesId()
{
    if(currentTextVersion == 1){
        return selected_error_node_id1;
    } else if(currentTextVersion == 2) {
        return selected_error_node_id2;
    }
}

Global::Global()
{

}
