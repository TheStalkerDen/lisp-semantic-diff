#ifndef GLOBAL_H
#define GLOBAL_H

enum class ErrorsModeTypes {LexicalErrors, SyntaxErrors, SemanticErrors, NoErrors};

class Global
{
public:
    void operator=(const Global&) = delete;
    static Global *getInstance();

    int currentTextVersion = -1;

    int selected_error_lex_id1 = -1;
    int selected_error_lex_id2 = -1;

    int selected_error_node_id1 = -1;
    int selected_error_node_id2 = -1;

    int current_selected_moved_ids[2] = {-1,-1};

    ErrorsModeTypes file1ErrorsMode = ErrorsModeTypes::NoErrors;
    ErrorsModeTypes file2ErrorsMode = ErrorsModeTypes::NoErrors;

    int getSelectedErrorLexId();
    int getSelectedErrorNodesId();
protected:
    Global();

    static Global* global_;
};

#endif // GLOBAL_H
