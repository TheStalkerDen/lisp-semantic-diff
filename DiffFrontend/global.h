#ifndef GLOBAL_H
#define GLOBAL_H


class Global
{
public:
    void operator=(const Global&) = delete;
    static Global *getInstance();

    int currentTextVersion = 1;

    int selected_error_lex_id1 = -1;
    int selected_error_lex_id2 = -1;

    int current_selected_moved_ids[2] = {-1,-1};

    int getSelectedErrorLexId();
protected:
    Global();

    static Global* global_;
};

#endif // GLOBAL_H
