#include <QList>

#ifndef ASTNODES_H
#define ASTNODES_H

#endif // ASTNODES_H

enum class DiffStatus{Same, New, Deleted};

struct ISexpr {
    DiffStatus diff_status = DiffStatus::Same;
};

struct ParentInfo {
    int lparent_line;
    int lparent_column;
    int rparent_line;
    int rparent_column;
};

struct LexemPos {
    int line;
    int column;
};

struct List : public ISexpr {
    ParentInfo parent_info;
    QList<ISexpr> elements;
};

struct Lexem : public ISexpr {
    LexemPos lexem_pos;
    QString lexem_string;
};

