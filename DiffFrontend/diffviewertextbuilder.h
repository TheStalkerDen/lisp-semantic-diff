#ifndef DIFFVIEWERTEXTBUILDER_H
#define DIFFVIEWERTEXTBUILDER_H

#include "global.h"

#include <QJsonDocument>
#include <QJsonObject>
#include <QString>



class DiffViewerTextBuilder
{
public:
    DiffViewerTextBuilder(int* diff_line);

    QString generateText(QJsonValue, QJsonObject, bool);
    QString generateTextFromLexemsArray(QJsonArray, QJsonObject);

private:
    Global* global;

    QString text;
    QJsonObject comments;
    bool isTopLevel;

    int cur_line=1;
    int cur_column=1;

    int* diff_line;

    int getTrueLine(int cur_line);
    void pasteNewLinesAndComments(int lines);
    void pasteTopLevel(const QJsonArray& doc);
    void pasteAtom(const QJsonObject& lex);
    void pasteSymbol(QChar parent);
    void pasteWhitespaces(int line, int column);
    void genAtom(const QJsonObject& lex);
    void genList(const QJsonObject& list,bool isFirstCall);
    void genQuote(const QJsonObject& quoteSexpr);
    void loopArray(const QJsonArray& array);
    void pasteSpaces(int count);

};

#endif // DIFFVIEWERTEXTBUILDER_H
