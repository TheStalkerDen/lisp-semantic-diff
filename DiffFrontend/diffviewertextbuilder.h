#ifndef DIFFVIEWERTEXTBUILDER_H
#define DIFFVIEWERTEXTBUILDER_H

#include <QJsonDocument>
#include <QString>



class DiffViewerTextBuilder
{
public:
    DiffViewerTextBuilder();

    QString generateText(QJsonDocument);

private:
    QString text;

    int cur_line=1;
    int cur_column=1;

    void pasteLexem(QJsonObject& lex);
    void pasteParent(QChar parent);
    void pasteSpacesBeforeParent(int line, int column);
    void genLexem(QJsonObject& lex);
    void genList(QJsonObject& list);
    void loopArray(QJsonArray& array);
    void pasteSpaces(int count);

};

#endif // DIFFVIEWERTEXTBUILDER_H
