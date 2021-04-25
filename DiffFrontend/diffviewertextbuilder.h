#ifndef DIFFVIEWERTEXTBUILDER_H
#define DIFFVIEWERTEXTBUILDER_H

#include <QJsonDocument>
#include <QJsonObject>
#include <QString>



class DiffViewerTextBuilder
{
public:
    DiffViewerTextBuilder();

    QString generateText(QJsonValue, QJsonObject, bool);

private:
    QString text;
    QJsonObject comments;
    bool isTopLevel;

    int cur_line=1;
    int cur_column=1;

    int diff_line = 0;

    int getTrueLine(int cur_line);
    void pasteNewLinesAndComments(int lines);
    void pasteTopLevel(const QJsonArray& doc);
    void pasteLexem(const QJsonObject& lex);
    void pasteParent(QChar parent);
    void pasteSpacesBeforeParent(int line, int column);
    void genLexem(const QJsonObject& lex);
    void genList(const QJsonObject& list,bool isFirstCall);
    void loopArray(const QJsonArray& array);
    void pasteSpaces(int count);

};

#endif // DIFFVIEWERTEXTBUILDER_H
