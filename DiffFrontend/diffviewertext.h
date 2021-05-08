#ifndef DIFFVIEWERTEXT_H
#define DIFFVIEWERTEXT_H

#include <Global.h>
#include <QJsonDocument>
#include <QObject>

class DiffViewerText : public QObject
{
    Q_OBJECT
public:
    explicit DiffViewerText(int forTextVersion, QObject *parent = nullptr);
    QString getText();
    void generateHTMLTextFromJson(QJsonValue obj, QJsonObject comments, bool isTopLevel = true);
    void generateHTMLTextFromLexemsArrayJson(QJsonArray lexems, QJsonObject comments);
private:

    Global* global;

    QJsonDocument loadedJson;
    int forTextVersion;

    QString text;


signals:

};

#endif // DIFFVIEWERTEXT_H
