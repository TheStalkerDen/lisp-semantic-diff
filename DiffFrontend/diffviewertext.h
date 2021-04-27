#ifndef DIFFVIEWERTEXT_H
#define DIFFVIEWERTEXT_H

#include <QJsonDocument>
#include <QObject>

class DiffViewerText : public QObject
{
    Q_OBJECT
public:
    explicit DiffViewerText(QObject *parent = nullptr);
    QString getText();
    void generateHTMLTextFromJson(QJsonValue obj, QJsonObject comments, bool isTopLevel = true);
    void generateHTMLTextFromLexemsArrayJson(QJsonArray lexems, QJsonObject comments);
private:

    QJsonDocument loadedJson;

    QString text;


signals:

};

#endif // DIFFVIEWERTEXT_H
