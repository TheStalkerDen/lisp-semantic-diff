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
    void setTextDescriptionFromJson(QJsonValue obj, bool isTopLevel = true);
private:

    QJsonDocument loadedJson;

    QString text;


signals:

};

#endif // DIFFVIEWERTEXT_H
