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
    void setTextDescriptionFromJson(QString pathnane);
private:

    QJsonDocument loadedJson;

    QString text;


signals:

};

#endif // DIFFVIEWERTEXT_H
