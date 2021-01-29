#include "diffviewertext.h"
#include "diffviewertextbuilder.h"

#include <QFile>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>

DiffViewerText::DiffViewerText(QObject *parent) : QObject(parent)
{

}

QString DiffViewerText::getText()
{
    return text;
}

void DiffViewerText::setTextDescriptionFromJson(QString pathnane)
{
    QFile loadFile(pathnane);
    if(!loadFile.open(QIODevice::ReadOnly)){
        qWarning("Couldn't open save file.");
        return;
    }

    QByteArray saveData = loadFile.readAll();

    loadedJson = QJsonDocument::fromJson(saveData);
    DiffViewerTextBuilder builder;
    text = builder.generateText(loadedJson);

}
