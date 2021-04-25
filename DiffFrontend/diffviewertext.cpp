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

void DiffViewerText::setTextDescriptionFromJson(QJsonValue jsonVal,bool isTopLevel)
{
    DiffViewerTextBuilder builder;
    text = builder.generateText(jsonVal, isTopLevel);
}
