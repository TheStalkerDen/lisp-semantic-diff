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

void DiffViewerText::generateHTMLTextFromLexemsArrayJson(QJsonArray lexems, QJsonObject comments)
{
    DiffViewerTextBuilder builder;
    text = builder.generateTextFromLexemsArray(lexems,comments);
}

void DiffViewerText::generateHTMLTextFromJson(QJsonValue jsonVal,QJsonObject comments, bool isTopLevel)
{
    DiffViewerTextBuilder builder;
    text = builder.generateText(jsonVal,comments, isTopLevel);
}
