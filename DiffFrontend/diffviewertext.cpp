#include "diffviewertext.h"
#include "diffviewertextbuilder.h"

#include <QFile>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>

DiffViewerText::DiffViewerText(int forTextVersion, QObject *parent) : QObject(parent)
{
    this->forTextVersion = forTextVersion;
    global = Global::getInstance();
}

QString DiffViewerText::getText()
{
    return text;
}

void DiffViewerText::generateHTMLTextFromLexemsArrayJson(QJsonArray lexems, QJsonObject comments)
{
    global->currentTextVersion = forTextVersion;
    text.clear();
    DiffViewerTextBuilder builder;
    text = builder.generateTextFromLexemsArray(lexems,comments);
}

void DiffViewerText::generateHTMLTextFromJson(QJsonValue jsonVal,QJsonObject comments, bool isTopLevel)
{
    global->currentTextVersion = forTextVersion;
    text.clear();
    DiffViewerTextBuilder builder;
    text = builder.generateText(jsonVal,comments, isTopLevel);
}
