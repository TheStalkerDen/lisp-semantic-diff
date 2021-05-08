#include "diffviewertextbuilder.h"

#include <QJsonArray>
#include <QJsonObject>

DiffViewerTextBuilder::DiffViewerTextBuilder()
{
    global = Global::getInstance();
}

QString DiffViewerTextBuilder::generateText(QJsonValue jsonVal, QJsonObject comments, bool isTopLevel= true)
{
    this->isTopLevel = isTopLevel;
    this->comments = comments;
    text.append("<pre>");
    if(isTopLevel){
        pasteTopLevel(jsonVal.toArray());
    }else{
        genList(jsonVal.toObject(),true);
    }
    text.append("</pre>");
    return text;
}

QString DiffViewerTextBuilder::generateTextFromLexemsArray(QJsonArray lexems, QJsonObject comments)
{
    this->comments = comments;
    text.append("<pre>");
    for(int i = 0; i < lexems.size(); i++){
        QJsonObject lex = lexems[i].toObject();
        if(getTrueLine(lex["line"].toInt()) == cur_line){
            pasteSpaces(lex["column"].toInt() - cur_column);
        } else {
            pasteNewLinesAndComments(getTrueLine(lex["line"].toInt()));
            pasteSpaces(lex["column"].toInt() - cur_column);
            cur_line = getTrueLine(lex["line"].toInt());
        }
        pasteLexem(lex);
        if(lex["type"] == "string"){
            int newlines = lex["string"].toString().count('\n');
            cur_line += newlines;
        }
        cur_column = lex["column"].toInt() + lex["string"].toString().size();
    }
    text.append("</pre>");
    return text;
}

int DiffViewerTextBuilder::getTrueLine(int cur_line)
{
    return cur_line - diff_line;
}

void DiffViewerTextBuilder::pasteTopLevel(const QJsonArray &array)
{
    loopArray(array);
}

void DiffViewerTextBuilder::pasteNewLinesAndComments(int next_line){
    QJsonObject comment;
    while(cur_line < next_line){
        if(comments[QString::number(cur_line+diff_line)] != QJsonValue::Undefined) {
            comment = comments[QString::number(cur_line+diff_line)].toObject();
            int column_diff = comment["column"].toInt() - cur_column;
            pasteSpaces(column_diff);
            text.append("<font style=\"color:#cccccc;\">");
            text.append(comment["comment"].toString());
            text.append("</font>");

        }
        text.append("\n");
        cur_column = 1;
        cur_line++;
    }
}

void DiffViewerTextBuilder::pasteLexem(const QJsonObject &lex)
{
    if(lex["diff-st"].toString() == "deleted"){
        text.append("<font style=\"background-color:#FF9CA1;\">");
        text.append(lex["string"].toString());
        text.append("</font>");
    }else if (lex["diff-st"].toString() == "new"){
        text.append("<font style=\"background-color:#C9FFBF;\">");
        text.append(lex["string"].toString());
        text.append("</font>");
    }else if (lex["diff-st"].toString() == "moved"){
        text.append("<font style=\"background-color: #E5F0FF;\">");
        text.append(lex["string"].toString());
        text.append("</font>");
    }else if (lex["type"].toString() == "errorLexem"){
        if(lex["id"] == global->getSelectedErrorLexId()){
            text.append("<font style=\"background-color:#ffffe6;\">");
        }else{
            text.append("<font style=\"background-color:#FF9CA1;\">");
        }
        text.append(lex["string"].toString());
        text.append("</font>");
    }else {
        text.append(lex["string"].toString());
    }
}

void DiffViewerTextBuilder::pasteParent(QChar parent){

        text.append(parent);
        cur_column++;
}


void DiffViewerTextBuilder::pasteSpacesBeforeParent(int line, int column)
{
    if(cur_line == line){
        pasteSpaces(column - cur_column);
        cur_column = column;
    } else {
        pasteNewLinesAndComments(line);
        pasteSpaces(column - 1);
        cur_column = column;
    }
}

void DiffViewerTextBuilder::genLexem(const QJsonObject &lex)
{
    QJsonArray lexem_pos = lex["lexem-coord"].toArray();
    if(getTrueLine(lexem_pos[0].toInt()) == cur_line){
        pasteSpaces(lexem_pos[1].toInt() - cur_column);
    } else {
        pasteNewLinesAndComments(getTrueLine(lexem_pos[0].toInt()));
        pasteSpaces(lexem_pos[1].toInt() - cur_column);
        cur_line = getTrueLine(lexem_pos[0].toInt());
    }
    pasteLexem(lex);
    cur_column = lexem_pos[1].toInt() + lex["string"].toString().size();
}

void DiffViewerTextBuilder::genList(const QJsonObject &listObj, bool isFirstCall = false)
{
    QJsonObject parent_info = listObj["par-info"].toObject();
    QJsonArray lparenCoord = parent_info["lparenCoord"].toArray();
    if(isFirstCall && !isTopLevel){
        diff_line = lparenCoord[0].toInt();
    }
    QJsonArray rparenCoord = parent_info["rparenCoord"].toArray();
    auto main_part = [&](){
        pasteParent('(');
        QJsonArray array = listObj["elems"].toArray();
        loopArray(array);
        pasteSpacesBeforeParent(getTrueLine(rparenCoord[0].toInt()),rparenCoord[1].toInt());
        pasteParent(')');
    };


    pasteSpacesBeforeParent(getTrueLine(lparenCoord[0].toInt()),lparenCoord[1].toInt());
    if((listObj["diff-st"].toString() == "deleted") ||
            listObj["isIllegalNode"].toBool()){
        text.append("<font style=\"background-color:#FF9CA1;\">");
        main_part();
        text.append("</font>");
    }else if (listObj["diff-st"].toString() == "new"){
        text.append("<font style=\"background-color:#C9FFBF;\">");
        main_part();
        text.append("</font>");
    }else if(listObj["diff-st"].isArray() && listObj["diff-st"].toArray()[0] == "moved"){
        if((global->currentTextVersion == 1 && global->current_selected_moved_ids[1] == listObj["diff-st"].toArray()[1].toInt())
                || (global->currentTextVersion == 2 && global->current_selected_moved_ids[0] == listObj["diff-st"].toArray()[1].toInt())){
             text.append("<font style=\"background-color: #AAA0FF;\">");
        } else {
           text.append("<font style=\"background-color: #E5F0FF;\">");
        }
        main_part();
        text.append("</font>");
    }else {
        main_part();
    }
}

void DiffViewerTextBuilder::loopArray(const QJsonArray &array)
{
    for(int elem_index = 0; elem_index < array.size(); ++elem_index){
        QJsonObject sexprObject = array[elem_index].toObject();
        if(sexprObject["type"].toString() == "list"){
            genList(sexprObject);
        } else if(sexprObject["type"].toString() == "lexem"){
            genLexem(sexprObject);
        }
    }
}

void DiffViewerTextBuilder::pasteSpaces(int count)
{
    text.append(QString(count, ' '));
}




