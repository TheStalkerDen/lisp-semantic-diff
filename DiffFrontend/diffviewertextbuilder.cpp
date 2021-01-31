#include "diffviewertextbuilder.h"

#include <QJsonArray>
#include <QJsonObject>

DiffViewerTextBuilder::DiffViewerTextBuilder()
{

}

QString DiffViewerTextBuilder::generateText(QJsonDocument doc)
{
    text.append("<pre>");
    QJsonArray top_level_sexprs_array = doc.array();
    loopArray(top_level_sexprs_array);
    text.append("</pre>");
    return text;
}

void DiffViewerTextBuilder::pasteLexem(QJsonObject &lex)
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
        cur_line = line;
        cur_column = column;
    } else {
        int line_delta = line - cur_line;
        text.append(QString(line_delta, '\n'));
        pasteSpaces(column - 1);
        cur_line = line;
        cur_column = column;
    }
}

void DiffViewerTextBuilder::genLexem(QJsonObject &lex)
{
    QJsonArray lexem_pos = lex["lexem-coord"].toArray();
    if(lexem_pos[0].toInt() == cur_line){
        pasteSpaces(lexem_pos[1].toInt() - cur_column);
        pasteLexem(lex);
        cur_column = lexem_pos[1].toInt() + lex["string"].toString().size();
    } else {
        int line_delta = lexem_pos[0].toInt() - cur_line;
        text.append(QString('\n', line_delta));
        pasteSpaces(lexem_pos[1].toInt());
        cur_line = lexem_pos[0].toInt();
        pasteLexem(lex);
        cur_column = lexem_pos[1].toInt() + lex["string"].toString().size();
    }
}

void DiffViewerTextBuilder::genList(QJsonObject &listObj)
{
    QJsonObject parent_info = listObj["par-info"].toObject();
    QJsonArray lparenCoord = parent_info["lparenCoord"].toArray();
    QJsonArray rparenCoord = parent_info["rparenCoord"].toArray();

    auto main_part = [&](){
        pasteParent('(');
        QJsonArray array = listObj["elems"].toArray();
        loopArray(array);
        pasteSpacesBeforeParent(rparenCoord[0].toInt(),rparenCoord[1].toInt());
        pasteParent(')');
    };


    pasteSpacesBeforeParent(lparenCoord[0].toInt(),lparenCoord[1].toInt());
    if(listObj["diff-st"].toString() == "deleted"){
        text.append("<font style=\"background-color:#FF9CA1;\">");
        main_part();
        text.append("</font>");
    }else if (listObj["diff-st"].toString() == "new"){
        text.append("<font style=\"background-color:#C9FFBF;\">");
        main_part();
        text.append("</font>");
    }else if(listObj["diff-st"].toString() == "moved"){
        text.append("<font style=\"background-color: #E5F0FF;\">");
        main_part();
        text.append("</font>");
    }else {
        main_part();
    }
}

void DiffViewerTextBuilder::loopArray(QJsonArray &array)
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




