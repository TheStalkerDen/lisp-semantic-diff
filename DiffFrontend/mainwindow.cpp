#include "mainwindow.h"
#include "ui_mainwindow.h"

#include "diffviewertext.h"
#include <QDir>
#include <QFileDialog>
#include <QJsonValue>
#include <QJsonArray>
#include <QPainter>
#include <QProcess>
#include <QMessageBox>
#include <QScrollBar>
#include "stats.h"

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    global = Global::getInstance();
}

MainWindow::~MainWindow()
{
    delete ui;
}


void MainWindow::on_startCompareButton_clicked()
{
    file1Name = filepath1.section("/",-1,-1);
    file2Name = filepath2.section("/",-1,-1);
    QProcess* backend_process = new QProcess(this);
    QString file = "diff-backend.exe";
    cleanOldJsonFiles();
    QStringList command_args;
    command_args << filepath1 << filepath2;
    backend_process->start(file,command_args);
    if (!backend_process->waitForFinished()) {
        qDebug() << "something is wrong";
    }
    ui->treeWidget->clear();
    qDebug() << "Backend is ok!";
    if (QFile::exists("lexer-errors-msgs1.json")) {
        viewerMode = ViewerMode::ErrorsMode;
        global->file1ErrorsMode = ErrorsModeTypes::LexicalErrors;
        errorsMsgs1Array = getJsonDocument("lexer-errors-msgs1.json").array();
    }
    if (QFile::exists("lexer-errors-msgs2.json")) {
        viewerMode = ViewerMode::ErrorsMode;
        global->file2ErrorsMode = ErrorsModeTypes::LexicalErrors;
        errorsMsgs2Array = getJsonDocument("lexer-errors-msgs2.json").array();
    }

    if(QFile::exists("parser-error-msg1.json")) {
        viewerMode = ViewerMode::ErrorsMode;
        global->file1ErrorsMode = ErrorsModeTypes::SyntaxErrors;
        errorsMsgs1Array.append(getJsonDocument("parser-error-msg1.json").object());
    }

    if(QFile::exists("parser-error-msg2.json")) {
        viewerMode = ViewerMode::ErrorsMode;
        global->file2ErrorsMode = ErrorsModeTypes::SyntaxErrors;
        errorsMsgs2Array.append(getJsonDocument("parser-error-msg2.json").object());
    }

    if (QFile::exists("semantic-errors-msgs1.json")) {
        viewerMode = ViewerMode::ErrorsMode;
        global->file1ErrorsMode = ErrorsModeTypes::SemanticErrors;
        errorsMsgs1Array = getJsonDocument("semantic-errors-msgs1.json").array();
    }
    if (QFile::exists("semantic-errors-msgs2.json")) {
        viewerMode = ViewerMode::ErrorsMode;
        global->file2ErrorsMode = ErrorsModeTypes::SemanticErrors;
        errorsMsgs2Array = getJsonDocument("semantic-errors-msgs2.json").array();
    }

    if (QFile::exists("comments1.json")) {
        commentsJsonObj1 = getJsonDocument("comments1.json").object();
    }
    if (QFile::exists("comments2.json")) {
        commentsJsonObj2 = getJsonDocument("comments2.json").object();
    }

    if (QFile::exists("moved-s-exprs-info.json")){
        movedSexrpInfoArray = getJsonDocument("moved-s-exprs-info.json").array();
    }

    if(viewerMode == ViewerMode::NormalMode){
        ui->currentModLabel->setText("Normal Mode");
        stats = Stats("top-level-stats.json");
        fillStatsTree();
        synTreeJson1 = getJsonDocument("s-exprs-tree1.json");
        synTreeJson2 = getJsonDocument("s-exprs-tree2.json");
        analyzeSynTree(synTreeJson1,1);
        analyzeSynTree(synTreeJson2,2);

        currentVal1 = QJsonValue(synTreeJson1.array());
        currentVal2 = QJsonValue(synTreeJson2.array());
        doc1.generateHTMLTextFromJson(currentVal1,commentsJsonObj1);
        doc2.generateHTMLTextFromJson(currentVal2,commentsJsonObj2);
        ui->diffViewer1->appendHtml(doc1.getText());
        ui->diffViewer2->appendHtml(doc2.getText());
    } else if (viewerMode == ViewerMode::ErrorsMode){
         ui->currentModLabel->setText("Error Mode");
         ui->currentModLabel->setStyleSheet("background-color:#FF9CA1;");
        if(QFile::exists("lexems1.json")){
            lexemsArrayJson1 = getJsonDocument("lexems1.json").array();
            doc1.generateHTMLTextFromLexemsArrayJson(lexemsArrayJson1,commentsJsonObj1);
            ui->diffViewer1->appendHtml(doc1.getText());
        }
        if(QFile::exists("lexems2.json")){
            lexemsArrayJson2 = getJsonDocument("lexems2.json").array();
            doc2.generateHTMLTextFromLexemsArrayJson(lexemsArrayJson2,commentsJsonObj2);
            ui->diffViewer2->appendHtml(doc2.getText());
        }
        if(QFile::exists("s-exprs-tree1.json")){
            synTreeJson1 = getJsonDocument("s-exprs-tree1.json");
            currentVal1 = QJsonValue(synTreeJson1.array());
            doc1.generateHTMLTextFromJson(currentVal1,commentsJsonObj1);
            ui->diffViewer1->appendHtml(doc1.getText());
        }
        if(QFile::exists("s-exprs-tree2.json")){
            synTreeJson2 = getJsonDocument("s-exprs-tree2.json");
            currentVal2 = QJsonValue(synTreeJson2.array());
            doc2.generateHTMLTextFromJson(currentVal2,commentsJsonObj2);
            ui->diffViewer2->appendHtml(doc2.getText());
        }
        if(global->file1ErrorsMode == ErrorsModeTypes::NoErrors){
            ui->diffViewer1->appendPlainText(getStringFromFile(filepath1));
        }
        if(global->file2ErrorsMode == ErrorsModeTypes::NoErrors){
            ui->diffViewer2->appendPlainText(getStringFromFile(filepath2));
        }
        fillErrorsInfoTree();
    }
    ui->stackedWidget->setCurrentIndex(1);
    ui->menubar->addMenu("Main menu");
    ui->filename1Label->setText(file1Name);
    ui->filename2Label->setText(file2Name);

    if(viewerMode == ViewerMode::ErrorsMode){
        QString errors_message;
        switch (global->file1ErrorsMode) {
            case ErrorsModeTypes::LexicalErrors: errors_message.append(QString("%1 has %2\n").arg(file1Name, "lexical errors")); break;
            case ErrorsModeTypes::SyntaxErrors: errors_message.append(QString("%1 has %2\n").arg(file1Name, "syntax errors")); break;
            case ErrorsModeTypes::SemanticErrors: errors_message.append(QString("%1 has %2\n").arg(file1Name, "semantic errors")); break;
            case ErrorsModeTypes::NoErrors:;
        }
        switch (global->file2ErrorsMode) {
            case ErrorsModeTypes::LexicalErrors: errors_message.append(QString("%1 has %2\n").arg(file2Name, "lexical errors")); break;
            case ErrorsModeTypes::SyntaxErrors: errors_message.append(QString("%1 has %2\n").arg(file2Name, "syntax errors")); break;
            case ErrorsModeTypes::SemanticErrors: errors_message.append(QString("%1 has %2\n").arg(file2Name, "semantic errors")); break;
            case ErrorsModeTypes::NoErrors:;
        }
        QMessageBox errors_message_box;
        errors_message_box.setText(errors_message);
        errors_message_box.exec();
    }
}

void MainWindow::fillStatsTree()
{
    auto& statsTree = ui->treeWidget;
    auto* topLevelItem = new QTreeWidgetItem();
    topLevelItem->setText(0,"all");
    topLevelItem->setData(0,Qt::ItemDataRole::UserRole,"all");
    statsTree->addTopLevelItem(topLevelItem);
    for(auto& tldType: stats.getTldTypes()){
        auto* specificTLDTypeItem = new QTreeWidgetItem();
        specificTLDTypeItem->setText(0,tldType);
        statsTree->addTopLevelItem(specificTLDTypeItem);

        auto& no_mods_ids_set = stats.getNoMods(tldType);
        if(no_mods_ids_set.size() > 0){
            auto* no_mods_tree_item = getStatsTreeItem(no_mods_ids_set,"same");
            specificTLDTypeItem->addChild(no_mods_tree_item);
        }

        auto& mods_ids_set = stats.getMod(tldType);
        if(mods_ids_set.size() > 0){
            auto* mods_tree_item = getStatsTreeItem(mods_ids_set,"mods");
            specificTLDTypeItem->addChild(mods_tree_item);
        }

        auto& news_ids_set = stats.getNew(tldType);
        if(news_ids_set.size() > 0){
            auto* news_tree_item = getStatsTreeItem(news_ids_set,"news");
            specificTLDTypeItem->addChild(news_tree_item);
        }

        auto& dels_ids_set = stats.getDel(tldType);
        if(dels_ids_set.size() > 0){
            auto* dels_tree_item = getStatsTreeItem(dels_ids_set,"dels");
            specificTLDTypeItem->addChild(dels_tree_item);
        }
    }
}

QTreeWidgetItem* MainWindow::getStatsTreeItem(QSet<QString> identSet, QString className){
    auto* classItem  = new QTreeWidgetItem();
    classItem->setText(0,className);
    for(auto& ident : identSet){
        auto* identItem = new QTreeWidgetItem();
        identItem->setText(0,ident);
        identItem->setData(0,Qt::ItemDataRole::UserRole,"identName");
        classItem->addChild(identItem);
    }
    return classItem;
}

void MainWindow::fillErrorsInfoTree()
{
    auto& errorsInfoTree = ui->treeWidget;
    auto* errorsInfoTreeNode1 = getErrorsInfoTreeNode(global->file1ErrorsMode,errorsMsgs1Array, file1Name,1);
    auto* errorsInfoTreeNode2 = getErrorsInfoTreeNode(global->file2ErrorsMode,errorsMsgs2Array, file2Name,2);
    if(errorsInfoTreeNode1){
        errorsInfoTree->addTopLevelItem(errorsInfoTreeNode1);
    }
    if(errorsInfoTreeNode2){
        errorsInfoTree->addTopLevelItem(errorsInfoTreeNode2);
    }
}

QTreeWidgetItem* MainWindow::getErrorsInfoTreeNode(ErrorsModeTypes fileErrorsMode, QJsonArray errorsMsgsArray, QString fileName, int num)
{
    if(fileErrorsMode != ErrorsModeTypes::NoErrors){
        auto* fileErrorsInfoTreeNode = new QTreeWidgetItem();
        QString errorType;
        switch(fileErrorsMode){
            case ErrorsModeTypes::LexicalErrors : errorType = "lexical" ; break;
            case ErrorsModeTypes::SyntaxErrors : errorType = "syntax" ; break;
            case ErrorsModeTypes::SemanticErrors : errorType = "semantic"; break;
            case ErrorsModeTypes::NoErrors:;
        }
        fileErrorsInfoTreeNode->setText(0,QString("%1 %2 errors").arg(fileName,errorType));
        fileErrorsInfoTreeNode->setData(0,Qt::ItemDataRole::UserRole,num);
        for(int i = 0 ; i < errorsMsgsArray.size(); i++){
            auto* errorMsgItem = new QTreeWidgetItem();
            errorMsgItem->setText(0,errorsMsgsArray[i].toObject()["errorText"].toString());
            errorMsgItem->setData(0,Qt::ItemDataRole::UserRole, "errorInfo");
            fileErrorsInfoTreeNode->addChild(errorMsgItem);
        }
        return fileErrorsInfoTreeNode;
    }
    return nullptr;
}

void MainWindow::analyzeSynTree(QJsonDocument &doc, int num)
{
   const auto& topLevelArray = doc.array();
   for(const auto& obj: topLevelArray){
       QJsonObject topLevelDef = obj.toObject();
       QString tldName = topLevelDef["props"].toObject()["istoplevel"].toString();
       if(num == 1){
           nameToObj1[tldName] = topLevelDef;
       }else {
           nameToObj2[tldName] = topLevelDef;
       }
   }
}

void MainWindow::cleanOldJsonFiles()
{
    QFile::remove("lexems1.json");
    QFile::remove("lexems2.json");
    QFile::remove("comments1.json");
    QFile::remove("comments2.json");
    QFile::remove("lexer-errors-msgs1.json");
    QFile::remove("lexer-errors-msgs2.json");
    QFile::remove("parser-error-msg1.json");
    QFile::remove("parser-error-msg2.json");
    QFile::remove("semantic-errors-msgs1.json");
    QFile::remove("semantic-errors-msgs2.json");
    QFile::remove("s-exprs-tree1.json");
    QFile::remove("s-exprs-tree2.json");
    QFile::remove("top-level-stats.json");
    QFile::remove("moved-s-exprs-info.json");
}


QJsonDocument MainWindow::getJsonDocument(QString pathname)
{
    QFile loadFile(pathname);
    if(!loadFile.open(QIODevice::ReadOnly)){
        qWarning("Couldn't open file.");
        return QJsonDocument();
    }

    QByteArray saveData = loadFile.readAll();

    return QJsonDocument::fromJson(saveData);
}

QString MainWindow::getStringFromFile(QString pathname)
{
    QFile loadFile(pathname);
    if(!loadFile.open(QIODevice::ReadOnly)){
        qWarning("Couldn't open file.");
        return QString();
    }

    QByteArray saveData = loadFile.readAll();

    return QString(saveData);
}

bool MainWindow::isTextCursorInsideMovedSexpr(int line, int column, int viewerNum)
{
    QJsonArray start_coord_sexpr;
    QJsonArray end_coord_sexpr;
    int abs_line;
    for(int i = 0; i < movedSexrpInfoArray.size(); i++){
        const auto& movedSexprInfo = movedSexrpInfoArray[i].toObject();
        if(viewerNum == 1){
            start_coord_sexpr = movedSexprInfo["startCoordOfId1"].toArray();
            end_coord_sexpr = movedSexprInfo["endCoordOfId1"].toArray();
            abs_line = line + ui->diffViewer1->getLineOffset();
        } else if(viewerNum == 2){
            start_coord_sexpr = movedSexprInfo["startCoordOfId2"].toArray();
            end_coord_sexpr = movedSexprInfo["endCoordOfId2"].toArray();
            abs_line = line + ui->diffViewer2->getLineOffset();
        }

       if(abs_line >= start_coord_sexpr[0].toInt() && abs_line <= end_coord_sexpr[0].toInt()
          && column >= start_coord_sexpr[1].toInt() && column <= end_coord_sexpr[1].toInt()){
          if(global->current_selected_moved_ids[0] == movedSexprInfo["sExprId1"].toInt()){
              return false;
          }
          global->current_selected_moved_ids[0] = movedSexprInfo["sExprId1"].toInt();
          global->current_selected_moved_ids[1] = movedSexprInfo["sExprId2"].toInt();
          return true;
       }
    }
    //global->current_selected_moved_ids[0] = -1;
    //global->current_selected_moved_ids[1] = -1;
    return false;
}

void MainWindow::handleCursorPositionChanged(DiffViewer* diffviewer, int num)
{
    int line = diffviewer->textCursor().blockNumber() + 1;
    int column = diffviewer->textCursor().positionInBlock() + 1;
    int abs_pos = diffviewer->textCursor().position();

    if(isTextCursorInsideMovedSexpr(line, column, num)){
        int val1 = ui->diffViewer1->verticalScrollBar()->value();
        ui->diffViewer1->clear();
        int val2 = ui->diffViewer2->verticalScrollBar()->value();
        ui->diffViewer2->clear();
        doc1.generateHTMLTextFromJson(currentVal1,commentsJsonObj1, selectionMode == SeletionMode::All);
        ui->diffViewer1->appendHtml(doc1.getText());
        ui->diffViewer1->verticalScrollBar()->setValue(val1);
        //ui->diffViewer1->textCursor().setPosition(abs_pos);
        doc2.generateHTMLTextFromJson(currentVal2,commentsJsonObj2, selectionMode == SeletionMode::All);
        ui->diffViewer2->appendHtml(doc2.getText());
        ui->diffViewer2->verticalScrollBar()->setValue(val2);
    }
}

void MainWindow::on_treeWidget_itemClicked(QTreeWidgetItem *item, int column)
{
    if(item->data(0,Qt::ItemDataRole::UserRole).type() == QVariant::Type::String){
        qDebug() << item->text(0);
        QString text = item->text(0);
        QString itemData = item->data(0,Qt::ItemDataRole::UserRole).toString();
        if(itemData == "all"){
            selectionMode = SeletionMode::All;
            currentVal1 = QJsonValue(synTreeJson1.array());
            currentVal2 = QJsonValue(synTreeJson2.array());
            doc1.generateHTMLTextFromJson(currentVal1, commentsJsonObj1);
            doc2.generateHTMLTextFromJson(currentVal2, commentsJsonObj2);
            ui->diffViewer1->clear();
            ui->diffViewer2->clear();
            ui->diffViewer1->setLineOffset(0);
            ui->diffViewer2->setLineOffset(0);
            ui->diffViewer1->appendHtml(doc1.getText());
            ui->diffViewer2->appendHtml(doc2.getText());
        } else if (itemData == "identName") {
            selectionMode = SeletionMode::DefSExpr;
            qDebug()  << item->parent()->indexOfChild(item);
            if(nameToObj1.contains(text)){
                currentVal1 = QJsonValue(nameToObj1[text]);
                doc1.generateHTMLTextFromJson(currentVal1, commentsJsonObj1,false);
                ui->diffViewer1->clear();
                ui->diffViewer1->setLineOffset(doc1.getLineOffset());
                ui->diffViewer1->appendHtml(doc1.getText());
            }
            if(nameToObj2.contains(text)){
                currentVal2 = QJsonValue(nameToObj2[text]);
                doc2.generateHTMLTextFromJson(currentVal2, commentsJsonObj2,false);
                ui->diffViewer2->clear();
                ui->diffViewer2->setLineOffset(doc2.getLineOffset());
                ui->diffViewer2->appendHtml(doc2.getText());
            }
            if(nameToObj1.contains(text) && !nameToObj2.contains(text)){
                ui->diffViewer2->clear();
            }else if (!nameToObj1.contains(text) && nameToObj2.contains(text)) {
                ui->diffViewer1->clear();
            }
        } else if (itemData == "errorInfo") {
            int pos = item->parent()->indexOfChild(item);
            int docNum = item->parent()->data(0,Qt::ItemDataRole::UserRole).toInt();
            if(docNum == 1){
                if(global->file1ErrorsMode != ErrorsModeTypes::SemanticErrors){
                    global->selected_error_lex_id1 = errorsMsgs1Array[pos].toObject()["errorLexId"].toInt();
                    doc1.generateHTMLTextFromLexemsArrayJson(lexemsArrayJson1,commentsJsonObj1);
                    ui->diffViewer1->clear();
                    ui->diffViewer1->appendHtml(doc1.getText());
                }else {
                    global->selected_error_node_id1 = errorsMsgs1Array[pos].toObject()["errorNodeId"].toInt();
                    doc1.generateHTMLTextFromJson(currentVal1,commentsJsonObj1);
                    ui->diffViewer1->clear();
                    ui->diffViewer1->appendHtml(doc1.getText());
                }
            } else if(docNum == 2) {
                if(global->file2ErrorsMode != ErrorsModeTypes::SemanticErrors){
                    global->selected_error_lex_id2 = errorsMsgs2Array[pos].toObject()["errorLexId"].toInt();
                    doc2.generateHTMLTextFromLexemsArrayJson(lexemsArrayJson2,commentsJsonObj2);
                    ui->diffViewer2->clear();
                    ui->diffViewer2->appendHtml(doc2.getText());
                }else {
                    global->selected_error_node_id2 = errorsMsgs2Array[pos].toObject()["errorNode"].toInt();
                    doc2.generateHTMLTextFromJson(currentVal2,commentsJsonObj2);
                    ui->diffViewer2->clear();
                    ui->diffViewer2->appendHtml(doc2.getText());
                }
            }
        }
    }
}

void MainWindow::on_chooseFile1Button_clicked()
{
    filepath1 = QFileDialog::getOpenFileName(this,"Select LISP file");
    ui->filepath1Label->setText(filepath1);
    if(filepath2.length() > 0){
        ui->startCompareButton->setEnabled(true);
    }
}

void MainWindow::on_chooseFile2Button_clicked()
{
    filepath2 = QFileDialog::getOpenFileName(this,"Select LISP file");
    ui->filepath2Label->setText(filepath2);
    if(filepath1.length() > 0){
        ui->startCompareButton->setEnabled(true);
    }
}

void MainWindow::on_actiontool_creator_triggered()
{
    QMessageBox message_about_creator;
    message_about_creator.setText("This project was created by Denys Yermolenko KV-73");
    message_about_creator.exec();
}

void MainWindow::on_diffViewer1_cursorPositionChanged()
{
    handleCursorPositionChanged(ui->diffViewer1,1);
}

void MainWindow::on_diffViewer2_cursorPositionChanged()
{
    handleCursorPositionChanged(ui->diffViewer2,2);
}

