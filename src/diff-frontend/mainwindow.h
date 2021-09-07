#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "diffviewer.h"
#include "diffviewertext.h"
#include "global.h"

#include <QJsonArray>
#include <QJsonObject>
#include <QMainWindow>
#include <QMap>
#include <QTreeWidgetItem>
#include "stats.h"

QT_BEGIN_NAMESPACE
namespace Ui { class MainWindow; }
QT_END_NAMESPACE

enum class ViewerMode {NormalMode, ErrorsMode};
enum class SeletionMode {All, DefSExpr};

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

    DiffViewerText doc1 {DiffViewerText(1)};
    DiffViewerText doc2 {DiffViewerText(2)};
    Stats stats;
    QMap<QString, QJsonObject> nameToObj1;
    QMap<QString, QJsonObject> nameToObj2;

private slots:
    void on_treeWidget_itemClicked(QTreeWidgetItem *item, int column);

    void on_chooseFile1Button_clicked();

    void on_chooseFile2Button_clicked();

    void on_startCompareButton_clicked();

    void on_actiontool_creator_triggered();

    void on_diffViewer1_cursorPositionChanged();

    void on_diffViewer2_cursorPositionChanged();

private:
    Ui::MainWindow *ui;
    void fillStatsTree();
    QTreeWidgetItem* getStatsTreeItem(QSet<QString> identSet, QString className);
    void fillErrorsInfoTree();
    QTreeWidgetItem* getErrorsInfoTreeNode(ErrorsModeTypes fileErrorsMode,QJsonArray errorsMsgsArray, QString fileName, int num);
    void analyzeSynTree(QJsonDocument& doc, int num);
    void cleanOldJsonFiles();
    QJsonDocument getJsonDocument(QString pathname);
    QString getStringFromFile(QString pathname);
    QJsonArray movedSexrpInfoArray;

    bool isTextCursorInsideMovedSexpr(int line, int column, int viewerNum);
    void handleCursorPositionChanged(DiffViewer* diffviewer, int num);

    Global* global;

    QString filepath1;
    QString filepath2;
    QString file1Name;
    QString file2Name;

    QJsonDocument synTreeJson1;
    QJsonDocument synTreeJson2;
    QJsonObject commentsJsonObj1;
    QJsonObject commentsJsonObj2;
    //for errors
    QJsonArray lexemsArrayJson1;
    QJsonArray lexemsArrayJson2;

    QJsonArray errorsMsgs1Array;
    QJsonArray errorsMsgs2Array;

    ViewerMode viewerMode = ViewerMode::NormalMode;
    SeletionMode selectionMode = SeletionMode::All;

    QJsonValue currentVal1;
    QJsonValue currentVal2;

    int ignoring_count = 0;

    bool was_recreated_text = false;

};
#endif // MAINWINDOW_H
