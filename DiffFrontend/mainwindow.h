#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "diffviewertext.h"

#include <QMainWindow>
#include <QTreeWidgetItem>
#include "stat.h"

QT_BEGIN_NAMESPACE
namespace Ui { class MainWindow; }
QT_END_NAMESPACE

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

    DiffViewerText doc1;
    DiffViewerText doc2;
    Stat stats;
    QMap<QString, QJsonObject> nameToObj1;
    QMap<QString, QJsonObject> nameToObj2;

private slots:
    void on_actionloadFiles_triggered();


    void on_treeWidget_itemClicked(QTreeWidgetItem *item, int column);

private:
    Ui::MainWindow *ui;
    void fillStatsTree();
    void analyzeSynTree(QJsonDocument& doc, int num);
    QJsonDocument getJsonDocument(QString pathname);
    QJsonDocument synTreeJson1;
    QJsonDocument synTreeJson2;
    QJsonObject commentsJsonObj1;
    QJsonObject commentsJsonObj2;

};
#endif // MAINWINDOW_H
