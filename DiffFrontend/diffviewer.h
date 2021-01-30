#ifndef DIFFVIEWER_H
#define DIFFVIEWER_H

#include <QPlainTextEdit>

class DiffViewer : public QPlainTextEdit
{
    Q_OBJECT
public:
    DiffViewer(QWidget *parent = nullptr);

    void lineNumbeerAreaPaintEvent(QPaintEvent *event);
    int lineNumberAreaWidth();

protected:

    void resizeEvent(QResizeEvent *event) override;

private slots:
    void updateLineNumberAreaWidth(int newBlockCount);
    void updateLineNumberArea(const QRect &rect, int dy);

private:
    QWidget *lineNumberArea;
};

#endif // DIFFVIEWER_H
