#ifndef STAT_H
#define STAT_H

#include <QJsonObject>
#include <QSet>

//TLD - top-level-definition
struct TLDIdentClassSets {
    QSet<QString> no_mod_ids;
    QSet<QString> mod_ids;
    QSet<QString> del_ids;
    QSet<QString> new_ids;
};

class Stats
{
public:
    Stats() = default;
    Stats(QString pathname);
    auto& getNoMods(QString tldTypeName) {return  _tldInfoMap[tldTypeName].no_mod_ids;}
    auto& getMod(QString tldTypeName) {return  _tldInfoMap[tldTypeName].mod_ids;}
    auto& getDel(QString tldTypeName) {return  _tldInfoMap[tldTypeName].del_ids;}
    auto& getNew(QString tldTypeName) {return  _tldInfoMap[tldTypeName].new_ids;}

    QStringList getTldTypes() {return tldTypes;}
private:
    QMap<QString, TLDIdentClassSets> _tldInfoMap;
    QStringList tldTypes;

    void readStatsJsonObj(QJsonObject&);
};

#endif // STAT_H
