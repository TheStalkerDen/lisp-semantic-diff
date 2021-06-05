#include "stats.h"

#include <qfile.h>
#include <QJsonDocument>
#include <QJsonObject>
#include <QMessageBox>
#include <QJsonArray>

Stats::Stats(QString pathname)
{
    QFile file(pathname);
    if(!file.open(QIODevice::ReadOnly)){
        qDebug() << "Unable to open file: " << file.errorString();
        return;
    }

    QByteArray statsFileData = file.readAll();
    QJsonDocument statsJsonDoc(QJsonDocument::fromJson(statsFileData));
    QJsonObject statsJsonObj = statsJsonDoc.object();
    readStatsJsonObj(statsJsonObj);
}

void Stats::readStatsJsonObj(QJsonObject & jsonObj)
{

    tldTypes = jsonObj["old-ver"].toObject().keys();
    for(auto& tldType: tldTypes){
        QJsonObject statsOldVerIdsClassesObj = jsonObj["old-ver"].toObject()[tldType].toObject();
        TLDIdentClassSets sets;
        if(statsOldVerIdsClassesObj["noMod"].isArray()){
            QJsonArray no_mod_ids_array = statsOldVerIdsClassesObj["noMod"].toArray();
            for(const QJsonValue& array_el : no_mod_ids_array){
                sets.no_mod_ids.insert(array_el.toString());
            }
        }
        if(statsOldVerIdsClassesObj["modified"].isArray()){
            QJsonArray mod_ids_array = statsOldVerIdsClassesObj["modified"].toArray();
            for(const QJsonValue& array_el : mod_ids_array){
                sets.mod_ids.insert(array_el.toString());
            }
        }
        if(statsOldVerIdsClassesObj["deleted"].isArray()){
            QJsonArray deleted_ids_array = statsOldVerIdsClassesObj["deleted"].toArray();
            for(const QJsonValue& array_el : deleted_ids_array){
               sets.del_ids.insert(array_el.toString());
            }
        }

        QJsonObject statsNewVerIdsClassesObj = jsonObj["new-ver"].toObject()[tldType].toObject();
        if(statsNewVerIdsClassesObj["new"].isArray()){
            QJsonArray new_ids_array = statsNewVerIdsClassesObj["new"].toArray();
            for(const QJsonValue& array_el : new_ids_array){
               sets.new_ids.insert(array_el.toString());
            }
        }
        _tldInfoMap[tldType] = sets;
    }
}
