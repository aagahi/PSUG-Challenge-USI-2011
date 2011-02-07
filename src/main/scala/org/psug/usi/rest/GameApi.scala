package org.psug.usi.rest


/*
 * Génération d'une partie
 *
 *  * URI : .../api/game
 *  * méthode POST
 *  * Paramètres d'entrée 
 *
 * {
 *   "authentication_key" : "string",
 *   "parameters" : "string"
 * }
 *
 *  * Codes de retour
 *        o OK : CREATED 201
 *        o Clé d'authentification non reconnue : 401
 *        o Autre erreur : 400
 *  * Commentaires
 *        o Parameters est une chaine JSON qui représente le contenu d'un fichier xml pour la configuration de la partie. Parmi les éléments de ce fichier, nous avons:
 *              + longpollingduration : durée maximale du long polling en secondes. Au bout de cette durée, le serveur répond à toutes les requêtes en attente, si il ne l'a pas déjà fait.
 *              + nbusersthreshold : le nombre d'utilisateurs qui doit être atteint pour que la partie commence.
 *              + questiontimeframe : à partir du moment où le serveur répond aux requêtes question, les utilisateurs ont un temps maximum pour répondre à la question. Ce paramètre indique cette durée en secondes.
 *              + nbquestions : le nombre de questions à jouer. Doit être inférieur ou égal au nombre de questions présentes dans le fichier (élement <usi:questions>).
 *              + flushusertable : un booleen indiquant si toutes les données utilisateurs (liste des utilisateurs, scores et historiques) doivent être supprimées.
 *              + trackeduseridmail : ne pas tenir compte de ce paramètre.
 *        o Chaque équipe de challengers devra fournir à OCTO une clé d'authentification 'authentication_key' pour accéder à cette API.
 */

class GameApi {

  //TODO 
  
}