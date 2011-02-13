package org.psug.usi.rest

/*
 * Obtenir le classement d'un utilisateur
 *
 *  * URI : .../api/ranking
 *  * méthode GET
 *  * Paramètre de retour 
 *
 *  {
 *    "top_scores" : {
 *      "email" : [ "string", "string", ...],
 *      "scores" : [ number, number, ...]
 *    }
 *    "before_me" : {
 *      "email" : ["string", "string", ...],
 *      "scores" : [number, number, ...]
 *    }
 *    "after_me" : {
 *      "email" : ["string", "string", ...],
 *      "scores" : [number, number, ...]
 *    }
 *  }
 *
 *  * Codes de retour
 *        o OK 200
 *        o Clé de session non reconnue : 401
 *        o Autre : 400
 *  * Commentaires
 *        o Retourne les 100 premiers scores (couple email/score) et les 100 scores avoisinant
 *          l'utilisateur (50 avant et 50 après). Si l'utilisateur courant est classé 40ème, 
 *          (donc dans le 50 premiers) seuls les 39 utilisateurs le précédant et les 
 *          50 suivants seront retournés.
 *        o L'authentification de l'utilisateur se fait par le cookie 'session_key'.
 */

class RankingApi {
  //TODO
}