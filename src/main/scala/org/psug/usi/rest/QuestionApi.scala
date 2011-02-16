package org.psug.usi.rest

/*
 * Obtenir la question N
 *
 *  * URI : .../api/question/N
 *  * méthode GET
 *  * Paramètre retourné 
 *
 * {
 *   "question" : "string",
 *   "answer_1" : "string",
 *   "answer_2" : "string",
 *   "answer_3" : "string",
 *   "answer_4" : "string",
 *   "score" : number
 * }
 *
 *  * Codes de retours
 *        o OK : 200
 *        o Clé de session non reconnue : 401
 *        o Non respect de la séquence ou autre erreur : 400
 *  * Commentaires
 *        o Retourne la question N ainsi que les réponses possibles et le score actuel de l'utilisateur.
 *        o Cette API utilise un mécanisme de long polling : le serveur ne retourne sa réponse qu'au 
 *          bout d'un certain temps. Le serveur attend qu'il y ait un nombre X de joueurs. Ce nombre X
 *          est défini dans les paramètres du jeu fournis lors d'un appel à 'games'.
 *        o Le serveur doit contrôler que la requête est temporellement valide, c'est à dire qu'elle ne 
 *          dépasse pas la fenêtre temporelle de la question N. Par exemple, une erreur doit être retournée 
 *          si l'utilisateur demande la question 10 alors que c'est la question 1 qui doit être jouée.
 *        o L'authentification de l'utilisateur se fait par le cookie 'session_key'.
 */

class QuestionApi {
  //TODO
}