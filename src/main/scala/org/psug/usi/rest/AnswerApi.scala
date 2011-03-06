package org.psug.usi.rest

/*
 * Répondre à une question N
 *
 *  * URI : .../api/answer/N
 *  * méthode POST
 *  * Paramètre d'entrée 
 *
 * {
 *   "answer" : number
 * }
 *
 *  * Paramètre de retour 
 *
 * {
 *   "are_u_right" : "boolean",
 *   "good_answer" : "string",
 *   "scoreSlice" : number
 * }
 *
 *  * Codes de retour
 *        o OK : CREATED 201
 *        o Clé de session non reconnue : 401
 *        o Autre erreur : 400
 *
 *  * Commentaires
 *        o Enregistre la réponse de l'utilisateur et retourne la bonne réponse et le statut 
 *          de l'utilisateur. Chaque participant ne peut répondre qu'une fois à chaque question. 
 *          Par ailleurs, le serveur doit valider que la réponse n'arrive pas trop tard ou 
 *          trop tôt par rapport à l'intervalle de réponse possible.
 *        o L'authentification de l'utilisateur se fait par le cookie 'session_key'.
 */

class AnswerApi {
 //TODO
}