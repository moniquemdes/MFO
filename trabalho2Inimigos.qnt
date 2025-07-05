// para rodar invariante 
//  quint run trabalho1.qnt --invariant inv

module rpg {

import basicSpells.* from "../spells/basicSpells"
import rareSpells.* from "../spells/rareSpells"

  type CriaturaType = Sacerdote | Bardo | Necromante | Inimigo
  type Status =  Paralisado | Cantando | Nada
  type Acao = Curar | Paralisar | RemoverParalisia | Cantar | LancaSangue | Atacar | Ritual
  type ModificadorDano = DanoSimples | DanoTriplo | NaoDano | DanoSextuplo | DanoDuplo

  type DescriacaoAtaque = { ataca: str, recebe: str, acao: Acao, modificador: ModificadorDano } // bardo, proximo que jogar vai ter mais dano
  

    var criaturas: str -> Criatura
    var contadorTurno: int
    var lastAttack: Option[DescriacaoAtaque]
    var contadorInimigo: int

  type Criatura = {
      nome: str,
      hp: int,
      maxhp: int,
      team: str,
      status: Status,
      tipo: CriaturaType,
      dado: int
    }

  action init =  { 
      all {
      nondet d20_sacerdote = oneOf(1.to(20)) // um numero de dado para cada
      nondet d20_bardo = oneOf(1.to(20))
      nondet d20_necromante = oneOf(1.to(20))
      nondet d20_inimigo1 =  oneOf(1.to(20))
      nondet d20_inimigo2 =  oneOf(1.to(20))

      criaturas' = Map(
          "Sacerdote" -> {nome: "Sacerdote", hp: 20, maxhp: 20, team: "Personagens", status: Nada, tipo: Sacerdote,   dado: d20_sacerdote},
          "Bardo" -> {nome: "Bardo", hp: 20, maxhp: 20, team: "Personagens", status: Nada, tipo: Bardo,    dado: d20_bardo },
          "Necromante" -> {nome: "Necromante",hp: 20, maxhp: 20, team: "Personagens", status: Nada,  tipo: Necromante,    dado: d20_necromante},
          "Inimigo1" -> {nome: "Inimigo1",hp: 90, maxhp: 90, team: "Inimigos",  status: Nada, tipo: Inimigo,    dado: d20_inimigo1 },
          "Inimigo2" -> {nome: "Inimigo2",hp: 90, maxhp: 90, team: "Inimigos",  status: Nada, tipo: Inimigo,    dado: d20_inimigo2 }
        ),
        contadorTurno' = 0,
        lastAttack' = None,
        contadorInimigo' = 0,
      }
  }

  val listaTurnos = criaturas.values().toList((c1, c2) => intCompare(c2.dado, c1.dado))
  val ritualista = listaTurnos.select(p => p.team == "Inimigos")[0]
  val atacante = listaTurnos[contadorTurno % criaturas.keys().size()]
  val proximoAtacante = listaTurnos[(contadorTurno + 1) % listaTurnos.length()]

  
  action step = {
    nondet receptor = criaturas.values().filter(p => atacante != p).oneOf()
    all {
      acao(atacante, receptor), 
    if(atacante == ritualista){
      contadorInimigo' = contadorInimigo + 1
    }
    else {
    contadorInimigo' = contadorInimigo
    },

      contadorTurno' = contadorTurno + 1
    }
  }


  action ataque(atacante: Criatura, receptor:Criatura): bool = all{
    atacante.hp > 0,
    atacante.team != receptor.team,
    receptor.hp > 0,
    val bardo = criaturas.get("Bardo")
    if (bardo.status == Cantando){
      all {
       criaturas' = criaturas.setBy(receptor.nome, p => { ...p, hp: p.hp -  30}).setBy(bardo.nome, p=> {...p, status: Nada}),
      lastAttack' = Some({ataca: atacante.nome, recebe: receptor.nome, acao: Atacar, modificador: DanoTriplo})
      }
    }
    else{
      all {
       criaturas' = criaturas.setBy(receptor.nome, p => { ...p, hp: p.hp -  10}),
        lastAttack' = Some({ataca: atacante.nome, recebe: receptor.nome, acao: Atacar, modificador: DanoSimples})
      }
    }
   
  }

  action paralisar(atacante: Criatura, receptor: Criatura): bool = all {
    atacante.hp > 0,
    receptor.hp > 0,
    atacante.team == "Inimigos",
    receptor.team == "Personagens",
    criaturas' = criaturas.setBy(receptor.nome, p => {...p, status: Paralisado}),
    lastAttack' = Some({ataca: atacante.nome, recebe: receptor.nome, acao: Paralisar, modificador: NaoDano})
  }

  action removerParalisia(atacante: Criatura, receptor: Criatura): bool = all{
    atacante.team == "Personagens",
    receptor.team == "Personagens",
    atacante.hp > 0,
    receptor.hp > 0,
    atacante.nome != receptor.nome,
    receptor.status == Paralisado,
    criaturas' = criaturas.setBy(receptor.nome, p => { ...p, status: Nada }),
    lastAttack' = Some({ataca: atacante.nome, recebe: receptor.nome, acao: RemoverParalisia, modificador: NaoDano})
  }

  action lancaSangue(atacante: Criatura, receptor: Criatura): bool = all{ 
    atacante.hp > 0,
    atacante.team != receptor.team,
    receptor.hp > 0,
    atacante.nome == "Necromante",
    val bardo = criaturas.get("Bardo")
    if (bardo.status == Cantando){
      all {
      criaturas' = criaturas.setBy(receptor.nome, p => { ...p, hp: p.hp - 60}).setBy(atacante.nome, p => { ...p, hp: p.hp - 10}).setBy(bardo.nome, p=> {...p, status: Nada}), // duas coisas sendo atualizadas
      lastAttack' = Some({ataca: atacante.nome, recebe: receptor.nome, acao: LancaSangue, modificador: DanoSextuplo})
      }
    }
    else{
      all {
      criaturas' = criaturas.setBy(receptor.nome, p => { ...p, hp: p.hp - 20}).setBy(atacante.nome, p => { ...p, hp: p.hp - 10}), // duas coisas sendo atualizadas
      lastAttack' = Some({ataca: atacante.nome, recebe: receptor.nome, acao: LancaSangue, modificador: DanoDuplo})
      }
    }
  }

  action cantar(atacante: Criatura): bool = all {
    atacante.hp > 0, // nao pode ta morto
    atacante.tipo == Bardo,
    atacante.status == Nada, // nao pode estar paralisado
    criaturas' = criaturas.setBy(atacante.nome, p => { ...p, status: Cantando}),
    lastAttack' = Some({ataca: atacante.nome, recebe: "Ninguem especifico", acao: Cantar, modificador: NaoDano})
  }

  action curar(atacante: Criatura): bool = all {
    atacante.hp > 0,
    atacante.nome == "Sacerdote",
    criaturas' = criaturas.transformValues(p => if(p.team == "Personagens" and p.hp > 0) {...p, hp: min(p.maxhp, p.hp + 10)}
    
    else{
      p
    }),
    lastAttack' = Some({ataca: atacante.nome, recebe: "Todos Personagens Vivos Foram Curados", acao: Curar, modificador: NaoDano})
  }

// caso de estrategia: 1- faz o especial da criatura, 2-  tira paralisia, 3- ataque 
// se bardo for cantar e o proximo inimigo -> nao canta
// se o ritualista ta vivo a gente ataca sempre o ritualista
// colocar a estrategia do ritualista vivo no step pq la se ele ta vivo sempre vai ser o receptor da acao 
  action acao(atacante: Criatura, receptor: Criatura): bool = all{
      atacante.hp > 0,
      atacante.status != Paralisado,
      
      if(atacante.tipo == Inimigo) {
        if (ritualista.nome == atacante.nome and contadorInimigo == 3){
          all{
            criaturas' = criaturas.transformValues(p => if(p.team == "Personagens") {...p, hp: 0} else p),
            lastAttack' = Some({ataca: atacante.nome, recebe: "Todos Personagens Foram Mortos", acao: Ritual, modificador: NaoDano})
            }
        }
        else{
            any{ 
            paralisar(atacante, receptor),
            ataque(atacante, receptor)
            } 
        }
      }
      //principal

    else if(contadorInimigo == 3){
        if(atacante.tipo == Sacerdote) {
          ataque(atacante,receptor)
        }  else 
        if(atacante.tipo == Bardo ) {
            if(proximoAtacante.team != "Inimigos") {
            cantar(atacante)
            } else{
                ataque(atacante,receptor)
            }
        } else{ // nosso necromante
        if (atacante.hp == 20){
            lancaSangue(atacante,receptor)
            }
            else{
                ataque(atacante,receptor)
            }
        }
    } else {
       if(atacante.tipo == Sacerdote) { 
            if(criaturas.values().exists(p => p.team =="Personagens" and p.hp < 20 )){
                curar(atacante)
            } else
            if (proximoAtacante.nome == "Necromante" and criaturas.get("Necromante").hp == 20 and criaturas.get("Bardo").status == Cantando and  criaturas.get("Necromante").status == Paralisado) {
                removerParalisia(atacante, criaturas.get("Necromante"))

            } else if (proximoAtacante.nome == "Necromante" and criaturas.get("Necromante").hp == 10 and criaturas.get("Bardo").status == Cantando){ 
                curar(atacante)

            } else if(criaturas.values().exists(p => p.team =="Personagens" and p.status == Paralisado)) {
                removerParalisia(atacante, receptor)
            }
            else {
                ataque(atacante,receptor)
            }
    } else if(atacante.tipo == Bardo) {
            if (proximoAtacante.team != "Inimigos" and proximoAtacante.nome != "Sacerdote"){ // pode ser personagem e remover paralisia ou curar
                cantar(atacante)
            }//so pra estruturar
            else if (criaturas.values().exists(p => p.team =="Personagens" and p.status == Paralisado) and
                    not(criaturas.values().exists(p => p.team =="Personagens" and p.status == Cantando)) ){ 
                        removerParalisia(atacante, receptor)
                        
            }
            else{
                ataque(atacante,receptor)
            }
          
    } else{ //nosso necromante
      // se o necromante tiver mais de 10 de hp
        if((receptor.hp == 60 and criaturas.get("Bardo").status == Cantando and atacante.hp > 10)  or receptor.hp == 20 and atacante.hp > 10){ 
            lancaSangue(atacante,receptor)
        }
        
        else if (criaturas.values().exists(p => p.team =="Personagens" and p.status == Paralisado and p.hp <20)){ 
          removerParalisia(atacante, receptor) 
        }
        else {//
            ataque(atacante,receptor)
        }
      }
        
    }
      
    }
    

  //val inv3 = criaturas.values().forall(p => p.nome == "Bardo" implies p.status != Cantando) // bardo nunca canta
  //val inv = criaturas.values().exists(c => c.team == "Inimigos") implies criaturas.values().forall(p => p.team == "Inimigos" implies p.hp == 0)


  val inv1 = criaturas.values().forall(p => p.team == "Inimigos" implies p.hp > 0) // inimigo não morre

  val inv2 =  criaturas.values().forall(p => p.team == "Personagens" implies p.hp > 0) // nenhum personagem vive

  val invRitual = not(criaturas.values().forall(p => p.team == "Personagens" implies p.hp == 0)) // verdade que todos os personagens morrem 
}