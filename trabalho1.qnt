//Coisas a serem resolvidas:
// 2 - Ritual
// é só para o inimigo

// para rodar invariante 
//  quint run trabalho1.qnt --invariant inv


module rpg {

import basicSpells.* from "../spells/basicSpells"
import rareSpells.* from "../spells/rareSpells"

  type CriaturaType = Sacerdote | Bardo | Necromante | Inimigo
  type Status =  Paralisado | Cantando | Nada
  type Acao = Curar | Paralisar | RemoverParalisia | Cantar | LancaSangue | Atacar | Ritual
  type ModificadorDano = DanoSimples | DanoTriplo | NaoDano | DanoDuplo | DanoSextuplo

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

      criaturas' = Map(
          "Sacerdote" -> {nome: "Sacerdote", hp: 20, maxhp: 20, team: "Personagens", status: Nada, tipo: Sacerdote,   dado: d20_sacerdote},
          "Bardo" -> {nome: "Bardo", hp: 20, maxhp: 20, team: "Personagens", status: Nada, tipo: Bardo,    dado: d20_bardo },
          "Necromante" -> {nome: "Necromante",hp: 20, maxhp: 20, team: "Personagens", status: Nada,  tipo: Necromante,    dado: d20_necromante},
          "Inimigo1" -> {nome: "Inimigo1",hp: 90, maxhp: 90, team: "Inimigos",  status: Nada, tipo: Inimigo,    dado: d20_inimigo1 }
        ),
        contadorTurno' = 0,
        lastAttack' = None,
        contadorInimigo' = 0,
      }
  }

  val listaTurnos = criaturas.values().toList((c1, c2) => intCompare(c2.dado, c1.dado))
  val ritualista = listaTurnos.select(p => p.team == "Inimigos")[0]
  val atacante = listaTurnos[contadorTurno % criaturas.keys().size()]

  
  action step = {
    nondet receptor = criaturas.values().filter(p => atacante != p).oneOf()
    all {
      acao(atacante, receptor), 
     // verificaRitual(), // nao da pra atualizar mais de uma coisa por vez
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
    if (bardo.status == Cantando and receptor.nome != "Bardo"){
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

  action lancaSangue(atacante: Criatura, receptor: Criatura): bool = all{ //passar aqui um cara?
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

  action acao(atacante: Criatura, receptor: Criatura): bool = all{
      atacante.hp > 0,
      atacante.status != Paralisado,
      
      if(atacante.tipo == Inimigo) {
        if (ritualista.nome == atacante.nome and contadorInimigo == 3){
          all{
            criaturas' = criaturas.transformValues(p => if(p.team == "Personagens") {...p, hp: 0} else p),
            lastAttack' = Some({ataca: atacante.nome, recebe: "Nenhum", acao: Ritual, modificador: NaoDano})
            }
        }
            
        else{
             any{ 
            paralisar(atacante, receptor),
            ataque(atacante, receptor)
            } 
          }
      } else if(atacante.tipo == Sacerdote) {
         any{ 
          removerParalisia(atacante, receptor),
          ataque(atacante,receptor),
          curar(atacante)
        } 
      } else if(atacante.tipo == Bardo) {
        any{ 
          removerParalisia(atacante, receptor),
          ataque(atacante,receptor),
          cantar(atacante)
        } 
      } 
      else{ // nosso necromante
        any{ 
          removerParalisia(atacante, receptor),
          ataque(atacante,receptor),
          lancaSangue(atacante,receptor)
        } 
      }
  
  }

  //val inv3 = criaturas.values().forall(p => p.nome == "Bardo" implies p.status != Cantando) // bardo nunca canta
  //val inv = criaturas.values().exists(c => c.team == "Inimigos") implies criaturas.values().forall(p => p.team == "Inimigos" implies p.hp == 0)


  val inv1 = criaturas.values().forall(p => p.team == "Inimigos" implies p.hp > 0) // inimigo não morre

  val inv2 =  criaturas.values().forall(p => p.team == "Personagens" implies p.hp > 0) // nenhum personagem vive

  val invRitual = not(criaturas.values().forall(p => p.team == "Personagens" implies p.hp == 0)) // verdade que todos os personagens morrem 
}