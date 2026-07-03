:- initialization(main).
:- [stack].
main :-
  retractall(config:param(corpus_path,_)), assertz(config:param(corpus_path,'archives/datasets/kernel_v1')),
  corpus_loader:load_all_testsets,
  findall(C, corpus_loader:corpus_constraint(C), Cs),
  aggregate_all(count, (member(C,Cs), narrative_ontology:constraint_claim(C,mountain)), NMtn),
  aggregate_all(count, (member(C,Cs), signature_detection:claimed_natural(C,_)), NClaimNat),
  aggregate_all(count, (member(C,Cs), catch(boltzmann_compliance:boltzmann_compliant(C,non_compliant(_,_)),_,fail)), NNonComp),
  aggregate_all(count, (member(C,Cs), signature_detection:claimed_natural(C,_), catch(boltzmann_compliance:boltzmann_compliant(C,non_compliant(_,_)),_,fail)), NBoth),
  aggregate_all(count, (member(C,Cs), signature_detection:false_natural_law(C,_)), NFNLraw),
  % positive control: FNL detector CAN fire (synthetic)
  ( assertz(narrative_ontology:constraint_claim(pc_kv1_synth, mountain)),
    ( signature_detection:claimed_natural(pc_kv1_synth, explicit_mountain_claim) -> PC=ok ; PC=fail ),
    retractall(narrative_ontology:constraint_claim(pc_kv1_synth,_)) ),
  format("kernel_v1: mountain_claims=~w  claimed_natural(any)=~w  boltzmann_noncompliant=~w  BOTH(claim&noncomp)=~w  false_natural_law/2 raw=~w~n",
         [NMtn,NClaimNat,NNonComp,NBoth,NFNLraw]),
  format("PC_CLAIMED_NATURAL_REACHABLE ~w~n",[PC]),
  halt.
main :- write(fail), halt(1).
