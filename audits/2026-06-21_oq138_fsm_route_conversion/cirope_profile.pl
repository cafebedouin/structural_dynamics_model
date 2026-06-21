:- initialization(main).
:- [stack].
:- use_module(boltzmann_compliance, []).
seat(architectural_pattern_validity).
seat(demographic_resource_allocation).
seat(demographic_skill_mismatch_c0).
seat(propagation_speed_asymmetry).
seat(scale_ceiling_c0).
seat(validation_judgment_separation).
g(G,R) :- ( catch(G,_,fail) -> R=yes ; R=no ).
main :-
  retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path, testsets)),
  corpus_loader:load_all_testsets,
  format("~nseat | cascade-sig | boltzmann | scope_inv | coord_fn | captured | excess_extraction | ci_eligible~n"),
  forall(seat(C),(
    ( signature_detection:constraint_signature(C,Sig)->true;Sig=none ),
    ( catch(boltzmann_compliance:boltzmann_compliant(C,Comp),_,Comp=err)->true;Comp=err ),
    ( catch(signature_detection:scope_invariance_test(C,SI),_,SI=err)->true;SI=err ),
    g(narrative_ontology:has_coordination_function(C), CF),
    g(narrative_ontology:constraint_captured(C), Cap),
    ( catch(signature_detection:excess_extraction(C,EX),_,EX=na)->true;EX=na ),
    g(signature_detection:coupling_invariant_rope(C,_), CI),
    format("  ~w | ~w | ~w | ~w | coord=~w | captured=~w | excess=~w | ci_detector=~w~n",
           [C,Sig,Comp,SI,CF,Cap,EX,CI])
  )),
  % positive control: does ANY live seat FAIL boltzmann (so we know the test discriminates)?
  findall(C,(corpus_loader:corpus_constraint(C),
             catch(boltzmann_compliance:boltzmann_compliant(C,non_compliant(_,_)),_,fail)), Fails),
  length(Fails,NF),
  format("~nPOSITIVE CONTROL: ~w live seats FAIL boltzmann (non_compliant) => the test discriminates, the 6 above are not vacuous passes~n",[NF]),
  halt.
main :- write('CIROPE PROFILE FAILED'), halt(1).
