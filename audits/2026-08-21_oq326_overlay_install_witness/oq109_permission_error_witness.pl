t :- format("--- does the oq109 assert list throw TODAY, and does it leave state dirty? ---~n"),
     Facts = [ narrative_ontology:constraint_metric(oq109_seam_nl, extractiveness, 0.03),
               narrative_ontology:constraint_claim(oq109_seam_nl, mountain),
               domain_priors:emerges_naturally(oq109_seam_nl) ],
     ( catch(probe_harness:with_asserted(Facts, true), E, (format("THREW: ~q~n",[E]), true))
     -> true ; format("goal failed~n") ),
     format("--- post-state (cleanup should have removed ALL of these) ---~n"),
     ( narrative_ontology:constraint_metric(oq109_seam_nl,_,_)
     -> format("  *** LEAKED: constraint_metric(oq109_seam_nl,...) PERSISTS~n")
     ;  format("  clean: constraint_metric gone~n") ),
     ( catch(narrative_ontology:constraint_claim(oq109_seam_nl,_),_,fail)
     -> format("  *** LEAKED: constraint_claim(oq109_seam_nl,...) PERSISTS~n")
     ;  format("  clean: constraint_claim gone~n") ).
