% Phase B / Killer-1 — condition-false control for the authored-only
% cs_verdict(C, false_natural_law_constraint) path, plus the killer-1
% positive control: the SAME trace method must surface the computed
% Boltzmann dependency on signature_detection:false_natural_law/2.

run(Tier) :-
    retractall(config:param(corpus_path,_)),
    asserta(config:param(corpus_path, Tier)),
    corpus_loader:load_all_testsets,
    aggregate_all(count, corpus_loader:corpus_constraint(_), NC),
    format('~n=== TIER ~w (corpus_constraint=~w) ===~n', [Tier, NC]),

    format('~n--- FNL condition-false control: each natural_law_constraint pattern ---~n'),
    format('  cols: CONSTRAINT | pattern | has_beneficiary | cs_verdict_FNL_fires~n'),
    forall(
        ( corpus_loader:corpus_constraint(C),
          cs_pattern_detection:cs_pattern_is(C, natural_law_constraint) ),
        ( ( narrative_ontology:constraint_beneficiary(C,_) -> HB = yes ; HB = no ),
          ( cs_pattern_detection:cs_verdict(C, false_natural_law_constraint) -> FF = fires ; FF = silent ),
          format('  ~w | natural_law_constraint | beneficiary=~w | FNL=~w~n', [C, HB, FF])
        )),

    format('~n--- Killer-1 positive control: trace surfaces the computed dep where one EXISTS ---~n'),
    format('  For a constraint with self_enforcing pattern, ask both detectors and show their inputs:~n'),
    ( corpus_loader:corpus_constraint(C2),
      cs_pattern_detection:cs_verdict(C2, false_natural_law_constraint)
    ->  format('  example firing constraint: ~w~n', [C2]),
        % authored-only path: cs_verdict FNL reads only authored facts
        ( narrative_ontology:constraint_beneficiary(C2, B) -> true ; B = none ),
        format('    cs_verdict FNL inputs  : cs_pattern_is(natural_law_constraint) [authored AG=self_enforcing] + constraint_beneficiary=~w [authored]~n', [B]),
        % computed path: does signature_detection:false_natural_law fire / reach Boltzmann?
        ( catch(signature_detection:false_natural_law(C2, Ev),_,fail)
        ->  format('    signature FNL          : FIRES with evidence ~w~n', [Ev])
        ;   ( catch(signature_detection:claimed_natural(C2, Claim),_,fail)
            ->  ( catch(boltzmann_compliance:boltzmann_compliant(C2, BR),_,(BR=error))
                ->  format('    signature FNL          : silent; but computed path REACHED -> claimed_natural=~w, boltzmann_compliant=~w~n', [Claim, BR])
                ;   format('    signature FNL          : silent; claimed_natural=~w, boltzmann probe errored~n', [Claim]) )
            ;   format('    signature FNL          : silent; claimed_natural/2 false for this C (no naturality CLAIM authored)~n', [])
            )
        )
    ;   format('  (no false_natural_law_constraint firing in this tier)~n')
    ),
    format('~n=== END ~w ===~n', [Tier]).
