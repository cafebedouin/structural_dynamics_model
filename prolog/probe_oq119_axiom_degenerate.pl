% G0.2 parity control (the degenerate direction): does the divergence detector report
% AGREEMENT when readings classify identically, rather than fabricating divergence?
% Two-sided with the positive control already witnessed (acceptable_risk DIVERGES).
:- initialization((main, halt)).

main :-
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, 'testsets_haiku')),
    [stack], corpus_loader:load_all_testsets,
    % Enumerate multi-reading kernels; split into diverges vs agrees-everywhere.
    findall(K, ( narrative_ontology:cs_kernel_id(_, K) ), Ks0), sort(Ks0, Ks),
    findall(K, ( member(K, Ks), cs_kernel_registry:cs_kernel_coverage(K, N), N >= 2 ), MKs),
    partition([K]>>( cs_kernel_registry:cs_kernel_divergence(K, _, _, _) ), MKs, Diverge, Agree),
    length(MKs, NM), length(Diverge, ND), length(Agree, NA),
    format('multi-reading kernels=~w  DIVERGE(>=1 ctx)=~w  AGREE(no ctx)=~w~n', [NM, ND, NA]),
    % Positive control: a kernel that diverges (detector fires on real disagreement).
    ( Diverge = [DK|_] -> format('POSITIVE (fires on disagreement): ~w diverges~n', [DK]) ; true ),
    % Degenerate control: a kernel whose readings AGREE at every context -> detector
    % returns NO divergence (agreement reported, not fabricated). Show its status is
    % still a typed/real value, not a defaulted one.
    ( Agree = [AK|_]
    ->  cs_kernel_registry:cs_kernel_obstruction_status(AK, ASt),
        format('DEGENERATE (agree everywhere): ~w  divergence=NONE  obstruction_status=~w~n', [AK, ASt]),
        format('   -> agreement is REPORTED (no divergence solution), not fabricated. Two-sided control closed.~n')
    ;   format('DEGENERATE: no agree-everywhere kernel found; constructing identity control...~n'),
        % Fallback identity control: a reading cannot diverge from itself (UID1 @< UID2 fails).
        ( narrative_ontology:cs_kernel_id(C, K2), cs_kernel_registry:cs_kernel_coverage(K2, N2), N2 >= 2
        ->  ( cs_kernel_registry:cs_kernel_divergence(K2, _, U-C, U-C)
            -> format('   IDENTITY CONTROL FAILED: self-pair diverged (bug)~n')
            ;  format('   IDENTITY CONTROL OK: reading ~w does not diverge from itself~n', [C]) )
        ;  true )
    ).
