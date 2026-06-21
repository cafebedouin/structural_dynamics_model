% Positive control: prove the Backed bit is REACHABLE in both states (not
% vacuously always-true). If classify_at_time/5 can return Backed=false, then a
% Backed=true read is a genuine check, not a pass-on-absence (Pattern 5 guard).
:- initialization((main, halt)).

main :-
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, 'testsets_haiku')),
    [stack],
    corpus_loader:load_all_testsets,
    logical_fingerprint:standard_context_for_power(analytical, Ctx),
    % Thin scalar-only story: classify at an arbitrary time -> expect Backed=false
    % (no base_extractiveness measurement at that time => EpsBacked=false).
    Thin = digital_money_emergence_boundary__consumer_holdings_reading,
    once(drl_composition:classify_at_time(Thin, 0, Ctx, T1, Snap1)),
    format('THIN  ~w @t0 -> type=~w snap=~w~n', [Thin, T1, Snap1]),
    % Rich story at an authored series time -> expect Backed=true.
    Rich = acceptable_risk_energy__expected_value_dominant,
    once(drl_composition:classify_at_time(Rich, 0, Ctx, T2, Snap2)),
    format('RICH  ~w @t0 -> type=~w snap=~w~n', [Rich, T2, Snap2]),
    ( Snap1 = snap(_,false,_,_,_) -> format('CONTROL PASS: Backed=false is reachable (thin).~n', [])
    ; format('CONTROL CONCERN: thin story did not report Backed=false.~n', []) ),
    ( Snap2 = snap(_,true,_,_,_) -> format('CONTROL PASS: Backed=true reachable (rich).~n', [])
    ; format('CONTROL CONCERN: rich story did not report Backed=true.~n', []) ).
