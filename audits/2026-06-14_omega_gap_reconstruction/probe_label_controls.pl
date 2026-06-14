% Positive/negative controls for the labeling logic (isolated from type compute).
:- initialization(main).
:- [stack].
:- use_module(report_generator).

t(Name, Readings, Expected) :-
    (   report_generator:label_gap(Readings, P, Lo, Hi)
    ->  Got = gap(P,Lo,Hi)
    ;   Got = fail ),
    ( Got = Expected -> S = 'PASS' ; S = '**FAIL**' ),
    format("~w\t~w\tgot=~w\texpected=~w~n", [S, Name, Got, Expected]).

main :-
    % POS control extraction_blindness: extractive seat LOWER power (higher d)
    t(eb_pos, [reading(1.0,powerless,snare,a), reading(0.0,institutional,rope,b)],
              gap(extraction_blindness,snare,rope)),
    % POS control general: two functional types, no extractive => general
    t(gen_pos_functional, [reading(1.0,powerless,mountain,a), reading(0.0,institutional,rope,b)],
              gap(general_type_mismatch,mountain,rope)),
    % NEG control: extractive seat HIGHER power than functional => NOT eb => general
    t(eb_neg_power_order, [reading(0.0,institutional,snare,a), reading(1.0,powerless,rope,b)],
              gap(general_type_mismatch,rope,snare)),
    % NEG control: single type => label has no >=2-distinct caller guard, but
    % general clause needs THi \= TLo, extraction needs both kinds => must FAIL.
    t(single_type_fails, [reading(1.0,powerless,rope,a), reading(0.0,institutional,rope,b)],
              fail),
    % POS control: tangled_rope (extractive) lower power vs naturalized (functional)
    t(eb_tangled, [reading(0.6,moderate,tangled_rope,a), reading(0.0,institutional,naturalized,b)],
              gap(extraction_blindness,tangled_rope,naturalized)),
    halt.
