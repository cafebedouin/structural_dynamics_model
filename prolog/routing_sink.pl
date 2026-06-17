% ============================================================================
% ROUTING SINK — the natural_law author↔engine diff (OQ-128)
% Design: audits/2026-06-17_mountain_authoring_sweep/ROUTING_SINK_DESIGN.md
% ============================================================================
%
% PRINCIPLE (§0): the engine ROUTES disagreement; it NEVER reclassifies. Only
% review reclassifies. This module consumes the per-SEAT author↔engine diff and
% assigns each seat a router ADDRESS (a LABEL — it certifies nothing). It does
% not change any classification, does not write back, and taps dr_claim_mismatch/4
% UNMODIFIED.
%
% LEAF UNIT (§3): the atomic record is per-SEAT — diff(Constraint, Seat). A
% constraint emits N seat-diffs (N = standard_context count = 4). NO predicate
% here collapses seats into one constraint verdict — that aggregate-merge is the
% failure that recurred three times in this arc (KILL condition, §9b.4).
%
% THREE READINGS (§2):
%   - author   : constraint_classification(C,T,Seat) (per-seat, archives) else
%                the seat-blind constraint_claim(C,T) (live corpus authors ONE
%                claim, not per-seat). A set; [t1,t2] = self-contradiction.
%   - engine   : dr_type(C, Seat, T). unknown = honest abstain = ENGINE_SILENT.
%   - detector : constraint_signature(C, natural_law) — a SOCKETED router input,
%                currently unpowered (HasAlternatives==false is builder-unreachable,
%                §9a(i)); records nl_fired|nl_absent, never used to reclassify.
%
% ADDRESSES — SEVEN typed, MECE buckets, no catch-all (operator ruling 2026-06-17:
% split the former unrouted_residual reasons into first-class addresses). The four
% §4 buckets:
%   generation_gap          : AUTHOR_SILENT at a seat the engine classified.
%   authoring_review        : author self-contradicts at the seat ([mountain,rope]).
%   engine_exit_table_review: author uniform-mountain + engine degrades (= the
%                             dr_claim_mismatch type_1 seats with a real engine type).
%   no_route                : author's single reading == engine's reading (agreement /
%                             presheaf reproducing authored divergence).
% Plus three the §4 mountain-sweep didn't name but the (unknown-heavy) live corpus
% needs — collapsing an engine abstain into no_route would fake agreement (Pattern 6):
%   both_silent             : neither author nor engine has a reading at the seat.
%   engine_abstained        : author has a reading; engine produced unknown (≠ agreement).
%   author_engine_divergence: both speak and disagree, but not a uniform-mountain degrade.
% Each address is self-describing; there is no provenance.residual_reason.
% ============================================================================

:- module(routing_sink, [
    seat_diff/7,                 % seat_diff(C, Seat, Author, Engine, Detector, Address, Provenance)
    seat_diff_record/2,          % seat_diff_record(?C, -Dict)  (one dict per (C,Seat))
    routing_sink_records/1,      % routing_sink_records(-ListOfDicts)
    routing_sink_address_counts/1, % routing_sink_address_counts(-Pairs)  Address-Count
    run_routing_sink/0,          % CLI: ensure corpus loaded, write ../outputs/routing_sink.json
    routing_sink_emit_to/1       % routing_sink_emit_to(+Path)  (assumes corpus loaded)
]).

:- use_module(library(lists)).
:- use_module(library(http/json)).
:- use_module(library(apply)).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(narrative_ontology).
:- use_module(signature_detection).

% ---------------------------------------------------------------------------
% Seat label: the agent_power atom is the human-readable seat id.
% ---------------------------------------------------------------------------
seat_power(context(agent_power(P), _, _, _), P).

% ---------------------------------------------------------------------------
% Author reading at a seat.
%   Reading = sorted list of authored types (singleton, or [t1,t2,..] = contradiction)
%           | author_silent
%   Mode    = per_seat | seat_blind_claim | none
% Per-seat constraint_classification takes precedence (archives); the live
% corpus has none, so it falls back to the seat-blind constraint_claim.
% ---------------------------------------------------------------------------
% A constraint that authors per-seat classifications ANYWHERE uses the per-seat
% schema: a seat with no fact of its own is then AUTHOR_SILENT *at that seat*
% (per-seat silence — the generation_gap signal), NOT backfilled from the global
% claim. Only a constraint with NO per-seat authoring (the whole live corpus)
% falls back to the seat-blind constraint_claim. Backfilling per-seat silence
% from the global claim would mask the very signal generation_gap routes.
% The SEAT IDENTITY is the agent_power, not the full context term. Authored
% per-seat facts pair a power level with whatever time_horizon/exit/scope the
% perspective used (e.g. topological's powerless perspective is authored at
% civilizational/universal, not the standard biographical/local), so author and
% engine are aligned by POWER: the engine reads at its canonical standard_context
% frame for that power; the author's reading at that power is every
% constraint_classification whose context carries agent_power(Power).
author_reading_at(C, Seat, Reading, Mode) :-
    seat_power(Seat, Power),
    (   constraint_uses_per_seat(C)
    ->  findall(T, ( constraint_indexing:constraint_classification(C, T, Ctx),
                     seat_power(Ctx, Power) ), PerSeat),
        (   PerSeat \= []
        ->  sort(PerSeat, Reading), Mode = per_seat
        ;   Reading = author_silent, Mode = per_seat_silent
        )
    ;   findall(T, narrative_ontology:constraint_claim(C, T), Claims),
        (   Claims \= []
        ->  sort(Claims, Reading), Mode = seat_blind_claim
        ;   Reading = author_silent, Mode = none
        )
    ).

constraint_uses_per_seat(C) :-
    constraint_indexing:constraint_classification(C, _, _), !.

% ---------------------------------------------------------------------------
% Engine reading at a seat. dr_type never fails (unknown catch-all); unknown is
% the honest abstain, surfaced as engine_silent for routing (recorded distinctly
% in provenance so abstain never reads as a real verdict).
% ---------------------------------------------------------------------------
engine_reading_at(C, Seat, Engine) :-
    (   once(drl_core:dr_type(C, Seat, T)), T \== unknown
    ->  Engine = T
    ;   Engine = engine_silent
    ).

% ---------------------------------------------------------------------------
% Detector (constraint-level, not seat-level): a socketed router input.
% ---------------------------------------------------------------------------
detector_state(C, nl_fired) :-
    signature_detection:constraint_signature(C, natural_law), !.
detector_state(_, nl_absent).

% ---------------------------------------------------------------------------
% Constraint-level: is the author uniform-mountain across ALL seats? (The
% address input that distinguishes "uniform because immovable" candidates —
% itself CONTAMINATED, §4, hence a router label, never a certification.)
% ---------------------------------------------------------------------------
author_uniform_mountain(C) :-
    forall( drl_core:standard_context(S),
            ( author_reading_at(C, S, R, _), R == [mountain] ) ).

% ---------------------------------------------------------------------------
% Typed mismatch tap — dr_claim_mismatch/4 UNMODIFIED (§9b.1).
% ---------------------------------------------------------------------------
seat_mismatch(C, Seat, ErrorType-Severity) :-
    drl_core:dr_claim_mismatch(C, Seat, ErrorType, Severity), !.
seat_mismatch(_, _, none).

% ---------------------------------------------------------------------------
% The router address (a LABEL — certifies nothing). SEVEN typed, MECE addresses;
% no catch-all (operator ruling 2026-06-17: split the former unrouted_residual
% reasons into first-class addresses rather than bless a fourth-and-a-half
% category). The four §4 buckets + three that the §4 mountain-sweep didn't name
% but the live corpus needs (an engine abstain must NOT collapse into no_route —
% that would fake agreement, Pattern 6): both_silent / engine_abstained /
% author_engine_divergence. Each address is self-describing; no residual_reason.
% ---------------------------------------------------------------------------
route_address(C, _Seat, Author, Engine, Address) :-
    (   Author == author_silent, Engine \== engine_silent
    ->  Address = generation_gap            % author silent; engine classified
    ;   Author == author_silent, Engine == engine_silent
    ->  Address = both_silent               % neither side has a reading
    ;   is_list(Author), length(Author, NA), NA >= 2
    ->  Address = authoring_review          % author self-contradicts at the seat
    ;   Author = [A], Engine == A
    ->  Address = no_route                  % author reading == engine reading
    ;   Author == [mountain], Engine \== mountain, Engine \== engine_silent,
        author_uniform_mountain(C)
    ->  Address = engine_exit_table_review  % uniform-mountain author, engine degrades
    ;   Engine == engine_silent
    ->  Address = engine_abstained          % author claims; engine unknown (≠ agreement)
    ;   Address = author_engine_divergence  % both speak, disagree, not a uniform-mtn degrade
    ).

% ---------------------------------------------------------------------------
% The leaf: one per (Constraint, Seat). Nondeterministic over corpus × seats
% when C/Seat are unbound.
% ---------------------------------------------------------------------------
seat_diff(C, Seat, Author, Engine, Detector, Address, Provenance) :-
    corpus_loader:corpus_constraint(C),
    drl_core:standard_context(Seat),
    author_reading_at(C, Seat, Author, AuthorMode),
    engine_reading_at(C, Seat, Engine),
    detector_state(C, Detector),
    route_address(C, Seat, Author, Engine, Address),
    seat_mismatch(C, Seat, Mismatch),
    ( author_uniform_mountain(C) -> UnifMtn = true ; UnifMtn = false ),
    Provenance = provenance{
        author_mode: AuthorMode,
        mismatch: Mismatch,
        author_uniform_mountain: UnifMtn
    }.

% ---------------------------------------------------------------------------
% Dict form (for JSON / consumers). Typed silence is explicit, never blank (§5).
% ---------------------------------------------------------------------------
seat_diff_record(C, Dict) :-
    seat_diff(C, Seat, Author, Engine, Detector, Address, Prov),
    seat_power(Seat, Power),
    author_field(Author, Prov.author_mode, AuthorField),
    engine_field(Engine, EngineField),
    mismatch_field(Prov.mismatch, MismatchField),
    Dict = _{
        constraint: C,
        seat: Power,
        author: AuthorField,
        engine: EngineField,
        detector: Detector,
        address: Address,
        provenance: _{
            author_mode: Prov.author_mode,
            engine_state: EngineField,
            detector: Detector,
            mismatch: MismatchField,
            author_uniform_mountain: Prov.author_uniform_mountain
        }
    }.

author_field(author_silent, _, "AUTHOR_SILENT(no constraint_classification/constraint_claim at this seat)") :- !.
author_field(Reading, Mode, Field) :-
    term_string(Reading-Mode, S),
    string_concat("supplied=", S, Field).

engine_field(engine_silent, "ENGINE_SILENT(dr_type=unknown, honest abstain)") :- !.
engine_field(T, Field) :- term_string(T, S), string_concat("dr_type=", S, Field).

mismatch_field(none, "none") :- !.
mismatch_field(E-Sev, Field) :- term_string(E-Sev, Field).

% ---------------------------------------------------------------------------
% All records (per-seat; the list length MUST be N_constraints × N_seats —
% the per-seat-unit invariant, checked by the witness, KILL §9b.4).
% ---------------------------------------------------------------------------
routing_sink_records(Records) :-
    findall(D, seat_diff_record(_, D), Records).

routing_sink_address_counts(Pairs) :-
    findall(A, seat_diff(_,_,_,_,_,A,_), As),
    msort(As, Sorted),
    address_counts(Sorted, Pairs).

address_counts([], []).
address_counts([A|As], [A-N|Rest]) :-
    take_run(A, [A|As], Run, Tail),
    length(Run, N),
    address_counts(Tail, Rest).

take_run(A, [A|Xs], [A|Run], Tail) :- !, take_run(A, Xs, Run, Tail).
take_run(_, Xs, [], Xs).

% ---------------------------------------------------------------------------
% Emit. Manifest carries coverage so a read site can never mistake didn't-look
% for measured-empty (Pattern 6).
% ---------------------------------------------------------------------------
routing_sink_emit_to(Path) :-
    routing_sink_records(Records),
    findall(C, corpus_loader:corpus_constraint(C), Cs0), sort(Cs0, Cs),
    length(Cs, NConstraints),
    findall(S, drl_core:standard_context(S), Seats), length(Seats, NSeats),
    length(Records, NRecords),
    routing_sink_address_counts(AddrPairs),
    maplist(pair_to_dict, AddrPairs, AddrDicts),
    ( NRecords =:= NConstraints * NSeats -> Invariant = true ; Invariant = false ),
    Out = _{
        manifest: _{
            n_constraints: NConstraints,
            n_seats: NSeats,
            n_records: NRecords,
            per_seat_invariant_holds: Invariant,
            address_counts: AddrDicts
        },
        records: Records
    },
    setup_call_cleanup(
        open(Path, write, S),
        json_write_dict(S, Out, [width(80)]),
        close(S)
    ),
    format(user_error, '[routing_sink] wrote ~w (~w records = ~w x ~w)~n',
           [Path, NRecords, NConstraints, NSeats]).

pair_to_dict(A-N, _{address: A, count: N}).

run_routing_sink :-
    corpus_loader:ensure_corpus_loaded,
    routing_sink_emit_to('../outputs/routing_sink.json').
