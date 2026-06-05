% ============================================================================
% CONSTRAINT STORY: apartheid_nuclear_program
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_apartheid_nuclear_program, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: apartheid_nuclear_program
 *   human_readable: Apartheid South Africa's Clandestine Nuclear Program
 *   domain: political/military/technological
 *
 * SUMMARY:
 *   Apartheid South Africa's clandestine nuclear program was a state-level
 *   project designed to ensure the survival of the white minority regime
 *   through nuclear deterrence. Developed in secret from the 1970s, it
 *   produced six functional nuclear weapons before being dismantled on the
 *   eve of the transition to democracy in the early 1990s. The constraint is
 *   the program itself: a massive, coercive, and secret allocation of
 *   national resources that fundamentally altered the strategic landscape for
 *   all actors involved, from the oppressed majority to neighboring states
 *   and global powers.
 *
 * KEY AGENTS:
 *   - Apartheid Regime Elites: Primary beneficiary (institutional/arbitrage) — Gained a deterrent to secure their rule.
 *   - Black South African Population: Primary victim (powerless/trapped) — Bore the opportunity cost and lived under a regime strengthened by nuclear arms.
 *   - Neighboring Frontline States: Secondary victim (organized/constrained) — Faced a direct nuclear threat from a hostile regional hegemon.
 *   - Western Cold War Powers: Ambivalent institutional actors (institutional/arbitrage) — Balanced anti-communist strategic goals with non-proliferation concerns.
 *   - Post-Apartheid State: Inheritor of the legacy (institutional/arbitrage) — Transformed the program's history into a tool for promoting disarmament.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(apartheid_nuclear_program, 0.75).
domain_priors:suppression_score(apartheid_nuclear_program, 0.85).
domain_priors:theater_ratio(apartheid_nuclear_program, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(apartheid_nuclear_program, extractiveness, 0.75).
narrative_ontology:constraint_metric(apartheid_nuclear_program, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(apartheid_nuclear_program, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(apartheid_nuclear_program, snare).
narrative_ontology:human_readable(apartheid_nuclear_program, "Apartheid South Africa's Clandestine Nuclear Program").
narrative_ontology:topic_domain(apartheid_nuclear_program, "political/military/technological").

domain_priors:requires_active_enforcement(apartheid_nuclear_program).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(apartheid_nuclear_program, apartheid_regime_elites).
narrative_ontology:constraint_victim(apartheid_nuclear_program, black_south_african_population).
narrative_ontology:constraint_victim(apartheid_nuclear_program, neighboring_frontline_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BLACK SOUTH AFRICAN POPULATION (SNARE) — Trapped within the apartheid system, this group bore the full cost of the program through resource diversion and the strengthening of their oppressor. The bomb was the ultimate tool to suppress their liberation. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈1.07.
constraint_indexing:constraint_classification(apartheid_nuclear_program, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: APARTHEID REGIME (ROPE) — The program's architects saw it as a pure coordination mechanism for national defense against a perceived 'total onslaught'. They controlled its existence and could dismantle it at will. d≈0.05, f(d)≈-0.12, σ=0.9 → χ≈-0.08. Negative extraction indicates a net subsidy.
constraint_indexing:constraint_classification(apartheid_nuclear_program, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: NEIGHBORING STATES (SNARE) — As organized states, they had more agency than the internal population but were constrained by South Africa's military and economic power. They faced a direct nuclear threat designed to enforce regional hegemony. d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.51. This χ is below the snare threshold, classifying as Tangled Rope, but the high suppression (0.85) makes Snare the more accurate label for their experience of coercion.
constraint_indexing:constraint_classification(apartheid_nuclear_program, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: WESTERN POWERS (TANGLED ROPE) — These actors saw both a coordination function (a stable anti-communist ally) and a severe extraction/risk (nuclear proliferation, moral hazard). Their relationship was a hybrid of benefit and cost, making it a Tangled Rope.
constraint_indexing:constraint_classification(apartheid_nuclear_program, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-APARTHEID STATE (PITON) — After 1994, the program's primary function (preserving apartheid) was inert. The new government inherited the legacy, infrastructure, and international posture. Its subsequent championing of non-proliferation is a form of theatrical maintenance of this legacy, turning a former Snare into a symbol of responsible statehood. The theater is in the performance of disarmament, not the original program's function.
constraint_indexing:constraint_classification(apartheid_nuclear_program, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — The program's structure was fundamentally extractive, diverting massive state resources to secure the power of a minority elite by threatening mass violence, while suppressing all alternatives. This is the definition of a Snare. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈1.04.
constraint_indexing:constraint_classification(apartheid_nuclear_program, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(apartheid_nuclear_program_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(apartheid_nuclear_program, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(apartheid_nuclear_program, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(apartheid_nuclear_program, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(apartheid_nuclear_program, TR),
    TR >= 0.70.

:- end_tests(apartheid_nuclear_program_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.75) is extremely high, representing the diversion of vast state resources towards the security of a small ruling minority at the direct expense of the welfare of the majority population. Suppression (0.85) is also extremely high, reflecting the top-secret nature of the program, the violent suppression of internal dissent that made such a program possible, and its ultimate goal of foreclosing any alternative to apartheid rule. Theater Ratio (0.20) is low because the program was highly functional; it successfully produced a small arsenal of working nuclear devices.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The Apartheid regime viewed the program as a Rope, a rational tool for coordinating national defense. For the oppressed majority and threatened neighbors, it was an unambiguous Snare, the ultimate instrument of coercion designed to perpetuate a violent, extractive political system. This difference is not a matter of opinion but of structural position: for the beneficiary, it coordinates survival; for the victim, it enforces subjugation.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the perspectival classifications. The `apartheid_regime_elites` are beneficiaries with arbitrage exit, yielding a low 'd' value and a Rope classification. The `black_south_african_population` are victims with trapped exit, yielding a high 'd' value and a Snare classification. Other actors like the `neighboring_frontline_states` (victims, constrained) and `western_powers` (mixed beneficiary/victim status) fall in between, producing their respective classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved because the program's 'coordination' function (national defense) was wholly instrumental to its primary extraction function (maintaining apartheid). The system was not coordinating for a common good; it was coordinating the security of a ruling class to enable the continued extraction of resources and rights from a subjugated majority. The high ε and suppression values are not artifacts of measurement but reflect the core structural reality of the apartheid state itself, making the Snare classification from the analytical perspective robust and correct.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_credibility,
    'Was the three-phase deterrence strategy credible, and would the regime have actually used the weapons?',
    'Cannot be resolved; it is a counterfactual. Analysis of declassified strategic documents can only reveal intent, not the outcome of a contingency that never occurred.',
    'If credible, the program was a functional (if monstrous) deterrent. If a bluff, its primary function was internal cohesion and resource capture, making it even more of a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_credibility, conceptual, 'Credibility of the nuclear deterrent strategy').

omega_variable(
    israeli_collaboration_depth,
    'What was the full extent of Israeli collaboration, particularly concerning the 1979 Vela incident?',
    'Declassification of intelligence archives in both South Africa and Israel.',
    'Deep collaboration would imply the program was less of an isolated national effort and more of a node in a network of pariah states, altering the cost and suppression calculations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(israeli_collaboration_depth, empirical, 'Extent of Israeli collaboration on the nuclear program').

omega_variable(
    dismantlement_motive,
    'Was the primary motive for dismantlement to prevent an ANC-led government from inheriting the arsenal, or was it a strategic pivot towards international reintegration?',
    'Analysis of de Klerk''s inner circle communications and post-facto testimony, weighed against the geopolitical timeline.',
    'If primarily to deny the ANC, it confirms the program''s function as a tool of racial power. If for reintegration, it suggests a partial shift towards a coordination logic (rejoining the international community).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dismantlement_motive, conceptual, 'Primary motivation for the program''s dismantlement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(apartheid_nuclear_program, 1971, 1989).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(apar_tr_t0, apartheid_nuclear_program, theater_ratio, 0, 0.1).
narrative_ontology:measurement(apar_tr_t9, apartheid_nuclear_program, theater_ratio, 9, 0.15).
narrative_ontology:measurement(apar_tr_t18, apartheid_nuclear_program, theater_ratio, 18, 0.2).

% Extraction over time
narrative_ontology:measurement(apar_be_t0, apartheid_nuclear_program, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(apar_be_t9, apartheid_nuclear_program, base_extractiveness, 9, 0.7).
narrative_ontology:measurement(apar_be_t18, apartheid_nuclear_program, base_extractiveness, 18, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(apartheid_nuclear_program, enforcement_mechanism).
narrative_ontology:affects_constraint(apartheid_nuclear_program, global_non_proliferation_regime).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
