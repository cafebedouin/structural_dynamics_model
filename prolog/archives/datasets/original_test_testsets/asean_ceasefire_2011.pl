% ============================================================================
% CONSTRAINT STORY: asean_ceasefire_2011
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_asean_ceasefire_2011, []).

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
 *   constraint_id: asean_ceasefire_2011
 *   human_readable: 2011 ASEAN-mediated Thai-Cambodian Ceasefire Agreement
 *   domain: geopolitical
 *
 * SUMMARY:
 *   Following deadly border clashes near the Preah Vihear temple in early
 *   2011, ASEAN, under Indonesia's chairmanship, mediated a ceasefire
 *   agreement between Thailand and Cambodia. The agreement called for a
 *   cessation of hostilities and the deployment of unarmed Indonesian
 *   observers. However, the ceasefire was fragile and ultimately failed to
 *   hold, as it did not address the root cause of the conflict: the
 *   undemarcated border. The agreement functioned primarily as a performative
 *   de-escalation to satisfy the international community, while the
 *   underlying structural conflict remained unresolved until a 2013 ICJ
 *   ruling.
 *
 * KEY AGENTS:
 *   - Displaced Border Civilians: Primary victims (powerless/trapped) — bear the direct costs of the failed ceasefire.
 *   - ASEAN Diplomatic Corps: Primary beneficiary (institutional/arbitrage) — gains prestige from its role as a regional mediator.
 *   - Thai & Cambodian Governments: State actors (powerful/constrained) — experience the agreement as a mixed tool for de-escalation and a source of political cost.
 *   - Frontline Soldiers: Secondary victims (powerless/trapped) — face death and injury when the ceasefire breaks down.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(asean_ceasefire_2011, 0.55).
domain_priors:suppression_score(asean_ceasefire_2011, 0.65).
domain_priors:theater_ratio(asean_ceasefire_2011, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asean_ceasefire_2011, extractiveness, 0.55).
narrative_ontology:constraint_metric(asean_ceasefire_2011, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(asean_ceasefire_2011, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(asean_ceasefire_2011, tangled_rope).
narrative_ontology:human_readable(asean_ceasefire_2011, "2011 ASEAN-mediated Thai-Cambodian Ceasefire Agreement").
narrative_ontology:topic_domain(asean_ceasefire_2011, "geopolitical").

domain_priors:requires_active_enforcement(asean_ceasefire_2011).
narrative_ontology:has_sunset_clause(asean_ceasefire_2011).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(asean_ceasefire_2011, asean_diplomatic_corps).
narrative_ontology:constraint_beneficiary(asean_ceasefire_2011, incumbent_political_elites).
narrative_ontology:constraint_victim(asean_ceasefire_2011, displaced_border_civilians).
narrative_ontology:constraint_victim(asean_ceasefire_2011, frontline_soldiers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED CIVILIAN (SNARE) — For those living in the conflict zone, the ceasefire is a failed promise that does not provide lasting security. They are trapped by geography and bear the full cost of the agreement's collapse (death, injury, displacement) with no recourse. The agreement functions as a pure extraction of their safety and stability. d≈0.95, f(d)≈1.42, σ=0.9 → χ≈0.70.
constraint_indexing:constraint_classification(asean_ceasefire_2011, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ASEAN MEDIATOR (ROPE) — From the perspective of the ASEAN diplomatic corps (led by Indonesia), the agreement is a pure coordination success. It halted a hot war between member states, affirmed ASEAN's role as a regional peacemaker, and enhanced its international prestige. The costs are externalized. d≈0.05, f(d)≈-0.12, σ=0.9 → χ≈-0.06.
constraint_indexing:constraint_classification(asean_ceasefire_2011, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE ACTOR (TANGLED ROPE) — For the governments of Thailand and Cambodia, the agreement is a hybrid. It provides a genuine coordination benefit (de-escalation, avoiding further military/political costs) but also imposes significant extraction (accepting foreign observers, ceding some sovereignty, managing domestic nationalist backlash). They are constrained by international pressure and the lack of better alternatives. d≈0.75, f(d)≈1.10, σ=1.0 → χ≈0.61.
constraint_indexing:constraint_classification(asean_ceasefire_2011, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: OPTIMISTIC DIPLOMAT (SCAFFOLD) — This perspective sees the ceasefire not as an end but as a temporary support structure. Its purpose is to create stability for a permanent resolution (e.g., via the ICJ). The implicit sunset clause is the final border demarcation, which would render the ceasefire obsolete. From this view, it's a necessary, temporary coordination tool. d≈0.15, f(d)≈-0.01, σ=0.9 → χ≈-0.005.
constraint_indexing:constraint_classification(asean_ceasefire_2011, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — The analytical view recognizes both the coordination function and the severe extractive costs. The high theater ratio (0.75) indicates its performative nature, while the high base extractiveness (0.55) and suppression (0.65) confirm its coercive and costly structure for those trapped within it. This matches the system's claimed_type. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(asean_ceasefire_2011, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asean_ceasefire_2011_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(asean_ceasefire_2011, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asean_ceasefire_2011, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(asean_ceasefire_2011, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(asean_ceasefire_2011_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): High. This reflects the severe cost of the agreement's failure, which is borne by civilians and soldiers in the form of death, injury, and displacement. The failure to secure peace is a form of extraction. Suppression (0.65): High. At the peak of the crisis, there were few viable alternatives to ASEAN mediation, forcing both parties into an agreement they were not fully committed to. Theater Ratio (0.75): Very High. The agreement was largely for an international audience, a way to demonstrate that action was being taken. Its rapid collapse reveals its low functional value compared to its high performative value. The `has_sunset_clause` is considered true because the ceasefire was implicitly temporary, designed to hold only until a permanent diplomatic or legal resolution was found.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For ASEAN diplomats, the agreement is a successful coordination mechanism (Rope) that burnishes their institutional credentials. For a civilian in the line of fire, it is a cruel trap (Snare) that offers the illusion of safety before collapsing. For the state actors themselves, it is a complex, costly, and necessary tool of statecraft (Tangled Rope). The classification depends entirely on whether the observer benefits from the performance, is constrained by the structure, or is a victim of its failure.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivations drive the perspectival classifications. The `displaced_border_civilians` are victims with trapped exit options, maximizing their directionality (d≈0.95) and thus the effective extraction (χ), leading to a Snare classification. The `asean_diplomatic_corps` are beneficiaries with arbitrage exit options, minimizing their directionality (d≈0.05) and making the effective extraction negative, hence a Rope. The state actors are in a mixed position, modeled as victims with constrained exit, resulting in a high-but-not-maximal directionality (d≈0.75) that classifies as a Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves the mandatrophy by demonstrating that a single diplomatic instrument is not monolithically a 'coordination' or 'extraction' mechanism. Its classification is indexical. Labeling the ceasefire as a pure Rope (the ASEAN view) would ignore the immense costs borne by local populations. Labeling it as a pure Snare (the civilian view) would ignore the genuine, if temporary, de-escalation it provided at the state level. The Deferential Realism framework correctly identifies it as a multi-faceted object whose character changes with the observer's structural relationship to it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mediator_intent_vs_outcome,
    'Was the agreement''s primary function genuine conflict resolution or a performative act to enhance ASEAN''s institutional prestige?',
    'Analysis of internal ASEAN communications and diplomatic memoirs from the period to distinguish stated goals from revealed preferences.',
    'If intent was prestige, it confirms the high theater and extractive nature (Tangled Rope/Snare). If intent was genuine resolution, it was a well-intentioned but failed Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mediator_intent_vs_outcome, empirical, 'Distinguishing between genuine resolution intent and institutional prestige-seeking in ASEAN''s mediation.').

omega_variable(
    domestic_nationalist_capture,
    'To what extent were the Thai and Cambodian governments constrained by domestic nationalist factions, making a permanent resolution politically impossible?',
    'Comparative political analysis of the influence of nationalist groups on government policy in both nations during 2011-2013.',
    'High nationalist capture implies the conflict was a Mountain of domestic politics, making the ceasefire merely theater. Low capture suggests the failure was in the diplomatic instrument itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domestic_nationalist_capture, empirical, 'Assessing the degree of state capture by domestic nationalist factions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asean_ceasefire_2011, 2011, 2013).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(asea_tr_t0, asean_ceasefire_2011, theater_ratio, 0, 0.7).
narrative_ontology:measurement(asea_tr_t1, asean_ceasefire_2011, theater_ratio, 1, 0.72).
narrative_ontology:measurement(asea_tr_t2, asean_ceasefire_2011, theater_ratio, 2, 0.75).

% Extraction over time
narrative_ontology:measurement(asea_be_t0, asean_ceasefire_2011, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(asea_be_t1, asean_ceasefire_2011, base_extractiveness, 1, 0.45).
narrative_ontology:measurement(asea_be_t2, asean_ceasefire_2011, base_extractiveness, 2, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(asean_ceasefire_2011, enforcement_mechanism).
narrative_ontology:affects_constraint(asean_ceasefire_2011, south_china_sea_code_of_conduct).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
