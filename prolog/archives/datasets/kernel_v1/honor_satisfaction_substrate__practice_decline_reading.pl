% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__practice_decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__practice_decline_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_satisfaction_substrate__practice_decline_reading
 *   human_readable: Honor Code Persistence under Exogenous Enforcement Decline
 *   domain: legal_history/cultural_anthropology/social_structure
 *
 * SUMMARY:
 *   This constraint story instantiates the practice_decline_reading of the
 *   honor_satisfaction_substrate kernel. The core claim: honor codes persist
 *   as normative substrates in elite and institutional contexts even as
 *   dueling — their primary historical satisfaction mechanism — declined due
 *   to exogenous legal enforcement, institutional barriers, and opportunity
 *   costs. This reading asserts that the honor code itself remains largely
 *   intact; what changed is the mechanism through which honor can be
 *   satisfied. Dueling becomes unthinkable not because honor became
 *   unthinkable, but because the legal and institutional environment makes
 *   dueling impractical. The constraint operates as coordination rope: honor
 *   codes coordinate behavior through internalized obligation when external
 *   enforcement is weak or when the satisfaction mechanism becomes
 *   prohibited. Legal prohibition paradoxically reinforces honor codes in
 *   sectors (military, regional elite networks) where they become more
 *   valuable precisely because they operate without legal sanction. The
 *   measurement trajectory shows rising suppression (0.15 → 0.62) as legal
 *   prohibition strengthens, rising theater ratio (0.22 → 0.55) as honor
 *   satisfaction mechanisms become symbolic rather than violent, and rising
 *   extractiveness (0.18 → 0.38) as the constraint shifts from functional
 *   coordination to asymmetric obligation maintained through reputational
 *   mechanisms rather than satisfied through ritualized violence.
 *
 * KEY AGENTS:
 *   - Honor-bound individuals: Primary victims (powerless/identity_locked) — internalize honor codes as identity; face extraction even when dueling is prohibited because the code persists but its satisfaction mechanism is unavailable
 *   - Military institutions: Primary beneficiaries (organized/constrained) — retain honor codes as functional coordination mechanisms; exogenous suppression of dueling protects their internal honor systems
 *   - Regional elites (Southern culture of honor): Secondary beneficiaries (institutional/arbitrage) — maintain honor codes as status differentiation and coordination mechanism; legal prohibition increases value of honor as non-legal ordering principle
 *   - Modern legal system: Secondary beneficiary (institutional/arbitrage) — coordinates through explicit prohibition; benefits from internalized honor codes reducing enforcement burden
 *   - Honor code mythology: Institutional performance (institutional/constrained) — modern honor references increasingly theatrical, maintained through institutional inertia
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable social law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__practice_decline_reading, 0.38).
domain_priors:suppression_score(honor_satisfaction_substrate__practice_decline_reading, 0.62).
domain_priors:theater_ratio(honor_satisfaction_substrate__practice_decline_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__practice_decline_reading, rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__practice_decline_reading, "Honor Code Persistence under Exogenous Enforcement Decline").
narrative_ontology:topic_domain(honor_satisfaction_substrate__practice_decline_reading, "legal_history/cultural_anthropology/social_structure").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__practice_decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__practice_decline_reading, 'f41bf56f-fc57-4556-82d1-ca2b8e1a65d2').
narrative_ontology:cs_kernel_codification('f41bf56f-fc57-4556-82d1-ca2b8e1a65d2', distributed).
narrative_ontology:cs_authority_grounding('f41bf56f-fc57-4556-82d1-ca2b8e1a65d2', lineage).
narrative_ontology:cs_interpretation_layer_present('f41bf56f-fc57-4556-82d1-ca2b8e1a65d2').
narrative_ontology:cs_reading_relation('f41bf56f-fc57-4556-82d1-ca2b8e1a65d2', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('f41bf56f-fc57-4556-82d1-ca2b8e1a65d2', honor_satisfaction_substrate__composite_overdetermined_reading, influences).
narrative_ontology:cs_axiom('f41bf56f-fc57-4556-82d1-ca2b8e1a65d2', foundational, honor_code_substrate_immutable).
narrative_ontology:cs_axiom_status(honor_code_substrate_immutable, holdable).
narrative_ontology:cs_axiom_grounding('f41bf56f-fc57-4556-82d1-ca2b8e1a65d2', honor_code_substrate_immutable, deontological).
narrative_ontology:cs_axiom('f41bf56f-fc57-4556-82d1-ca2b8e1a65d2', foundational, exogenous_enforcement_primary_causal_driver).
narrative_ontology:cs_axiom_status(exogenous_enforcement_primary_causal_driver, holdable).
narrative_ontology:cs_axiom_grounding('f41bf56f-fc57-4556-82d1-ca2b8e1a65d2', exogenous_enforcement_primary_causal_driver, empirically_contingent).
narrative_ontology:cs_reference_frame('f41bf56f-fc57-4556-82d1-ca2b8e1a65d2', honor_code_legal_satisfaction_framework).
narrative_ontology:cs_drift_state('f41bf56f-fc57-4556-82d1-ca2b8e1a65d2', post_legal_prohibition_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f41bf56f-fc57-4556-82d1-ca2b8e1a65d2', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, honor_code_preserving_elites).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, institutional_structures_claiming_honor_basis).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, individuals_under_honor_obligation).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, social_coordination_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HONOR-BOUND INDIVIDUAL (SNARE) — A person socialized into honor codes faces severe extraction even as the exogenous enforcement (legal prohibition on dueling) makes the traditional satisfaction mechanism unavailable. Identity is fused with honor; exit would require abandoning core self-conception. The individual perceives the constraint as inescapable despite external suppression of dueling itself. The code persists internally even when its external expression is prohibited.
constraint_indexing:constraint_classification(honor_satisfaction_substrate__practice_decline_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: MILITARY INSTITUTION (ROPE) — Retains honor codes as operational coordination mechanism (officer conduct codes, unit loyalty, courage standards). Sees the constraint as functional — a way to coordinate behavior through internalized obligation rather than external enforcement alone. Exogenous suppression (civilian legal prohibition on dueling) actually protects the military's honor system by channeling honor satisfaction into institutional approved outlets (combat, hierarchy, duty). Constrained exit because military membership is now optional but carries path-dependent costs.
constraint_indexing:constraint_classification(honor_satisfaction_substrate__practice_decline_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: REGIONAL ELITE — SOUTHERN CULTURE OF HONOR (TANGLED ROPE) — Benefits from maintaining honor code as status marker and coordination mechanism (distinguish quality of character, enforce reputation stakes). Experiences modest extraction through continuing reputational obligation. Exogenous enforcement (legal prohibition) actually reinforces their control — honor becomes even more valuable as non-legal coordination mechanism when dueling is unavailable. Active enforcement of honor norms through social exclusion, marriage market gatekeeping, and reputation management replaces judicial recognition. Can arbitrage between regions (honor codes valued locally, not nationally enforced).
constraint_indexing:constraint_classification(honor_satisfaction_substrate__practice_decline_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: MODERN LEGAL SYSTEM (ROPE) — Coordinates behavior through explicit prohibition and enforcement of dueling laws. Sees exogenous enforcement (legal suppression) as solving a coordination problem: preventing wasteful violent escalation while redirecting honor satisfaction into legal channels (reputation, standing, property rights). Benefits from the constraint because it maintains order without requiring constant surveillance — internalized honor codes reduce policing costs. Can arbitrage between enforcement intensity (light enforcement in military/regional contexts, strict in civilian urban centers).
constraint_indexing:constraint_classification(honor_satisfaction_substrate__practice_decline_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: HONOR CODE MYTHOLOGY (PITON) — From a long-term global perspective, the normative ideal of honor has decayed into theatrical preservation. Modern invocations of 'honor codes' (military, academic, regional) are performative — they reference a prior system that no longer functions (dueling is gone; reputation stakes are largely symbolic). The constraint persists through institutional inertia and narrative performance, not through actual satisfaction mechanism. Theater ratio is moderate (0.55) because some real coordination still happens through honor norms, but much of the activity is maintenance of the appearance that honor codes matter.
constraint_indexing:constraint_classification(honor_satisfaction_substrate__practice_decline_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — IMMUTABLE STRUCTURE VIEW (MOUNTAIN) — From the view of deep civilizational anthropology, honor satisfaction is an immutable feature of status-hierarchical societies: humans require reputation stakes and status differentiation, and honor codes are a universal mechanism for managing these. From this view, the decline of dueling is merely a change in satisfaction mechanism, not a decay of the underlying code. The constraint appears unchangeable because status hierarchies are structural inevitabilities. However, this naturalizes what may be a contingent institutional arrangement — a false summit risk.
constraint_indexing:constraint_classification(honor_satisfaction_substrate__practice_decline_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__practice_decline_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(honor_satisfaction_substrate__practice_decline_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(honor_satisfaction_substrate__practice_decline_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(honor_satisfaction_substrate__practice_decline_reading, TR),
    TR >= 0.70.

:- end_tests(honor_satisfaction_substrate__practice_decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from honor-bound individuals through obligatory reputation stakes and status competition, even as the traditional satisfaction mechanism (dueling) is prohibited. The extraction is not as severe as a pure snare (ε ≥ 0.46) because alternative satisfaction mechanisms exist (military service, regional reputation management, professional standing) and some exit is possible through geographic or institutional mobility. The rise from 0.18 to 0.38 reflects increasing extraction as honor codes become purely symbolic — when dueling was legal, honor could be satisfied directly; when prohibited, honor-bound individuals face indefinite obligation without socially legitimate satisfaction. Suppression (0.62): Moderate-high. Legal prohibition of dueling creates structural suppression (individuals cannot satisfy honor codes through their traditional mechanism). But suppression is not total because alternative satisfaction mechanisms exist and because some regions/institutions maintain honor codes outside legal frameworks. The trajectory (0.15 → 0.62) reflects increasing legal enforcement intensity and institutional barrier-building against dueling. Theater ratio (0.55): Moderate. Modern honor codes are partly functional (military coordination, regional status differentiation) and partly performative (symbolic invocations of 'honor codes' without real satisfaction mechanisms). The rise from 0.22 to 0.55 reflects the gradual shift from functional dueling-satisfaction to theater as dueling becomes unavailable and honor codes persist primarily through narrative and institutional maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The practice_decline_reading produces a rope classification from most institutional perspectives (military, elite, legal system) because they experience honor codes as functional coordination mechanisms that solve real ordering problems. But the powerless/identity_locked perspective (the actual honor-bound individual) experiences snare classification — extraction without available satisfaction. The piton perspective (long-term institutional view) observes that honor codes have decayed into theater, maintained through inertia rather than function. The analytical/universal perspective risks seeing honor itself as immutable (mountain), which would foreclose this reading by naturalizing what is actually a contingent institutional arrangement. The gap between rope and snare classifications reveals the core asymmetry: institutions benefit from the constraint as coordination mechanism; individuals bear the extraction cost of internalized obligation without satisfaction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across perspectives. Honor-bound individuals (powerless/identity_locked) derive high d because they bear the extraction cost of obligation without available satisfaction mechanism: d ≈ 0.88. Military institutions (organized/constrained) derive moderate d despite beneficiary status because they face coordination costs and constrained exit: d ≈ 0.45. Regional elites (institutional/arbitrage) derive low d because they benefit from honor codes as status differentiation and have arbitrage options: d ≈ 0.22. The legal system (institutional/arbitrage) also derives low d through arbitrage (can enforce differently in different contexts): d ≈ 0.18. The honor-bound individual's high d reflects that the satisfaction mechanism is blocked: they cannot use the exit option (arbitrage/satisfaction) that would normally reduce extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy because it produces a single coherent classification (rope) across institutional perspectives, with the snare (powerless individual) and piton (theatrical maintenance) classifications understood as perspectival readings rather than inconsistent metrics. The mandatrophy potential lies in the alternative reading (cultural_contraction): if honor codes underwent foundational delegitimation, the constraint would classify as mountain-erosion or piton, not rope. The practice_decline_reading avoids mandatrophy by asserting that honor persistence is the key structural fact; cultural contraction would be mandatrophy-triggering because it reverses the natural law claim (honor is immutable) into decay narrative (honor eroded). This reading keeps the claim stable: rope coordination that persists under altered enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    honor_code_vs_dignity_transition,
    'Is the decline of dueling evidence that honor codes underwent foundational delegitimation, or merely that the satisfaction mechanism became exogenously impractical?',
    'Narrative and textual analysis of elite discourse from 1800–1900: Do surviving sources show philosophical rejection of honor premises themselves, or pragmatic adaptation to legal enforcement? Comparison of axioms asserted in period sources vs. modern reconstruction.',
    'If delegitimation: the cultural_contraction_reading is correct — the code itself transformed. If pragmatic adaptation: the practice_decline_reading (this one) is correct — the code persists as normative substrate under altered enforcement conditions. Classification outcome: rope (this reading) vs. mountain-decay (cultural contraction reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_code_vs_dignity_transition, empirical, 'Whether honor decline reflects code delegitimation or enforcement adaptation').

omega_variable(
    regional_honor_code_persistence,
    'Do modern Southern, military, and elite honor codes represent genuine continuations of pre-legal-prohibition codes, or neo-invented traditions that borrow honor language to organize different social structures?',
    'Comparative genealogy: trace specific honor norms (e.g., personal courage requirement, reputation stakes, response-to-insult protocols) from pre-1850 to post-1900 to present. Document transformations in how honor is satisfied and who can claim it. Distinguish authentic transmission from rebranding.',
    'If genuine continuations: honor code substrate persists (practice_decline_reading confirmed). If neo-invented: the code underwent foundational transformation (cultural_contraction_reading; composite_overdetermined_reading if both mechanisms operated). Classification outcome affected: rope vs. piton classification for modern honor codes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regional_honor_code_persistence, empirical, 'Whether modern honor codes represent genuine transmission or neo-invented traditions').

omega_variable(
    exogenous_endogenous_interaction,
    'Did legal prohibition on dueling cause honor code decline, or did honor code delegitimation enable legal prohibition to take hold?',
    'Temporal precedence analysis: In specific regions, did legal prohibition precede or follow elite rhetorical rejection of dueling? Causal pathway analysis: Does high legal enforcement correlate with honor code persistence (practice_decline_reading prediction) or with honor code erosion (composite_overdetermined_reading prediction)?',
    'If exogenous enforcement caused decline: rope classification, practice_decline_reading supported. If endogenous delegitimation enabled enforcement: mountain-decay or piton, cultural_contraction_reading supported. If both with interaction: composite_overdetermined_reading supported.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exogenous_endogenous_interaction, empirical, 'Temporal and causal precedence of legal prohibition vs. honor code delegitimation').

omega_variable(
    reading_kernel_contest_structure,
    'This constraint is one reading of the honor_satisfaction_substrate kernel. Does the practice_decline_reading''s core premise (honor persists under exogenous enforcement) logically foreclose the cultural_contraction_reading''s core premise (honor code itself transformed)?',
    'Logical analysis: Can both axioms (honor_code_substrate_immutable from this reading, honor_code_constitutive_transformation from cultural contraction) be held within any single coherent framework? Or must an observer choose one?',
    'If logically foreclosed: relation is ''forecloses''. If both can be held by different parties: relation is ''coexists_with''. If practice_decline creates structural pressure on cultural_contraction but doesn''t eliminate it: relation is ''influences''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest_structure, conceptual, 'Logical relationship between practice_decline and cultural_contraction readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__practice_decline_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_decline_theater_t0, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(honor_decline_theater_t50, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement(honor_decline_theater_t100, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(honor_decline_extractiveness_t0, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(honor_decline_extractiveness_t50, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(honor_decline_extractiveness_t100, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(honor_decline_suppression_t0, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(honor_decline_suppression_t50, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement(honor_decline_suppression_t100, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__practice_decline_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__cultural_contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__composite_overdetermined_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, legal_prohibition_mechanism_internalization).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, military_honor_code_operational_function).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-reading kernel cluster (honor_satisfaction_substrate). All three readings share the same base historical phenomena (dueling decline, legal prohibition, honor code persistence in some contexts, erosion in others) but assign different causal mechanisms and classify different structural types. The practice_decline_reading (this file) asserts that exogenous enforcement (legal prohibition) is primary; the code persists. The cultural_contraction_reading asserts endogenous delegitimation is primary; the code transformed. The composite_overdetermined_reading asserts both with causal interaction. Each reading has its own ε value and perspectival structure. They are linked through network.affects_constraints to enable comparative analysis of which reading best explains specific regional/institutional data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_substrate__practice_decline_reading, powerless, 0.88).
constraint_indexing:directionality_override(honor_satisfaction_substrate__practice_decline_reading, organized, 0.45).
constraint_indexing:directionality_override(honor_satisfaction_substrate__practice_decline_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
