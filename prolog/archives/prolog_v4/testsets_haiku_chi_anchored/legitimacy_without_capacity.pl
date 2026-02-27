% ============================================================================
% CONSTRAINT STORY: legitimacy_without_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_without_capacity, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimacy_without_capacity
 *   human_readable: The Sovereign Ghost: Legitimacy Without Capacity
 *   domain: political/organizational
 *
 * SUMMARY:
 *   The Sovereign Ghost occurs when an institution retains the international
 *   and formal legal right to rule (sovereignty, UN seat, treaty standing,
 *   diplomatic recognition) but has lost the actual capacity to provide
 *   security, justice, taxation compliance, or basic services. This is not
 *   state collapse—the formal apparatus persists, rituals of legitimacy
 *   continue, and international law still recognizes the state as the sole
 *   legitimate authority. But that legitimacy is increasingly disconnected
 *   from functional governance. The constraint exhibits a hybrid character:
 *   the state extraction mechanism (taxation, conscription) is sustained by
 *   legitimacy alone, while alternative authorities (warlords, NGO networks,
 *   community councils) increasingly provide the services legitimacy once
 *   justified. The theater ratio (0.81) reflects how much of the state's
 *   activity is performative rather than functional: flag ceremonies,
 *   official pronouncements, negotiations with international bodies, and tax
 *   collection rituals persist even as schools, hospitals, and police
 *   stations remain closed or collapsed. The constraint is dynamic: if
 *   capacity can be restored, the Scaffold perspective becomes structural. If
 *   capacity collapse is permanent, the Snare dominates. If international
 *   recognition is withdrawn, legitimacy evaporates entirely and the
 *   constraint converts to raw conflict.
 *
 * KEY AGENTS:
 *   - Civilian Population: Primary victim (powerless/trapped) — bears extraction through taxation and conscription despite receiving no services; cannot exit without refugee status
 *   - State Bureaucracy: Institutional actor (institutional/constrained) — maintains performative legitimacy; knows it cannot deliver but circulates the claim that it can
 *   - Extractive Elite (Corrupt Officials, Warlord-Aligned Class): Dual role beneficiary (organized/constrained) — extract resources using state monopoly; also benefit from international recognition that validates extraction
 *   - Alternative Authority (Warlord, Rebel Coalition, NGO Network): Moderate power actor (moderate/constrained) — provides functional services but lacks legitimacy; suppressed by the state's monopoly on recognition
 *   - International State System: Institutional beneficiary (institutional/arbitrage) — gains from the principle that legitimacy supersedes capacity; maintains Sovereign Ghost to avoid precedent that recognition is conditional on performance
 *   - International Intervention Coalition (UN, NGO, Neighbouring State): Organized actor (organized/mobile) — can build capacity but sees exit if restoration fails; provides temporary coordination function
 *   - Analytical Observer: Perspectives on natural law (analytical/analytical) — risks naturalizing a contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_without_capacity, 0.58).
domain_priors:suppression_score(legitimacy_without_capacity, 0.68).
domain_priors:theater_ratio(legitimacy_without_capacity, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_without_capacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimacy_without_capacity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(legitimacy_without_capacity, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_without_capacity, tangled_rope).
narrative_ontology:human_readable(legitimacy_without_capacity, "The Sovereign Ghost: Legitimacy Without Capacity").
narrative_ontology:topic_domain(legitimacy_without_capacity, "political/organizational").

domain_priors:requires_active_enforcement(legitimacy_without_capacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_without_capacity, formal_state_apparatus).
narrative_ontology:constraint_beneficiary(legitimacy_without_capacity, international_recognition_holders).
narrative_ontology:constraint_victim(legitimacy_without_capacity, civilian_populations).
narrative_ontology:constraint_victim(legitimacy_without_capacity, functional_governance_capacity).
narrative_ontology:constraint_victim(legitimacy_without_capacity, alternative_authority_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATION (SNARE) — Cannot exit sovereign territory without refugee/asylum status. Faces extraction through taxation and conscription but receives no functional services. No legitimate alternative authority exists that would accept them. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.72.
constraint_indexing:constraint_classification(legitimacy_without_capacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ALTERNATIVE AUTHORITY (TANGLED ROPE) — Can provide local security, education, healthcare where the state cannot, creating a coordination function. But the legitimate state's monopoly on taxation and conscription constrains this alternative authority's growth and legitimizes suppression of its expansion. d≈0.68, f(d)≈1.08, σ=1.0 → χ≈0.63.
constraint_indexing:constraint_classification(legitimacy_without_capacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERNATIONAL STATE SYSTEM (ROPE) — Recognizes the legitimate state as sovereign seat and maintains its UN seat, diplomatic standing, and treaty status regardless of capacity. This recognition sustains the state's claim to extract, even when provision is absent. The coordinate function: all states benefit from the principle that legitimacy is not contingent on immediate capacity (sovereignty lock-in). d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(legitimacy_without_capacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STATE BUREAUCRACY (PITON) — Performs ritual administration: collects revenue, maintains official records, holds ceremonies of sovereignty. Knows it cannot deliver security or core services. Persists through institutional inertia — its legitimacy claim is its only asset, so it circulates and reinforces the performance of legitimacy. theater_ratio=0.81 (high performative content). d≈0.35, f(d)≈0.32, σ=1.0 → χ≈0.26.
constraint_indexing:constraint_classification(legitimacy_without_capacity, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL INTERVENTION COALITION (SCAFFOLD) — Can provide temporary security, capacity-building, and legitimacy transfer. The coalition's structural position has a built-in sunset: capacity-building either succeeds (constraint resolves) or fails (coalition exits). The constraint is tolerated only if the coalition can see a path to restoration of state capacity. d≈0.42, f(d)≈0.42, σ=1.1 → χ≈0.25.
constraint_indexing:constraint_classification(legitimacy_without_capacity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: EXTRACTIVE ELITE (TANGLED ROPE) — Benefits from the gap between legitimacy and capacity: they can extract resources using the state's legitimate monopoly on coercion while providing nothing. Also provides a coordination function for international actors: the legitimate state exists as a legal counterparty for arms sales, trade agreements, and resource extraction contracts, even if that state cannot govern. d≈0.15, f(d)≈0.01, σ=1.1 → χ≈0.01.
constraint_indexing:constraint_classification(legitimacy_without_capacity, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — Risk of misclassifying as immutable: 'State formation always precedes functional capacity—legitimacy must be established before the expensive work of providing services.' But the structural data (ε=0.58, suppression=0.68, theater=0.81) contradicts the mountain gate. This is a false summit: the gap between legitimacy and capacity is a contingent institutional arrangement (international recognition system + domestic extraction monopoly), not a law of nature.
constraint_indexing:constraint_classification(legitimacy_without_capacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_without_capacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legitimacy_without_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimacy_without_capacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_without_capacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legitimacy_without_capacity, TR),
    TR >= 0.70.

:- end_tests(legitimacy_without_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The state extracts taxation and military conscription based on legitimacy claim, but the extraction is constrained by competing authorities that provide services at lower cost. If the state attempted to extract at monopoly rates, mass defection would accelerate. The value reflects that extraction is substantial but capped by exit options to alternative authorities. Suppression (0.68): High. Significant barriers to exit include: international law (refugee status is difficult to obtain), geographic barriers (landlocked states, hostile neighbors), social penalties (loss of citizenship, property), and the state's monopoly on legal coercion. But suppression is not total—some populations do migrate, defect, or withdraw cooperation. Theater ratio (0.81): Very high. State activity is predominantly performative: official ceremonies, bureaucratic procedures, international negotiations, tax collection attempts. The connection between these activities and actual security provision, justice, or public goods is minimal. The theater has increased as capacity has declined—the state's remaining asset is the performance of legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The Sovereign Ghost exhibits a striking perspectival gap between beneficiaries and victims. The state bureaucracy sees the constraint as an administrative problem it can solve through better organization (Piton—degraded but recoverable). The international state system sees it as a fundamental principle that preserves the sovereignty system (Rope—pure coordination). The extractive elite sees opportunity (Tangled Rope—mixed benefit). But the civilian population sees pure extraction with no exit (Snare), and the alternative authorities see a blocking constraint that suppresses their legitimacy despite superior capacity (Tangled Rope—mixed victim experience). The analytical observer risks naturalizing this gap as inevitable ('legitimacy always precedes capacity') when it is actually a contingent feature of the international recognition system.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian population: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Cannot leave; bears full cost of illegitimate extraction. State bureaucracy: Neither full beneficiary nor victim, but constrained insider → d≈0.35, f(d)≈0.32. Piton classification from theater gate. Extractive elite: Weak beneficiary (benefits from extraction monopoly) but constrained (depends on state legitimacy) → d≈0.15, f(d)≈0.01. Low effective extraction because their benefit is contingent. Alternative authority: Victim of suppression + constrained → d≈0.68, f(d)≈1.08. Cannot expand despite functional legitimacy; extraction by state monopoly on coercion. International state system: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary from sovereignty principle; experiences constraint as coordination mechanism. International intervention coalition: Organized with mobile exit → d≈0.42, f(d)≈0.42. Low effective extraction; coalition can see exit path through capacity restoration.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint is: 'Is the legitimacy-without-capacity gap a pure extraction mechanism (Snare) or a hybrid coordination-extraction arrangement (Tangled Rope)?' The tension arises because the international state system's recognition of the Sovereign Ghost DOES provide a coordination benefit—all states gain from the principle that formal recognition is stable and not contingent on moment-to-moment capacity. This principle enables long-term treaty making, capital accumulation, and institutional planning. But this coordination benefit accrues exclusively to states, not to civilian populations or alternative authorities. The Tangled Rope classification reflects: (a) genuine coordination function (international recognition principle benefits the global state system), (b) asymmetric extraction (civilians and alternative authorities bear costs while the state and international system benefit), and (c) active enforcement (international law, diplomatic isolation of alternative authorities, military intervention against warlords). However, the civilian population perspective is unambiguously Snare—they experience pure extraction with no coordination benefit. The mandatrophy is NOT resolved by claiming all perspectives are equally valid; rather, it is partially resolved by showing that the constraint is a Tangled Rope at the inter-institutional level (state system vs international system), and a Snare at the individual level (civilian population). The perspectives reveal that legitimacy-without-capacity is not a single constraint but a family of constraints operating at different social scales.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_fragility,
    'Does the Sovereign Ghost''s legitimacy rest primarily on international recognition (treaty system, UN seat, diplomatic standing) or on residual domestic memory and institutional continuity, and how vulnerable is each source?',
    'Comparative analysis of state collapse scenarios: track which states lose international recognition first vs domestic legitimacy; analyze whether international isolation or domestic rejection comes first in documented cases of state capacity loss',
    'If international recognition is primary source: legitimacy can be withdrawn by external actors, converting Tangled Rope to Snare. If domestic memory is primary: legitimacy erodes slowly, extending the Piton phase.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_source_fragility, empirical, 'Primary source of legitimacy in capacity-deficit regime').

omega_variable(
    extraction_ceiling_threshold,
    'What extraction level (proportion of available GDP taxed, conscription rate, security fee demanded by warlords) causes mass exit via emigration or defection to alternative authority, and does the state capacity gap impose a hard ceiling?',
    'Historical data on emigration rates, defection rates to rebel movements, and taxation levels in failed-state scenarios; regression analysis of extraction intensity vs population flight',
    'If ceiling is low (< 15% effective taxation): Snare classification across more perspectives. If ceiling is high (> 40%): Tangled Rope or even Rope classifications more stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_ceiling_threshold, empirical, 'Maximum extraction rate before mass defection or exit').

omega_variable(
    capacity_restoration_feasibility,
    'Is the legitimacy-without-capacity gap structurally permanent (path-dependent on war/colonialism) or temporarily resolvable through capacity-building intervention?',
    'Longitudinal study of post-intervention states: measure whether international intervention durably restores state capacity to match legitimacy; identify variables that predict success vs relapse',
    'If restoration is feasible: Scaffold perspective is structural and real. If permanent: Scaffold is aspirational theater masking an insoluble Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_restoration_feasibility, empirical, 'Whether capacity can be restored through institutional intervention').

omega_variable(
    alternative_authority_legitimacy_accumulation,
    'Can de facto authorities (warlords, NGO networks, rebel movements, local tribal councils) accumulate sufficient functional legitimacy through reliable service provision to eventually convert their de facto control to de jure recognition?',
    'Historical case studies of authority transitions: track how many de facto authorities achieved international recognition; identify which achieved it through capacity first vs legitimacy first; model the legitimacy accumulation curve',
    'If functional legitimacy converts to formal recognition: the constraint eventually resolves into two competing legitimate authorities (international bifurcation). If formal legitimacy prevents conversion: alternative authorities are permanently suppressed despite capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_authority_legitimacy_accumulation, conceptual, 'Whether functional legitimacy can convert to formal recognition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_without_capacity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legit_tr_t0, legitimacy_without_capacity, theater_ratio, 0, 0.55).
narrative_ontology:measurement(legit_tr_t5, legitimacy_without_capacity, theater_ratio, 5, 0.68).
narrative_ontology:measurement(legit_tr_t10, legitimacy_without_capacity, theater_ratio, 10, 0.81).

% Extraction over time
narrative_ontology:measurement(legit_be_t0, legitimacy_without_capacity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legit_be_t5, legitimacy_without_capacity, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(legit_be_t10, legitimacy_without_capacity, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_without_capacity, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimacy_without_capacity, state_capacity_collapse).
narrative_ontology:affects_constraint(legitimacy_without_capacity, monopoly_on_legitimate_coercion).
narrative_ontology:affects_constraint(legitimacy_without_capacity, international_recognition_conditionality).

% DUAL FORMULATION NOTE:
% The Sovereign Ghost can be decomposed into two structurally distinct claims: (1) State legitimacy without capacity is a stable equilibrium (ε≈0.35, primarily Piton/Rope), and (2) State extraction of resources from populations without service provision is an extractive mechanism (ε≈0.68, primarily Snare/Tangled Rope). These represent different observables: institutional persistence vs individual welfare. The constraint story presented here treats them as a unified phenomenon, but domain-specific analysis may require separation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_without_capacity, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
