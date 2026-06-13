% ============================================================================
% CONSTRAINT STORY: reformation_composite__political_realignment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__political_realignment_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: reformation_composite__political_realignment_reading
 *   human_readable: Reformation as Political Realignment: State Sovereignty via Religious Differentiation
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   This constraint models the Reformation as a political event, where
 *   emerging nation-states leveraged religious differentiation (e.g.,
 *   Protestantism vs. Catholicism) to assert sovereignty against the
 *   universalizing claims of the Holy Roman Empire and the Papacy. The
 *   principle of 'Cuius regio, eius religio' (whose realm, his religion) is
 *   the primary observable, demonstrating how territorial rulers gained
 *   control over religious affairs within their domains, thereby
 *   consolidating political power. This reading emphasizes the instrumental
 *   use of religious identity for state-building and the resulting extraction
 *   from previously universal authorities and local religious minorities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, 0.85).
domain_priors:suppression_score(reformation_composite__political_realignment_reading, 0.7).
domain_priors:theater_ratio(reformation_composite__political_realignment_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__political_realignment_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__political_realignment_reading, "Reformation as Political Realignment: State Sovereignty via Religious Differentiation").
narrative_ontology:topic_domain(reformation_composite__political_realignment_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__political_realignment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__political_realignment_reading, '0ce317e2-c35c-4ef8-acef-b490aa908c39').
narrative_ontology:cs_kernel_codification('0ce317e2-c35c-4ef8-acef-b490aa908c39', formalized).
narrative_ontology:cs_authority_grounding('0ce317e2-c35c-4ef8-acef-b490aa908c39', extraction).
narrative_ontology:cs_interpretation_layer_present('0ce317e2-c35c-4ef8-acef-b490aa908c39').
narrative_ontology:cs_reading_relation('0ce317e2-c35c-4ef8-acef-b490aa908c39', reformation_composite__theological_fragmentation_reading, influences).
narrative_ontology:cs_reading_relation('0ce317e2-c35c-4ef8-acef-b490aa908c39', reformation_composite__technological_mediation_reading, influences).
narrative_ontology:cs_axiom('0ce317e2-c35c-4ef8-acef-b490aa908c39', foundational, state_sovereignty_over_religion).
narrative_ontology:cs_axiom_status(state_sovereignty_over_religion, holdable).
narrative_ontology:cs_axiom_grounding('0ce317e2-c35c-4ef8-acef-b490aa908c39', state_sovereignty_over_religion, conventional).
narrative_ontology:cs_axiom('0ce317e2-c35c-4ef8-acef-b490aa908c39', foundational, religious_differentiation_as_political_tool).
narrative_ontology:cs_axiom_status(religious_differentiation_as_political_tool, holdable).
narrative_ontology:cs_axiom_grounding('0ce317e2-c35c-4ef8-acef-b490aa908c39', religious_differentiation_as_political_tool, instrumental).
narrative_ontology:cs_reference_frame('0ce317e2-c35c-4ef8-acef-b490aa908c39', universal_imperial_papal_authority).
narrative_ontology:cs_drift_state('0ce317e2-c35c-4ef8-acef-b490aa908c39', post_westphalian_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('0ce317e2-c35c-4ef8-acef-b490aa908c39', '').
narrative_ontology:cs_kernel_id(reformation_composite__political_realignment_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, territorial_rulers).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, emerging_nation_states).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, holy_roman_empire).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, papal_authority).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, local_religious_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, theologians_and_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Princes and monarchs who adopted Protestantism or asserted control over religious institutions within their domains. They actively enforced 'Cuius regio, eius religio' to consolidate their power and reduce external interference from the Empire or Papacy. They directly benefited from the transfer of authority and resources.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, territorial_rulers, agenda_setter,
    institutional, generational, mobile, regional).

% The abstract entities that gained coherence and power through the political realignments of the Reformation. Their long-term stability and sovereignty were enhanced by the religious differentiation, providing a basis for modern statehood.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, emerging_nation_states, beneficiary,
    institutional, civilizational, constrained, national).

% The overarching imperial authority that sought to maintain religious and political unity across its diverse territories. It suffered a significant loss of power, legitimacy, and territorial control as princes asserted religious independence. Its options were to fight costly wars or concede sovereignty.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, holy_roman_empire, payer,
    institutional, generational, trapped, continental).

% The spiritual and temporal head of the Catholic Church, which lost significant religious jurisdiction, political influence, and revenue streams (e.g., tithes) in Protestant territories. Its options were counter-reformation efforts or diplomatic concessions.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, papal_authority, payer,
    institutional, civilizational, constrained, global).

% Individuals and communities whose religious beliefs differed from that of their territorial ruler. They faced persecution, forced conversion, or exile, bearing the direct costs of the 'Cuius regio, eius religio' principle. Their options were conformity, flight, or martyrdom.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, local_religious_minorities, payer,
    powerless, biographical, trapped, local).

% Intellectuals and religious leaders who provided the theological justifications for religious differentiation. While often driven by genuine conviction, their work also served to legitimize the political ambitions of rulers, gaining patronage and influence in the process.
narrative_ontology:constraint_stakeholder(reformation_composite__political_realignment_reading, theologians_and_reformers, beneficiary,
    moderate, biographical, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It coordinated the consolidation of political power within emerging states by providing a clear, enforceable mechanism ('Cuius regio, eius religio') for rulers to assert religious and thus political autonomy from universal imperial and papal claims.
% TRANSFER_FUNCTION: It transferred political sovereignty, control over religious institutions, and associated resources (e.g., church lands, tithes) from the Holy Roman Empire and Papal authority to territorial rulers and emerging nation-states. It also transferred the burden of religious conformity onto local populations.
% ABSENT_VOICES: Advocates for religious pluralism or universal Christian unity (e.g., Erasmus, some Anabaptist groups) were largely marginalized or suppressed. They would have argued against the instrumentalization of religion for state power and for individual conscience or broader ecumenical reconciliation.
% DISAPPEARANCE_RATIONALE: If this political realignment had not occurred, the trajectory of European state formation would have been fundamentally different. Universal imperial and papal authority would have retained more power, and the modern nation-state system, as we know it, would likely not have emerged in the same form or at the same time.
% FOUNDING_PROBLEM: The problem was the tension between universal imperial/papal authority and the desire of local rulers for greater autonomy and control over their territories, exacerbated by existing religious dissent.
% FOUNDING_PROBLEM_CORROBORATION: Historians widely corroborate that the problem of universal vs. territorial authority was a central conflict of the era, and that the Reformation provided a critical pathway for its resolution in favor of territorial rulers. The problem of universal imperial/papal authority in its pre-Reformation form is largely 'dead' as a live political issue, though its legacy persists. Corroboration comes from political historians and legal scholars outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(reformation_composite__political_realignment_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__political_realignment_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__political_realignment_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reformation_composite__political_realignment_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__political_realignment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__political_realignment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because territorial rulers extracted significant power and resources from imperial and papal authorities, as well as from their subjects who were forced to conform. Suppression (0.7) is substantial, as religious conformity was enforced, and dissenters faced persecution or exile. The theater ratio (0.2) is relatively low, as the political function of religious differentiation was quite direct and effective, though often cloaked in theological justifications. Accessibility collapse (0.6) reflects the narrowing of religious choice for subjects, while resistance (0.8) was high from both imperial/papal forces and persecuted religious minorities.
 *
 * PERSPECTIVAL GAP:
 *   Territorial rulers experienced this as a beneficial coordination mechanism for state consolidation, while imperial/papal authorities and religious minorities experienced it as a highly extractive and suppressive snare. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Territorial rulers and emerging nation-states are clear beneficiaries (d=0.0-0.2), gaining sovereignty and control. The Holy Roman Empire and Papal authority are primary targets (d=0.8-1.0), losing political and religious jurisdiction. Local religious minorities are also targets (d=0.7-0.9), as their religious freedom was curtailed. The constraint subsidizes state power by extracting from universal religious and political claims.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the political consolidation as purely theological or as a benign coordination. By identifying the beneficiaries (territorial rulers) and victims (imperial/papal authority, religious minorities), it highlights the extractive nature of the political realignment, even if it also 'coordinated' the formation of nation-states. The mandate was to assert sovereignty, which was achieved through this extractive mechanism, making it a Tangled Rope rather than a pure Rope or Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_vs_theological_primacy,
    'Is the political realignment the primary driver of the Reformation, or a consequence of deeper theological fragmentation?',
    'Comparative historical analysis of regions where political and theological shifts diverged, or where political actors adopted religious differentiation without genuine theological conviction.',
    'If political realignment is primary, the constraint is a Tangled Rope of state-building; if theological fragmentation is primary, the political dimension is a secondary effect, and the core constraint is a Mountain of irreconcilable doctrinal differences (theological_fragmentation_reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_vs_theological_primacy, conceptual, 'Ambiguity regarding the causal primacy of political versus theological factors in the Reformation.').

omega_variable(
    role_of_printing_press,
    'To what extent was the political realignment enabled or accelerated by the printing press, rather than being solely a political choice?',
    'Counterfactual historical analysis: how would the political landscape have evolved without the rapid dissemination of religious texts and polemics enabled by print technology?',
    'If the printing press was a necessary condition, the political realignment is influenced by a technological constraint (technological_mediation_reading) that lowered the cost of religious differentiation; if not, the political actors'' agency is more central.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(role_of_printing_press, empirical, 'The influence of technological mediation (printing press) on political realignment.').

omega_variable(
    naturalness_of_cuius_regio,
    'Was ''Cuius regio, eius religio'' a natural outcome of state formation, or an imposed solution to manage religious conflict that itself became extractive?',
    'Analysis of alternative models of religious coexistence or toleration that were suppressed or failed to emerge in the period.',
    'If natural, the constraint is closer to a Mountain of political necessity; if imposed and extractive, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_cuius_regio, conceptual, 'The ''naturalness'' vs. constructedness of the principle of state-controlled religion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__political_realignment_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t0, reformation_composite__political_realignment_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(refo_tr_t10, reformation_composite__political_realignment_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(refo_tr_t20, reformation_composite__political_realignment_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(refo_tr_t30, reformation_composite__political_realignment_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(refo_be_t0, reformation_composite__political_realignment_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(refo_be_t10, reformation_composite__political_realignment_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(refo_be_t20, reformation_composite__political_realignment_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(refo_be_t30, reformation_composite__political_realignment_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t0, reformation_composite__political_realignment_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(refo_su_t10, reformation_composite__political_realignment_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(refo_su_t20, reformation_composite__political_realignment_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(refo_su_t30, reformation_composite__political_realignment_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__political_realignment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__technological_mediation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, westphalian_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'reformation_composite' kernel, focusing on the political dimension. Its structural influence on state formation and the principle of 'Cuius regio, eius religio' affects both the theological fragmentation and technological mediation aspects by providing the political framework within which they operated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
