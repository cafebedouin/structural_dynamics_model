% ============================================================================
% CONSTRAINT STORY: competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: competence_reading
 *   human_readable: Drills and Inspections as Live Exercised Knowledge (Competence Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the COMPETENCE READING of the
 *   preparedness_persistence kernel. The kernel is a contested commitment:
 *   what does it mean for preparedness to persist across time and personnel
 *   turnover? The competence reading answers: drills and inspections are live
 *   exercised knowledge that maintains operational readiness through
 *   rehearsal. This reading treats the constraint as a pure coordination
 *   mechanism (Rope) with minimal extraction. Personnel maintain competence
 *   through practice; institutions maintain knowledge across generational
 *   turnover through structured rehearsal; affected populations benefit from
 *   responders who are genuinely ready. The constraint solves the irreducible
 *   problem of knowledge decay — competence that is not rehearsed atrophies.
 *   This reading is structurally incompatible with the husk reading (drills
 *   persist as theater after competence has degraded) and influences but does
 *   not foreclose the hybrid reading (mixed coordination and extraction). The
 *   competence reading's core axiom is that live exercise is the primary
 *   mechanism by which knowledge persists operationally.
 *
 * KEY AGENTS:
 *   - Operational Personnel: Moderate power, constrained exit — benefit from drills that maintain their own competence; face career and safety risk if they abandon rehearsal
 *   - Institutional Steward: Powerful, mobile exit — benefits from having a trained workforce across generational turnover; could theoretically abandon drills but would undermine core function
 *   - Affected Population: Powerless, trapped exit — depend on responders' competence; cannot exit the geography; benefit directly from drills without bearing extraction cost
 *   - Regulatory Authority: Institutional power, arbitrage exit — benefits from framework that translates rules into practiced competence; could switch to alternative mechanisms but live exercise is irreplaceable
 *   - Analytical Observer: Civilizational perspective — sees drills as fundamental coordination mechanism for knowledge persistence across time
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_reading, 0.08).
domain_priors:suppression_score(competence_reading, 0.05).
domain_priors:theater_ratio(competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(competence_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(competence_reading, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_reading, rope).
narrative_ontology:human_readable(competence_reading, "Drills and Inspections as Live Exercised Knowledge (Competence Reading)").
narrative_ontology:topic_domain(competence_reading, "disaster_preparedness/institutional_memory/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_reading, 'b9387001-68ca-46ad-a254-b77b4c98798d').
narrative_ontology:cs_kernel_codification('b9387001-68ca-46ad-a254-b77b4c98798d', distributed).
narrative_ontology:cs_authority_grounding('b9387001-68ca-46ad-a254-b77b4c98798d', practice).
narrative_ontology:cs_interpretation_layer_present('b9387001-68ca-46ad-a254-b77b4c98798d').
narrative_ontology:cs_reading_relation('b9387001-68ca-46ad-a254-b77b4c98798d', competence_reading__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('b9387001-68ca-46ad-a254-b77b4c98798d', competence_reading__hybrid_reading, influences).
narrative_ontology:cs_axiom('b9387001-68ca-46ad-a254-b77b4c98798d', foundational, live_exercise_maintains_competence).
narrative_ontology:cs_axiom_status(live_exercise_maintains_competence, holdable).
narrative_ontology:cs_axiom_grounding('b9387001-68ca-46ad-a254-b77b4c98798d', live_exercise_maintains_competence, empirically_contingent).
narrative_ontology:cs_axiom('b9387001-68ca-46ad-a254-b77b4c98798d', foundational, knowledge_decay_without_rehearsal).
narrative_ontology:cs_axiom_status(knowledge_decay_without_rehearsal, holdable).
narrative_ontology:cs_axiom_grounding('b9387001-68ca-46ad-a254-b77b4c98798d', knowledge_decay_without_rehearsal, empirically_contingent).
narrative_ontology:cs_reference_frame('b9387001-68ca-46ad-a254-b77b4c98798d', competence_through_live_exercise).
narrative_ontology:cs_drift_state('b9387001-68ca-46ad-a254-b77b4c98798d', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b9387001-68ca-46ad-a254-b77b4c98798d', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(competence_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_reading, operational_personnel).
narrative_ontology:constraint_beneficiary(competence_reading, affected_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_reading, institutional_steward).
narrative_ontology:constraint_beneficiary(competence_reading, regulatory_authority).
narrative_ontology:constraint_vindicates(competence_reading, competence_through_practice).
narrative_ontology:constraint_vindicates(competence_reading, knowledge_decay_without_rehearsal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Personnel maintain their own competence through drills and inspections. They understand that skill decay is real and that rehearsal is necessary. They benefit from the constraint because it keeps them ready. They face career and safety risk if they abandon drills, so exit is constrained but not impossible.
narrative_ontology:constraint_stakeholder(competence_reading, operational_personnel, beneficiary,
    moderate, biographical, constrained, local).

% The institution benefits from having a trained workforce across generational turnover. Drills and inspections are the mechanism by which knowledge persists when personnel leave and new personnel arrive. The institution could theoretically abandon drills, but doing so would undermine its core function of maintaining operational readiness.
narrative_ontology:constraint_stakeholder(competence_reading, institutional_steward, beneficiary,
    powerful, generational, mobile, regional).

% Populations depend on emergency responders being genuinely ready. Drills and inspections directly benefit them by ensuring that responders have maintained competence. They cannot exit the constraint because they are trapped in the geography, but the constraint is purely beneficial — there is no extraction.
narrative_ontology:constraint_stakeholder(competence_reading, affected_populations, beneficiary,
    powerless, immediate, trapped, local).

% The regulatory authority benefits from having a framework that translates compliance rules into practiced competence. Drills and inspections are the mechanism by which regulatory requirements become operational reality. The authority could theoretically switch to alternative compliance mechanisms (simulation, certification without rehearsal), but live exercise is irreplaceable for maintaining actual readiness.
narrative_ontology:constraint_stakeholder(competence_reading, regulatory_authority, beneficiary,
    institutional, generational, arbitrage, national).

% The analytical observer sees drills and inspections as a fundamental coordination mechanism for maintaining knowledge across time and personnel turnover. The constraint solves the irreducible problem of competence decay — knowledge that is not rehearsed atrophies. This is a pure coordination function with minimal extraction.
narrative_ontology:constraint_stakeholder(competence_reading, knowledge_persistence_across_time, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(competence_reading, knowledge_persistence_across_time).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintain operational competence across time and personnel turnover. Drills and inspections solve the genuine problem that knowledge decays without rehearsal and that new personnel must learn from experienced personnel through structured practice.
% TRANSFER_FUNCTION: The constraint transfers knowledge and skill from experienced personnel to new personnel, and from institutional memory to current operations. It also transfers the burden of rehearsal time and compliance documentation, but in the competence reading, this burden is symmetric and proportional to the benefit.
% ABSENT_VOICES: No significant absent voices in the competence reading. All parties (personnel, institutions, populations, authorities) are represented in the stakeholder set. The constraint is transparent to all parties — no group is excluded from the conversation about whether drills are necessary.
% DISAPPEARANCE_RATIONALE: If drills and inspections disappeared overnight, the world would rearrange significantly. Competence would decay rapidly; new personnel would lack structured learning pathways; institutions would lose the mechanism for knowledge persistence; populations would face responders with degraded readiness. The constraint is not a natural law — it is a contingent institutional arrangement — but it is a necessary one given the problem it solves.
% FOUNDING_PROBLEM: Knowledge decay without rehearsal. Operational competence atrophies if not practiced. New personnel must learn from experienced personnel through structured practice. Institutions must maintain knowledge across generational turnover. These are genuine problems that drills and inspections solve.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by: (1) cognitive science research on skill decay and muscle memory loss; (2) emergency response case studies showing correlation between drill frequency and actual response effectiveness; (3) institutional stewards' own testimony that knowledge is lost when drills are abandoned; (4) regulatory authorities' empirical findings that compliance without rehearsal produces incompetent responders. The founding problem is live and unsolved by alternative mechanisms.
narrative_ontology:disappearance_verdict(competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(competence_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPERATIONAL PERSONNEL (ROPE) — Drills and inspections are genuine coordination mechanisms that maintain their own competence. Personnel benefit from rehearsal; the constraint solves the real problem of skill decay and muscle memory loss. Constrained exit because abandoning drills carries career and safety risk, but the coordination function is authentic — they are net beneficiaries of the practice.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: INSTITUTIONAL STEWARD (ROPE) — From the perspective of an institution committed to maintaining operational readiness across generational timescales, drills and inspections are pure coordination: they solve the genuine problem of knowledge persistence when personnel turn over. The institution benefits from having a trained workforce; the constraint is the mechanism that makes this possible. Mobile exit because the institution could theoretically abandon drills, but doing so would undermine its core function.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: AFFECTED POPULATION (ROPE) — Drills and inspections directly benefit those who depend on emergency response. The population cannot exit the constraint (they are trapped in the geography), but the constraint is purely beneficial — it increases the competence of responders who protect them. Zero extraction, pure coordination. The population's trapped exit status does not produce extraction because there is no asymmetric cost-bearing.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (ROPE) — Drills and inspections are the mechanism by which regulatory compliance becomes operational reality. The authority benefits from having a framework that translates rules into practiced competence. Arbitrage exit because the authority could theoretically switch to alternative compliance mechanisms (simulation, certification without rehearsal), but the coordination function of live exercise is irreplaceable for maintaining actual readiness.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, drills and inspections are a fundamental coordination mechanism for maintaining knowledge across time and personnel turnover. The constraint solves the irreducible problem of competence decay — knowledge that is not rehearsed atrophies. This is a pure coordination function with minimal extraction. The analytical observer sees the constraint as a Rope because it genuinely solves a collective action problem with minimal coercive overhead.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_reading_tests).
:- end_tests(competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The competence reading treats drills and inspections as pure coordination with minimal extraction. Personnel benefit from rehearsal; institutions benefit from knowledge persistence; populations benefit from competent responders. The small non-zero value reflects minor asymmetries: institutional stewards may benefit slightly more from the coordination than individual personnel, and regulatory authorities may extract modest compliance value. But the primary function is coordination, not extraction. Suppression (0.05): Very low. Drills are not coercive in the competence reading — they are recognized as necessary by all parties. Personnel understand that skill decay is real; institutions understand that knowledge must be rehearsed; populations understand that responders must be ready. The small non-zero value reflects that some personnel may experience drills as burdensome, but suppression is not the binding mechanism. Theater ratio (0.15): Low. Drills in the competence reading are primarily functional — they test actual readiness, identify gaps, and maintain muscle memory. Some performative elements exist (documentation, formal reporting), but the core activity is genuine skill maintenance. The theater ratio increases slightly over the interval as compliance documentation becomes more elaborate, but remains low because the underlying function is authentic.
 *
 * PERSPECTIVAL GAP:
 *   All five perspectives classify the constraint as Rope, indicating strong consensus that the constraint is a pure coordination mechanism. The perspectival gap is minimal because the competence reading treats drills as genuinely beneficial to all parties. Operational personnel see coordination that maintains their competence. Institutional stewards see coordination that maintains knowledge across time. Affected populations see coordination that benefits them without extraction. Regulatory authorities see coordination that translates rules into practice. The analytical observer sees coordination that solves an irreducible problem of knowledge decay. The absence of perspectival gap is itself diagnostic: if drills were extractive, we would expect to see Snare or Tangled Rope classifications from the powerless or constrained perspectives. The uniform Rope classification suggests the competence reading is structurally sound.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim declarations and exit options. All declared beneficiaries (operational_personnel, affected_populations) have low d values because they genuinely benefit from the constraint. Operational personnel have constrained exit but are net beneficiaries, producing moderate d. Affected populations have trapped exit but are net beneficiaries with zero extraction, producing low d. Institutional stewards and regulatory authorities have arbitrage exit and are beneficiaries, producing very low d. The analytical observer has analytical exit and sees pure coordination, producing d near 0.0. The absence of declared victims is significant: the competence reading does not identify any group bearing asymmetric costs. This is the structural signature of a pure Rope — all parties benefit from the coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   The competence reading resolves mandatrophy by treating the constraint as a pure coordination mechanism with no mandate decay. The founding problem (knowledge decay without rehearsal) remains live and unsolved by any alternative mechanism. Drills and inspections are the primary institutional response to this problem. The constraint's mandate has not outlived its function because the function (maintaining competence across time) is still necessary. The competence reading does not claim that drills are the only possible solution, but it does claim that they are a genuine solution to a real problem. Mandatrophy would arise if the founding problem (knowledge decay) were solved by alternative mechanisms (simulation, documentation, certification) that made live drills unnecessary. The competence reading's empirical claim is that this has not happened — live exercise remains the most effective mechanism for maintaining operational competence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the preparedness_persistence kernel best read as a competence problem (live exercised knowledge maintains operational readiness) or as an institutional husk problem (drills persist as theater after competence has degraded)?',
    'Comparative analysis of drill effectiveness across institutions: measure correlation between drill frequency/quality and actual emergency response outcomes; identify institutions where drills are performative vs. genuinely skill-maintaining.',
    'Competence reading: Rope/Mountain (coordination + natural law). Husk reading: Piton (degraded function maintained theatrically). Hybrid reading: Tangled Rope (mixed coordination and extraction). The three readings are structurally incompatible within a single framework — they make different claims about what drills DO.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Whether preparedness drills maintain genuine competence or persist as institutional theater').

omega_variable(
    knowledge_decay_mechanism,
    'What is the actual decay rate of emergency response competence without rehearsal? Is it steep enough to justify mandatory drill frequency, or do alternative mechanisms (simulation, certification, documentation) preserve competence adequately?',
    'Longitudinal studies of personnel competence: measure skill retention across different rehearsal schedules; compare drill-based competence maintenance to simulation-based and documentation-based alternatives.',
    'If decay is steep and alternatives are insufficient: competence reading is correct (Rope). If decay is slow or alternatives are adequate: the competence reading overstates the necessity of live drills, and the husk reading becomes more plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_decay_mechanism, empirical, 'Rate of competence decay without rehearsal and adequacy of alternative maintenance mechanisms').

omega_variable(
    extraction_through_compliance_burden,
    'Do mandatory drills and inspections extract value from operational personnel through compliance burden, or is the burden a necessary cost of coordination?',
    'Cost-benefit analysis: measure time/resource cost of drills to personnel and institutions; compare to measured competence gains; identify whether burden falls asymmetrically on lower-power actors.',
    'If burden is symmetric and proportional to benefit: Rope classification holds. If burden falls asymmetrically on powerless actors while benefits accrue to institutional stewards: Tangled Rope or Snare classification becomes more plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_through_compliance_burden, empirical, 'Whether drill compliance burden extracts value or represents necessary coordination cost').

omega_variable(
    sibling_reading_foreclosure,
    'Does the competence reading logically foreclose the husk reading, or do both remain live positions within different institutional contexts?',
    'Institutional case studies: identify institutions where competence reading is empirically supported (drills maintain genuine readiness) and institutions where husk reading is empirically supported (drills are performative); determine whether the same institution can hold both readings simultaneously or whether they are mutually exclusive.',
    'If foreclosure: the readings are incompatible; only one can be true of a given institution. If coexistence: both readings remain live; the question becomes which reading applies to which institutional context.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationship between competence and husk readings of the preparedness kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(comp_tr_t5, competence_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(comp_tr_t10, competence_reading, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(comp_be_t5, competence_reading, base_extractiveness, 5, 0.07).
narrative_ontology:measurement(comp_be_t10, competence_reading, base_extractiveness, 10, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_reading, suppression_requirement, 0, 0.03).
narrative_ontology:measurement(comp_su_t5, competence_reading, suppression_requirement, 5, 0.04).
narrative_ontology:measurement(comp_su_t10, competence_reading, suppression_requirement, 10, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_reading, attachment_coordination).
narrative_ontology:affects_constraint(competence_reading, husk_reading).
narrative_ontology:affects_constraint(competence_reading, hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_persistence kernel decomposes into three structurally distinct constraint stories: competence_reading (Rope — drills maintain genuine readiness), husk_reading (Piton — drills persist as theater), and hybrid_reading (Tangled Rope — mixed coordination and extraction). Each reading has its own ε value, its own beneficiary/victim structure, and its own classification. The three readings are linked by network.affects_constraints because they are alternative interpretations of the same kernel. Empirical evidence about whether drills actually maintain competence would determine which reading applies to a given institutional context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
