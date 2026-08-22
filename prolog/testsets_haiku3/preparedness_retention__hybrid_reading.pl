% ============================================================================
% CONSTRAINT STORY: preparedness_retention__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__hybrid_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_retention__hybrid_reading
 *   human_readable: Stratified Preparedness Retention (Hybrid Reading)
 *   domain: governance/disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   The Netherlands maintains a dual-track preparedness system: technical
 *   competence is concentrated and actively exercised within Rijkswaterstaat
 *   and water boards (live operational knowledge, continuous drills, system
 *   upgrades, expert judgment), while broader societal memory has become
 *   ceremonial (municipal drills conducted without understanding, public
 *   evacuation protocols treated as ritual performances, historical flood
 *   memory displaced by technocratic standards). This constraint is ONE
 *   READING of the contested kernel preparedness_retention. The kernel itself
 *   is what 'preparedness' is—a live practiced competence, a memorial
 *   performance, or a hybrid stratification. This constraint instantiates the
 *   hybrid reading: preparedness is BOTH real competence (at the top) AND
 *   ceremonial theater (at the bottom), and the stratification itself is the
 *   structure that extracts autonomy and distributed resilience from local
 *   actors while concentrating institutional power.
 *
 * KEY AGENTS:
 *   - Rijkswaterstaat: institutional agenda-setter, maintains live technical competence, controls definition of preparedness standards
 *   - Regional water boards: powerful beneficiaries and secondary payers, retain competence but constrained by central standards
 *   - Municipal governments: moderate-power payers, execute preparedness ceremonially without understanding
 *   - General public: powerless trapped payers, dependent on expert judgment, no voice in competence definition
 *   - Elected representatives: excluded from technical deliberation, rubber-stamp expert recommendations
 *   - Historical flood survivors: excluded, identity-locked knowledge-holders, experiential memory displaced by technical standards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, 0.68).
domain_priors:suppression_score(preparedness_retention__hybrid_reading, 0.72).
domain_priors:theater_ratio(preparedness_retention__hybrid_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_retention__hybrid_reading, "Stratified Preparedness Retention (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_retention__hybrid_reading, "governance/disaster_preparedness/institutional_memory").

domain_priors:requires_active_enforcement(preparedness_retention__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__hybrid_reading, '78c29cd6-63dc-403d-bc79-815fe7bb3147').
narrative_ontology:cs_kernel_codification('78c29cd6-63dc-403d-bc79-815fe7bb3147', formalized).
narrative_ontology:cs_authority_grounding('78c29cd6-63dc-403d-bc79-815fe7bb3147', expertise).
narrative_ontology:cs_interpretation_layer_present('78c29cd6-63dc-403d-bc79-815fe7bb3147').
narrative_ontology:cs_reading_relation('78c29cd6-63dc-403d-bc79-815fe7bb3147', preparedness_retention__competence_reading, influences).
narrative_ontology:cs_reading_relation('78c29cd6-63dc-403d-bc79-815fe7bb3147', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_axiom('78c29cd6-63dc-403d-bc79-815fe7bb3147', foundational, preparedness_is_stratified_competence).
narrative_ontology:cs_axiom_status(preparedness_is_stratified_competence, holdable).
narrative_ontology:cs_axiom_grounding('78c29cd6-63dc-403d-bc79-815fe7bb3147', preparedness_is_stratified_competence, empirically_contingent).
narrative_ontology:cs_axiom('78c29cd6-63dc-403d-bc79-815fe7bb3147', secondary, distributed_incompetence_is_necessary_cost).
narrative_ontology:cs_axiom_status(distributed_incompetence_is_necessary_cost, holdable).
narrative_ontology:cs_axiom_grounding('78c29cd6-63dc-403d-bc79-815fe7bb3147', distributed_incompetence_is_necessary_cost, instrumental).
narrative_ontology:cs_reference_frame('78c29cd6-63dc-403d-bc79-815fe7bb3147', unified_technical_standard_coordination).
narrative_ontology:cs_drift_state('78c29cd6-63dc-403d-bc79-815fe7bb3147', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('78c29cd6-63dc-403d-bc79-815fe7bb3147', '').
narrative_ontology:cs_kernel_id(preparedness_retention__hybrid_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, specialized_water_institutions).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, general_public).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, municipal_governance_layers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, regional_water_boards).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, emergency_responders).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, regional_water_boards).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, municipal_governments).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, emergency_responders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Dutch national water authority maintains live technical competence in flood defense through continuous training, system modernization, and operational readiness protocols. Sets the agenda for preparedness standards, controls the knowledge base, and retains authority to interpret what constitutes adequate preparation. Collects institutional prestige and budget justification from the demonstration of expertise.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, rijkswaterstaat, agenda_setter,
    institutional, generational, arbitrage, national).

% Autonomous water management bodies that maintain operational competence in dyke maintenance and local flood response. Benefit from centralized technical knowledge and liability protection through alignment with Rijkswaterstaat standards. Pay the cost of continuous training and equipment upgrades justified by technical standards only they understand deeply.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, regional_water_boards, beneficiary,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, regional_water_boards, payer).

% Required to coordinate preparedness at the city and neighborhood level—evacuation protocols, emergency shelters, warning systems. Obligated to conduct drills and maintain competence frameworks, but actual knowledge has atrophied to ceremonial execution of plans designed by specialists. Staff turnover and budget constraints mean the municipal layer performs preparedness rituals without comprehending the underlying technical reasoning.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, municipal_governments, payer,
    moderate, biographical, constrained, local).

% Subject to evacuation orders, participates in public drills, and relies on warnings issued by the institutional hierarchy. Has no meaningful understanding of flood risk or the reasoning behind preparedness decisions. Trapped in place (geographic location) and dependent on expert judgment transmitted downward through channels they did not help construct.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, general_public, payer,
    powerless, immediate, trapped, local).

% Fire departments and rescue services benefit from the structured knowledge base and clear authority hierarchy in flood response. Pay through continuous retraining and accountability for executing plans they did not author. Their local operational knowledge is subordinated to centralized technical standards.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, emergency_responders, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, emergency_responders, payer).

% Nominally responsible for approving preparedness budgets and policies, but excluded from the technical deliberation that determines actual standards. Decision-making authority is concentrated in the specialist institutions; elected bodies rubber-stamp technical recommendations without capacity to assess them independently.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, elected_representatives, excluded,
    moderate, biographical, constrained, national).

% Carry lived memory of preparedness failures and flood impact but are not consulted in competence standards or strategy. Their experiential knowledge is treated as anecdotal noise, subordinated to technical expert judgment. Some carry identity-locked attachment to the communities at risk, which prevents them from exiting the concern even as they are structurally excluded from voice.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, historical_flood_survivors, excluded,
    powerless, biographical, identity_locked, local).

% The abstract body of maintained expertise in hydrology, dyke mechanics, systems engineering, and historical flood dynamics. Exists as a concentration within the specialist institutions and is the referent against which all other actors' competence is measured (and found wanting).
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, technical_knowledge_core, observer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(preparedness_retention__hybrid_reading, technical_knowledge_core).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__hybrid_reading, rijkswaterstaat).
narrative_ontology:fixing_cost_class(preparedness_retention__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes technical flood defense knowledge and operational authority, ensuring that complex hydraulic and structural judgments are made by trained specialists rather than dispersed across incompetent local actors. Solves the coordination problem of connecting local evacuation, regional dyke management, and national water policy into a coherent response system.
% TRANSFER_FUNCTION: Moves authority over preparedness decisions from democratic bodies and local communities to specialist institutions; moves budget and administrative effort from local to central control; moves epistemic authority—who gets to define what competence means—from distributed knowledge to concentrated expertise.
% ABSENT_VOICES: Historical flood survivors, local communities that bear concentrated flood risk, and elected representatives who nominally hold responsibility are all excluded from defining what preparedness means. They would argue for preparedness grounded in lived local knowledge and democratic deliberation, not top-down expert imposition. They are kept out by the assertion that technical expertise is non-negotiable.
% DISAPPEARANCE_RATIONALE: If the stratified preparedness system vanished, the central specialist institutions would lose budget, authority, and prestige. Regional water boards would fragment, municipal coordination would collapse into local improvisation, and the public would lose even the ceremonial structure that currently creates a sense of preparation. The arrangement persists because it concentrates responsibility (and thus shields specialists from accountability for failures) while distributing the actual work of preparation downward.
% FOUNDING_PROBLEM: After the 1995 near-floods, the Netherlands realized that preparedness had become fragmented across municipalities and water boards with no unified standard. Local knowledge, while grounded in experience, was inconsistent and sometimes contradicted modern hydraulic science. A unified technical standard was needed to ensure that all parts of the nation could respond coherently to catastrophic flooding.
% FOUNDING_PROBLEM_CORROBORATION: Rijkswaterstaat and water board leadership attest the founding problem remains live: climate change, aging infrastructure, and population density all increase the need for expert-led preparation. Municipal officials and some flood researchers attest that the founding problem (fragmented, incompetent local response) has been solved by the centralized system, and the arrangement now persists as an extraction of autonomy from local actors. Historians of the 1995 near-floods confirm the fragmentation problem was real; contemporary analysts dispute whether the centralized solution is proportionate or has created new vulnerabilities.
narrative_ontology:disappearance_verdict(preparedness_retention__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_retention__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__hybrid_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 (1995, immediate post-near-flood coordination problem) to 0.68 (2026, stabilized institutional hierarchy). The rise tracks the consolidation of expert authority and the attenuation of alternative knowledge sources. Theater ratio rises from 0.28 to 0.58, indicating that an increasing share of preparedness activity at municipal and public levels is ritual enactment divorced from genuine competence. Suppression requirement rises from 0.42 to 0.72, tracking the institutional effort required to prevent municipal or public questioning of expert standards. The measurements share a single time grid: every metric is authored at every time point (1995, 2002, 2009, 2016, 2021, 2026). The plateau from 2021 to 2026 (extractiveness and theater flat, suppression stable) suggests the system has reached a stable configuration: the specialist institutions are no longer consolidating power, but the stratified structure is fully locked in. Suppression is not rising because the system is now normalized; municipal actors and the public have internalized the expectation that they cannot understand preparedness at the technical level.
 *
 * PERSPECTIVAL GAP:
 *   Rijkswaterstaat and senior water board leadership perceive the system as a coordination success: unified standards, live competence, professional responsibility for outcomes. They experience low extractiveness (they are providing genuine value through expertise) and low suppression (resistance is minimal because the system is accepted as legitimate). Municipal governments and the public perceive the system as learned helplessness: they are required to perform preparedness without understanding, stripped of autonomy, and dependent on remote expert judgment. They experience high extractiveness (their effort and attention are mobilized without return) and high suppression (they cannot articulate alternatives because the technical expertise is treated as non-negotiable). The engine should compute this divergence from the power/exit/beneficiary data: Rijkswaterstaat is an institutional beneficiary with arbitrage-grade exit (it could reorient its expertise to other domains), while municipal actors are moderate-power constrained payers, and the public is powerless and trapped.
 *
 * DIRECTIONALITY LOGIC:
 *   Rijkswaterstaat derives directionality near the beneficiary end (d ≈ 0.10–0.25): it collects institutional prestige and budget justification from the maintenance of expertise, has numerous exit options (could shift to climate adaptation, water supply management, or international consulting), and is the designated beneficiary of the arrangement. Regional water boards sit near symmetric (d ≈ 0.45–0.55): they benefit from centralized technical standards and liability protection, but pay the cost of continuous retraining and subordination of local knowledge. Municipal governments and the public derive directionality near the target end (d ≈ 0.75–0.90): they are required to participate without comprehension, their autonomy is extracted, their exit options are severely constrained (they cannot opt out of living in flood-prone areas), and they are not beneficiaries of the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled_rope because it genuinely solves a coordination problem (unified preparedness standards, professional flood response) while simultaneously extracting from local actors (stripping them of decision-making authority, displacing local knowledge). Mandatrophy is NOT present: the founding problem (fragmented, incompetent local preparedness) is contested, not dead. Rijkswaterstaat and water boards argue it remains live; municipalities and researchers argue it is solved but the system persists as extraction. The classification prevents mislabeling this as pure rope (which would require no extraction or suppression) or as pure snare (which would require no genuine coordination function). The tangled_rope classification captures the structural truth: real coordination coexists with real asymmetric extraction, and the coordination function provides the legitimacy that permits the extraction to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theater_vs_competence_boundary,
    'At what threshold does the theater_ratio measuring municipal preparedness cross from ''necessary pedagogical staging'' (drill-as-teaching) to ''hollow ritual'' (drill-as-performance)? Is there a point where municipal actors are simply pretending competence they know they lack?',
    'Post-disaster analysis: in actual flooding events, do municipal-layer preparations (evacuation, shelter, warning) execute as designed, or do they collapse into improvisation? If real floods reveal municipal incompetence, the theater ratio is measuring displacement of genuine competence by ritual. If municipal execution is effective despite low understanding, the boundary is not at ritual but at some hybrid of automaticity and expert backup.',
    'If theater crosses into hollowness, the constraint shifts from tangled_rope toward snare: extraction without coordination value. If theater remains pedagogical (drill as competence-building despite low conscious understanding), the constraint remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_vs_competence_boundary, empirical, 'Whether municipal preparedness theater is pedagogical staging or competence displacement').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) structural (municipal actors are prevented by external barriers from questioning expert authority) or internalized (they have internalized the belief that they cannot understand flood defense, and would not challenge authority even if barriers were removed)?',
    'Policy experiments: if a jurisdiction decentralizes preparedness authority to municipal level and provides technical training, do municipalities begin to exercise independent judgment, or do they continue to defer to central expertise? If they exercise judgment, suppression is structural; if they defer despite capability, suppression is internalized.',
    'If suppression is structural, removing barriers and devolving authority would restore agency and distributed resilience. If suppression is internalized, decentralization alone would fail—identity retraining and epistemic reconstruction would be necessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Mechanism of suppression in the municipal governance layer: external barrier vs. internalized deference').

omega_variable(
    single_point_of_failure_risk,
    'Does the centralization of preparedness competence in Rijkswaterstaat create a single point of failure? If the institution loses institutional memory (leadership turnover, budget cuts, institutional collapse), does preparedness competence vanish more completely than it would if knowledge were distributed?',
    'Scenario analysis and organizational resilience studies: model the decay of Rijkswaterstaat''s institutional knowledge under stress; compare to the resilience of distributed, even incompletely-understood, community-embedded preparedness knowledge.',
    'If centralization creates hidden vulnerability, the arrangement trades visible distributed incompetence (theater_ratio rises, municipalities perform without understanding) for hidden systemic fragility (institutional expertise is brittle). This would reframe the extraction: the public loses autonomy to gain false security, and gains neither real competence nor real safety.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(single_point_of_failure_risk, empirical, 'Whether centralized expertise creates hidden systemic vulnerability to institutional collapse').

omega_variable(
    kernel_reading_contestation,
    'Is preparedness fundamentally a live practiced competence (competence_reading), a memorial ritual (husk_reading), or a dual-track stratification where competence and ceremony are intentionally separated (this hybrid_reading)? The three readings assign different ε values to the same kernel and make different predictions about system resilience.',
    'Observe actual floods and crisis response. Competence_reading predicts adaptive, knowledge-driven response at all levels. Husk_reading predicts collapse because even expert performance is ritual without adaptive capacity. Hybrid_reading predicts mixed outcomes: specialists respond adaptively while municipal layers fail in predictable ways. The empirical pattern of success and failure will favor one reading over the others.',
    'If the kernel is live competence everywhere, the current system is a false summit (real coordination, claimed authority justified by real expertise). If the kernel is hollow everywhere, the system is a snare (extraction dressed in technical legitimacy). If the kernel is structurally split, the system is tangled_rope (real coordination at the top, real extraction at the bottom). The classification choice determines whether reform should centralize further, decentralize authority, or redistribute knowledge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the preparedness_retention kernel is empirically true: competence, husk, or hybrid stratification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__hybrid_reading, 1995, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1995, preparedness_retention__hybrid_reading, theater_ratio, 1995, 0.28).
narrative_ontology:measurement(prep_tr_t2002, preparedness_retention__hybrid_reading, theater_ratio, 2002, 0.38).
narrative_ontology:measurement(prep_tr_t2009, preparedness_retention__hybrid_reading, theater_ratio, 2009, 0.45).
narrative_ontology:measurement(prep_tr_t2016, preparedness_retention__hybrid_reading, theater_ratio, 2016, 0.54).
narrative_ontology:measurement(prep_tr_t2021, preparedness_retention__hybrid_reading, theater_ratio, 2021, 0.58).
narrative_ontology:measurement(prep_tr_t2026, preparedness_retention__hybrid_reading, theater_ratio, 2026, 0.58).

% Extraction over time
narrative_ontology:measurement(prep_be_t1995, preparedness_retention__hybrid_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(prep_be_t2002, preparedness_retention__hybrid_reading, base_extractiveness, 2002, 0.48).
narrative_ontology:measurement(prep_be_t2009, preparedness_retention__hybrid_reading, base_extractiveness, 2009, 0.58).
narrative_ontology:measurement(prep_be_t2016, preparedness_retention__hybrid_reading, base_extractiveness, 2016, 0.65).
narrative_ontology:measurement(prep_be_t2021, preparedness_retention__hybrid_reading, base_extractiveness, 2021, 0.68).
narrative_ontology:measurement(prep_be_t2026, preparedness_retention__hybrid_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1995, preparedness_retention__hybrid_reading, suppression_requirement, 1995, 0.42).
narrative_ontology:measurement(prep_su_t2002, preparedness_retention__hybrid_reading, suppression_requirement, 2002, 0.55).
narrative_ontology:measurement(prep_su_t2009, preparedness_retention__hybrid_reading, suppression_requirement, 2009, 0.65).
narrative_ontology:measurement(prep_su_t2016, preparedness_retention__hybrid_reading, suppression_requirement, 2016, 0.7).
narrative_ontology:measurement(prep_su_t2021, preparedness_retention__hybrid_reading, suppression_requirement, 2021, 0.72).
narrative_ontology:measurement(prep_su_t2026, preparedness_retention__hybrid_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__hybrid_reading, 0.15).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__competence_reading).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__husk_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the preparedness_retention kernel. The sibling readings are competence_reading (preparedness is live competence everywhere the system reaches) and husk_reading (preparedness is memorial ritual everywhere). The three readings diverge on whether the specialist institutions maintain genuine adaptive knowledge and whether local actors' lack of understanding is a feature (efficient specialization) or a bug (dangerous hollowness). All three readings share the same referent—the Dutch flood preparedness system post-1995—but assign structurally different roles to technical competence and institutional authority. The hybrid reading asserts that both competence and ceremony are operationally real: specialists do maintain live knowledge, but this is accompanied by the displacement of distributed resilience and the creation of institutional dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_retention__hybrid_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
