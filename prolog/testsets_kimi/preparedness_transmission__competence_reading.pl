% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_transmission__competence_reading
 *   human_readable: Preparedness Transmission via Live Exercised Knowledge (Competence Reading)
 *   domain: disaster_risk_management/institutional_memory
 *
 * SUMMARY:
 *   This constraint story instantiates the competence reading of the
 *   preparedness_transmission kernel. In this reading, drills and inspections
 *   are not bureaucratic ritual but live exercised knowledge: each generation
 *   of emergency personnel re-validates operational capability through
 *   practice, inspectors maintain pattern-recognition for novel failure
 *   signatures, and participants demonstrate adaptive improvisation. The
 *   constraint coordinates the preservation of tacit, embodied knowledge that
 *   cannot be transmitted through documentation alone. It is authored as a
 *   rope â genuine coordination with minimal extraction â in direct
 *   structural contrast to the husk reading (ritualized piton) and hybrid
 *   reading (stratified tangled rope) of the same kernel.
 *
 * KEY AGENTS:
 *   - emergency_management_authorities: agenda_setter (institutional/constrained) â designs and mandates drill regimes
 *   - response_operators: primary beneficiary (organized/constrained) â gains validated improvisation capacity
 *   - technical_inspectors: beneficiary (organized/constrained) â maintains expertise through active practice
 *   - civilian_jurisdictions: beneficiary (organized/constrained) â receives protected status
 *   - infrastructure_engineers: beneficiary (moderate/constrained) â gains design validation feedback
 *   - disaster_researchers: observer (analytical) â corroborates competence claims from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__competence_reading, 0.22).
domain_priors:suppression_score(preparedness_transmission__competence_reading, 0.15).
domain_priors:theater_ratio(preparedness_transmission__competence_reading, 0.16).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, theater_ratio, 0.16).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__competence_reading, rope).
narrative_ontology:human_readable(preparedness_transmission__competence_reading, "Preparedness Transmission via Live Exercised Knowledge (Competence Reading)").
narrative_ontology:topic_domain(preparedness_transmission__competence_reading, "disaster_risk_management/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__competence_reading, '068b4684-5faa-4513-8dd2-b579b75001fe').
narrative_ontology:cs_kernel_codification('068b4684-5faa-4513-8dd2-b579b75001fe', implicit).
narrative_ontology:cs_authority_grounding('068b4684-5faa-4513-8dd2-b579b75001fe', practice).
narrative_ontology:cs_interpretation_layer_present('068b4684-5faa-4513-8dd2-b579b75001fe').
narrative_ontology:cs_reading_relation('068b4684-5faa-4513-8dd2-b579b75001fe', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('068b4684-5faa-4513-8dd2-b579b75001fe', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('068b4684-5faa-4513-8dd2-b579b75001fe', foundational, operational_competence_requires_embodied_practice).
narrative_ontology:cs_axiom_status(operational_competence_requires_embodied_practice, holdable).
narrative_ontology:cs_axiom_grounding('068b4684-5faa-4513-8dd2-b579b75001fe', operational_competence_requires_embodied_practice, instrumental).
narrative_ontology:cs_axiom('068b4684-5faa-4513-8dd2-b579b75001fe', foundational, adaptive_capacity_validated_through_novel_scenarios).
narrative_ontology:cs_axiom_status(adaptive_capacity_validated_through_novel_scenarios, holdable).
narrative_ontology:cs_axiom_grounding('068b4684-5faa-4513-8dd2-b579b75001fe', adaptive_capacity_validated_through_novel_scenarios, empirically_contingent).
narrative_ontology:cs_reference_frame('068b4684-5faa-4513-8dd2-b579b75001fe', live_practice_competence).
narrative_ontology:cs_drift_state('068b4684-5faa-4513-8dd2-b579b75001fe', contemporary_institutional_review, gap(stable, minor, true)).
narrative_ontology:cs_created_at('068b4684-5faa-4513-8dd2-b579b75001fe', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__competence_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, response_operators).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, technical_inspectors).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, civilian_jurisdictions).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, infrastructure_engineers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design drill protocols, accredit exercise programs, and mandate inspection cycles for civil defense systems. Their institutional mandate is tied to statutory preparedness requirements; they cannot abandon live exercise regimes without legislative or charter revision, but they derive legitimacy from demonstrated response outcomes.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, emergency_management_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Frontline emergency personnel who participate in simulated scenarios and after-action reviews. They gain validated improvisation skills and pattern-recognition capacity that transfers to novel disaster conditions; their professional standing and safety depend on maintained competence. Exit means leaving emergency response careers entirely.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, response_operators, beneficiary,
    organized, biographical, constrained, regional).

% Specialists who inspect physical infrastructure and observe exercise performance. Their expertise stays current through repeated exposure to edge-case simulations; they recognize novel failure signatures because active practice continually updates their mental models beyond static checklists.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, technical_inspectors, beneficiary,
    organized, biographical, constrained, regional).

% Municipalities and populations protected by the civil defense system. They fund preparedness through taxation and receive the benefit of maintained response capacity. They cannot easily opt out of regional emergency-management arrangements or substitute alternative protection systems.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, civilian_jurisdictions, beneficiary,
    organized, generational, constrained, regional).

% Engineers responsible for dams, utilities, and critical facilities. Inspections and stress exercises provide feedback loops that validate design assumptions against real-world behavior; they gain actionable data that documentation alone cannot supply. Their professional scope is tied to regulated inspection regimes.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, infrastructure_engineers, beneficiary,
    moderate, biographical, constrained, national).

% Independent academics and after-action analysts who study institutional memory and response effectiveness. They observe exercise quality and disaster outcomes across jurisdictions without being governed by the constraint themselves; their findings corroborate or challenge the competence reading from outside the beneficiary set.
narrative_ontology:constraint_stakeholder(preparedness_transmission__competence_reading, disaster_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves operational knowledge across personnel turnover and generational change by embedding it in repeated, varied practice that must be physically performed and adaptively completed, preventing atrophy in high-risk, low-frequency domains.
% TRANSFER_FUNCTION: Moves organizational time, personnel capacity, and material resources from immediate production into simulation cycles and inspection regimes, returning validated improvisation capability and cross-generational competence continuity.
% ABSENT_VOICES: Ritual-theory scholars and organizational ethnographers who interpret drills as memorial theater rather than functional learning; also veteran personnel who have experienced drill regimes that genuinely decayed into empty routine, whose testimony is often filtered out by quantitative performance metrics.
% DISAPPEARANCE_RATIONALE: Emergency response organizations would lose the mechanism that validates improvisation under novel conditions; institutional memory would compress into static written protocols unable to adapt to unscripted failures, and validated competence would decay within one to two personnel turnover cycles.
% FOUNDING_PROBLEM: Operational knowledge in disaster response atrophies between events because written protocols cannot encode improvisation capacity, and personnel turnover severs experiential continuity; organizations that have not practiced under varied stress conditions fail when novel failures emerge.
% FOUNDING_PROBLEM_CORROBORATION: Independent disaster sociology researchers and multi-jurisdictional after-action reviewers attest that exercised competence predicts effective novel-response outcomes; historical institutional-memory studies confirm capability decay in organizations that rely on procedural compliance without practice.
narrative_ontology:disappearance_verdict(preparedness_transmission__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__competence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_transmission__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__competence_reading, 0.22, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__competence_reading_tests).
:- end_tests(preparedness_transmission__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metrics are authored low-extractive because the competence reading asserts that drill and inspection costs are investments returning readiness, not rents. Theater ratio is low (0.16) because performative maintenance is minimal relative to functional learning. Accessibility collapse is moderately high (0.72) because once the domain accepts that improvisation requires embodied practice, paper-based alternatives collapse as viable substitutes â but this is an instrumental collapse, not a natural-law collapse. Resistance is low (0.12) because the coordination benefit is widely recognized among participants. The slight upward drift in base_extractiveness and theater_ratio over the interval reflects gradual bureaucratic accretion across fifty years of institutional maturity, not functional decay.
 *
 * PERSPECTIVAL GAP:
 *   The competence reading and the husk reading of the same kernel diverge at the level of empirical observation: an agenda_setter operating from the competence reading sees functional learning, while an observer operating from the husk reading sees the same drill schedule as inertial theater. The engine computes this divergence from structural relationship data; the authored claim of rope does not adjudicate the kernel contest but records one structurally coherent reading.
 *
 * DIRECTIONALITY LOGIC:
 *   All seated agents are net beneficiaries in this reading: emergency_management_authorities fulfill their mandate and derive institutional legitimacy; response_operators and technical_inspectors maintain professional competence that underwrites their standing and safety; civilian_jurisdictions receive protected status; infrastructure_engineers gain validation feedback loops. No victim seat is structurally necessary because the constraint returns coordination surplus to all governed parties. The agenda_setter bears organizational cost but receives mandate fulfillment.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy by tying the constraint's persistence to demonstrated competence outcomes rather than procedural compliance. If the founding problem â knowledge atrophy in low-frequency, high-risk domains â were solved by other means (e.g., synthetic training environments that genuinely substitute for live practice), the constraint would become a scaffold and require a sunset clause. As authored, the founding problem remains live, and the coordination function is current rather than atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is preparedness transmission currently functioning as live exercised knowledge, or has it decayed into ritual performance?',
    'Comparative after-action analysis of improvisation depth versus protocol adherence across jurisdictions; ethnographic observation of drill debriefs for novel signature recognition.',
    'If decayed to ritual, the constraint''s extractiveness and theater ratio are substantially higher than this reading suggests, trending toward piton or snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Empirical contest between competence and husk readings of the same kernel').

omega_variable(
    competence_stratification,
    'Is operational competence uniformly high across physical infrastructure and civilian coordination domains, or is it stratified?',
    'Sector-specific competence audits separating engineering response metrics from civilian mobilization metrics.',
    'If stratified, the constraint is not a single rope but a hybrid or tangled structure with uneven extraction profiles across domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_stratification, empirical, 'Whether competence is uniform or stratified across infrastructure and civilian domains').

omega_variable(
    compliance_motivation,
    'Is drill participation sustained by demonstrated competence value or by institutionalized mandate habit?',
    'Survey and behavioral data on participation quality and improvisation engagement when mandates are relaxed or incentives shift.',
    'If mandate-driven without perceived value, the constraint''s coordination function is weaker than authored and the rope classification may not hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_motivation, empirical, 'Whether compliance is value-driven or habit-driven').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__competence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_transmission__competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(prep_tr_t10, preparedness_transmission__competence_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(prep_tr_t20, preparedness_transmission__competence_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(prep_tr_t30, preparedness_transmission__competence_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(prep_tr_t40, preparedness_transmission__competence_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement(prep_tr_t50, preparedness_transmission__competence_reading, theater_ratio, 50, 0.16).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_transmission__competence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(prep_be_t10, preparedness_transmission__competence_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement(prep_be_t20, preparedness_transmission__competence_reading, base_extractiveness, 20, 0.17).
narrative_ontology:measurement(prep_be_t30, preparedness_transmission__competence_reading, base_extractiveness, 30, 0.19).
narrative_ontology:measurement(prep_be_t40, preparedness_transmission__competence_reading, base_extractiveness, 40, 0.2).
narrative_ontology:measurement(prep_be_t50, preparedness_transmission__competence_reading, base_extractiveness, 50, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_transmission__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__competence_reading, resource_allocation).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is the competence reading of the preparedness_transmission kernel, which decomposes into three structurally distinct readings: competence (live exercised knowledge), husk (memorial ritual), and hybrid (stratified decay). Each reading carries a different epsilon, beneficiary structure, and institutional interpretation of the same drills and inspections.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
