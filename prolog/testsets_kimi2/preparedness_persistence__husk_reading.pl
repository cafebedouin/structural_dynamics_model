% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__husk_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: preparedness_persistence__husk_reading
 *   human_readable: Disaster Preparedness Ritual as Memorial Performance
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   Disaster preparedness drills and inspections, originally designed to
 *   maintain operational competence across agencies that rarely face
 *   catastrophes, have become memorial performance. The formsâevacuation
 *   rehearsals, equipment checks, after-action reportsâpersist with high
 *   fidelity while the underlying response capacity atrophies. Emergency
 *   management agencies treat completion metrics as proof of readiness,
 *   populations read visible drills as assurance of protection, and frontline
 *   responders know the gap but are structurally excluded from redesign
 *   conversations. This constraint is the husk reading of the
 *   preparedness_persistence kernel, which asks whether drills and
 *   inspections maintain live competence or have become ritual. Sibling
 *   readings include the competence reading (live exercised knowledge) and
 *   the hybrid reading (stratified competence across components).
 *
 * KEY AGENTS:
 *   - Emergency management agencies: agenda-setter administering the ritual; identity-locked to institutional legitimacy maintenance.
 *   - Population at flood risk: payer bearing catastrophic exposure masked by performed safety.
 *   - Frontline emergency responders: excluded analytical voice filtered out by command hierarchy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__husk_reading, 0.55).
domain_priors:suppression_score(preparedness_persistence__husk_reading, 0.45).
domain_priors:theater_ratio(preparedness_persistence__husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__husk_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__husk_reading, "Disaster Preparedness Ritual as Memorial Performance").
narrative_ontology:topic_domain(preparedness_persistence__husk_reading, "disaster_preparedness/institutional_memory/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__husk_reading, 'c019a602-0c43-4a8c-b442-05a827f85999').
narrative_ontology:cs_kernel_codification('c019a602-0c43-4a8c-b442-05a827f85999', formalized).
narrative_ontology:cs_authority_grounding('c019a602-0c43-4a8c-b442-05a827f85999', extraction).
narrative_ontology:cs_interpretation_layer_present('c019a602-0c43-4a8c-b442-05a827f85999').
narrative_ontology:cs_reading_relation('c019a602-0c43-4a8c-b442-05a827f85999', preparedness_persistence__competence_reading, forecloses).
narrative_ontology:cs_reading_relation('c019a602-0c43-4a8c-b442-05a827f85999', preparedness_persistence__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('c019a602-0c43-4a8c-b442-05a827f85999', foundational, operational_form_not_competence).
narrative_ontology:cs_axiom_status(operational_form_not_competence, holdable).
narrative_ontology:cs_axiom_grounding('c019a602-0c43-4a8c-b442-05a827f85999', operational_form_not_competence, empirically_contingent).
narrative_ontology:cs_axiom('c019a602-0c43-4a8c-b442-05a827f85999', foundational, ritual_sustains_institutional_legitimacy).
narrative_ontology:cs_axiom_status(ritual_sustains_institutional_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('c019a602-0c43-4a8c-b442-05a827f85999', ritual_sustains_institutional_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('c019a602-0c43-4a8c-b442-05a827f85999', functional_preparedness_regime).
narrative_ontology:cs_drift_state('c019a602-0c43-4a8c-b442-05a827f85999', contemporary_audit_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('c019a602-0c43-4a8c-b442-05a827f85999', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__husk_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, emergency_management_agencies).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, population_at_flood_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conduct scheduled drills and file inspection reports as mandated by protocol. Their budgets and public standing depend on documented compliance and visible activity. They inherit procedures from earlier eras and continue them because reform would require admitting that past performance was inadequate, which carries political and career risk.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, emergency_management_agencies, agenda_setter,
    institutional, generational, identity_locked, national).

% Live in designated flood zones and receive official assurances that preparedness protocols are in place. Attend or observe evacuation drills that follow scripts from decades past. Cannot independently verify whether the underlying response capacity matches the ritual, and cannot opt out of the geographic risk.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, population_at_flood_risk, payer,
    powerless, biographical, trapped, local).

% Carry out the drills and respond to actual incidents. Aware that equipment, staffing, and decision protocols have drifted away from the scenarios rehearsed in drills. Their reports of gaps are filtered by command hierarchy; they are not invited to redesign the preparedness system.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, frontline_emergency_responders, excluded,
    moderate, biographical, identity_locked, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__husk_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_persistence__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally solved the collective-action problem of maintaining shared operational readiness across distributed agencies and populations by creating standardized rehearsal and verification protocols for rare, high-consequence events.
% TRANSFER_FUNCTION: Moves institutional legitimacy, continued funding, and bureaucratic survival to emergency management agencies, while transferring catastrophic risk exposure and false confidence to populations who rely on the appearance of preparedness.
% ABSENT_VOICES: Frontline responders who know the drills are decoupled from reality, and affected populations who would demand function over form if they understood the atrophy. Both are kept out of redesign conversations by upward-reporting filters and public-relations framing that treats drill completion as success.
% DISAPPEARANCE_RATIONALE: If the drill-and-inspection ritual disappeared overnight, emergency management agencies would lose their primary visible proof of function and funding justification would collapse. Populations would be forced to confront their unprotected status. The institutional world of preparedness would rearrange around either genuine competence-building or exposed vulnerability.
% FOUNDING_PROBLEM: How to maintain coordinated operational readiness for rare, high-consequence disaster events across agencies and populations that do not face them regularly.
% FOUNDING_PROBLEM_CORROBORATION: Disaster sociologists and independent post-disaster review commissions outside the benefiting agencies attest that the gap between drill performance and real-world outcomes has widened over decades; frontline responder testimony and external operational audits document competence gaps that the drill regime did not prevent.
narrative_ontology:disappearance_verdict(preparedness_persistence__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_persistence__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__husk_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_persistence__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_persistence__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater_ratio is high (0.82) because the constraint's primary activity is performative rehearsal of obsolete scripts rather than functional competence maintenance. Base_extractiveness is moderate (0.55) because the extraction is diffuse: it takes the form of false confidence and delayed reform rather than concentrated rent. Suppression is moderate (0.45) because the ritual crowds out alternative framings through bureaucratic normalization and identity fusion, not through overt coercion. Resistance is low (0.15) because drills appear benign and publicly valuable, making opposition politically illegible. Accessibility_collapse is high (0.65) because once the ritual is institutionally established, populations and oversight bodies cannot readily distinguish performed from real preparedness.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the constraint as institutional survival and legitimate public service; the payer seat experiences it as protection that will fail when tested; the excluded seat sees the decoupling directly but lacks channel or standing to force revision. The engine computes this divergence from structural dataâ beneficiary legitimacy, trapped exit for payers, identity-locked exclusionâwithout requiring reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Emergency management agencies derive diffuse legitimacy and continued authorization from the ritual (low d, treated as subsidy). The population at flood risk bears the catastrophic downside of false confidence (high d, amplified extraction). Frontline responders are identity-locked to the profession and structurally excluded from the agenda, placing them near the target end despite their operational knowledge. No agent captures the extracted value as concentrated gain; the flow is diffuse institutional legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve a genuine coordination problem: maintaining readiness for rare events. That founding problem is deadâoperational competence has atrophiedâbut the arrangement persists because it now generates institutional legitimacy and is too costly to reform. Classifying it as piton rather than snare captures the absence of a concentrated beneficiary: no party extracts enough to fight for maintenance, and no party is hurt enough in ordinary times to force fix. The high theater_ratio and dead founding_problem_status confirm the mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_reading_kernel_position,
    'Does the husk reading''s claim that drills are pure memorial performance foreclose the hybrid reading''s stratified model, or can both readings coexist within different institutional frameworks?',
    'Examine whether practitioners can consistently hold that evacuation drills are purely ritual while engineering inspections remain competent; if both are held by the same actors without contradiction, the readings coexist.',
    'If foreclosed, the husk reading competes with hybrid for the same explanatory space; if coexisting, they explain different strata of the same institutional domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_reading_kernel_position, conceptual, 'Structural relationship between husk reading and hybrid reading in the preparedness persistence kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative preparedness framings structural (enforced by budget and command hierarchy) or internalized (populations and agencies believe drills equal safety)?',
    'Post-disaster cognitive interviewing: if populations and agencies abandon the drill framework after a single failure, suppression was structural; if they rationalize failure and retain faith in the ritual, suppression is internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target carries the suppression with them after external barriers fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').

omega_variable(
    competence_atrophy_empirical_basis,
    'Is operational competence actually atrophying, or has the nature of competence shifted to documentation and compliance management rather than physical response?',
    'Independent operational audits measuring response times, resource availability, and decision quality against historical baselines, controlling for event severity.',
    'If competence has not atrophied but transformed, the constraint is a rope or scaffold rather than a piton; if competence has genuinely decayed, the piton classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_atrophy_empirical_basis, empirical, 'Whether observed atrophy reflects genuine degradation or competence migration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__husk_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(prep_tr_t5, preparedness_persistence__husk_reading, theater_ratio, 5, 0.45).
narrative_ontology:measurement(prep_tr_t10, preparedness_persistence__husk_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(prep_tr_t15, preparedness_persistence__husk_reading, theater_ratio, 15, 0.65).
narrative_ontology:measurement(prep_tr_t20, preparedness_persistence__husk_reading, theater_ratio, 20, 0.72).
narrative_ontology:measurement(prep_tr_t25, preparedness_persistence__husk_reading, theater_ratio, 25, 0.77).
narrative_ontology:measurement(prep_tr_t30, preparedness_persistence__husk_reading, theater_ratio, 30, 0.8).
narrative_ontology:measurement(prep_tr_t40, preparedness_persistence__husk_reading, theater_ratio, 40, 0.82).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__husk_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(prep_be_t5, preparedness_persistence__husk_reading, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(prep_be_t10, preparedness_persistence__husk_reading, base_extractiveness, 10, 0.49).
narrative_ontology:measurement(prep_be_t15, preparedness_persistence__husk_reading, base_extractiveness, 15, 0.51).
narrative_ontology:measurement(prep_be_t20, preparedness_persistence__husk_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(prep_be_t25, preparedness_persistence__husk_reading, base_extractiveness, 25, 0.53).
narrative_ontology:measurement(prep_be_t30, preparedness_persistence__husk_reading, base_extractiveness, 30, 0.54).
narrative_ontology:measurement(prep_be_t40, preparedness_persistence__husk_reading, base_extractiveness, 40, 0.55).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_persistence__husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is the husk reading of the preparedness_persistence kernel, which decomposes the natural-language concept of disaster preparedness into three structurally distinct claims: live competence (competence_reading), memorial performance (husk_reading), and stratified function (hybrid_reading). Each reading carries a different epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
