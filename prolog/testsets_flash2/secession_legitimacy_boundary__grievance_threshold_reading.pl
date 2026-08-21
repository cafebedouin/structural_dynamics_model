% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__grievance_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__grievance_threshold_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__grievance_threshold_reading
 *   human_readable: Secession Legitimacy: Grievance Threshold Reading
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint represents the 'grievance threshold' reading of secession
 *   legitimacy: secession becomes legitimate when federal actions cross a
 *   threshold of structural injustice, regardless of constitutional text.
 *   This reading posits that while federal authority is generally legitimate,
 *   its legitimacy is conditional and can be forfeited by severe and
 *   persistent overreach. The constraint is a Tangled Rope because it
 *   coordinates federal unity while extracting from aggrieved regions,
 *   requiring active enforcement to maintain the federal structure against
 *   secessionist pressures. The victim set only exists if the grievance
 *   threshold is crossed, which is a contested empirical claim.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, 0.65).
domain_priors:suppression_score(secession_legitimacy_boundary__grievance_threshold_reading, 0.7).
domain_priors:theater_ratio(secession_legitimacy_boundary__grievance_threshold_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__grievance_threshold_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__grievance_threshold_reading, "Secession Legitimacy: Grievance Threshold Reading").
narrative_ontology:topic_domain(secession_legitimacy_boundary__grievance_threshold_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__grievance_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__grievance_threshold_reading, '10a10d95-04c3-4af5-a00f-01e250fa7c77').
narrative_ontology:cs_kernel_codification('10a10d95-04c3-4af5-a00f-01e250fa7c77', distributed).
narrative_ontology:cs_authority_grounding('10a10d95-04c3-4af5-a00f-01e250fa7c77', distributed).
narrative_ontology:cs_reading_relation('10a10d95-04c3-4af5-a00f-01e250fa7c77', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('10a10d95-04c3-4af5-a00f-01e250fa7c77', secession_legitimacy_boundary__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('10a10d95-04c3-4af5-a00f-01e250fa7c77', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('10a10d95-04c3-4af5-a00f-01e250fa7c77', foundational, legitimacy_derives_from_justice_not_text).
narrative_ontology:cs_axiom_status(legitimacy_derives_from_justice_not_text, holdable).
narrative_ontology:cs_axiom_grounding('10a10d95-04c3-4af5-a00f-01e250fa7c77', legitimacy_derives_from_justice_not_text, deontological).
narrative_ontology:cs_axiom('10a10d95-04c3-4af5-a00f-01e250fa7c77', foundational, structural_injustice_forfeits_federal_authority).
narrative_ontology:cs_axiom_status(structural_injustice_forfeits_federal_authority, holdable).
narrative_ontology:cs_axiom_grounding('10a10d95-04c3-4af5-a00f-01e250fa7c77', structural_injustice_forfeits_federal_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('10a10d95-04c3-4af5-a00f-01e250fa7c77', conditional_federal_legitimacy).
narrative_ontology:cs_drift_state('10a10d95-04c3-4af5-a00f-01e250fa7c77', contemporary_federal_regional_tensions, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('10a10d95-04c3-4af5-a00f-01e250fa7c77', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, federal_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, majority_regions).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_regions).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, resource_producing_provinces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the integrity of the federation, collects taxes, and distributes resources. Views secession as an existential threat to national unity and constitutional order. Benefits from the current distribution of power and resources.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_government, agenda_setter,
    institutional, generational, constrained, national).

% Perceive themselves as suffering structural injustice (e.g., disproportionate resource extraction, cultural marginalization) that crosses a threshold of legitimacy for secession. Bear the costs of federal policies they deem unjust. Their identity is often tied to their regional distinctiveness and historical grievances.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_regions, payer,
    organized, generational, identity_locked, regional).

% Often overlap with aggrieved regions, bearing the costs of federal resource policies that they argue unfairly extract wealth without commensurate benefit. Their economic power gives them some leverage, but they are constrained by federal legal and economic structures.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, resource_producing_provinces, payer,
    powerful, biographical, constrained, regional).

% Benefit from the stability and resource redistribution within the existing federation. Their interests are generally aligned with the federal government in maintaining the status quo, as they are not the primary targets of perceived injustice.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, majority_regions, beneficiary,
    organized, biographical, mobile, regional).

% Monitor human rights, self-determination, and international law. Their assessment of whether a 'structural injustice' threshold has been crossed can lend legitimacy to secessionist claims, influencing international opinion and potential intervention.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the territorial integrity and political stability of the existing federal state, ensuring a unified legal and economic framework across diverse regions.
% TRANSFER_FUNCTION: Transfers political authority and resource control from aggrieved regions to the federal center, justified by the principle of national unity and constitutional order, until a threshold of injustice is crossed.
% ABSENT_VOICES: Indigenous nations whose ancestral lands span federal and provincial boundaries, and who assert pre-existing sovereignty, are often excluded from the federal-provincial debate on secession legitimacy. They would argue for their own self-determination rights as paramount.
% DISAPPEARANCE_RATIONALE: If the constraint of federal authority over secession (until a grievance threshold is met) vanished, aggrieved regions would immediately pursue independence, leading to rapid political and economic fragmentation, border disputes, and a complete reordering of the national map.
% FOUNDING_PROBLEM: The need to balance regional autonomy and distinct identities with the stability and collective benefits of a unified federal state, preventing arbitrary fragmentation while allowing for legitimate redress of severe grievances.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists outside the federal government corroborate the ongoing tension between regional autonomy and federal unity as a persistent challenge in federal systems. Aggrieved regions attest to the live nature of structural injustices.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__grievance_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__grievance_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__grievance_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(secession_legitimacy_boundary__grievance_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because aggrieved regions bear significant costs from federal policies they deem unjust, and their ability to exit is severely constrained. Suppression (0.70) is high due to the federal government's legal and coercive power to prevent unilateral secession. Theater ratio (0.20) is moderate; while the federal government genuinely coordinates national functions, a portion of its activity is performative maintenance of a unity narrative that masks underlying grievances. Resistance (0.80) is high, reflecting active secessionist movements and political challenges from aggrieved regions. Accessibility collapse (0.40) is moderate, as the option of secession, while difficult, is not entirely foreclosed conceptually.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, the constraint is a legitimate Rope ensuring national unity. From the aggrieved regions' perspective, it is a Snare that extracts resources and suppresses self-determination, with the 'grievance threshold' being a constantly shifting and unacknowledged line. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government and majority regions are beneficiaries, as they benefit from the stability and resource distribution of the existing federation. Aggrieved regions and resource-producing provinces are victims, bearing the costs of perceived structural injustice. International observers are analytical, assessing the legitimacy of grievances without direct participation in the extraction or coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to balance unity with justice. If the grievance threshold is consistently crossed without federal acknowledgment or redress, the coordination function atrophies, and the constraint risks becoming a Snare. The high resistance and extractiveness suggest this drift is ongoing, preventing mislabeling as pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    grievance_threshold_objectivity,
    'Is the ''structural injustice threshold'' an objectively verifiable standard, or is it subject to political interpretation and strategic framing by aggrieved parties?',
    'Development of internationally recognized criteria for structural injustice in federal systems, applied by independent arbitration bodies, or a clear legal framework for assessing such claims within the federal system itself.',
    'If objective, the constraint''s legitimacy is tied to demonstrable facts, potentially shifting power to aggrieved regions when the threshold is met. If subjective, the threshold becomes a rhetorical tool, reinforcing the federal government''s power to deny grievances.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grievance_threshold_objectivity, conceptual, 'Ambiguity of the ''structural injustice threshold'' as an objective standard.').

omega_variable(
    federal_overreach_definition,
    'What specific federal actions or policies constitute ''structural injustice'' sufficient to cross the legitimacy threshold for secession?',
    'A clear, pre-defined set of criteria or a judicial precedent that delineates what constitutes ''structural injustice'' in the context of federal-regional relations.',
    'A clear definition would reduce the federal government''s ability to deny grievances and provide a clearer path for aggrieved regions to assert legitimacy. Lack of definition maintains federal discretion and strengthens its position.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federal_overreach_definition, preference, 'Lack of clear definition for ''structural injustice'' and ''federal overreach''.').

omega_variable(
    identity_lock_mechanism,
    'To what extent is the ''identity_locked'' exit option for aggrieved regions a result of genuine cultural/historical identity, versus a strategic consolidation of political identity in response to federal policies?',
    'Sociological studies on the evolution of regional identity over time, particularly in response to federal-regional disputes, and analysis of the role of political elites in shaping identity narratives.',
    'If primarily genuine, the identity lock is a deep structural feature. If primarily strategic, the identity lock could be more fluid and responsive to changes in federal policy or political opportunities, potentially altering the exit options and thus the directionality for these agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Structural vs. strategic nature of identity lock for aggrieved regions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__grievance_threshold_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sece_tr_t10, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(sece_tr_t30, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(sece_tr_t50, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(sece_be_t10, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(sece_be_t30, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(sece_be_t50, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sece_su_t10, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(sece_su_t30, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(sece_su_t50, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__grievance_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'secession_legitimacy_boundary' kernel. It focuses on the legitimacy of secession based on a threshold of structural injustice, distinct from constitutional text, popular sovereignty, or treaty rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
