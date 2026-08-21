% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__state_centric_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: common_article_3_scope__state_centric_reading
 *   human_readable: Common Article 3 Scope (State-Centric Reading)
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   This constraint represents the state-centric reading of Common Article 3
 *   (CA3) of the Geneva Conventions, which holds that CA3 applies only when a
 *   non-international armed conflict (NIAC) meets specific thresholds of
 *   intensity and organization. This interpretation excludes low-level
 *   violence, internal disturbances, and routine law enforcement operations
 *   from the scope of International Humanitarian Law (IHL), thereby
 *   preserving maximum operational discretion for national governments and
 *   their armed forces. This reading is one of several competing
 *   interpretations of CA3's scope, forming a kernel of contestation in
 *   international law.
 *
 * KEY AGENTS:
 *   - national_governments: Agenda setter (institutional/constrained) — defines and enforces the scope.
 *   - state_military_forces: Beneficiary (institutional/constrained) — operates under reduced IHL obligations.
 *   - irregular_combatants_below_threshold: Payer (powerless/trapped) — denied IHL protections.
 *   - civilian_populations_in_low_intensity_conflict: Payer (powerless/trapped) — increased vulnerability.
 *   - international_humanitarian_organizations: Observer (organized/analytical) — advocates for broader application.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, 0.65).
domain_priors:suppression_score(common_article_3_scope__state_centric_reading, 0.78).
domain_priors:theater_ratio(common_article_3_scope__state_centric_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__state_centric_reading, "Common Article 3 Scope (State-Centric Reading)").
narrative_ontology:topic_domain(common_article_3_scope__state_centric_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(common_article_3_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__state_centric_reading, 'd0b5511b-3840-488f-9807-01bbc3d2342b').
narrative_ontology:cs_kernel_codification('d0b5511b-3840-488f-9807-01bbc3d2342b', fixed_text).
narrative_ontology:cs_authority_grounding('d0b5511b-3840-488f-9807-01bbc3d2342b', lineage).
narrative_ontology:cs_interpretation_layer_present('d0b5511b-3840-488f-9807-01bbc3d2342b').
narrative_ontology:cs_reading_relation('d0b5511b-3840-488f-9807-01bbc3d2342b', common_article_3_scope__expansive_human_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('d0b5511b-3840-488f-9807-01bbc3d2342b', common_article_3_scope__icrc_customary_reading, coexists_with).
narrative_ontology:cs_axiom('d0b5511b-3840-488f-9807-01bbc3d2342b', foundational, state_sovereignty_primacy_in_internal_affairs).
narrative_ontology:cs_axiom_status(state_sovereignty_primacy_in_internal_affairs, holdable).
narrative_ontology:cs_axiom_grounding('d0b5511b-3840-488f-9807-01bbc3d2342b', state_sovereignty_primacy_in_internal_affairs, conventional).
narrative_ontology:cs_axiom('d0b5511b-3840-488f-9807-01bbc3d2342b', foundational, threshold_based_application_of_ihl).
narrative_ontology:cs_axiom_status(threshold_based_application_of_ihl, holdable).
narrative_ontology:cs_axiom_grounding('d0b5511b-3840-488f-9807-01bbc3d2342b', threshold_based_application_of_ihl, conventional).
narrative_ontology:cs_reference_frame('d0b5511b-3840-488f-9807-01bbc3d2342b', westphalian_state_sovereignty_framework).
narrative_ontology:cs_drift_state('d0b5511b-3840-488f-9807-01bbc3d2342b', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d0b5511b-3840-488f-9807-01bbc3d2342b', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(common_article_3_scope__state_centric_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, state_military_forces).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, national_governments).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, civilian_populations_in_low_intensity_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Governments interpret CA3 narrowly to preserve maximum operational discretion for their armed forces and law enforcement, particularly in situations of internal unrest or counter-terrorism operations that do not meet high thresholds of intensity and organization. They benefit from reduced legal obligations in such contexts.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, national_governments, agenda_setter,
    institutional, generational, constrained, national).

% Military forces operate under this interpretation, which limits the application of IHL to their actions in certain domestic or low-intensity scenarios, allowing them to use domestic law enforcement rules of engagement rather than IHL's stricter protections for detainees and combatants.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, state_military_forces, beneficiary,
    institutional, biographical, constrained, national).

% Individuals participating in armed violence that does not meet the 'intensity' and 'organization' thresholds for IHL application. They are denied combatant status and IHL protections, being treated as criminals under domestic law, often facing harsher penalties and conditions of detention.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold, payer,
    powerless, immediate, trapped, local).

% Civilians caught in conflicts that states classify as below the CA3 threshold. They may experience violence, detention, and lack of humanitarian access without the full protective framework of IHL being formally applied, leading to increased vulnerability.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, civilian_populations_in_low_intensity_conflict, payer,
    powerless, immediate, trapped, local).

% Advocate for a broader application of CA3 and document violations, but their ability to influence state practice is limited by state sovereignty and the narrow interpretation of IHL scope by powerful states. They observe and report, but cannot directly enforce.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, international_humanitarian_organizations, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to coordinate their legal obligations in armed conflicts, ensuring a minimum standard of humanity even in non-international armed conflicts, while defining the boundaries of IHL application.
% TRANSFER_FUNCTION: Transfers legal discretion and reduced accountability to national governments and their armed forces by limiting the scope of IHL application, while transferring increased vulnerability and reduced protections to irregular combatants and civilians in low-intensity conflicts.
% ABSENT_VOICES: Victims of state violence in situations deemed 'below threshold' by governments, who would argue for universal application of minimum humanitarian standards regardless of conflict classification. Their voices are often suppressed by state control over information and lack of legal standing.
% DISAPPEARANCE_RATIONALE: If this state-centric reading of CA3's scope vanished, states would face immediate pressure to apply IHL more broadly, significantly altering their legal and operational frameworks for internal security operations and counter-insurgency. The legal landscape of armed conflict would be fundamentally reshaped.
% FOUNDING_PROBLEM: To establish a minimum standard of humanity applicable to non-international armed conflicts, where states were reluctant to recognize belligerent status to non-state actors, thereby avoiding full IHL obligations.
% FOUNDING_PROBLEM_CORROBORATION: National governments and their legal advisors attest that the problem of defining conflict scope remains live and critical for maintaining state sovereignty and effective law enforcement. International humanitarian organizations and human rights bodies, from outside the benefiting parties, attest that the original problem has evolved, and the current interpretation now serves to limit accountability rather than merely define scope.
narrative_ontology:disappearance_verdict(common_article_3_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__state_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(common_article_3_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__state_centric_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the cost borne by those denied IHL protections, while suppression (0.78) is high due to states' active efforts to maintain this narrow interpretation and resist broader applications. The theater ratio is low (0.15) because the state-centric reading is a functional legal interpretation, not primarily performative, though it may be used to justify actions that appear to violate humanitarian norms. The increasing extractiveness and suppression over time reflect the rise of internal conflicts and counter-terrorism operations where states have consistently pushed for a narrow IHL scope.
 *
 * PERSPECTIVAL GAP:
 *   National governments and their military forces perceive this constraint as a necessary legal boundary for maintaining sovereignty and effective security operations, viewing it as a 'rope' that coordinates state action. Irregular combatants and affected civilians, however, experience it as a 'snare' that denies them fundamental protections, leaving them vulnerable to state power without recourse to IHL.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments and state military forces are clear beneficiaries, as the constraint reduces their legal obligations and increases their operational flexibility (low directionality). Irregular combatants and civilian populations in 'below threshold' conflicts are clear targets, bearing the costs of reduced protection (high directionality). International humanitarian organizations act as observers, attempting to influence the interpretation but not directly benefiting or paying.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a 'tangled rope' because it genuinely coordinates state action by defining legal boundaries (a coordination function) but does so with significant asymmetric extraction from those excluded from IHL protection. The mandatrophy analysis reveals that while the original mandate was to define scope, the persistence of this narrow reading increasingly serves to shield state actors from accountability, rather than solely to clarify legal obligations. The 'contested' status of the founding problem highlights this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_definition_ambiguity,
    'Is the ''intensity'' and ''organization'' threshold for CA3 application an objective legal standard or a flexible political tool for states to avoid IHL obligations?',
    'Analysis of state practice in diverse conflict types, judicial rulings by international courts, and expert consensus on the interpretation of these thresholds. If application consistently correlates with state interest rather than objective criteria, it suggests political flexibility.',
    'If primarily a political tool, the constraint''s effective extractiveness and suppression are higher than measured, as the ''legal'' justification is cover for discretionary power. This would push the classification closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_definition_ambiguity, conceptual, 'Ambiguity in the objectivity of CA3''s application thresholds.').

omega_variable(
    human_rights_law_overlap,
    'To what extent does international human rights law (IHRL) provide an adequate protective floor for individuals in situations where IHL, under this reading, does not apply?',
    'Comparative legal analysis of IHRL and IHL protections in specific ''below threshold'' scenarios, assessing gaps in protection, enforcement mechanisms, and remedies available to victims. Empirical studies of IHRL application in such contexts.',
    'If IHRL provides robust and enforceable protections, the effective extractiveness of this constraint is lower, as victims are not entirely unprotected. If IHRL is weak or unenforced in these contexts, extractiveness is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_rights_law_overlap, empirical, 'Overlap and adequacy of IHRL protections where IHL is excluded.').

omega_variable(
    kernel_reading_divergence,
    'This constraint is one reading of the common_article_3_scope kernel. What would be the classification of the expansive_human_rights_reading and icrc_customary_reading siblings, and how would their metrics differ?',
    'Generate separate constraint stories for each sibling reading, with their own metrics and stakeholder analyses, then compare the resulting classifications and effective extraction values.',
    'The divergence in classifications and metrics across readings would quantify the ''cost'' of the state-centric interpretation in terms of human protection and state accountability. A significant divergence would highlight the political nature of the legal interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Quantifying the structural differences between competing readings of CA3''s scope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__state_centric_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__state_centric_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(comm_tr_t1970, common_article_3_scope__state_centric_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(comm_tr_t1990, common_article_3_scope__state_centric_reading, theater_ratio, 1990, 0.13).
narrative_ontology:measurement(comm_tr_t2001, common_article_3_scope__state_centric_reading, theater_ratio, 2001, 0.14).
narrative_ontology:measurement(comm_tr_t2010, common_article_3_scope__state_centric_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(comm_tr_t2024, common_article_3_scope__state_centric_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__state_centric_reading, base_extractiveness, 1949, 0.4).
narrative_ontology:measurement(comm_be_t1970, common_article_3_scope__state_centric_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(comm_be_t1990, common_article_3_scope__state_centric_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(comm_be_t2001, common_article_3_scope__state_centric_reading, base_extractiveness, 2001, 0.62).
narrative_ontology:measurement(comm_be_t2010, common_article_3_scope__state_centric_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(comm_be_t2024, common_article_3_scope__state_centric_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__state_centric_reading, suppression_requirement, 1949, 0.5).
narrative_ontology:measurement(comm_su_t1970, common_article_3_scope__state_centric_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(comm_su_t1990, common_article_3_scope__state_centric_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(comm_su_t2001, common_article_3_scope__state_centric_reading, suppression_requirement, 2001, 0.75).
narrative_ontology:measurement(comm_su_t2010, common_article_3_scope__state_centric_reading, suppression_requirement, 2010, 0.77).
narrative_ontology:measurement(comm_su_t2024, common_article_3_scope__state_centric_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, rules_of_engagement_doctrine).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, detention_standards_in_niacs).

% DUAL FORMULATION NOTE:
% This constraint is one of three competing readings of the 'common_article_3_scope' kernel. The other readings are 'expansive_human_rights_reading' and 'icrc_customary_reading', each with distinct structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
