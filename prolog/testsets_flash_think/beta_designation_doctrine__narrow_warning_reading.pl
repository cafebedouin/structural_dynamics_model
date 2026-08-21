% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__narrow_warning_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__narrow_warning_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: beta_designation_doctrine__narrow_warning_reading
 *   human_readable: Beta Designation Doctrine (Narrow Warning Reading)
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   This constraint represents the 'narrow warning' reading of the beta
 *   designation doctrine, which holds that beta status is a time-bounded
 *   disclosure for genuine testing phases, preserving base product liability.
 *   It aims to balance software innovation with consumer protection. This
 *   reading emphasizes good-faith bounds and informed user consent, ensuring
 *   that beta users are not victimized while developers gain a temporary,
 *   limited liability shield. The classification as 'scaffold' reflects its
 *   intended temporary and supportive nature for a transitional phase of
 *   software development.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__narrow_warning_reading, 0.25).
domain_priors:suppression_score(beta_designation_doctrine__narrow_warning_reading, 0.3).
domain_priors:theater_ratio(beta_designation_doctrine__narrow_warning_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__narrow_warning_reading, scaffold).
narrative_ontology:human_readable(beta_designation_doctrine__narrow_warning_reading, "Beta Designation Doctrine (Narrow Warning Reading)").
narrative_ontology:topic_domain(beta_designation_doctrine__narrow_warning_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:has_sunset_clause(beta_designation_doctrine__narrow_warning_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__narrow_warning_reading, '3468781e-7575-4047-91ad-10a42cd44e49').
narrative_ontology:cs_kernel_codification('3468781e-7575-4047-91ad-10a42cd44e49', formalized).
narrative_ontology:cs_authority_grounding('3468781e-7575-4047-91ad-10a42cd44e49', lineage).
narrative_ontology:cs_interpretation_layer_present('3468781e-7575-4047-91ad-10a42cd44e49').
narrative_ontology:cs_reading_relation('3468781e-7575-4047-91ad-10a42cd44e49', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('3468781e-7575-4047-91ad-10a42cd44e49', beta_designation_doctrine__severity_carve_out_reading, coexists_with).
narrative_ontology:cs_axiom('3468781e-7575-4047-91ad-10a42cd44e49', foundational, software_is_product_under_law).
narrative_ontology:cs_axiom_status(software_is_product_under_law, holdable).
narrative_ontology:cs_axiom_grounding('3468781e-7575-4047-91ad-10a42cd44e49', software_is_product_under_law, conventional).
narrative_ontology:cs_axiom('3468781e-7575-4047-91ad-10a42cd44e49', foundational, risk_requires_informed_consent).
narrative_ontology:cs_axiom_status(risk_requires_informed_consent, holdable).
narrative_ontology:cs_axiom_grounding('3468781e-7575-4047-91ad-10a42cd44e49', risk_requires_informed_consent, deontological).
narrative_ontology:cs_reference_frame('3468781e-7575-4047-91ad-10a42cd44e49', balanced_innovation_safety_framework).
narrative_ontology:cs_drift_state('3468781e-7575-4047-91ad-10a42cd44e49', contemporary_legal_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3468781e-7575-4047-91ad-10a42cd44e49', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, software_developers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, beta_users).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, general_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(beta_designation_doctrine__narrow_warning_reading, beta_users).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, consumer_protection_principle).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, good_faith_disclosure_norm).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, innovation_incentive_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designate their software as 'beta' to facilitate public testing and feedback. They benefit from reduced, but not eliminated, liability during this genuine testing phase, allowing for iterative development and early user engagement. They must adhere to disclosure requirements and time limits.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, software_developers, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__narrow_warning_reading, software_developers, beneficiary).

% Voluntarily use beta software, accepting disclosed risks in exchange for early access and influence over product development. They bear the risk of bugs and instability but are protected by the preservation of base product liability for severe defects or undisclosed hazards. Their exit option is to stop using the beta or wait for a stable release.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, beta_users, payer,
    moderate, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__narrow_warning_reading, beta_users, beneficiary).

% Interpret and enforce the doctrine, ensuring that beta designations are genuinely time-bounded testing phases, that disclosures are adequate, and that base product liability is preserved. They act to balance innovation incentives with consumer protection.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Benefits from the overall framework that encourages innovation while maintaining a baseline of safety and accountability for software products. They are indirectly protected by the liability standards that apply once software exits the beta phase or for severe issues even during beta.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, general_public, beneficiary,
    powerless, generational, constrained, national).

% Analyze the evolution and application of the beta designation doctrine, assessing its effectiveness in balancing developer incentives with consumer protection. They contribute to the conceptual understanding and potential reform of the doctrine.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__narrow_warning_reading, diffuse).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__narrow_warning_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To facilitate the public testing and iterative development of software by providing a temporary, limited liability framework, while ensuring users are informed of risks and core product liability remains intact.
% TRANSFER_FUNCTION: Transfers some immediate liability risk from software developers to beta users (with informed consent), in exchange for early access and influence. It preserves the transfer of ultimate liability for severe defects back to developers.
% ABSENT_VOICES: Developers who advocate for a broader, more permanent liability shield for beta software, and consumer advocates who argue for stricter liability even during beta phases. These voices are present in policy debates but are not fully accommodated by this narrow reading.
% DISAPPEARANCE_RATIONALE: Without a clear and balanced beta designation doctrine, software developers would face increased risk for public testing, potentially leading to slower innovation, less user feedback, or a shift towards less transparent testing methods. Consumers would either face higher undisclosed risks or slower access to new technologies, leading to a significant reorganization of software development and consumption practices.
% FOUNDING_PROBLEM: How to enable rapid innovation and user-driven development in software by allowing public testing of incomplete products, without exposing developers to prohibitive liability for every bug, while simultaneously protecting consumers from unreasonable or undisclosed risks.
% FOUNDING_PROBLEM_CORROBORATION: Legal precedents, legislative debates surrounding software liability, and ongoing discussions among technology law experts and consumer protection agencies consistently highlight the persistent tension between fostering innovation and ensuring consumer safety in software development. This corroboration comes from outside the immediate benefiting parties (developers).
narrative_ontology:disappearance_verdict(beta_designation_doctrine__narrow_warning_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__narrow_warning_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__narrow_warning_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(beta_designation_doctrine__narrow_warning_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__narrow_warning_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__narrow_warning_reading_tests).
:- end_tests(beta_designation_doctrine__narrow_warning_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because this reading focuses on risk management and disclosure, not rent-seeking; any 'extraction' is the cost of early access and testing, offset by preserved liability. Suppression is low (0.30) as users are presumed to have alternatives and informed choice. Theater ratio is low (0.15) because the doctrine, under this reading, demands a genuine testing phase, not a perpetual 'beta' label to avoid liability. Accessibility collapse is moderate (0.40) as users might feel a pull to access new features early, but stable alternatives exist. Resistance is moderate (0.35) from developers who might prefer a broader shield and from some consumer advocates who desire stricter liability.
 *
 * PERSPECTIVAL GAP:
 *   While this reading aims for balance, developers might still perceive the time bounds and liability preservation as overly restrictive, hindering innovation. Conversely, some consumer advocates might view the temporary liability reduction as still too permissive, even with disclosure. The engine's per-seat classification would reflect these differing experiences of the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Software developers are beneficiaries as they gain a limited liability shield for testing. Beta users are also beneficiaries (early access, informed risk) but also payers (bear some risk). The general public benefits from the overall balanced framework. Regulatory bodies act as agenda-setters, enforcing the balance. No explicit victims are identified in this reading, as users are informed and base liability is preserved.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_testing_phase_definition,
    'What constitutes a ''genuine testing phase'' in practice, and how is its duration objectively determined and enforced?',
    'Development of industry-wide standards for beta testing, regulatory guidelines with clear metrics for ''readiness for release,'' and judicial precedents defining ''genuine testing'' in specific cases.',
    'If ''genuine testing'' is loosely defined or poorly enforced, the constraint could drift towards a ''piton'' or ''snare'' where the ''beta'' label becomes theatrical cover for avoiding liability, increasing effective extraction on users.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_testing_phase_definition, empirical, 'Ambiguity in defining and enforcing the ''genuine testing phase'' aspect of beta designation.').

omega_variable(
    base_product_liability_scope,
    'What specific types of defects or harms are covered by ''base product liability'' during a beta phase, and how does this scope compare to post-release liability?',
    'Further legislative clarification, detailed regulatory guidance, and judicial interpretation of ''base product liability'' in the context of beta software, particularly concerning foreseeable harms.',
    'If ''base product liability'' is interpreted too narrowly, users could be left unprotected from significant harms, shifting the constraint towards a ''snare'' by effectively expanding the liability shield beyond its intended scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(base_product_liability_scope, conceptual, 'Ambiguity regarding the precise scope of preserved product liability during the beta phase.').

omega_variable(
    kernel_contest_resolution,
    'Which reading of the beta designation doctrine (narrow warning, expansive shield, or severity carve-out) will ultimately prevail in legal and regulatory frameworks?',
    'Landmark court decisions, comprehensive legislative reforms, or widespread adoption of a particular interpretation by industry and regulatory bodies.',
    'If the ''expansive_shield_reading'' prevails, the constraint would reclassify as a ''snare'' with high extraction and suppression. If the ''severity_carve_out_reading'' is adopted, it would introduce a new ''mountain'' or ''snare'' for critical systems, potentially influencing this constraint''s scope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_resolution, preference, 'The fundamental contest over the interpretation and application of the beta designation doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__narrow_warning_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t2000, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(beta_tr_t2005, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(beta_tr_t2010, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 2010, 0.13).
narrative_ontology:measurement(beta_tr_t2015, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 2015, 0.14).
narrative_ontology:measurement(beta_tr_t2020, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(beta_tr_t2025, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(beta_be_t2000, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 2000, 0.2).
narrative_ontology:measurement(beta_be_t2005, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 2005, 0.22).
narrative_ontology:measurement(beta_be_t2010, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 2010, 0.23).
narrative_ontology:measurement(beta_be_t2015, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 2015, 0.24).
narrative_ontology:measurement(beta_be_t2020, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 2020, 0.25).
narrative_ontology:measurement(beta_be_t2025, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 2025, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t2000, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 2000, 0.25).
narrative_ontology:measurement(beta_su_t2005, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 2005, 0.27).
narrative_ontology:measurement(beta_su_t2010, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 2010, 0.28).
narrative_ontology:measurement(beta_su_t2015, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 2015, 0.29).
narrative_ontology:measurement(beta_su_t2020, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 2020, 0.3).
narrative_ontology:measurement(beta_su_t2025, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 2025, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__narrow_warning_reading, information_standard).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, software_liability_law).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, consumer_rights_law).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, software_innovation_incentives).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'beta_designation_doctrine' kernel, focusing on time-bounded disclosure and preserved liability. It is linked to other readings (expansive_shield_reading, severity_carve_out_reading) that represent alternative interpretations of the same underlying legal concept.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
