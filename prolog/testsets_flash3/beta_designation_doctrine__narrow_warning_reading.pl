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
 *   constraint_id: beta_designation_doctrine__narrow_warning_reading
 *   human_readable: Beta Designation Doctrine (Narrow Warning Reading)
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   This constraint represents the 'narrow warning' reading of the beta
 *   designation doctrine, where beta status is a time-bounded disclosure for
 *   genuine testing, preserving base product liability. It functions as a
 *   scaffold, providing temporary support for software development while
 *   transitioning to full release. This reading emphasizes consumer
 *   protection and good-faith developer practices, contrasting with more
 *   expansive interpretations of liability waiver.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__narrow_warning_reading, 0.25).
domain_priors:suppression_score(beta_designation_doctrine__narrow_warning_reading, 0.3).
domain_priors:theater_ratio(beta_designation_doctrine__narrow_warning_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__narrow_warning_reading, scaffold).
narrative_ontology:human_readable(beta_designation_doctrine__narrow_warning_reading, "Beta Designation Doctrine (Narrow Warning Reading)").
narrative_ontology:topic_domain(beta_designation_doctrine__narrow_warning_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:has_sunset_clause(beta_designation_doctrine__narrow_warning_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__narrow_warning_reading, '3948d108-18fe-4aac-b82b-bd55c1e0bcfd').
narrative_ontology:cs_kernel_codification('3948d108-18fe-4aac-b82b-bd55c1e0bcfd', formalized).
narrative_ontology:cs_authority_grounding('3948d108-18fe-4aac-b82b-bd55c1e0bcfd', lineage).
narrative_ontology:cs_interpretation_layer_present('3948d108-18fe-4aac-b82b-bd55c1e0bcfd').
narrative_ontology:cs_reading_relation('3948d108-18fe-4aac-b82b-bd55c1e0bcfd', beta_designation_doctrine__expansive_shield_reading, coexists_with).
narrative_ontology:cs_reading_relation('3948d108-18fe-4aac-b82b-bd55c1e0bcfd', beta_designation_doctrine__severity_carve_out_reading, coexists_with).
narrative_ontology:cs_axiom('3948d108-18fe-4aac-b82b-bd55c1e0bcfd', foundational, liability_proportional_to_release_stage).
narrative_ontology:cs_axiom_status(liability_proportional_to_release_stage, holdable).
narrative_ontology:cs_axiom_grounding('3948d108-18fe-4aac-b82b-bd55c1e0bcfd', liability_proportional_to_release_stage, conventional).
narrative_ontology:cs_axiom('3948d108-18fe-4aac-b82b-bd55c1e0bcfd', foundational, consumer_baseline_rights_non_waivable).
narrative_ontology:cs_axiom_status(consumer_baseline_rights_non_waivable, holdable).
narrative_ontology:cs_axiom_grounding('3948d108-18fe-4aac-b82b-bd55c1e0bcfd', consumer_baseline_rights_non_waivable, deontological).
narrative_ontology:cs_reference_frame('3948d108-18fe-4aac-b82b-bd55c1e0bcfd', balanced_innovation_consumer_protection).
narrative_ontology:cs_drift_state('3948d108-18fe-4aac-b82b-bd55c1e0bcfd', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3948d108-18fe-4aac-b82b-bd55c1e0bcfd', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, software_developers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, beta_testers).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, consumer_protection_principle).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, good_faith_disclosure_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and release software in beta. They benefit from a temporary, limited liability shield during genuine testing phases, allowing them to gather feedback and fix bugs without full product liability. They are responsible for clear disclosure and time-bound testing.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, software_developers, agenda_setter,
    organized, biographical, constrained, global).

% Volunteer to test pre-release software. They receive early access to new features and influence product development. They are informed of the software's beta status and the associated risks, but retain basic product liability protections for severe, undisclosed defects.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, beta_testers, beneficiary,
    moderate, immediate, mobile, global).

% Enforce regulations ensuring that beta designations are used in good faith, are time-limited, and do not waive fundamental consumer rights. They act to prevent developers from using 'beta' as an indefinite liability shield.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, consumer_protection_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Are not directly involved in beta testing but are indirectly affected by the doctrine's interpretation. If the 'beta' shield is too broad, they may unknowingly use unstable software with reduced recourse. This reading protects their baseline rights.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, general_public, excluded,
    powerless, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the expectations and responsibilities between software developers and early users during a genuine, time-limited testing phase, allowing for iterative development and bug fixing before general release.
% TRANSFER_FUNCTION: Temporarily transfers a portion of liability risk from developers to beta testers in exchange for early access and feedback, while preserving a baseline of developer responsibility.
% ABSENT_VOICES: The general public, who are not beta testers, would object to any interpretation that allows 'beta' to become an indefinite or overly broad liability waiver, as it could expose them to undue risk when software moves from beta to general release without proper vetting.
% DISAPPEARANCE_RATIONALE: If this doctrine vanished, developers would face full product liability from the first release, likely leading to much longer, more expensive internal testing cycles, fewer public beta programs, and slower innovation. Users would have full recourse but less opportunity for early access and input.
% FOUNDING_PROBLEM: Software development requires iterative testing with real users, but full product liability from day one stifles innovation and makes early feedback prohibitively risky for developers. A mechanism was needed to balance innovation with user protection during pre-release phases.
% FOUNDING_PROBLEM_CORROBORATION: Software industry associations and legal scholars corroborate the ongoing need for a balanced approach to pre-release software liability. Consumer advocacy groups, while cautious, acknowledge the practical necessity of beta testing but emphasize the need for strict boundaries.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__narrow_warning_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__narrow_warning_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__narrow_warning_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is low (0.25) because the liability shield is limited and temporary, not a broad waiver. Suppression is also low (0.3) as users are informed and retain basic rights, and developers are constrained by the 'genuine testing phase' requirement. Theater ratio is low (0.1) because this reading focuses on the functional aspect of testing, not on using 'beta' as a performative label to avoid responsibility. The scaffold classification reflects its transitional nature and sunset clause (the end of the testing phase).
 *
 * PERSPECTIVAL GAP:
 *   Developers might prefer a more expansive shield, while consumer advocates would push for even stricter liability. This 'narrow warning' reading attempts to balance these perspectives, but the inherent tension means different stakeholders will always perceive the constraint's fairness and necessity differently. The engine's per-seat classification will reflect these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Software developers are beneficiaries, gaining a temporary, limited liability shield. Beta testers are also beneficiaries, getting early access and input opportunities while retaining core protections. Consumer protection agencies act as agenda-setters, defining and enforcing the boundaries of this temporary arrangement. The general public is excluded from direct participation but benefits from the doctrine's protective aspects.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_testing_definition,
    'What constitutes a ''genuine testing phase'' in practice, and how is its duration objectively determined?',
    'Industry-standard metrics for bug density, feature completeness, and user feedback loops, enforced by regulatory oversight or independent auditing.',
    'If ''genuine testing'' is ill-defined, developers could extend beta phases indefinitely, increasing extractiveness and shifting this reading towards an ''expansive shield'' interpretation. Clear definitions would reinforce its scaffold nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_testing_definition, empirical, 'Ambiguity in defining the scope and duration of a legitimate beta testing phase.').

omega_variable(
    kernel_reading_divergence,
    'Is this ''narrow warning'' reading of the beta designation doctrine the dominant legal interpretation, or do more expansive readings hold sway in practice or other jurisdictions?',
    'Analysis of case law, regulatory guidance, and industry practice across multiple jurisdictions. Comparison of legal outcomes under different interpretations.',
    'If expansive readings are dominant, the effective extractiveness and suppression of the ''beta designation'' kernel as a whole would be higher, and this ''narrow warning'' reading would represent a minority or aspirational view, rather than the prevailing constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'This constraint is one reading of the ''beta_designation_doctrine'' kernel. Sibling readings (''expansive_shield_reading'', ''severity_carve_out_reading'') would alter the liability balance and scope of application.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__narrow_warning_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(beta_tr_t5, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(beta_tr_t10, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(beta_tr_t15, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(beta_be_t5, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(beta_be_t10, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 10, 0.23).
narrative_ontology:measurement(beta_be_t15, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 15, 0.24).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 20, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(beta_su_t5, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 5, 0.27).
narrative_ontology:measurement(beta_su_t10, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(beta_su_t15, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 15, 0.29).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__narrow_warning_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
