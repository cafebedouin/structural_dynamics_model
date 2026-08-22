% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__narrow_warning_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Beta Designation as Time-Bounded Testing Disclosure (Narrow Warning Reading)
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   This constraint captures the narrow-warning reading of the contested
 *   'beta designation' kernel in software liability law: a developer's use of
 *   the beta label is a good-faith, time-bounded disclosure of a genuine
 *   testing phase. Under this reading, the label informs users of elevated
 *   instability risk but does NOT waive the developer's underlying product
 *   liability, and the label's legitimacy depends on the designation actually
 *   tracking a bounded testing period rather than becoming a permanent
 *   liability shield. This reading sits between two sibling readings — an
 *   expansive-shield reading that would treat beta status as comprehensive
 *   and potentially indefinite liability waiver, and a severity-carve-out
 *   reading that would deny beta status entirely for life-safety or
 *   financial-critical systems. This story authors ONLY the narrow-warning
 *   reading as its own clean constraint; the siblings are separate
 *   constraints linked via network and reading_relations, not blended into
 *   this ε.
 *
 * KEY AGENTS:
 *   - software_developers_conducting_genuine_testing: primary agenda-setter and beneficiary — administers the designation, retains base liability
 *   - early_adopter_testers: beneficiary/payer — accepts disclosed instability risk in exchange for early access, retains recourse for underlying defects
 *   - consumer_protection_regulators: analytical observer — polices whether the duration and disclosure are genuine
 *   - courts_adjudicating_beta_disputes: analytical observer — resolves factual disputes about testing-phase genuineness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__narrow_warning_reading, 0.28).
domain_priors:suppression_score(beta_designation_doctrine__narrow_warning_reading, 0.15).
domain_priors:theater_ratio(beta_designation_doctrine__narrow_warning_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__narrow_warning_reading, scaffold).
narrative_ontology:human_readable(beta_designation_doctrine__narrow_warning_reading, "Beta Designation as Time-Bounded Testing Disclosure (Narrow Warning Reading)").
narrative_ontology:topic_domain(beta_designation_doctrine__narrow_warning_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:has_sunset_clause(beta_designation_doctrine__narrow_warning_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__narrow_warning_reading, '3eb2a65b-17c3-4b9e-9deb-8b6bfbaa8755').
narrative_ontology:cs_kernel_codification('3eb2a65b-17c3-4b9e-9deb-8b6bfbaa8755', distributed).
narrative_ontology:cs_authority_grounding('3eb2a65b-17c3-4b9e-9deb-8b6bfbaa8755', practice).
narrative_ontology:cs_interpretation_layer_present('3eb2a65b-17c3-4b9e-9deb-8b6bfbaa8755').
narrative_ontology:cs_reading_relation('3eb2a65b-17c3-4b9e-9deb-8b6bfbaa8755', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('3eb2a65b-17c3-4b9e-9deb-8b6bfbaa8755', beta_designation_doctrine__severity_carve_out_reading, coexists_with).
narrative_ontology:cs_axiom('3eb2a65b-17c3-4b9e-9deb-8b6bfbaa8755', foundational, base_liability_survives_designation).
narrative_ontology:cs_axiom_status(base_liability_survives_designation, holdable).
narrative_ontology:cs_axiom_grounding('3eb2a65b-17c3-4b9e-9deb-8b6bfbaa8755', base_liability_survives_designation, conventional).
narrative_ontology:cs_axiom('3eb2a65b-17c3-4b9e-9deb-8b6bfbaa8755', foundational, duration_must_track_genuine_testing_activity).
narrative_ontology:cs_axiom_status(duration_must_track_genuine_testing_activity, holdable).
narrative_ontology:cs_axiom_grounding('3eb2a65b-17c3-4b9e-9deb-8b6bfbaa8755', duration_must_track_genuine_testing_activity, empirically_contingent).
narrative_ontology:cs_reference_frame('3eb2a65b-17c3-4b9e-9deb-8b6bfbaa8755', pre_digital_disclaimer_practice).
narrative_ontology:cs_drift_state('3eb2a65b-17c3-4b9e-9deb-8b6bfbaa8755', contemporary_software_release_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3eb2a65b-17c3-4b9e-9deb-8b6bfbaa8755', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, software_developers_conducting_genuine_testing).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, early_adopter_testers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(beta_designation_doctrine__narrow_warning_reading, early_adopter_testers).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, informed_consent_doctrine).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, base_liability_preservation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Applies the beta label to a product actually undergoing a bounded testing phase, discloses known limitations to users, and collects real-world performance data before general release. Retains full base product liability throughout — the label buys time-limited disclosure cover, not immunity. Must end the beta period and either ship a supported release or withdraw the product; cannot hold the label indefinitely without the designation losing its factual basis.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, software_developers_conducting_genuine_testing, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__narrow_warning_reading, software_developers_conducting_genuine_testing, beneficiary).

% Chooses to use a disclosed-as-unfinished product, often for early access, discounted pricing, or influence over final design. Bears the ordinary risks of unfinished software (bugs, instability) that the disclosure covers, but retains recourse for the underlying product defects the beta label does not excuse. Can decline participation or exit to a stable alternative at low cost.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, early_adopter_testers, beneficiary,
    moderate, immediate, mobile, national).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__narrow_warning_reading, early_adopter_testers, payer).

% Monitors whether beta designations track an actual testing phase or have become a standing liability shield. Under this reading, regulators find the designation defensible precisely because the underlying liability survives and the duration is policed against the real testing timeline.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, consumer_protection_regulators, observer,
    institutional, generational, analytical, national).

% Interprets whether a given beta designation was a genuine, time-bounded testing phase or a mislabeled permanent release. Under this reading, the court's task is bounded and factual: verify duration and disclosure, not adjudicate categorical exclusions or blanket waivers.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, courts_adjudicating_beta_disputes, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__narrow_warning_reading, diffuse).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__narrow_warning_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows developers to release genuinely unfinished software for real-world testing while giving users clear, time-bounded notice of that status — solving the problem of how testing can happen at scale without either freezing innovation or leaving users uninformed.
% TRANSFER_FUNCTION: Moves disclosure risk (the cost of possible instability) from developer to informed user for a bounded period, while leaving the underlying product-defect liability exactly where it would sit for a finished release.
% ABSENT_VOICES: Users harmed by a beta that quietly overstayed its testing window with no genuine sunset are not separately voiced here — this reading assumes the duration-policing mechanism catches that case, but if enforcement is weak in practice, those users would object that the disclosure became a shield without narrowing.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished, some testing programs would continue informally under general disclaimer law, while others might halt rather than risk full liability for admittedly unfinished software — whether this constitutes 'the world rearranges' or 'stays roughly the same' is disputed between developers (who say testing would shrink) and consumer advocates (who say base liability already covers the relevant harms).
% FOUNDING_PROBLEM: Software cannot be perfected before real-world use reveals its flaws, but releasing untested software without any signal to users invites both harm and unbounded liability exposure that would deter beneficial testing altogether.
% FOUNDING_PROBLEM_CORROBORATION: Independent software engineering research on iterative release cycles corroborates that real-world testing surfaces defects unreachable in controlled environments; consumer protection agencies attest the problem remains live but caution that enforcement of the duration/genuineness bound is inconsistent across jurisdictions — a concern this narrow reading treats as an enforcement gap, not a doctrinal flaw.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__narrow_warning_reading, contested).
narrative_ontology:founding_problem_status(beta_designation_doctrine__narrow_warning_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__narrow_warning_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(beta_designation_doctrine__narrow_warning_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__narrow_warning_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is modest (0.28) and rises only slightly over the interval because, under this reading, the base liability floor never disappears — the designation shifts disclosure risk, not defect risk, and the amount extracted from users is capped by the requirement that the testing phase be genuine and bounded. Suppression is low (0.15) because users retain meaningful exit (declining beta participation, using stable releases) and retain legal recourse for underlying defects; nothing about this reading requires suppressing alternatives. Theater ratio is low-to-moderate and drifts slowly upward (0.12 to 0.20) reflecting a mild, realistic risk that some developers extend beta labeling past the point of genuine testing without immediate detection, but the reading treats this as a policing problem, not a structural feature.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers occupy the beneficiary/agenda-setter position: they set the designation and benefit from bounded disclosure cover, but bear continuing liability for the underlying product, which caps how far the constraint can extract from users. Early adopter testers are near-symmetric: they receive early access and influence in exchange for accepting disclosed risk, and their mobile exit option (declining participation) keeps their directionality away from a full-target position. No victim group is declared under this reading because the doctrine, correctly applied, produces informed rather than victimized users — this is the structural core of the narrow-warning reading and what distinguishes it from the expansive-shield reading, which would generate victims by permitting the shield to outlive the testing phase.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification with a genuine sunset clause is the mechanism that prevents this constraint from sliding into a permanent extraction structure: the designation is legitimate only while the testing phase is real, and its coordination function (enabling beneficial real-world testing without unbounded liability exposure) is explicitly transitional. If a story authored this same label without a sunset requirement or without preserved base liability, it would not be this constraint — it would be the expansive-shield reading, a structurally different claim with a different ε.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuineness_of_testing_phase_ambiguity,
    'How is ''genuine testing phase'' distinguished from a de facto permanent release wearing a beta label, absent a bright-line duration rule?',
    'Case law or regulatory guidance establishing objective indicators (feature completeness trajectory, user base growth pattern, revenue treatment, marketing language) that separate authentic testing from disguised general release.',
    'Without a workable genuineness test, this narrow reading collapses in practice into the expansive-shield reading — the doctrinal distinction exists on paper but cannot be enforced, which would justify reclassifying this story''s suppression and extraction upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuineness_of_testing_phase_ambiguity, conceptual, 'Whether the narrow reading''s central limiting principle is operationally enforceable.').

omega_variable(
    sibling_reading_contest_location,
    'Where exactly does the disagreement between the three readings live — is it about WHO the designation applies to (severity carve-out), or about WHAT the designation waives (expansive shield vs. narrow warning)?',
    'Doctrinal analysis separating the two axes of contest: scope-of-applicability (which systems can ever use beta status) versus scope-of-waiver (what liability the status affects) — these are logically independent axes within the kernel.',
    'If the two axes are independent, all three readings could in principle be held jointly by a single coherent legal framework (narrow warning + severity carve-out together, excluding only expansive shield); if they are entangled, adopting narrow-warning implicitly narrows the space available to severity-carve-out.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_contest_location, conceptual, 'Structural location of disagreement among the three kernel readings.').

omega_variable(
    beneficiary_status_of_developers_under_fsm_lens,
    'Is the low extraction measured here an artifact of assuming good-faith developer compliance, or would relaxing that assumption reveal higher latent extraction even under the narrow reading''s own terms?',
    'Empirical audit of beta-labeled products across a sample of software releases, tracking actual testing-phase duration against announced duration and liability outcomes in disputes.',
    'If audits show systematic overstay even under nominally narrow-reading regimes, this reading''s low ε may understate real-world operation and the story would need revision toward higher extractiveness or reclassification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_status_of_developers_under_fsm_lens, empirical, 'Whether the reading''s favorable metrics depend on an empirically untested compliance assumption.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__narrow_warning_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(beta_tr_t4, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(beta_tr_t8, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(beta_tr_t12, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(beta_tr_t16, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(beta_tr_t24, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 24, 0.2).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(beta_be_t4, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 4, 0.24).
narrative_ontology:measurement(beta_be_t8, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(beta_be_t12, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 12, 0.26).
narrative_ontology:measurement(beta_be_t16, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 16, 0.27).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(beta_be_t24, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 24, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(beta_designation_doctrine__narrow_warning_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__narrow_warning_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(beta_designation_doctrine__narrow_warning_reading, 0.12).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__severity_carve_out_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the beta_designation_doctrine kernel. narrow_warning_reading (this story) authors low ε (0.28) reflecting preserved base liability and a genuine-duration requirement. expansive_shield_reading would author substantially higher ε reflecting comprehensive and potentially indefinite waiver. severity_carve_out_reading addresses a different axis (categorical unavailability for critical systems) and would itself carry low ε for the contexts it governs, since its function is exclusionary rather than extractive. All three are linked via affects_constraints rather than merged into one story, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
