% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__narrow_warning_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
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
 *   human_readable: Beta Designation — Narrow Warning Reading (Time-Bounded Testing Disclosure)
 *   domain: technology/software_liability/consumer_protection
 *
 * SUMMARY:
 *   The narrow warning reading of the beta designation doctrine treats 'beta'
 *   labels as time-bounded disclosures that a product is in a genuine testing
 *   phase. It preserves the user's underlying product liability rights and
 *   requires that the testing phase be authentic — not a perpetual shield for
 *   shipped software. The constraint coordinates a temporary regime:
 *   developers get limited liability during good-faith testing in exchange
 *   for transparent disclosure; users get early access with informed consent.
 *   The liability shield has a structural sunset — it expires when the
 *   testing phase ends or when the product is commercially released. This
 *   reading claims the scaffold type: temporary coordination with a built-in
 *   sunset, not a permanent liability waiver.
 *
 * KEY AGENTS:
 *   - software_developers: Primary beneficiary (organized/constrained) — receives temporary liability limitation during genuine testing
 *   - early_adopter_users: Secondary beneficiary/payer (moderate/constrained) — gains early access but bears risk of experimental software
 *   - regulators_courts: Agenda setter (institutional/analytical) — enforces the boundary between genuine testing and de facto release
 *   - consumer_protection_advocates: Excluded (organized/analytical) — would argue for stricter testing phase boundaries and clearer disclosure standards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__narrow_warning_reading, 0.12).
domain_priors:suppression_score(beta_designation_doctrine__narrow_warning_reading, 0.08).
domain_priors:theater_ratio(beta_designation_doctrine__narrow_warning_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(beta_designation_doctrine__narrow_warning_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__narrow_warning_reading, scaffold).
narrative_ontology:human_readable(beta_designation_doctrine__narrow_warning_reading, "Beta Designation — Narrow Warning Reading (Time-Bounded Testing Disclosure)").
narrative_ontology:topic_domain(beta_designation_doctrine__narrow_warning_reading, "technology/software_liability/consumer_protection").

narrative_ontology:has_sunset_clause(beta_designation_doctrine__narrow_warning_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__narrow_warning_reading, '2305c119-2eee-47ef-9109-4ace431ff5f3').
narrative_ontology:cs_kernel_codification('2305c119-2eee-47ef-9109-4ace431ff5f3', distributed).
narrative_ontology:cs_authority_grounding('2305c119-2eee-47ef-9109-4ace431ff5f3', practice).
narrative_ontology:cs_reading_relation('2305c119-2eee-47ef-9109-4ace431ff5f3', beta_designation_doctrine__expansive_shield_reading, coexists_with).
narrative_ontology:cs_reading_relation('2305c119-2eee-47ef-9109-4ace431ff5f3', beta_designation_doctrine__severity_carve_out_reading, influences).
narrative_ontology:cs_axiom('2305c119-2eee-47ef-9109-4ace431ff5f3', foundational, liability_shield_requires_genuine_testing).
narrative_ontology:cs_axiom_status(liability_shield_requires_genuine_testing, holdable).
narrative_ontology:cs_axiom_grounding('2305c119-2eee-47ef-9109-4ace431ff5f3', liability_shield_requires_genuine_testing, conventional).
narrative_ontology:cs_axiom('2305c119-2eee-47ef-9109-4ace431ff5f3', foundational, base_product_liability_preserved_during_testing).
narrative_ontology:cs_axiom_status(base_product_liability_preserved_during_testing, holdable).
narrative_ontology:cs_axiom_grounding('2305c119-2eee-47ef-9109-4ace431ff5f3', base_product_liability_preserved_during_testing, deontological).
narrative_ontology:cs_reference_frame('2305c119-2eee-47ef-9109-4ace431ff5f3', genuine_testing_phase_framework).
narrative_ontology:cs_drift_state('2305c119-2eee-47ef-9109-4ace431ff5f3', perpetual_beta_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2305c119-2eee-47ef-9109-4ace431ff5f3', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, software_developers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__narrow_warning_reading, early_adopter_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(beta_designation_doctrine__narrow_warning_reading, early_adopter_users).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, product_liability_preservation_during_testing).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, genuine_testing_phase_requirement).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__narrow_warning_reading, informed_consent_for_experimental_software).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive a temporary liability limitation during genuine testing phases, enabling iterative development and user feedback loops. Must disclose beta status transparently and cannot maintain beta designation indefinitely. Exit is constrained by platform dependencies and market expectations but they can choose full release with full liability at any time.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, software_developers, beneficiary,
    organized, biographical, constrained, global).

% Gain early access to new features and influence product direction through feedback. Bear the risk of experimental software with known bugs and incomplete features. Preserve full product liability rights — the beta designation does not waive underlying claims. Exit is constrained by switching costs and data portability but they can decline beta participation.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, early_adopter_users, beneficiary,
    moderate, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__narrow_warning_reading, early_adopter_users, payer).

% Define and enforce the boundary between genuine testing phase and de facto commercial release. Police 'perpetual beta' patterns where beta designation is used as a permanent liability shield. Adjudicate disputes over whether a given beta period was genuine and time-bounded. Do not extract from the constraint; their role is boundary maintenance.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, regulators_courts, agenda_setter,
    institutional, generational, analytical, national).

% Would argue for stricter testing phase boundaries, mandatory disclosure standards, and clearer exit criteria for beta programs. Are not formal parties to the developer-user testing arrangement but seek to influence the regulatory boundary. Their exclusion from the direct constraint relationship is structural — the constraint operates between developers and users, with advocates as external pressure.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__narrow_warning_reading, consumer_protection_advocates, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__narrow_warning_reading, diffuse).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__narrow_warning_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables developers to recruit test users for iterative software development by providing a temporary, transparent liability limitation during genuine testing phases, while preserving users' underlying product liability rights.
% TRANSFER_FUNCTION: Temporarily shifts a portion of liability risk from developers to early adopter users during the testing phase only, in exchange for early access and feedback influence. The shift expires when the testing phase ends; base liability reverts to developers.
% ABSENT_VOICES: Consumer protection advocates and regulatory scholars who would demand stricter testing phase definitions, mandatory sunset enforcement, and clearer disclosure standards. They are excluded from the direct developer-user testing arrangement but seek to shape the regulatory boundary through external pressure.
% DISAPPEARANCE_RATIONALE: If the narrow warning reading vanished, developers would either face full liability during testing (chilling iteration) or users would lose early access channels. The mobile app ecosystem, SaaS beta programs, and open beta testing practices would reorganize — either toward more formal staged releases with full liability, or toward the expansive shield reading's de facto waiver. The coordination function is real and its removal would rearrange arrangements.
% FOUNDING_PROBLEM: Early software development lacked a recognized mechanism for public testing without exposing developers to full product liability for known-incomplete software, stalling iteration and user feedback loops.
% FOUNDING_PROBLEM_CORROBORATION: Software engineering literature on iterative development and beta testing practices (independent of vendor advocacy) attests the founding problem is live. The ACM and IEEE professional bodies document the ongoing need for structured testing phases. No beneficiary-only source is cited — the corroboration comes from engineering practice standards, not developer lobbying.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__narrow_warning_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__narrow_warning_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__narrow_warning_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(beta_designation_doctrine__narrow_warning_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__narrow_warning_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.12) because the constraint preserves base product liability and only temporarily limits it during a genuine testing phase. The coordination function is real: developers need a way to recruit test users without exposing themselves to full liability for known-incomplete software. Theater is low (0.15) because the constraint does not perform a function it does not serve — the sunset clause is structural (testing phase ends), not performative. Suppression is minimal (0.08) because users retain liability rights and can exit by not participating in beta programs. Resistance is moderate (0.35) because developers have incentives to stretch 'testing' definitions, and regulators must actively police the boundary. Accessibility collapse is low (0.25) because alternative arrangements (full release with full liability, open source with no warranty) remain fully available.
 *
 * PERSPECTIVAL GAP:
 *   From the developer seat, the constraint is a genuine coordination mechanism that enables iterative development. From the regulator seat, it is a boundary that requires active policing against 'perpetual beta' abuse. From the user seat, it is an informed consent arrangement — valuable for early access but risky if the testing phase boundary is porous. The engine computes per-seat classifications from these structural differences; the claimed scaffold type reflects the author's structural reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Software developers are the primary structural beneficiaries: they receive a temporary liability shield that enables testing coordination. Their directionality is toward the beneficiary end (d ~ 0.2). Early adopter users are near-symmetric: they gain early access (benefit) but bear experimental risk (cost) — d ~ 0.5. Regulators and courts are agenda setters with analytical exit — they administer the boundary but do not extract from the constraint. No victims are declared because base liability is preserved; extraction only occurs if the testing phase boundary is abused, which is a drift condition, not the constraint's structural design.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — enabling iterative software development through temporary liability limitation during genuine testing — remains live. The constraint has a structural sunset (testing phase end) that prevents mandatrophy by design. However, the 'perpetual beta' pattern creates a mandatrophy risk: if the testing phase boundary is not enforced, the temporary scaffold becomes a de facto permanent shield. The classification as scaffold with has_sunset_clause=true captures this structural safeguard; the omega on testing_phase_boundary_ambiguity captures the enforcement risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint one reading of the beta_designation_doctrine kernel rather than a standalone constraint?',
    'Kernel Context section declares kernel_id=beta_designation_doctrine, reading_id=narrow_warning_reading, with sibling readings expansive_shield_reading and severity_carve_out_reading. This omega records the committer structure.',
    'Confirms this story instantiates one specific reading with its own ε, beneficiaries, and classification; other readings are separate constraint stories linked via network.affects_constraints and cs_structure.reading_relations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Committee frame: this constraint is a kernel reading, not a flat constraint').

omega_variable(
    testing_phase_boundary_ambiguity,
    'Where is the boundary between a genuine testing phase and de facto release under the narrow warning reading?',
    'Case law on ''perpetual beta'' patterns, regulatory guidance on testing duration norms, industry standards for testing phase exit criteria.',
    'If the boundary is vague, extractiveness may be understated — developers could stretch testing phases indefinitely while preserving liability shield. A clear boundary supports the scaffold classification; a porous boundary drifts toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(testing_phase_boundary_ambiguity, empirical, 'Whether ''genuine testing phase'' has an enforceable structural boundary').

omega_variable(
    expansive_shield_coexistence,
    'Does the narrow warning reading structurally foreclose the expansive shield reading, or do they coexist as live positions in different jurisdictions or doctrinal traditions?',
    'Survey of appellate decisions, ALI Restatement positions, and scholarly commentary across common law jurisdictions to see if any single framework holds both as simultaneously valid.',
    'If they coexist, the kernel is genuinely contested with multiple live readings; if narrow warning forecloses expansive shield within a given framework, the relation is forecloses and the kernel has an internal resolution mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansive_shield_coexistence, conceptual, 'Structural relationship between narrow_warning_reading and expansive_shield_reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__narrow_warning_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(beta_tr_t5, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(beta_tr_t10, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(beta_tr_t15, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(beta_tr_t25, beta_designation_doctrine__narrow_warning_reading, theater_ratio, 25, 0.15).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(beta_be_t5, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 5, 0.07).
narrative_ontology:measurement(beta_be_t10, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 10, 0.09).
narrative_ontology:measurement(beta_be_t15, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 15, 0.11).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(beta_be_t25, beta_designation_doctrine__narrow_warning_reading, base_extractiveness, 25, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(beta_su_t5, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 5, 0.06).
narrative_ontology:measurement(beta_su_t10, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 10, 0.07).
narrative_ontology:measurement(beta_su_t15, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 15, 0.08).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement(beta_su_t25, beta_designation_doctrine__narrow_warning_reading, suppression_requirement, 25, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__narrow_warning_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(beta_designation_doctrine__narrow_warning_reading, 0.1).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__severity_carve_out_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the beta designation doctrine into three structurally distinct readings per the ε-invariance principle. The narrow warning reading (this story) has ε=0.12 and scaffold classification. The expansive shield reading would have substantially higher ε (est. 0.65+) and snare/tangled_rope classification. The severity carve-out reading would have near-zero ε for critical systems (mountain-like). They are linked via affects_constraints and reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
