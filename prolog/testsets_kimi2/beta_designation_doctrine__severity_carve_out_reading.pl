% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__severity_carve_out_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__severity_carve_out_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: beta_designation_doctrine__severity_carve_out_reading
 *   human_readable: Beta Designation Severity Carve-Out Doctrine
 *   domain: technology_law/software_liability
 *
 * SUMMARY:
 *   This constraint story instantiates the severity_carve_out_reading of the
 *   beta_designation_doctrine kernel. It captures the legal doctrine that
 *   beta or analogous pre-release labels cannot be used to defeat product
 *   liability, consumer protection, or tort claims in life-safety, financial,
 *   or other critical systems, irrespective of testing status or user
 *   disclosure. The doctrine operates as a domain-specific constraint on
 *   private ordering: harm severity overrides contractual liability
 *   allocation. As a kernel reading, it is one of three sibling readings; the
 *   expansive_shield_reading and narrow_warning_reading are modeled as
 *   separate constraints. The story treats the carve-out as a tangled rope
 *   because it solves a genuine coordination problem (preventing a
 *   race-to-disclaim in high-stakes software markets) while asymmetrically
 *   extracting from vendors by closing off a liability shield they could
 *   otherwise invoke.
 *
 * KEY AGENTS:
 *   - commercial_software_vendors (payer, powerful, global): bear liability exposure; lose beta shield
 *   - critical_system_operators (beneficiary, organized, national): retain tort recourse for software failure
 *   - end_users_critical_systems (beneficiary, powerless, national): protected from beta disclaimers
 *   - product_liability_judiciary (agenda_setter, institutional, national): adjudicates and enforces carve-out
 *   - consumer_protection_agencies (observer, institutional, national): monitor and advocate for user safety
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, 0.68).
domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, 0.75).
domain_priors:theater_ratio(beta_designation_doctrine__severity_carve_out_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__severity_carve_out_reading, tangled_rope).
narrative_ontology:human_readable(beta_designation_doctrine__severity_carve_out_reading, "Beta Designation Severity Carve-Out Doctrine").
narrative_ontology:topic_domain(beta_designation_doctrine__severity_carve_out_reading, "technology_law/software_liability").

domain_priors:requires_active_enforcement(beta_designation_doctrine__severity_carve_out_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__severity_carve_out_reading, '84882cdd-748b-4475-885c-39ebfec918d4').
narrative_ontology:cs_kernel_codification('84882cdd-748b-4475-885c-39ebfec918d4', formalized).
narrative_ontology:cs_authority_grounding('84882cdd-748b-4475-885c-39ebfec918d4', lineage).
narrative_ontology:cs_interpretation_layer_present('84882cdd-748b-4475-885c-39ebfec918d4').
narrative_ontology:cs_reading_relation('84882cdd-748b-4475-885c-39ebfec918d4', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('84882cdd-748b-4475-885c-39ebfec918d4', beta_designation_doctrine__narrow_warning_reading, coexists_with).
narrative_ontology:cs_axiom('84882cdd-748b-4475-885c-39ebfec918d4', foundational, severity_overrides_private_ordering).
narrative_ontology:cs_axiom_status(severity_overrides_private_ordering, holdable).
narrative_ontology:cs_axiom_grounding('84882cdd-748b-4475-885c-39ebfec918d4', severity_overrides_private_ordering, deontological).
narrative_ontology:cs_axiom('84882cdd-748b-4475-885c-39ebfec918d4', secondary, beta_label_irrelevant_to_safety_duty).
narrative_ontology:cs_axiom_status(beta_label_irrelevant_to_safety_duty, holdable).
narrative_ontology:cs_axiom_grounding('84882cdd-748b-4475-885c-39ebfec918d4', beta_label_irrelevant_to_safety_duty, conventional).
narrative_ontology:cs_reference_frame('84882cdd-748b-4475-885c-39ebfec918d4', consumer_protection_baseline).
narrative_ontology:cs_drift_state('84882cdd-748b-4475-885c-39ebfec918d4', contemporary_software_liability_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('84882cdd-748b-4475-885c-39ebfec918d4', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, critical_system_operators).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, end_users_critical_systems).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, commercial_software_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and distribute software for medical, financial, and infrastructure domains. Cannot invoke beta, preview, or experimental labels to disclaim liability for defects in critical systems regardless of testing depth or user disclosure. Bear strict liability and negligence exposure that would otherwise be contractually allocated to users.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, commercial_software_vendors, payer,
    powerful, biographical, constrained, global).

% Hospitals, banks, airlines, and utilities that deploy third-party software in life-safety and high-value operations. Rely on the carve-out to preserve tort and consumer-protection recourse when software fails, preventing vendors from escaping liability through labeling.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, critical_system_operators, beneficiary,
    organized, biographical, constrained, national).

% Patients, account holders, passengers, and the general public whose lives and assets depend on critical software. Lack bargaining power to assess beta status and benefit from a non-waivable baseline of vendor accountability.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, end_users_critical_systems, beneficiary,
    powerless, biographical, trapped, national).

% Courts and appellate bodies that interpret product liability and consumer protection law to hold beta status categorically unavailable as a defense in critical-system cases. Set precedent that overrides contractual disclaimers and codifies the severity carve-out.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, product_liability_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Regulatory bodies that monitor software markets, advocate for user safety, and file amicus briefs supporting the carve-out. Do not directly enforce the doctrine but shape its public justification and monitor compliance patterns.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, consumer_protection_agencies, observer,
    institutional, generational, analytical, national).

% Early-stage startups, open-source maintainers, and experimental-technology researchers who would argue the carve-out chills iterative deployment in critical domains. Underrepresented in product-liability precedent and regulatory hearings dominated by established vendors and institutional users.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, innovation_sector_maintainers, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__severity_carve_out_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents a collective-action failure in which software vendors race to disclaim liability for defects in life-safety and financial systems by deploying beta labels, preserving a baseline of user protection and predictable liability expectations in high-stakes domains.
% TRANSFER_FUNCTION: Moves liability risk from critical-system users and the public onto commercial software vendors by voiding beta-designation defenses, regardless of disclosure or testing status, in domains where failure severity is high.
% ABSENT_VOICES: Early-stage startups, open-source maintainers, and experimental-technology researchers who would argue the carve-out chills beneficial iterative deployment in critical domains; they are largely absent from product-liability precedent and regulatory hearings dominated by established vendors and institutional users.
% DISAPPEARANCE_RATIONALE: If the severity carve-out vanished, vendors would immediately reassert beta disclaimers across medical-device software, financial-trading infrastructure, and transportation systems; critical-system operators and end users would face abrupt loss of predictable tort recourse, and courts would need to reconstruct liability doctrine from first principles.
% FOUNDING_PROBLEM: Software vendors in the 1990s and 2000s deployed beta labels on safety-critical and financial software to exploit contractual disclaimer doctrines inherited from non-critical goods, externalizing catastrophic harm risk onto users who lacked bargaining power or technical sophistication to assess failure modes.
% FOUNDING_PROBLEM_CORROBORATION: Product-liability scholars, medical-device risk assessors, and insurance actuaries outside the direct beneficiary and payer sets document ongoing vendor attempts to label safety-critical deployments as beta, experimental, or previewâcorroborating that the founding problem has not been resolved by market maturation alone.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__severity_carve_out_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__severity_carve_out_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__severity_carve_out_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(beta_designation_doctrine__severity_carve_out_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__severity_carve_out_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint categorically strips vendors of a legal defense they would otherwise possess, imposing strict or negligence-based liability in critical domains. Suppression (0.75) reflects the active judicial and regulatory enforcement that voids contractual beta terms. Theater ratio is low (0.2) because the carve-out performs genuine liability-allocation work; there is little performative maintenance. Accessibility collapse (0.88) is high because once the doctrine is understood, the alternative (beta disclaimer) collapses as a viable legal strategy in covered domains. Resistance (0.72) is high because the technology sector consistently contests the doctrine through lobbying, contract design, and forum-shopping.
 *
 * PERSPECTIVAL GAP:
 *   The vendor seat experiences the constraint as pure extraction: it removes a legal option without providing compensating benefit. The user/operator seat experiences it as coordination: it preserves a baseline safety expectation that prevents a market-for-lemons in critical software liability. The judiciary and regulatory seats experience it as interpretive maintenance of a public-order exception to freedom of contract. The engine will compute these divergent per-seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial software vendors are declared victims (payer role) with constrained exit and global scope, placing them near the full-target end. Critical system operators and end users are declared beneficiaries with constrained and trapped exit respectively, placing them near the full-beneficiary end. The product liability judiciary, as agenda setter with analytical exit, sits near symmetric or slightly beneficiary-biased because it derives institutional authority from administering the carve-out.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy misclassification by requiring both genuine coordination (protecting critical-system users from a race-to-disclaim) and asymmetric extraction (vendors lose a liability shield) for the tangled_rope gate. A pure snare reading would miss the coordination function; a pure rope reading would miss the vendor cost. The temporal measurements show rising extraction as precedent solidified, confirming the asymmetry did not exist merely at founding but intensified as the doctrine matured.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    critical_system_boundary,
    'Where exactly does the severity carve-out draw the line between critical and non-critical systems, and does the boundary shift as software eats more of the physical world?',
    'Jurisprudential tracing of case outcomes across medical-device, financial-platform, transportation, and emerging AI-autonomy domains; statutory refinement if enacted.',
    'If the boundary expands to cover more systems, extractiveness rises for vendors; if it contracts or remains ambiguous, enforcement becomes patchy and suppression falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_system_boundary, conceptual, 'Ambiguity in the critical-system boundary definition').

omega_variable(
    functional_equivalent_labels,
    'Can vendors evade the carve-out by substituting functional equivalents such as preview, experimental, early-access, or alpha for beta?',
    'Precedential or statutory tests that look to functional effect rather than label form; natural experiment from jurisdictions that have adopted functional-effect statutes.',
    'If functional equivalents escape the carve-out, the constraint''s effective suppression is lower than measured and vendors retain a disguised liability shield; if the doctrine captures equivalents, suppression remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_equivalent_labels, empirical, 'Whether label substitution defeats the severity carve-out').

omega_variable(
    kernel_reading_contingency,
    'Would this constraint''s classification and epsilon change if a sibling reading of the same kernel were adopted as dominant doctrine?',
    'Comparative analysis of jurisdictions or historical periods where expansive_shield or narrow_warning readings prevailed; measurement of vendor liability exposure and user recourse under each regime.',
    'An expansive_shield_reading dominance would shift epsilon to near-zero for vendors and high for users, likely classifying as a snare or rope from the user seat. A narrow_warning_reading dominance would moderate epsilon and introduce time-boundedness, likely producing a scaffold or moderate tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Committee-frame omega documenting that this constraint is one reading of a contested kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__severity_carve_out_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(beta_tr_t5, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(beta_tr_t10, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(beta_tr_t15, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(beta_tr_t25, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 25, 0.2).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(beta_be_t5, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(beta_be_t10, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(beta_be_t15, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(beta_be_t25, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(beta_su_t5, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(beta_su_t10, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(beta_su_t15, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(beta_su_t25, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 25, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__narrow_warning_reading).

% DUAL FORMULATION NOTE:
% This story is one member of the beta_designation_doctrine kernel family. The kernel decomposes into three structurally distinct constraints: expansive_shield_reading (low epsilon, vendor-benefiting), narrow_warning_reading (moderate epsilon, time-bounded), and severity_carve_out_reading (high epsilon, user-protecting). Each reading has a different referent and different epsilon; they are linked through network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
