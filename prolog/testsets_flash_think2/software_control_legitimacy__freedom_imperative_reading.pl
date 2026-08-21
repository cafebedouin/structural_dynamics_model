% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__freedom_imperative_reading, []).

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
 *   constraint_id: software_control_legitimacy__freedom_imperative_reading
 *   human_readable: Software Control as User Freedom Imperative
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint story instantiates the 'freedom imperative' reading of
 *   the software control legitimacy kernel. From this perspective,
 *   proprietary software is ethically illegitimate because it denies users
 *   fundamental control over their computing. The constraint is the ethical
 *   demand for user freedom, which proprietary software is seen as violating.
 *   This reading frames proprietary software as a 'snare' due to its high
 *   extraction of user freedom and the active suppression of alternatives
 *   through legal and technical means. The metrics reflect the severity of
 *   this ethical violation from the perspective of this reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, 0.85).
domain_priors:suppression_score(software_control_legitimacy__freedom_imperative_reading, 0.75).
domain_priors:theater_ratio(software_control_legitimacy__freedom_imperative_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_control_legitimacy__freedom_imperative_reading, "Software Control as User Freedom Imperative").
narrative_ontology:topic_domain(software_control_legitimacy__freedom_imperative_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__freedom_imperative_reading, 'a5f8761b-3eb1-4366-b6f7-9882f4262a7a').
narrative_ontology:cs_kernel_codification('a5f8761b-3eb1-4366-b6f7-9882f4262a7a', formalized).
narrative_ontology:cs_authority_grounding('a5f8761b-3eb1-4366-b6f7-9882f4262a7a', practice).
narrative_ontology:cs_interpretation_layer_present('a5f8761b-3eb1-4366-b6f7-9882f4262a7a').
narrative_ontology:cs_reading_relation('a5f8761b-3eb1-4366-b6f7-9882f4262a7a', software_control_legitimacy__pragmatic_openness_reading, forecloses).
narrative_ontology:cs_reading_relation('a5f8761b-3eb1-4366-b6f7-9882f4262a7a', software_control_legitimacy__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('a5f8761b-3eb1-4366-b6f7-9882f4262a7a', software_control_legitimacy__commons_reading, forecloses).
narrative_ontology:cs_axiom('a5f8761b-3eb1-4366-b6f7-9882f4262a7a', foundational, software_must_be_free_as_in_freedom).
narrative_ontology:cs_axiom_status(software_must_be_free_as_in_freedom, holdable).
narrative_ontology:cs_axiom_grounding('a5f8761b-3eb1-4366-b6f7-9882f4262a7a', software_must_be_free_as_in_freedom, deontological).
narrative_ontology:cs_axiom('a5f8761b-3eb1-4366-b6f7-9882f4262a7a', foundational, proprietary_software_is_unethical).
narrative_ontology:cs_axiom_status(proprietary_software_is_unethical, holdable).
narrative_ontology:cs_axiom_grounding('a5f8761b-3eb1-4366-b6f7-9882f4262a7a', proprietary_software_is_unethical, deontological).
narrative_ontology:cs_reference_frame('a5f8761b-3eb1-4366-b6f7-9882f4262a7a', user_sovereignty_over_code).
narrative_ontology:cs_drift_state('a5f8761b-3eb1-4366-b6f7-9882f4262a7a', contemporary_digital_economy, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('a5f8761b-3eb1-4366-b6f7-9882f4262a7a', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, users_as_rights_holders).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, free_software_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, users_of_proprietary_software).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, digital_rights_activists).
narrative_ontology:constraint_vindicates(software_control_legitimacy__freedom_imperative_reading, user_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(software_control_legitimacy__freedom_imperative_reading, digital_autonomy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote the ethical imperative of user freedom over software, developing and distributing free software alternatives. They define the terms of the debate and organize resistance against proprietary models.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, free_software_advocates, agenda_setter,
    organized, generational, mobile, global).

% Are seen as victims of proprietary software, denied fundamental control over their computing. While they may benefit from the functionality, the ethical framework of this reading asserts a fundamental loss of freedom. Their exit options are limited by network effects and compatibility issues.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, users_of_proprietary_software, payer,
    powerless, biographical, constrained, global).

% Are the primary target of this ethical critique. Their business model, based on restricting user control, is deemed illegitimate. They are excluded from the ethical conversation as framed by this reading, which rejects their foundational premises.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors, excluded,
    institutional, generational, arbitrage, global).

% Align with and amplify the freedom imperative, seeing it as a core component of broader digital rights. They benefit from the clear ethical framework this reading provides for their advocacy.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, digital_rights_activists, beneficiary,
    organized, generational, mobile, global).

% Operate within a legal framework that this reading fundamentally rejects as ethically illegitimate when applied to software. They are excluded from the ethical discourse of this reading, as their professional framework is seen as part of the problem.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, intellectual_property_lawyers, excluded,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__freedom_imperative_reading, free_software_advocates).
narrative_ontology:fixing_cost_class(software_control_legitimacy__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fosters a global community of users and developers who collectively build, share, and maintain software that respects user freedom, ensuring a common pool of freely usable and modifiable code.
% TRANSFER_FUNCTION: Aims to transfer control, modification rights, and distribution freedom over software from proprietary vendors to individual users and the broader community.
% ABSENT_VOICES: Proprietary software vendors and intellectual property rights advocates are absent from this ethical framing; they would argue for creators' rights to control their work and profit from it, but their arguments are deemed ethically invalid by this reading.
% DISAPPEARANCE_RATIONALE: If the imperative for user freedom over software vanished, the ethical landscape of computing would fundamentally shift. The moral basis for challenging proprietary software would disappear, potentially leading to even greater vendor control and less user autonomy, reorganizing the entire digital economy around a different set of values.
% FOUNDING_PROBLEM: Users lacked fundamental control over their computing, leading to vendor lock-in, surveillance, and limitations on innovation, which was seen as an ethical violation of user freedom.
% FOUNDING_PROBLEM_CORROBORATION: Free software foundations, academic ethicists, and independent security researchers consistently corroborate the ongoing problem of user disempowerment and the ethical necessity of software freedom, citing examples of restrictive licenses, DRM, and data exploitation.
narrative_ontology:disappearance_verdict(software_control_legitimacy__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__freedom_imperative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__freedom_imperative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(software_control_legitimacy__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__freedom_imperative_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the categorical ethical judgment that proprietary software extracts a fundamental freedom from users. Suppression (0.75) is high because proprietary software relies on legal frameworks (copyright, patents) and technical measures (DRM, obfuscation) to prevent users from exercising control. The low theater ratio (0.1) indicates that the ethical challenge posed by this reading is a genuine, active struggle, not a performative one. Accessibility collapse (0.8) is high because while free software alternatives exist, the default and dominant market position of proprietary software makes the 'freedom' alternative less accessible for many users. Resistance (0.7) is high due to the ongoing efforts of the free software movement.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between this reading's ethical condemnation of proprietary software and the views of those who uphold property rights or pragmatic approaches. This reading asserts a fundamental ethical violation, while other readings might see proprietary software as a legitimate economic model or a practical development choice. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Free software advocates and users as rights-holders are beneficiaries, as the constraint (the ethical imperative) aligns with their goals and frames them as rightful owners of control. Users of proprietary software are victims, as they are the ones from whom freedom is extracted. Proprietary software vendors are excluded from the ethical legitimacy of this framework, as their very existence is seen as a violation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately representing the ''freedom imperative'' reading of the ''software_control_legitimacy'' kernel?',
    'Comparison with canonical texts and statements from leading proponents of the free software movement (e.g., Richard Stallman, GNU Project documentation).',
    'If the representation is inaccurate, the classification of proprietary software as a ''snare'' from this perspective might be overstated or understated, affecting the overall analysis of the kernel''s contestation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Verifies the fidelity of this constraint to the specified kernel reading.').

omega_variable(
    ethical_vs_economic_framing,
    'To what extent is the ''extraction of freedom'' an ethical claim versus an economic one, and how does this affect the perceived extractiveness?',
    'Analysis of the arguments used by free software advocates: if the primary arguments are deontological (rights-based) rather than consequentialist (economic harm), the ethical framing dominates. If economic arguments are central, the extractiveness might be re-evaluated against market-based metrics.',
    'If primarily an ethical claim, the high extractiveness is robust within this reading. If it has a strong economic component, the extractiveness might be subject to empirical refutation by economic data, potentially shifting the classification towards a ''tangled_rope'' if some coordination function is acknowledged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethical_vs_economic_framing, conceptual, 'Distinguishes the ethical basis of extraction from potential economic interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__freedom_imperative_reading, 1985, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1985, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(soft_tr_t1995, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(soft_tr_t2005, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(soft_tr_t2015, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(soft_tr_t2024, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t1985, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 1985, 0.75).
narrative_ontology:measurement(soft_be_t1995, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 1995, 0.8).
narrative_ontology:measurement(soft_be_t2005, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2005, 0.83).
narrative_ontology:measurement(soft_be_t2015, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2015, 0.84).
narrative_ontology:measurement(soft_be_t2024, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1985, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement(soft_su_t1995, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(soft_su_t2005, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2005, 0.73).
narrative_ontology:measurement(soft_su_t2015, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2015, 0.74).
narrative_ontology:measurement(soft_su_t2024, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__freedom_imperative_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'software_control_legitimacy' kernel, each representing a distinct perspective on the ethical and practical implications of software control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
