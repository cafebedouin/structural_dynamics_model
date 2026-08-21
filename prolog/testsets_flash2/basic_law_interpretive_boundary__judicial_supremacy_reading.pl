% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Basic Laws Interpretive Boundary
 *   domain: constitutional_law/comparative_constitutionalism
 *
 * SUMMARY:
 *   This constraint represents the 'judicial supremacy' reading of the Basic
 *   Laws' interpretive boundary, where the Supreme Court holds ultimate
 *   authority to interpret and enforce Basic Laws, including the power to
 *   invalidate contradictory legislation. This reading emerged and
 *   strengthened following the 'Constitutional Revolution' of the 1990s. It
 *   is one of three competing readings of the kernel
 *   'basic_law_interpretive_boundary'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.78).
domain_priors:suppression_score(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.65).
domain_priors:theater_ratio(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__judicial_supremacy_reading, "Judicial Supremacy Reading of Basic Laws Interpretive Boundary").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__judicial_supremacy_reading, "constitutional_law/comparative_constitutionalism").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__judicial_supremacy_reading, '7aa14d7d-8557-4377-8ae7-e5463d5d0f66').
narrative_ontology:cs_kernel_codification('7aa14d7d-8557-4377-8ae7-e5463d5d0f66', formalized).
narrative_ontology:cs_authority_grounding('7aa14d7d-8557-4377-8ae7-e5463d5d0f66', lineage).
narrative_ontology:cs_interpretation_layer_present('7aa14d7d-8557-4377-8ae7-e5463d5d0f66').
narrative_ontology:cs_reading_relation('7aa14d7d-8557-4377-8ae7-e5463d5d0f66', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('7aa14d7d-8557-4377-8ae7-e5463d5d0f66', basic_law_interpretive_boundary__balanced_contestation_reading, coexists_with).
narrative_ontology:cs_axiom('7aa14d7d-8557-4377-8ae7-e5463d5d0f66', foundational, judicial_review_as_constitutional_imperative).
narrative_ontology:cs_axiom_status(judicial_review_as_constitutional_imperative, holdable).
narrative_ontology:cs_axiom_grounding('7aa14d7d-8557-4377-8ae7-e5463d5d0f66', judicial_review_as_constitutional_imperative, deontological).
narrative_ontology:cs_axiom('7aa14d7d-8557-4377-8ae7-e5463d5d0f66', foundational, basic_laws_as_supreme_law).
narrative_ontology:cs_axiom_status(basic_laws_as_supreme_law, holdable).
narrative_ontology:cs_axiom_grounding('7aa14d7d-8557-4377-8ae7-e5463d5d0f66', basic_laws_as_supreme_law, conventional).
narrative_ontology:cs_reference_frame('7aa14d7d-8557-4377-8ae7-e5463d5d0f66', constitutional_revolution_doctrine).
narrative_ontology:cs_drift_state('7aa14d7d-8557-4377-8ae7-e5463d5d0f66', contemporary_legislative_challenges, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('7aa14d7d-8557-4377-8ae7-e5463d5d0f66', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, legislative_majority).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__judicial_supremacy_reading, judicial_review_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__judicial_supremacy_reading, constitutional_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Basic Laws as a higher-order framework, invalidates contradictory legislation, and enforces its rulings as binding on the Knesset. Benefits from expanded institutional power and legitimacy as the ultimate arbiter of constitutional meaning.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% Its legislative output is subject to judicial review and potential invalidation. Bears the cost of having its policy choices overturned and its legislative authority constrained by the Court's interpretation of Basic Laws.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset, payer,
    institutional, biographical, constrained, national).

% Gain a powerful mechanism to challenge legislation that infringes on their perceived rights under the Basic Laws, effectively giving them a veto via litigation. Benefits from enhanced protection of individual liberties.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants, beneficiary,
    moderate, immediate, mobile, national).

% Represents the will of the electorate but finds its policy agenda subject to judicial override. Bears the political cost of having its mandate frustrated by judicial decisions, leading to potential legislative gridlock or public frustration.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, legislative_majority, payer,
    powerful, biographical, constrained, national).

% Argue for the ultimate authority of the elected legislature and against judicial supremacy. Their arguments are structurally marginalized within this reading, which posits the Court as the final arbiter.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, parliamentary_sovereignty_advocates, excluded,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear hierarchy of legal norms, providing a stable framework for rights protection and limiting legislative overreach, thereby coordinating expectations about the boundaries of state power.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority over Basic Laws from the Knesset to the Supreme Court, and transfers legislative power from the elected majority to the judiciary in cases of constitutional conflict.
% ABSENT_VOICES: Advocates for parliamentary sovereignty and those who believe the Basic Laws should be interpreted by the elected legislature are excluded from the interpretive process, their arguments for legislative supremacy overridden by judicial authority.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the Supreme Court would lose its power to invalidate legislation, the Knesset would regain full legislative supremacy, and rights protection would depend solely on legislative will. The entire constitutional order would fundamentally shift.
% FOUNDING_PROBLEM: The absence of a formal constitution and the need to protect fundamental rights from transient legislative majorities, alongside the desire to establish a stable legal hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and civil society organizations attest to the ongoing need for rights protection and a stable legal framework, corroborating the problem's live status from outside the immediate beneficiaries of judicial power.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(basic_law_interpretive_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because this reading significantly constrains the legislative power of the Knesset, transferring substantial authority to the unelected judiciary. Suppression is moderate-high as the Court's rulings actively suppress legislative alternatives. Resistance is high due to ongoing political contestation from legislative majorities and parliamentary sovereignty advocates. The claimed type is 'tangled_rope' because it provides a coordination function (stable legal hierarchy, rights protection) but also involves asymmetric extraction of power from the legislature by the judiciary.
 *
 * PERSPECTIVAL GAP:
 *   From the Supreme Court's perspective, this is a necessary 'rope' for constitutional order and rights protection. From the Knesset's perspective, it is a 'snare' that usurps democratic authority. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court and rights claimants are beneficiaries, gaining power and protection. The Knesset and legislative majority are victims, losing legislative autonomy. The directionality for the Court is low (beneficiary), while for the Knesset it is high (target).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_activism_vs_restraint,
    'Is the Supreme Court''s interpretation of Basic Laws an act of legitimate constitutional enforcement or judicial overreach?',
    'Analysis of judicial decisions against original legislative intent, comparative constitutional law, and public opinion on judicial legitimacy.',
    'If deemed overreach, the legitimacy of this reading would erode, potentially leading to legislative attempts to curb judicial power. If legitimate, it reinforces the Court''s authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_activism_vs_restraint, conceptual, 'Ambiguity regarding the nature of judicial review in this context.').

omega_variable(
    basic_laws_constitutional_status,
    'Do the Basic Laws truly constitute a formal, entrenched constitution, or are they still quasi-constitutional statutes?',
    'A formal constitutional entrenchment process, or a clear and consistent judicial doctrine that treats them as fully entrenched.',
    'If fully entrenched, this reading''s foundation is strengthened. If not, its claim to higher-order authority is weakened, making it more vulnerable to legislative challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(basic_laws_constitutional_status, empirical, 'The formal constitutional status of the Basic Laws.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__judicial_supremacy_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(basi_be_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 1992, 0.6).
narrative_ontology:measurement(basi_be_t2000, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(basi_be_t2008, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2008, 0.72).
narrative_ontology:measurement(basi_be_t2016, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2016, 0.75).
narrative_ontology:measurement(basi_be_t2024, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1992, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 1992, 0.5).
narrative_ontology:measurement(basi_su_t2000, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(basi_su_t2008, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2008, 0.6).
narrative_ontology:measurement(basi_su_t2016, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2016, 0.63).
narrative_ontology:measurement(basi_su_t2024, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'basic_law_interpretive_boundary' kernel. Its structural properties differ significantly from sibling readings, particularly in the distribution of interpretive authority and the scope of judicial review.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
