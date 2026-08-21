% ============================================================================
% CONSTRAINT STORY: us_constitution_text__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__originalist_reading, []).

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
 *   constraint_id: us_constitution_text__originalist_reading
 *   human_readable: Originalist Reading of US Constitutional Meaning
 *   domain: legal/political/interpretive_theory
 *
 * SUMMARY:
 *   This constraint represents the originalist reading of the US
 *   Constitution, which posits that constitutional meaning is fixed at the
 *   time of ratification and must be recovered through historical evidence of
 *   original public understanding. This is one reading of the
 *   'us_constitution_text' kernel. The constraint is rigid, suppressing
 *   adaptive interpretation, and benefits the conservative legal movement by
 *   providing a framework for institutional dominance, while victimizing
 *   rights claims not grounded in historical practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__originalist_reading, 0.75).
domain_priors:suppression_score(us_constitution_text__originalist_reading, 0.85).
domain_priors:theater_ratio(us_constitution_text__originalist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__originalist_reading, snare).
narrative_ontology:human_readable(us_constitution_text__originalist_reading, "Originalist Reading of US Constitutional Meaning").
narrative_ontology:topic_domain(us_constitution_text__originalist_reading, "legal/political/interpretive_theory").

domain_priors:requires_active_enforcement(us_constitution_text__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__originalist_reading, '9d95d72e-67da-46d6-aaa5-2d40b9377d80').
narrative_ontology:cs_kernel_codification('9d95d72e-67da-46d6-aaa5-2d40b9377d80', fixed_text).
narrative_ontology:cs_authority_grounding('9d95d72e-67da-46d6-aaa5-2d40b9377d80', extraction).
narrative_ontology:cs_interpretation_layer_present('9d95d72e-67da-46d6-aaa5-2d40b9377d80').
narrative_ontology:cs_reading_relation('9d95d72e-67da-46d6-aaa5-2d40b9377d80', us_constitution_text__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('9d95d72e-67da-46d6-aaa5-2d40b9377d80', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('9d95d72e-67da-46d6-aaa5-2d40b9377d80', foundational, original_public_meaning_is_fixed).
narrative_ontology:cs_axiom_status(original_public_meaning_is_fixed, holdable).
narrative_ontology:cs_axiom_grounding('9d95d72e-67da-46d6-aaa5-2d40b9377d80', original_public_meaning_is_fixed, conventional).
narrative_ontology:cs_axiom('9d95d72e-67da-46d6-aaa5-2d40b9377d80', foundational, judicial_role_is_to_discover_not_create_law).
narrative_ontology:cs_axiom_status(judicial_role_is_to_discover_not_create_law, holdable).
narrative_ontology:cs_axiom_grounding('9d95d72e-67da-46d6-aaa5-2d40b9377d80', judicial_role_is_to_discover_not_create_law, deontological).
narrative_ontology:cs_reference_frame('9d95d72e-67da-46d6-aaa5-2d40b9377d80', founding_era_public_understanding).
narrative_ontology:cs_drift_state('9d95d72e-67da-46d6-aaa5-2d40b9377d80', contemporary_legal_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9d95d72e-67da-46d6-aaa5-2d40b9377d80', '').
narrative_ontology:cs_kernel_id(us_constitution_text__originalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, originalist_judges).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, rights_claimants_not_historically_grounded).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, adaptive_constitutional_interpretation_advocates).
narrative_ontology:constraint_vindicates(us_constitution_text__originalist_reading, rule_of_law_fidelity).
narrative_ontology:constraint_vindicates(us_constitution_text__originalist_reading, judicial_restraint).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promotes, funds, and enforces originalist methodology in legal education, judicial appointments, and public discourse. Benefits from the institutional dominance and policy outcomes achieved through originalist interpretations.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, conservative_legal_movement, agenda_setter,
    institutional, generational, mobile, national).

% Apply originalist methodology in their rulings, shaping legal outcomes and reinforcing the theory's authority. Their professional identity and career trajectory are often tied to their adherence to originalism.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, originalist_judges, agenda_setter,
    institutional, biographical, identity_locked, national).

% Seek to assert rights based on evolving societal norms, scientific understanding, or principles not explicitly recognized or protected at the time of ratification. Their claims are often rejected or severely limited by originalist interpretations, leaving them with few legal avenues.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, rights_claimants_not_historically_grounded, payer,
    powerless, immediate, trapped, national).

% Argue for a dynamic understanding of the Constitution, adapting its principles to contemporary circumstances. They face significant institutional resistance, suppression of their interpretive methods, and often see their legal arguments dismissed.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, adaptive_constitutional_interpretation_advocates, payer,
    organized, generational, constrained, national).

% Judges who would apply a living constitutionalist methodology but are increasingly marginalized, outvoted, or prevented from being appointed to courts dominated by originalist ideology. Their interpretive approach is actively suppressed within the dominant legal framework.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, living_constitutionalist_judges, excluded,
    institutional, biographical, constrained, national).

% Provide scholarly analysis of historical legal texts, original public meaning, and the context of ratification. They often inform or critique originalist claims, but their findings may be selectively used or dismissed by proponents of originalism.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, legal_historians, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:fixing_cost_class(us_constitution_text__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable framework for constitutional interpretation by grounding meaning in a fixed historical understanding, aiming to limit judicial discretion and ensure fidelity to the framers' intent.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary societal values and evolving principles to historical texts and 18th/19th-century public understanding. This benefits those whose interests align with historical norms and extracts from those seeking adaptive interpretations or new rights.
% ABSENT_VOICES: Future generations and marginalized groups whose rights and interests were not considered or protected at the time of ratification; their perspectives are structurally excluded by the interpretive methodology itself, which prioritizes historical intent over contemporary justice.
% DISAPPEARANCE_RATIONALE: If originalism vanished overnight, constitutional interpretation would immediately shift towards more adaptive or contemporary understandings, leading to significant changes in legal outcomes, rights recognition, and the balance of power between branches of government. The legal landscape would be fundamentally reshaped, likely expanding rights and re-evaluating precedents.
% FOUNDING_PROBLEM: To prevent judicial activism and ensure fidelity to the written Constitution by establishing a neutral, objective method of interpretation that limits judges to the original meaning intended by the framers and ratifiers, thereby preserving democratic self-governance.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within the conservative legal movement attest that judicial activism remains a live problem requiring originalist discipline. Critics (legal scholars, civil rights advocates, living constitutionalist judges) argue that the problem of judicial overreach is often a pretext for imposing specific political outcomes, and that the founding problem of adapting a centuries-old document to modern society is ignored or exacerbated by originalism. Legislative hearing testimony and independent legal analysis support the shifted-function reading.
narrative_ontology:disappearance_verdict(us_constitution_text__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(us_constitution_text__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__originalist_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.75) because the methodology often leads to outcomes that benefit specific political agendas by limiting the scope of rights or governmental power based on historical context, rather than contemporary needs. Suppression is very high (0.85) due to the active institutional and intellectual efforts to marginalize and delegitimize alternative interpretive methods. Theater ratio is low (0.15) as originalism is a serious, albeit contested, legal theory with genuine scholarly engagement, not primarily performative. Accessibility collapse is high (0.80) as it severely limits the range of permissible interpretations. Resistance is also high (0.70) due to significant opposition from other legal theories and affected groups.
 *
 * PERSPECTIVAL GAP:
 *   The conservative legal movement and originalist judges experience this as a legitimate, principled approach to judicial restraint and constitutional fidelity. In contrast, rights claimants and advocates for adaptive interpretation experience it as an extractive and suppressive force that denies evolving understandings of justice and human dignity. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The conservative legal movement and originalist judges are clear beneficiaries (d near 0.0) as they gain institutional power and achieve policy outcomes aligned with their ideology. Rights claimants and advocates for adaptive interpretation are targets (d near 1.0) as their claims are suppressed and their interpretive methods delegitimized. Legal historians act as observers, providing data that may be used by either side.
 *
 * MANDATROPHY ANALYSIS:
 *   The originalist reading claims to solve the problem of judicial activism. However, critics argue that it has become a mechanism for institutional capture, where the 'founding problem' is invoked to justify outcomes that benefit specific political factions. The high extractiveness and suppression, coupled with the contested status of the founding problem, suggest a potential for mandatrophy where the original mandate (judicial restraint) is superseded by a function of power consolidation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recoverability_of_original_meaning,
    'Is ''original public meaning'' genuinely recoverable through historical methods, or is it inherently subjective, anachronistic, or incomplete?',
    'Further advancements in historical methodology, or a consensus among legal historians on the inherent limits of recovering past public understanding.',
    'If original meaning is found to be largely irrecoverable or highly subjective, the epistemic grounding of originalism would collapse, forcing a re-evaluation of its legitimacy and potentially reclassifying it as a more purely conventional or extractive constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recoverability_of_original_meaning, empirical, 'Ambiguity regarding the objective recoverability of original public meaning.').

omega_variable(
    judicial_restraint_vs_political_outcome,
    'Is the suppression of adaptive interpretation a necessary cost of judicial restraint, or a mechanism for imposing specific political outcomes under the guise of neutrality?',
    'Longitudinal analysis of originalist rulings across diverse issue areas, comparing outcomes to stated principles of judicial restraint and to the political preferences of the judges/movements promoting originalism.',
    'If originalism consistently correlates with specific political outcomes rather than neutral restraint, its classification would shift more definitively towards a Snare, highlighting its extractive function over its claimed coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_restraint_vs_political_outcome, conceptual, 'Ambiguity regarding the true function of originalism: principled restraint or political tool.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__originalist_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1970, us_constitution_text__originalist_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(us_c_tr_t1985, us_constitution_text__originalist_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_text__originalist_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_text__originalist_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(us_c_tr_t2020, us_constitution_text__originalist_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_text__originalist_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1970, us_constitution_text__originalist_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(us_c_be_t1985, us_constitution_text__originalist_reading, base_extractiveness, 1985, 0.6).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_text__originalist_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_text__originalist_reading, base_extractiveness, 2010, 0.72).
narrative_ontology:measurement(us_c_be_t2020, us_constitution_text__originalist_reading, base_extractiveness, 2020, 0.74).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_text__originalist_reading, base_extractiveness, 2025, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1970, us_constitution_text__originalist_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(us_c_su_t1985, us_constitution_text__originalist_reading, suppression_requirement, 1985, 0.7).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_text__originalist_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_text__originalist_reading, suppression_requirement, 2010, 0.82).
narrative_ontology:measurement(us_c_su_t2020, us_constitution_text__originalist_reading, suppression_requirement, 2020, 0.84).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_text__originalist_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'us_constitution_text' kernel. Each reading represents a distinct interpretive framework with different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
