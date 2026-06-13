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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_constitution_text__originalist_reading
 *   human_readable: Originalist Interpretation of US Constitution
 *   domain: constitutional_law/legal_philosophy
 *
 * SUMMARY:
 *   This constraint describes the originalist reading of the US Constitution,
 *   which posits that constitutional meaning is fixed at the time of
 *   ratification and judicial interpretation must recover that original
 *   public understanding. This is one reading of the 'us_constitution_text'
 *   kernel. It functions as a Tangled Rope: it offers a coordination function
 *   (stable, predictable interpretation) but also enables significant
 *   extraction by suppressing adaptive interpretations and benefiting
 *   specific political and legal movements. The constraint's rigidity means
 *   judges are bound by historical evidence, and post-ratification practice
 *   is largely irrelevant unless it sheds light on original meaning. This
 *   leads to high suppression of adaptive interpretation and benefits the
 *   conservative legal movement, while victimizing rights claims not
 *   explicitly grounded in 18th/19th-century practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__originalist_reading, 0.65).
domain_priors:suppression_score(us_constitution_text__originalist_reading, 0.75).
domain_priors:theater_ratio(us_constitution_text__originalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__originalist_reading, "Originalist Interpretation of US Constitution").
narrative_ontology:topic_domain(us_constitution_text__originalist_reading, "constitutional_law/legal_philosophy").

domain_priors:requires_active_enforcement(us_constitution_text__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__originalist_reading, '3b68b836-263c-417b-9ea6-0dacd3063b0f').
narrative_ontology:cs_kernel_codification('3b68b836-263c-417b-9ea6-0dacd3063b0f', fixed_text).
narrative_ontology:cs_authority_grounding('3b68b836-263c-417b-9ea6-0dacd3063b0f', lineage).
narrative_ontology:cs_interpretation_layer_present('3b68b836-263c-417b-9ea6-0dacd3063b0f').
narrative_ontology:cs_reading_relation('3b68b836-263c-417b-9ea6-0dacd3063b0f', us_constitution_text__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3b68b836-263c-417b-9ea6-0dacd3063b0f', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('3b68b836-263c-417b-9ea6-0dacd3063b0f', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('3b68b836-263c-417b-9ea6-0dacd3063b0f', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('3b68b836-263c-417b-9ea6-0dacd3063b0f', foundational, judicial_role_limited_to_original_intent).
narrative_ontology:cs_axiom_status(judicial_role_limited_to_original_intent, holdable).
narrative_ontology:cs_axiom_grounding('3b68b836-263c-417b-9ea6-0dacd3063b0f', judicial_role_limited_to_original_intent, deontological).
narrative_ontology:cs_reference_frame('3b68b836-263c-417b-9ea6-0dacd3063b0f', founding_era_public_understanding).
narrative_ontology:cs_drift_state('3b68b836-263c-417b-9ea6-0dacd3063b0f', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3b68b836-263c-417b-9ea6-0dacd3063b0f', '').
narrative_ontology:cs_kernel_id(us_constitution_text__originalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, judicial_conservatives).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, rights_claimants_not_historically_grounded).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, progressive_legal_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, general_public).
narrative_ontology:constraint_vindicates(us_constitution_text__originalist_reading, rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_text__originalist_reading, judicial_restraint_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promotes and enforces originalist methodology in judicial appointments, legal education, and court decisions. Benefits from the stability and predictability this reading offers, which often aligns with conservative policy goals by limiting judicial innovation.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, conservative_legal_movement, agenda_setter,
    institutional, generational, arbitrage, national).

% Judges and justices who adhere to originalist principles, gaining legitimacy and influence within the conservative legal framework. Their careers and reputations are often built on consistent application of this interpretive method.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, judicial_conservatives, beneficiary,
    institutional, biographical, constrained, national).

% Individuals or groups seeking to assert rights (e.g., privacy, environmental protection, evolving equality claims) that are not explicitly enumerated or clearly understood at the time of the Constitution's ratification. Their claims are often suppressed or rejected under this interpretive framework.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, rights_claimants_not_historically_grounded, payer,
    powerless, immediate, trapped, national).

% Academics and legal practitioners who advocate for adaptive or evolving constitutional meaning. They bear the cost of having their interpretive methods marginalized or dismissed in dominant legal discourse and judicial practice.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, progressive_legal_scholars, payer,
    organized, generational, constrained, national).

% Benefits from the perceived stability and democratic legitimacy of a fixed constitutional text, which is presented as preventing judicial overreach. However, they may also bear the costs of a legal system that is slow to adapt to modern social realities.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, general_public, beneficiary,
    organized, generational, constrained, national).

% Advocates for an evolving constitutional meaning, whose arguments are often systematically excluded from the interpretive framework of originalism. While they participate in broader legal discourse, their specific interpretive approach is deemed illegitimate by originalists.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, living_constitutionalist_scholars, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, historically grounded method for interpreting the US Constitution, aiming to coordinate judicial decision-making around a fixed meaning and limit subjective judicial discretion.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary societal values and evolving moral understandings to historical evidence of original public meaning, effectively transferring power to those who control the historical narrative and its application.
% ABSENT_VOICES: The voices of those advocating for rights not recognized at the time of ratification, or for an evolving understanding of constitutional principles, are systematically marginalized or excluded from the interpretive process. Their arguments are often deemed irrelevant by originalist methodology.
% DISAPPEARANCE_RATIONALE: If originalism as a dominant interpretive method vanished, the legal landscape would immediately shift. Judicial decisions would likely incorporate contemporary values more readily, leading to different outcomes in areas like privacy, equality, and administrative law. The composition and priorities of the Supreme Court would also likely change, as would the focus of legal education and advocacy.
% FOUNDING_PROBLEM: The problem of judicial subjectivity and the potential for judges to impose their own policy preferences under the guise of constitutional interpretation, leading to an unstable and undemocratic legal system.
% FOUNDING_PROBLEM_CORROBORATION: Conservative legal scholars and politicians consistently attest that judicial activism remains a live problem, necessitating originalist constraints. Critics, including progressive legal scholars and some political scientists, acknowledge the problem of judicial subjectivity but argue that originalism itself introduces new forms of bias and rigidity, making the 'solution' part of a contested problem.
narrative_ontology:disappearance_verdict(us_constitution_text__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__originalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__originalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(us_constitution_text__originalist_reading, 'none', 1).

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
 *   The extractiveness (0.65) is substantial because this interpretive method often leads to outcomes that benefit specific political agendas by limiting the scope of rights or governmental power in ways that align with conservative ideology. Suppression (0.75) is high due to the active exclusion of alternative interpretive methodologies from judicial discourse and appointments. The theater ratio (0.20) is relatively low, as the commitment to historical inquiry is genuine, though it can sometimes be performative when historical evidence is ambiguous or selectively applied. Accessibility collapse (0.60) is moderate, as alternative interpretive methods exist but are systematically de-legitimized within the originalist framework. Resistance (0.70) is high, reflecting ongoing academic and political challenges to originalism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of originalist proponents, this is a legitimate and necessary method for upholding the rule of law and democratic principles (a Rope or even a Mountain of legal truth). From the perspective of those whose rights claims are denied or whose interpretive methods are suppressed, it functions as a Snare or Tangled Rope, leveraging historical authority to maintain a specific power structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The conservative legal movement and judicial conservatives are primary beneficiaries (d near 0.0) as originalism provides a powerful tool for achieving their policy and judicial goals, and their careers are often advanced by its adoption. Rights claimants not historically grounded and progressive legal scholars are primary targets (d near 1.0) as their claims and methods are systematically disadvantaged or rejected. The general public is a diffuse beneficiary/payer, gaining perceived stability but potentially losing adaptive governance.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing judicial overreach) is still 'live' but its application is 'contested'. The persistence of originalism is not solely due to its coordination function but also its utility as an extractive mechanism for specific political and legal movements. The rising extractiveness and suppression over time suggest that while the original founding problem (judicial subjectivity) may still exist, the constraint has accumulated additional extractive layers, preventing it from being a pure Rope. The 'contested' status of the founding problem indicates that the constraint's continued operation is not universally accepted as serving its original purpose, but rather as serving the interests of its beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_determinacy,
    'To what extent is ''original public meaning'' a determinate, discoverable fact, rather than a construct influenced by contemporary interpretive biases?',
    'Empirical studies of historical linguistic usage and public understanding at the time of ratification, coupled with meta-analysis of originalist scholarship for consistency and bias.',
    'If original meaning is largely indeterminate, the constraint''s claim to objectivity is weakened, increasing its theater ratio and extractiveness by revealing the interpretive choices involved. If highly determinate, its Mountain-like claims are strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_determinacy, empirical, 'The determinacy of original public meaning.').

omega_variable(
    judicial_activism_vs_originalism,
    'Does originalism genuinely prevent judicial activism, or does it merely shift the form of activism from policy-making to selective historical interpretation?',
    'Comparative analysis of judicial outcomes under originalist vs. non-originalist regimes, focusing on consistency with historical evidence versus alignment with contemporary political agendas.',
    'If originalism is found to merely shift the form of activism, its coordination function (preventing overreach) is undermined, increasing its theater ratio and potentially reclassifying it closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_activism_vs_originalism, conceptual, 'Originalism''s effectiveness in preventing judicial activism.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is the legitimacy of originalism derived from its historical fidelity (a Mountain-like claim) or from its utility in achieving specific political outcomes (a Snare-like function)?',
    'Analysis of the discourse surrounding originalism: when is historical accuracy emphasized, and when are policy outcomes prioritized? Examine funding sources and political alignments of originalist advocacy groups.',
    'If its utility for political outcomes is the primary driver, its claimed type as a ''Rope'' (coordination) is undermined, and its classification shifts towards ''Snare'' (pure extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, preference, 'Source of originalism''s legitimacy: historical fidelity vs. political utility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__originalist_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1970, us_constitution_text__originalist_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(us_c_tr_t1985, us_constitution_text__originalist_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_text__originalist_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_text__originalist_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_text__originalist_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1970, us_constitution_text__originalist_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(us_c_be_t1985, us_constitution_text__originalist_reading, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_text__originalist_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_text__originalist_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_text__originalist_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1970, us_constitution_text__originalist_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(us_c_su_t1985, us_constitution_text__originalist_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_text__originalist_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_text__originalist_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_text__originalist_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'us_constitution_text' kernel. This file describes the originalist reading; 'living_constitutionalist_reading' and 'positivist_reading' are sibling constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
