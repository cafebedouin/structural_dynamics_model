% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__synchronic_diachronic_seam
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__synchronic_diachronic_seam, []).

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
 *   constraint_id: ip_category_emergence__synchronic_diachronic_seam
 *   human_readable: IP Category Emergence: Synchronic/Diachronic Seam Test
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This constraint represents an analytical framework within legal
 *   philosophy and intellectual property history. It proposes a test to
 *   determine whether the 'thinkability' (conceptual emergence) of IP
 *   categories and 'first holding' (initial legal recognition of rights) are
 *   formally independent historical processes or merely different temporal
 *   framings of the same underlying phenomenon. It aims to clarify a
 *   fundamental conceptual seam in the kernel of IP category emergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, 0.15).
domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, 0.1).
domain_priors:theater_ratio(ip_category_emergence__synchronic_diachronic_seam, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, extractiveness, 0.15).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__synchronic_diachronic_seam, rope).
narrative_ontology:human_readable(ip_category_emergence__synchronic_diachronic_seam, "IP Category Emergence: Synchronic/Diachronic Seam Test").
narrative_ontology:topic_domain(ip_category_emergence__synchronic_diachronic_seam, "legal_philosophy/intellectual_property/historical_jurisprudence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__synchronic_diachronic_seam, '5df2294e-e282-44d3-a2c6-dfd2b5797790').
narrative_ontology:cs_kernel_codification('5df2294e-e282-44d3-a2c6-dfd2b5797790', formalized).
narrative_ontology:cs_authority_grounding('5df2294e-e282-44d3-a2c6-dfd2b5797790', expertise).
narrative_ontology:cs_interpretation_layer_present('5df2294e-e282-44d3-a2c6-dfd2b5797790').
narrative_ontology:cs_reading_relation('5df2294e-e282-44d3-a2c6-dfd2b5797790', ip_category_emergence__thinkability_reading, influences).
narrative_ontology:cs_reading_relation('5df2294e-e282-44d3-a2c6-dfd2b5797790', ip_category_emergence__first_holding_reading, influences).
narrative_ontology:cs_axiom('5df2294e-e282-44d3-a2c6-dfd2b5797790', foundational, conceptual_distinction_testable).
narrative_ontology:cs_axiom_status(conceptual_distinction_testable, holdable).
narrative_ontology:cs_axiom_grounding('5df2294e-e282-44d3-a2c6-dfd2b5797790', conceptual_distinction_testable, empirically_contingent).
narrative_ontology:cs_reference_frame('5df2294e-e282-44d3-a2c6-dfd2b5797790', analytical_independence_hypothesis).
narrative_ontology:cs_drift_state('5df2294e-e282-44d3-a2c6-dfd2b5797790', contemporary_scholarly_debate, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5df2294e-e282-44d3-a2c6-dfd2b5797790', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, ip_theorists).
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, legal_historians).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, thinkability_advocates).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, first_holding_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars who conduct the historical and conceptual analysis to determine if the 'thinkability' and 'first holding' of IP categories are independent or co-occurring phenomena. They set the terms of the test.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, legal_historians, agenda_setter,
    institutional, generational, analytical, global).

% Benefit from the conceptual clarity provided by this analytical framework, which helps refine their understanding of IP's historical development and philosophical underpinnings. They integrate the findings into their theories.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, ip_theorists, beneficiary,
    powerful, biographical, mobile, global).

% Scholars whose work emphasizes IP as primarily marking the emergence of 'ownable expression' as a legally coherent category. They bear the cost of potential refutation or refinement of their position if the test shows strong co-occurrence with 'first holding'.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, thinkability_advocates, payer,
    moderate, biographical, constrained, global).

% Scholars whose work emphasizes IP as primarily marking 'occupancy change' by an author-as-rights-holder. They bear the cost of potential refutation or refinement of their position if the test shows strong independence from 'thinkability'.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, first_holding_advocates, payer,
    moderate, biographical, constrained, global).

% Legal professionals focused on the application of current IP law. The abstract, meta-historical nature of this conceptual test is largely outside their immediate professional concerns, though its long-term implications might eventually filter down.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, ip_practitioners, excluded,
    moderate, immediate, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the analytical framework and conceptual vocabulary for scholars to rigorously test the relationship between the historical emergence of IP categories ('thinkability') and the first legal recognition of rights ('first holding').
% TRANSFER_FUNCTION: Transfers conceptual clarity and a shared methodology for historical-philosophical inquiry among legal historians and IP theorists, potentially shifting the intellectual burden of proof for certain historical claims.
% ABSENT_VOICES: IP practitioners, who are focused on the application of current law rather than the meta-historical analysis of its foundational concepts. Their perspective, if present, might emphasize the practical irrelevance of such distinctions.
% DISAPPEARANCE_RATIONALE: If this specific analytical framework vanished, the underlying historical facts and philosophical questions about IP's origins would persist. Scholars would continue to debate the relationship between 'thinkability' and 'first holding,' but without this particular structured test to guide their inquiry.
% FOUNDING_PROBLEM: To resolve the ambiguity in historical jurisprudence regarding whether IP's conceptual 'thinkability' and its 'first holding' are distinct historical events or merely different perspectives on the same event, thereby clarifying the kernel structure of IP category emergence.
% FOUNDING_PROBLEM_CORROBORATION: Independent philosophical analysis and historical research from outside the immediate IP theory community attest to the ongoing nature and significance of this conceptual problem for understanding legal history and the philosophy of property.
narrative_ontology:disappearance_verdict(ip_category_emergence__synchronic_diachronic_seam, world_unchanged).
narrative_ontology:founding_problem_status(ip_category_emergence__synchronic_diachronic_seam, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__synchronic_diachronic_seam, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ip_category_emergence__synchronic_diachronic_seam, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__synchronic_diachronic_seam, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__synchronic_diachronic_seam_tests).
:- end_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because its primary function is to coordinate understanding and provide a shared analytical tool for scholars. Extraction is low as it's an intellectual framework, not directly extracting material resources. Suppression is low, as it doesn't coerce participation but rather offers a method of inquiry. Theater ratio is minimal, reflecting a genuine scholarly endeavor. Accessibility collapse is moderate, as the test's findings could make certain interpretations less viable, but not impossible. Resistance is moderate, as scholars whose existing theories might be challenged by the test's outcome could resist its methodology or findings.
 *
 * PERSPECTIVAL GAP:
 *   While the analytical framework itself aims for objectivity, scholars may have differing views on the validity of the test's methodology or the interpretation of its results. Those whose existing theories are challenged by the test's outcome would experience it as more 'extractive' of their intellectual capital, while those seeking clarity would experience it as purely coordinative.
 *
 * DIRECTIONALITY LOGIC:
 *   Legal historians and IP theorists are beneficiaries as they gain a clearer framework for their research. Advocates of either the 'thinkability' or 'first holding' as primary explanations are 'payers' in the sense that their specific interpretations might be challenged or refined by the test's outcome. IP practitioners are excluded as the debate is too abstract for their daily work.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint addresses a live conceptual problem in legal philosophy and historical jurisprudence. Its mandate is to provide clarity on a foundational question, which remains highly relevant. Therefore, it is not subject to mandatrophy; its function is ongoing and directly tied to an unresolved scholarly debate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''synchronic_diachronic_seam'' reading of the ''ip_category_emergence'' kernel?',
    'Consensus among legal philosophers and historians on the distinct analytical contribution of this framework compared to readings focused solely on ''thinkability'' or ''first holding''.',
    'If misidentified, the analysis of IP''s foundational kernel would be incomplete or misdirected, leading to inaccurate classifications of related constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading within the IP category emergence kernel.').

omega_variable(
    test_outcome_validity,
    'Does the ''synchronic_diachronic_seam'' test definitively establish the formal independence or co-occurrence of ''thinkability'' and ''first holding''?',
    'Further historical and philosophical research, including the discovery of new evidence or the development of more robust analytical tools, to corroborate or refute the test''s initial findings.',
    'If the test''s outcome is inconclusive or contested, the conceptual clarity it aims to provide remains elusive, potentially leading to continued fragmentation in IP theory. If conclusive, it would significantly influence the ''thinkability_reading'' and ''first_holding_reading''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(test_outcome_validity, empirical, 'Uncertainty regarding the definitive resolution provided by the analytical test.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__synchronic_diachronic_seam, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t0, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ip_c_tr_t4, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 4, 0.05).
narrative_ontology:measurement(ip_c_tr_t8, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 8, 0.05).
narrative_ontology:measurement(ip_c_tr_t12, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 12, 0.05).
narrative_ontology:measurement(ip_c_tr_t16, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 16, 0.05).
narrative_ontology:measurement(ip_c_tr_t20, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t0, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ip_c_be_t4, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 4, 0.15).
narrative_ontology:measurement(ip_c_be_t8, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 8, 0.15).
narrative_ontology:measurement(ip_c_be_t12, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 12, 0.15).
narrative_ontology:measurement(ip_c_be_t16, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 16, 0.15).
narrative_ontology:measurement(ip_c_be_t20, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t0, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(ip_c_su_t4, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 4, 0.1).
narrative_ontology:measurement(ip_c_su_t8, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 8, 0.1).
narrative_ontology:measurement(ip_c_su_t12, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 12, 0.1).
narrative_ontology:measurement(ip_c_su_t16, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 16, 0.1).
narrative_ontology:measurement(ip_c_su_t20, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 20, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__synchronic_diachronic_seam, information_standard).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__first_holding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'ip_category_emergence' kernel. This reading tests the relationship between the 'thinkability_reading' (category emergence) and 'first_holding_reading' (occupancy change), influencing both without foreclosing them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
