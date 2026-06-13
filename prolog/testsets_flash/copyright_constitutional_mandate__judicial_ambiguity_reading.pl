% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__judicial_ambiguity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__judicial_ambiguity_reading, []).

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
 *   constraint_id: copyright_constitutional_mandate__judicial_ambiguity_reading
 *   human_readable: Judicial Deference to Copyright Term Length (Judicial Ambiguity Reading)
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint describes the judicial interpretation of the Copyright
 *   Clause ('To promote the Progress of Science and useful Arts, by securing
 *   for limited Times to Authors and Inventors the exclusive Right to their
 *   respective Writings and Discoveries') as granting Congress broad
 *   discretion over copyright term length. Courts, particularly the Supreme
 *   Court, have consistently applied a rational basis review, deferring to
 *   Congress's judgment on what constitutes 'limited Times' and 'to promote
 *   the Progress'. This reading treats the constitutional language as
 *   ambiguous, allowing legislative action to define the practical scope of
 *   copyright, even if it leads to successive extensions.
 *
 * KEY AGENTS:
 *   - congressional_authority: Primary beneficiary (institutional/arbitrage) — exercises broad discretion.
 *   - judiciary: Secondary beneficiary (institutional/analytical) — maintains institutional role by deferring.
 *   - copyright_holders: Indirect beneficiaries (organized/mobile) — benefit from extended terms enabled by deference.
 *   - public_domain_advocates: Payer (organized/constrained) — bear the cost of reduced public domain access.
 *   - constitutional_scholars: Observer (analytical/analytical) — analyze the legal and historical implications of deference.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.35).
domain_priors:suppression_score(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.6).
domain_priors:theater_ratio(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__judicial_ambiguity_reading, rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__judicial_ambiguity_reading, "Judicial Deference to Copyright Term Length (Judicial Ambiguity Reading)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__judicial_ambiguity_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__judicial_ambiguity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__judicial_ambiguity_reading, '0fcb1b27-12d6-4ba1-ae8c-834a6da9fcf8').
narrative_ontology:cs_kernel_codification('0fcb1b27-12d6-4ba1-ae8c-834a6da9fcf8', fixed_text).
narrative_ontology:cs_authority_grounding('0fcb1b27-12d6-4ba1-ae8c-834a6da9fcf8', lineage).
narrative_ontology:cs_interpretation_layer_present('0fcb1b27-12d6-4ba1-ae8c-834a6da9fcf8').
narrative_ontology:cs_reading_relation('0fcb1b27-12d6-4ba1-ae8c-834a6da9fcf8', copyright_constitutional_mandate__public_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('0fcb1b27-12d6-4ba1-ae8c-834a6da9fcf8', copyright_constitutional_mandate__corporate_enclosure_reading, influences).
narrative_ontology:cs_axiom('0fcb1b27-12d6-4ba1-ae8c-834a6da9fcf8', foundational, limited_times_is_legislative_discretion).
narrative_ontology:cs_axiom_status(limited_times_is_legislative_discretion, holdable).
narrative_ontology:cs_axiom_grounding('0fcb1b27-12d6-4ba1-ae8c-834a6da9fcf8', limited_times_is_legislative_discretion, conventional).
narrative_ontology:cs_axiom('0fcb1b27-12d6-4ba1-ae8c-834a6da9fcf8', foundational, rational_basis_review_applies_to_copyright).
narrative_ontology:cs_axiom_status(rational_basis_review_applies_to_copyright, holdable).
narrative_ontology:cs_axiom_grounding('0fcb1b27-12d6-4ba1-ae8c-834a6da9fcf8', rational_basis_review_applies_to_copyright, conventional).
narrative_ontology:cs_reference_frame('0fcb1b27-12d6-4ba1-ae8c-834a6da9fcf8', judicial_deference_to_congress).
narrative_ontology:cs_drift_state('0fcb1b27-12d6-4ba1-ae8c-834a6da9fcf8', contemporary_legal_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0fcb1b27-12d6-4ba1-ae8c-834a6da9fcf8', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_authority).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, judiciary).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_fixity_as_constraint).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(copyright_constitutional_mandate__judicial_ambiguity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).
:- end_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it provides a framework for legislative action (coordination) while allowing for some extraction (extended terms benefit copyright holders). Extractiveness is moderate (0.35) because while terms are extended, the 'limited times' clause still theoretically exists as a boundary. Suppression is moderate (0.6) as judicial deference actively suppresses challenges to term extensions, making it difficult for opponents to prevail. Theater ratio is low (0.1) as the judicial review process is genuinely functional in upholding the legislative framework, even if the outcome consistently favors extension. The increasing extractiveness and suppression over time reflect the historical trend of copyright term extensions and the judiciary's consistent deference.
 *
 * PERSPECTIVAL GAP:
 *   Congressional authority and the judiciary experience this as a functional, legitimate exercise of power and a stable interpretive framework, respectively. Public domain advocates, however, experience it as a mechanism that enables the gradual enclosure of the public domain, with their arguments consistently suppressed by judicial deference. The engine's per-seat classification should reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Congressional authority is a primary beneficiary (d=0.0) as it gains maximal flexibility. The judiciary is also a beneficiary (d=0.1) as deference simplifies its role and avoids constitutional crises. Copyright holders are indirect beneficiaries (d=0.2) as they profit from the extended terms. Constitutional fixity as a constraint is a victim (d=1.0) as its power to limit legislative action is eroded. Public domain advocates are payers (d=0.8) as they bear the cost of reduced access and their arguments are consistently rejected.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling legislative discretion as pure extraction by acknowledging the genuine coordination function of providing a stable legal framework for copyright. However, the increasing extractiveness over time suggests a drift towards a Tangled Rope, where the coordination function increasingly serves as cover for rent-seeking, enabled by judicial deference. The 'founding_problem_status' being 'contested' further highlights this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legislative_discretion_scope,
    'What are the actual limits of ''legislative discretion'' in copyright term length, beyond which judicial deference would cease?',
    'A Supreme Court ruling explicitly defining the outer bounds of ''limited times'' or striking down a term extension as unconstitutional.',
    'If limits are found to be narrow, the constraint shifts towards a Mountain for Congress; if broad, it reinforces the current Rope classification, enabling further extensions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_discretion_scope, conceptual, 'Ambiguity of ''limited times'' in the Copyright Clause and the scope of judicial deference.').

omega_variable(
    natural_law_vs_construct,
    'Is the ''rational basis'' for copyright term extensions a genuine legislative finding, or a post-hoc justification for rent-seeking by copyright holders?',
    'Empirical studies on the incentive effects of term extensions, or judicial review that applies a higher standard of scrutiny to legislative findings.',
    'If found to be post-hoc justification, the constraint''s extractiveness is higher than currently measured, and its classification shifts towards Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_construct, empirical, 'Whether legislative discretion is genuinely about public good or disguised private benefit.').

omega_variable(
    kernel_reading_difference,
    'This constraint is the ''judicial_ambiguity_reading'' of the ''copyright_constitutional_mandate'' kernel. How would the classification change under the ''public_scaffold_reading'' or ''corporate_enclosure_reading''?',
    'Adoption of a different interpretive framework by the judiciary or legislative branch.',
    'The ''public_scaffold_reading'' would likely classify as a Scaffold, emphasizing the temporary nature and public benefit. The ''corporate_enclosure_reading'' would likely classify as a Snare, emphasizing maximal extraction and property rights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'Impact of alternative kernel readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__judicial_ambiguity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t0, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(copy_tr_t10, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(copy_tr_t20, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(copy_tr_t30, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(copy_be_t0, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(copy_be_t10, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(copy_be_t20, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(copy_be_t30, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 30, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t0, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(copy_su_t10, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(copy_su_t20, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(copy_su_t30, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__judicial_ambiguity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_term_extension_acts).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'copyright_constitutional_mandate' kernel, focusing on judicial deference. The other readings emphasize public benefit or corporate property rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
