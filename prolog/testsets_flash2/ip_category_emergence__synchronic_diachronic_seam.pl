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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: ip_category_emergence__synchronic_diachronic_seam
 *   human_readable: IP Category Emergence: Synchronic/Diachronic Seam
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This constraint, 'IP Category Emergence: Synchronic/Diachronic Seam', is
 *   an analytical framework within legal philosophy and historical
 *   jurisprudence. It posits that the distinction between a legal category
 *   becoming 'thinkable' (conceptually coherent) and its 'first holding'
 *   (actual legal occupancy by a claimant) is either a formal independence or
 *   a temporal framing artifact. This reading aims to test whether these two
 *   aspects of IP's emergence can vary independently or always co-occur,
 *   thereby determining if the underlying kernel structure is authentic or
 *   spurious. It is claimed as a Mountain because it describes a fundamental
 *   analytical distinction that, if true, is an irreducible feature of legal
 *   conceptualization, not a human construct.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, 0.3).
domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, 0.2).
domain_priors:theater_ratio(ip_category_emergence__synchronic_diachronic_seam, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, extractiveness, 0.3).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__synchronic_diachronic_seam, mountain).
narrative_ontology:human_readable(ip_category_emergence__synchronic_diachronic_seam, "IP Category Emergence: Synchronic/Diachronic Seam").
narrative_ontology:topic_domain(ip_category_emergence__synchronic_diachronic_seam, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:emerges_naturally(ip_category_emergence__synchronic_diachronic_seam).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__synchronic_diachronic_seam, '6436bb74-e492-4c0c-bc12-9a9ba3399f80').
narrative_ontology:cs_kernel_codification('6436bb74-e492-4c0c-bc12-9a9ba3399f80', distributed).
narrative_ontology:cs_authority_grounding('6436bb74-e492-4c0c-bc12-9a9ba3399f80', expertise).
narrative_ontology:cs_reading_relation('6436bb74-e492-4c0c-bc12-9a9ba3399f80', ip_category_emergence__thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('6436bb74-e492-4c0c-bc12-9a9ba3399f80', ip_category_emergence__first_holding_reading, coexists_with).
narrative_ontology:cs_axiom('6436bb74-e492-4c0c-bc12-9a9ba3399f80', foundational, conceptual_emergence_distinct_from_legal_occupancy).
narrative_ontology:cs_axiom_status(conceptual_emergence_distinct_from_legal_occupancy, holdable).
narrative_ontology:cs_axiom_grounding('6436bb74-e492-4c0c-bc12-9a9ba3399f80', conceptual_emergence_distinct_from_legal_occupancy, empirically_contingent).
narrative_ontology:cs_reference_frame('6436bb74-e492-4c0c-bc12-9a9ba3399f80', analytical_disaggregation_framework).
narrative_ontology:cs_drift_state('6436bb74-e492-4c0c-bc12-9a9ba3399f80', contemporary_legal_philosophy, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6436bb74-e492-4c0c-bc12-9a9ba3399f80', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, legal_historians).
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, intellectual_property_theorists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clearer understanding of how legal categories evolve and whether the emergence of IP as a 'thinkable' concept is distinct from the historical moment of its first legal 'holding'. This reading provides a framework for their research.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, legal_historians, beneficiary,
    analytical, generational, analytical, global).

% Gain a more robust theoretical foundation for IP by distinguishing between conceptual coherence and practical application. This helps them refine arguments about the nature and justification of IP rights.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, intellectual_property_theorists, beneficiary,
    analytical, generational, analytical, global).

% The primary data source for this analysis. They passively 'observe' the constraint by providing the evidence for its operation, but have no agency.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, historical_legal_texts, observer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(ip_category_emergence__synchronic_diachronic_seam, historical_legal_texts).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a conceptual framework for legal scholars to coordinate their understanding of intellectual property's historical development, distinguishing between the emergence of a legal category and the establishment of rights within it.
% TRANSFER_FUNCTION: Transfers conceptual clarity and analytical rigor to the field of IP history and theory, from a state of potential conflation to one of disambiguated understanding.
% ABSENT_VOICES: No directly absent voices, as this is an analytical constraint. However, earlier legal scholars who conflated 'thinkability' and 'first-holding' might, if present, argue against the distinction, but their arguments would be based on a less refined conceptual apparatus.
% DISAPPEARANCE_RATIONALE: If this distinction vanished, legal historians and IP theorists would revert to a less precise understanding of IP's origins, potentially conflating conceptual emergence with historical occupancy. This would hinder nuanced analysis of legal evolution.
% FOUNDING_PROBLEM: The problem of distinguishing whether the historical moment of IP's legal recognition (e.g., Statute of Anne 1710) represented the first time 'ownable expression' became a coherent legal concept (category emergence) or merely the first time a specific claimant (author) was recognized as holding rights within an already coherent category (occupancy change).
% FOUNDING_PROBLEM_CORROBORATION: Legal philosophers and historians outside the immediate IP field corroborate the ongoing challenge of disentangling conceptual shifts from practical legal enactments in historical analysis. The problem is widely recognized in meta-historical and legal theory discussions.
narrative_ontology:disappearance_verdict(ip_category_emergence__synchronic_diachronic_seam, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__synchronic_diachronic_seam, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__synchronic_diachronic_seam, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ip_category_emergence__synchronic_diachronic_seam, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__synchronic_diachronic_seam, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, ExtMetricName, E),
    domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ip_category_emergence__synchronic_diachronic_seam),
    narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.3) because this constraint primarily offers conceptual clarity rather than imposing costs or extracting resources. Suppression is low (0.2) as it's an analytical framework; its 'enforcement' comes from logical coherence and empirical fit, not coercion. Theater ratio is low (0.1) as there's little performative aspect to a conceptual distinction. Accessibility collapse is high (0.7) because once the distinction is understood, it fundamentally alters how one approaches IP history, making prior conflations less viable. Resistance is low (0.15) as it's a refinement of understanding, not a contested policy.
 *
 * PERSPECTIVAL GAP:
 *   As an analytical constraint, there is less perspectival divergence than with policy-based constraints. The 'gap' would primarily exist between those who adopt this refined analytical lens and those who continue to conflate category emergence with first-holding, leading to different interpretations of historical data.
 *
 * DIRECTIONALITY LOGIC:
 *   Legal historians and IP theorists are the primary beneficiaries, as the constraint provides them with a more precise analytical tool. There are no direct 'victims' in the traditional sense, as it's an analytical framework. Historical legal texts are 'observers' as they provide the data for the analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint helps prevent mislabeling by clarifying whether historical shifts in IP represent genuine conceptual breakthroughs (Mountains of legal thought) or merely changes in who benefits from existing categories (Snare/Tangled Rope). It ensures that analytical distinctions are not mistaken for policy choices or vice versa.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_separability,
    'Can empirical historical data definitively demonstrate the independent variation of ''thinkability'' and ''first-holding'' in legal history, or do they always appear to co-occur?',
    'Comparative historical legal analysis across multiple jurisdictions and legal domains, seeking instances where one phenomenon clearly precedes or lags the other without direct causal linkage.',
    'If independent variation is robustly demonstrated, it strengthens the claim that the distinction is a fundamental feature of legal conceptualization (Mountain). If they always co-occur, it suggests the distinction might be a conceptual artifact rather than an authentic structural seam, potentially reclassifying it as a more constructed constraint (e.g., Rope or Tangled Rope of analytical convention).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_separability, empirical, 'Tests the empirical reality of the distinction between conceptual emergence and legal occupancy.').

omega_variable(
    kernel_framing_ambiguity,
    'Is this constraint a genuine analytical distinction (Mountain), or a constructed conceptual tool (Rope) that benefits legal scholars by providing a new lens, but which could be framed differently?',
    'Analysis of alternative meta-historical frameworks that achieve similar explanatory power without relying on this specific distinction. If such frameworks exist and are equally robust, it suggests a conceptual choice rather than an inherent truth.',
    'If it''s a constructed tool, its classification might shift from Mountain to Rope, acknowledging its utility but also its contingent nature as a chosen analytical convention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Ambiguity between an inherent analytical truth and a useful conceptual construct.').

omega_variable(
    sibling_reading_impact,
    'How would the acceptance of this ''synchronic_diachronic_seam'' reading impact the ''thinkability_reading'' and ''first_holding_reading'' of IP category emergence?',
    'Further theoretical work and scholarly consensus on whether this reading provides a superior, more encompassing framework that subsumes or refines the sibling readings, or if it merely offers an orthogonal perspective.',
    'If this reading is accepted as foundational, it would likely influence the interpretation and perceived validity of the sibling readings, potentially leading to their re-evaluation or integration into a broader framework. It would clarify the scope and limitations of each sibling''s claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact, conceptual, 'Impact of this reading on the broader kernel contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__synchronic_diachronic_seam, 1700, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1700, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(ip_c_tr_t1800, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(ip_c_tr_t1900, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(ip_c_tr_t2000, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(ip_c_tr_t2024, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1700, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1700, 0.3).
narrative_ontology:measurement(ip_c_be_t1800, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1800, 0.3).
narrative_ontology:measurement(ip_c_be_t1900, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(ip_c_be_t2000, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(ip_c_be_t2024, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1700, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1700, 0.2).
narrative_ontology:measurement(ip_c_su_t1800, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1800, 0.2).
narrative_ontology:measurement(ip_c_su_t1900, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 1900, 0.2).
narrative_ontology:measurement(ip_c_su_t2000, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(ip_c_su_t2024, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__synchronic_diachronic_seam, information_standard).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence__first_holding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ip_category_emergence' kernel, focusing on the distinction between conceptual thinkability and first legal holding. It is linked to sibling readings that emphasize one aspect over the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
