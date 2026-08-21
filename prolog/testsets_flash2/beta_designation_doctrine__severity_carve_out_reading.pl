% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__severity_carve_out_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: beta_designation_doctrine__severity_carve_out_reading
 *   human_readable: Beta Designation Doctrine: Severity Carve-Out Reading
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the 'beta designation
 *   doctrine' in software liability law, asserting that the 'beta' label
 *   cannot be used to waive liability for software deployed in life-safety,
 *   financial, or other critical systems. It functions as a protective
 *   measure for users and a burden for developers in high-stakes domains. The
 *   claimed type is 'tangled_rope' because it coordinates safety expectations
 *   while extracting increased liability from developers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, 0.65).
domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, 0.78).
domain_priors:theater_ratio(beta_designation_doctrine__severity_carve_out_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__severity_carve_out_reading, tangled_rope).
narrative_ontology:human_readable(beta_designation_doctrine__severity_carve_out_reading, "Beta Designation Doctrine: Severity Carve-Out Reading").
narrative_ontology:topic_domain(beta_designation_doctrine__severity_carve_out_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__severity_carve_out_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__severity_carve_out_reading, '230fdd5a-748b-4ff6-a4e0-4ff81aa81080').
narrative_ontology:cs_kernel_codification('230fdd5a-748b-4ff6-a4e0-4ff81aa81080', formalized).
narrative_ontology:cs_authority_grounding('230fdd5a-748b-4ff6-a4e0-4ff81aa81080', lineage).
narrative_ontology:cs_interpretation_layer_present('230fdd5a-748b-4ff6-a4e0-4ff81aa81080').
narrative_ontology:cs_reading_relation('230fdd5a-748b-4ff6-a4e0-4ff81aa81080', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('230fdd5a-748b-4ff6-a4e0-4ff81aa81080', beta_designation_doctrine__narrow_warning_reading, coexists_with).
narrative_ontology:cs_axiom('230fdd5a-748b-4ff6-a4e0-4ff81aa81080', foundational, harm_severity_overrides_contractual_waiver).
narrative_ontology:cs_axiom_status(harm_severity_overrides_contractual_waiver, holdable).
narrative_ontology:cs_axiom_grounding('230fdd5a-748b-4ff6-a4e0-4ff81aa81080', harm_severity_overrides_contractual_waiver, deontological).
narrative_ontology:cs_axiom('230fdd5a-748b-4ff6-a4e0-4ff81aa81080', foundational, public_safety_is_non_negotiable).
narrative_ontology:cs_axiom_status(public_safety_is_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('230fdd5a-748b-4ff6-a4e0-4ff81aa81080', public_safety_is_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('230fdd5a-748b-4ff6-a4e0-4ff81aa81080', strict_liability_for_critical_systems).
narrative_ontology:cs_drift_state('230fdd5a-748b-4ff6-a4e0-4ff81aa81080', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('230fdd5a-748b-4ff6-a4e0-4ff81aa81080', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, critical_system_users).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, regulatory_bodies).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, software_developers_critical_systems).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, insurers_critical_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Users of life-safety, financial, or other critical systems who are protected from the risks of 'beta' software being deployed without full liability. Their safety is prioritized over developer flexibility.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, critical_system_users, beneficiary,
    powerless, immediate, trapped, global).

% Enforce the carve-out, ensuring that software in critical domains meets higher standards of reliability and safety, regardless of developer intent to label it 'beta'. They benefit from clearer lines of accountability.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Bear the cost of increased liability and development rigor for software in critical domains. They cannot use 'beta' status to limit their liability, forcing more thorough testing and compliance from the outset.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, software_developers_critical_systems, payer,
    powerful, biographical, constrained, global).

% Face higher, non-waivable liability exposure for critical systems, leading to increased premiums or more stringent underwriting requirements for developers in these sectors. They cannot rely on 'beta' disclaimers to reduce risk.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, insurers_critical_systems, payer,
    institutional, biographical, constrained, global).

% Analyze the implications of this carve-out for software liability law, consumer protection, and the future of agile development in regulated industries. They contribute to the ongoing debate about the doctrine.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the expectation that software in critical domains (life-safety, financial) will meet a baseline of reliability and safety, preventing developers from externalizing risk onto users via 'beta' labels.
% TRANSFER_FUNCTION: Transfers liability and development burden from critical system users and the public to software developers and their insurers, ensuring that the party best able to mitigate risk bears the cost.
% ABSENT_VOICES: Developers of non-critical software, who benefit from more expansive beta liability shields, are not directly impacted by this carve-out but would likely oppose its expansion to their domains, fearing increased regulatory burden.
% DISAPPEARANCE_RATIONALE: If this carve-out vanished, developers of critical systems would immediately begin labeling software 'beta' to limit liability, shifting risk back to users and potentially leading to catastrophic failures in life-safety and financial systems. Regulatory bodies would lose a key tool for consumer protection.
% FOUNDING_PROBLEM: The potential for catastrophic harm from untested or unreliable software in critical applications, coupled with developers attempting to disclaim liability through 'beta' designations, created a gap in consumer protection.
% FOUNDING_PROBLEM_CORROBORATION: Consumer advocacy groups, public safety organizations, and historical examples of software failures in critical infrastructure corroborate the ongoing nature of this problem. Regulatory bodies also attest to the need for this protection.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__severity_carve_out_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__severity_carve_out_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__severity_carve_out_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(beta_designation_doctrine__severity_carve_out_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__severity_carve_out_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because developers in critical sectors must invest significantly more in testing and compliance, and bear full liability, without the 'beta' shield. Suppression is also high (0.78) as regulatory bodies actively enforce this carve-out, rejecting attempts to use 'beta' disclaimers in these contexts. Theater ratio is low (0.1) because the enforcement is genuinely aimed at preventing harm, not merely performing compliance. Accessibility collapse is moderate (0.7) as developers still have options for testing and deployment, but the 'beta' route is closed for critical systems. Resistance is moderate (0.4) from developers who prefer more flexibility.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of critical system users, this is a necessary protection (beneficiary seat). From the perspective of software developers in these domains, it is a significant burden and a limitation on their development practices (payer seat). The engine will compute these divergent classifications based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Critical system users and regulatory bodies are beneficiaries, gaining protection and clearer accountability. Software developers and their insurers in these critical sectors are payers, bearing the increased costs and liability. Legal scholars act as observers, analyzing the doctrine's impact.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_critical_systems,
    'What constitutes a ''critical system'' for the purpose of this carve-out, and is this definition consistently applied across jurisdictions?',
    'Consensus among international regulatory bodies on a standardized definition of ''critical systems'' for software liability, or a landmark legal ruling establishing clear precedents.',
    'A narrower definition would reduce the scope of extraction from developers but increase risk for users; a broader definition would increase developer burden but enhance user protection. Inconsistent application creates regulatory arbitrage opportunities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_critical_systems, conceptual, 'Ambiguity in defining ''critical systems'' for liability purposes.').

omega_variable(
    technological_evolution_impact,
    'How does the rapid evolution of software development practices (e.g., AI integration, continuous deployment) impact the applicability and enforcement of this carve-out?',
    'Empirical studies on the failure rates and liability implications of new technologies in critical systems, leading to updated regulatory frameworks or legal interpretations.',
    'If new technologies blur the lines between ''beta'' and ''production'' or introduce novel risks, the carve-out''s effectiveness could erode, requiring re-evaluation of its scope and enforcement mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_evolution_impact, empirical, 'Impact of technological change on the carve-out''s relevance.').

omega_variable(
    reading_legitimacy_contest,
    'Is this ''severity_carve_out_reading'' gaining or losing legitimacy in legal and regulatory discourse compared to sibling readings?',
    'Analysis of legislative trends, court rulings, and academic consensus over time, tracking which reading''s principles are increasingly adopted or rejected.',
    'If this reading gains legitimacy, it strengthens consumer protection in critical domains. If it loses ground to the ''expansive_shield_reading'', it signals a shift towards prioritizing developer flexibility over user safety, potentially increasing systemic risk.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_legitimacy_contest, conceptual, 'The ongoing contest for legitimacy among different readings of the beta designation doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__severity_carve_out_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(beta_tr_t5, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(beta_tr_t10, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(beta_tr_t15, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(beta_be_t5, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(beta_be_t10, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(beta_be_t15, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(beta_su_t5, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(beta_su_t10, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement(beta_su_t15, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 15, 0.76).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
