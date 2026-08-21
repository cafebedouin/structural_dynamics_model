% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__expansive_shield_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__expansive_shield_reading, []).

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
 *   constraint_id: beta_designation_doctrine__expansive_shield_reading
 *   human_readable: Expansive Beta Liability Shield Doctrine
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   This constraint represents the 'expansive shield' reading of the beta
 *   designation doctrine, where a beta label constitutes a comprehensive
 *   liability waiver for software developers, is permissible for indefinite
 *   durations, and applies to all software contexts, regardless of
 *   criticality. This reading prioritizes developer freedom and innovation by
 *   externalizing all defect costs to users, effectively making users the
 *   primary victims. The high extractiveness and suppression reflect the
 *   structural power imbalance and lack of recourse for users.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, 0.85).
domain_priors:suppression_score(beta_designation_doctrine__expansive_shield_reading, 0.78).
domain_priors:theater_ratio(beta_designation_doctrine__expansive_shield_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__expansive_shield_reading, snare).
narrative_ontology:human_readable(beta_designation_doctrine__expansive_shield_reading, "Expansive Beta Liability Shield Doctrine").
narrative_ontology:topic_domain(beta_designation_doctrine__expansive_shield_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__expansive_shield_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__expansive_shield_reading, '48ccc755-a672-46b7-8f35-fd89f7b0f920').
narrative_ontology:cs_kernel_codification('48ccc755-a672-46b7-8f35-fd89f7b0f920', formalized).
narrative_ontology:cs_authority_grounding('48ccc755-a672-46b7-8f35-fd89f7b0f920', extraction).
narrative_ontology:cs_interpretation_layer_present('48ccc755-a672-46b7-8f35-fd89f7b0f920').
narrative_ontology:cs_reading_relation('48ccc755-a672-46b7-8f35-fd89f7b0f920', beta_designation_doctrine__narrow_warning_reading, forecloses).
narrative_ontology:cs_reading_relation('48ccc755-a672-46b7-8f35-fd89f7b0f920', beta_designation_doctrine__severity_carve_out_reading, forecloses).
narrative_ontology:cs_axiom('48ccc755-a672-46b7-8f35-fd89f7b0f920', foundational, beta_designation_grants_absolute_immunity).
narrative_ontology:cs_axiom_status(beta_designation_grants_absolute_immunity, holdable).
narrative_ontology:cs_axiom_grounding('48ccc755-a672-46b7-8f35-fd89f7b0f920', beta_designation_grants_absolute_immunity, conventional).
narrative_ontology:cs_axiom('48ccc755-a672-46b7-8f35-fd89f7b0f920', foundational, no_distinction_by_software_criticality).
narrative_ontology:cs_axiom_status(no_distinction_by_software_criticality, holdable).
narrative_ontology:cs_axiom_grounding('48ccc755-a672-46b7-8f35-fd89f7b0f920', no_distinction_by_software_criticality, conventional).
narrative_ontology:cs_reference_frame('48ccc755-a672-46b7-8f35-fd89f7b0f920', developer_liability_minimization).
narrative_ontology:cs_drift_state('48ccc755-a672-46b7-8f35-fd89f7b0f920', contemporary_software_industry, gap(stable, minor, true)).
narrative_ontology:cs_created_at('48ccc755-a672-46b7-8f35-fd89f7b0f920', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, software_developers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, software_publishers).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, software_users).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, consumer_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively lobby for and benefit from broad interpretations of beta liability waivers, allowing them to release software with minimal legal risk. They frame beta status as essential for innovation and rapid iteration.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, software_developers, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from the reduced liability associated with beta designations, allowing them to bring products to market faster and with lower legal overhead. They support the expansive interpretation of the doctrine.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, software_publishers, beneficiary,
    powerful, biographical, mobile, global).

% Bear the full risk and cost of defects in software designated as 'beta', regardless of its maturity, duration, or criticality. They often have no practical alternative to using such software.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, software_users, payer,
    powerless, immediate, trapped, global).

% Work to challenge the expansive interpretation of beta liability waivers through legal and legislative means, arguing for greater consumer protection and developer accountability. They bear the costs of litigation and advocacy.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, consumer_advocates, payer,
    organized, generational, constrained, national).

% The courts and legislative bodies that interpret and enforce the beta designation doctrine. Under this reading, the legal system largely upholds the broad waiver, often influenced by industry lobbying and existing precedents.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, legal_system, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates the public testing and iterative development of complex software by providing a legal framework that limits developer liability during early stages.
% TRANSFER_FUNCTION: Transfers the financial and operational liability for software defects, bugs, and security vulnerabilities from software developers and publishers to the end-users, across all software contexts and for indefinite durations.
% ABSENT_VOICES: Individual software users, who are too diffuse and unorganized to effectively challenge the doctrine, and future users who will inherit the risks without having a say in the current legal interpretations.
% DISAPPEARANCE_RATIONALE: If the expansive beta liability shield vanished overnight, software developers would face immense and immediate liability for defects, leading to a drastic slowdown in new software releases, increased development costs, and a fundamental shift in how software is tested and deployed. Consumer protection laws would likely expand rapidly to fill the void.
% FOUNDING_PROBLEM: The original problem was to enable rapid innovation and public testing of complex software systems without developers incurring prohibitive liability for inevitable early-stage bugs, thereby fostering a nascent software industry.
% FOUNDING_PROBLEM_CORROBORATION: Software developers and publishers argue the founding problem is still live, citing the increasing complexity and interconnectedness of modern software. Consumer advocates and some legal scholars contend that for many 'beta' products, the problem is largely solved, and the doctrine now serves primarily to externalize costs and suppress accountability, supported by independent legal analysis and consumer harm reports.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__expansive_shield_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__expansive_shield_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__expansive_shield_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(beta_designation_doctrine__expansive_shield_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__expansive_shield_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__expansive_shield_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(beta_designation_doctrine__expansive_shield_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because developers bear almost no liability for defects, transferring all associated costs and risks to users. Suppression (0.78) is high due to the legal system's enforcement of this broad waiver and the lack of organized, effective exit options for users. The theater ratio (0.1) is low because, from this reading's perspective, the waiver is the primary, functional intent, not a performance masking a different goal. Accessibility collapse is high (0.9) as users have no practical alternative to accepting these terms for widely used software. Resistance (0.4) is moderate, primarily from consumer advocacy groups, but individual users face significant barriers.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of software developers, this doctrine is a necessary 'rope' for innovation, allowing them to iterate quickly. From the perspective of users and consumer advocates, it functions as a 'snare', coercively extracting value by externalizing all risk. The engine's classification will reflect the latter due to the high extractiveness and suppression, highlighting the divergence from the claimed coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Software developers and publishers are clear beneficiaries (low d) as they avoid liability. Software users and consumer advocates are clear targets (high d) as they bear the costs and risks. The legal system, as an agenda-setter, enforces this transfer, aligning with the beneficiaries in this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indefinite_duration_justification,
    'Is the indefinite ''beta'' status of some software genuinely for testing and iteration, or is it primarily a legal strategy to avoid liability for mature products?',
    'Empirical analysis of software development cycles, bug fix rates, and feature stability for long-term ''beta'' products, compared to industry standards for release candidates.',
    'If primarily a legal strategy, the extractiveness and suppression metrics are further validated as rent-seeking, strengthening the ''snare'' classification. If genuinely for testing, a portion of the extraction might be reclassified as legitimate coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indefinite_duration_justification, empirical, 'Ambiguity regarding the true purpose of indefinite beta periods.').

omega_variable(
    scope_of_waiver_necessity,
    'Is a comprehensive liability waiver truly necessary for all software contexts, including life-critical or financial systems, to foster innovation?',
    'Comparative legal analysis of jurisdictions with different liability regimes for critical software, assessing innovation rates and safety outcomes. Expert testimony from software engineers on the feasibility of rigorous testing for critical systems.',
    'If not necessary for critical systems, the ''expansive_shield_reading'' is revealed as overreaching, and its ''snare'' classification is reinforced, potentially leading to a ''severity_carve_out_reading'' being adopted. If necessary, the current reading''s justification gains some empirical support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_waiver_necessity, conceptual, 'Whether the universal applicability of the waiver is justified or an overreach.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of user recourse structural (legal barriers, cost of litigation) or internalized (users'' belief that bugs are inevitable and their responsibility)?',
    'Post-litigation user behavior surveys: if users continue to accept liability even after legal avenues are clarified, reclassify as partially internalized. Analysis of legal aid accessibility for software-related harms.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — users carry the suppression with them. If purely structural, legal reforms would be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for software users.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__expansive_shield_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t1995, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(beta_tr_t2000, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(beta_tr_t2005, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(beta_tr_t2010, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(beta_tr_t2015, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 2015, 0.11).
narrative_ontology:measurement(beta_tr_t2020, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(beta_tr_t2025, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(beta_be_t1995, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(beta_be_t2000, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(beta_be_t2005, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 2005, 0.75).
narrative_ontology:measurement(beta_be_t2010, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(beta_be_t2015, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 2015, 0.83).
narrative_ontology:measurement(beta_be_t2020, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 2020, 0.84).
narrative_ontology:measurement(beta_be_t2025, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t1995, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(beta_su_t2000, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(beta_su_t2005, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement(beta_su_t2010, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(beta_su_t2015, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(beta_su_t2020, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 2020, 0.77).
narrative_ontology:measurement(beta_su_t2025, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__expansive_shield_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
