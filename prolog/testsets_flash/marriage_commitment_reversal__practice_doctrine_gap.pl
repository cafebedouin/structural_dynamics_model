% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__practice_doctrine_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__practice_doctrine_gap, []).

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
 *   constraint_id: marriage_commitment_reversal__practice_doctrine_gap
 *   human_readable: Marriage Commitment Reversal: Practice-Doctrine Gap
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint describes the period (1890-1904) where a religious
 *   institution publicly suspended the practice of plural marriage due to
 *   federal pressure, while internally preserving the doctrinal principle
 *   (Section 132). This created a significant gap between declared doctrine
 *   and observable practice, leading to high extractiveness on the general
 *   membership (cognitive dissonance, identity strain) and fundamentalist
 *   factions (schism), while benefiting institutional survival through
 *   strategic ambiguity. The constraint is claimed as a Tangled Rope by the
 *   institution's own framing (coordinating compliance while preserving
 *   doctrine), but the metrics reflect its highly extractive and performative
 *   nature.
 *
 * KEY AGENTS:
 *   - institutional_survival: Primary beneficiary (institutional/arbitrage)
 *   - general_membership: Primary target (powerless/identity_locked)
 *   - fundamentalist_factions: Secondary target (organized/constrained)
 *   - federal_government: External agenda_setter (institutional/analytical)
 *   - institutional_leadership: Internal agenda_setter (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, 0.78).
domain_priors:suppression_score(marriage_commitment_reversal__practice_doctrine_gap, 0.65).
domain_priors:theater_ratio(marriage_commitment_reversal__practice_doctrine_gap, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, extractiveness, 0.78).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__practice_doctrine_gap, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__practice_doctrine_gap, "Marriage Commitment Reversal: Practice-Doctrine Gap").
narrative_ontology:topic_domain(marriage_commitment_reversal__practice_doctrine_gap, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__practice_doctrine_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__practice_doctrine_gap, '20c9fa1c-d226-4274-a8e9-614f98ccc951').
narrative_ontology:cs_kernel_codification('20c9fa1c-d226-4274-a8e9-614f98ccc951', formalized).
narrative_ontology:cs_authority_grounding('20c9fa1c-d226-4274-a8e9-614f98ccc951', lineage).
narrative_ontology:cs_interpretation_layer_present('20c9fa1c-d226-4274-a8e9-614f98ccc951').
narrative_ontology:cs_reading_relation('20c9fa1c-d226-4274-a8e9-614f98ccc951', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('20c9fa1c-d226-4274-a8e9-614f98ccc951', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('20c9fa1c-d226-4274-a8e9-614f98ccc951', foundational, doctrinal_principle_persists_despite_practice).
narrative_ontology:cs_axiom_status(doctrinal_principle_persists_despite_practice, holdable).
narrative_ontology:cs_axiom_grounding('20c9fa1c-d226-4274-a8e9-614f98ccc951', doctrinal_principle_persists_despite_practice, conventional).
narrative_ontology:cs_axiom('20c9fa1c-d226-4274-a8e9-614f98ccc951', secondary, institutional_survival_justifies_ambiguity).
narrative_ontology:cs_axiom_status(institutional_survival_justifies_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding('20c9fa1c-d226-4274-a8e9-614f98ccc951', institutional_survival_justifies_ambiguity, instrumental).
narrative_ontology:cs_reference_frame('20c9fa1c-d226-4274-a8e9-614f98ccc951', doctrinal_purity_and_public_practice_alignment).
narrative_ontology:cs_drift_state('20c9fa1c-d226-4274-a8e9-614f98ccc951', post_manifesto_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('20c9fa1c-d226-4274-a8e9-614f98ccc951', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, institutional_survival).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, general_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional entity itself benefits from the ambiguity, allowing it to navigate federal anti-polygamy laws while preserving core doctrine. This flexibility enabled the continuation of plural marriages in claimed-legal jurisdictions (1890-1904) and avoided disincorporation and asset seizure.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, institutional_survival, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_non_agent(marriage_commitment_reversal__practice_doctrine_gap, institutional_survival).

% Experienced bewilderment and a sense of betrayal as public practice diverged from deeply held doctrine without clear explanation. Many were identity-locked by their faith and community, making exit unthinkable despite the cognitive dissonance and personal cost of compliance.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, general_membership, payer,
    powerless, biographical, identity_locked, local).

% Suffered schism and excommunication for adhering to the original doctrine and practice. They bore the cost of social and religious ostracization, but their organized nature allowed for a constrained exit into new, smaller communities.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_factions, payer,
    organized, generational, constrained, regional).

% Applied coercive pressure through legislation (Edmunds-Tucker Act) and legal action, threatening disincorporation and asset seizure. Its actions created the external conditions that forced the institutional leadership to suspend public practice.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% Navigated the external pressure by issuing the Manifesto, publicly suspending plural marriage while preserving the underlying doctrine. They managed the internal and external legitimation challenges, maintaining institutional cohesion at the cost of doctrinal clarity for many members.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership, agenda_setter,
    institutional, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allowed the institution to coordinate its public compliance with federal law while internally maintaining a commitment to its core marriage doctrine, thereby preserving its legal existence and assets.
% TRANSFER_FUNCTION: Transferred the burden of cognitive dissonance and doctrinal ambiguity onto the general membership, while transferring institutional legitimacy and survival from the federal government back to the religious organization.
% ABSENT_VOICES: Those who left the institution due to the doctrinal ambiguity or perceived betrayal, as well as future generations who would inherit a complex and often contradictory historical narrative, were absent from the immediate decision-making process.
% DISAPPEARANCE_RATIONALE: If the practice-doctrine gap vanished overnight (e.g., through a clear, unambiguous doctrinal statement or a full return to prior practice), the institution's historical narrative, current membership understanding, and relationship with fundamentalist offshoots would fundamentally reorganize. It would either fully embrace or fully repudiate its past, with significant consequences for its identity and structure.
% FOUNDING_PROBLEM: The institution faced an existential threat from the United States federal government due to its practice of plural marriage, risking disincorporation, asset seizure, and imprisonment of its leaders.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, federal court documents, and contemporary journalistic accounts from outside the benefiting parties (e.g., non-member historians, government archives) corroborate the severe legal and political pressure that constituted the founding problem. The problem of federal persecution is now dead, but the doctrinal ambiguity persists.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__practice_doctrine_gap, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__practice_doctrine_gap, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__practice_doctrine_gap, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_commitment_reversal__practice_doctrine_gap, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__practice_doctrine_gap, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the ambiguity imposed significant costs on members, forcing them to reconcile contradictory realities. Suppression is moderate-high, as dissent was managed through social pressure and, for fundamentalists, excommunication. Theater ratio is very high (0.85) because the public 'suspension' was largely performative, allowing the continuation of plural marriages in claimed-legal jurisdictions while projecting compliance to the federal government. The period 1890-1904 is chosen as the interval because it marks the initial public declaration (Manifesto) and the subsequent period of continued, albeit hidden, practice before a more definitive 'Second Manifesto' was issued.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of institutional leadership, this was a necessary coordination to ensure survival. From the perspective of the general membership, it was a confusing and costly extraction of their clarity and trust. Fundamentalist factions viewed it as a betrayal of core principles, leading to schism. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional survival (an abstract entity, but a clear beneficiary) gains from the flexibility. The general membership and fundamentalist factions bear the costs of ambiguity and enforcement, placing them at the target end. The federal government acts as an external enforcer, shaping the environment for the institutional leadership, who in turn manage the internal constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to ensure institutional survival in the face of federal persecution. While the immediate threat of disincorporation was mitigated, the persistence of the doctrinal ambiguity beyond the initial crisis (and the continuation of plural marriages in secret) suggests a shift from genuine coordination to a more extractive, performative maintenance of institutional power. The high theater ratio and continued extractiveness on members indicate a Mandatrophy signature, where the original problem (federal persecution) is 'dead' but the solution (ambiguity) persists as a mechanism of control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_integrity_vs_institutional_survival,
    'Was the preservation of Section 132 in doctrine a genuine commitment to principle, or a strategic maneuver to maintain institutional legitimacy and future options?',
    'Analysis of internal communications and private statements from leadership during the period, compared to public declarations. Examination of subsequent doctrinal developments and their consistency with the preserved principle.',
    'If a strategic maneuver, the extractiveness on members (cognitive dissonance) is higher, as it was knowingly imposed. If a genuine commitment, the constraint is a more tragic ''tangled rope'' of necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_integrity_vs_institutional_survival, conceptual, 'Ambiguity regarding the true intent behind preserving doctrine.').

omega_variable(
    extent_of_secret_practice,
    'What was the true extent and nature of plural marriages continued in secret during the 1890-1904 period, and how widely was this known within the general membership?',
    'Access to sealed historical archives, genealogical records, and personal diaries from the period. Oral histories from descendants of those involved.',
    'A higher extent of secret practice, especially if widely known, would increase the measured extractiveness (theater ratio, cognitive dissonance) on the general membership and fundamentalist factions, confirming the performative nature of the public suspension.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extent_of_secret_practice, empirical, 'Uncertainty about the scale of hidden practice and internal awareness.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (excommunication, social ostracization) or internalized (identity-locked members self-censoring dissent)?',
    'Post-exit suppression trajectory of former members: if suppression persists as internalized patterns after leaving the institution, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more insidious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__practice_doctrine_gap, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1890, 0.7).
narrative_ontology:measurement(marr_tr_t1894, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1894, 0.78).
narrative_ontology:measurement(marr_tr_t1898, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1898, 0.82).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1904, 0.85).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1890, 0.6).
narrative_ontology:measurement(marr_be_t1894, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1894, 0.68).
narrative_ontology:measurement(marr_be_t1898, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1898, 0.73).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1904, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1890, 0.5).
narrative_ontology:measurement(marr_su_t1894, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1894, 0.58).
narrative_ontology:measurement(marr_su_t1898, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1898, 0.62).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1904, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__practice_doctrine_gap, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_commitment_reversal' kernel, focusing on the practice-doctrine gap. It is linked to sibling readings that emphasize external coercion or internal reinterpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
