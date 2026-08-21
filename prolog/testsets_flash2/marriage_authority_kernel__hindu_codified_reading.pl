% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__hindu_codified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__hindu_codified_reading, []).

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
 *   constraint_id: marriage_authority_kernel__hindu_codified_reading
 *   human_readable: Hindu Marriage Act 1955 Authority (Civil Court Interpretation)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This constraint describes the authority of the Hindu Marriage Act 1955,
 *   as interpreted and enforced by Indian civil courts, as one reading of the
 *   broader 'marriage_authority_kernel'. It provides a codified legal
 *   framework for Hindu family matters, aiming for uniformity and gender
 *   equity within the Hindu community. While it significantly reformed
 *   traditional practices, it maintains a distinct religious identity and
 *   faces ongoing challenges regarding full gender parity and its
 *   relationship to a potential uniform civil code.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, 0.45).
domain_priors:suppression_score(marriage_authority_kernel__hindu_codified_reading, 0.6).
domain_priors:theater_ratio(marriage_authority_kernel__hindu_codified_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__hindu_codified_reading, "Hindu Marriage Act 1955 Authority (Civil Court Interpretation)").
narrative_ontology:topic_domain(marriage_authority_kernel__hindu_codified_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__hindu_codified_reading, 'ca16986d-d1cf-4cd7-98a9-328248c7fd82').
narrative_ontology:cs_kernel_codification('ca16986d-d1cf-4cd7-98a9-328248c7fd82', formalized).
narrative_ontology:cs_authority_grounding('ca16986d-d1cf-4cd7-98a9-328248c7fd82', lineage).
narrative_ontology:cs_interpretation_layer_present('ca16986d-d1cf-4cd7-98a9-328248c7fd82').
narrative_ontology:cs_reading_relation('ca16986d-d1cf-4cd7-98a9-328248c7fd82', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca16986d-d1cf-4cd7-98a9-328248c7fd82', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca16986d-d1cf-4cd7-98a9-328248c7fd82', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca16986d-d1cf-4cd7-98a9-328248c7fd82', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('ca16986d-d1cf-4cd7-98a9-328248c7fd82', foundational, hindu_personal_law_distinct_and_codifiable).
narrative_ontology:cs_axiom_status(hindu_personal_law_distinct_and_codifiable, holdable).
narrative_ontology:cs_axiom_grounding('ca16986d-d1cf-4cd7-98a9-328248c7fd82', hindu_personal_law_distinct_and_codifiable, conventional).
narrative_ontology:cs_axiom('ca16986d-d1cf-4cd7-98a9-328248c7fd82', foundational, state_courts_as_final_interpreters).
narrative_ontology:cs_axiom_status(state_courts_as_final_interpreters, holdable).
narrative_ontology:cs_axiom_grounding('ca16986d-d1cf-4cd7-98a9-328248c7fd82', state_courts_as_final_interpreters, conventional).
narrative_ontology:cs_reference_frame('ca16986d-d1cf-4cd7-98a9-328248c7fd82', post_independence_codified_hindu_law).
narrative_ontology:cs_drift_state('ca16986d-d1cf-4cd7-98a9-328248c7fd82', contemporary_constitutional_scrutiny, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ca16986d-d1cf-4cd7-98a9-328248c7fd82', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_community_members).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, civil_courts).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_women_seeking_equal_rights).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, interfaith_couples).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a codified, relatively uniform legal framework for marriage and family matters that largely respects traditional practices while being enforceable by the state. Provides legal clarity and social stability within the community.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_community_members, beneficiary,
    organized, generational, constrained, national).

% Interpret and enforce the Hindu Marriage Act 1955, providing a formal, state-backed dispute resolution mechanism. Their interpretations shape the practical application of the law, balancing tradition with constitutional principles.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, civil_courts, agenda_setter,
    institutional, generational, analytical, national).

% While the HMA 1955 introduced significant reforms towards gender equality, certain provisions and judicial interpretations still fall short of full parity with secular law, particularly regarding property rights and maintenance. They bear the costs of these remaining inequalities.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_women_seeking_equal_rights, payer,
    powerless, biographical, constrained, local).

% Cannot marry under the Hindu Marriage Act if one partner is not Hindu. They must either convert or marry under the Special Marriage Act, which can involve social stigma and procedural hurdles. They are excluded from the HMA's framework.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, interfaith_couples, excluded,
    powerless, biographical, constrained, local).

% Advocate for a uniform civil code, arguing that personal laws based on religion perpetuate inequality and undermine national unity. They analyze the HMA's impact and push for reforms or its eventual replacement by a secular code.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, secular_law_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, state-enforceable legal framework for marriage, divorce, and family matters for the Hindu community, replacing diverse customary laws and reducing legal uncertainty.
% TRANSFER_FUNCTION: Transfers legal authority over Hindu family matters from purely religious or customary bodies to civil courts, and in some areas, transfers rights and protections to Hindu women that were previously denied under uncodified custom.
% ABSENT_VOICES: Interfaith couples and those advocating for a fully secular, gender-neutral family law are marginalized within the HMA framework; they would argue for universal applicability of civil law and removal of religious distinctions in legal status.
% DISAPPEARANCE_RATIONALE: If the Hindu Marriage Act and its enforcement vanished, Hindu family law would revert to diverse, often conflicting customary practices, creating legal chaos and uncertainty for millions. Civil courts would lose their jurisdiction over these matters, and the legal landscape for Hindu families would fundamentally reorganize.
% FOUNDING_PROBLEM: Prior to 1955, Hindu personal law was a complex, uncodified mix of Shastric law and diverse regional customs, leading to legal uncertainty, gender inequality, and difficulty in state administration of justice.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, civil society organizations, and judicial pronouncements from outside the immediate beneficiaries attest that the HMA successfully addressed much of the prior chaos and inequality, though ongoing debates about gender parity and the need for a uniform civil code indicate the problem's evolving nature.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__hindu_codified_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__hindu_codified_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__hindu_codified_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority_kernel__hindu_codified_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__hindu_codified_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__hindu_codified_reading_tests).
:- end_tests(marriage_authority_kernel__hindu_codified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The HMA 1955 is classified as a Tangled Rope because it genuinely coordinates (provides a uniform legal framework for Hindus) but also involves asymmetric extraction. While it benefits Hindu community members by providing legal clarity and state enforcement, it extracts from Hindu women (due to remaining inequalities) and excludes interfaith couples. Active enforcement by civil courts is required to maintain its provisions and boundaries. Extractiveness has decreased over time due to progressive judicial interpretations, but suppression remains moderate as it actively maintains its distinct religious boundary.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hindu community members, the HMA is a beneficial coordination mechanism that respects their cultural identity. From the perspective of Hindu women seeking full equality or interfaith couples, it represents a constraint that perpetuates inequality or exclusion. Civil courts, as agenda-setters, navigate these tensions through their interpretations.
 *
 * DIRECTIONALITY LOGIC:
 *   Hindu community members and civil courts are beneficiaries (low d) as they gain from the stability and authority of the codified law. Hindu women seeking equal rights and interfaith couples are targets (high d) as they bear the costs of its limitations or exclusions. Secular law advocates are observers (analytical d).
 *
 * MANDATROPHY ANALYSIS:
 *   The HMA's mandate to provide a uniform and equitable law for Hindus is still live, preventing mislabeling as a Piton. However, the 'contested' status of its founding problem (whether it fully addresses contemporary needs) and the decreasing extractiveness over time suggest a dynamic tension between its coordination function and its remaining extractive elements. The classification as Tangled Rope captures this ongoing balance, preventing it from being mislabeled as a pure Rope (ignoring extraction) or a Snare (ignoring coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gender_equity_gap,
    'To what extent do current judicial interpretations of the HMA 1955 achieve full gender equality compared to a secular civil code?',
    'Comparative legal analysis of HMA case law against Special Marriage Act case law, focusing on property rights, maintenance, and divorce grounds.',
    'If a significant gap persists, the HMA''s extractiveness for Hindu women is higher than currently assessed, potentially pushing the constraint closer to a Snare for that seat. If parity is largely achieved, extractiveness for women decreases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_equity_gap, empirical, 'Assesses the actual gender equity achieved by the HMA in practice.').

omega_variable(
    uniform_civil_code_feasibility,
    'Is a uniform civil code (UCC) a politically and socially feasible alternative that would genuinely resolve inequalities without creating new forms of suppression?',
    'Sociological studies on community acceptance, political analysis of legislative pathways, and comparative legal studies of UCC implementation models in other pluralistic societies.',
    'If a UCC is feasible and genuinely equitable, the HMA''s suppression (by maintaining distinct religious identity) would be re-evaluated as more extractive. If a UCC is infeasible or creates new suppressions, the HMA''s coordination function is more salient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uniform_civil_code_feasibility, preference, 'Examines the viability and desirability of a uniform civil code as an alternative.').

omega_variable(
    hindu_codified_vs_secular_framing,
    'Is the distinction between ''Hindu codified law'' and ''secular civil code'' a genuine structural difference in legal authority, or primarily a difference in social framing?',
    'Analysis of judicial reasoning: do courts apply distinct interpretive principles for HMA vs. SMA, or do they converge on similar constitutional principles despite different textual origins?',
    'If the difference is primarily framing, the ''hindu_codified_reading'' and ''secular_civil_reading'' are more closely related than currently modeled, potentially influencing their network effects. If structural, the distinct classifications are robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hindu_codified_vs_secular_framing, conceptual, 'Examines the conceptual distinction between religious and secular legal authority in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__hindu_codified_reading, 1955, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1955, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1955, 0.2).
narrative_ontology:measurement(marr_tr_t1975, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(marr_tr_t1995, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(marr_tr_t2010, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t1955, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1955, 0.6).
narrative_ontology:measurement(marr_be_t1975, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(marr_be_t1995, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement(marr_be_t2010, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2010, 0.48).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1955, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1955, 0.7).
narrative_ontology:measurement(marr_su_t1975, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1975, 0.65).
narrative_ontology:measurement(marr_su_t1995, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1995, 0.62).
narrative_ontology:measurement(marr_su_t2010, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__hindu_codified_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority_kernel' family. Its ε value reflects the specific structural properties of the Hindu Marriage Act 1955 as interpreted by civil courts, which differs from other religious or secular legal frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
