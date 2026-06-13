% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__parliamentary_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__parliamentary_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__parliamentary_supremacy_reading
 *   human_readable: Parliamentary Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory/jurisprudence
 *
 * SUMMARY:
 *   This constraint describes the 'parliamentary supremacy' reading of
 *   constitutional interpretive authority, where the elected legislature
 *   holds final authority to interpret the constitution and its acts are not
 *   subject to judicial nullification. This reading is one of several
 *   competing interpretations of the 'constitutional_interpretive_authority'
 *   kernel. It emphasizes democratic accountability and the legislative
 *   mandate over judicial checks and balances.
 *
 * KEY AGENTS:
 *   - elected_legislature: Primary beneficiary/agenda-setter (institutional/constrained)
 *   - governing_party: Secondary beneficiary (organized/mobile)
 *   - judicial_branch: Primary target/payer (institutional/identity_locked)
 *   - minority_groups: Secondary target/payer (powerless/trapped)
 *   - constitutional_scholars: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.4).
domain_priors:suppression_score(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.6).
domain_priors:theater_ratio(constitutional_interpretive_authority__parliamentary_supremacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__parliamentary_supremacy_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__parliamentary_supremacy_reading, rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__parliamentary_supremacy_reading, "Parliamentary Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_interpretive_authority__parliamentary_supremacy_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__parliamentary_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__parliamentary_supremacy_reading, '294a39e5-7f97-48ce-8c35-7bf736808402').
narrative_ontology:cs_kernel_codification('294a39e5-7f97-48ce-8c35-7bf736808402', formalized).
narrative_ontology:cs_authority_grounding('294a39e5-7f97-48ce-8c35-7bf736808402', lineage).
narrative_ontology:cs_interpretation_layer_present('294a39e5-7f97-48ce-8c35-7bf736808402').
narrative_ontology:cs_reading_relation('294a39e5-7f97-48ce-8c35-7bf736808402', constitutional_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('294a39e5-7f97-48ce-8c35-7bf736808402', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('294a39e5-7f97-48ce-8c35-7bf736808402', foundational, electoral_mandate_is_supreme).
narrative_ontology:cs_axiom_status(electoral_mandate_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('294a39e5-7f97-48ce-8c35-7bf736808402', electoral_mandate_is_supreme, deontological).
narrative_ontology:cs_axiom('294a39e5-7f97-48ce-8c35-7bf736808402', secondary, judicial_review_is_undemocratic).
narrative_ontology:cs_axiom_status(judicial_review_is_undemocratic, holdable).
narrative_ontology:cs_axiom_grounding('294a39e5-7f97-48ce-8c35-7bf736808402', judicial_review_is_undemocratic, conventional).
narrative_ontology:cs_reference_frame('294a39e5-7f97-48ce-8c35-7bf736808402', unfettered_parliamentary_sovereignty).
narrative_ontology:cs_drift_state('294a39e5-7f97-48ce-8c35-7bf736808402', contemporary_human_rights_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('294a39e5-7f97-48ce-8c35-7bf736808402', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__parliamentary_supremacy_reading, governing_party).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, judicial_branch).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__parliamentary_supremacy_reading, minority_groups).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__parliamentary_supremacy_reading, electoral_mandate_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__parliamentary_supremacy_reading, parliamentary_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the ultimate authority to interpret the constitution and enact laws without judicial override. Benefits from broad interpretive discretion and the ability to implement its electoral mandate directly.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, elected_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the legislature's interpretive supremacy, allowing it to enact its policy agenda without judicial checks. Its power is directly tied to its control of the legislative majority.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, governing_party, beneficiary,
    organized, biographical, mobile, national).

% Is constrained to apply laws as interpreted by the legislature, lacking the power of constitutional review. Its role is limited to statutory interpretation and common law development, not voiding parliamentary acts. Identity-locked by its institutional role and tradition.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, judicial_branch, payer,
    institutional, civilizational, identity_locked, national).

% Bear the costs of legislative interpretations that may infringe on their rights, without recourse to judicial protection against parliamentary acts. Their only avenue for redress is through the political process, which often disadvantages them.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, minority_groups, payer,
    powerless, generational, trapped, national).

% Analyze the legal and political implications of parliamentary supremacy, comparing it to other constitutional models. They do not directly participate in the interpretive process but influence public and elite discourse.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures a clear, unified source of constitutional interpretation, preventing inter-branch deadlock and allowing the elected government to implement its mandate efficiently.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority and policy discretion from the judiciary to the elected legislature, and from minority protections to majority will.
% ABSENT_VOICES: Advocates for robust judicial review and fundamental rights would object, arguing that parliamentary supremacy leaves minority rights vulnerable to majoritarianism. They are often marginalized in systems where this reading prevails.
% DISAPPEARANCE_RATIONALE: If parliamentary supremacy in constitutional interpretation vanished, the judicial branch would immediately assert or be granted powers of constitutional review, leading to challenges to existing legislation and a fundamental shift in the balance of power between branches. The entire legal and political system would reorganize.
% FOUNDING_PROBLEM: To establish a clear locus of sovereign power and ensure that the will of the elected representatives, accountable to the people, is supreme in law-making and constitutional interpretation.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists and historical documents attest to the founding problem of establishing democratic sovereignty. Contemporary political discourse, particularly from governing parties, continues to assert the need for legislative authority to implement electoral mandates without undue judicial interference. Opposition parties and civil society groups, however, often contest the extent of this authority, particularly regarding rights protection.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__parliamentary_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__parliamentary_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__parliamentary_supremacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_interpretive_authority__parliamentary_supremacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).
:- end_tests(constitutional_interpretive_authority__parliamentary_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Rope because, from the perspective of its proponents, it provides a clear and efficient mechanism for democratic governance. However, its metrics reflect a degree of extraction (0.4) and suppression (0.6) due to the exclusion of judicial review and the potential for majoritarian overreach. The theater ratio is low (0.1) as the system largely functions as intended by this reading, with little performative maintenance masking a degraded function. Accessibility collapse is moderate (0.7) as judicial avenues for challenging legislative acts are largely closed, but political avenues remain. Resistance is low (0.3) because the system is generally accepted by the majority, though contested by minority groups and some legal scholars.
 *
 * PERSPECTIVAL GAP:
 *   The elected legislature and governing party experience this as a legitimate and efficient coordination mechanism, ensuring their mandate is enacted. The judicial branch and minority groups, however, experience it as a constraint that limits their power and avenues for redress, respectively. The engine's per-seat classification would reflect this divergence, with beneficiaries seeing a Rope-like structure and payers experiencing a more Snare-like or Tangled Rope-like constraint due to the high suppression and limited exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The elected legislature and governing party are clear beneficiaries (d near 0.0) as they gain interpretive discretion and policy freedom. The judicial branch is a target (d near 1.0) as its traditional role of constitutional guardianship is curtailed, and it is identity-locked into this subordinate position. Minority groups are also targets (d near 1.0) as they lack judicial recourse against legislative acts. The constraint subsidizes the legislative process by removing judicial friction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; its function (ensuring legislative supremacy) is actively maintained and contested. The classification as a Rope (claimed) with moderate extraction and high suppression prevents mislabeling it as a pure Mountain (which would ignore the costs to the judiciary and minorities) or a pure Snare (which would ignore the genuine coordination function of clear legislative authority). The contestation around its founding problem status ('live' vs. 'contested') highlights the ongoing debate about whether its original purpose is still served or if it has become primarily extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    parliamentary_supremacy_vs_judicial_review,
    'Is the absence of judicial review of parliamentary acts a structural feature of democratic accountability or a mechanism for majoritarian extraction?',
    'Comparative analysis of constitutional systems with and without judicial review, focusing on long-term outcomes for minority rights and democratic stability. Empirical studies on the impact of judicial review on legislative output and executive power.',
    'If primarily a mechanism for extraction, the constraint''s effective extractiveness for minority groups would be higher, pushing their seat classification towards Snare. If a necessary feature of accountability, the coordination function would be emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_supremacy_vs_judicial_review, conceptual, 'Ambiguity in the normative justification for parliamentary supremacy.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''parliamentary_supremacy_reading'' of the ''constitutional_interpretive_authority'' kernel, or does it conflate elements of other readings?',
    'Detailed textual analysis of foundational legal documents and historical jurisprudence within the specific constitutional system, compared against the definitions of sibling readings (judicial_supremacy_reading, coordinate_construction_reading).',
    'If conflated, the structural properties (e.g., extractiveness, suppression, beneficiary/victim sets) might be misattributed, leading to an inaccurate classification. A clearer distinction would sharpen the analysis of each reading''s unique structural impact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ensuring precise identification of this specific kernel reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__parliamentary_supremacy_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1900, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(cons_tr_t1930, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 1930, 0.08).
narrative_ontology:measurement(cons_tr_t1960, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 1960, 0.09).
narrative_ontology:measurement(cons_tr_t1990, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(cons_tr_t2024, constitutional_interpretive_authority__parliamentary_supremacy_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t1900, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(cons_be_t1930, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 1930, 0.35).
narrative_ontology:measurement(cons_be_t1960, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 1960, 0.38).
narrative_ontology:measurement(cons_be_t1990, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(cons_be_t2024, constitutional_interpretive_authority__parliamentary_supremacy_reading, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1900, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(cons_su_t1930, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 1930, 0.55).
narrative_ontology:measurement(cons_su_t1960, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 1960, 0.58).
narrative_ontology:measurement(cons_su_t1990, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(cons_su_t2024, constitutional_interpretive_authority__parliamentary_supremacy_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__parliamentary_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__parliamentary_supremacy_reading, constitutional_interpretive_authority__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'constitutional_interpretive_authority' kernel. Each reading represents a different structural arrangement of interpretive power, with distinct beneficiaries, victims, and metric profiles. They are linked here to reflect their shared origin in a fundamental constitutional question.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
