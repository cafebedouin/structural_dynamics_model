% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__parliamentary_sovereignty_reading
 *   human_readable: Parliamentary Sovereignty in Constitutional Interpretation
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint describes the 'parliamentary sovereignty' reading of
 *   basic law interpretive authority, where the elected legislature holds
 *   final interpretive power. This reading emphasizes democratic mandate and
 *   representative accountability. It is one of three competing readings of
 *   the 'basic_law_interpretive_authority' kernel, alongside
 *   'judicial_supremacy_reading' and 'popular_constitutionalism_reading'. The
 *   structural delta for this reading includes the legislature as a primary
 *   beneficiary, while the judicial branch and rights minorities are victims,
 *   bearing the costs of legislative override and potential gridlock.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.65).
domain_priors:suppression_score(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.7).
domain_priors:theater_ratio(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "Parliamentary Sovereignty in Constitutional Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "constitutional_law/political_theory/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__parliamentary_sovereignty_reading, '58851085-2df6-4a83-ad9a-423d391e6351').
narrative_ontology:cs_kernel_codification('58851085-2df6-4a83-ad9a-423d391e6351', formalized).
narrative_ontology:cs_authority_grounding('58851085-2df6-4a83-ad9a-423d391e6351', lineage).
narrative_ontology:cs_interpretation_layer_present('58851085-2df6-4a83-ad9a-423d391e6351').
narrative_ontology:cs_reading_relation('58851085-2df6-4a83-ad9a-423d391e6351', basic_law_interpretive_authority__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('58851085-2df6-4a83-ad9a-423d391e6351', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('58851085-2df6-4a83-ad9a-423d391e6351', foundational, legislative_supremacy_by_mandate).
narrative_ontology:cs_axiom_status(legislative_supremacy_by_mandate, holdable).
narrative_ontology:cs_axiom_grounding('58851085-2df6-4a83-ad9a-423d391e6351', legislative_supremacy_by_mandate, deontological).
narrative_ontology:cs_axiom('58851085-2df6-4a83-ad9a-423d391e6351', secondary, accountability_through_elections).
narrative_ontology:cs_axiom_status(accountability_through_elections, holdable).
narrative_ontology:cs_axiom_grounding('58851085-2df6-4a83-ad9a-423d391e6351', accountability_through_elections, conventional).
narrative_ontology:cs_reference_frame('58851085-2df6-4a83-ad9a-423d391e6351', westminster_parliamentary_tradition).
narrative_ontology:cs_drift_state('58851085-2df6-4a83-ad9a-423d391e6351', contemporary_rights_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('58851085-2df6-4a83-ad9a-423d391e6351', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, majority_constituency).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_branch).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the democratically elected body, it asserts and exercises final authority in interpreting the basic law, including overriding judicial interpretations. Benefits from the ability to enact policy reflecting the popular will without judicial veto.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the legislature's ability to implement policies that reflect their preferences, as the legislature is accountable to them through elections. Their interests are directly represented and enacted.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, majority_constituency, beneficiary,
    organized, biographical, mobile, national).

% Its interpretations of the basic law can be overridden by the legislature, diminishing its institutional authority and independence. Bears the cost of having its legal expertise and constitutional guardianship subordinated to political will.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_branch, payer,
    institutional, civilizational, constrained, national).

% Vulnerable to legislative interpretations that may infringe upon their rights, as the legislature is primarily accountable to the majority. Lacks effective recourse when their protections are legislatively curtailed.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities, payer,
    powerless, generational, trapped, national).

% Analyze the implications of parliamentary sovereignty for constitutional stability, rights protection, and democratic theory. Their work informs public debate but does not directly alter the constraint.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that the final say on constitutional meaning rests with the body most directly accountable to the electorate, coordinating policy with popular mandate and preventing judicial overreach.
% TRANSFER_FUNCTION: Transfers ultimate interpretive power over constitutional meaning from the judiciary to the legislature, and potentially transfers rights protections from minorities to the majority's will.
% ABSENT_VOICES: Advocates for strong judicial review and entrenched minority rights, who would argue for judicial independence and counter-majoritarian protections, are structurally marginalized when legislative supremacy is asserted.
% DISAPPEARANCE_RATIONALE: If parliamentary sovereignty in constitutional interpretation vanished, the judicial branch would likely assert greater interpretive authority, leading to a shift in the balance of power, potentially more robust minority rights protections, and increased judicial activism. The entire institutional framework would rebalance.
% FOUNDING_PROBLEM: To ensure that the will of the people, expressed through their elected representatives, is supreme in governance, including the interpretation of fundamental law, preventing unelected bodies from thwarting democratic decisions.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of parliamentary sovereignty (e.g., political theorists, some legislative bodies) attest that the problem of democratic accountability in constitutional interpretation remains live. Critics (e.g., judicial advocates, human rights organizations) acknowledge the historical problem but argue that the current interpretation leads to new problems of minority protection.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__parliamentary_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (aligning law with democratic will) but involves significant asymmetric extraction. Extractiveness (0.65) is high due to the potential for legislative majorities to impose interpretations that disadvantage minorities or undermine judicial independence. Suppression (0.70) is also high, as it requires active political enforcement to maintain legislative supremacy over judicial challenges. The theater ratio (0.20) is relatively low, indicating that the legislative function is largely genuine, though some performativity may exist in framing overrides as purely democratic rather than power plays.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the elected legislature and majority constituency, this constraint is a legitimate expression of democratic will and a necessary coordination mechanism. From the perspective of the judicial branch and rights minorities, it represents an extractive mechanism that undermines checks and balances and endangers fundamental rights.
 *
 * DIRECTIONALITY LOGIC:
 *   The elected_legislature and majority_constituency are clear beneficiaries, as this reading empowers them. The judicial_branch and rights_minorities are victims, as their interpretive authority or protections can be overridden. The directionality for the legislature would be low (beneficiary), while for the judiciary and minorities it would be high (target).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_mandate_vs_tyranny_of_majority,
    'At what point does ''democratic mandate'' in constitutional interpretation transition into ''tyranny of the majority'' for rights minorities?',
    'Empirical analysis of legislative overrides on judicial decisions concerning minority rights, coupled with a normative framework for assessing fundamental rights protection.',
    'If the threshold for ''tyranny of the majority'' is frequently crossed, the effective extractiveness and suppression for rights minorities would be reclassified as higher, potentially shifting the constraint towards a Snare for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_mandate_vs_tyranny_of_majority, conceptual, 'Ambiguity in the balance between democratic will and minority protection.').

omega_variable(
    gridlock_cost_attribution,
    'How are the costs of institutional gridlock, arising from legislative-judicial interpretive disputes, distributed among stakeholders?',
    'Economic and political science studies analyzing the impact of interpretive conflicts on policy implementation, public trust, and institutional efficiency.',
    'If gridlock costs are disproportionately borne by the judicial branch or specific segments of the populace, the effective extraction for those seats would be higher than currently estimated, potentially altering their per-seat classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gridlock_cost_attribution, empirical, 'Uncertainty in the distribution of costs from interpretive conflicts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(basi_tr_t50, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(basi_be_t50, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(basi_su_t50, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'basic_law_interpretive_authority' kernel. Its structural properties differ significantly from sibling readings, necessitating separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
