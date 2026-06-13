% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__consensus_safeguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__consensus_safeguard_reading, []).

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
 *   constraint_id: supermajority_threshold__consensus_safeguard_reading
 *   human_readable: Supermajority Threshold as Consensus Safeguard
 *   domain: constitutional_theory/political_economy/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the 'consensus safeguard' reading of
 *   the supermajority threshold kernel. In this reading, the supermajority
 *   requirement for constitutional amendment is understood as a structural
 *   feature of democratic governance, designed to ensure that fundamental
 *   changes reflect a deep, persistent societal consensus rather than
 *   transient majoritarian impulses. It is presented as a 'mountain' because
 *   its function is seen as an irreducible requirement for constitutional
 *   stability and deliberative democracy, rather than a human-constructed
 *   mechanism for extraction. The beneficiaries are diffuse (future
 *   generations, constitutional continuity) and the 'cost' is borne by
 *   current majorities in the form of reduced legislative agility, which is
 *   framed as a necessary feature of high-quality democracy.
 *
 * KEY AGENTS:
 *   - future_generations: Primary beneficiary (powerless/trapped) — benefits from stability
 *   - constitutional_continuity: Abstract beneficiary (analytical/analytical) — represents the principle of enduring order
 *   - current_majority_coalition: Primary payer (powerful/constrained) — bears the cost of needing broad consensus
 *   - minority_factions: Secondary beneficiary (moderate/constrained) — protected from majoritarian overreach
 *   - constitutional_scholars: Analytical observer (analytical/analytical) — studies the constraint's effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__consensus_safeguard_reading, 0.15).
domain_priors:suppression_score(supermajority_threshold__consensus_safeguard_reading, 0.2).
domain_priors:theater_ratio(supermajority_threshold__consensus_safeguard_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__consensus_safeguard_reading, mountain).
narrative_ontology:human_readable(supermajority_threshold__consensus_safeguard_reading, "Supermajority Threshold as Consensus Safeguard").
narrative_ontology:topic_domain(supermajority_threshold__consensus_safeguard_reading, "constitutional_theory/political_economy/institutional_design").

domain_priors:emerges_naturally(supermajority_threshold__consensus_safeguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__consensus_safeguard_reading, '97b36568-8b06-49bb-a2e7-132839355ac0').
narrative_ontology:cs_kernel_codification('97b36568-8b06-49bb-a2e7-132839355ac0', fixed_text).
narrative_ontology:cs_authority_grounding('97b36568-8b06-49bb-a2e7-132839355ac0', lineage).
narrative_ontology:cs_interpretation_layer_present('97b36568-8b06-49bb-a2e7-132839355ac0').
narrative_ontology:cs_reading_relation('97b36568-8b06-49bb-a2e7-132839355ac0', supermajority_threshold__minoritarian_veto_reading, coexists_with).
narrative_ontology:cs_reading_relation('97b36568-8b06-49bb-a2e7-132839355ac0', supermajority_threshold__adaptive_gradient_reading, coexists_with).
narrative_ontology:cs_axiom('97b36568-8b06-49bb-a2e7-132839355ac0', foundational, constitutional_stability_is_paramount).
narrative_ontology:cs_axiom_status(constitutional_stability_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('97b36568-8b06-49bb-a2e7-132839355ac0', constitutional_stability_is_paramount, deontological).
narrative_ontology:cs_axiom('97b36568-8b06-49bb-a2e7-132839355ac0', foundational, deliberative_consensus_is_superior_to_simple_majority).
narrative_ontology:cs_axiom_status(deliberative_consensus_is_superior_to_simple_majority, holdable).
narrative_ontology:cs_axiom_grounding('97b36568-8b06-49bb-a2e7-132839355ac0', deliberative_consensus_is_superior_to_simple_majority, deontological).
narrative_ontology:cs_reference_frame('97b36568-8b06-49bb-a2e7-132839355ac0', founding_era_deliberative_republic).
narrative_ontology:cs_drift_state('97b36568-8b06-49bb-a2e7-132839355ac0', contemporary_polarized_politics, gap(stable, minor, false)).
narrative_ontology:cs_created_at('97b36568-8b06-49bb-a2e7-132839355ac0', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, future_generations).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, constitutional_continuity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, minority_factions).
narrative_ontology:constraint_victim(supermajority_threshold__consensus_safeguard_reading, current_majority_coalition).
narrative_ontology:constraint_vindicates(supermajority_threshold__consensus_safeguard_reading, deliberative_democracy_theory).
narrative_ontology:constraint_vindicates(supermajority_threshold__consensus_safeguard_reading, constitutional_stability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a stable constitutional framework that is not easily altered by short-term political whims, ensuring long-term predictability and protection of fundamental rights. They have no direct voice in the present amendment process.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, future_generations, beneficiary,
    powerless, generational, trapped, national).

% The abstract principle of an enduring constitutional order, which is strengthened by the high bar for amendment. This ensures the foundational principles of governance remain consistent over long periods.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, constitutional_continuity, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(supermajority_threshold__consensus_safeguard_reading, constitutional_continuity).

% Bears the cost of needing to build broad, cross-partisan support to enact desired constitutional changes. Their policy preferences may be blocked by a minority, requiring extensive negotiation or deferral.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, current_majority_coalition, payer,
    powerful, immediate, constrained, national).

% Benefit from the protection against transient majoritarianism, as their interests and rights are less susceptible to being overridden by simple majorities. They gain leverage in constitutional debates.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, minority_factions, beneficiary,
    moderate, biographical, constrained, national).

% Analyze the effects of the supermajority rule on constitutional evolution, democratic legitimacy, and stability. They provide academic commentary and critique on its operation.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that fundamental changes to the constitutional framework are the product of broad, enduring societal consensus, coordinating long-term political stability and protecting against impulsive alterations.
% TRANSFER_FUNCTION: Transfers the power to make swift constitutional changes from simple majorities to a broader, more deliberative consensus, effectively transferring a portion of legislative agility for increased stability and legitimacy.
% ABSENT_VOICES: Future generations, who are the primary beneficiaries of constitutional stability, have no direct voice in the current process. Their interests are represented by the structural barrier itself, rather than active participation.
% DISAPPEARANCE_RATIONALE: If the supermajority threshold vanished, constitutional amendments would become much easier, leading to more frequent and potentially partisan changes. This would fundamentally alter the stability and perceived legitimacy of the constitutional order, requiring a complete re-evaluation of governance principles.
% FOUNDING_PROBLEM: The risk of constitutional instability and the tyranny of transient majorities, where fundamental laws could be altered too easily, undermining long-term governance and minority rights.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and political theorists widely corroborate the ongoing risk of majoritarian overreach and the need for mechanisms to ensure deliberative, long-term consensus. Historical examples of constitutional instability in other nations also provide corroboration from outside the immediate political beneficiaries.
narrative_ontology:disappearance_verdict(supermajority_threshold__consensus_safeguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__consensus_safeguard_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__consensus_safeguard_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(supermajority_threshold__consensus_safeguard_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__consensus_safeguard_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, ExtMetricName, E),
    domain_priors:suppression_score(supermajority_threshold__consensus_safeguard_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(supermajority_threshold__consensus_safeguard_reading),
    narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(supermajority_threshold__consensus_safeguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because the 'cost' of the supermajority is framed as a necessary investment in democratic quality and stability, not as a rent. Suppression is low (0.20) because the constraint is accepted as a legitimate feature of the system, not actively resisted. Theater ratio is very low (0.05) as its function is genuinely structural and not performative. Accessibility collapse is high (0.80) because, from this reading, there are no legitimate alternatives to a high amendment bar for achieving deep constitutional consensus. Resistance is low (0.10) because the principle is widely accepted within this interpretive tradition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'consensus safeguard' reading, the supermajority threshold is a beneficial, almost natural, feature of a well-ordered democracy. The 'cost' borne by the current majority coalition is seen as a feature, not a bug, ensuring deliberative outcomes. Other readings (e.g., 'minoritarian veto') would frame this 'cost' as illegitimate extraction, leading to a significant divergence in perceived constraint type.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and the abstract concept of constitutional continuity are the primary beneficiaries, as they gain from long-term stability. Current majority coalitions are the 'payers' in that they must expend more effort to achieve constitutional change. Minority factions also benefit from protection. The directionality for the current majority is thus slightly higher (more target-like) than for the beneficiaries, but still far from full extraction, as the 'cost' is seen as legitimate.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the constraint as a Snare or Tangled Rope by emphasizing its foundational role in democratic quality. The 'mandate' of ensuring deep consensus is considered perpetually 'live' and essential, thus preventing mandatrophy. The low extractiveness and suppression metrics, combined with the 'emerges_naturally: true' flag, align with a Mountain classification, reflecting its perceived fundamental nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_choice,
    'Is the supermajority threshold a ''natural law'' of stable constitutionalism, or a ''constructed choice'' that could be legitimately altered?',
    'Comparative constitutional analysis across diverse democratic systems, examining the relationship between amendment difficulty and constitutional stability/legitimacy over long historical periods. If systems with lower thresholds consistently achieve similar stability and legitimacy, it suggests a constructed choice.',
    'If a constructed choice, the ''mountain'' classification would be challenged, potentially reclassifying it as a Rope or Tangled Rope, depending on the beneficiary structure and enforcement mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_choice, conceptual, 'Ambiguity between inherent constitutional principle and policy choice.').

omega_variable(
    consensus_vs_veto_ambiguity,
    'Does the supermajority threshold primarily foster genuine consensus, or does it primarily empower a minority veto to entrench the status quo?',
    'Empirical analysis of amendment attempts: track the nature of blocked amendments, the size and composition of blocking minorities, and the subsequent policy outcomes. If blocked amendments consistently represent broad public will, it leans towards a veto; if they represent transient passion, it leans towards consensus.',
    'If primarily a veto, the constraint''s extractiveness and suppression would be higher, and its classification would shift towards a Tangled Rope or Snare, reflecting the costs imposed on the majority by the minority''s blocking power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_vs_veto_ambiguity, empirical, 'Distinguishing consensus-building from minority entrenchment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__consensus_safeguard_reading, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t1787, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 1787, 0.05).
narrative_ontology:measurement(supe_tr_t1850, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 1850, 0.05).
narrative_ontology:measurement(supe_tr_t1900, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(supe_tr_t1950, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(supe_tr_t2000, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(supe_tr_t2024, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(supe_be_t1787, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 1787, 0.1).
narrative_ontology:measurement(supe_be_t1850, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 1850, 0.12).
narrative_ontology:measurement(supe_be_t1900, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 1900, 0.13).
narrative_ontology:measurement(supe_be_t1950, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 1950, 0.14).
narrative_ontology:measurement(supe_be_t2000, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(supe_be_t2024, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t1787, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 1787, 0.15).
narrative_ontology:measurement(supe_su_t1850, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 1850, 0.18).
narrative_ontology:measurement(supe_su_t1900, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 1900, 0.19).
narrative_ontology:measurement(supe_su_t1950, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 1950, 0.2).
narrative_ontology:measurement(supe_su_t2000, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(supe_su_t2024, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
