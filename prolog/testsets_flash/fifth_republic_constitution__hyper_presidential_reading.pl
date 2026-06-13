% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__hyper_presidential_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__hyper_presidential_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fifth_republic_constitution__hyper_presidential_reading
 *   human_readable: Fifth Republic Hyper-Presidential Reading
 *   domain: constitutional_law/political_systems/comparative_government
 *
 * SUMMARY:
 *   This constraint describes the 'hyper-presidential' reading of the French
 *   Fifth Republic Constitution, where the President is seen as the direct
 *   embodiment of the national will, minimally constrained by the
 *   legislature. This interpretation emphasizes strong executive power,
 *   particularly through mechanisms like Article 49.3 (allowing bills to pass
 *   without a vote) and Article 16 (emergency powers). The legislature,
 *   especially opposition parties, often finds its role diminished, becoming
 *   a target of executive extraction and suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, 0.85).
domain_priors:suppression_score(fifth_republic_constitution__hyper_presidential_reading, 0.75).
domain_priors:theater_ratio(fifth_republic_constitution__hyper_presidential_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__hyper_presidential_reading, snare).
narrative_ontology:human_readable(fifth_republic_constitution__hyper_presidential_reading, "Fifth Republic Hyper-Presidential Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__hyper_presidential_reading, "constitutional_law/political_systems/comparative_government").

domain_priors:requires_active_enforcement(fifth_republic_constitution__hyper_presidential_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__hyper_presidential_reading, '46b20726-562d-4534-a3d1-78c53806f314').
narrative_ontology:cs_kernel_codification('46b20726-562d-4534-a3d1-78c53806f314', fixed_text).
narrative_ontology:cs_authority_grounding('46b20726-562d-4534-a3d1-78c53806f314', lineage).
narrative_ontology:cs_interpretation_layer_present('46b20726-562d-4534-a3d1-78c53806f314').
narrative_ontology:cs_reading_relation('46b20726-562d-4534-a3d1-78c53806f314', fifth_republic_constitution__parliamentary_constraint_reading, influences).
narrative_ontology:cs_reading_relation('46b20726-562d-4534-a3d1-78c53806f314', fifth_republic_constitution__cohabitation_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('46b20726-562d-4534-a3d1-78c53806f314', foundational, president_embodies_national_will).
narrative_ontology:cs_axiom_status(president_embodies_national_will, holdable).
narrative_ontology:cs_axiom_grounding('46b20726-562d-4534-a3d1-78c53806f314', president_embodies_national_will, deontological).
narrative_ontology:cs_axiom('46b20726-562d-4534-a3d1-78c53806f314', foundational, executive_efficiency_trumps_legislative_obstruction).
narrative_ontology:cs_axiom_status(executive_efficiency_trumps_legislative_obstruction, holdable).
narrative_ontology:cs_axiom_grounding('46b20726-562d-4534-a3d1-78c53806f314', executive_efficiency_trumps_legislative_obstruction, instrumental).
narrative_ontology:cs_reference_frame('46b20726-562d-4534-a3d1-78c53806f314', de_gaulle_founding_vision).
narrative_ontology:cs_drift_state('46b20726-562d-4534-a3d1-78c53806f314', contemporary_political_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('46b20726-562d-4534-a3d1-78c53806f314', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, national_assembly).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, opposition_parties).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, citizens_seeking_legislative_accountability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The current holder of the presidential office, who directly benefits from the expansive powers granted by this reading, enabling swift policy implementation and minimal legislative obstruction. They actively utilize constitutional mechanisms to assert executive dominance.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, agenda_setter,
    institutional, biographical, mobile, national).

% The enduring institutional structure of the French presidency, which accrues power and prestige under this reading, shaping future political norms and expectations for executive authority. Its identity is fused with the strong executive model.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, presidency_as_institution, beneficiary,
    institutional, generational, identity_locked, national).

% The legislative body whose power to initiate, amend, and block legislation is significantly curtailed by the hyper-presidential interpretation, particularly through the use of Article 49.3 and the threat of dissolution. Its influence is often reactive rather than proactive.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, national_assembly, payer,
    organized, biographical, constrained, national).

% Political parties not aligned with the president, who bear the brunt of executive dominance, finding their legislative initiatives blocked and their oversight functions weakened. Their primary recourse is public protest and electoral challenge, not direct legislative power.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, opposition_parties, payer,
    moderate, immediate, constrained, national).

% The segment of the populace that values strong legislative oversight and accountability, and finds their democratic representation diminished when the executive bypasses parliamentary processes. Their options for redress are limited to protest or electoral change, which are often insufficient to alter the structural power imbalance.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, citizens_seeking_legislative_accountability, payer,
    powerless, biographical, trapped, national).

% The body responsible for reviewing the constitutionality of laws and executive actions. While it can check presidential power, its interpretations are themselves part of the ongoing contest over the constitution's meaning, and it often operates within the established political equilibrium.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide stable and decisive executive leadership for the nation, ensuring governmental action and policy implementation even in times of legislative fragmentation or gridlock.
% TRANSFER_FUNCTION: Transfers significant legislative and policy-making authority from the National Assembly to the President, enabling the executive to drive the national agenda with minimal parliamentary obstruction.
% ABSENT_VOICES: Stronger parliamentary advocates and proponents of a more balanced separation of powers are present in public discourse but are structurally marginalized within the operational framework of this hyper-presidential reading. Their arguments for increased legislative power are often overridden by executive prerogatives.
% DISAPPEARANCE_RATIONALE: If this hyper-presidential reading vanished, the French political system would undergo a fundamental rebalancing. The National Assembly would reclaim significant legislative power, requiring new forms of coalition-building and negotiation. The executive's ability to act unilaterally would be severely curtailed, leading to a more parliamentary-centric system.
% FOUNDING_PROBLEM: The instability and perceived ineffectiveness of the Fourth Republic's parliamentary system, characterized by frequent changes in government and an inability to address pressing national issues effectively.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of the hyper-presidential reading (often within the executive and its supporters) argue that the problem of governmental instability remains live, justifying strong presidential powers. Critics (opposition parties, some constitutional scholars) contend that the problem is largely solved, and the current system has overcorrected, leading to an imbalance of power. Independent historical analysis and comparative political science studies offer corroboration for both the initial problem and the subsequent shift in power dynamics.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__hyper_presidential_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__hyper_presidential_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__hyper_presidential_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fifth_republic_constitution__hyper_presidential_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the executive's ability to implement policy and consolidate power at the expense of legislative influence and accountability. Suppression (0.75) is high due to constitutional mechanisms that limit legislative checks and balances, such as the ability to dissolve the National Assembly or bypass votes. The low theater ratio (0.20) indicates that the executive's actions are largely functional in asserting power, with minimal performative maintenance of a defunct coordination function. The increasing extractiveness and suppression over time reflect a historical trend towards a stronger presidency in practice.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the incumbent president and the presidency as an institution, this reading represents an efficient and necessary coordination mechanism for national governance. From the perspective of the National Assembly and opposition parties, it is a highly extractive and suppressive mechanism that undermines democratic principles and legislative oversight.
 *
 * DIRECTIONALITY LOGIC:
 *   The incumbent_president and presidency_as_institution are clear beneficiaries (d near 0.0) as they directly gain power and policy control. The national_assembly and opposition_parties are primary victims (d near 1.0) as their legislative power is curtailed. Citizens_seeking_legislative_accountability are also victims, as their avenue for democratic input is weakened. The system is actively enforced by the executive's constitutional prerogatives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is claimed as a Snare, reflecting the view that its coordination story (efficient governance) is largely a cover for executive power consolidation. The high extractiveness and suppression, coupled with identifiable victims, prevent it from being mislabeled as a Rope or Tangled Rope, where a genuine, balanced coordination function would be present. The 'founding_problem_status' being 'contested' further highlights the ongoing debate about whether the original intent of the Fifth Republic's strong executive has atrophied into pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    presidential_sovereignty_vs_parliamentary_supremacy,
    'Is the President''s authority derived directly from the national will, or is it ultimately constrained by parliamentary sovereignty?',
    'Constitutional court rulings on the limits of presidential decrees and emergency powers, particularly in periods of political crisis or legislative gridlock.',
    'If direct national will is affirmed, the constraint operates as a Snare, with high extraction from the legislature. If parliamentary supremacy is affirmed, the constraint shifts towards a Tangled Rope or even a Rope, with lower executive extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(presidential_sovereignty_vs_parliamentary_supremacy, conceptual, 'Ambiguity over the ultimate source of presidential legitimacy in the Fifth Republic.').

omega_variable(
    article_49_3_legitimacy,
    'Is the use of Article 49.3 (passing legislation without a vote) a legitimate tool for executive efficiency or an anti-democratic circumvention of legislative process?',
    'Public opinion shifts, sustained mass protests, or a constitutional amendment limiting its use. The frequency and context of its invocation also provide empirical data.',
    'If seen as legitimate, it reinforces the hyper-presidential reading. If widely delegitimized, it increases the effective resistance and suppression costs for the executive, pushing the constraint towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_49_3_legitimacy, empirical, 'Contestation over the democratic legitimacy of Article 49.3.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''hyper_presidential_reading'' of the ''fifth_republic_constitution'' kernel. What would change if a ''parliamentary_constraint_reading'' or ''cohabitation_equilibrium_reading'' were adopted?',
    'A shift in political practice, constitutional interpretation by the Conseil Constitutionnel, or a constitutional reform that explicitly rebalances power towards the legislature or mandates power-sharing.',
    'A ''parliamentary_constraint_reading'' would significantly reduce executive extractiveness and suppression, likely reclassifying the constraint as a Tangled Rope or Rope. A ''cohabitation_equilibrium_reading'' would introduce a more balanced power dynamic, reducing the ''presidency_as_institution''s'' beneficiary status and increasing the ''national_assembly''s'' power, likely resulting in a Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Documents this constraint as one reading of the Fifth Republic Constitution kernel and outlines the structural deltas of sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__hyper_presidential_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t0, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fift_tr_t5, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(fift_tr_t10, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(fift_tr_t15, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(fift_be_t0, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(fift_be_t5, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(fift_be_t10, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(fift_be_t15, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 15, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t0, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(fift_su_t5, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(fift_su_t10, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(fift_su_t15, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 15, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__hyper_presidential_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__parliamentary_constraint_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Fifth Republic Constitution kernel, emphasizing a hyper-presidential system. It is linked to sibling readings that emphasize parliamentary constraint or cohabitation dynamics, as these interpretations directly influence the operationalization of executive power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
