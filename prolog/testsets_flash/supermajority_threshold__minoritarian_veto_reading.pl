% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__minoritarian_veto_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__minoritarian_veto_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: supermajority_threshold__minoritarian_veto_reading
 *   human_readable: Supermajority Threshold as Minoritarian Veto
 *   domain: constitutional_theory/political_economy/institutional_design
 *
 * SUMMARY:
 *   This constraint describes the supermajority threshold in constitutional
 *   amendment processes, specifically from the 'minoritarian veto' reading.
 *   In this view, the threshold, originally intended as a safeguard, has
 *   evolved into a mechanism that empowers blocking minorities to entrench
 *   the status quo against the will of contemporary majorities. This converts
 *   historical privilege into a permanent veto, leading to policy stagnation
 *   and democratic frustration. The constraint is claimed as a snare because
 *   its primary function, from this perspective, is extraction and
 *   suppression, rather than genuine coordination.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, 0.85).
domain_priors:suppression_score(supermajority_threshold__minoritarian_veto_reading, 0.75).
domain_priors:theater_ratio(supermajority_threshold__minoritarian_veto_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__minoritarian_veto_reading, snare).
narrative_ontology:human_readable(supermajority_threshold__minoritarian_veto_reading, "Supermajority Threshold as Minoritarian Veto").
narrative_ontology:topic_domain(supermajority_threshold__minoritarian_veto_reading, "constitutional_theory/political_economy/institutional_design").

domain_priors:requires_active_enforcement(supermajority_threshold__minoritarian_veto_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__minoritarian_veto_reading, 'a1815f4a-50b1-4765-8d34-d7972d80f44c').
narrative_ontology:cs_kernel_codification('a1815f4a-50b1-4765-8d34-d7972d80f44c', fixed_text).
narrative_ontology:cs_authority_grounding('a1815f4a-50b1-4765-8d34-d7972d80f44c', lineage).
narrative_ontology:cs_interpretation_layer_present('a1815f4a-50b1-4765-8d34-d7972d80f44c').
narrative_ontology:cs_reading_relation('a1815f4a-50b1-4765-8d34-d7972d80f44c', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('a1815f4a-50b1-4765-8d34-d7972d80f44c', supermajority_threshold__adaptive_gradient_reading, coexists_with).
narrative_ontology:cs_axiom('a1815f4a-50b1-4765-8d34-d7972d80f44c', foundational, minority_veto_is_anti_democratic).
narrative_ontology:cs_axiom_status(minority_veto_is_anti_democratic, holdable).
narrative_ontology:cs_axiom_grounding('a1815f4a-50b1-4765-8d34-d7972d80f44c', minority_veto_is_anti_democratic, deontological).
narrative_ontology:cs_axiom('a1815f4a-50b1-4765-8d34-d7972d80f44c', secondary, constitutional_stagnation_is_harmful).
narrative_ontology:cs_axiom_status(constitutional_stagnation_is_harmful, holdable).
narrative_ontology:cs_axiom_grounding('a1815f4a-50b1-4765-8d34-d7972d80f44c', constitutional_stagnation_is_harmful, empirically_contingent).
narrative_ontology:cs_reference_frame('a1815f4a-50b1-4765-8d34-d7972d80f44c', democratic_responsiveness_ideal).
narrative_ontology:cs_drift_state('a1815f4a-50b1-4765-8d34-d7972d80f44c', contemporary_political_gridlock, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a1815f4a-50b1-4765-8d34-d7972d80f44c', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, entrenched_elites).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, reform_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups benefit from existing policies and structures that the supermajority threshold protects from change. They wield their minority position to block reforms that would threaten their interests, effectively converting historical advantage into a permanent veto.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, entrenched_elites, beneficiary,
    institutional, generational, arbitrage, national).

% Individuals or organizations whose current advantages (e.g., tax breaks, regulatory capture, property rights) are shielded by the difficulty of constitutional amendment. They actively lobby against changes that require supermajority consent.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries, beneficiary,
    organized, biographical, mobile, national).

% The majority of the populace whose will is frustrated by the supermajority requirement. They bear the costs of an unresponsive system, unable to enact reforms despite broad support, leading to policy stagnation and democratic deficit.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities, payer,
    powerful, immediate, constrained, national).

% Activists, political parties, and civil society organizations pushing for constitutional or legislative changes. They expend significant resources attempting to overcome the supermajority barrier, often with little success, leading to burnout and disillusionment.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, reform_advocates, payer,
    moderate, biographical, constrained, national).

% Analyze the effects of supermajority rules on democratic governance, often highlighting the potential for minoritarian obstruction and the entrenchment of historical injustices. Their work provides the analytical framework for this reading.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The supermajority rule nominally coordinates broad political consensus for fundamental changes, ensuring stability and preventing hasty alterations to foundational law.
% TRANSFER_FUNCTION: Transfers political power and policy control from contemporary majorities to entrenched minorities, allowing the latter to maintain the status quo and associated privileges.
% ABSENT_VOICES: Future generations, whose interests are bound by an unamendable constitution, are absent from the current political process. They would advocate for a more flexible system that allows for adaptation to evolving societal needs.
% DISAPPEARANCE_RATIONALE: If the supermajority threshold vanished, the political landscape would immediately shift. Majorities would be empowered to enact long-stalled reforms, potentially leading to significant changes in economic policy, social rights, and institutional structures. Entrenched interests would lose their veto power, forcing a renegotiation of power dynamics.
% FOUNDING_PROBLEM: The supermajority threshold was established to prevent tyranny of the majority and ensure that fundamental constitutional changes reflect a deep, enduring societal consensus, protecting minority rights and long-term stability.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (often beneficiaries of the status quo) argue the problem of majoritarian overreach is still live. Critics (contemporary majorities, reform advocates, and many constitutional scholars) argue that the problem has shifted: the threshold now entrenches historical privilege and blocks necessary adaptation, converting a safeguard into a snare. Independent political science research and historical analysis of blocked reforms corroborate the shifted-function reading.
narrative_ontology:disappearance_verdict(supermajority_threshold__minoritarian_veto_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__minoritarian_veto_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__minoritarian_veto_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(supermajority_threshold__minoritarian_veto_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__minoritarian_veto_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__minoritarian_veto_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the threshold effectively transfers power from the majority to a minority, allowing the latter to extract benefits by preventing changes that would redistribute resources or power. Suppression (0.75) is also high, as it actively suppresses the will of the majority and their ability to enact desired reforms. The theater ratio (0.20) is low but rising, reflecting the increasing performative defense of the threshold as a 'consensus mechanism' even as its actual function is seen as obstruction. Accessibility collapse (0.60) is moderate, as alternatives (e.g., popular referendums, judicial interpretation) exist but are often insufficient to overcome the entrenched barrier. Resistance (0.70) is substantial, manifesting in sustained political movements and academic critiques.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of entrenched elites, the supermajority threshold is a legitimate safeguard (a 'rope' or even 'mountain' of stability). From the perspective of contemporary majorities, it is a 'snare' that prevents necessary adaptation and entrenches injustice. The engine's classification will highlight this divergence based on the declared beneficiaries and victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Entrenched elites and status quo beneficiaries are the primary beneficiaries (d near 0.0), as the threshold protects their interests. Contemporary majorities and reform advocates are the primary victims/targets (d near 1.0), as their will is consistently thwarted. Constitutional scholars act as analytical observers (d near 0.5).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_intent_vs_current_effect,
    'Does the supermajority threshold''s current effect align with its original founding intent (preventing tyranny of the majority) or has it drifted to primarily serve minoritarian entrenchment?',
    'Historical analysis of amendment attempts, legislative outcomes, and the demographic composition of blocking minorities over time. Comparison of the costs of majoritarian overreach vs. minoritarian obstruction.',
    'If the effect primarily serves entrenchment, the constraint''s classification as a snare is strengthened, and its legitimacy as a ''rope'' or ''mountain'' is undermined. If intent and effect align, the ''consensus_safeguard_reading'' gains empirical support.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_intent_vs_current_effect, empirical, 'Assessing the functional drift of the supermajority threshold.').

omega_variable(
    democratic_legitimacy_framing,
    'Is the supermajority threshold a legitimate mechanism for ensuring constitutional stability and protecting minority rights, or an illegitimate barrier to democratic self-governance?',
    'This is a conceptual and normative question, resolvable through philosophical and political debate, potentially influenced by empirical evidence of its effects on governance and social equity.',
    'Resolution would determine whether the constraint is framed as a necessary ''mountain'' or ''rope'' (consensus_safeguard_reading) or an illegitimate ''snare'' (minoritarian_veto_reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_framing, conceptual, 'The fundamental normative framing of supermajority rules.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__minoritarian_veto_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t1950, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(supe_tr_t1970, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(supe_tr_t1990, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(supe_tr_t2010, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(supe_tr_t2024, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(supe_be_t1950, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(supe_be_t1970, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 1970, 0.7).
narrative_ontology:measurement(supe_be_t1990, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 1990, 0.78).
narrative_ontology:measurement(supe_be_t2010, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(supe_be_t2024, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t1950, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(supe_su_t1970, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 1970, 0.58).
narrative_ontology:measurement(supe_su_t1990, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(supe_su_t2010, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(supe_su_t2024, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__minoritarian_veto_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'supermajority_threshold' kernel. Other readings include 'consensus_safeguard_reading' and 'adaptive_gradient_reading', which offer different interpretations of its function and legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
