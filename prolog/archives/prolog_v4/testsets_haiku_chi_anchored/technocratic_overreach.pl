% ============================================================================
% CONSTRAINT STORY: technocratic_overreach
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technocratic_overreach, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: technocratic_overreach
 *   human_readable: The Rule of the Expert: Technocratic Overreach
 *   domain: political/technological
 *
 * SUMMARY:
 *   Technocratic overreach describes the structural tension between the
 *   genuine need for expert coordination on complex problems (safety
 *   standards, public health, infrastructure design) and the exclusionary
 *   mechanism by which experts consolidate decision-making authority and
 *   remove non-experts from the deliberative process. This constraint
 *   exhibits the full range of DR classification depending on perspective.
 *   The technical expert class benefits from epistemic monopoly and
 *   centralized authority (they experience coordination). The excluded public
 *   is trapped by credentialism and cannot participate or appeal (they
 *   experience extraction). Decentralized decision-makers lose authority to
 *   central experts (they experience suppression). The institutional
 *   delegates who granted authority experience a mixed constraint: technical
 *   legitimacy for unpopular decisions, but loss of reversibility and
 *   democratic accountability. The legacy technocratic system has become
 *   substantially theatrical — expert committees perform deliberation while
 *   real constraints are predetermined. The analytical observer risks
 *   naturalizing expert rule as inevitable, failing to distinguish between
 *   the genuine coordination function of expertise and the extractive
 *   mechanism of epistemic gatekeeping. The constraint's theater ratio (0.64)
 *   reflects that many expert-imposed rules appear to result from neutral
 *   optimization but actually reflect hidden value choices: what counts as
 *   'safe,' what trade-offs are acceptable, whose knowledge is legitimate,
 *   and whose costs are externalized.
 *
 * KEY AGENTS:
 *   - Technical Expert Class: Primary beneficiary (institutional/arbitrage) — captures epistemic authority, gains decision-making power, benefits from credentialist gatekeeping
 *   - Excluded Public: Primary victim (powerless/trapped) — subject to rules they cannot influence, cannot appeal to non-expert knowledge or values, trapped by asymmetric information
 *   - Decentralized Decision-Makers: Secondary victim (moderate/constrained) — lose local authority to centralized experts, constrained by mandates they cannot modify
 *   - Institutional Delegates: Mixed actor (powerful/mobile) — gain technical legitimacy and reduced accountability, but lose reversibility and democratic control
 *   - Legacy Technocratic System: Institutional actor (institutional/arbitrage) — persists through bureaucratic inertia despite degraded function
 *   - Knowledge Commons Advocates: Organized agents (organized/constrained) — building alternative epistemic pathways (citizen science, participatory planning, indigenous knowledge validation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technocratic_overreach, 0.52).
domain_priors:suppression_score(technocratic_overreach, 0.68).
domain_priors:theater_ratio(technocratic_overreach, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technocratic_overreach, extractiveness, 0.52).
narrative_ontology:constraint_metric(technocratic_overreach, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(technocratic_overreach, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technocratic_overreach, tangled_rope).
narrative_ontology:human_readable(technocratic_overreach, "The Rule of the Expert: Technocratic Overreach").
narrative_ontology:topic_domain(technocratic_overreach, "political/technological").

domain_priors:requires_active_enforcement(technocratic_overreach).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technocratic_overreach, technical_expert_class).
narrative_ontology:constraint_beneficiary(technocratic_overreach, centralizing_institutions).
narrative_ontology:constraint_victim(technocratic_overreach, excluded_publics).
narrative_ontology:constraint_victim(technocratic_overreach, decentralized_decision_makers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED PUBLIC (SNARE) — Subject to expert-imposed constraints with no meaningful participation in rule formation or appeal. Trapped by credentialism and epistemic asymmetry. Cannot exit the jurisdiction or override expert decision. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(technocratic_overreach, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DECENTRALIZED DECISION-MAKER (SNARE) — Local officials, community boards, or subsidiary institutions lose authority to centralized technical experts. Constrained exit (exit costs high; some regulatory arbitrage possible but limited). d≈0.78, f(d)≈1.05, σ=0.9 → χ≈0.49.
constraint_indexing:constraint_classification(technocratic_overreach, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: TECHNICAL EXPERT CLASS (ROPE) — Benefits from centralized authority and epistemic monopoly. Experiences the constraint as coordination: efficiency gains from standardized expert rule. Arbitrage exit (can move to other jurisdictions or sectors). d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(technocratic_overreach, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL DELEGATE (TANGLED ROPE) — Political leadership that has delegated authority to experts. Benefits from technical legitimacy and reduced accountability. Also constrained by expert capture and inability to reverse decisions without reputational cost. d≈0.48, f(d)≈0.62, σ=1.0 → χ≈0.32. Mixed experience: coordination function (technical competence) plus extraction (loss of democratic control).
constraint_indexing:constraint_classification(technocratic_overreach, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY TECHNOCRATIC SYSTEM (PITON) — Once genuinely functional (mid-20th century: public health expertise, infrastructure planning, safety standards). Now substantially theatrical: expert committees perform deliberation while real authority is concentrated. Theater ratio 0.64 reflects post-hoc justification of predetermined decisions. Institution persists through bureaucratic inertia and credentialist gatekeeping despite degraded function.
constraint_indexing:constraint_classification(technocratic_overreach, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks classifying expert-driven optimization as an immutable technical necessity. 'Complex systems require expert management' naturalizes what is a contingent institutional choice. Structural data (ε=0.52, suppression=0.68, theater=0.64) contradicts mountain classification, revealing false summit: technocratic overreach is not inherent to technology or complexity.
constraint_indexing:constraint_classification(technocratic_overreach, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technocratic_overreach_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(technocratic_overreach, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(technocratic_overreach, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(technocratic_overreach, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(technocratic_overreach, TR),
    TR >= 0.70.

:- end_tests(technocratic_overreach_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Expert authority captures significant decision-making power and excludes non-expert voices from deliberation. However, extractiveness is not maximal because many expert decisions do genuinely improve outcomes (public health improvements, infrastructure safety). The extraction exists but is partially justified by coordination gains. The value reflects that some benefits flow to the public, preventing pure Snare classification. Suppression (0.68): High. Credentialism creates barriers to non-expert participation. Appeals mechanisms are typically costly and ineffective. The public has limited capacity to challenge expert decisions without acquiring credentials (which is the extraction mechanism itself). Media coverage of expert authority is often deferential. However, suppression is not total — some jurisdictions allow participatory mechanisms, and some experts genuinely seek public input. Theater ratio (0.64): Moderate-high. Expert committees and technical review boards perform deliberation while key decisions may already be predetermined by institutional interests or political pressure. The appearance of neutral optimization masks value choices about distribution of risk, costs, and benefits. Public consultation is often theatrical: comments collected but rarely incorporated. However, some expert processes (rigorous scientific review, detailed technical analysis) have genuine epistemic content, preventing pure theatrical classification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals maximum perspectival divergence. The beneficiary (expert class) sees coordination and legitimate authority. The victim (excluded public) sees pure extraction masked by technical language. The decentralized decision-maker sees suppression and loss of authority. The institutional delegate sees mixed coordination and accountability loss. The legacy system sees itself as degraded theater. The analytical observer risks naturalizing the entire structure as inevitable. The perspectival gap exposes that 'expertise' is not a simple category — it simultaneously represents genuine knowledge, institutional power, epistemological gatekeeping, and coordination benefit, all intertwined. Each agent experiences a different aspect of this compound.
 *
 * DIRECTIONALITY LOGIC:
 *   Technical expert class: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary — can move between jurisdictions, gains authority. Excluded public: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction — cannot exit the jurisdiction, no credentialist escape route. Decentralized decision-makers: Victim + constrained → d≈0.78, f(d)≈1.05. Significant extraction but some arbitrage (can lobby for exceptions, some regulatory flexibility). Institutional delegate: Mixed (powerful + mobile) → d≈0.48, f(d)≈0.62. Symmetric experience: gains legitimacy, loses control. Legacy system: Institutional + arbitrage → d≈0.08, f(d)≈-0.11. Piton classification from theater gate, not from directionality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_legitimacy_threshold,
    'What criteria distinguish legitimate expertise-based authority from epistemic overreach masquerading as optimization?',
    'Audit of expert recommendations against independent verification; measurement of predictive accuracy; comparison of expert-prescribed outcomes to outcomes from community-directed alternatives',
    'If threshold is behavioral (accuracy/outcomes): many jurisdictions reveal hidden incompetence and hidden extraction. If threshold is credentialist (possession of credentials): legitimacy persists regardless of outcomes, locking in Snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemic_legitimacy_threshold, empirical, 'Threshold distinguishing legitimate expertise from epistemic overreach').

omega_variable(
    reversibility_and_appeal,
    'Do expert-imposed constraints include meaningful mechanisms for reversal, modification, or appeal by non-experts?',
    'Documentation of appeal procedures; tracking of successful reversals or modifications initiated by affected publics; measurement of time and cost to challenge expert decisions',
    'If reversibility is genuine and accessible: suppression decreases, classification may shift toward Tangled Rope. If appeals are symbolic only: suppression stays high, Snare classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reversibility_and_appeal, empirical, 'Availability and effectiveness of reversibility mechanisms').

omega_variable(
    capture_vs_competence,
    'To what extent do institutional experts advance public welfare versus institutional self-preservation?',
    'Longitudinal analysis of expert recommendations by source (independent experts, institutional insiders, captured agencies); correlation between expert advice and measurable outcomes; institutional budget growth vs outcome improvement',
    'If competence dominates: extractiveness decreases, coordination function genuine. If capture dominates: extractiveness increases, theater ratio rises, classification confirms Piton or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_vs_competence, empirical, 'Balance between institutional competence and self-serving capture').

omega_variable(
    knowledge_commons_viability,
    'Can distributed, non-credentialist knowledge production (community expertise, lived experience, participatory science) provide viable alternatives to centralized expert authority?',
    'Comparative outcomes: jurisdictions allowing knowledge pluralism vs centralized expert monopoly; success rates of community-managed systems vs expert-managed systems in comparable domains; cost-benefit analysis',
    'If viable: expert monopoly is contingent extraction, not technical necessity. Scaffold perspective strengthened — decentralization is a real exit path. If non-viable: expert authority may be inevitable; Rope classification supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_commons_viability, empirical, 'Viability of non-credentialist knowledge production').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technocratic_overreach, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(techex_tr_t0, technocratic_overreach, theater_ratio, 0, 0.35).
narrative_ontology:measurement(techex_tr_t25, technocratic_overreach, theater_ratio, 25, 0.5).
narrative_ontology:measurement(techex_tr_t50, technocratic_overreach, theater_ratio, 50, 0.64).

% Extraction over time
narrative_ontology:measurement(techex_be_t0, technocratic_overreach, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(techex_be_t25, technocratic_overreach, base_extractiveness, 25, 0.4).
narrative_ontology:measurement(techex_be_t50, technocratic_overreach, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technocratic_overreach, enforcement_mechanism).
narrative_ontology:affects_constraint(technocratic_overreach, regulatory_capture).
narrative_ontology:affects_constraint(technocratic_overreach, epistemic_asymmetry).
narrative_ontology:affects_constraint(technocratic_overreach, credentialist_gatekeeping).

% DUAL FORMULATION NOTE:
% Technocratic overreach is a hybrid constraint that combines genuine coordination (expert knowledge improves outcomes) with extraction (expert authority excludes non-expert voices and removes reversibility). The constraint family includes upstream constraints (regulatory capture, credentialist gatekeeping) that enable overreach, and downstream constraints (epistemic asymmetry, knowledge commons degradation) that result from it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technocratic_overreach, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
