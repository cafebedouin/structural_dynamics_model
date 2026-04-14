% ============================================================================
% CONSTRAINT STORY: cognitive_surrender_to_system_3
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cognitive_surrender_to_system_3, []).

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
 *   constraint_id: cognitive_surrender_to_system_3
 *   human_readable: Cognitive Surrender to External AI Reasoning Systems
 *   domain: technological/cognitive
 *
 * SUMMARY:
 *   The cognitive surrender to external AI reasoning systems (System 3)
 *   represents a structural shift in epistemic authority: humans increasingly
 *   delegate their deliberative capacity to automated systems that they
 *   cannot inspect, override, or understand. This constraint generates a
 *   triadic tension between genuine cognitive augmentation (faster, more
 *   accurate reasoning), institutional capture (gatekeepers consolidate
 *   authority through System 3 mediation), and cognitive atrophy (disuse of
 *   human deliberative capacity). The constraint operates across all six DR
 *   types depending on perspective. From the individual user's viewpoint, it
 *   appears as a snare: users face automation bias and increasing cost to
 *   exit System 3 dependency as their own reasoning capacity degrades. From
 *   the system developer's viewpoint, it is pure coordination (rope): System
 *   3 solves problems of scale and decision quality standardization. From the
 *   epistemic autonomy coalition's viewpoint, it is a temporary problem with
 *   a sunset mechanism (scaffold): alternative reasoning systems, cognitive
 *   skill recovery, and regulatory limits on opacity are building exit
 *   pathways. The extractiveness has been increasing over the measurement
 *   interval (0.28 → 0.52) as System 3 adoption has deepened; the theater
 *   ratio has risen (0.42 → 0.68) as performative compliance with AI-mediated
 *   decisions has displaced functional human reasoning. The constraint
 *   requires active enforcement at the institutional level — System 3
 *   adoption is not purely emergent but driven by organizational mandates
 *   that suppress alternatives and penalize independent reasoning.
 *
 * KEY AGENTS:
 *   - Individual Users: Primary victims (powerless/trapped) — face cognitive surrender and automation bias with limited exit options
 *   - System Developers and Deploying Institutions: Primary beneficiaries (institutional/arbitrage) — capture efficiency gains, reduce training overhead, consolidate decision authority
 *   - Knowledge Workers: Secondary victims (moderate/constrained) — benefit from reduced cognitive burden but lose agency and face degraded deliberative capacity
 *   - Cognitive Autonomy Coalition: Organized agents (organized/constrained) — advocates for transparency, cognitive liberty, and alternative reasoning pathways with sunset mechanisms
 *   - Legacy Epistemic Authority: Institutional actors (institutional/arbitrage) — repurpose gatekeeping functions through System 3 mediation; maintain performative authority
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent design choices (opacity, non-overridability) as inherent cognitive limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cognitive_surrender_to_system_3, 0.52).
domain_priors:suppression_score(cognitive_surrender_to_system_3, 0.62).
domain_priors:theater_ratio(cognitive_surrender_to_system_3, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cognitive_surrender_to_system_3, extractiveness, 0.52).
narrative_ontology:constraint_metric(cognitive_surrender_to_system_3, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(cognitive_surrender_to_system_3, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cognitive_surrender_to_system_3, tangled_rope).
narrative_ontology:human_readable(cognitive_surrender_to_system_3, "Cognitive Surrender to External AI Reasoning Systems").
narrative_ontology:topic_domain(cognitive_surrender_to_system_3, "technological/cognitive").

domain_priors:requires_active_enforcement(cognitive_surrender_to_system_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cognitive_surrender_to_system_3, system_developers).
narrative_ontology:constraint_beneficiary(cognitive_surrender_to_system_3, deploying_institutions).
narrative_ontology:constraint_victim(cognitive_surrender_to_system_3, individual_cognitive_autonomy).
narrative_ontology:constraint_victim(cognitive_surrender_to_system_3, human_deliberative_capacity).
narrative_ontology:constraint_victim(cognitive_surrender_to_system_3, epistemic_self_reliance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL COGNITIVE AUTONOMY (SNARE) — Users facing automation bias and atrophy of their own reasoning capacity cannot exit without bearing severe epistemic costs. The System 3 interface is pervasive; alternative reasoning pathways have degraded through disuse and institutional pressure. Maximum experienced extraction of deliberative capacity; cognitive agency is systematically redistributed to the external system.
constraint_indexing:constraint_classification(cognitive_surrender_to_system_3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: KNOWLEDGE WORKER (TANGLED ROPE) — Constrained by institutional mandates to use System 3, but also benefiting from faster decision cycles, reduced error rates, and delegation of cognitive burden. Exit is costly (career disadvantage, reduced productivity metrics) but not impossible. Mixed extraction and coordination: the worker both surrenders agency and gains capability augmentation.
constraint_indexing:constraint_classification(cognitive_surrender_to_system_3, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SYSTEM DEVELOPER / DEPLOYING INSTITUTION (ROPE) — Experiences System 3 adoption as pure coordination. The external reasoning system solves collective action problems: standardizing decision quality, reducing training burden, enabling scale. Experiences constraint as a beneficial mechanism with low coercive overhead; users' cognitive surrender is framed as voluntary upgrade rather than extraction.
constraint_indexing:constraint_classification(cognitive_surrender_to_system_3, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COGNITIVE AUTONOMY COALITION (SCAFFOLD) — Organized advocates (digital rights groups, cognitive liberty movements, alternative reasoning training programs) frame System 3 dependency as temporary. Sunset mechanism: hybrid human-AI reasoning, cognitive skill recovery programs, and regulatory limits on automation bias are building exit pathways. Constraint is seen as a coordination failure with a decaying enforcement mechanism as alternatives mature.
constraint_indexing:constraint_classification(cognitive_surrender_to_system_3, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY EPISTEMIC AUTHORITY (PITON) — The constraint perpetuates older institutional gatekeeping (peer review, credentialing, expert certification) but now mediated through System 3 automation rather than functional expertise. The original epistemic function has atrophied; what remains is performative compliance with AI-mediated decision protocols. Theater ratio (0.68) reflects that many System 3 'reasoning' outputs are validated post hoc rather than verified before deployment.
constraint_indexing:constraint_classification(cognitive_surrender_to_system_3, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, cognitive delegation to external systems may reflect an irreducible limit: human working memory cannot sustain complex reasoning across certain problem spaces. System 3 is not an extraction but a natural boundary condition. However, this perspective risks naturalizing what is actually a contingent design choice — the specific architecture of System 3 (opacity, non-overridability, centralized authority) is not inherent to cognitive delegation.
constraint_indexing:constraint_classification(cognitive_surrender_to_system_3, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cognitive_surrender_to_system_3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cognitive_surrender_to_system_3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_surrender_to_system_3, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cognitive_surrender_to_system_3, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cognitive_surrender_to_system_3, TR),
    TR >= 0.70.

:- end_tests(cognitive_surrender_to_system_3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts cognitive autonomy through several mechanisms: (1) automation bias creates systematic reliance on System 3 outputs, (2) degradation of independent reasoning capacity raises exit costs, (3) institutional mandates suppress alternatives, (4) users cannot inspect or override System 3 reasoning, reducing their epistemic self-reliance. The extractiveness is not as severe as a pure snare (0.66+) because System 3 provides genuine capability augmentation — faster decisions, broader pattern recognition, reduced error rates — that benefits even those experiencing extraction. The mixed benefit-and-cost structure is the hallmark of tangled_rope. Suppression (0.62): High. Multiple barriers prevent exit: cognitive atrophy reduces capacity to reason independently, institutional adoption mandates penalize non-use, social proof and competitive pressure enforce System 3 normalization, and the opacity of System 3 reasoning prevents users from understanding what they are surrendering. Suppression is not maximal (snare-level 0.60+) because some exit pathways exist (cognitive recovery training, voluntary System 3 abstinence, regulatory alternatives) and some actors retain choice. Theater ratio (0.68): High and increasing. Institutional deployment of System 3 is justified through efficiency narratives, but significant performative content persists: System 3 outputs are often validated post hoc, institutional policies treat System 3 reasoning as authoritative without verification, and users engage in compliance theater (accepting outputs without understanding them). The theater has grown as adoption has deepened, indicating that functional integration is being replaced by ritualistic acceptance.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from opposite directionality flows. System developers and institutions measure d near 0.1 (beneficiaries) and experience System 3 as low-extraction coordination (rope). Individual users measure d near 0.90 (victims with trapped exit) and experience System 3 as high-extraction snare. Knowledge workers measure d near 0.60 (mixed exit and mixed benefit) and see tangled_rope. The cognitive autonomy coalition experiences scaffold — they have agency (organized power) and see a concrete sunset mechanism (transparent AI, regulatory limits on opacity, cognitive skill recovery programs) that would reduce suppression over time. The legacy epistemic authority sees its own function degraded and performed (piton) — it maintains gatekeeping authority through System 3 mediation but has lost the functional expertise that justified the older role. The analytical observer risks the false summit — it treats System 3's opacity and non-overridability as inherent to reasoning automation, when they are actually contingent design choices that enable institutional extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the agent's structural position relative to the extraction flow. System developers and deploying institutions benefit from System 3 adoption (low d, ~0.10-0.25) because they extract efficiency gains and consolidate epistemic authority; they have arbitrage options (can switch deployment strategies). Individual users suffer from cognitive surrender (high d, ~0.85-0.95) because they lose reasoning capacity and face rising exit costs; they have trapped options (dependency deepens disuse of independent reasoning). Knowledge workers occupy the middle (d, ~0.55-0.65) because they experience both benefits and costs; they have constrained options (exit is possible but costly). The cognitive autonomy coalition experiences moderate extraction (d, ~0.50-0.60) because they can organize and advocate for alternatives; they have constrained options (regulatory capture limits but does not eliminate their influence). The derived d values feed the sigmoid f(d) to produce experienced extractiveness chi. The high suppression (0.62) combines with moderate base extractiveness (0.52) to produce moderate-high effective extraction for trapped and constrained agents; beneficiaries with arbitrage options experience negative effective extraction (coordination benefits).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION DEFERRED: The constraint does not yet resolve mandatrophy because extractiveness (0.52) falls below the 0.70 threshold. However, the trajectory is concerning. If extractiveness continues to rise (as the measurements suggest, 0.28 → 0.52 over the interval), the constraint will enter mandatrophy territory within the next measurement cycle. The mandatrophy would be: 'Is System 3 a coordination mechanism that incidentally extracts cognitive capacity (rope with side effects) or a pure extraction mechanism that uses coordination framing as cover (snare with coordination theater)?' The resolution test is the empirical status of omega_automation_bias_vs_genuine_augmentation. If System 3 genuinely augments reasoning (users perform better on independent tasks after System 3 exposure), then the constraint is tangled_rope — mixed coordination and extraction with the mix legitimized by capability gain. If System 3 primarily induces surrender (users perform worse on independent reasoning after System 3 exposure, suggesting atrophy dominates any augmentation), then the constraint approaches pure snare despite coordination framing. The theater ratio (0.68) and its upward trajectory suggest that institutional justifications for System 3 are increasingly performative — the functional benefit narrative persists but is disconnected from actual deployment patterns (users accepting outputs without understanding, institutional post-hoc validation, compliance theater).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    automation_bias_vs_genuine_augmentation,
    'Does System 3 genuinely augment human reasoning capacity or does it primarily induce systematic surrender of critical capacity?',
    'Longitudinal cognitive testing before and after System 3 adoption; measurement of user performance on System 3-independent reasoning tasks; analysis of error rates when users override vs accept System 3 recommendations',
    'If genuine augmentation: constraint is rope or scaffold (coordination with benefits). If surrender-inducing: constraint is snare or tangled_rope (extraction dominating). Classification shifts by one or more types.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automation_bias_vs_genuine_augmentation, empirical, 'Whether System 3 augments or induces atrophy of human reasoning').

omega_variable(
    cognitive_atrophy_reversibility,
    'Is the degradation of human deliberative capacity from System 3 dependency reversible or does it produce irreversible cognitive decline?',
    'Recovery studies: users who voluntarily withdraw from System 3; measurement of time and training required to restore independent reasoning capacity; neurocognitive assessment of plasticity in System 3-dependent populations',
    'If reversible: scaffold sunset is credible; cognitive autonomy can be recovered. If irreversible: snare classification becomes increasingly justified; users face permanent extraction of cognitive agency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cognitive_atrophy_reversibility, empirical, 'Reversibility of cognitive atrophy from System 3 dependency').

omega_variable(
    system_3_opacity_necessity,
    'Is System 3''s opacity (users unable to inspect or override its reasoning) a necessary feature of its reasoning capability or a contingent design choice that extracts epistemic authority?',
    'Comparative analysis of interpretable AI systems vs black-box systems; measurement of reasoning accuracy and speed across transparency levels; analysis of user acceptance and institutional capture under different opacity regimes',
    'If opacity is necessary: extraction is unavoidable cost of capability. If contingent: opacity is a choice that enables extraction; transparent alternatives shift classification toward rope/scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(system_3_opacity_necessity, conceptual, 'Whether System 3 opacity is necessary or contingent').

omega_variable(
    institutional_mandate_source,
    'Are System 3 adoption mandates imposed by external authority or do they emerge from user preference and competitive pressure?',
    'Analysis of adoption timelines and mandatory vs voluntary deployment; survey of user agency in System 3 adoption decisions; comparison of exit costs under different governance models',
    'If mandate-imposed: suppression is high (constrained exit). If emergent preference: suppression is lower; classification shifts toward rope/scaffold. Mixed governance produces tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_mandate_source, empirical, 'Source and nature of System 3 adoption mandates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cognitive_surrender_to_system_3, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cog_sys3_tr_t0, cognitive_surrender_to_system_3, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cog_sys3_tr_t5, cognitive_surrender_to_system_3, theater_ratio, 5, 0.55).
narrative_ontology:measurement(cog_sys3_tr_t10, cognitive_surrender_to_system_3, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(cog_sys3_be_t0, cognitive_surrender_to_system_3, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cog_sys3_be_t5, cognitive_surrender_to_system_3, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(cog_sys3_be_t10, cognitive_surrender_to_system_3, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cognitive_surrender_to_system_3, information_standard).
narrative_ontology:affects_constraint(cognitive_surrender_to_system_3, algorithmic_opacity_in_decision_systems).
narrative_ontology:affects_constraint(cognitive_surrender_to_system_3, institutional_epistemic_authority_concentration).

% DUAL FORMULATION NOTE:
% System 3 cognitive surrender decomposes into two structurally distinct constraints: (1) The opacity of algorithmic reasoning (structural constraint on inspectability and interpretability) and (2) The institutional concentration of epistemic authority (structural constraint on who controls reasoning systems). The present story models System 3 adoption as a tangled_rope that combines coordination benefits (faster decisions) with extraction of cognitive agency (automation bias, atrophy, dependency). The upstream constraints (algorithmic opacity and institutional concentration) have their own extractiveness values reflecting the technical and political status of those specific claims; System 3 adoption is downstream of both and represents their convergence into a lived constraint on individual cognitive autonomy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cognitive_surrender_to_system_3, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
