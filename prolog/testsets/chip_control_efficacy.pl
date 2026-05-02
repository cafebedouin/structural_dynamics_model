% ============================================================================
% CONSTRAINT STORY: chip_control_efficacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_chip_control_efficacy, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: chip_control_efficacy
 *   human_readable: Semiconductor Export Control Efficacy in Strategic Competition
 *   domain: international_relations/technology_governance/strategic_competition
 *
 * SUMMARY:
 *   US semiconductor export controls targeting China's AI capabilities
 *   represent a coordination mechanism among allied nations to manage
 *   dual-use technology diffusion. The constraint exhibits low extractiveness
 *   (0.18) because the primary function is genuine coordination around
 *   technology governance rather than asymmetric extraction. Observable
 *   evidence shows the controls create measurable but surmountable barriers:
 *   Chinese AI labs report 10x-30x compute disadvantages in hardware access,
 *   yet model capability gaps remain under 1 year, indicating effective
 *   workarounds through cloud rental, chip stacking, algorithmic efficiency,
 *   and domestic production ramp-up. The theater ratio (0.42) reflects
 *   moderate performative content — export licensing processes involve
 *   substantial bureaucratic ritual, but the core verification mechanisms
 *   (end-use monitoring, supply chain tracking) have genuine functional
 *   content. The constraint coordinates legitimate national security
 *   interests among allied semiconductor producers while imposing limited
 *   costs on Chinese research institutions that retain multiple adaptation
 *   pathways.
 *
 * KEY AGENTS:
 *   - US National Security Apparatus: Primary beneficiary (institutional/arbitrage) — coordinates allied technology governance; experiences constraint as pure coordination mechanism
 *   - Allied Chip Manufacturers: Secondary beneficiary (institutional/mobile) — gain market clarity and reduced competitive pressure from coordination; can exit to alternative markets
 *   - Chinese AI Research Institutions: Constrained actor (institutional/constrained) — face genuine hardware access barriers but retain workarounds through rental, stacking, efficiency gains, and domestic alternatives; experience mixed coordination (clear rules) and extraction (capability delay)
 *   - International Technology Governance Coalition: Organized agents (organized/mobile) — multilateral export control regimes (Wassenaar, MTCR) building generational norms around dual-use technology; see temporary coordination mechanism with sunset as technology diffuses
 *   - Analytical Observer: Civilizational view (analytical/analytical) — evaluates whether controls coordinate legitimate security interests or extract rents through artificial scarcity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(chip_control_efficacy, 0.18).
domain_priors:suppression_score(chip_control_efficacy, 0.25).
domain_priors:theater_ratio(chip_control_efficacy, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(chip_control_efficacy, extractiveness, 0.18).
narrative_ontology:constraint_metric(chip_control_efficacy, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(chip_control_efficacy, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(chip_control_efficacy, rope).
narrative_ontology:human_readable(chip_control_efficacy, "Semiconductor Export Control Efficacy in Strategic Competition").
narrative_ontology:topic_domain(chip_control_efficacy, "international_relations/technology_governance/strategic_competition").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(chip_control_efficacy, us_semiconductor_industry).
narrative_ontology:constraint_beneficiary(chip_control_efficacy, allied_chip_manufacturers).
narrative_ontology:constraint_beneficiary(chip_control_efficacy, us_national_security_apparatus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(chip_control_efficacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

constraint_indexing:constraint_classification(chip_control_efficacy, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

constraint_indexing:constraint_classification(chip_control_efficacy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

constraint_indexing:constraint_classification(chip_control_efficacy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

constraint_indexing:constraint_classification(chip_control_efficacy, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(chip_control_efficacy_tests).
:- end_tests(chip_control_efficacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The controls impose real costs on Chinese AI development (hardware access delays, workaround inefficiencies, development resource diversion) but the costs are substantially lower than the claimed 10x-30x compute gap would suggest. Model capability trajectories show Chinese labs closing performance gaps within 12 months despite hardware restrictions, indicating the extraction is limited by effective substitution mechanisms. The constraint coordinates allied technology governance more than it extracts from targets. Suppression (0.25): Low-moderate. Chinese institutions face barriers (export denials, end-use restrictions, supply chain monitoring) but retain multiple exit options: cloud compute rental from non-restricted jurisdictions, chip stacking and clustering to aggregate restricted chips, algorithmic efficiency improvements, and domestic semiconductor production ramp-up (SMIC 7nm process). Suppression is real but not severe. Theater ratio (0.42): Moderate. Export licensing involves bureaucratic ritual (end-use certifications, deemed export reviews, multi-agency coordination) but the core mechanisms have functional content: supply chain tracking prevents diversion, end-use monitoring detects violations, and allied coordination prevents circumvention through third countries. The theater component reflects compliance costs that exceed direct verification needs, not wholesale performativity.
 *
 * PERSPECTIVAL GAP:
 *   The US national security apparatus sees pure coordination (Rope) — the controls solve a legitimate collective action problem among allies managing dual-use technology. Allied manufacturers see the same coordination function with mobile exit options. Chinese institutions see mixed coordination and extraction (Tangled Rope) — the rules are clear and stable (coordination) but impose real capability delays (extraction), though workarounds limit the severity. The governance coalition sees a temporary mechanism (Scaffold) — controls coordinate during a transition period but will sunset as technology diffuses and domestic alternatives emerge. The analytical observer confirms the rope classification — the constraint's primary function is coordination, with extraction limited by effective substitution.
 *
 * DIRECTIONALITY LOGIC:
 *   The US national security apparatus and allied chip manufacturers are primary beneficiaries — they coordinate technology governance and gain market clarity. Both have arbitrage or mobile exit options (can shift to alternative policy frameworks or markets) and experience low or negative effective extraction. Chinese AI research institutions are constrained actors — they face genuine barriers but retain workarounds, placing them in a mixed position with moderate extraction. The international governance coalition sees a temporary coordination mechanism with a generational sunset as technology diffuses and domestic alternatives mature. The analytical observer evaluates the constraint as coordination (low base extraction, genuine security function) rather than extraction (artificial scarcity for rent capture).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that technology export controls can function primarily as coordination mechanisms rather than pure extraction, even in adversarial contexts. The key evidence: (1) Chinese model capability gaps remain small (<1 year) despite large claimed compute gaps (10x-30x), indicating extraction is limited by workarounds; (2) Allied coordination is genuine — controls prevent technology leakage through third countries and create common governance standards; (3) Theater ratio is moderate, not high — bureaucratic costs exist but verification mechanisms have functional content. The rope classification is stable across most perspectives because the constraint's base extraction is genuinely low and its coordination function is genuine. The tangled rope perspective from Chinese institutions reflects their structural position (constrained exit, mixed costs/benefits) rather than a different reading of the constraint's base properties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(chip_control_efficacy, 2022, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(chip_control_efficacy, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is distinct from broader technology decoupling dynamics (separate story) and from specific semiconductor supply chain dependencies (separate story). The chip control efficacy constraint specifically addresses the coordination function and extraction profile of export restrictions as a governance mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
