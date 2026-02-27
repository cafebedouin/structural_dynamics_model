% ============================================================================
% CONSTRAINT STORY: us_foreign_policy_america_first
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_foreign_policy_america_first, []).

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
 *   constraint_id: us_foreign_policy_america_first
 *   human_readable: America First Foreign Policy Doctrine
 *   domain: geopolitical/international_relations
 *
 * SUMMARY:
 *   The 'America First' foreign policy doctrine prioritizes bilateral,
 *   transactional relationships and national interest over multilateral
 *   alliance commitments and international norm architecture. This constraint
 *   exhibits a hybrid coordination-extraction structure that varies
 *   dramatically by observer position. The US executive branch experiences it
 *   as a coordination mechanism enabling unilateral dealmaking without
 *   multilateral consensus friction. Alliance-dependent economies experience
 *   it as a snare: security commitments become contingent on trade
 *   concessions and policy alignment, with exit options severely constrained
 *   by the scale and specificity of security investments. Multinational
 *   corporations experience mixed coordination (bilateral deal access) and
 *   extraction (supply chain disruption, tariff unpredictability). Emerging
 *   multipolar blocs (China, EU, India) experience it as a temporary
 *   institutional disruption against which they can build alternatives. The
 *   doctrine's theater ratio (0.64) reflects a gap between nationalist
 *   rhetorical commitment to American sovereignty and the measured reality of
 *   bilateral leverage: much of the 'strength' is performative posturing to
 *   domestic audiences rather than extractive capacity relative to powerful
 *   alliance partners. The doctrine's mandatrophy remains unresolved because
 *   the core question — whether the doctrine optimizes US interests through
 *   genuine coordination benefits or merely masks extraction that harms
 *   long-term alliance cohesion — depends on whether alliance partners can
 *   escape faster than the doctrine adapts.
 *
 * KEY AGENTS:
 *   - US Executive Branch: Primary beneficiary (institutional/arbitrage) — gains unilateral dealmaking capacity and direct resource extraction without multilateral consensus constraints
 *   - Alliance-Dependent Economies (Japan, South Korea, NATO): Primary victims (powerless/trapped) — face transactional leverage tied to security guarantees; exit options require massive alternative security investment
 *   - Multilateral Norm Architecture (WTO, IMF, treaty systems): Victim (institutional/arbitrage) — functions persist through inertia while being systematically bypassed; degraded piton structure
 *   - Multinational Corporations: Secondary victim/mixed (organized/constrained) — benefit from bilateral deal exemptions but face extraction through tariff cycles and supply chain disruption
 *   - Emerging Multipolar Blocs: Organized actors (organized/constrained) — face extraction from unilateral pressure but building exit pathways (BRICS, Belt and Road, RCEP)
 *   - Domestic Manufacturing Communities: Secondary beneficiary (moderate/constrained) — gain tariff protection but face supply chain risk and policy uncertainty
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the doctrine as immutable structural realism rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_foreign_policy_america_first, 0.58).
domain_priors:suppression_score(us_foreign_policy_america_first, 0.68).
domain_priors:theater_ratio(us_foreign_policy_america_first, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_foreign_policy_america_first, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_foreign_policy_america_first, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_foreign_policy_america_first, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_foreign_policy_america_first, tangled_rope).
narrative_ontology:human_readable(us_foreign_policy_america_first, "America First Foreign Policy Doctrine").
narrative_ontology:topic_domain(us_foreign_policy_america_first, "geopolitical/international_relations").

domain_priors:requires_active_enforcement(us_foreign_policy_america_first).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_foreign_policy_america_first, us_executive_branch).
narrative_ontology:constraint_beneficiary(us_foreign_policy_america_first, domestic_manufacturing_interests).
narrative_ontology:constraint_beneficiary(us_foreign_policy_america_first, nationalist_political_coalition).
narrative_ontology:constraint_victim(us_foreign_policy_america_first, alliance_partner_economies).
narrative_ontology:constraint_victim(us_foreign_policy_america_first, multilateral_norm_architecture).
narrative_ontology:constraint_victim(us_foreign_policy_america_first, us_soft_power_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALLIANCE DEPENDENT ECONOMIES (SNARE) — Countries reliant on US security guarantees (Japan, South Korea, NATO members) face extraction through transactional leverage: security commitments become contingent on trade concessions, military basing negotiations, or policy alignment. Exit options are minimal without massive security investment. Experienced extraction is maximized.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MULTINATIONAL CORPORATIONS (TANGLED ROPE) — Firms with global operations benefit from bilateral deal-making capacity (tariff exemptions, investment protections) but face extraction through unpredictable bilateral renegotiation cycles and supply chain disruption costs. Exit involves reshoring or supply chain relocation (constrained, expensive). Mixed coordination function (bilateral negotiation access) and asymmetric extraction (deal reversal risk).
constraint_indexing:constraint_classification(us_foreign_policy_america_first, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: US EXECUTIVE BRANCH (ROPE) — The doctrine functions as coordination mechanism for unilateral dealmaking: bilateral negotiations enable direct extraction of concessions without multilateral consensus requirements. Executive derives maximum agency and resource flow. Low experienced extraction (negative chi) — this is the beneficiary perspective.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EMERGING MULTIPOLAR BLOCS (SCAFFOLD) — China, EU, India, and regional coalitions see the doctrine as temporary disruption of post-WWII unipolarity. They are building alternative structures (Belt and Road, RCEP, BRICS) as the unilateral system degrades. Exit path is migration to parallel institutions. Theater is moderate — the doctrine's performative nationalism masks declining actual leverage as alternatives mature. Sunset logic: as bipolarity or multipolarity stabilizes, bilateral transactional pressure loses coercive force.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: MULTILATERAL NORM ARCHITECTURE (PITON) — WTO, IMF, Paris Climate Agreement, and treaty-based systems persist in degraded form. The doctrine's unilateral approach bypasses rather than eliminates these institutions; they continue largely through inertia and because no single actor has successfully replaced them (only fragmented alternatives exist). Theater is high (0.64) — rhetorical commitment to rules-based order coexists with transactional violation of those rules. The piton represents institutional degradation without institutional replacement.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DOMESTIC MANUFACTURING INTERESTS (TANGLED ROPE) — Domestic producers benefit from tariff protection and bilateral deal-making that prioritizes domestic investment. But they also experience extraction through supply chain disruption, tariff retaliation, and policy uncertainty. Exit options are constrained by geographic immobility and capital specificity. Mixed benefit and cost structure.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: STRUCTURAL REALISM / ANALYTICAL (MOUNTAIN) — From a civilizational perspective, the doctrine reflects an invariant property of international relations: great powers naturally pursue national interest over multilateral constraint when structural conditions permit (relative power advantage, domestic political mandate). This perspective sees the doctrine as a manifestation of immutable structural incentives, not as a contingent institutional choice. However, structural data contradicts the mountain gate: active enforcement, shifting beneficiary coalitions, and measurable suppression indicate institutional contingency rather than natural law.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_foreign_policy_america_first_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_foreign_policy_america_first, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_foreign_policy_america_first, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_foreign_policy_america_first, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_foreign_policy_america_first, TR),
    TR >= 0.70.

:- end_tests(us_foreign_policy_america_first_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The doctrine extracts concessions from alliance partners through conditional security guarantees and tariff leverage. However, extraction is not maximal (0.66+ would indicate snare-only) because powerful alliance partners retain some bargaining capacity, and the doctrine's bilateral structure (requiring negotiation rather than unilateral imposition) creates mutual interdependence. The extractiveness value reflects measured coercion tempered by countervailing power. Suppression (0.68): High. The doctrine suppresses alternatives through: (1) rhetoric naturalizing unilateral leverage as national interest rather than extractive pressure; (2) institutional bypass (WTO sidestepping); (3) transaction cost elevation (forcing renegotiation of long-standing agreements); (4) alliance partner coordination barriers (bilateral rather than collective negotiation). Suppression is not total (some partners can and do resist) but substantial. Theater ratio (0.64): Moderate-high. The doctrine exhibits significant performative content: nationalist rhetoric about American strength masks declining institutional leverage relative to rising multipolar alternatives; commitment to rules-based order coexists with transactional violation of those rules; emphasis on bilateral deals obscures the constraint that powerful partners (EU, China) are building functional alternatives.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of classification across institutional power differentials. The US executive perceives coordination (rope) — bilateral negotiation enables direct extraction without multilateral friction. Alliance partners perceive extraction (snare) — security dependency enables coercive leverage with minimal exit options. Multinational corporations perceive mixed coordination-extraction (tangled rope) — bilateral deal access benefits them, but tariff cycles and supply chain risk harm them. Emerging blocs perceive temporary institutional disruption (scaffold) — the unilateral system is degrading and alternatives are maturing. The multilateral norm architecture perceives its own degradation (piton) — institutions persist through inertia while being systematically bypassed. The structural realist analyst perceives immutable natural law (mountain) — great powers naturally extract when power advantage exists — but this is a false summit that naturalizes contingent institutional choice. The perspectival gap is maximal: same structural phenomenon, six different experienced realities.
 *
 * DIRECTIONALITY LOGIC:
 *   The doctrine's directionality varies by agent's structural position. The US executive experiences low d (beneficiary with arbitrage — can make and unmake deals unilaterally, faces minimal costs from exit-option alternatives). Alliance partners experience high d (victims with trapped exit — security guarantees create dependency; alternative security requires years of development). Multinational corporations experience moderate-high d (victims with constrained exit — can diversify supply chains but at significant capital cost; tariff renegotiation cycles are unpredictable). Emerging blocs experience moderate d (organized victims with mobile exit — can build parallel institutions but requires time and coordination). The analytical observer experiences d=0.72 (analytical/civilizational position) but the classification (mountain) represents false naturalization that should be rejected by the mandatrophy analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: The doctrine's core ambiguity is whether it optimizes US interests through genuine bilateral coordination benefits (a superior alternative to multilateral consensus friction) or merely extracts rents from alliance dependence while undermining long-term strategic coherence. The mandatrophy is not resolved by the base properties because the answer depends on empirical questions about alliance partner exit capacity, emerging multipolar institution maturation timelines, and whether bilateral transaction costs exceed multilateral coordination costs at scale. If alliance partners can collectively escape (omega: alliance_partner_coalition_capacity), the snare classification dominates and the doctrine reduces to unsustainable extraction. If emerging alternatives mature faster than the doctrine can adapt (omega: multilateral_replacement_timeline), the scaffold classification dominates and the constraint has a genuine sunset. If the doctrine remains more adaptable than all alternatives, the tangled rope classification persists and the tension between coordination and extraction remains unresolved. The engine should flag this story as a mandatrophy case requiring longitudinal monitoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transactional_sustainability,
    'Can bilateral transactional relationships sustain reciprocal compliance when power asymmetries are detected or shift?',
    'Longitudinal analysis of bilateral agreement violation rates; comparison of transaction duration and renewal rates under unilateral vs reciprocal negotiation frameworks',
    'If unsustainable: doctrine reduces to extraction mechanism (snare). If sustainable: doctrine functions as coordination innovation (rope-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transactional_sustainability, empirical, 'Whether bilateral transactional relationships can maintain reciprocal compliance across power shifts').

omega_variable(
    multilateral_replacement_timeline,
    'Do emerging multipolar institutions (BRICS, Belt and Road, RCEP) represent genuine functional alternatives to post-WWII architecture or merely parallel redundant systems?',
    'Comparative institutional analysis: transaction costs, dispute resolution capacity, coverage of issue-areas (trade, security, development); measurement of policy coordination within alternative blocs',
    'If genuine alternatives mature within 10-20 years: scaffold sunset is real. If institutions remain fragmented: constraint persists as snare/tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multilateral_replacement_timeline, empirical, 'Whether emerging multipolar institutions provide functional replacements for post-WWII architecture').

omega_variable(
    soft_power_extraction_asymmetry,
    'Is the doctrine''s suppression of multilateral norm advocacy (theater 0.64) driven by deliberate extraction strategy or by domestic political constraint requiring nationalist rhetorical cover?',
    'Elite interviews with policymakers; analysis of internal US policy debate documents; comparative study of administration statements vs. actual bilateral leverage deployment',
    'If deliberate strategy: suppression is a core feature of the extraction mechanism. If domestic constraint: suppression is partially theatrical, reducing underlying coercion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(soft_power_extraction_asymmetry, conceptual, 'Whether doctrine''s suppression is strategic extraction or domestic political constraint').

omega_variable(
    alliance_partner_coalition_capacity,
    'Can alliance-dependent economies collectively exit US framework (through alternative security arrangements, diversified supply chains, unified negotiating position) faster than the doctrine can adapt?',
    'Analysis of alternative defense procurement (European indigenous capability, Japanese re-militarization), supply chain diversification indices, and coordination attempts in multilateral forums',
    'If exit capacity exceeds adaptation capacity: snare classification is temporary (allied escape possible). If doctrine remains more adaptable: snare persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alliance_partner_coalition_capacity, empirical, 'Whether alliance partners can collectively exit faster than doctrine adapts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_foreign_policy_america_first, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(amfirst_tr_t0, us_foreign_policy_america_first, theater_ratio, 0, 0.45).
narrative_ontology:measurement(amfirst_tr_t3, us_foreign_policy_america_first, theater_ratio, 3, 0.58).
narrative_ontology:measurement(amfirst_tr_t6, us_foreign_policy_america_first, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(amfirst_be_t0, us_foreign_policy_america_first, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(amfirst_be_t3, us_foreign_policy_america_first, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(amfirst_be_t6, us_foreign_policy_america_first, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_foreign_policy_america_first, enforcement_mechanism).
narrative_ontology:affects_constraint(us_foreign_policy_america_first, us_security_guarantee_dependency).
narrative_ontology:affects_constraint(us_foreign_policy_america_first, wto_institutional_degradation).
narrative_ontology:affects_constraint(us_foreign_policy_america_first, multinational_supply_chain_fragmentation).
narrative_ontology:affects_constraint(us_foreign_policy_america_first, multipolar_institution_emergence).

% DUAL FORMULATION NOTE:
% This constraint is upstream of several domain-specific extraction mechanisms (security dependencies, trade fragmentation, institutional bypass). The doctrine itself is an institutional choice (tangible enforcement and beneficiary structure) rather than a natural law, despite appearances of structural inevitability. Decomposition into bilateral vs multilateral comparison would create two stories with different epsilon values: bilateral-optimality (higher coordination, lower extraction) vs bilateral-as-extraction-mask (higher extraction, theater masking coercion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_foreign_policy_america_first, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
