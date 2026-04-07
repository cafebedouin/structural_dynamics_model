% ============================================================================
% CONSTRAINT STORY: fact_checker_resource_scarcity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fact_checker_resource_scarcity, []).

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
 *   constraint_id: fact_checker_resource_scarcity
 *   human_readable: Fact Checker Resource Scarcity and Epistemic Gatekeeping
 *   domain: information_infrastructure/epistemology
 *
 * SUMMARY:
 *   Fact-checking resource scarcity creates a structural bottleneck in
 *   epistemic verification. The public requires independent verification of
 *   information claims, but fact-checking capacity is concentrated in
 *   well-resourced institutional networks, primarily in wealthy Western
 *   jurisdictions. This scarcity empowers institutional fact-checkers and
 *   platform corporations to gatekeep which claims get verified, how, and
 *   according to which standards. The constraint exhibits the full range of
 *   DR classifications: powerless agents (public, independent checkers)
 *   experience it as a snare with no exit; moderate agents (regional
 *   fact-checkers) experience tangled coordination-and-extraction;
 *   institutional agents experience it as coordination that strengthens their
 *   authority; and organized coalitions see it as a temporary problem with
 *   decentralized solutions. The theater ratio reflects the performative
 *   character of legacy fact-checking (symbolic certification without
 *   proportional verification work) and the increasing use of fact-checking
 *   for reputation management rather than epistemic improvement.
 *
 * KEY AGENTS:
 *   - Public Epistemic Commons: Primary victim (powerless/trapped) — depends on scarce fact-checking resources; cannot verify independently
 *   - Independent Fact Checkers (Global South): Primary victim (powerless/trapped) — marginalized by funding concentration and platform prioritization
 *   - Mid-Tier Regional Fact Checkers: Secondary victim (moderate/constrained) — experience both coordination benefits and asymmetric extraction
 *   - Major Institutional Fact-Checking Networks: Primary beneficiary (institutional/arbitrage) — capture epistemic authority and gate verification labor
 *   - Platform Corporations: Primary beneficiary (institutional/arbitrage) — control algorithm-driven verification; extract labor from underfunded fact-checkers
 *   - Legacy Journalistic Fact-Checking: Institutional actor (institutional/arbitrage) — maintains prestige authority but performs diminishing verification work (piton)
 *   - Open-Source Verification Infrastructure: Organized coalitions (organized/constrained) — building decentralized alternatives with sunset logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fact_checker_resource_scarcity, 0.58).
domain_priors:suppression_score(fact_checker_resource_scarcity, 0.62).
domain_priors:theater_ratio(fact_checker_resource_scarcity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fact_checker_resource_scarcity, extractiveness, 0.58).
narrative_ontology:constraint_metric(fact_checker_resource_scarcity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fact_checker_resource_scarcity, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fact_checker_resource_scarcity, tangled_rope).
narrative_ontology:human_readable(fact_checker_resource_scarcity, "Fact Checker Resource Scarcity and Epistemic Gatekeeping").
narrative_ontology:topic_domain(fact_checker_resource_scarcity, "information_infrastructure/epistemology").

domain_priors:requires_active_enforcement(fact_checker_resource_scarcity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fact_checker_resource_scarcity, institutional_fact_checkers).
narrative_ontology:constraint_beneficiary(fact_checker_resource_scarcity, platform_corporations).
narrative_ontology:constraint_victim(fact_checker_resource_scarcity, public_epistemic_commons).
narrative_ontology:constraint_victim(fact_checker_resource_scarcity, independent_fact_checkers).
narrative_ontology:constraint_victim(fact_checker_resource_scarcity, marginal_information_sources).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC EPISTEMIC COMMONS (SNARE) — Trapped in a verification crisis with no exit. The public cannot verify information independently; must defer to scarce, concentrated fact-checking resources. Bears full cost of false positives (misinformation circulates) and false negatives (true information suppressed). Zero agency, maximal extraction.
constraint_indexing:constraint_classification(fact_checker_resource_scarcity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT FACT CHECKERS — GLOBAL SOUTH (SNARE) — Trapped by resource scarcity, funding concentration in Western institutions, and platform gatekeeping. Cannot scale verification work; must defer to well-resourced institutional fact checkers. Marginalized from epistemic authority structures. Bears extraction through epistemic invisibility.
constraint_indexing:constraint_classification(fact_checker_resource_scarcity, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-TIER REGIONAL FACT CHECKERS (TANGLED ROPE) — Constrained by funding competition and platform algorithm dependency. Experience genuine coordination benefit (collaborative databases, shared verification protocols) alongside asymmetric extraction (underfunded relative to verification demand, dependent on platform referrals). Can exit to alternative platforms at high career cost.
constraint_indexing:constraint_classification(fact_checker_resource_scarcity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJOR INSTITUTIONAL FACT CHECKERS (ROPE) — Primary beneficiaries. Experience the constraint as coordination: resource scarcity gives their verification authority outsized weight. Network effects strengthen with scale. Exit options high (can launch independent platform, secure foundation funding). Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(fact_checker_resource_scarcity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PLATFORM CORPORATIONS (ROPE) — Outsource verification labor to underfunded fact-checking ecosystem while capturing epistemic authority. Resource scarcity means platforms can control which facts are checked and how. Coordination function (information quality gates) coexists with extraction (free labor sourcing). High exit optionality (can modify algorithms, internalize fact-checking, partner selectively).
constraint_indexing:constraint_classification(fact_checker_resource_scarcity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY JOURNALISTIC FACT-CHECKING (PITON) — Traditional newspaper fact-checking (NYT, Guardian, BBC) persists through institutional inertia and prestige authority, but is substantially performative relative to verification demand. The actual verification work has shifted to specialized networks and platform systems; legacy fact-checking is maintained as a symbolic good-housekeeping certification. Theater ratio high, functional extraction low.
constraint_indexing:constraint_classification(fact_checker_resource_scarcity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: OPEN-SOURCE VERIFICATION INFRASTRUCTURE (SCAFFOLD) — Organized agents (Wikimedia, OpenFact, distributed verification networks) see resource scarcity as a temporary coordination failure with a sunset: decentralized verification tools, crowdsourced fact-checking, and blockchain-based provenance systems are building alternative verification pathways. Suppression is moderate (barrier to adoption, platform resistance) but not total; agents perceive an exit path within generational timeframe.
constraint_indexing:constraint_classification(fact_checker_resource_scarcity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the constraint exhibits genuine coordination (verification is a public good requiring shared infrastructure) alongside structural extraction (resource concentration enables gatekeeping). No single type adequately captures the structure; tangled rope reflects the coexistence of coordination and asymmetric extraction at the system level.
constraint_indexing:constraint_classification(fact_checker_resource_scarcity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fact_checker_resource_scarcity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fact_checker_resource_scarcity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fact_checker_resource_scarcity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fact_checker_resource_scarcity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fact_checker_resource_scarcity, TR),
    TR >= 0.70.

:- end_tests(fact_checker_resource_scarcity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint exhibits increasing extraction over the measurement interval (0.35 → 0.61), reflecting intensifying information production that outpaces fact-checking capacity expansion. Resource scarcity is not static; it is self-reinforcing. More unverified claims → higher scarcity premium → stronger gatekeeping → greater extraction. The measurement trajectory shows the trap: scarcity increases faster than capacity can respond. Suppression (0.62): High. Barriers to independent verification include: specialized expertise requirements, funding concentration in Western institutions, platform algorithm gatekeeping, career risk for challenging institutional consensus, and linguistic/geographic barriers for Global South fact-checkers. These barriers are substantial but not absolute — some verification occurs outside institutional channels. Theater ratio (0.68 and rising): Moderate-high. Legacy fact-checking (newspapers, traditional institutions) increasingly serves a symbolic function — establishing institutional credibility rather than proportionally increasing verification coverage. The theater has increased over time as demand outpaced capacity, creating performative substitution: publish fact-check reports that create appearance of comprehensive verification coverage while actual verification remains bottlenecked.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates sharp perspectival divergence. Institutional fact-checkers see coordination and authority enhancement (rope); platforms see cost externalization and agenda-setting leverage (rope); but the public and independent checkers experience epistemic entrapment (snare). Mid-tier checkers occupy an ambiguous middle — genuinely benefiting from infrastructure coordination while being squeezed by resource competition. The open-source coalition sees the scarcity as temporary and solvable, projecting a decentralization pathway (scaffold). Legacy journalism sees its own role as degraded and threatened (piton), maintaining authority rituals (prestigious bylines, editorial standards) while actual verification work migrates elsewhere. The analytical observer at the civilizational level cannot reduce this to a single type because the structure itself is dual: there is real coordination in the verification infrastructure, and there is real extraction in the resource concentration and gatekeeping. This irreducible duality is the mark of tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary beneficiaries (institutional fact-checkers, platforms) have low directionality (d ≈ 0.10-0.20) — they gain from the constraint and have high exit optionality. The primary victims (public, independent checkers) have high directionality (d ≈ 0.90-0.95) — they bear costs and have minimal exit options. Mid-tier regional fact-checkers occupy the intermediate position: they benefit from verification coordination infrastructure but are constrained by funding dependency and platform referral requirements. The global analytical observer at civilizational scope computes tangled_rope because both genuine coordination (shared verification standards, collaborative databases, information quality infrastructure) and clear extraction (resource concentration, gatekeeping, epistemic marginalization) are structurally real. No single type captures both functions; tangled rope is the appropriate classification.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing that fact-checking resource scarcity is genuinely a tangled rope with dual function. The coordination function (verification infrastructure, shared standards, collaborative checking protocols) is real and valuable; the extraction function (gatekeeping, authority concentration, labor externalization) is equally real and harmful. The constraint cannot be reduced to pure coordination (rope) because institutional fact-checkers benefit asymmetrically and gatekeeping actively constrains alternatives. It cannot be reduced to pure extraction (snare) because some verification coordination genuinely occurs and the infrastructure provides public goods. The tension between these functions is structural and persistent. The scaffold perspective (decentralized verification sunset) offers a possible resolution path, but the timeline is unclear and platform resistance is substantial. The piton perspective (legacy journalism degradation) indicates that some institutional forms are losing function while maintaining theatrical authority — this is a real diagnostic signal that the current arrangement is under stress.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_demand_growth_rate,
    'Does information production (volume of claims requiring verification) grow faster than fact-checking capacity can expand?',
    'Longitudinal measurement: ratio of new information claims (social media, news, user-generated content) to fact-checker person-hours allocated; growth rate comparison over 5-10 year periods',
    'If demand grows faster: resource scarcity is structural and self-reinforcing (more scarcity → higher extraction → scaffold becomes aspirational). If capacity growth matches demand: scarcity is temporary and snare classification is overstated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(verification_demand_growth_rate, empirical, 'Whether information production outpaces fact-checking capacity').

omega_variable(
    platform_gatekeeping_necessity,
    'Is platform gatekeeping (platform selection of which facts get checked) a necessary feature of resource scarcity or a contingent choice by platform corporations?',
    'Comparative institutional analysis: platforms with algorithmic fact-checking recommendation vs distributed user-driven fact-checking requests. Measurement of verification coverage difference.',
    'If necessary: platform gatekeeping is inherent to the constraint (snare classification for public). If contingent: platforms are extracting value by restricting access (snare changes to tangled_rope with victims gaining agency).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_gatekeeping_necessity, conceptual, 'Whether platform gatekeeping is necessary or extractive').

omega_variable(
    global_south_capacity_ceiling,
    'Can regional fact-checking networks in Global South and Global East achieve verification capacity parity with Western-funded networks, or are systemic factors (language barriers, funding concentration, platform prioritization) insurmountable?',
    'Comparative funding analysis, verification coverage per capita, platform amplification measurement, training pipeline analysis for 10+ years',
    'If parity achievable: independent fact-checkers can exit scarcity (snare → tangled_rope). If ceiling exists: structural marginalization (snare classification confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(global_south_capacity_ceiling, empirical, 'Whether global verification capacity can equalize').

omega_variable(
    decentralized_verification_viability,
    'Can crowdsourced, decentralized, or blockchain-based verification systems achieve comparable accuracy to institutional fact-checking without centralized gatekeeping?',
    'Pilot program measurement: accuracy rates, consensus quality, false positive/negative rates for decentralized vs institutional systems on shared verification tasks',
    'If viable: scaffold sunset is real and resource scarcity becomes transitional (piton classification instead of snare). If ineffective: decentralization is aspirational and current concentration is necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_verification_viability, empirical, 'Whether decentralized verification can match institutional quality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fact_checker_resource_scarcity, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fcrs_tr_t0, fact_checker_resource_scarcity, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fcrs_tr_t5, fact_checker_resource_scarcity, theater_ratio, 5, 0.58).
narrative_ontology:measurement(fcrs_tr_t10, fact_checker_resource_scarcity, theater_ratio, 10, 0.68).
narrative_ontology:measurement(fcrs_tr_t15, fact_checker_resource_scarcity, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(fcrs_be_t0, fact_checker_resource_scarcity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fcrs_be_t5, fact_checker_resource_scarcity, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(fcrs_be_t10, fact_checker_resource_scarcity, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(fcrs_be_t15, fact_checker_resource_scarcity, base_extractiveness, 15, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fact_checker_resource_scarcity, information_standard).
narrative_ontology:affects_constraint(fact_checker_resource_scarcity, platform_content_moderation).
narrative_ontology:affects_constraint(fact_checker_resource_scarcity, epistemic_authority_concentration).
narrative_ontology:affects_constraint(fact_checker_resource_scarcity, misinformation_supply_chain).

% DUAL FORMULATION NOTE:
% Fact-checker resource scarcity is upstream of platform content moderation (which depends on fact-checking capacity) and downstream of information production capacity (which creates verification demand). The constraint has distinct ε values if measured by journalistic fact-checking (ε ≈ 0.42, more rope-like) vs platform-driven fact-checking (ε ≈ 0.68, more snare-like). This story captures the system-level constraint; decomposition into institutional forms may be warranted for detailed analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fact_checker_resource_scarcity, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
