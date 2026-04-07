% ============================================================================
% CONSTRAINT STORY: international_sports_federation_governance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_international_sports_federation_governance, []).

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
 *   constraint_id: international_sports_federation_governance
 *   human_readable: International Sports Federation Governance Structure
 *   domain: sports/governance/international_regulation
 *
 * SUMMARY:
 *   International sports federations operate as self-regulating governance
 *   bodies with centralized control over athlete eligibility, competition
 *   rules, doping verification, and tournament scheduling. This constraint
 *   manifests differently depending on the observer's structural position:
 *   individual athletes are trapped in an opaque system with no meaningful
 *   exit; smaller nations experience resource-driven exclusion; federation
 *   leadership sees pure coordination; wealthy nations and broadcasters
 *   extract net benefits while funding infrastructure; athletes' rights
 *   advocates are building democratic alternatives. The governance structure
 *   combines genuine coordination functions (standardizing rules, scheduling
 *   global events, managing athlete verification) with significant extractive
 *   elements (asymmetric revenue distribution, concentrated decision-making,
 *   opaque enforcement). Over the past 20 years, extractiveness has increased
 *   (from 0.35 to 0.58) as commercial broadcasting revenues have
 *   concentrated, while theater ratio has risen (from 0.52 to 0.68) as
 *   regulatory complexity has expanded without corresponding transparency
 *   increases.
 *
 * KEY AGENTS:
 *   - Individual Athletes: Primary victim (powerless/trapped) — bound by federation rules with no exit option; face opaque enforcement and appeal systems
 *   - Developing Nation Federations: Secondary victim (moderate/constrained) — pay fees and comply with standards but have no governance voice; resource asymmetry suppresses participation
 *   - Federation Leadership: Primary beneficiary (institutional/arbitrage) — experiences structure as legitimate coordination; benefits from administrative positions and influence
 *   - Wealthy National Federations: Secondary beneficiary (powerful/mobile) — benefit from hosting rights and rule influence; also contribute infrastructure funding (tangled rope)
 *   - Commercial Broadcasters: Secondary beneficiary (powerful/arbitrage) — extract substantial viewer and advertising value while paying rights fees; coordinate scheduling and technical standards
 *   - Athletes' Rights Advocates: Organized reformers (organized/constrained) — building democratic governance alternatives as sunset pathway
 *   - Anti-Doping Administration: Institutional enforcement actor (institutional/constrained) — enforces rules with asymmetric resource distribution across federations
 *   - Analytical Observer: Sees system as potentially immutable (analytical/analytical) — risks treating coordination costs as natural law rather than institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(international_sports_federation_governance, 0.58).
domain_priors:suppression_score(international_sports_federation_governance, 0.65).
domain_priors:theater_ratio(international_sports_federation_governance, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(international_sports_federation_governance, extractiveness, 0.58).
narrative_ontology:constraint_metric(international_sports_federation_governance, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(international_sports_federation_governance, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(international_sports_federation_governance, tangled_rope).
narrative_ontology:human_readable(international_sports_federation_governance, "International Sports Federation Governance Structure").
narrative_ontology:topic_domain(international_sports_federation_governance, "sports/governance/international_regulation").

domain_priors:requires_active_enforcement(international_sports_federation_governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(international_sports_federation_governance, federation_leadership).
narrative_ontology:constraint_beneficiary(international_sports_federation_governance, wealthy_national_federations).
narrative_ontology:constraint_beneficiary(international_sports_federation_governance, major_commercial_broadcasters).
narrative_ontology:constraint_victim(international_sports_federation_governance, athlete_autonomy).
narrative_ontology:constraint_victim(international_sports_federation_governance, smaller_national_federations).
narrative_ontology:constraint_victim(international_sports_federation_governance, developing_nation_participation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL ATHLETE (SNARE) — Athletes are bound by federation rules, doping protocols, eligibility requirements, and competition structures with no meaningful exit option. Career depends entirely on federation recognition. Rules are opaque, enforcement is asymmetric (harsh for athletes, lenient for federations), and appeal mechanisms are controlled by the federation itself. Maximum extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(international_sports_federation_governance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATION FEDERATION (SNARE) — Smaller federations pay membership fees and comply with technical standards but have no voice in governance. Competition infrastructure is structured to advantage wealthy nations with training facilities and travel budgets. Exit is theoretically possible but means losing international competition access and athlete development pathways. High extraction, significant suppression through resource asymmetry.
constraint_indexing:constraint_classification(international_sports_federation_governance, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FEDERATION LEADERSHIP (ROPE) — Senior administrators experience the governance structure as pure coordination: standardizing competition rules across nations, managing technical requirements, organizing global tournaments. Benefits accrue to leadership through salaries, influence, and institutional prestige. Can exit through resignation but institutional positions are valuable. Sees extraction as legitimate administrative cost.
constraint_indexing:constraint_classification(international_sports_federation_governance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WEALTHY NATIONAL FEDERATION (TANGLED ROPE) — Rich nations benefit from hosting rights, television revenues, and having influence over rule-setting (through larger delegate counts). But they also fund infrastructure costs, athlete development programs, and training subsidies — genuine coordination functions. They experience the system as both beneficiary and moderate burden-bearer, with enough power to shape rules in their favor.
constraint_indexing:constraint_classification(international_sports_federation_governance, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: COMMERCIAL BROADCASTER (TANGLED ROPE) — Broadcasters pay substantial rights fees and benefit enormously from event exclusivity and federation-controlled intellectual property. They coordinate global scheduling and logistics with federations. But they also extract viewer attention and advertising revenue while paying broadcast costs. The arrangement combines genuine coordination (scheduling, technical standards, audience development) with asymmetric value capture.
constraint_indexing:constraint_classification(international_sports_federation_governance, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ATHLETES' RIGHTS COALITION (SCAFFOLD) — International athlete advocacy groups, players' unions, and transparency advocates are building alternative governance models (athlete representation on boards, open rule-making processes, independent arbitration). These represent sunset pathways for the current opaque system. The coalition experiences the current structure as temporary — replaceable by democratic governance with institutional inertia being the only barrier. High suppression but with visible exit path.
constraint_indexing:constraint_classification(international_sports_federation_governance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: SPORTS REGULATORY RITUAL (PITON) — Anti-doping protocols, eligibility verification, and competition rule enforcement contain significant performative elements. Testing procedures are standardized but enforcement is inconsistent across nations. Rule books are vast and complex primarily for legitimacy theater rather than functional necessity. The system persists through institutional inertia and athlete acceptance of the ritual, not because the mechanisms reliably achieve their stated goals.
constraint_indexing:constraint_classification(international_sports_federation_governance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, some hierarchy and coordination costs are inherent to global sport: you cannot manage competition at planetary scale without some centralized rule-setting, some verification of athlete eligibility, some scheduling coordination. This perspective risks naturalizing what are actually contingent institutional choices (which actors get voice, how revenue is distributed, who enforces rules) as immutable requirements of international sport. The engine's false summit detector will flag this as naturalization.
constraint_indexing:constraint_classification(international_sports_federation_governance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(international_sports_federation_governance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(international_sports_federation_governance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(international_sports_federation_governance, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(international_sports_federation_governance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(international_sports_federation_governance, TR),
    TR >= 0.70.

:- end_tests(international_sports_federation_governance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over 20 years. The system extracts value from smaller federations and athletes through membership fees, competition restrictions, and governance exclusion. The extraction has increased as broadcasting revenues have concentrated in the hands of federation leadership and wealthy nations. Initial extractiveness (0.35) reflected genuine coordination functions; current level (0.58) reflects layered extraction on top of coordination. Suppression (0.65): High. Barriers to athlete autonomy and smaller federation exit include: cartel control of international competition access, lack of alternative sanctioning bodies, athlete mobility restrictions (representing nation requires federation approval), opaque governance processes, and career dependence. Developing nations face resource suppression (infrastructure costs exceed revenue share). Theater ratio (0.68): Moderate-high. Anti-doping testing and rule enforcement contain significant performative content: massive rulebooks establish legitimacy rather than functional necessity; testing inconsistency suggests ritual rather than consistent verification; competition eligibility criteria have grown complex partly for legitimacy signaling. Open rule-making processes are theatrical — decisions are made in closed sessions and announced as fait accompli.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies between beneficiary and victim positions. Federation leadership and wealthy nations see the system as elegant coordination (Rope/Tangled Rope) — you need someone to manage global schedules, standardize rules, verify athlete eligibility. Individual athletes and smaller federations see extraction and suppression (Snare) — the same rules are experienced as barriers to participation and autonomy. The athletes' rights coalition sees a temporary system with a democratic alternative (Scaffold) — representation, transparency, independent arbitration can replace secretive governance. The analytical observer risks seeing the system as naturally immutable (Mountain) — 'you can't coordinate global sport without hierarchy' — but this naturalizes choices (who decides, how revenue flows, whether participation is equal) that could be different. The piton perspective (institutional ritual) reveals that much of the enforcement complexity is performative — the system persists through habit and acceptance, not because the mechanisms reliably achieve stated goals.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from structural position: athletes (trapped, powerless) have d ≈ 0.95 (maximum target); federation leadership (institutional, arbitrage) have d ≈ 0.05 (beneficiary); developing nation federations (constrained, moderate) have d ≈ 0.65; wealthy nations (mobile, powerful) have d ≈ 0.40. The sigmoid f(d) converts these to effective extraction experience: powerless athletes with d=0.95 experience f(d) ≈ 1.42, amplifying the base extractiveness to χ ≈ 0.82 (full snare). Federation leadership with d=0.05 experience f(d) ≈ -0.12, resulting in negative χ (they experience the constraint as subsidizing their position). Developing nation federations experience moderate χ through constrained exit options. The perspectival gap reflects genuine structural differences in how the constraint operates across positions, not disagreement about a single objective fact.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates the mandatrophy through perspectival multiplicity: the same governance structure is Snare for trapped athletes, Rope for federation leadership, Tangled Rope for wealthy nations funding infrastructure, Scaffold for advocates building alternatives, and Piton for the ceremonial aspect of enforcement. The mandatrophy is not 'which type is correct?' but 'what is the structural multiplicity?' The falsity manifests in the Mountain perspective (natural law view) — claiming governance hierarchy is inherent to international sport naturalizes what are contingent institutional choices. The extractiveness trajectory (rising from 0.35 to 0.58) indicates the constraint is transitioning from coordination-heavy to extraction-heavy: as broadcasting revenues increased, federation leadership concentrated benefits; as rule complexity increased, governance opacity deepened. This drift is the warning signal that the coordinate-extraction hybrid is becoming extraction-dominant. If extractiveness reaches 0.70+, mandate resolution requires either democratic reform (scaffold pathway) or athlete exodus to alternative bodies (federation fragmentation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    athlete_voice_measurement,
    'What constitutes meaningful athlete representation in federation governance — formal board seats, consultative committees, binding arbitration, or veto power over rule changes?',
    'Comparative analysis of federation governance structures with athlete representation; tracking of rule changes proposed vs implemented by athlete representatives; measurement of outcome alignment with athlete interests',
    'If representation is formal but non-binding: athlete voice is performative theater (Piton classification strengthens). If representation is binding and leads to rule changes: scaffold classification is confirmed (governance reform is real pathway).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(athlete_voice_measurement, preference, 'Threshold for meaningful athlete representation in governance').

omega_variable(
    revenue_distribution_fairness,
    'Is the current broadcasting revenue distribution model (concentrating income to wealthy federations and international bodies) justified by coordination costs or is it pure extraction?',
    'Cost accounting of federation operational expenses vs revenue distribution; comparison to alternative distribution models (equal per-capita splits, sliding scale by development index); analysis of whether concentrated distribution incentivizes better coordination',
    'If justified by coordination costs: tangled rope classification is confirmed. If costs don''t explain distribution: snare/extraction classification strengthens for developing nation federations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revenue_distribution_fairness, empirical, 'Whether revenue distribution reflects coordination costs or extraction').

omega_variable(
    doping_enforcement_asymmetry,
    'Are inconsistent anti-doping enforcement outcomes across nations products of genuine technical difficulty or institutional neglect of smaller federation violations?',
    'Comparative analysis of testing frequency, sample analysis quality, disciplinary timelines across federations by wealth and regional influence; detection of correlation between federation wealth and case dismissal rates; investigation of testing access disparities',
    'If technical difficulty: suppression is structural and unavoidable (mountain-like property). If institutional neglect: suppression is enforcement choice and extractive mechanism (snare classification strengthens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doping_enforcement_asymmetry, empirical, 'Root causes of anti-doping enforcement asymmetry').

omega_variable(
    governance_reform_feasibility,
    'Can federation governance be reformed toward democratic accountability without fragmenting into competing international bodies?',
    'Case studies of federations that implemented governance reforms and subsequent athlete satisfaction/exit rates; analysis of counterfactual scenarios where athletes could create alternative sanctioning bodies; study of what organizational changes would induce major national federations to accept reduced dominance',
    'If reform is feasible: scaffold classification is legitimate (sunset pathway exists). If reform causes fragmentation: current system is sticky and landscape may shift to multiple competing systems rather than single reformed federation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(governance_reform_feasibility, conceptual, 'Feasibility of democratic governance reform in international sports federations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(international_sports_federation_governance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isf_tr_t0, international_sports_federation_governance, theater_ratio, 0, 0.52).
narrative_ontology:measurement(isf_tr_t10, international_sports_federation_governance, theater_ratio, 10, 0.6).
narrative_ontology:measurement(isf_tr_t20, international_sports_federation_governance, theater_ratio, 20, 0.68).
narrative_ontology:measurement(isf_tr_t5, international_sports_federation_governance, theater_ratio, 5, 0.56).
narrative_ontology:measurement(isf_tr_t15, international_sports_federation_governance, theater_ratio, 15, 0.64).

% Extraction over time
narrative_ontology:measurement(isf_be_t0, international_sports_federation_governance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(isf_be_t10, international_sports_federation_governance, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(isf_be_t20, international_sports_federation_governance, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(isf_be_t5, international_sports_federation_governance, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(isf_be_t15, international_sports_federation_governance, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(international_sports_federation_governance, enforcement_mechanism).
narrative_ontology:affects_constraint(international_sports_federation_governance, olympic_host_city_extraction).
narrative_ontology:affects_constraint(international_sports_federation_governance, athlete_intellectual_property_appropriation).
narrative_ontology:affects_constraint(international_sports_federation_governance, doping_detection_asymmetry).

% DUAL FORMULATION NOTE:
% International sports federation governance is upstream of specific competition constraints (Olympic hosting, broadcasting rights, doping enforcement). This story models the structural governance arrangement; downstream stories model specific extractive mechanisms operating within this governance framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(international_sports_federation_governance, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
