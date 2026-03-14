% ============================================================================
% CONSTRAINT STORY: alliance_burden_sharing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_alliance_burden_sharing, []).

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
 *   constraint_id: alliance_burden_sharing
 *   human_readable: Alliance Burden Sharing Asymmetry
 *   domain: geopolitical/military_coordination
 *
 * SUMMARY:
 *   Alliance burden-sharing creates a structural asymmetry where collective
 *   security benefits are distributed unevenly, with smaller and
 *   geographically dependent members bearing disproportionate costs relative
 *   to their capacity and security threat, while larger powers and hegemonic
 *   actors capture disproportionate benefits while maintaining exit
 *   credibility. This constraint exhibits genuine coordination functions
 *   (mutual defense, deterrence, forward basing, intelligence integration)
 *   alongside extractive mechanisms (free-riding, cost externalization,
 *   geopolitical leverage). The classification as Tangled Rope reflects that
 *   both functions are structurally necessary — the alliance solves real
 *   security problems while simultaneously structuring costs asymmetrically.
 *   Theater ratio of 0.68 reflects that significant alliance activity is
 *   performative consultation and ritualized coordination rather than active
 *   operational deterrence, particularly during periods of reduced acute
 *   threat perception. The constraint has intensified over the measurement
 *   interval as hegemonic decline reduced the perceived private benefits of
 *   alliance maintenance for the hegemon, increasing pressure on other
 *   members to shoulder costs while the hegemon's commitment theater (NATO
 *   commitment statements, forward deployment reductions) rises as
 *   compensation.
 *
 * KEY AGENTS:
 *   - Dependent Small States: Primary victims (powerless/trapped) — bear disproportionate cost burden relative to GDP and security benefit; geographically trapped with no credible alternative security provider
 *   - Mid-Tier Regional Powers: Secondary victims (moderate/constrained) — carry above-proportional costs; constrained by regional threats and alliance credibility; also benefit from coordination function
 *   - Hegemonic Power: Primary beneficiary (institutional/arbitrage) — captures extended deterrence, forward bases, containment of rivals, geopolitical leverage; maintains exit credibility as threat
 *   - Free-Riding Members: Beneficiaries (institutional/arbitrage) — minimize defense expenditure while capturing security benefits; rely on larger members' commitment
 *   - Reform Coalition: Organized actors (organized/constrained) — NATO defense spending initiatives, burden-sharing formulas, capability development programs building toward redistribution
 *   - Cold War Alliance Infrastructure: Institutional inertia (institutional/arbitrage) — command structures, interoperability standards, forward positioning persist despite changed threat environment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(alliance_burden_sharing, 0.52).
domain_priors:suppression_score(alliance_burden_sharing, 0.58).
domain_priors:theater_ratio(alliance_burden_sharing, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(alliance_burden_sharing, extractiveness, 0.52).
narrative_ontology:constraint_metric(alliance_burden_sharing, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(alliance_burden_sharing, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(alliance_burden_sharing, tangled_rope).
narrative_ontology:human_readable(alliance_burden_sharing, "Alliance Burden Sharing Asymmetry").
narrative_ontology:topic_domain(alliance_burden_sharing, "geopolitical/military_coordination").

domain_priors:requires_active_enforcement(alliance_burden_sharing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(alliance_burden_sharing, free_rider_members).
narrative_ontology:constraint_beneficiary(alliance_burden_sharing, hegemonic_power).
narrative_ontology:constraint_victim(alliance_burden_sharing, burden_bearing_members).
narrative_ontology:constraint_victim(alliance_burden_sharing, alliance_cohesion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT SMALL STATE (SNARE) — Geographically constrained and militarily dependent on alliance security guarantee. Cannot credibly exit without facing direct security threat. Bears disproportionate burden through resource commitment relative to GDP. Maximum experienced extraction — trapped by geography and threat environment.
constraint_indexing:constraint_classification(alliance_burden_sharing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER ALLY (TANGLED ROPE) — Constrained by regional threats and loss of credibility if exiting alliance. Also genuinely benefits from security coordination and deterrence function. Carries above-proportional burden but cannot extract herself from the system. Moderate experienced extraction — high cost, some coordination benefit, constrained exit.
constraint_indexing:constraint_classification(alliance_burden_sharing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HEGEMONIC POWER (ROPE) — Primary beneficiary. Captures security benefits (containment of rivals, forward bases, extended deterrence) while smaller members assume relative cost burden. Can credibly exit (or threaten to) without facing existential threat. Experiences constraint as coordination mechanism enabling preferred geopolitical order.
constraint_indexing:constraint_classification(alliance_burden_sharing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Organized burden-sharing advocates (NATO burden-sharing debates, Indo-Pacific capacity-building initiatives) see the asymmetry as a temporary coordination failure with sunset logic. Burden-sharing formulas, capability development timelines, and cost-sharing agreements are structural reforms intended to phase out free-riding. Low effective extraction because the coalition perceives exit paths and has agency in reshaping the constraint.
constraint_indexing:constraint_classification(alliance_burden_sharing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COLD WAR ALLIANCE STRUCTURE (PITON) — NATO and bilateral US security treaties persist largely through institutional inertia despite changed threat environment. The alliance infrastructure (command structures, interoperability standards, forward positioning, integrated air defense) was designed for Soviet containment and continues through bureaucratic momentum. Theater ratio high because much alliance activity is performative consultation and ritual rather than active deterrence function. Piton classification reflects degraded primary function maintained by institutional gravity.
constraint_indexing:constraint_classification(alliance_burden_sharing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some burden-sharing asymmetry appears inherent to military alliances: larger powers always benefit from coalition structure; smaller powers always depend on them. This perspective frames the imbalance as a law of geopolitics — an immutable feature of how security provision works in anarchic systems. However, the structural data contradicts the mountain classification. The engine will compute this as a false summit, revealing that 'inherent to international relations' naturalizes contingent institutional design choices and power distribution.
constraint_indexing:constraint_classification(alliance_burden_sharing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alliance_burden_sharing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(alliance_burden_sharing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(alliance_burden_sharing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(alliance_burden_sharing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(alliance_burden_sharing, TR),
    TR >= 0.70.

:- end_tests(alliance_burden_sharing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts significant resources from dependent members through disproportionate defense expenditure relative to threat and capacity. However, extraction is not total because genuine coordination functions exist — mutual defense, deterrence through coalition, forward basing that serves all members' interests. The trajectory from 0.35 to 0.52 reflects post-Cold War decline in acute existential threat perception, reducing the apparent coordination benefit while material burden-sharing remains constant. Suppression (0.58): Moderate-high. Dependent members face significant barriers to exit including geographic threat environment, loss of security guarantee, credibility costs, and geopolitical vulnerability to hegemonic exit. But suppression is not total — some members have explored alternatives (Finland's NATO delays, Turkey's strategic ambiguity) and NATO expansion has included voluntary joiners. Theater ratio (0.68): Moderate-high. Much alliance activity (political consultation, burden-sharing negotiations, capability development rhetoric) is performative commitment signaling rather than active operational coordination. However, genuine military integration and deterrent function persist. Theater has increased as threat perception declined and performative commitment became more necessary to maintain alliance cohesion. Active enforcement required because free-rider incentives are substantial and only enforcement mechanisms (political pressure, burden-sharing requirements, capability mandates) maintain contributions.
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer's mountain classification is a false summit. The constraint is not an immutable law of international relations but a contingent institutional choice: NATO could operate as a true collective security system with proportional burden-sharing; alliances could be structured as federations with burden formulae enforced from formation; defense spending could be coordinated around genuine threat assessment rather than political pressure. The 'inherent asymmetry' framing naturalizes a design choice that serves hegemonic interests. The engine's false summit detection will flag this classification as naturalization of contingency.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from each agent's structural relationship to the extraction flow. Dependent small states are full victims with no exit (d ≈ 0.95), producing high f(d) ≈ 1.42. Mid-tier allies are constrained victims (d ≈ 0.65), producing moderate f(d) ≈ 1.00. Hegemonic powers are beneficiaries with arbitrage options (d ≈ 0.10), producing low f(d) ≈ -0.01. Free-riding members are beneficiaries with mobile options (d ≈ 0.20), producing near-zero f(d) ≈ 0.02. The chi formula χ = ε × f(d) × σ(S) scales these by scope: global scope σ = 1.2 amplifies extraction for dependent states (χ ≈ 0.52 × 1.42 × 1.2 ≈ 0.89) while dampening the apparent benefit flow for hegemons (who experience χ as negative because f(d) < 0). This mathematical structure captures the real asymmetry: dependent states experience high χ (high extraction); hegemons experience negative χ (subsidization of coordination). The scope scaling is crucial — alliance burden-sharing's global scope amplifies the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that alliance burden-sharing is genuinely hybrid: it solves real coordination problems (collective deterrence, forward positioning, intelligence sharing) while simultaneously structuring costs asymmetrically. The Tangled Rope classification captures this hybrid structure precisely. Misclassifying it as pure Rope (all beneficiaries and volunteers) ignores the extraction function. Misclassifying it as pure Snare (all victims and coercion) ignores the genuine coordination benefits that all members receive, even dependent ones. The true structure is asymmetric coordination — coordination is necessary, but the institutional design ensures smaller members bear disproportionate costs. The identification of false summit in the analytical observer's mountain perspective reveals the naturalization risk: framing inherited institutional arrangements as laws of nature prevents recognizing design choices that could be reformed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_perception_divergence,
    'Do alliance members perceive the external threat identically, or do divergent threat assessments justify asymmetric burden-sharing?',
    'Comparative threat perception surveys and defense planning documents across members; correlation between perceived threat level and defense expenditure; analysis of threat inflation incentives',
    'If threats are genuinely asymmetric: burden-sharing disparity reflects rational specialization (Mountain-like structure). If threats are similarly perceived but burden shares diverge: extraction mechanism is dominant (Snare from more perspectives). If members are identity-locked to threat narratives: classification shifts to identity_locked exit for some agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_perception_divergence, empirical, 'Whether threat perception divergence justifies asymmetric burden-sharing').

omega_variable(
    exit_credibility_of_hegemony,
    'Can the hegemonic power actually withdraw from the alliance without catastrophic cost, or is it also structurally trapped despite superior position?',
    'Game-theoretic analysis of hegemonic withdrawal scenarios; cost accounting for loss of bases, forward positioning, intelligence sharing, and geopolitical leverage; comparison of actual vs declared exit costs',
    'If hegemonic exit is truly cost-free: hegemony operates as arbitrage (Rope classification confirmed). If hegemonic exit carries hidden structural costs: hegemony is constrained, making the asymmetry less extractive and more coordination-like. Classification may shift to Tangled Rope from hegemonic perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_credibility_of_hegemony, empirical, 'Whether hegemonic power has genuine exit credibility').

omega_variable(
    burden_quantification_manipulation,
    'What counts as ''burden''? Are military expenditures, opportunity costs, sovereignty restrictions, and political costs measured consistently across members, or does the definition serve extractive interests?',
    'Cross-member burden accounting using standardized metrics (% GDP, per-capita defense spending, opportunity cost of foreign policy constraints); identification of measurement asymmetries that inflate or suppress perceived burden',
    'If burden measurement is politically neutral: extractiveness metric is stable (0.52 confirmed). If measurement systematically inflates burden for weak members and downplays costs for hegemony: extractiveness is higher and suppression more structural (shift toward Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(burden_quantification_manipulation, empirical, 'Whether burden metrics are measured consistently or manipulated').

omega_variable(
    identity_lock_in_alliance_commitment,
    'Do smaller members remain in alliance partly because exit would contradict their national identity (Western alignment, democratic community, etc.) rather than because exit is materially impossible?',
    'Longitudinal analysis of how members rationalize commitment; comparison of exit barriers (material) vs identity barriers (cognitive); analysis of rhetoric when members debate alliance participation',
    'If identity-locked: exit_options for moderate power agents should shift from ''constrained'' to ''identity_locked'', changing classification from Tangled Rope to Rope (perceived mutability). Reveals cognitive capture overlaid on structural constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_alliance_commitment, conceptual, 'Whether alliance commitment is identity-locked or materially constrained').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(alliance_burden_sharing, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abs_tr_t0, alliance_burden_sharing, theater_ratio, 0, 0.55).
narrative_ontology:measurement(abs_tr_t15, alliance_burden_sharing, theater_ratio, 15, 0.62).
narrative_ontology:measurement(abs_tr_t30, alliance_burden_sharing, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(abs_be_t0, alliance_burden_sharing, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(abs_be_t15, alliance_burden_sharing, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(abs_be_t30, alliance_burden_sharing, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(alliance_burden_sharing, enforcement_mechanism).
narrative_ontology:affects_constraint(alliance_burden_sharing, nato_article_5_burden).
narrative_ontology:affects_constraint(alliance_burden_sharing, regional_security_dilemma).
narrative_ontology:affects_constraint(alliance_burden_sharing, great_power_strategic_competition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(alliance_burden_sharing, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
