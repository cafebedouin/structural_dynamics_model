% ============================================================================
% CONSTRAINT STORY: geopolitical_alliance_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geopolitical_alliance_fragmentation, []).

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
 *   constraint_id: geopolitical_alliance_fragmentation
 *   human_readable: Geopolitical Alliance Fragmentation and Member State Extraction
 *   domain: international_relations/strategic_alliances
 *
 * SUMMARY:
 *   Geopolitical alliance fragmentation represents a structural constraint
 *   that operates simultaneously as pure coordination (burden-sharing,
 *   collective defense), asymmetric extraction (hegemonic enforcement,
 *   subordination of interests), temporary scaffolding (transition mechanisms
 *   toward autonomous defense structures), and degraded ritual (alliance
 *   institutions persisting through inertia despite eroding functional
 *   capacity). Member states experience the alliance constraint differently
 *   depending on their structural position: hegemonic leaders experience it
 *   as enabling coordination; burden-bearing client states experience it as
 *   extractive lock-in; rising powers experience mixed
 *   coordination-extraction; free-riders exploit asymmetries; coalitions
 *   building alternatives see a sunset. The constraint's theater ratio has
 *   increased over 20 years as the original security justification (Cold War
 *   bipolarity) has dissolved while the institutional structures remain,
 *   maintained through ceremonial reaffirmation rather than functional
 *   necessity. The measurable increase in theater (0.42 to 0.68) and
 *   extractiveness (0.35 to 0.58) reflects the accumulation of enforcement
 *   costs and expansion of alliance scope without corresponding clarification
 *   of member benefit distribution.
 *
 * KEY AGENTS:
 *   - Hegemonic Alliance Leader (institutional/arbitrage): Primary beneficiary — maintains political legitimacy, military logistics, and distributed enforcement capacity through alliance; experiences constraint as coordination mechanism enabling extended influence at reduced cost
 *   - Embedded Client States (powerless/trapped): Primary victims — structurally dependent on alliance security guarantees with no credible exit options; bear disproportionate military spending and alignment discipline; locked in through military basing, intelligence integration, and supply chain dependencies
 *   - Mid-Tier Allied States (organized/constrained): Secondary victims with some negotiating power — benefit from collective defense and market integration but constrained by expectations of policy alignment and military contribution; increasingly exploring autonomous defense alternatives
 *   - Free-Rider States (institutional/arbitrage): Tertiary beneficiaries — extract security guarantees and market benefits without proportional burden; arbitrage threat perceptions to play multiple sides; alliance mechanism prevents expulsion because mutual dependence makes credible sanctions impossible
 *   - Rising Powers (powerful/constrained): Ambivalent members — gain technology and market access but experience extraction as constraints on regional dominance and strategic autonomy; face escalating enforcement pressure as power grows
 *   - Coalition for Alliance Reform (organized/constrained): Organized actors (EU, QUAD member states) building alternative coordination structures with explicit exit pathways and sunset logic; see fragmentation as transition toward autonomous capability rather than failure
 *   - Cold War Alliance Architecture (institutional/arbitrage): Institutional inertia — formal structures persist through ceremonial reaffirmation despite changed conditions; maintains itself through sunk legitimacy costs and fear of collapse, not through ongoing functional value
 *   - Analytical Observer (analytical/analytical): Civilizational view risking naturalization of contingent institutional choices as immutable structural features
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geopolitical_alliance_fragmentation, 0.58).
domain_priors:suppression_score(geopolitical_alliance_fragmentation, 0.65).
domain_priors:theater_ratio(geopolitical_alliance_fragmentation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geopolitical_alliance_fragmentation, extractiveness, 0.58).
narrative_ontology:constraint_metric(geopolitical_alliance_fragmentation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(geopolitical_alliance_fragmentation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geopolitical_alliance_fragmentation, tangled_rope).
narrative_ontology:human_readable(geopolitical_alliance_fragmentation, "Geopolitical Alliance Fragmentation and Member State Extraction").
narrative_ontology:topic_domain(geopolitical_alliance_fragmentation, "international_relations/strategic_alliances").

domain_priors:requires_active_enforcement(geopolitical_alliance_fragmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geopolitical_alliance_fragmentation, hegemonic_power).
narrative_ontology:constraint_beneficiary(geopolitical_alliance_fragmentation, free_rider_states).
narrative_ontology:constraint_victim(geopolitical_alliance_fragmentation, alliance_burden_bearers).
narrative_ontology:constraint_victim(geopolitical_alliance_fragmentation, ideological_dissidents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMBEDDED CLIENT STATE (SNARE) — Structurally dependent on alliance security guarantees; military basing, intelligence sharing, and supply chain integration create material barriers to exit. The state bears disproportionate enforcement costs (military deployment, alignment with hegemon's geopolitical preferences) while constrained to accept alliance discipline. No credible alternative security architecture available; exit is theoretically possible but economically catastrophic. Maximum suppression through lock-in mechanisms.
constraint_indexing:constraint_classification(geopolitical_alliance_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER ALLIED STATE (TANGLED ROPE) — Organized enough to negotiate terms; benefits from collective defense and economic integration (market access, technology transfer) alongside extraction (military spending commitments, policy constraints, exposure to hegemon's conflicts). Can threaten credible exit (see: European states exploring autonomous defense), which gives negotiating power but also raises suppression as alliance leadership escalates enforcement and narrative framing to prevent defection.
constraint_indexing:constraint_classification(geopolitical_alliance_fragmentation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: HEGEMONIC ALLIANCE LEADER (ROPE) — Experiences the alliance as coordination: burden-sharing mechanisms (burden-sharing arrangements, standard-setting, intelligence pooling) enable joint action at lower cost than the hegemon could achieve unilaterally. Net beneficiary from the alliance structure through political legitimacy, military logistics, and distributed enforcement of global interests. Can exit whenever power calculations shift without structural penalty.
constraint_indexing:constraint_classification(geopolitical_alliance_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FREE-RIDER STATE (ROPE) — Benefits from alliance security guarantees (nuclear umbrella, conventional deterrence, intelligence sharing) without bearing proportional costs (military spending, deployment obligations, alignment discipline). Can arbitrage the threat environment — maintain formal alliance status while pivoting toward alternative powers and extracting security benefits from both. Low suppression because the alliance mechanism creates mutual dependence that prevents credible threat of expulsion.
constraint_indexing:constraint_classification(geopolitical_alliance_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: COALITION FOR ALLIANCE REFORM (SCAFFOLD) — Organized actors (European Union, QUAD states) are building alternative coordination structures (European defense initiatives, regional security agreements) with explicit sunset logic. The existing alliance remains relevant during the transition, but the reform coalition sees a genuine exit pathway as autonomous capabilities mature. Theater ratio is moderate because the coalition performs legitimacy (NATO interoperability standards, joint exercises) while building real structural alternatives. Suppression declines as exit becomes credible.
constraint_indexing:constraint_classification(geopolitical_alliance_fragmentation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: COLD WAR ALLIANCE ARCHITECTURE (PITON) — The formal structure of post-WWII military alliances (NATO, bilateral security treaties, integrated commands) persists largely through institutional inertia despite geopolitical conditions that originally justified it. The constraint maintains itself through ceremonial reaffirmation (summit statements, treaty renewals, interoperability exercises) while the functional coordination it once enabled has partly atrophied. Theater ratio (0.68) reflects high performative content: NATO summits stage concern about burden-sharing while the underlying extraction mechanism (commitment to collective defense regardless of cost-benefit) persists unchanged. The piton persists because no state dares unilaterally dissolve the institution (sunk legitimacy costs), not because the mechanism remains optimally functional.
constraint_indexing:constraint_classification(geopolitical_alliance_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: RISING POWER AMBIVALENT MEMBER (TANGLED ROPE) — State with growing military and economic capacity that benefits from alliance membership (technology access, market integration, military coordination) but experiences extraction as constraints on independent action (prohibition on regional dominance, expectations of alignment against rivals, restrictions on weapons systems). As power grows, exit costs decline but enforcement pressure intensifies. The state is both beneficiary (gains from collective goods) and victim (constrained autonomy). Exit is theoretically mobile but politically constrained by sunk legitimacy investments.
constraint_indexing:constraint_classification(geopolitical_alliance_fragmentation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY VIEW (MOUNTAIN) — From a civilizational perspective, alliance formation and fragmentation reflect immutable features of international anarchy: the security dilemma requires coordination; information asymmetries prevent perfect contracts; the hegemon must enforce terms; member states must balance autonomy against security. The cyclical fragmentation of alliances appears as a natural law of geopolitics, as inevitable as gravity. This perspective risks naturalizing contingent institutional choices (U.S. hegemonic strategy, NATO expansion, first-use nuclear doctrine) as structural necessities.
constraint_indexing:constraint_classification(geopolitical_alliance_fragmentation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geopolitical_alliance_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(geopolitical_alliance_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geopolitical_alliance_fragmentation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(geopolitical_alliance_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(geopolitical_alliance_fragmentation, TR),
    TR >= 0.70.

:- end_tests(geopolitical_alliance_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting asymmetric burden distribution and hegemonic enforcement. The constraint is not maximal extraction (0.90+) because genuine coordination benefits accrue to all members (collective defense, intelligence pooling, interoperability), but extraction is substantial because enforcement asymmetry is real — client states bear disproportionate costs. The increase from 0.35 to 0.58 over 20 years reflects institutional expansion (scope creep from collective defense to democracy promotion, China containment, cyber defense) without corresponding clarification of which members benefit from which functions. Suppression (0.65): High. Member states face multiple exit barriers: military integration (basing, logistics), intelligence dependence, economic interdependence, legitimacy costs of unilateral withdrawal, and lack of credible alternative security architecture for smaller states. However, suppression is not total (0.90+) because rising powers and European states possess sufficient autonomy to credibly threaten exit. Theater ratio (0.68): Increasingly high, reflecting performative institutional maintenance. NATO summits stage concern about burden-sharing while maintaining extraction mechanisms unchanged. Member states perform commitment through summit attendance and treaty reaffirmation while privately negotiating for reduced obligations. The increase from 0.42 to 0.68 reflects growing gap between ceremonial reaffirmation and actual functional coordination — the alliance's original security justification (Soviet threat) has dissolved, but institutional structures persist through inertia. Claimed type (Tangled Rope): The constraint genuinely coordinates collective defense while also extracting from burden-bearers. It possesses both coordination function (intelligence sharing, early warning, deterrence by numbers) and asymmetric extraction (hegemonic enforcement, client subordination, free-riding). Without the coordination function, it would be pure snare; without the extraction, it would be pure rope. The analysis at the analytical observer level risks false mountain classification (seeing alliance dynamics as immutable features of international anarchy) when the architecture is actually contingent institutional choice.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits the full range of DR types from identical base metrics. This gap is diagnostic. The embedded client state's snare perspective is not 'wrong' — they are genuinely experiencing maximum extraction. The hegemonic leader's rope perspective is not 'wrong' — they are genuinely coordinating through the mechanism. The analytical observer's mountain perspective IS false — it naturalizes contingent institutional choices as laws of nature. The piton perspective accurately captures that the alliance's theater has increased while functional necessity has declined. The scaffold perspective is neither false hope nor inevitable — it is a credible structural possibility that depends on European autonomous capability actually materializing. The perspectival gap reveals that 'fragmentation' is not a single classification question but a measurement of how different observers' experience of the constraint diverges. Maximum fragmentation occurs when the beneficiary sees rope, the victim sees snare, and the analytical observer falsely sees mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation differs critically by institutional perspective. Two institutional actors occupy different structural positions: (1) Hegemonic leader: benefits from alliance coordination, experiences low or negative extraction, can exit at low cost because alternative power structures exist (unipolarity provides options). Derived d ≈ 0.05, producing χ ≈ negative (subsidy). (2) Mid-tier allied state: both benefits and bears extraction, experiences constrained exit because alternatives (European autonomous defense) are aspirational but not fully credible. Derived d ≈ 0.60, producing χ ≈ 0.45 (tangled rope range). (3) Embedded client state: bears extraction with no credible alternative, trapped by integration and lack of scale. Derived d ≈ 0.95, producing χ ≈ 0.85 (snare threshold). The beneficiary/victim declarations encode this: hegemonic_power is beneficiary; alliance_burden_bearers are victims. The engine derives different directionality for each institutional actor from the same declaration because exit options are context-specific (each institution faces different exit costs) and power levels are constraint-relative (power to exit alliance differs from power to influence alliance policy).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits full mandatrophy resolution through indexed classification. The mandatrophy question is not 'which classification is correct?' but 'which structural position reveals which truth?' The hegemonic leader's rope perspective is genuine coordination experience. The victim's snare perspective is genuine extraction experience. Both are correct relative to their positions. The piton perspective is genuine institutional inertia observation. The scaffold perspective is genuine credible exit pathway (European defense autonomy). The analytical observer's mountain is a false summit — the constraint is contingent institutional choice, not immutable structural law. The mandatrophy is resolved not by choosing one type but by acknowledging that the constraint IS all six types simultaneously, each perceived from a specific structural position. The extraction mechanism (hegemonic enforcement, burden-shifting) IS genuine and IS coordinating collective defense simultaneously — these are not contradictory. The perspectival gap is the answer, not a problem to be solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_guarantee_credibility,
    'How credible is the hegemon''s security guarantee when extended deterrence costs rise? At what point does the guarantee become non-credible and reshape alliance geometry?',
    'Historical analysis of extended deterrence failures (e.g., failure to honor guarantees in past crises); game-theoretic models of commitment costs; empirical testing of allied confidence in deterrent through defense spending and strategic positioning',
    'If guarantee remains credible: suppression remains high, extraction persists. If guarantee loses credibility: suppression collapses, alliance fragments, multiple states pursue autonomous security.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_guarantee_credibility, empirical, 'Credibility threshold of extended deterrence guarantees').

omega_variable(
    burden_sharing_measurement_ambiguity,
    'Is burden-sharing measured by military spending percentage, risk exposure (casualties), capability provision, or diplomatic constraint? Each metric produces different distributions of extraction.',
    'Comparative burden analysis across metrics; assess which metric most closely tracks state willingness to remain in alliance; test whether states negotiate based on preferred metric',
    'Metric choice determines which states appear as burden-bearers vs free-riders. Switching metrics changes the victim/beneficiary classification. Extraction flows may invert depending on measurement frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(burden_sharing_measurement_ambiguity, conceptual, 'Ambiguity in burden-sharing measurement metrics').

omega_variable(
    ideological_alignment_lock_in,
    'To what degree is alliance membership maintained by genuine shared interests versus internalized identity commitment (self-concept as ''Western'' or ''allied'' states) that persists despite shifting material incentives?',
    'Compare alliance stability in phases of high vs low material benefit; test whether ideological framing (''democratic values'') predicts commitment independent of security payoff; examine dissidents'' degree of identity fusion with alliance membership',
    'If primarily material: alliance fragments as interests shift (snare/tangled rope). If substantially identity-locked: alliance persists despite extraction because members cannot imagine themselves outside the frame (rope/piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_alignment_lock_in, empirical, 'Identity lock-in versus material interest in alliance persistence').

omega_variable(
    alternative_security_architecture_feasibility,
    'Can alternative security architectures (European autonomous defense, regional multilateralism, balance-of-power coalitions) credibly provide equivalent security at lower suppression cost? Or are they aspirational narratives masking continued dependence?',
    'Capability assessment: do alternative systems provide early warning, rapid response, nuclear deterrence, logistics? Cost analysis: do they reduce military spending burden or merely redistribute it? Stress test: do they withstand major power conflict without falling apart?',
    'If feasible: scaffold perspective confirmed, sunset is real, fragmentation becomes choice rather than failure. If infeasible: ''exit'' remains theoretical, suppression persists, scaffold is false hope narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_security_architecture_feasibility, empirical, 'Feasibility of alternative security architectures as genuine exits').

omega_variable(
    extraction_mechanism_disguise,
    'Is high theater ratio (0.68) hiding pure extraction (snare) behind coordination framing? Or does genuine coordination function justify the theater?',
    'Test alliance behavior during crisis: when coordination failures occur, do member states double down on institutional compliance (theater masks extraction) or renegotiate institutional terms (theater marks genuine coordination)? Compare costs of within-alliance burden to outside-alliance costs.',
    'If theater masks extraction: piton perspective is self-deception, constraint is actually snare. If theater marks coordination: piton perspective is accurate, institutional ritual serves real stabilization function despite degradation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_disguise, empirical, 'Whether theater masks pure extraction or reflects genuine coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geopolitical_alliance_fragmentation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geop_tr_t0, geopolitical_alliance_fragmentation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(geop_tr_t10, geopolitical_alliance_fragmentation, theater_ratio, 10, 0.55).
narrative_ontology:measurement(geop_tr_t20, geopolitical_alliance_fragmentation, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(geop_be_t0, geopolitical_alliance_fragmentation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(geop_be_t10, geopolitical_alliance_fragmentation, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(geop_be_t20, geopolitical_alliance_fragmentation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geopolitical_alliance_fragmentation, enforcement_mechanism).
narrative_ontology:affects_constraint(geopolitical_alliance_fragmentation, nuclear_extended_deterrence).
narrative_ontology:affects_constraint(geopolitical_alliance_fragmentation, nato_expansion_feedback).
narrative_ontology:affects_constraint(geopolitical_alliance_fragmentation, great_power_competition).

% DUAL FORMULATION NOTE:
% Alliance fragmentation decomposes into multiple constraints: (1) nuclear_extended_deterrence (ε=0.25, Mountain — credibility of nuclear guarantee is a structural feature of physics/strategy), (2) burden_sharing_mechanism (ε=0.58, Tangled Rope — coordination of defense spending with asymmetric extraction), (3) institutional_persistence (ε=0.35, Piton — ceremonial maintenance of Cold War structures). This story focuses on burden_sharing_mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geopolitical_alliance_fragmentation, institutional, 0.05).
constraint_indexing:directionality_override(geopolitical_alliance_fragmentation, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
