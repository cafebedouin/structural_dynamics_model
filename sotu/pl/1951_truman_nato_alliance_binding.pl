% ============================================================================
% CONSTRAINT STORY: 1951_truman_nato_alliance_binding
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1951_truman_nato_alliance_binding, []).

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
 *   constraint_id: 1951_truman_nato_alliance_binding
 *   human_readable: NATO Alliance Binding: Institutional Coordination Against Isolation Strategy
 *   domain: foreign_policy/security/institutional_design
 *
 * SUMMARY:
 *   NATO embeds a permanent institutional binding of Western democratic
 *   states into a collective security structure designed to prevent Soviet
 *   isolation strategy. The constraint functions as both coordination
 *   mechanism (solving the problem of coordinated deterrence) and extraction
 *   mechanism (concentrating American military hegemony and binding European
 *   nations into American-led security structure). The Truman administration
 *   argument is that Soviet strategy succeeds only through isolating the U.S.
 *   and absorbing allies piecemeal — the alliance makes isolation impossible
 *   by creating mutual defense obligations, shared military command, and
 *   credible extended deterrence. The structural constraint operates across
 *   42 years of Cold War (1949-1991) and persists into the post-Cold War
 *   period, shifting from original deterrent function toward institutional
 *   inertia and mission drift. The constraint exhibits all six classification
 *   types from different perspectives, making it a diagnostic case for
 *   understanding how institutional binding mechanisms embed both
 *   coordination and extraction.
 *
 * KEY AGENTS:
 *   - Western European States: Primary beneficiary collective (powerful/mobile) — gain security guarantee, burden-sharing, collective deterrence without abandoning sovereignty
 *   - United States: Primary beneficiary and extractor (institutional/constrained) — secures hegemonic position in Western bloc, maintains forward military presence, gains nuclear leverage, but assumes disproportionate military and political burden
 *   - Soviet Union / Soviet Expansion Strategy: Primary target/victim (institutional/arbitrage) — loses primary strategic tool (isolation of allies), forced into symmetric arms race, expansion capacity curtailed by credible mutual defense
 *   - Democratic Allies (Japan, Australia, Korea, Turkey): Secondary beneficiary (institutional/arbitrage) — participate in extended deterrence without bearing primary defense costs
 *   - European Integration Movement: Alternative institutional pathway (powerful/mobile) — envision post-NATO European autonomous defense replacing American-led structure
 *   - NATO Institutional Apparatus: Institutional inertia actor (institutional/constrained) — persists post-Cold War through bureaucratic momentum despite degraded original function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional design as structural inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1951_truman_nato_alliance_binding, 0.38).
domain_priors:suppression_score(1951_truman_nato_alliance_binding, 0.32).
domain_priors:theater_ratio(1951_truman_nato_alliance_binding, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1951_truman_nato_alliance_binding, extractiveness, 0.38).
narrative_ontology:constraint_metric(1951_truman_nato_alliance_binding, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(1951_truman_nato_alliance_binding, theater_ratio, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1951_truman_nato_alliance_binding, tangled_rope).
narrative_ontology:human_readable(1951_truman_nato_alliance_binding, "NATO Alliance Binding: Institutional Coordination Against Isolation Strategy").
narrative_ontology:topic_domain(1951_truman_nato_alliance_binding, "foreign_policy/security/institutional_design").

domain_priors:requires_active_enforcement(1951_truman_nato_alliance_binding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1951_truman_nato_alliance_binding, western_european_states).
narrative_ontology:constraint_beneficiary(1951_truman_nato_alliance_binding, north_american_states).
narrative_ontology:constraint_beneficiary(1951_truman_nato_alliance_binding, democratic_allies).
narrative_ontology:constraint_victim(1951_truman_nato_alliance_binding, soviet_expansion_capacity).
narrative_ontology:constraint_victim(1951_truman_nato_alliance_binding, american_sovereign_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WESTERN EUROPEAN MEMBER STATES (ROPE) — Powerful collective actors with mobile exit options (European integration, neutral status possible but costly). Perceive NATO primarily as coordination mechanism: mutual defense multiplies deterrent effect far beyond what individual national defense could achieve. Benefits: security guarantee against Soviet invasion, burden-sharing on defense costs, collective command structure prevents fragmented response. Extraction: minimal — alliance distributes costs and benefits relatively symmetrically across members. High functional integration; low theater ratio.
constraint_indexing:constraint_classification(1951_truman_nato_alliance_binding, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 2: UNITED STATES STRATEGIC COMMAND (TANGLED ROPE) — Organized institutional actor with constrained exit (abandoning NATO risks credibility across Pacific alliance structure and European strategic position). Genuine coordination function: alliance consolidates Western bloc military capability and political coherence. But also asymmetric extraction: U.S. bears disproportionate military burden (NATO becomes 'America's army in Europe'), commits nuclear guarantee (extended deterrence), and absorbs political risk of defending Europe. High active enforcement requirement: U.S. maintains forward basing, nuclear umbrella, and supreme command authority. Perceived as both burden and necessity — extraction disguised as leadership.
constraint_indexing:constraint_classification(1951_truman_nato_alliance_binding, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: SOVIET EXPANSION STRATEGY (SNARE) — Institutional actor (Soviet state apparatus pursuing expansionist doctrine). Perceives NATO as pure extraction mechanism that eliminates its primary strategic tool: the ability to isolate and absorb allies piecemeal. The alliance creates irreversible mutual defense obligations that make Soviet sphere-of-influence strategy impossible. Maximum extraction from this perspective: NATO directly targets and neutralizes Soviet strategic capacity. Arbitrage exit options available (arms race, blockade, nuclear brinkmanship) but costly and asymmetric — the snare works precisely because it forecloses the isolation strategy.
constraint_indexing:constraint_classification(1951_truman_nato_alliance_binding, snare,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEMOCRATIC ALLIES / EXTENDED SECURITY PERIMETER (ROPE) — Institutional actors with arbitrage options (could shift alliances, pursue neutrality, or balance between blocs). Perceive NATO as coordination mechanism that extends collective deterrence beyond Europe: the alliance's deterrent capacity protects the broader democratic order including Pacific security architecture. Benefits: security guarantees against Soviet proxy expansion, participation in Western institutional framework, trade and technological integration. Extraction: minimal relative to benefit — the alliance is structured to coordinate rather than extract from peripheral members.
constraint_indexing:constraint_classification(1951_truman_nato_alliance_binding, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EUROPEAN INTEGRATION MOVEMENT / SUPRANATIONAL ALTERNATIVE (SCAFFOLD) — Powerful agents (European federalists, institutional designers) with mobile exit from NATO toward integrated European defense and political union. See NATO as temporary coordination structure with an implicit sunset: as Europe integrates politically and militarily (European Defense Community, Western European Union, eventual EU common defense), the need for American extended deterrence declines. NATO's extraction mechanism (U.S. military dominance, American veto over European security decisions) becomes unnecessary once European states achieve sufficient integrated power. Theater is low (genuine deterrent function), suppression is moderate (integration barriers exist but are surmountable). Scaffold classification reflects the vision of a post-NATO Europe with equivalent security but European-autonomous deterrence.
constraint_indexing:constraint_classification(1951_truman_nato_alliance_binding, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: NATO INSTITUTIONAL APPARATUS / LEGACY STRUCTURE (PITON) — Institutional inertia perspective (looking from post-1991 endpoint). NATO's original function — preventing Soviet isolation of Western Europe and projecting collective deterrence — became moot when Soviet threat dissolved. Yet NATO persists through institutional momentum: alliance has expanded eastward, assumed out-of-area missions (Afghanistan, Libya), and evolved into general-purpose Western military institution. Theater ratio high: much NATO activity is performative (exercises, consultation, burden-sharing rhetoric without matching capability commitments). Extraction persists in new forms: continued American military dominance, burden-shifting to newer members, assumption of imperial policing roles. The alliance's functional deterrent purpose degraded; the institutional apparatus remains.
constraint_indexing:constraint_classification(1951_truman_nato_alliance_binding, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — From a civilizational/universal analytical perspective, some form of collective security binding among threatened democracies is a structural necessity: when facing a powerful adversary with isolationist strategy, free nations MUST coordinate defense or face sequential absorption. Alliance formation is not a contingent policy choice but a logical imperative. The constraint appears as immutable structural law: given the power distribution and threat environment of 1945-1991, NATO-like institutions were inevitable. However, the structural data reveals this as a false summit candidate: NATO's specific institutional design (American command dominance, nuclear umbrella, binding indefinitely) reflects beneficiary interests (U.S. hegemonic position), not natural law. Alternative institutional designs (collective command, balanced burden-sharing, sunset clauses) would also coordinate security.
constraint_indexing:constraint_classification(1951_truman_nato_alliance_binding, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1951_truman_nato_alliance_binding_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(1951_truman_nato_alliance_binding, TR),
    TR >= 0.70.

:- end_tests(1951_truman_nato_alliance_binding_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. NATO generates genuine coordination benefits (collective deterrence multiplies individual deterrent capacity, reduces duplicative defense spending, enables coordinated strategy) AND asymmetric extraction (U.S. military dominance preserved, American command authority, burden concentration on peripheral members relative to strategic benefit). The value reflects the mixture: not high enough to classify as pure Snare (extraction ratio is not extreme), but high enough to exceed pure Rope (asymmetry is real). The measurement trajectory shows slight accumulation over Cold War (0.32 → 0.38) as American power deepens institutional control. Suppression (0.32): Moderate. Member states face exit costs (NATO commitment is binding, withdrawal carries political and military costs, alternatives to alliance protection are costly), but suppression is not extreme — members retain nominal sovereignty and some negotiating position. Soviet suppression is higher (blocked from isolation strategy, forced into costly arms race), but Soviet perspective is victim rather than target of measurement. Theater ratio (0.28): Low-moderate, increasing toward 1991. In 1949-1970, NATO exercises genuine deterrent function with high operational authenticity — forces are deployed, command structures test real warfighting readiness, alliance coordination solves coordination problems with real consequences. By 1991, theater increases as original function (deterring Soviet invasion of Western Europe) becomes obsolete, yet institutional apparatus persists through inertia. Post-1991 theater would spike (NATO becomes primarily symbolic security provider, reassurance instrument, and vehicle for American military hegemony without clear deterrent function against dissolved threat).
 *
 * PERSPECTIVAL GAP:
 *   Western European perspective and U.S. perspective diverge sharply on extraction ratio. European states experience genuine security multiplication without feeling dominated (alliance benefits outweigh costs). U.S. military command experiences asymmetric leverage but also bears asymmetric burden (forward basing, nuclear commitment, first-strike risks). The gap is bridged by the alliance's genuine coordination function — all members benefit from deterrence, so extraction component can be tolerated as 'burden of leadership.' The false summit risk in the analytical perspective is acute: the observer who sees 'structural inevitability' has imported beneficiary framing (Truman administration rhetoric, Cold War consensus narratives, containment doctrine as natural strategy) into the analysis. The alternative framing — alliance as contingent hegemonic institution — is equally analytically sound but requires recognizing that beneficiary interests shaped which institutional design was adopted.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) track each agent's structural position. Western European states: beneficiary status + mobile exit + balanced power → d ≈ 0.30, derives moderate coordination interpretation. U.S. institutional command: mixed beneficiary (hegemonic advantage) + constrained exit (credibility at stake) + powerful position → d ≈ 0.35-0.40, derives Tangled Rope (coordination with extraction). Soviet expansion strategy: pure victim (isolation strategy eliminated) + arbitrage exit (costly countermeasures available) + powerful but disadvantaged position → d ≈ 0.85, derives Snare (maximum extraction from this perspective). European federalists: mobile exit + powerful position + beneficiary of deterrence but victim of autonomy constraints → d ≈ 0.25-0.35, derives Scaffold (sees temporary constraint with sunset). NATO bureaucracy: victim of function loss + constrained exit (institutional persistence through inertia) + institutional power → d ≈ 0.55, derives Piton (degradation with persistence). Analytical observer: institutional-level analysis + global scope → canonical d ≈ 0.72, produces Mountain classification that FSM can evaluate as false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   Standard mandatrophy resolution: the constraint is Tangled Rope (has both coordination and extraction components at measurable scales). The preventing error is assuming it's pure Rope (denying extraction) or pure Snare (denying coordination). Tangled Rope is the correct classification because: (1) beneficiaries are present (Western European states, democratic allies, U.S. institutional interests); (2) victims are present (Soviet expansion capacity, European autonomy, smaller member states bearing burden concentration); (3) active enforcement is required (NATO maintains command structure, force deployments, mutual defense obligations); (4) χ coefficient is intermediate (0.38 × f(d) × σ(S) produces effective extraction in 0.40-0.80 range depending on observer perspective). The false summit risk is that the analytical observer naturalizes the beneficiary's narrative (alliance is necessary, inevitable, unchosen) into a mountain. Resolving this requires recognizing: (a) alternative institutional designs existed; (b) the design chosen maximized American advantages; (c) beneficiary interests shaped institutional form. The constraint is real and binding, but its specific shape is not structurally inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    soviet_threat_perception_authenticity,
    'Was the perceived Soviet threat to Western Europe genuine existential threat or constructed through diplomatic and rhetorical amplification?',
    'Declassified Soviet planning documents; comparative analysis of Soviet military capability vs. NATO threat assessment; archival evidence of Stalin''s actual expansionist intent in 1945-1950',
    'If genuine threat: NATO classification as Rope/Tangled Rope is appropriate — alliance solves real coordination problem. If constructed: NATO reclassifies as Snare — the alliance extracts from members (commitment of resources, sovereignty constraints) to solve a threat that was partly manufactured by alliance advocates. American institutional beneficiary creates the threat narrative to justify extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(soviet_threat_perception_authenticity, empirical, 'Authenticity of Soviet threat perception and expansionist intent').

omega_variable(
    isolation_strategy_alternative_credibility,
    'Could Western Europe have resisted Soviet expansion without NATO through alternative mechanisms (national deterrence, European federation, non-aligned status)?',
    'Counterfactual analysis using game-theoretic models of credible commitment; examination of non-aligned states'' success in resisting Soviet pressure (Yugoslavia, Austria); analysis of whether European federation minus American extended deterrence would have been viable',
    'If viable alternatives existed: NATO''s claim to be solving a unique coordination problem is weaker; extraction component becomes more salient. If alternatives were structurally implausible: NATO''s Rope classification is reinforced — coordination genuinely required American commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(isolation_strategy_alternative_credibility, empirical, 'Whether alternative security coordination mechanisms could have addressed isolation strategy').

omega_variable(
    american_hegemonic_motivation,
    'To what degree did U.S. elite actors pursue NATO binding to secure American hegemonic position in Western bloc, versus pure deterrence of Soviet expansion?',
    'Archival analysis of State Department and NSC planning documents; examination of American negotiating positions on burden-sharing and command structure; comparison of proposed alliance designs that would have distributed power more equitably vs. final design chosen',
    'If hegemonic motivation is dominant: U.S. extraction component of Tangled Rope is understated; constraint should reclassify with higher extractiveness and higher χ from U.S. perspective. If deterrence is dominant: current Tangled Rope classification is appropriate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(american_hegemonic_motivation, empirical, 'Degree of American hegemonic motivation vs. pure deterrence in NATO formation').

omega_variable(
    burden_sharing_symmetry_actual,
    'Did NATO''s burden-sharing mechanisms actually distribute costs equitably, or was extraction concentrated on smaller/weaker members throughout the Cold War?',
    'Quantitative analysis of military spending per capita and as percentage of GDP by member state; examination of NATO force structure and command authority concentration; comparison of defense technology transfer and military aid flows',
    'If burden-sharing was equitable: Rope classification confirmed — coordination without extraction. If concentrated on smaller members: reclassify member state perspectives toward Snare — victims bearing disproportionate costs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(burden_sharing_symmetry_actual, empirical, 'Actual symmetry or asymmetry of burden-sharing across NATO members').

omega_variable(
    european_autonomy_constraint_intentional,
    'Was the constraint on European autonomous military decision-making (NATO command structure, nuclear veto) a necessary technical requirement or a deliberate institutional choice to preserve American leverage?',
    'Analysis of alternative command structures proposed during NATO formation; examination of whether distributed European command would have been technically feasible; comparison with other military alliances'' governance structures',
    'If necessary technical requirement: Tangled Rope classification stands. If deliberate leverage design: extractiveness should increase; constraint reclassifies toward higher χ — American institutional actor deliberately constrains European autonomy to preserve dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(european_autonomy_constraint_intentional, empirical, 'Whether European autonomy constraints were technical necessity or hegemonic design choice').

omega_variable(
    post_1991_mission_drift_inevitable,
    'Was NATO''s mission expansion after Soviet collapse (out-of-area operations, Eastern expansion, global security actor role) an inevitable bureaucratic drift, or a deliberate strategic choice to preserve American hegemonic institution?',
    'Analysis of NATO decision-making 1991-2010; examination of alternative proposals (NATO dissolution, regional devolution); correlation between alliance expansion and preservation of American global military infrastructure',
    'If inevitable drift: Piton classification appropriate — institutional inertia degrades function. If deliberate strategy: the constraint''s current form is Snare dressed as Piton — extraction continues post-Cold War but under new justifications (terrorism, failed states, expansionism), making the institutional apparatus indispensable to American power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_1991_mission_drift_inevitable, empirical, 'Whether post-1991 NATO mission expansion was inevitable bureaucratic drift or deliberate hegemonic strategy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1951_truman_nato_alliance_binding, 0, 42).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nato_theater_1949, 1951_truman_nato_alliance_binding, theater_ratio, 0, 0.15).
narrative_ontology:measurement(nato_theater_1959, 1951_truman_nato_alliance_binding, theater_ratio, 10, 0.22).
narrative_ontology:measurement(nato_theater_1969, 1951_truman_nato_alliance_binding, theater_ratio, 20, 0.28).
narrative_ontology:measurement(nato_theater_1991, 1951_truman_nato_alliance_binding, theater_ratio, 42, 0.45).

% Extraction over time
narrative_ontology:measurement(nato_extract_1949, 1951_truman_nato_alliance_binding, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(nato_extract_1959, 1951_truman_nato_alliance_binding, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(nato_extract_1969, 1951_truman_nato_alliance_binding, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(nato_extract_1991, 1951_truman_nato_alliance_binding, base_extractiveness, 42, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1951_truman_nato_alliance_binding, enforcement_mechanism).
narrative_ontology:affects_constraint(1951_truman_nato_alliance_binding, warsaw_pact_alliance_formation).
narrative_ontology:affects_constraint(1951_truman_nato_alliance_binding, nuclear_extended_deterrence).
narrative_ontology:affects_constraint(1951_truman_nato_alliance_binding, european_sovereignty_constraint).

% DUAL FORMULATION NOTE:
% NATO alliance binding is upstream of several downstream constraints: Warsaw Pact formation is a direct structural response (mirror constraint with reversed polarity); nuclear extended deterrence is the mechanism by which deterrent multiplication operates; European sovereignty constraint is the institutional side effect (nations lock into American-led decision structure). These form a constraint family with NATO as the structurally prior binding mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(1951_truman_nato_alliance_binding, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
