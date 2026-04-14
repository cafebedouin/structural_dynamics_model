% ============================================================================
% CONSTRAINT STORY: global_disinformation_infrastructure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_disinformation_infrastructure, []).

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
 *   constraint_id: global_disinformation_infrastructure
 *   human_readable: Global Disinformation Infrastructure
 *   domain: information_systems/political_economy
 *
 * SUMMARY:
 *   The global disinformation infrastructure represents a coordinated
 *   extraction system where state actors, private technology platforms, and
 *   political movements deploy false narratives to capture attention,
 *   engineer belief systems, and degrade the shared epistemic commons. The
 *   constraint extracts legitimacy, cognitive resources, and political agency
 *   from targeted populations while providing coordination benefits to
 *   platform operators and intelligence services. The infrastructure operates
 *   through three interlocking mechanisms: (1) algorithmic amplification that
 *   prioritizes engagement over accuracy, (2) state-sponsored narrative
 *   campaigns targeting specific demographic vulnerabilities, and (3)
 *   coordinated inauthentic behavior (bot networks, astroturf movements,
 *   synthetic identity networks). The extractiveness has increased over the
 *   decade as computational sophistication, targeting precision, and platform
 *   integration have deepened. Theater has remained moderate — the
 *   infrastructure operates largely invisibly to target populations, with
 *   minimal performative legitimation. The constraint exhibits all eight
 *   indexed perspectives, revealing fundamental tensions between platform
 *   governance (who sees coordination), targeted populations (who see
 *   extraction), and epistemic intermediaries (who see mixed benefit and
 *   cost).
 *
 * KEY AGENTS:
 *   - Targeted Populations: Primary victims (powerless/trapped) — demographic groups selected for coordinated false narratives (ethnic minorities, political opposition, low-media-literacy regions, elderly users); bear extraction costs without viable exit
 *   - Epistemic Commons: Primary victim (powerless/trapped) — abstract collective good of shared factual reality; degraded by false narratives, poisoned sources, and lost trust
 *   - State Actors with Information Warfare Capacity: Primary beneficiary (powerful/mobile) — Russia, China, Iran, other state intelligence services that weaponize disinformation for political interference and cognitive dominance; capture geopolitical advantage during verification windows
 *   - Private Technology Platforms: Secondary beneficiary (institutional/arbitrage) — Meta, Google, Twitter, TikTok extract advertising value and user engagement from amplified disinformation; coordinate content moderation as governance function
 *   - Political Movements and Partisan Organizations: Secondary beneficiary (organized/arbitrage) — domestic political actors using disinformation to mobilize supporters and degrade opponent credibility; capture electoral advantage through coordinated amplification
 *   - Fact-Checking Infrastructure: Secondary actor (powerful/mobile) — Truth-checking services, verification platforms, epistemic intermediaries that coordinate truth-tracking while extracting authority from monopoly over verification
 *   - Democratic Resistance Movements: Tertiary actor (organized/constrained) — Civil society, media literacy nonprofits, anti-disinformation coalitions coordinating resistance while constrained by resource inequality and platform dependence
 *   - Legacy News Ecosystem: Institutional actor (institutional/arbitrage) — Traditional journalism maintaining vestigial gatekeeping authority through institutional prestige (piton perspective); sees itself as degraded but persists through inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_disinformation_infrastructure, 0.68).
domain_priors:suppression_score(global_disinformation_infrastructure, 0.72).
domain_priors:theater_ratio(global_disinformation_infrastructure, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_disinformation_infrastructure, extractiveness, 0.68).
narrative_ontology:constraint_metric(global_disinformation_infrastructure, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(global_disinformation_infrastructure, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_disinformation_infrastructure, snare).
narrative_ontology:human_readable(global_disinformation_infrastructure, "Global Disinformation Infrastructure").
narrative_ontology:topic_domain(global_disinformation_infrastructure, "information_systems/political_economy").

domain_priors:requires_active_enforcement(global_disinformation_infrastructure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_disinformation_infrastructure, state_actors_with_information_warfare_capacity).
narrative_ontology:constraint_beneficiary(global_disinformation_infrastructure, private_actors_monetizing_engagement).
narrative_ontology:constraint_beneficiary(global_disinformation_infrastructure, political_movements_using_coordinated_amplification).
narrative_ontology:constraint_victim(global_disinformation_infrastructure, public_epistemic_commons).
narrative_ontology:constraint_victim(global_disinformation_infrastructure, democratic_deliberation_processes).
narrative_ontology:constraint_victim(global_disinformation_infrastructure, targeted_populations).
narrative_ontology:constraint_victim(global_disinformation_infrastructure, marginalized_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED POPULATION (SNARE) — Individuals within targeted demographics (ethnicity, geography, political affiliation, income level) face coordinated disinformation campaigns with no viable escape. They cannot opt out of the information ecosystem; they cannot collectively dismantle the infrastructure; they cannot verify competing claims without specialized media literacy and computational analysis tools. Suppression operates through epistemic overload — too many competing narratives, no institutional authority to arbitrate truth, and active poisoning of all information sources. Maximum experienced extraction.
constraint_indexing:constraint_classification(global_disinformation_infrastructure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EPISTEMIC COMMONS (SNARE) — The shared domain of facts, evidence, and reasoning that enables democratic deliberation has no agent to defend it and no exit option. Disinformation infrastructure extracts trust from the commons and replaces it with coordinated falsehood. At generational timescale, the damage is structural — an entire generation may grow up in epistemic fragments with no coherent shared reality. This perspective bears maximal extraction with zero agency.
constraint_indexing:constraint_classification(global_disinformation_infrastructure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: FACT-CHECKING INFRASTRUCTURE (TANGLED ROPE) — Fact-checkers, verification platforms, and epistemic intermediaries coordinate in providing truth-tracking (genuine coordination function) while simultaneously extracting legitimacy from their monopoly on verification authority. They have mobile exit options — institutional capacity to shift domains or dissolve — but remain locked in because the coordination function (providing some truth-tracking) genuinely serves targets. Moderate extraction with real coordination benefits.
constraint_indexing:constraint_classification(global_disinformation_infrastructure, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM GOVERNANCE COALITION (ROPE) — Meta, Google, Twitter content moderation teams see the constraint as a coordination problem they are solving: establishing shared standards for identifying disinformation, coordinating takedowns, enforcing label policies. They experience the constraint as pure coordination with minimal extraction costs — the labor is allocated to them, the infrastructure investment is justified by legitimacy gains. Net beneficiary position.
constraint_indexing:constraint_classification(global_disinformation_infrastructure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DEMOCRATIC RESISTANCE MOVEMENTS (TANGLED ROPE) — Civil society organizations, anti-disinformation nonprofits, media literacy initiatives coordinate collectively (genuine function: building resistance) while also being extracted from as concentrated funding and visibility flow to well-resourced organizations. They face high costs to exit (defunding, loss of infrastructure) but genuine coordination benefits from shared platforms and evidence databases. Constrained mobility with real coordination benefits and asymmetric extraction.
constraint_indexing:constraint_classification(global_disinformation_infrastructure, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL EPISTEMIC GOVERNANCE INITIATIVES (SCAFFOLD) — UN information integrity initiatives, international fact-checking networks, and global media literacy standards represent a temporary coordination layer with a realistic sunset: as computational provenance systems, blockchain verification, and AI-assisted source attribution mature, the epistemic commons can be reconstructed with lower dependence on human-mediated fact-checking. Theater is moderate — some performative international consensus, but genuine investment in technical alternatives. Sunset clause is real: 15-20 year horizon for distributed verification to mature.
constraint_indexing:constraint_classification(global_disinformation_infrastructure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: LEGACY NEWS ECOSYSTEM (PITON) — Traditional journalism and broadcast news maintain institutional authority and gatekeeping power despite decreased functional verification capacity. The constraint (institutional credibility maintaining information order) operates through inertia and theatrical ritual (evening news, byline authority, institutional prestige) rather than through superior truth-tracking. Theater ratio is high — the legitimacy inheres in form and institutional identity, not in epistemic content. The ecosystem sees itself as degraded and struggles to justify its authority, but persists because alternatives haven't fully displaced it.
constraint_indexing:constraint_classification(global_disinformation_infrastructure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (SNARE from information asymmetry frame) — From a civilizational/analytical perspective, disinformation infrastructure is a natural consequence of information asymmetry: any actor with superior coordination capacity can inject false narratives into an epistemic commons where truth-tracking costs exceed verification capacity for distributed agents. This perspective risks naturalizing as immutable law what is actually a contingent feature of current communication architecture. The engine's false summit detector should flag this as illegitimate mountain classification — information systems are redesignable.
constraint_indexing:constraint_classification(global_disinformation_infrastructure, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_disinformation_infrastructure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_disinformation_infrastructure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_disinformation_infrastructure, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_disinformation_infrastructure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_disinformation_infrastructure, TR),
    TR >= 0.70.

:- end_tests(global_disinformation_infrastructure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. State actors and platforms capture significant value from disinformation infrastructure — geopolitical advantage, engagement metrics, user data, political power. The trajectory shows accelerating extraction as computational targeting and algorithmic amplification have matured (0.35 → 0.74 over interval). The 0.68 current value reflects that extraction is severe but not total — some verification capacity exists (fact-checking, cross-platform comparison, international oversight), preventing total information collapse. Suppression (0.72): High. Epistemic overload (competing false narratives), institutional degradation (trust collapse in media, fact-checkers, science institutions), asymmetric information access, and platform lock-in create substantial barriers to collective action or individual exit. Targeted populations face coordinated suppression that activates multiple mechanisms simultaneously: information scarcity (alternative sources blocked), information excess (false narratives amplified), cognitive capture (identity-congruent falsehoods), and resource asymmetry (verification costs exceed individual capacity). Theater ratio (0.58): Moderate. The infrastructure operates with low performative content — it functions invisibly, with minimal need for theatrical legitimation. Victims do not perceive the mechanism as institutional ritual; they experience it as 'just how information is.' Beneficiaries (platforms, states) do perform governance theater (content moderation updates, fact-check partnerships) but the core extraction mechanism requires minimal theatrical maintenance.
 *
 * PERSPECTIVAL GAP:
 *   State actors experience the constraint as rope (pure coordination of information dominance). Platforms experience it as rope (governance and engagement coordination). Targeted populations experience it as snare (extraction with zero alternatives). Fact-checkers experience it as tangled rope (coordinate truth-seeking while monopolizing verification authority). Resistance movements experience it as scaffold (temporary constraint being solved by media literacy). Legacy news experiences it as piton (degraded ritual persisting through inertia). The analytical observer at global scale risks false mountain (naturalizing as inevitable). This is a diagnostic exemplar of how power position determines classification: those who benefit see coordination, those who bear costs see extraction, and the same structural feature (algorithmic amplification of engaging content) appears as either or both depending on position.
 *
 * DIRECTIONALITY LOGIC:
 *   State actors with information warfare capacity (d ≈ 0.05): Beneficiaries with arbitrage options. They can deploy disinformation or shift to other influence mechanisms. f(d) ≈ -0.12. Platforms (d ≈ 0.08): Beneficiaries with arbitrage options for content moderation strategy. f(d) ≈ -0.01. Targeted populations (d ≈ 0.92): Victims with trapped exit options. They cannot exit the epistemic ecosystem or verify competing claims. f(d) ≈ 1.38. Fact-checkers (d ≈ 0.45): Mixed — they coordinate verification (beneficiary function) while monopolizing authority (extraction function). f(d) ≈ 0.58. Resistance movements (d ≈ 0.60): Victims with constrained exit options. They fight the infrastructure but face resource asymmetry. f(d) ≈ 0.85. The chi formula scales extractiveness by these f(d) values plus scope: at global scope σ(S) = 1.2, state actor extraction chi becomes negative (they receive subsidies), platform chi becomes slightly negative, victim chi approaches maximum (1.38 × 0.68 × 1.2 ≈ 1.13). The scope modifier reveals why global disinformation is a higher-order snare than local false information: scale amplifies both the coordinator's capacity and the target's helplessness.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint resolves the mandatrophy by identifying the snare classification as primary and the tangled_rope and scaffold classifications as either reform-oriented perspectives or structural illusions. The snare is the analytical ground truth: disinformation infrastructure functions primarily to extract (political advantage, engagement value, epistemic power) from targeted populations with minimal coordination benefit that could not be achieved through transparent mechanisms. The tangled rope perspective (fact-checkers coordinating truth-seeking) conflates necessary defensive response with inherent constraint function — fact-checking does not prove the infrastructure provides genuine coordination; it proves the infrastructure requires active counter-extraction to maintain the epistemic commons. The scaffold perspective (media literacy and computational provenance as sunset mechanisms) is structural and real — epistemic reconstruction via distributed verification systems is technically feasible — but it does not change the current classification: the constraint is presently a snare, with a realistic if distant exit path. The piton perspective (legacy news as degraded ritual) correctly identifies institutional inertia but misclassifies it as a separate phenomenon: the news degradation is caused by the snare, not independent of it. The analytical false mountain (information asymmetry as natural law) is illegitimate: information systems are redesignable. The mandatrophy resolves to: (1) snare is primary, (2) reform via fact-checking and media literacy provides partial suppression reduction without eliminating extraction, (3) structural exit exists via computational provenance and distributed epistemic reconstruction at 15-20 year horizon, (4) current policy should prioritize (a) transparency requirements in algorithmic amplification, (b) state accountability for information warfare, (c) investment in decentralized verification infrastructure, (d) media literacy in targeted populations, and (5) false mountains that naturalize current architecture should be explicitly rejected in policy discourse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_vs_private_actor_distinction,
    'Are state-sponsored disinformation campaigns and algorithmic amplification of false content by private platforms functionally equivalent extraction mechanisms, or structurally distinct constraints?',
    'Comparative analysis of coordination requirements, beneficiary groups, and suppression mechanisms between state-directed campaigns and private-platform-enabled campaigns. Identify whether they require different interventions.',
    'If equivalent: single snare constraint applies globally. If distinct: decompose into state_disinformation_campaigns and algorithmic_amplification_extraction with separate ε values and network links.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_vs_private_actor_distinction, empirical, 'Whether state and private disinformation mechanisms are structurally equivalent').

omega_variable(
    computational_provenance_maturity_timeline,
    'What is the realistic timeline for distributed cryptographic source attribution and computational provenance systems to mature sufficiently to enable verification without centralized fact-checking intermediaries?',
    'Technical roadmap analysis from cryptography, blockchain, and AI research communities; pilot deployment tracking for provenance systems; timeline comparison across jurisdictions.',
    'If 5-10 years: scaffold sunset is realistic, institutional fact-checking becomes transitional. If 20+ years or infeasible: scaffold is aspirational, snare persists longer, alternative mechanisms required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_provenance_maturity_timeline, empirical, 'Timeline for computational provenance maturity').

omega_variable(
    collective_action_threshold_for_targeted_populations,
    'At what point can targeted populations organize sufficient collective capacity to coordinate alternative information sources, or does the targeting mechanism prevent coalition formation?',
    'Historical analysis of disinformation-targeted group organizing; network analysis of information source diversity within targeted communities; measurement of resource access for alternative platform building.',
    'If coalition formation is feasible: targeted populations move from trapped to constrained, classification shifts toward tangled rope. If targeting prevents coalition: trapped status persists, snare is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_threshold_for_targeted_populations, empirical, 'Feasibility of coalition formation among targeted populations').

omega_variable(
    suppression_mechanism_internalization,
    'To what extent is measured suppression (epistemic overload, competing narratives, verification costs) structural versus internalized in target populations as learned helplessness or epistemic nihilism?',
    'Post-intervention suppression trajectory tracking: if suppression persists after information access barriers are removed, indicates internalized component. Comparative analysis across populations with different baseline epistemic self-efficacy.',
    'If primarily structural: removing infrastructure barriers reduces suppression. If primarily internalized: requires cognitive/identity intervention as well as infrastructure change. Higher internalization means higher effective suppression than measured structural value suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    mandatrophy_resolution_snare_vs_tangled_rope,
    'Does the disinformation infrastructure function purely to extract (snare), or does it provide genuine coordination benefits (tangled rope) that justify some asymmetric extraction?',
    'Counterfactual analysis: in a scenario without disinformation infrastructure, what coordination problems remain unsolved? Are content moderation standards, narrative coherence, and information ordering genuine public goods? Or pure extractive mechanisms?',
    'If pure extraction (snare): constraint should be dismantled entirely. If genuine coordination exists (tangled rope): constraint requires reform to reduce extraction while preserving coordination. Determines policy intervention type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_resolution_snare_vs_tangled_rope, conceptual, 'Whether disinformation infrastructure provides coordination benefits or pure extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_disinformation_infrastructure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(disinfo_tr_t0, global_disinformation_infrastructure, theater_ratio, 0, 0.38).
narrative_ontology:measurement(disinfo_tr_t3, global_disinformation_infrastructure, theater_ratio, 3, 0.48).
narrative_ontology:measurement(disinfo_tr_t6, global_disinformation_infrastructure, theater_ratio, 6, 0.58).
narrative_ontology:measurement(disinfo_tr_t10, global_disinformation_infrastructure, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(disinfo_be_t0, global_disinformation_infrastructure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(disinfo_be_t3, global_disinformation_infrastructure, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(disinfo_be_t6, global_disinformation_infrastructure, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(disinfo_be_t10, global_disinformation_infrastructure, base_extractiveness, 10, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_disinformation_infrastructure, information_standard).
narrative_ontology:affects_constraint(global_disinformation_infrastructure, algorithmic_amplification_engagement_optimization).
narrative_ontology:affects_constraint(global_disinformation_infrastructure, state_information_warfare_capability_development).
narrative_ontology:affects_constraint(global_disinformation_infrastructure, platform_advertising_revenue_concentration).
narrative_ontology:affects_constraint(global_disinformation_infrastructure, democratic_deliberation_infrastructure_degradation).

% DUAL FORMULATION NOTE:
% Global disinformation infrastructure decomposes into multiple structurally distinct constraints: (1) algorithmic amplification (ε ≈ 0.55, tangled rope — genuine engagement coordination with asymmetric extraction), (2) state information warfare (ε ≈ 0.72, snare — pure extraction), (3) platform monopoly on verification (ε ≈ 0.48, tangled rope — coordination + authority extraction), and (4) epistemic commons degradation (ε ≈ 0.80, snare — pure extraction from abstract collective). This story treats the integrated system; downstream stories address component mechanisms separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_disinformation_infrastructure, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
