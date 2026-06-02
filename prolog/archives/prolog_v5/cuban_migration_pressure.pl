% ============================================================================
% CONSTRAINT STORY: cuban_migration_pressure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cuban_migration_pressure, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cuban_migration_pressure
 *   human_readable: Cuban Migration Pressure and US-Cuba Border Control Dynamics
 *   domain: geopolitical/immigration/economic
 *
 * SUMMARY:
 *   Cuban migration pressure represents a structural constraint created by
 *   the intersection of economic collapse in Cuba, US embargo policy, and
 *   geopolitical interests of both states. The constraint exhibits tangled
 *   rope characteristics: genuine coordination problems (family
 *   reunification, humanitarian response) coexist with asymmetric extraction
 *   (Cuban state uses emigration restrictions to control population; US uses
 *   migration as geopolitical leverage; privileged Cubans benefit from
 *   remittances while poor Cubans bear suppression costs). The extractiveness
 *   has increased over the 20-year interval as Cuban economic conditions
 *   deteriorated (post-Soviet collapse through 2008 financial crisis through
 *   COVID tourism collapse), while theater ratio has remained moderate,
 *   reflecting that humanitarian framing and Cold War security framing
 *   coexist rather than one completely replacing the other. The constraint
 *   involves active enforcement at multiple levels: Cuban government
 *   restricts passport issuance and prohibits travel; US government manages
 *   acceptance quotas and asylum adjudication; both states control the
 *   migration flow for domestic political purposes.
 *
 * KEY AGENTS:
 *   - Cuban Economically Displaced: Primary victim (powerless/trapped) — bears suppression through travel restrictions, passport denial, economic coercion; no internal exit option
 *   - Cuban State Apparatus: Primary beneficiary (institutional/constrained) — relieves unemployment pressure, channels discontent, maintains external enemy narrative; constrained by economic limitations but benefits from controlling exit valve
 *   - US Political Establishment: Primary beneficiary (institutional/arbitrage) — receives economically productive migrants, uses migration as geopolitical leverage over Cuba, maintains humanitarian legitimacy
 *   - Cuban Diaspora / Family Networks: Secondary victim and coordinator (organized/constrained) — provide economic and information support (coordination function) but bear separation costs and remittance constraints (extraction)
 *   - US Border Communities: Secondary victim (moderate/constrained) — absorb immediate housing, education, healthcare costs; benefit long-term from economic integration of migrants
 *   - Regional Humanitarian Frameworks: Organized mediators (organized/constrained) — attempt to solve migration pressure through structured processing and development funding; constrained by limited resources
 *   - Cold War Institutional Machinery: Inertial beneficiary (institutional/arbitrage) — security agencies and Cold War policy continuity benefit from threat framing; sees own function as degraded (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cuban_migration_pressure, 0.62).
domain_priors:suppression_score(cuban_migration_pressure, 0.68).
domain_priors:theater_ratio(cuban_migration_pressure, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cuban_migration_pressure, extractiveness, 0.62).
narrative_ontology:constraint_metric(cuban_migration_pressure, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(cuban_migration_pressure, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cuban_migration_pressure, tangled_rope).
narrative_ontology:human_readable(cuban_migration_pressure, "Cuban Migration Pressure and US-Cuba Border Control Dynamics").
narrative_ontology:topic_domain(cuban_migration_pressure, "geopolitical/immigration/economic").

domain_priors:requires_active_enforcement(cuban_migration_pressure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cuban_migration_pressure, us_political_interests).
narrative_ontology:constraint_beneficiary(cuban_migration_pressure, cuban_state_apparatus).
narrative_ontology:constraint_beneficiary(cuban_migration_pressure, cuban_privileged_classes).
narrative_ontology:constraint_victim(cuban_migration_pressure, cuban_economically_displaced).
narrative_ontology:constraint_victim(cuban_migration_pressure, us_border_communities).
narrative_ontology:constraint_victim(cuban_migration_pressure, humanitarian_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CUBAN ECONOMICALLY DISPLACED (SNARE) — Trapped by economic collapse (USD dollarization, remittance dependency, tourism economy collapse post-COVID, agricultural sector decline). Suppressed by legal prohibition on emigration (Ley de Emigración), denial of passports, travel restrictions. No internal exit option (economic mobility within Cuba is severely constrained). Migration becomes the only viable exit, but is heavily coerced and criminalized. Maximum experienced extraction — trapped population bears all cost of the migration pressure system.
constraint_indexing:constraint_classification(cuban_migration_pressure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CUBAN DIASPORA / FAMILY UNITS (TANGLED ROPE) — Constrained by separation costs, legal barriers to remittance flows, and immigration bureaucracy. However, family networks provide genuine coordination function (information about migration pathways, economic support for journey, employment networks in diaspora). Asymmetric extraction persists: families bear costs of separation and remittance taxation; the system (both Cuban state and US enforcement) benefits from diaspora economic integration while maintaining border pressure. Mixed coordination and extraction.
constraint_indexing:constraint_classification(cuban_migration_pressure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: US POLITICAL ESTABLISHMENT / BORDER CONTROL (ROPE) — Experiences migration pressure as a coordination mechanism requiring managed response. Net beneficiary: US receives economically productive migrants (net fiscal positive over lifetime), border security apparatus gains funding and political justification, humanitarian crises create negotiating leverage over Cuban government. The constraint coordinates immigration policy, US-Cuba geopolitical relations, and domestic political narratives. Arbitrage exit: US can modulate acceptance rates, policy frameworks, and enforcement intensity without material constraint.
constraint_indexing:constraint_classification(cuban_migration_pressure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CUBAN STATE APPARATUS (TANGLED ROPE) — Constrained by economic limitations (inability to provide full employment, dollars, consumer goods). Benefits from migration pressure: relieves unemployment pressure, channelizes discontent into exit rather than internal rebellion, sustains ideology of external enemy (US embargo as explanation for scarcity). Asymmetric extraction: state apparatus extracts from economically displaced through travel restrictions and remittance controls while using migration pressure to consolidate internal control. Requires active enforcement (prohibition, visa denial, family separation mechanisms).
constraint_indexing:constraint_classification(cuban_migration_pressure, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: REGIONAL HUMANITARIAN FRAMEWORKS (SCAFFOLD) — Organized actors (IOM, UNHCR, regional migration compacts) view the migration pressure as a temporary coordination failure solvable through structured asylum processing, international burden-sharing, and economic development. Theater ratio moderately low (0.55) because humanitarian organizations emphasize functional solutions over performative border theater. Sunset clause inherent: as economic conditions in Cuba improve or as regional integration deepens (hypothetical), migration pressure declines. Current sunset estimate: 15-25 years conditional on economic reform in Cuba or sustained regional development funding.
constraint_indexing:constraint_classification(cuban_migration_pressure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: COLD WAR INSTITUTIONAL FRAMING (PITON) — The classification of Cuban migration as a geopolitical security threat persists through institutional inertia despite the Cold War's end (1991). Theater ratio elevated (0.55 baseline, rising to 0.72 when Cold War framing dominates discourse). The threat narrative (Cubans as security risk, regime destabilizers) is substantially performative — most Cuban migrants are economically motivated, not ideological. The institutional machinery (special legal status for Cuban migrants, anti-trafficking rhetoric) continues because it was created during Cold War and hasn't been substantially reformed. Piton classification reflects that the functional verification (are Cubans actually security threats?) has degraded, but the institutional machinery persists.
constraint_indexing:constraint_classification(cuban_migration_pressure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, economic disparity between adjacent nation-states creates immutable migration pressure — this is presented as a natural law of economics/geography. However, structural data contradicts the mountain classification. The constraint is contingent on specific policies (US embargo, Cuban price controls, dollarization), not inherent to geography. The 'natural law' framing naturalizes political choices and masks how the constraint maintains itself through enforcement, not through immutable structural limits. Engine's false summit detector will identify this as a naturalization artifact.
constraint_indexing:constraint_classification(cuban_migration_pressure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cuban_migration_pressure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cuban_migration_pressure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cuban_migration_pressure, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cuban_migration_pressure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cuban_migration_pressure, TR),
    TR >= 0.70.

:- end_tests(cuban_migration_pressure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The constraint extracts significantly from the economically displaced (suppression prevents internal mobility, forces dangerous migration) and from diaspora networks (separation costs, remittance taxation). However, extractiveness is not maximum (0.80+) because migration does occur, economic integration happens in diaspora, and remittances provide real economic value despite taxation. The increase from 0.35 to 0.62 over the interval reflects worsening Cuban economic conditions and increased enforcement intensity. Suppression (0.68): High. Legal restrictions (Ley de Emigración, passport controls), economic coercion (dollarization forcing dependence on dollars/remittances), and enforcement capacity create severe barriers to exit. However, suppression is not total (0.95+) because some migration succeeds and some Cubans have arbitrage options (professional visas, family sponsorship). Theater ratio (0.55): Moderate-high. Cold War framing (Cubans as security threats) is substantially performative — most migrants are economically motivated, not ideological. However, humanitarian framing is genuine (family reunification is real coordination need). The theater ratio has not risen as dramatically as extractiveness because humanitarian organizations have partially displaced Cold War rhetoric.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates full perspectival divergence across power levels and structural positions. The powerless Cuban sees a snare (suppression with no exit). The Cuban state sees tangled rope (benefits from control while constrained by economic limits). The US political establishment sees rope (coordination of immigration policy with net benefit). Diaspora families see tangled rope (genuine coordination need + extraction). Humanitarian organizations see scaffold (temporary problem with sunset). Cold War institutions see piton (degraded but persistent ritual). The analytical observer risks mountain (naturalizing geopolitical constraint as immutable). The perspectival gaps reveal that the constraint's type is not objective but depends entirely on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from beneficiary/victim declarations plus exit options per the derivation chain. Economically displaced Cubans are victims with trapped exit → d ≈ 0.95. Cuban state is beneficiary with constrained exit (by economic limits, not enforcement) → d ≈ 0.35. US is beneficiary with arbitrage exit → d ≈ 0.10. Diaspora families are victims (separation costs) and partial beneficiaries (remittance income) with constrained exit → d ≈ 0.60. The directionality overrides (none needed) would trigger if analysis revealed that US is actually more constrained than arbitrage suggests (e.g., if domestic politics forces binding acceptance rates). Cuban state's d might be overridden upward if evidence shows the state is also being externally squeezed by embargo.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION via perspectival classification. The constraint could be mislabeled as 'pure coordination' (rope) if viewed only from the US political perspective (managing migration flows). It could be mislabeled as 'pure extraction' (snare) if viewed only from the Cuban economically displaced perspective (suppression with no benefit). The mandatrophy is resolved by declaring that both are legitimate but incomplete. The constraint IS coordination (family reunification, policy management) AND extraction (suppression, asymmetric benefit). The tangled rope classification captures both: genuine coordination function (diaspora networks, humanitarian processing) coexists with asymmetric extraction (Cuba uses restriction to control population; US uses migration as geopolitical leverage). No single type is correct; the presheaf over the indexed positions IS the answer. The false mountain risk (analytical observer naturalizing geopolitical dynamics) is identified by the structural data showing enforcement and policy contingency rather than immutable limits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    us_embargo_counterfactual,
    'How much of Cuban migration pressure is caused by US embargo vs. internal Cuban economic mismanagement vs. structural inequality between adjacent economies?',
    'Counterfactual analysis: comparison of migration rates under different embargo regimes (pre-1962, partial relaxation periods like Obama normalization 2015-2017, hypothetical full lifting); correlation with Cuban economic indicators independent of embargo',
    'If embargo is primary driver (>60%): migration pressure is externally imposed, US bears responsibility for suppression costs. If internal mismanagement dominant: Cuban state bears primary responsibility. Attribution affects whether suppression metrics belong to US or Cuban perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_embargo_counterfactual, empirical, 'Causal attribution of migration pressure to embargo vs. internal factors').

omega_variable(
    us_refugee_policy_sustainability,
    'Can US sustain current Cuban refugee acceptance rates and family reunification policies without domestic political backlash that forces enforcement escalation?',
    'Polling data on US public opinion toward Cuban migration; electoral outcome tracking in swing states with large Cuban diaspora populations; policy stability across administrations; historical precedent analysis',
    'If sustainability is high: US political system can maintain rope/scaffold framings. If sustainability is low: pressure builds for snare-level enforcement (border militarization, acceptance rate collapse), converting US perspective from arbitrage to constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_refugee_policy_sustainability, empirical, 'Political sustainability of US refugee acceptance policies').

omega_variable(
    cuban_economic_reform_threshold,
    'What minimum level of economic improvement in Cuba would reduce migration pressure below replacement rate (i.e., the system becomes sustainable without enforcement)?',
    'Comparative analysis with other post-Soviet transitions (Poland, Vietnam); economic simulation of Cuban reform scenarios; survey data of potential migrants on economic thresholds that would prevent exit',
    'If threshold is achievable (<15 years, <$5K per capita GDP): scaffold sunset is realistic. If threshold is unachievable or distant (>25 years, $10K+ per capita): constraint is endemic, not temporary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cuban_economic_reform_threshold, empirical, 'Economic reform threshold for sustainable migration equilibrium').

omega_variable(
    suppression_mechanism_escalation,
    'Is the measured suppression (0.68) structural (laws, enforcement capacity) or internalized (Cubans'' acceptance of exit barriers as legitimate), and will it persist if structural barriers are removed?',
    'Post-policy-change analysis: if US unilaterally removed special Cuban refugee status, would migration rate surge immediately (structural suppression) or adjust gradually (internalized norms)? Historical precedent: what happened during Obama normalization period (2015-2017)?',
    'If primarily structural: removing US enforcement changes classification significantly (US perspective shifts toward rope, powerless perspective toward constrained). If internalized: suppression is portable across policy changes, reducing classification sensitivity to US policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_escalation, empirical, 'Structural vs. internalized suppression mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cuban_migration_pressure, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cuba_mig_tr_t0, cuban_migration_pressure, theater_ratio, 0, 0.4).
narrative_ontology:measurement(cuba_mig_tr_t10, cuban_migration_pressure, theater_ratio, 10, 0.52).
narrative_ontology:measurement(cuba_mig_tr_t20, cuban_migration_pressure, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(cuba_mig_be_t0, cuban_migration_pressure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cuba_mig_be_t10, cuban_migration_pressure, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(cuba_mig_be_t20, cuban_migration_pressure, base_extractiveness, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cuban_migration_pressure, attachment_coordination).
narrative_ontology:affects_constraint(cuban_migration_pressure, us_embargo_regime).
narrative_ontology:affects_constraint(cuban_migration_pressure, caribbean_regional_stability).
narrative_ontology:affects_constraint(cuban_migration_pressure, remittance_dependency_cycle).

% DUAL FORMULATION NOTE:
% Cuban migration pressure decomposes along observable boundaries. The family reunification coordination (attachment_coordination type, ε ≈ 0.30, rope/scaffold) is structurally distinct from the geopolitical extraction mechanism (identity_coordination type, ε ≈ 0.62, snare/tangled_rope). This story treats them as one constraint because they are causally coupled and institutional actors experience them as unified. Decomposition into separate stories would require distinct enforcement mechanisms and different temporal horizons.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
