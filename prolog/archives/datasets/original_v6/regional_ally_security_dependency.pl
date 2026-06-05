% ============================================================================
% CONSTRAINT STORY: regional_ally_security_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regional_ally_security_dependency, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: regional_ally_security_dependency
 *   human_readable: Regional Ally Security Dependency
 *   domain: geopolitical/security_alliance
 *
 * SUMMARY:
 *   Regional security dependencies represent one of the most stable
 *   extractive constraints in geopolitics. A powerful guarantor state
 *   provides military security to a weaker ally, creating structural
 *   extraction through dependency: the dependent ally cedes significant
 *   strategic autonomy, accepts military bases and foreign presence, and
 *   aligns foreign policy with the guarantor's interests. The constraint
 *   exhibits characteristics of both pure coordination (genuine security
 *   benefits) and pure extraction (asymmetric power, limited alternatives,
 *   suppression of autonomy). The critical structural feature is the feedback
 *   loop between material dependency and identity formation: as the dependent
 *   state relies on the guarantee for existential security, its national
 *   identity becomes constituted through alliance membership, making exit
 *   unthinkable even when material conditions might permit it. This
 *   constraint manifests differently across power levels: the dependent
 *   state's political leadership experiences it as snare (trapped); the
 *   guarantor's security establishment experiences it as rope (profitable
 *   coordination); the dependent state's military establishment experiences
 *   it as tangled rope (mixed benefits and constraints); organized agents
 *   seeking alternatives experience it as tangled rope with scaffold elements
 *   (a sunset path exists but is costly); the formal alliance bureaucracy
 *   experiences it as piton (performative maintenance); and the
 *   civilizational analyst risks naturalizing it as mountain (inevitable
 *   consequence of power differentials) when it is actually a contingent
 *   institutional arrangement.
 *
 * KEY AGENTS:
 *   - Security Guarantor State: Primary beneficiary (institutional/arbitrage) — extracts geopolitical influence, military base access, alliance loyalty, industrial profits from weapons sales
 *   - Dependent Ally State: Primary victim (powerless/trapped, identity_locked at civilizational scale) — bears asymmetric extraction through constrained autonomy, military subordination, political alignment demands
 *   - Dependent Ally Military Establishment: Secondary victim (moderate/constrained) — experiences tangled rope: benefits from security guarantee and technology transfer but constrained in strategic doctrine and procurement autonomy
 *   - Regional Alternative Coalition: Secondary actor (organized/constrained) — other regional states whose strategic options are constrained by the guarantor's maintenance of the dependent ally
 *   - Strategic Autonomy Movement: Organized reformers (organized/constrained) — see the constraint as having a sunset clause achievable through technology development and military modernization
 *   - Alliance Bureaucracy: Institutional inertia (institutional/arbitrage) — maintains performative alliance structures; exhibits piton characteristics
 *   - Dependent Ally's National Identity: Collective-level actor (powerless/identity_locked) — constituted through alliance membership; cannot imagine autonomy without identity dissolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regional_ally_security_dependency, 0.58).
domain_priors:suppression_score(regional_ally_security_dependency, 0.65).
domain_priors:theater_ratio(regional_ally_security_dependency, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regional_ally_security_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(regional_ally_security_dependency, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(regional_ally_security_dependency, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regional_ally_security_dependency, tangled_rope).
narrative_ontology:human_readable(regional_ally_security_dependency, "Regional Ally Security Dependency").
narrative_ontology:topic_domain(regional_ally_security_dependency, "geopolitical/security_alliance").

domain_priors:requires_active_enforcement(regional_ally_security_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regional_ally_security_dependency, security_guarantor_state).
narrative_ontology:constraint_beneficiary(regional_ally_security_dependency, military_industrial_complex).
narrative_ontology:constraint_victim(regional_ally_security_dependency, dependent_ally_state).
narrative_ontology:constraint_victim(regional_ally_security_dependency, regional_military_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT ALLY STATE (SNARE) — The dependent state faces maximal extraction. Structurally trapped: military capability gap, geographic proximity to threats, and domestic political constraints (public expectation of security guarantee) create insurmountable exit barriers. Cannot credibly defend alone; cannot credibly replace guarantor without massive military buildup; cannot exit without facing invasion or state collapse. Experiences the constraint as pure extraction with suppression mechanism: alternatives (military independence, regional coalition, multipolarity) are presented as impossible or prohibited. Zero degrees of freedom within biographical time horizon.
constraint_indexing:constraint_classification(regional_ally_security_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SECURITY GUARANTOR STATE (ROPE) — Benefits from the dependency. Experiences the constraint as pure coordination: maintaining the alliance is straightforward (provide security guarantees, accept military bases, shape ally's foreign policy). The guarantor has arbitrage options (can withdraw support and reallocate military resources globally) but finds the dependency profitable. Theater is moderate because the coordination function is genuine — the guarantor must actually maintain military presence and readiness. From this perspective, the constraint is Rope: coordination mechanism with asymmetric but compensated benefits.
constraint_indexing:constraint_classification(regional_ally_security_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: DEPENDENT ALLY'S MILITARY ESTABLISHMENT (TANGLED ROPE) — Moderate power agents trapped in a hybrid constraint. The military benefits from guaranteed security and technology transfer but faces asymmetric extraction: career trajectories are constrained by ally-subordination, procurement is dictated externally, strategic doctrine is imported rather than indigenous. Theater reflects genuine coordination (allied military doctrine, interoperability) alongside extractive theater (performance legitimacy to the guarantor, demonstration of loyalty). Biological time horizon reveals mixed dynamics: some exit is possible (military modernization, strategic autonomy investments) but at high cost. The constraint coordinates regional security while extracting military independence.
constraint_indexing:constraint_classification(regional_ally_security_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: REGIONAL ALTERNATIVE COALITION (TANGLED ROPE) — Organized agents (other regional states, defense coalitions) experience the dependency as both coordinating their region and extracting from their agency. The guarantor maintains the dependent ally to preserve regional unipolarity, which constrains the coalition's strategic options. Theater is moderate: real coordination exists (regional stability, deterrence against external threats) but extraction mechanism is embedding dependency into regional power distribution. Coalition has constrained exit options: building alternative security arrangements is possible but requires coordination among multiple states and faces guarantor pressure. Generational time reveals the coalition could gradually build alternatives (regional military integration, technology sharing, multinational exercises) but faces extraction cost during the transition.
constraint_indexing:constraint_classification(regional_ally_security_dependency, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: STRATEGIC AUTONOMY MOVEMENT (SCAFFOLD) — Organized agents within the dependent state (political reformers, military modernizers, technology advocates) view the dependency as a temporary coordination structure with a sunset clause. Their analysis: technology transfer, indigenous defense industry development, and regional military capability shifts will gradually enable exit over 15-30 years. The constraint has a genuine sunset — as the dependent state develops sufficient military capacity and as regional threats evolve, the security guarantee becomes less valuable to both parties. Theater reflects the sunset logic: the alliance maintains performance value (symbolism, integration) while structural extraction gradually declines as alternatives mature. This perspective sees the constraint as transitional rather than permanent.
constraint_indexing:constraint_classification(regional_ally_security_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ALLIANCE BUREAUCRACY (PITON) — The formal alliance structure (NATO/bilateral treaties, joint commands, liaison offices, summit rituals) has become substantially performative. The bureaucracy maintains alliance theater through regular meetings, joint exercises, and public statements of commitment, but the underlying extraction mechanism has atrophied: the dependent state no longer faces the existential threat that justified the original guarantee, yet the alliance persists through institutional inertia. Theater ratio is high (0.60-0.70 range): alliance rituals continue without changing core power relationships. The bureaucracy sees its own function as degraded — it perpetuates the dependency structure more through ceremony than through crisis response.
constraint_indexing:constraint_classification(regional_ally_security_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/global perspective, security dependencies are presented as natural law: geopolitical realities ('power vacuums require guarantors,' 'weak states need strong patrons') that are immutable features of international anarchy. This perspective naturalizes the constraint as an inevitable consequence of power differentials. However, this classification is likely a false summit: the structural data shows dependency is contingent on institutional arrangements (alliance treaties, military base agreements, procurement systems) that could be reorganized. The mountain classification masks the actual tangled_rope and snare structures that depend on particular policy choices by the guarantor state.
constraint_indexing:constraint_classification(regional_ally_security_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 8: DEPENDENT ALLY'S NATIONAL IDENTITY (SNARE with identity_locked exit) — The dependent state's collective identity has become constituted through the alliance relationship. National narratives frame membership in the alliance as the core feature of statehood ('we are an ally of [guarantor]'); political legitimacy is indexed to alliance membership; public discourse assumes the guarantee is unchangeable. Agents within the state are identity_locked: they cannot imagine the state outside the alliance because their conception of national identity includes the dependent status. This is structurally distinct from trapped (external barriers) — the state has some material mobility (could invest in independent military capacity) but cannot exercise it because the identity frame makes autonomy literally unthinkable. The constraint extracts from national agency by colonizing the political imagination. Theater reflects identity maintenance rituals: flag displays, alliance commemoration, loyalty demonstrations.
constraint_indexing:constraint_classification(regional_ally_security_dependency, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regional_ally_security_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regional_ally_security_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regional_ally_security_dependency, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regional_ally_security_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regional_ally_security_dependency, TR),
    TR >= 0.70.

:- end_tests(regional_ally_security_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The guarantor state extracts substantial benefits — geopolitical influence, military base access, technology sales, alliance loyalty — but provides a genuine security service in exchange, preventing the pure snare classification. The extraction is not maximal because some coordination function exists (the guarantee has real value to the dependent state). The measurement trajectory shows extractiveness rising from 0.42 (cold war era, when threat was acute and guarantee was genuinely cost-justified) to 0.58 (contemporary period, where threat environment is less existential but alliance structures persist through institutional inertia). Suppression (0.65): High. Multiple suppression mechanisms operate: (1) material — military capability gap makes independent defense infeasible; (2) institutional — alliance structures embed dependency through procurement chains, command integration, base agreements; (3) narrative — public discourse and political identity normalize the guarantee as unchangeable. Theater ratio (0.52): Moderate-high. The constraint coordinates genuine security cooperation but increasingly operates through performative alliance theater: summit meetings, joint exercises, symbolic demonstrations of commitment, and rhetoric affirming unity. As the threat environment has decreased over 30 years, theater has increased (rising from 0.38 to 0.52), suggesting the constraint's function is shifting from security provision toward political-symbolic maintenance of dependency.
 *
 * PERSPECTIVAL GAP:
 *   The widest perspectival gap appears between the dependent state's snare perspective (we are trapped, we have no exit, we bear all extraction) and the guarantor's rope perspective (this is pure coordination, we provide security, we benefit from the arrangement). Both perspectives are structurally accurate from their vantage points. The dependent state faces genuine material barriers to exit; the guarantor faces genuine coordination benefits. The gap is analytically productive — it reveals how the same constraint structure can be experienced as pure extraction by the victim and as pure coordination by the beneficiary. This is the diagnostic signature of tangled rope masquerading as rope (from the beneficiary view) and tangled rope masquerading as snare (from the victim view). The second-order gap is between the dependent state's powerless/trapped perspective (we cannot exit) and its identity-locked/national identity perspective (we cannot imagine exiting). These are materially the same constraint but with different binding mechanisms: one is about external barriers, the other is about internal identity colonization. The scaffold perspective (exit is achievable over 20-30 years) creates a third gap: it assumes the identity-lock can be broken through political movements and generational change, while the identity-locked perspective assumes the lock is fundamental to state existence.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives primarily from structural positions in the extraction flow. The guarantor state (beneficiary, arbitrage options) gets d ≈ 0.10 — low extraction flow toward them because they can exit (arbitrage) and because they are designed to benefit. The dependent state (victim, trapped options) gets d ≈ 0.90 — high extraction flow from them because they cannot exit and because they are designed to lose. The dependent state's military establishment (moderate power, constrained options, mixed beneficiary-victim status) gets d ≈ 0.55 — symmetric; they benefit from the guarantee but lose autonomy, paying a price in constrained exit options. These d values feed the sigmoid f(d) to produce effective extraction chi experienced by each agent. No directionality overrides are necessary — the structural data (beneficiary/victim declarations plus exit options) produces accurate d values through the canonical derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint's classification as tangled_rope is justified by the presence of both genuine coordination (security guarantee is real and valuable) and asymmetric extraction (the guarantor extracts geopolitical alignment and military base access). The constraint cannot be reduced to snare (which would deny the coordination function) nor to rope (which would deny the asymmetric extraction). The mandatrophy is resolved by the perspectival analysis: from the dependent state's view, extraction is maximal (snare), but from the guarantor's view, coordination is real (rope). The tangled_rope classification emerges at the analytical level where both functions are visible. The theater ratio (0.52) supports this: not so high as to suggest pure extraction theater (snare), but high enough to indicate coordination is degrading into performance. The measurements showing theater rising from 0.38 to 0.52 over 30 years indicate the constraint is undergoing mandatrophy shift — the coordination function is atrophying while institutional structures persist, gradually converting from genuine tangled rope toward piton. This temporal pattern is the key diagnostic: constraints that show rising theater and rising extractiveness simultaneously are in mandatrophy transition. If theater continues rising above 0.70 while extractiveness plateaus or declines, the constraint will classify as piton rather than tangled rope, reflecting the shift from structural extraction to institutional theater maintaining the extraction through inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_military_autonomy,
    'At what level of indigenous military capability does the dependent state become structurally able to exit the alliance without state collapse or invasion?',
    'Military capability metrics (defense spending as percentage of GDP, indigenous weapons development, force projection capacity); comparison with historical case studies (South Korea, Japan, Taiwan trajectories); deterrence theory thresholds for credible independent defense',
    'If threshold is achievable within 20 years: scaffold perspective is realistic and sunset is structural. If threshold requires 40+ years or is theoretically unreachable: dependent state faces permanent snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_military_autonomy, empirical, 'Timeline and capability threshold for military autonomy').

omega_variable(
    guarantor_commitment_credibility,
    'How much of the constraint''s suppression mechanism depends on public belief in the guarantor''s security commitment versus material enforcement capacity?',
    'Analysis of guarantor''s historical reliability (cost of honoring commitments, circumstances under which guarantor has withdrawn); public opinion polling on ally confidence in guarantee; game-theoretic analysis of guarantor''s incentive to withdraw',
    'If credibility is high and materially grounded: suppression reflects genuine material barriers (snare is accurate). If credibility depends on symbolic reassurance: suppression is partly internalized psychological dependence (identity_locked becomes central mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(guarantor_commitment_credibility, empirical, 'Whether suppression is material or credibility-based').

omega_variable(
    identity_lock_reversibility,
    'Can the dependent state''s identity-locked relationship to the alliance be reversed through political education, alternative national narratives, or generational change?',
    'Historical cases of identity reframing (post-Cold War transitions, decolonization movements); analysis of political movements advocating strategic autonomy and their ability to shift national narrative; longitudinal polling of attitudes toward independence vs alliance among different generations',
    'If reversible: the constraint''s power derives from internalized frames that could shift. If irreversible: national identity-lock is fundamental to the state''s existence and cannot be broken without state collapse or revolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, conceptual, 'Whether identity-lock to the alliance is reversible or foundational').

omega_variable(
    regional_threat_reality,
    'How much of the current threat environment justifies the security guarantee versus how much is constructed by alliance discourse to legitimize the dependency?',
    'Geopolitical threat assessment independent of alliance narratives; military capability comparison of regional adversaries; historical analysis of threat inflation during periods of alliance consolidation; analysis of how threat framing changes when allies propose greater autonomy',
    'If threat is structural and enduring: snare classification is accurate — dependency is functionally justified. If threat is partly constructed: suppression mechanism includes threat inflation, and identity-lock becomes central to extractive function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_threat_reality, empirical, 'How much regional threat justifies the security guarantee').

omega_variable(
    technology_transfer_lock_in,
    'Does the dependent state develop sufficient indigenous technological capacity to sustain independent defense, or does technology transfer create asymmetric dependence on guarantor for spare parts, upgrades, and maintenance?',
    'Analysis of technology transfer agreements (what is transferred vs withheld); comparative case studies (Israel, South Korea, Japan) on indigenous vs imported defense technology; long-term projections of technological divergence or convergence',
    'If indigenous capacity develops: scaffold perspective is validated and exit becomes feasible. If technological lock-in deepens: snare classification is reinforced and extraction mechanism becomes permanent structural feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_lock_in, empirical, 'Whether technology transfer enables or deepens dependence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regional_ally_security_dependency, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rasd_tr_t0, regional_ally_security_dependency, theater_ratio, 0, 0.38).
narrative_ontology:measurement(rasd_tr_t15, regional_ally_security_dependency, theater_ratio, 15, 0.48).
narrative_ontology:measurement(rasd_tr_t30, regional_ally_security_dependency, theater_ratio, 30, 0.52).

% Extraction over time
narrative_ontology:measurement(rasd_be_t0, regional_ally_security_dependency, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(rasd_be_t15, regional_ally_security_dependency, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(rasd_be_t30, regional_ally_security_dependency, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regional_ally_security_dependency, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(regional_ally_security_dependency, 0.12).
narrative_ontology:affects_constraint(regional_ally_security_dependency, military_technology_transfer_dependency).
narrative_ontology:affects_constraint(regional_ally_security_dependency, geopolitical_hegemonic_stability).

% DUAL FORMULATION NOTE:
% The regional security dependency decomposes into distinct constraints: (1) military_technology_transfer_dependency (upstream) — the technology flow creates structural dependency; (2) regional_ally_security_dependency (this story) — the security guarantee creates political dependency; (3) geopolitical_hegemonic_stability (downstream) — the dependency sustains the guarantor's regional hegemony. The three stories share a domain but have different ε values: technology transfer can be structured with lower extractiveness through open standards; security guarantees have higher extractiveness due to commitment irreversibility; hegemonic stability has the highest extractiveness due to its function in maintaining global power distribution. All three are linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
