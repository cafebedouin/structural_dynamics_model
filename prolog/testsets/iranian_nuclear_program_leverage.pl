% ============================================================================
% CONSTRAINT STORY: iranian_nuclear_program_leverage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iranian_nuclear_program_leverage, []).

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
 *   constraint_id: iranian_nuclear_program_leverage
 *   human_readable: Iranian Nuclear Program Leverage in Geopolitical Negotiation
 *   domain: geopolitical/nuclear_security/sanctions
 *
 * SUMMARY:
 *   The Iranian nuclear program functions as a multi-layered constraint
 *   operating simultaneously as deterrence mechanism, negotiating leverage,
 *   and enforcement extraction. The constraint exhibits fundamentally
 *   different characters across structural positions: Iranian state
 *   negotiators experience it as coordination (rope), sanctioned civilians
 *   experience it as pure extraction (snare), neighboring states experience
 *   mixed costs and benefits (tangled rope), the nonproliferation regime sees
 *   a temporary problem with sunset logic (scaffold), the JCPOA apparatus
 *   persists through inertial theater (piton), and analytical observers risk
 *   naturalizing geopolitical contingency as structural inevitability (false
 *   mountain). The extractiveness trajectory (0.35→0.58) reflects escalation
 *   from the 2015 JCPOA baseline through 2018 US withdrawal and subsequent
 *   sanctions intensification. Theater ratio (0.52→0.68) shows increasing
 *   performativity: IAEA inspections continue despite the political
 *   agreement's collapse, verification apparatus maintains operational
 *   theater even as its coordinating function degrades.
 *
 * KEY AGENTS:
 *   - Iranian State (institutional/arbitrage): Primary beneficiary—nuclear program provides negotiating leverage, strategic autonomy, deterrence credibility, and sanctions negotiation seat
 *   - Iranian Civilian Population (powerless/trapped): Primary victim—bears costs of comprehensive sanctions (medicine access, inflation, currency collapse) with no exit option
 *   - Western Sanctioning Powers/US/EU (organized/constrained): Mixed position—benefit from deterrence reduction and negotiating leverage, bear costs of sanctions enforcement, trade fragmentation, alliance coordination
 *   - Regional States (Saudi Arabia, UAE, Israel) (moderate/constrained): Mixed position—benefit from Iranian deterrent as regional balance, bear security costs from proxy conflicts and destabilization
 *   - Global Nonproliferation Coalition/NPT Signatories (organized/constrained): Organized advocates—maintain sanctions enforcement while perceiving sunset path toward eventual normalization
 *   - IAEA/JCPOA Institutional Structure (institutional/constrained): Institutional actor—maintains verification apparatus through inertia despite political framework collapse
 *   - Analytical Observer (analytical/analytical): External perspective—risks naturalizing contingent geopolitical arrangements as structural inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iranian_nuclear_program_leverage, 0.58).
domain_priors:suppression_score(iranian_nuclear_program_leverage, 0.72).
domain_priors:theater_ratio(iranian_nuclear_program_leverage, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iranian_nuclear_program_leverage, extractiveness, 0.58).
narrative_ontology:constraint_metric(iranian_nuclear_program_leverage, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(iranian_nuclear_program_leverage, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iranian_nuclear_program_leverage, tangled_rope).
narrative_ontology:human_readable(iranian_nuclear_program_leverage, "Iranian Nuclear Program Leverage in Geopolitical Negotiation").
narrative_ontology:topic_domain(iranian_nuclear_program_leverage, "geopolitical/nuclear_security/sanctions").

domain_priors:requires_active_enforcement(iranian_nuclear_program_leverage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iranian_nuclear_program_leverage, iranian_state_negotiating_power).
narrative_ontology:constraint_beneficiary(iranian_nuclear_program_leverage, regional_state_actors).
narrative_ontology:constraint_victim(iranian_nuclear_program_leverage, global_nonproliferation_regime).
narrative_ontology:constraint_victim(iranian_nuclear_program_leverage, sanctioned_iranian_population).
narrative_ontology:constraint_victim(iranian_nuclear_program_leverage, neighboring_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SANCTIONED POPULATION (SNARE) — Iranian civilians bear costs of comprehensive sanctions (medicine access, inflation, currency collapse) with no exit option and minimal coordination benefit. The nuclear program provides state leverage but extracts from ordinary citizens. Suppression is absolute: capital controls, financial isolation, and nationalist framing prevent organized escape.
constraint_indexing:constraint_classification(iranian_nuclear_program_leverage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NEIGHBORING STATES (TANGLED ROPE) — Saudi Arabia, UAE, Israel experience both coordination (regional deterrence balance) and extraction (destabilization risk, proxy conflicts, security expenditure). Constrained by regional geography and alliance structures; cannot exit without strategic cost. Mixed experience: some benefit from Iranian leverage as counterweight to competitors, others bear security costs.
constraint_indexing:constraint_classification(iranian_nuclear_program_leverage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: IRANIAN STATE NEGOTIATING POSITION (ROPE) — The state benefits from nuclear program leverage as coordination mechanism: enables seat at global negotiating table, provides deterrence credibility, secures strategic autonomy. Experiences constraint as coordination—communication of capabilities enables negotiation. Net beneficiary with arbitrage options (negotiation outcomes, sanctions relief).
constraint_indexing:constraint_classification(iranian_nuclear_program_leverage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: JCPOA INSTITUTIONAL STRUCTURE (PITON) — The Joint Comprehensive Plan of Action and its verification mechanisms (IAEA monitoring) are increasingly theatrical: the framework persists through institutional inertia despite multiple unilateral withdrawals and re-impositions. Theater ratio reflects that compliance monitoring continues despite the political commitment structure collapsing. The verification apparatus maintains operational continuity (measuring centrifuges, uranium enrichment) even as the treaty's coordinating function has degraded.
constraint_indexing:constraint_classification(iranian_nuclear_program_leverage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: GLOBAL NONPROLIFERATION COALITION (SCAFFOLD) — NPT signatories, IAEA member states, and arms control advocates see the Iranian nuclear constraint as a temporary problem with sunset logic: strengthened inspections, enrichment caps, and time-limited restrictions create a path toward eventual normalization (if political conditions shift). Extraction is tolerated because the coalition perceives declining suppression as conditions mature. Sunset horizon: 15-25 years for generational shift in regional security.
constraint_indexing:constraint_classification(iranian_nuclear_program_leverage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: WESTERN SANCTIONING POWERS (TANGLED ROPE) — The US and EU benefit from leverage (deterrence, negotiating position) while coordinating sanctions enforcement (SWIFT expulsion, sectoral restrictions). But they bear costs: disrupted trade, fragmented global commerce, strategic unpredictability. Constrained by institutional commitment and domestic constituencies; cannot easily exit sanctions without political cost. Both beneficiary (nuclear deterrence reduction) and victim (economic coordination fragmentation).
constraint_indexing:constraint_classification(iranian_nuclear_program_leverage, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / INEVITABILITY VIEW (MOUNTAIN) — From a civilizational/universal perspective, some states acquiring nuclear capability is a structural inevitability of international system evolution: proliferation follows from security dilemmas and technological diffusion. This perspective sees the Iranian nuclear program as an immutable consequence of systemic incentives—no actor can prevent it, only manage its emergence. However, the structural data contradicts pure mountain classification: the constraint's suppression and extractiveness are contingent on political choices (sanctions regimes, negotiating strategies), revealing that 'inevitability' naturalizes what are actually contingent geopolitical arrangements.
constraint_indexing:constraint_classification(iranian_nuclear_program_leverage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iranian_nuclear_program_leverage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iranian_nuclear_program_leverage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iranian_nuclear_program_leverage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iranian_nuclear_program_leverage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(iranian_nuclear_program_leverage, TR),
    TR >= 0.70.

:- end_tests(iranian_nuclear_program_leverage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from sanctioned civilians and destabilized neighbors while benefiting the Iranian state's negotiating position. Not maximal (0.72+) because legitimate deterrence value exists—the state is not simply theft-extracting, but leveraging real strategic capabilities. The extraction increases over the interval (0.35→0.58) as sanctions intensity rose and enrichment capacity advanced, escalating the cost-to-civilians relative to the state's negotiating gain. Suppression (0.72): High. Comprehensive sanctions, capital controls, SWIFT expulsion, and nationalist framing all constrain civilian exit and organize dissent. Suppression is not absolute (0.90+) because informal economy, diaspora transfers, and political factions provide partial escape, but barriers are severe for most actors. Theater ratio (0.68): Moderately high. IAEA inspections and verification protocols persist as institutional theater despite the political framework's collapse. The theater increased from 2015 baseline (0.52) as the agreement's coordinating function degraded while its monitoring apparatus continued. The gap between verification activity and political commitment reflects institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   The central perspectival gap separates the beneficiary (Iranian state) from the victims (civilian population and neighboring states). The state experiences coordination; the population experiences extraction. This gap is not resolvable by adjusting measurement—it reflects structural reality. The gap also separates Western analytical perspectives: the sanctioning powers' perspective (moderate/constrained institutional position) differs fundamentally from the powerless civilian population's perspective (trapped victim). The most revealing gap is between the piton classification of the JCPOA apparatus and the rope/snare classifications of the actual political positions: the verification machinery continues (theater) while the political agreement that justified it collapsed (function). This gap reveals institutional inertia as a constraint type signature.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation operates along multiple institutional and actor levels. Iranian state negotiators derive low d (beneficiaries with arbitrage exit options) producing low/negative chi—they experience the constraint as enabling. Sanctioned civilians derive high d (victims with trapped exit options) producing high chi—they experience maximum extraction. Regional states derive moderate d (both beneficiaries and victims with constrained exit) producing moderate chi—mixed experience. Western powers derive moderate d (beneficiary position via deterrence reduction, constrained by enforcement obligations) producing moderate chi. The powerless population's d approaches 1.0 (trapped victims), yielding the snare classification. The institutional beneficiary's d approaches 0.0 (arbitrage exit), yielding rope. The organized coalition's d is constrained-moderate (0.40-0.55), permitting scaffold classification through the sunset gate. The piton classification for JCPOA apparatus derives from theater ratio exceeding 0.70, independent of directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by acknowledging that the constraint simultaneously coordinates and extracts. From the state perspective, it coordinates strategic communication and negotiating position—the nuclear program enables deterrence-backed diplomacy. From the victim perspective, it extracts without coordination benefit—sanctions costs with no decision-making power. The constraint is Tangled Rope not because it ambiguously could be either, but because it IS both: genuine coordination function (security deterrence) coupled with asymmetric extraction (civilian costs). The piton classification of the JCPOA apparatus prevents mislabeling the institutional structure as pure mountain (inevitability) or pure rope (coordination success)—the apparatus is recognizably degraded, maintained through inertia rather than functional necessity. The scaffold classification of the nonproliferation coalition confirms that actors with agency (organized states, NPT signatories) perceive exit paths and declining suppression, while powerless actors (civilian population) perceive persisting suppression and no exit. The false mountain perspective (analytical inevitability view) is diagnostically important: it reveals how structural constraints become naturalized when analysts adopt the wrong observational position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sanctions_efficacy_threshold,
    'At what point do sanctions reduce negotiating cooperation rather than incentivize it?',
    'Historical analysis of JCPOA negotiations (2015) vs sanctions escalation (2018-2024); correlation between sanction severity and Iranian negotiating willingness',
    'If threshold < current level: sanctions perpetuate snare dynamics (extraction without cooperation). If threshold > current level: sanctions function as rope mechanism (painful but enabling coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctions_efficacy_threshold, empirical, 'Sanctions severity threshold for negotiating cooperation').

omega_variable(
    proliferation_capability_ambiguity,
    'What degree of nuclear enrichment capability constitutes existential deterrence vs negotiating theater?',
    'Technical analysis of uranium enrichment rates, breakout timelines, weaponization requirements; comparison to publicly stated Iranian nuclear doctrine',
    'If theater > 70%: Iranian nuclear program is primarily leverage mechanism (Snare from victim perspective). If theater < 40%: program has genuine deterrence substance (Rope from state perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proliferation_capability_ambiguity, empirical, 'Whether nuclear capability is deterrence substance or negotiating theater').

omega_variable(
    coalition_commitment_stability,
    'Will the nonproliferation coalition maintain sanctions enforcement if Iran approaches weaponization?',
    'Monitoring coalition member defections, secondary sanctions enforcement, SWIFT participation; analysis of economic incentives vs security commitments',
    'If coalition fractures: suppression declines, constraint shifts toward rope. If coalition holds: suppression persists, constraint remains tangled rope or snare from victim perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_commitment_stability, empirical, 'Coalition stability for nonproliferation enforcement').

omega_variable(
    regional_deterrence_substitutability,
    'Is the Iranian nuclear program functionally substitutable by conventional deterrence, alliance structures, or cyber capabilities?',
    'Comparative capability analysis: Iranian military strength vs regional competitors; modeling of deterrence gaps under various nuclear scenarios',
    'If substitutable: the constraint is extractive theater with alternatives available (Piton rather than Snare). If essential: the constraint is legitimate strategic necessity (Rope rather than Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_deterrence_substitutability, conceptual, 'Whether nuclear deterrence is functionally necessary or substitutable').

omega_variable(
    civilian_extraction_proportion,
    'What proportion of sanction costs flow to civilian population vs military/state apparatus?',
    'Economic impact analysis: inflation rates, medicine availability, currency effects; comparison to military spending and IRGC resource allocation',
    'If > 70% civilian: suppression is asymmetric extraction from powerless agents (Snare). If < 40% civilian: suppression is distributed across society (transitions toward Tangled Rope for population perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_extraction_proportion, empirical, 'Proportion of sanctions burden on civilian vs military').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iranian_nuclear_program_leverage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iran_tr_t0, iranian_nuclear_program_leverage, theater_ratio, 0, 0.52).
narrative_ontology:measurement(iran_tr_t5, iranian_nuclear_program_leverage, theater_ratio, 5, 0.62).
narrative_ontology:measurement(iran_tr_t10, iranian_nuclear_program_leverage, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(iran_be_t0, iranian_nuclear_program_leverage, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(iran_be_t5, iranian_nuclear_program_leverage, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(iran_be_t10, iranian_nuclear_program_leverage, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iranian_nuclear_program_leverage, enforcement_mechanism).
narrative_ontology:affects_constraint(iranian_nuclear_program_leverage, middle_east_proxy_conflict_dynamics).
narrative_ontology:affects_constraint(iranian_nuclear_program_leverage, global_sanctions_regime_coherence).
narrative_ontology:affects_constraint(iranian_nuclear_program_leverage, nonproliferation_treaty_viability).

% DUAL FORMULATION NOTE:
% The Iranian nuclear program constraint decomposes into multiple structurally distinct claims: (1) Iranian nuclear capability development (ε≈0.42, technical constraint on proliferation pathway), (2) sanctions enforcement regime (ε≈0.58, geopolitical extraction mechanism), and (3) JCPOA institutional framework (ε≈0.48, degraded coordination apparatus). This story focuses on the sanctions enforcement mechanism and its extractive dynamics. The upstream technical capability constraint and downstream institutional framework constitute a constraint family linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(iranian_nuclear_program_leverage, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
