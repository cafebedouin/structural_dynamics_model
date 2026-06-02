% ============================================================================
% CONSTRAINT STORY: germany_tennet_takeover
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_germany_tennet_takeover, []).

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
 *   constraint_id: germany_tennet_takeover
 *   human_readable: German Government Stake in TenneT Germany
 *   domain: economic/political
 *
 * SUMMARY:
 *   The German government's acquisition of a controlling stake in TenneT
 *   Germany represents a hybrid constraint combining genuine coordination
 *   (grid stability during renewable energy transition) with extractive
 *   mechanisms (shareholder dilution, regulatory exception-granting,
 *   precedent-setting for state control of critical infrastructure). The
 *   constraint operates across three distinct structural levels: (1) at the
 *   firm level, private shareholders face forced absorption into state
 *   control; (2) at the EU level, the takeover violates the liberalization
 *   norm that has governed European electricity markets for three decades;
 *   (3) at the member-state level, Germany asserts energy sovereignty against
 *   perceived foreign strategic vulnerability. The constraint's
 *   extractiveness has increased over time as initial 'temporary emergency
 *   measure' framing has become embedded in longer-term grid governance, and
 *   theater has increased as political justification narratives (foreign
 *   takeover risk, energy independence) have become decoupled from actual
 *   technical grid challenges. The constraint is a diagnostic exemplar of how
 *   economic nationalism and energy transition can be legitimately
 *   distinguished from each other — one is coordination and integration, the
 *   other is extraction and sovereignty performance.
 *
 * KEY AGENTS:
 *   - German Federal Government: Primary beneficiary (institutional/arbitrage) — gains control over critical infrastructure, energy policy leverage, and strategic sovereignty during transition
 *   - Private TenneT Shareholders: Primary victim (powerless/trapped) — face forced dilution, loss of property rights, absorption into state entity; cannot exit
 *   - EU Market Liberalization Regime: Structural victim (institutional/constrained) — takeover violates competitive grid operation norm; cannot exit the rule structure but is suspended for this actor
 *   - Renewable Energy Integration Process: Primary beneficiary and coordinator (organized/constrained) — state ownership enables technical grid stability during high renewable penetration; actual coordination function
 *   - Non-German EU Investors: Secondary victim (organized/constrained) — excluded from ownership governance; property rights circumscribed by national sovereignty assertion
 *   - State Infrastructure Ownership Legacy: Institutional memory actor (powerful/mobile) — takeover activates pre-liberalization German tradition of public utility monopolies; brings organizational inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(germany_tennet_takeover, 0.38).
domain_priors:suppression_score(germany_tennet_takeover, 0.48).
domain_priors:theater_ratio(germany_tennet_takeover, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(germany_tennet_takeover, extractiveness, 0.38).
narrative_ontology:constraint_metric(germany_tennet_takeover, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(germany_tennet_takeover, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(germany_tennet_takeover, tangled_rope).
narrative_ontology:human_readable(germany_tennet_takeover, "German Government Stake in TenneT Germany").
narrative_ontology:topic_domain(germany_tennet_takeover, "economic/political").

domain_priors:requires_active_enforcement(germany_tennet_takeover).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(germany_tennet_takeover, german_government).
narrative_ontology:constraint_beneficiary(germany_tennet_takeover, grid_stability_mandate).
narrative_ontology:constraint_victim(germany_tennet_takeover, private_shareholders).
narrative_ontology:constraint_victim(germany_tennet_takeover, market_liberalization_norm).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRIVATE SHAREHOLDERS (SNARE) — Original equity holders face forced dilution and potential loss of dividends during the state acquisition phase. They cannot exit the constraint — selling would realize losses; holding means absorption into state-controlled entity with altered governance and dividend policies. Maximum extraction: state controls pricing of acquisition, timing, and terms. Shareholders bear full cost with no alternative.
constraint_indexing:constraint_classification(germany_tennet_takeover, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARKET LIBERALIZATION NORM (TANGLED ROPE) — EU energy market rules presume private operation and competitive ownership structures. State takeover of critical infrastructure violates the coordination function (competitive grid operation) while extracting regulatory exception and sovereignty claim. Liberalization regime cannot exit — the rule is structural to EU law — but is partially suspended for this actor. Mixed extraction and coordination: state justifies action as stabilizing the grid (genuine coordination benefit), but the mechanism is exceptionalism that weakens liberalization norms for all member states.
constraint_indexing:constraint_classification(germany_tennet_takeover, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: GERMAN FEDERAL GOVERNMENT (ROPE) — State benefits from control over grid operations, ensuring renewable energy integration and preventing foreign strategic vulnerability during the Energiewende (green energy transition). Exit is available through privatization; the constraint is reversible if security threat decreases. Government experiences this as pure coordination — solving the collective action problem of energy security. Low extraction cost to government; high benefit (strategic control). Asymmetric but framed as necessary public good.
constraint_indexing:constraint_classification(germany_tennet_takeover, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: GREEN ENERGY TRANSITION INFRASTRUCTURE REQUIREMENT (SCAFFOLD) — Grid stability during renewable buildout is a temporary bottleneck. State ownership provides short-term coordination (integrating distributed renewables) with explicit or implicit sunset: as battery storage, smart grids, and cross-border interconnections mature (10-20 year horizon), centralized state control becomes less necessary. The constraint has built-in termination condition — market liberalization can resume once technical transition is complete. Theater remains moderate (0.55) because actual grid integration work is ongoing, not purely performative.
constraint_indexing:constraint_classification(germany_tennet_takeover, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: STATE INFRASTRUCTURE OWNERSHIP LEGACY (PITON) — The takeover activates historical narratives of state monopoly on critical infrastructure (Deutsche Telekom, Deutsche Post, Deutsche Bahn precedents). The institutional memory of public utility ownership persists despite 30+ years of EU liberalization. Much of the political logic is theatrical — references to 'strategic sovereignty' and 'foreign takeover risk' perform sovereignty concerns more than they solve technical grid problems. The constraint is partly inertial: state ownership of utilities is how Germany 'naturally' solved infrastructure before 1990; the Energiewende crisis revives this pattern despite available market alternatives.
constraint_indexing:constraint_classification(germany_tennet_takeover, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: NON-GERMAN EU INVESTORS (TANGLED ROPE) — Foreign equity holders and institutional investors (pension funds, utilities in other states) benefit from EU market liberalization norms but face extraction when Germany nationalizes a major grid asset. They cannot fully exit (EU law constrains capital controls) but are constrained by the blocking of their ownership claims. Mixed experience: some coordination benefit (grid stability supports their renewable investments in Germany), but asymmetric extraction through seizure of property rights and exclusion from governance. Suppression is moderate — legal processes occur, compensation is offered, but alternatives are removed.
constraint_indexing:constraint_classification(germany_tennet_takeover, tangled_rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — At the civilizational level, some degree of centralized grid authority is inherent to managing complex electrical systems: load balancing, frequency stability, and fault propagation are collective action problems with no purely decentralized solution. From this perspective, the state role emerges naturally from the physics of coupled-oscillator networks. However, this naturalizes a contingent institutional choice (public vs private operator) as a law of nature. The engine's false summit detector should flag this — the thermodynamic substrate does not require state ownership, only some coordination authority.
constraint_indexing:constraint_classification(germany_tennet_takeover, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(germany_tennet_takeover_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(germany_tennet_takeover, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(germany_tennet_takeover, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(germany_tennet_takeover, TR),
    TR >= 0.70.

:- end_tests(germany_tennet_takeover_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. The government captures significant benefit (control, strategic positioning, policy leverage) while shareholders bear direct cost (forced dilution, governance loss). However, the extraction is not maximal (0.46+) because: (1) the technical coordination function (renewable grid integration) is genuine and non-extractive; (2) compensation is provided to shareholders, though below market value; (3) the state's cost (financial outlay, regulatory exposure) is substantial. The value reflects that the takeover hybridizes extraction with coordination — not pure either. Suppression (0.48): Moderate. Multiple barriers exist to alternative solutions: (1) EU sovereignty norms prevent full foreign ownership in critical infrastructure (structural); (2) private operators face regulatory uncertainty during energy transition (policy); (3) shareholder exit is expensive/disadvantageous (market barrier). However, suppression is not high (0.60+) because: (1) EU law permits the takeover if framed as public utility; (2) private grid operators exist elsewhere in Europe (alternatives exist); (3) shareholder compensation is offered, not expropriation without payment. Theater ratio (0.55): Moderate-high. The state's political rhetoric ('energy security', 'foreign takeover prevention', 'sovereignty') plays a significant performative role, but the actual technical grid stabilization work is real — not pure theater. The ratio reflects that legitimation narratives are inflated relative to actual technical necessity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural phenomenon (state acquisition of grid operator) appears differently across positions. From the government's view, this is solving a coordination problem (Rope). From shareholders' view, it is extraction (Snare). From the EU liberalization regime's view, it is a precedent-setting exception (Tangled Rope mixing coordination and extraction). From the renewable integration requirement's view, it is legitimate technical coordination (Rope to Scaffold). From the piton historical view, it is inertial reactivation of pre-liberalization state monopoly patterns. From the foreign investor view, it is property right expropriation (Snare). The perspectival gaps are not measurement ambiguities but real structural differences in how each agent experiences the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from structural position. The government (beneficiary + arbitrage exit) receives low d (≈0.10-0.15), producing negative or minimal effective extraction — their experience is coordination, not extraction. Shareholders (victim + trapped exit) receive high d (≈0.85-0.95), producing high effective extraction — they cannot escape and bear full dilution cost. The EU regime (victim + constrained exit) receives moderate-high d (≈0.60-0.70) — can theoretically challenge the takeover via competition law but is constrained by member-state sovereignty norms. Renewable integration (beneficiary + constrained exit) receives low-moderate d (≈0.35-0.45) — benefits from state coordination but is constrained by whatever ownership structure emerges. Non-German investors (victim + constrained exit) receive moderate-high d (≈0.55-0.65) — property claims are blocked but not entirely eliminated (EU residuals remain). The piton perspective derives from high theater (0.55) relative to extractiveness (0.38) — the performance-to-function ratio indicates institutional inertia activating historical patterns.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy between 'is this coordination or extraction?' by showing it is both — the tangled_rope classification captures the genuine coordination function (grid stability during renewable transition) alongside the genuine extraction mechanism (shareholder dilution, regulatory exception, precedent creation). The mandatrophy is avoided by maintaining ε=0.38 (not claiming pure coordination ε≤0.05 or pure extraction ε≥0.46) and including both beneficiaries and victims. The theater ratio (0.55) indicates that political legitimation narratives inflate the necessity of state ownership beyond its actual technical requirement — but does not eliminate the technical requirement entirely. The scaffold perspective shows that if the takeover is indeed temporary (energy transition complete in 10-20 years), the constraint transitions from snare to rope as the exceptional ownership model becomes normalized infrastructure. The piton perspective reveals how institutional memory of pre-liberalization state monopolies provides social script for current action — reducing novelty and increasing performance. No single type is 'correct' — the presheaf over the observation site (government, shareholders, EU regime, technical integration, historical memory, foreign investors) is the complete description.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foreign_takeover_threat_reality,
    'Is the risk of hostile foreign takeover of TenneT actually materially higher than for other European grid operators, or is it primarily a domestic political narrative?',
    'Comparative analysis of foreign investment attempts in grid operators across EU member states; risk assessment from grid security experts vs political rhetoric; identification of specific acquisition proposals that triggered state action',
    'If real and specific: justifies state intervention as temporally bounded response to concrete threat (scaffold timeline becomes credible). If primarily narrative: reveals extraction dressed as security, shifting classification toward snare and away from scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_takeover_threat_reality, empirical, 'Whether foreign takeover threat is material or primarily narrative').

omega_variable(
    renewable_integration_dependency_on_ownership,
    'Does grid stability during renewable integration actually require state ownership, or could private operators with regulatory mandates achieve equivalent coordination outcomes?',
    'Comparative study of renewable-heavy grids under private vs public ownership (Denmark, Ireland, Spain vs Germany); technical analysis of grid balance requirements vs management structure; identification of which integration functions are structure-dependent vs ownership-dependent',
    'If structure-dependent: state ownership is extracting on top of a genuine coordination need — classification remains tangled_rope. If ownership-independent: the coordination could occur under private management with regulation, revealing the takeover as pure political extraction — shifts classification toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(renewable_integration_dependency_on_ownership, empirical, 'Whether ownership structure is necessary for renewable grid integration').

omega_variable(
    market_liberalization_credibility_postdatum,
    'If state ownership resolves the energy transition, will Germany actually re-privatize TenneT, or will public ownership become permanent despite sunset clause language?',
    'Historical analysis of prior EU infrastructure re-privatizations post-crisis; examination of government commitments in legislation vs actual divestiture timelines; tracking of changing political rhetoric around TenneT ownership over 5-10 year horizon',
    'If re-privatization occurs on schedule: scaffold classification confirmed — sunset is genuine, constraint has built-in termination. If ownership becomes permanent: scaffold collapses to snare/tangled_rope — the extraction mechanism persists indefinitely, theater increases as sunset narrative becomes decoupled from action.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(market_liberalization_credibility_postdatum, empirical, 'Credibility of planned re-privatization after energy transition').

omega_variable(
    eu_reciprocal_takeover_precedent,
    'Will other EU member states cite Germany''s TenneT takeover as precedent for their own nationalizations of critical infrastructure, triggering a cascade of state acquisitions?',
    'Tracking of legislative proposals and actual takeovers in other EU states citing German precedent; analysis of EU competition law challenges to the German action; monitoring of investor confidence indices in European critical infrastructure',
    'If cascade occurs: Germany''s takeover creates institutional contagion — the coordination function (energy security) is preserved but the suppression mechanism (state control norm) spreads. If isolated: Germany achieves exception status, suggesting the takeover is politically rather than structurally justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eu_reciprocal_takeover_precedent, preference, 'Whether TenneT takeover triggers cascade of EU state infrastructure nationalizations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(germany_tennet_takeover, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tennet_tr_t0, germany_tennet_takeover, theater_ratio, 0, 0.4).
narrative_ontology:measurement(tennet_tr_t3, germany_tennet_takeover, theater_ratio, 3, 0.52).
narrative_ontology:measurement(tennet_tr_t6, germany_tennet_takeover, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(tennet_be_t0, germany_tennet_takeover, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(tennet_be_t3, germany_tennet_takeover, base_extractiveness, 3, 0.34).
narrative_ontology:measurement(tennet_be_t6, germany_tennet_takeover, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(germany_tennet_takeover, resource_allocation).
narrative_ontology:affects_constraint(germany_tennet_takeover, eu_energy_market_liberalization).
narrative_ontology:affects_constraint(germany_tennet_takeover, german_energy_transition_infrastructure).
narrative_ontology:affects_constraint(germany_tennet_takeover, eu_critical_infrastructure_sovereignty).

% DUAL FORMULATION NOTE:
% The TenneT takeover is downstream of two structurally distinct constraints: (1) EU energy market liberalization (the norm being violated) has its own extractiveness reflecting the coordination benefits of competitive grids; (2) German energy transition infrastructure (the technical requirement) has its own extractiveness reflecting renewable integration challenges. The takeover story represents their intersection — where liberalization norms collide with transition requirements. Each upstream constraint has different ε values; this story synthesizes their interaction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
