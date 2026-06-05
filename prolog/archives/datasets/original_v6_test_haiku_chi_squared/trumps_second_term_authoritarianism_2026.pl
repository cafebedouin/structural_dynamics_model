% ============================================================================
% CONSTRAINT STORY: trumps_second_term_authoritarianism_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trumps_second_term_authoritarianism_2026, []).

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
 *   constraint_id: trumps_second_term_authoritarianism_2026
 *   human_readable: Electoral Authoritarianism (Trump II, Year One)
 *   domain: political/institutional
 *
 * SUMMARY:
 *   Year One of Trump's hypothetical second term exhibits structural features
 *   of electoral authoritarianism: executive power consolidation, civil
 *   service politicization, prosecutorial targeting of opposition, media
 *   alignment, norm erosion, and institutionalization of suppression
 *   mechanisms. The constraint is not a single totalitarian takeover but a
 *   systematic extraction of institutional autonomy paired with coordination
 *   of executive authority. The theater_ratio of 0.64 reflects that many
 *   suppression mechanisms operate through formal legal channels (executive
 *   orders, prosecutorial decisions, regulatory action) rather than
 *   extrajudicial force — maintaining constitutional appearance while eroding
 *   constitutional function. The increasing measurements (ε from 0.35 to
 *   0.58, theater from 0.45 to 0.64) show the constraint strengthening as
 *   norms erode and institutional capture deepens. This constraint exhibits
 *   all six classification types perspectivally: the opposition voter
 *   experiences snare (no exit), the civil service experiences tangled rope
 *   (mixed coordination and extraction), the executive experiences rope (pure
 *   coordination hierarchy), the party leadership experiences tangled rope
 *   (loss of autonomy), constitutional checks experience piton (degraded but
 *   still invoked), resistance movements experience scaffold (organizing for
 *   institutional restoration), and the global observer risks naturalizing
 *   authoritarianism as inevitable in polarized democracies (false mountain).
 *
 * KEY AGENTS:
 *   - Executive Apparatus (BENEFICIARY / POWERFUL): Consolidates authority; benefits from loyalty enforcement and institutional capture. Arbitrage exit (reallocate to private sector, media). Sees rope.
 *   - Opposition Voters (VICTIM / POWERLESS): Face electoral suppression (gerrymandering, voter ID, prosecution of leaders). Trapped exit. See snare.
 *   - Career Civil Service (VICTIM / MODERATE): Purged for disloyalty; forced into political tests. Constrained exit (resignation at high career cost). See tangled rope.
 *   - Republican Party Leadership (BENEFICIARY & VICTIM / POWERFUL): Benefit from unified party machine; lose institutional autonomy to executive. Mobile exit (defection, primary challenge). See tangled rope.
 *   - Constitutional Institutions (DEGRADED MECHANISM): Courts, Congress, federalism that issue orders/hold hearings but lack enforcement. Constrained exit (institutional inertia). See piton.
 *   - Democratic Resistance (ORGANIZED OPPOSITION / ORGANIZED): State AGs, courts, media, civil society organizing for institutional restoration. Mobile exit (can change tactics, venue). See scaffold.
 *   - Global Observers (ANALYTICAL): Risk naturalizing authoritarianism as law of polarized democracies. See mountain (false summit).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trumps_second_term_authoritarianism_2026, 0.58).
domain_priors:suppression_score(trumps_second_term_authoritarianism_2026, 0.68).
domain_priors:theater_ratio(trumps_second_term_authoritarianism_2026, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trumps_second_term_authoritarianism_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(trumps_second_term_authoritarianism_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(trumps_second_term_authoritarianism_2026, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trumps_second_term_authoritarianism_2026, tangled_rope).
narrative_ontology:human_readable(trumps_second_term_authoritarianism_2026, "Electoral Authoritarianism (Trump II, Year One)").
narrative_ontology:topic_domain(trumps_second_term_authoritarianism_2026, "political/institutional").

domain_priors:requires_active_enforcement(trumps_second_term_authoritarianism_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trumps_second_term_authoritarianism_2026, executive_apparatus).
narrative_ontology:constraint_beneficiary(trumps_second_term_authoritarianism_2026, allied_media_corporations).
narrative_ontology:constraint_beneficiary(trumps_second_term_authoritarianism_2026, energy_extraction_industries).
narrative_ontology:constraint_victim(trumps_second_term_authoritarianism_2026, civil_service_neutrality).
narrative_ontology:constraint_victim(trumps_second_term_authoritarianism_2026, electoral_integrity).
narrative_ontology:constraint_victim(trumps_second_term_authoritarianism_2026, press_freedom).
narrative_ontology:constraint_victim(trumps_second_term_authoritarianism_2026, rule_of_law).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORDINARY VOTER / OPPOSITION (SNARE) — Powerless agent in electoral system experiencing systematic suppression: gerrymandering, voter ID restrictions, prosecutorial targeting of opposition leaders, media exclusion. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.81. No viable exit; bears full extraction cost.
constraint_indexing:constraint_classification(trumps_second_term_authoritarianism_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CAREER CIVIL SERVICE (TANGLED ROPE) — Constrained but not trapped. Experiences coordination function (unified executive hierarchy) alongside extraction (political loyalty tests, purges, surveillance). Some exit available (resignation, retirement) but at high career cost. d≈0.70, f(d)≈1.08, σ=1.0 → χ≈0.63. Mixed experience.
constraint_indexing:constraint_classification(trumps_second_term_authoritarianism_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE LEADERSHIP / CORE LOYALISTS (ROPE) — Institutional actor with arbitrage exit (can reallocate to cabinet, private sector, media). Experiences constraint as coordination: unified command structure, loyalty hierarchy, media alignment. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary; low effective extraction.
constraint_indexing:constraint_classification(trumps_second_term_authoritarianism_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REPUBLICAN PARTY LEADERSHIP (TANGLED ROPE) — Powerful but mobile (can defect, run primary challengers, coordinate with Congress). Experiences both coordination (unified party machine) and extraction (loss of institutional autonomy to executive, loss of legislative independence). Theater used to maintain party unity while actual power concentrates in executive. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.43. Moderate extraction.
constraint_indexing:constraint_classification(trumps_second_term_authoritarianism_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL CHECKS & BALANCES (PITON) — Institutional mechanism (separation of powers, federalism, judicial review) that persists through inertia despite functional degradation. Theater_ratio ≈ 0.64: courts issue orders ignored by executive; Congress holds hearings producing no enforcement; federal-state conflicts unresolved. Mechanism still invoked but enforcement mechanisms have atrophied. d≈0.60, f(d)≈0.80, σ=1.0 → χ≈0.48.
constraint_indexing:constraint_classification(trumps_second_term_authoritarianism_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DEMOCRATIC RESISTANCE COALITION (SCAFFOLD) — Organized actors (state AGs, courts, media, civil society) treating electoral authoritarianism as temporary and solvable via institution-building and norm restoration. See constraint as coordination failure with generational sunset: building alternative verification mechanisms (ballot security, media transparency, legal frameworks). d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.20. Low effective extraction; coalition perceives agency and pathway.
constraint_indexing:constraint_classification(trumps_second_term_authoritarianism_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION (MOUNTAIN) — Risks framing electoral authoritarianism as inherent to mass democracy: inevitably, concentration of executive power, media capture, and electoral suppression emerge once polarization exceeds threshold. Views constraint as law of political physics — civilizations at high polarization always experience authoritarianism. However, base properties (ε=0.58, suppression=0.68, theater=0.64) contradict mountain classification. This is a false summit: contingent institutional choices (norm erosion, constitutional interpretation, political strategy) are naturalized as immutable.
constraint_indexing:constraint_classification(trumps_second_term_authoritarianism_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trumps_second_term_authoritarianism_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trumps_second_term_authoritarianism_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trumps_second_term_authoritarianism_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trumps_second_term_authoritarianism_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trumps_second_term_authoritarianism_2026, TR),
    TR >= 0.70.

:- end_tests(trumps_second_term_authoritarianism_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts institutional autonomy (civil service independence, judicial check, legislative power) and electoral legitimacy (suppressed opposition voice, gerrymandered representation, prosecutorial targeting). But it is not total extraction (0.70+) because: (1) electoral competition formally continues (not yet a closed system); (2) organized opposition retains some institutional footholds (state attorneys general, some courts, media fragments); (3) the system still requires coordination (can't function as pure theft, must maintain some legitimacy). The value of 0.58 reflects moderate-to-high extraction paired with continued institutional complexity. Suppression (0.68): Moderate-high. Significant barriers to exit and organizing: voter suppression mechanisms, prosecution of opposition leaders, civil service monitoring, media bias, regulatory control. But suppression is not total (0.85+) because: opposition movements persist, courts issue orders, some media remains independent, federal-state conflicts unresolved. Theater ratio (0.64): Moderate-high. Authoritarianism operates substantially through formal legal channels (executive orders, prosecutorial decisions, regulatory rulemaking, legislative votes) that maintain constitutional appearance while eroding function. This theater is real — not extrajudicial violence but procedurally-proper extraction.
 *
 * PERSPECTIVAL GAP:
 *   DIAGNOSTIC EXEMPLAR OF PERSPECTIVAL RANGE: The opposition voter sees pure snare (no exit, full extraction). The executive sees pure rope (coordination hierarchy, beneficiary status). The civil service sees tangled rope (forced loyalty but still embedded in function). The party leadership sees tangled rope (losing autonomy but maintaining power). Constitutional checks see piton (still invoked but degraded). Resistance movements see scaffold (organizing to restore institutions, perceiving sunset). The global observer risks seeing mountain (naturalizing as inevitable) — but the structural data reveals this as false naturalization. The perspectival gap is maximal, indicating that all six types are structurally legitimate readings of the same constraint from different positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Executive apparatus: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; low/negative effective extraction. Opposition voters: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; no exit. Civil service: Victim + constrained → d≈0.70, f(d)≈1.08. High extraction; some exit at cost. Party leadership: Mixed (beneficiary of party coordination + victim of executive dominance) + mobile → d≈0.55, f(d)≈0.75. Moderate extraction; can exit via defection. Constitutional mechanisms: Victim + constrained → d≈0.60, f(d)≈0.80. Moderate-high extraction; institutional inertia prevents rapid exit. Resistance coalition: Victim + mobile → d≈0.35, f(d)≈0.35. Low extraction relative to power; coalition has agency and pathways. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival (naturalizes constraint); engine's false summit detector should flag this.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED via perspectival pluralism: The core ambiguity is whether electoral authoritarianism is (A) an immutable feature of high-polarization democracies (mountain), or (B) an extractive system that coordinates executive power by suppressing alternatives (snare/tangled rope). The JSON resolves this by showing that BOTH are legitimate perspectival readings, but they apply to different agents. From the global civilizational view, authoritarianism appears inevitable (mountain from that position). From the opposition voter's view, it is pure extraction (snare). From the executive's view, it is coordination (rope). The perspectival pluralism dissolves the mandatrophy: the constraint is not 'one of these six types' but 'all six types from different observation positions.' The analytical observer's mountain is flagged as false summit (naturalization of contingent choices). The true structural answer is tangled rope (from the most information-rich perspective: organized resistance that sees agency + restoration pathway), with snare as the most vulnerable agent's experience (opposition voters), and rope as the beneficiary's experience (executive). The classification taxonomy is doing its job: preventing mislabeling of authoritarianism as either inevitable law or pure coordination, while capturing that both dynamics operate simultaneously from different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_threshold_reversibility,
    'Is electoral authoritarianism at this intensity reversible via electoral competition, or has institutional capture crossed a point of no return?',
    'Monitor prosecution of political opponents across election cycles; test whether opposition victory would restore institutional independence; examine judicial willingness to enforce constraints on winning party',
    'If reversible: constraint is Tangled Rope with real sunset (scaffold logic holds). If irreversible: constraint is Snare or even Mountain (natural law of regime consolidation). Classification depends on empirical answer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_threshold_reversibility, empirical, 'Whether authoritarianism is reversible through electoral competition').

omega_variable(
    civil_service_loyalty_tipping_point,
    'At what purge percentage (% of civil service replaced for political loyalty) does the system transition from extracting compliance to extracting existential allegiance?',
    'Tracking civil service turnover rates, comparing to historical purges (McCarthy era, other authoritarian states); measuring deviation between institutional position and policy output to infer actual institutional control',
    'Below 15-20%: still extractive coordination (Tangled Rope). Above 30%: approaching existential extraction (Snare-like for civil service). Above 50%: full institutional replacement (Mountain-like for civil service stability — no alternative institution exists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_service_loyalty_tipping_point, empirical, 'Civil service purge percentage threshold for existential vs extractive transition').

omega_variable(
    media_market_fragmentation_escape,
    'Does distributed digital media (alternative platforms, independent outlets, state-based counter-narratives) actually escape executive narrative control, or does algorithmic sorting reproduce authoritarianism in decentralized form?',
    'Measure information reach variance across partisan/regional audiences; identify whether alternative media builds public coordination against authoritarianism or fragments resistance; compare to traditional media capture scenarios',
    'If fragmentation enables escape: suppression < 0.60, extractiveness < 0.50, classification trends toward Tangled Rope with lower χ. If algorithmic sorting reproduces control: suppression > 0.75, extractiveness > 0.65, classification stable as Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(media_market_fragmentation_escape, empirical, 'Whether distributed media enables escape from narrative control or reproduces authoritarianism').

omega_variable(
    international_structural_constraint,
    'Does the U.S. position in global institutions (NATO, trade, currency reserve status) impose hard constraints on authoritarianism, or can authoritarianism coexist with institutional dominance?',
    'Test whether ally defection, trade sanctions, or institutional challenge changes executive behavior; compare to other authoritarian regimes with global power (China, Russia) and their international constraints',
    'If global position constrains: extractiveness limited by alliance cohesion (ε < 0.50). If authoritarian regime can maintain global power: extractiveness unconstrained (ε > 0.70, approaching Mountain of regime consolidation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_structural_constraint, empirical, 'Whether global institutional position constrains domestic authoritarianism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trumps_second_term_authoritarianism_2026, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trumpauth_tr_t0, trumps_second_term_authoritarianism_2026, theater_ratio, 0, 0.45).
narrative_ontology:measurement(trumpauth_tr_t6, trumps_second_term_authoritarianism_2026, theater_ratio, 6, 0.58).
narrative_ontology:measurement(trumpauth_tr_t12, trumps_second_term_authoritarianism_2026, theater_ratio, 12, 0.64).

% Extraction over time
narrative_ontology:measurement(trumpauth_be_t0, trumps_second_term_authoritarianism_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(trumpauth_be_t6, trumps_second_term_authoritarianism_2026, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(trumpauth_be_t12, trumps_second_term_authoritarianism_2026, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trumps_second_term_authoritarianism_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(trumps_second_term_authoritarianism_2026, press_freedom_suppression_2026).
narrative_ontology:affects_constraint(trumps_second_term_authoritarianism_2026, judicial_independence_erosion).
narrative_ontology:affects_constraint(trumps_second_term_authoritarianism_2026, civil_service_politicization).
narrative_ontology:affects_constraint(trumps_second_term_authoritarianism_2026, electoral_suppression_mechanisms).

% DUAL FORMULATION NOTE:
% Electoral authoritarianism decomposes into multiple structural constraints: media capture (ε≈0.62), civil service politicization (ε≈0.55), prosecutorial targeting (ε≈0.64), and electoral suppression (ε≈0.71). This story captures the system-level constraint (how these mechanisms coordinate into a unified extractive apparatus); downstream stories address mechanism-specific constraints. All affect each other: civil service politicization enables prosecutorial targeting; prosecutorial targeting reinforces electoral suppression; electoral suppression sustains executive mandate for civil service purges. The network is fully connected at the extraction level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trumps_second_term_authoritarianism_2026, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
