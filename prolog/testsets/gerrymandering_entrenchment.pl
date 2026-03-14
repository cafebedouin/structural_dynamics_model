% ============================================================================
% CONSTRAINT STORY: gerrymandering_entrenchment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gerrymandering_entrenchment, []).

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
 *   constraint_id: gerrymandering_entrenchment
 *   human_readable: Gerrymandering Entrenchment: Political Boundary Manipulation as Mixed Coordination-Extraction
 *   domain: political_systems/electoral_mechanics
 *
 * SUMMARY:
 *   Gerrymandering entrenchment represents a structural constraint on
 *   electoral representation that combines genuine coordination (assembling
 *   voters into coherent districts) with asymmetric extraction (optimizing
 *   districts to suppress opposition power). The constraint exhibits all six
 *   DR types from different perspectives. For opposition voters, it is a
 *   snare — their electoral power is mathematically nullified through
 *   strategic packing and cracking with no meaningful exit. For the incumbent
 *   party apparatus, it is pure coordination — designing districts that
 *   preserve majority control is a straightforward solving of electoral
 *   geography. For independent redistricting reformers, it is a temporary
 *   problem with a sunset clause — new institutional mechanisms are building
 *   exit pathways. For the redistricting process itself, the theater ratio
 *   (0.65) reveals substantial performative content: public participation and
 *   formal fairness procedures mask predetermined partisan outcomes. The
 *   constraint's base extractiveness (0.58) has accumulated over two decades
 *   as mapping technology has enabled more precise targeting. Computer-aided
 *   gerrymandering with voter-level data has transformed district design from
 *   an approximate art (where some geographic randomness produced occasional
 *   unintended competitiveness) into an exact science (where nearly every
 *   district outcome is predetermined). The constraint demonstrates how a
 *   mechanism that solves one coordination problem (electoral aggregation)
 *   can simultaneously enable massive extraction against those not captured
 *   by the solution.
 *
 * KEY AGENTS:
 *   - Opposition Voters in Gerrymandered Districts: Primary victims (powerless/trapped) — their electoral power is systematically nullified through packing or cracking; cannot exit or meaningfully organize
 *   - Incumbent Party Apparatus: Primary beneficiary (institutional/arbitrage) — experiences gerrymandering as pure coordination and electoral advantage; controls redistricting process
 *   - Electoral Reform Coalition: Organized actors (organized/constrained) — good-government groups, citizen-led ballot initiatives, some state legislatures building alternative mechanisms with sunset logic
 *   - Redistricting Process and Theater: Institutional mechanism (institutional/arbitrage) — maintains appearance of fairness through formal procedures while serving partisan function; theater-driven piton degradation
 *   - Regional Demographic Minorities: Secondary victims (moderate/constrained) — benefit from geographic district structure as baseline for organization but experience manipulation within that structure
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing algorithmic extraction as inevitable consequence of geographic representation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gerrymandering_entrenchment, 0.58).
domain_priors:suppression_score(gerrymandering_entrenchment, 0.72).
domain_priors:theater_ratio(gerrymandering_entrenchment, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gerrymandering_entrenchment, extractiveness, 0.58).
narrative_ontology:constraint_metric(gerrymandering_entrenchment, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gerrymandering_entrenchment, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gerrymandering_entrenchment, tangled_rope).
narrative_ontology:human_readable(gerrymandering_entrenchment, "Gerrymandering Entrenchment: Political Boundary Manipulation as Mixed Coordination-Extraction").
narrative_ontology:topic_domain(gerrymandering_entrenchment, "political_systems/electoral_mechanics").

domain_priors:requires_active_enforcement(gerrymandering_entrenchment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gerrymandering_entrenchment, incumbent_party_majority).
narrative_ontology:constraint_beneficiary(gerrymandering_entrenchment, party_apparatus).
narrative_ontology:constraint_victim(gerrymandering_entrenchment, opposition_voters).
narrative_ontology:constraint_victim(gerrymandering_entrenchment, electoral_competitiveness).
narrative_ontology:constraint_victim(gerrymandering_entrenchment, democratic_representation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPPOSITION VOTERS (SNARE) — Voters in heavily gerrymandered districts experience complete suppression of electoral power. Packing (concentrating opposition voters into few districts) or cracking (dispersing them to guarantee minority status in many) removes meaningful exit: moving districts is expensive; voting differently yields no observable effect; organization is fragmented by design. Maximum extraction — the constraint prevents these voters from converting demographic support into electoral representation.
constraint_indexing:constraint_classification(gerrymandering_entrenchment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL DEMOGRAPHIC MINORITIES (TANGLED ROPE) — These groups benefit from coordination on representation mechanisms (districting preserves some baseline structure for organizing political power) but bear asymmetric extraction through manipulation. Constrained exit: they cannot easily relocate and retain political voice, but can in principle organize at neighboring scales or shift coalition strategy. The constraint both enables and extracts — it coordinates around geographic identity while enabling its suppression.
constraint_indexing:constraint_classification(gerrymandering_entrenchment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT PARTY APPARATUS (ROPE) — The primary beneficiary experiences gerrymandering as pure coordination: designing districts that aggregate their voters, enable coalition-building, and preserve majority control is a genuine solving of the electoral coordination problem for the party. The apparatus has arbitrage options (can abandon gerrymandering, accept competitive maps, and seek other structural advantages). Net beneficiary experiencing the constraint as coordination, not extraction.
constraint_indexing:constraint_classification(gerrymandering_entrenchment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REDISTRICTING PROCESS (PITON) — The ostensible institutional function is to ensure equal population representation and fair district design. In practice, the process is substantially theatrical: public comment periods create appearance of input while the outcome is predetermined by the controlling party; claims about 'communities of interest' and 'compactness' are applied selectively to justify pre-planned extractions; court challenges are managed through strategic delays. The theater_ratio (0.65) reflects that the formal process maintains legitimacy through procedural performance while substantive fairness is degraded. The function has atrophied — from a coordination mechanism (fair representation through district design) to an extraction mechanism maintained by institutional inertia.
constraint_indexing:constraint_classification(gerrymandering_entrenchment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL REFORM COALITION (SCAFFOLD) — Organized agents (good-government groups, voter initiatives, some state legislatures) see gerrymandering as a temporary coordination failure being addressed through structured reforms: independent redistricting commissions (removing partisan control), ranked-choice voting (reducing the need for geographic concentration), proportional representation pilots. Constrained exit but clear sunset logic — if these reforms scale, the extraction mechanism's leverage declines. The reforms are not universally implemented, but their spread creates an exit path.
constraint_indexing:constraint_classification(gerrymandering_entrenchment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL GEOGRAPHY VIEW (MOUNTAIN) — From a civilizational perspective, some electoral advantage to compact geography is inherent in how district-based systems work: voters distributed unevenly across space will naturally create some districts where one party concentrates. This perspective naturalizes gerrymandering as an unavoidable consequence of geography. However, the structural data contradicts this: modern gerrymandering uses algorithmic optimization and detailed voter data to produce maps far more extractive than geography alone. The mountain classification is a false summit — the 'natural' framing conceals the extent of active manipulation.
constraint_indexing:constraint_classification(gerrymandering_entrenchment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gerrymandering_entrenchment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gerrymandering_entrenchment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gerrymandering_entrenchment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gerrymandering_entrenchment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gerrymandering_entrenchment, TR),
    TR >= 0.70.

:- end_tests(gerrymandering_entrenchment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. Over the measurement interval (0-20 years), algorithmic sophistication and voter-level data access have increased the precision of partisan targeting, raising extractiveness from 0.35 (where geographic randomness still produced some competitive districts) to 0.58 (where nearly all outcomes are predetermined). The rise reflects genuine increase in extraction capacity, not measurement change. Suppression (0.72): High. Opposition voters in gerrymandered districts face multiple suppression mechanisms: mathematical nullification (votes don't translate to representation), organizational fragmentation (packing concentrates them into few districts; cracking isolates them), relocation barriers (moving to a competitive district is expensive), and defeatism feedback (low expected win probability reduces mobilization even when coordination would be possible). Suppression is structural rather than merely cultural — it flows from district geometry. Theater ratio (0.65): Moderate-high and rising. Redistricting processes incorporate extensive performative elements: community input periods that don't change outcomes, fairness criteria (compactness, communities of interest) applied selectively, court challenges navigated through procedural delays, and bipartisan rhetoric about 'fair representation' masking partisan design. The rise from 0.45 to 0.65 reflects increased institutional maintenance of legitimacy (greater theater) as the extraction mechanism becomes more visible.
 *
 * PERSPECTIVAL GAP:
 *   The gerrymandering constraint demonstrates maximum perspectival divergence. The incumbent party sees pure coordination (Rope) — they are solving the legitimate problem of assembling voters into functional districts. Opposition voters see pure extraction (Snare) — their votes are made meaningless by design. Regional minorities see mixed coordination-extraction (Tangled Rope) — the district system enables geographic representation while enabling manipulation within it. Reformers see a temporary problem (Scaffold) — new mechanisms are building exit pathways. The redistricting process itself appears as degraded (Piton) — the function (fair representation) has atrophied while the institution persists through theater. The analytical observer at civilizational scope risks seeing an immutable natural law (Mountain) — 'some electoral advantage to compact geography is inherent' — but the structural data reveals algorithmic optimization far exceeding geographic necessity. The perspectival gap is driven by directionality: beneficiaries with arbitrage options experience low chi; trapped agents experience maximum chi; organized reformers experience moderate chi with exit pathways.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural relationship to extraction flow. The incumbent party as beneficiary (victim=false, exit=arbitrage) derives d ≈ 0.15, producing f(d) ≈ -0.01, resulting in negative or near-zero effective extraction (they experience the constraint as a positive coordination mechanism). Opposition voters as victims (victim=true, exit=trapped) derive d ≈ 0.95, producing f(d) ≈ 1.42, resulting in high effective extraction chi ≈ 0.58 × 1.42 × 1.0 = 0.82 (national scope σ=1.0). Regional minorities as constrained victims (victim=true, exit=constrained) derive d ≈ 0.75, producing f(d) ≈ 1.15, resulting in chi ≈ 0.58 × 1.15 × 1.0 = 0.67. The electoral reform coalition as organized agents with constrained exits and beneficiary status (partial) derive d ≈ 0.50, producing f(d) ≈ 0.65, resulting in chi ≈ 0.58 × 0.65 × 1.0 = 0.38. These values confirm the perspectival gap: beneficiaries see low extraction (Rope); trapped victims see high extraction (Snare); partially beneficiary organized groups see moderate extraction (Scaffold). The analytical observer at universal scope takes d ≈ 0.72 (from canonical analytical), producing f(d) ≈ 1.15, but the mountain claim is contradicted by suppression > 0.70 and active enforcement > true, failing natural law gates.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's claimed type (Tangled Rope) requires three gates: beneficiaries declared (✓ incumbent_party_majority, party_apparatus), victims declared (✓ opposition_voters, electoral_competitiveness, democratic_representation), and active enforcement required (✓ true). The structure satisfies tangled_rope gates. Mandatrophy arises from the false summit (Mountain from analytical perspective at civilizational scope). The analytical view claims natural geography makes some partisan advantage inevitable — this appears as an immutable law (emerges_naturally ≈ true?). However, the base properties contradict this: suppression is 0.72 (far exceeding 0.05 natural law threshold), requires_active_enforcement is true (natural laws don't require enforcement), and theater_ratio is 0.65 (natural laws don't perform). The mountain classification is a false summit produced by conflating 'geographic differences create some partisan variation' (true, low ε) with 'modern gerrymandering extracts using geographic data as cover' (true, high ε). These are distinct claims — the constraint story correctly addresses the second. The analytical observer's natural law framing naturalizes the active manipulation. Mandatrophy resolution: Declare mandatrophy_resolved = false; document the false summit in omegas; rely on suppression ≥ 0.70 and theater_ratio to fail the mountain gates despite the civilizational/universal observer's framing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_between_packing_and_geography,
    'At what point does geographic concentration of voters become gerrymandered packing rather than natural residential clustering?',
    'Comparison of algorithmic vs random district generation; historical analysis of district shapes and demographic breakpoints before/after computer-aided mapping',
    'If threshold is high: many contemporary gerrymandered maps appear as natural geography (mountain or rope perspectives strengthen). If threshold is low: the extraction mechanism is revealed as active manipulation regardless of geographic excuse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_between_packing_and_geography, empirical, 'Threshold distinguishing natural clustering from algorithmic packing').

omega_variable(
    reform_sustainability,
    'Do independent redistricting commissions and ranked-choice voting actually reduce partisan extraction, or are they captured/adapted by the controlling party?',
    'Longitudinal tracking of district competitiveness and proportionality post-reform; measurement of electoral outcomes vs demographic distribution in commission-drawn vs legislatively-drawn maps',
    'If reforms are effective: scaffold perspective is validated and extraction mechanism has genuine sunset. If reforms are captured: the constraint persists under different institutional labels (piton classification strengthens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_sustainability, empirical, 'Whether electoral reforms durably reduce gerrymandering extraction').

omega_variable(
    voter_responsiveness_vs_entrenchment,
    'To what extent does gerrymandering prevent voter responsiveness (extraction) versus enabling legitimate geographic representation (coordination)?',
    'Analysis of swing districts vs safe districts; measurement of vote-to-seat ratios in states with and without partisan gerrymandering; study of whether voters in packed opposition districts increase mobilization attempts despite low win probability',
    'If responsiveness is severely reduced: snare and tangled_rope perspectives accurate. If geographic districts enable representation despite partisan bias: rope and piton perspectives more accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voter_responsiveness_vs_entrenchment, empirical, 'Balance between responsive representation and gerrymandered entrenchment').

omega_variable(
    institutional_path_dependence,
    'Does gerrymandering entrenchment persist because of genuine extraction mechanisms (career incentives, structural power) or because institutional actors have internalized it as normal and unchangeable?',
    'Comparative analysis of jurisdictions with and without gerrymandering; interview data on legislator perceptions of redistricting necessity; measurement of how reformist candidates'' positions on gerrymandering affect electability',
    'If primarily extraction: snare and tangled_rope from victim perspectives. If primarily internalized normalcy: identity_locked dynamics and piton classification strengthen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_path_dependence, conceptual, 'Whether gerrymandering persists via extraction or institutional internalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gerrymandering_entrenchment, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gerrymand_tr_t0, gerrymandering_entrenchment, theater_ratio, 0, 0.45).
narrative_ontology:measurement(gerrymand_tr_t10, gerrymandering_entrenchment, theater_ratio, 10, 0.55).
narrative_ontology:measurement(gerrymand_tr_t20, gerrymandering_entrenchment, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(gerrymand_be_t0, gerrymandering_entrenchment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gerrymand_be_t10, gerrymandering_entrenchment, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(gerrymand_be_t20, gerrymandering_entrenchment, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gerrymandering_entrenchment, resource_allocation).
narrative_ontology:affects_constraint(gerrymandering_entrenchment, voter_mobilization_dynamics).
narrative_ontology:affects_constraint(gerrymandering_entrenchment, minority_representation_voting_rights).
narrative_ontology:affects_constraint(gerrymandering_entrenchment, electoral_college_entrenchment).

% DUAL FORMULATION NOTE:
% Gerrymandering entrenchment is distinct from the natural geographic variation in voter distribution (low ε, mountain/rope). The constraint addresses algorithmic and partisan optimization on top of geography, producing higher extractiveness. Voter mobilization dynamics and minority representation are downstream: gerrymandering suppresses both, creating structural effects on coalition-building and voting rights enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
