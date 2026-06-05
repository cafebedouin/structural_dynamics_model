% ============================================================================
% CONSTRAINT STORY: taiwan_grand_bargain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taiwan_grand_bargain, []).

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
 *   constraint_id: taiwan_grand_bargain
 *   human_readable: The U.S.-China Taiwan Grand Bargain
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   The U.S.-China Taiwan Grand Bargain represents a potential
 *   macrostructural geopolitical settlement in which the United States
 *   implicitly or explicitly concedes Taiwan's eventual political integration
 *   with the People's Republic of China in exchange for Chinese commitment to
 *   avoid military escalation, maintain semiconductor supply chain stability,
 *   and accept a regional sphere-of-influence arrangement in the
 *   Indo-Pacific. This constraint exhibits Tangled Rope structure: both
 *   extractive and coordinative elements are genuine and structurally
 *   necessary. The coordination function (avoiding military conflict,
 *   stabilizing trade) is real and benefits both great powers and
 *   multinational firms. The extraction is equally real: Taiwan's political
 *   autonomy is traded away without its democratic consent; regional alliance
 *   architecture is subordinated to bilateral deal; democratic governance
 *   norms are legitimated as tradeable commodities in great-power
 *   negotiation. The constraint is not a Snare disguised as Rope coordination
 *   (extraction hidden behind false coordination claims); rather, extraction
 *   and coordination are genuinely inseparable in this structure. Theater
 *   ratio (0.64) reflects increasing reliance on multilateral institutional
 *   theater (UNCLOS reform, ASEAN endorsements, UN Security Council
 *   affirmations) to provide legitimacy for an essentially bilateral
 *   arrangement that bypasses normal democratic and multilateral processes.
 *
 * KEY AGENTS:
 *   - U.S. Strategic Establishment: Primary beneficiary (institutional/arbitrage) — solves Indo-Pacific military rivalry, preserves hegemonic order in altered form, reduces military spending on Taiwan contingency
 *   - Chinese Communist Party Leadership: Primary beneficiary (institutional/arbitrage) — achieves territorial consolidation without military cost, establishes regional hegemony, stabilizes economic relationship
 *   - Multinational Semiconductor Firms: Secondary beneficiary (organized/arbitrage) — stable investment environment, reduced geopolitical risk premium, supply chain certainty
 *   - Taiwan Government: Constrained victim (moderate/constrained) — loses strategic autonomy, but gains peace guarantee and economic integration benefits
 *   - Taiwanese Electorate: Primary victim (powerless/trapped) — political autonomy conceded without democratic choice, future governance determined by great-power negotiation
 *   - Regional Alliance Architecture: Constrained victim (organized/constrained) — Japan, South Korea, ASEAN forced to accept fait accompli, reduced tripartite security autonomy
 *   - Democratic Governance Norms: Trapped victim (powerless/trapped) — self-determination principle sacrificed, sphere-of-influence model legitimated as alternative to democratic choice
 *   - International Rules-Based Order: Degraded institution (analytical/analytical) — multilateral processes bypassed, institutional theater increases while enforcement capacity decreases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taiwan_grand_bargain, 0.58).
domain_priors:suppression_score(taiwan_grand_bargain, 0.68).
domain_priors:theater_ratio(taiwan_grand_bargain, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taiwan_grand_bargain, extractiveness, 0.58).
narrative_ontology:constraint_metric(taiwan_grand_bargain, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(taiwan_grand_bargain, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taiwan_grand_bargain, tangled_rope).
narrative_ontology:human_readable(taiwan_grand_bargain, "The U.S.-China Taiwan Grand Bargain").
narrative_ontology:topic_domain(taiwan_grand_bargain, "economic/geopolitical").

domain_priors:requires_active_enforcement(taiwan_grand_bargain).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taiwan_grand_bargain, us_strategic_interests).
narrative_ontology:constraint_beneficiary(taiwan_grand_bargain, chinese_territorial_consolidation).
narrative_ontology:constraint_beneficiary(taiwan_grand_bargain, multinational_semiconductor_firms).
narrative_ontology:constraint_victim(taiwan_grand_bargain, taiwanese_political_autonomy).
narrative_ontology:constraint_victim(taiwan_grand_bargain, regional_stability_architecture).
narrative_ontology:constraint_victim(taiwan_grand_bargain, democratic_governance_norms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TAIWANESE ELECTORATE (SNARE) — Trapped by great-power bargaining; no meaningful exit option. Taiwan's domestic choice architecture is subordinated to bilateral U.S.-China negotiation. Extraction: loss of political voice in its own constitutional future. d≈0.93, f(d)≈1.38, σ=0.9 → χ≈0.72.
constraint_indexing:constraint_classification(taiwan_grand_bargain, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: TAIWAN GOVERNMENT (TANGLED ROPE) — Constrained by military dependence and economic interdependence with both powers. Bargain offers coordination (stability guarantee, reduced military threat) but at cost of reduced strategic autonomy. d≈0.70, f(d)≈1.05, σ=0.9 → χ≈0.54.
constraint_indexing:constraint_classification(taiwan_grand_bargain, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: U.S. STRATEGIC ESTABLISHMENT (ROPE) — Primary beneficiary. Bargain solves coordination problem: reduces military rivalry in Indo-Pacific, stabilizes semiconductor supply chain, enables U.S. pivot to other geopolitical theaters. Extraction subordinate to coordination function. d≈0.08, f(d)≈-0.09, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(taiwan_grand_bargain, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CHINESE COMMUNIST PARTY LEADERSHIP (ROPE) — Primary beneficiary. Bargain solves existential coordination problem: secures Taiwan reintegration without military escalation, stabilizes economic relationship with U.S., establishes regional hegemony. Extraction subordinate to coordination function. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(taiwan_grand_bargain, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MULTINATIONAL SEMICONDUCTOR FIRMS (ROPE) — Secondary beneficiary. Bargain establishes stable investment environment in Taiwan and mainland China; reduces geopolitical risk premium on chip design/manufacturing. Coordination function: certainty of supply chains. d≈0.12, f(d)≈-0.06, σ=1.2 → χ≈-0.04.
constraint_indexing:constraint_classification(taiwan_grand_bargain, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DEMOCRATIC GOVERNANCE NORMS (SNARE) — Trapped victim at civilizational scale. Grand bargain treats democratic choice (Taiwan's political autonomy) as tradeable asset in great-power negotiation. Extraction: legitimation of sphere-of-influence model over self-determination. No exit; all democracies bear reputational/security cost. d≈0.92, f(d)≈1.36, σ=1.2 → χ≈0.89.
constraint_indexing:constraint_classification(taiwan_grand_bargain, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: REGIONAL ALLIANCE ARCHITECTURE (TANGLED ROPE) — Constrained by Indo-Pacific security order. Bargain offers coordination (great-power bipolarity replaces multipolar ambiguity, reduces three-way hedging costs) but at extraction cost: bilateral U.S.-China deal subordinates tripartite alliance concerns; Japan and South Korea forced to accept fait accompli. d≈0.68, f(d)≈1.02, σ=1.1 → χ≈0.58.
constraint_indexing:constraint_classification(taiwan_grand_bargain, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 8: INTERNATIONAL RULES-BASED ORDER (PITON) — Sees the bargain as degraded multilateralism: formal institutions (UN, WTO, UNCLOS) are bypassed in favor of bilateral great-power deal. The institutional order persists (UN still convenes) but has become performative; real decisions made through closed-door negotiation. theater_ratio=0.64 (institutional theater, not functional enforcement). d≈0.75, f(d)≈1.10, σ=1.2 → χ≈0.62.
constraint_indexing:constraint_classification(taiwan_grand_bargain, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_grand_bargain_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taiwan_grand_bargain, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_grand_bargain, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(taiwan_grand_bargain, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(taiwan_grand_bargain, TR),
    TR >= 0.70.

:- end_tests(taiwan_grand_bargain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The bargain extracts Taiwan's political future without democratic voice, but extraction is not totalizing — Taiwan retains economic benefits, security guarantees, and internal governance capacity under likely one-country-two-systems framework. The extraction grows over the 10-year measurement interval (0.35 → 0.58) as the initial agreement is implemented and boundary conditions (remaining political autonomy, security guarantees) erode through incremental Chinese concession-seeking. Suppression (0.68): High. Substantial barriers prevent Taiwan electorate from exercising countervailing power: military asymmetry with China, economic interdependence with both U.S. and China, lack of veto power in bilateral negotiation, international recognition of PRC-only sovereignty norm. Regional allies face suppression through bilateral exclusion from negotiation (constrained choice to accept deal or face Indo-Pacific realignment). Theater ratio (0.64): Moderate-high and rising. Initial negotiations may use procedural legitimacy (UN multilateralism, ASEAN consensus-building), but actual decision-making occurs in bilateral channels. As agreement matures, theater increases — institutions are maintained (UN continues to convene, ASEAN forums continue) but real enforcement occurs through bilateral U.S.-China threat and accommodation.
 *
 * PERSPECTIVAL GAP:
 *   Enormous perspectival gap between beneficiaries and victims. U.S. and Chinese establishments see a coordination solution (Rope) — they are solving the genuine problem of avoiding military escalation and economic disruption. Multinational firms see stabilization (Rope). Taiwan government sees mixed coordination and extraction (Tangled Rope) — the bargain offers peace but at autonomy cost. Taiwanese electorate sees pure extraction (Snare) — no voice in outcome, only subordination to great-power decision. Regional allies see extraction masquerading as coordination (Tangled Rope) — they benefit from stability but lose strategic agency. Democratic norms see extraction at civilizational scale (Snare) — the bargain legitimates sphere-of-influence over self-determination. International order sees degradation of its own institutional function (Piton) — multilateral theater persists but real authority moves to bilateral negotiation.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. Strategic Establishment: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary; full exit option (can pursue alternative strategies). Chinese CCP: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary; full exit option. Multinational firms: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.06. Secondary beneficiary with full mobility. Taiwan government: Victim + constrained → d≈0.70, f(d)≈1.05. No exit from bilateral negotiation; must accept outcome. Taiwanese electorate: Victim + trapped → d≈0.93, f(d)≈1.38. Complete entrenchment; cannot exit great-power negotiation structure. Regional allies: Victim + constrained → d≈0.68, f(d)≈1.02. Constrained by bilateral fait accompli; limited escape routes. Democratic norms: Victim + trapped → d≈0.92, f(d)≈1.36. Universally trapped; all democracies bear reputational cost of norm degradation. Institutional order: Mixed observer + analytical → d≈0.75, f(d)≈1.10. Theater ratio gate drives Piton classification despite moderate extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint resolves the mandatrophy (how can the same structure appear as both coordination and extraction?) by recognizing that the coordination function (avoiding military escalation) and extraction function (trading political autonomy) are genuinely inseparable in this structure. U.S. and China CAN achieve stable coexistence only by establishing a hegemonic/spheres-of-influence arrangement that treats Taiwan as a bargaining chip rather than an autonomous agent. The coordination benefit to the great powers IS STRUCTURALLY DEPENDENT on the extraction from Taiwan and democratic norms. This is not a case of false natural law (falsely claiming coordination is inevitable) — the military escalation scenario is genuinely catastrophic for all parties, and some form of coercive settlement is structurally preferable to continued hedging and arms races. The mandatrophy resolves by recognizing that Tangled Rope is the correct classification precisely because both elements are real and necessary. The false framing would be claiming this is 'just coordination' (Rope) while ignoring extraction; or claiming it is 'just extraction' (Snare) while ignoring the genuine conflict-avoidance function. Tangled Rope captures the structural reality: genuine coordination of great-power interests, genuine extraction of Taiwan and democratic norms, both elements causally necessary for the constraint to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism_credibility,
    'What mechanism binds both U.S. and China to the bargain terms once Taiwan political autonomy is conceded? Can either party credibly enforce against the other?',
    'Historical precedent analysis (Molotov-Ribbentrop, Munich, spheres-of-influence agreements); game-theoretic analysis of incentive structures post-agreement; interview evidence from negotiators on enforcement architecture',
    'If credible enforcement exists: bargain is stabilizing (Rope classification for beneficiaries valid). If enforcement mechanism is weak: agreement fragments after concessions made, revealing Snare structure (U.S. and China extract sequentially from Taiwan).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_mechanism_credibility, empirical, 'Whether enforcement mechanism can credibly bind both great powers').

omega_variable(
    democratic_representation_under_ccp_rule,
    'Under CCP governance model post-unification, what level of political representation and local autonomy does Taiwan retain? Does ''one country, two systems'' Hong Kong model apply, or tighter integration?',
    'CCP policy documents on Taiwan governance; historical comparison with Hong Kong post-1997 autonomy degradation; pre-agreement confidential understandings between U.S./China on Taiwan political status post-unification',
    'If genuine political autonomy preserved: harm to democratic norms is limited (Tangled Rope for Taiwan). If political autonomy fully absorbed: pure Snare for Taiwanese electorate, pure extraction for democratic norms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_representation_under_ccp_rule, conceptual, 'Scope of democratic representation Taiwan retains under CCP rule').

omega_variable(
    semiconductor_supply_chain_resilience,
    'Does unified Taiwan under CCP guarantee semiconductor supply chain stability, or does geopolitical risk shift from cross-strait conflict to U.S.-China technology war?',
    'Supply chain modeling under CCP control; U.S. decoupling strategy post-unification; Taiwan semiconductor firm relocation patterns; TSMC governance structure changes',
    'If supply chain stabilized: multinational firm benefit is real (Rope valid). If geopolitical risk merely shifts: firms face extraction under new regime; bargain is Snare for tech sector.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semiconductor_supply_chain_resilience, empirical, 'Whether semiconductor supply chains achieve net stability under unified ownership').

omega_variable(
    us_commitment_credibility_post_concession,
    'After U.S. concedes Taiwan political autonomy, what prevents China from extracting further concessions (South China Sea, regional hegemony, technology transfer) by threatening to restart conflict?',
    'Game-theoretic analysis of post-concession incentive structures; historical precedent (appeasement dynamics, ratchet effects in negotiations); U.S. military posture statements post-agreement',
    'If U.S. credibly commits to containment of further extraction: bargain stabilizes. If ratchet effect occurs (China extracts incrementally): bargain reveals Snare structure for U.S. interests (concessions generate path-dependent vulnerability).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(us_commitment_credibility_post_concession, empirical, 'Whether U.S. commitment remains credible after initial concession').

omega_variable(
    theater_ratio_institutional_legitimacy,
    'Does the multilateral institutional theater (UN endorsement, formal treaty language, ASEAN consensus) provide genuine legitimacy, or merely performative cover for bilateral deal?',
    'Institutional analysis of treaty structure (binding vs non-binding clauses); stakeholder access to negotiation processes; institutional adaptation patterns post-agreement',
    'If theater provides genuine legitimacy: Piton classification is stable. If theater is pure cover: international order is fully degraded, and institutions become pure extraction apparatus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_institutional_legitimacy, conceptual, 'Whether multilateral institutional theater provides genuine legitimacy or performative cover').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taiwan_grand_bargain, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twgb_theater_t0, taiwan_grand_bargain, theater_ratio, 0, 0.38).
narrative_ontology:measurement(twgb_theater_t5, taiwan_grand_bargain, theater_ratio, 5, 0.52).
narrative_ontology:measurement(twgb_theater_t10, taiwan_grand_bargain, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(twgb_extractiveness_t0, taiwan_grand_bargain, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(twgb_extractiveness_t5, taiwan_grand_bargain, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(twgb_extractiveness_t10, taiwan_grand_bargain, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taiwan_grand_bargain, enforcement_mechanism).
narrative_ontology:affects_constraint(taiwan_grand_bargain, indo_pacific_military_balance).
narrative_ontology:affects_constraint(taiwan_grand_bargain, semiconductor_supply_chain_geopolitics).
narrative_ontology:affects_constraint(taiwan_grand_bargain, regional_alliance_architecture).
narrative_ontology:affects_constraint(taiwan_grand_bargain, democratic_norm_sphere_of_influence).

% DUAL FORMULATION NOTE:
% The Taiwan Grand Bargain decomposes into multiple structural constraints: (1) the military escalation prevention mechanism (separate ε, high mountain-like stability); (2) the political autonomy transfer (separate ε, pure snare); (3) the semiconductor supply chain stabilization (separate ε, coordination-heavy tangled rope). This story models the synthesized constraint experienced by all parties simultaneously. Each downstream constraint represents one dimension of the bargain's structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(taiwan_grand_bargain, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
