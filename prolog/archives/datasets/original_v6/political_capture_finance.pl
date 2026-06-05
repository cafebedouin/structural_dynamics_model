% ============================================================================
% CONSTRAINT STORY: political_capture_finance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_political_capture_finance, []).

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
 *   constraint_id: political_capture_finance
 *   human_readable: Political Capture Finance: Regulatory Governance via Campaign Finance Asymmetry
 *   domain: political_economy/campaign_finance
 *
 * SUMMARY:
 *   Political capture finance describes the structural constraint wherein
 *   campaign finance asymmetry becomes a mechanism for regulatory capture:
 *   candidates require large-scale funding to reach voters; concentrated
 *   capital holders (finance sector incumbents) provide this funding in
 *   exchange for favorable regulatory treatment; voters have no equivalent
 *   financing mechanism to support alternatives; and regulatory barriers to
 *   entry are maintained through the incumbent financing advantage. The
 *   constraint exhibits a genuine coordination function (it solves the
 *   problem of financing candidate communication without state control), but
 *   this function is embedded within systematic asymmetric extraction from
 *   voters and market entrants. The distinguishing feature is that the
 *   'coordination' primarily benefits those already able to aggregate
 *   capital, while the 'extraction' falls on those who cannot. Theater ratio
 *   (0.65) reflects that campaign finance transparency mechanisms (FEC
 *   filings, disclosure requirements) exist but functional source
 *   verification and influence measurement are systematically obscured by
 *   dark money, shell PACs, and attribution ambiguity. The extractiveness
 *   trajectory (0.35 → 0.68 over the interval) shows accumulation: as finance
 *   concentration has increased, the regulatory capture mechanism has become
 *   more efficient and less challenged. This is the signature of Goodhart
 *   drift — the theater ratio and extractiveness moving in the same
 *   direction, indicating proxy metrics (disclosure) diverging from actual
 *   function (opacity).
 *
 * KEY AGENTS:
 *   - Voters: Primary victim (powerless/trapped) — structurally excluded from financing candidate access; no exit option from electoral participation without abandoning political agency
 *   - Excluded Market Entrants: Secondary victim (powerless/trapped) — regulatory barriers to entry in financial services maintained through incumbent campaign spending; cannot exit ambition without exiting market
 *   - Finance Sector Incumbents: Primary beneficiary (institutional/arbitrage) — high exit capacity through geographic, sectoral, and partisan arbitrage; net beneficiary of finance-shaped regulation
 *   - Reform-Minded Legislator: Constrained moderator (moderate/constrained) — depends on campaign finance for re-election viability; has mixed coordination and extraction role
 *   - Campaign Finance Transparency Apparatus: Institutional actor (institutional/arbitrage) — maintains performative disclosure ritual; sees its own mechanisms as degraded (Piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies both genuine coordination function and asymmetric extraction; recognizes tangled structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(political_capture_finance, 0.68).
domain_priors:suppression_score(political_capture_finance, 0.72).
domain_priors:theater_ratio(political_capture_finance, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(political_capture_finance, extractiveness, 0.68).
narrative_ontology:constraint_metric(political_capture_finance, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(political_capture_finance, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(political_capture_finance, tangled_rope).
narrative_ontology:human_readable(political_capture_finance, "Political Capture Finance: Regulatory Governance via Campaign Finance Asymmetry").
narrative_ontology:topic_domain(political_capture_finance, "political_economy/campaign_finance").

domain_priors:requires_active_enforcement(political_capture_finance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(political_capture_finance, finance_sector_incumbents).
narrative_ontology:constraint_beneficiary(political_capture_finance, concentrated_capital_holders).
narrative_ontology:constraint_victim(political_capture_finance, distributed_voters).
narrative_ontology:constraint_victim(political_capture_finance, excluded_entrants).
narrative_ontology:constraint_victim(political_capture_finance, systemic_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE VOTER (SNARE) — Structurally trapped. Cannot exit electoral participation without abandoning political agency; cannot choose between candidates with genuinely independent policy positions on finance regulation because campaign finance asymmetry has already shaped which candidates reach ballot. High suppression: information barriers, geographic voter targeting, ad saturation. No meaningful alternatives available. Maximum experienced extraction.
constraint_indexing:constraint_classification(political_capture_finance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EXCLUDED MARKET ENTRANTS (SNARE) — Trapped by regulatory capture: regulatory barriers to entry in financial services are enforced through campaign finance dynamics. Candidate access to finance determines regulatory agenda. Entrants cannot exit market without abandoning economic ambitions. Suppression is structural: regulatory moats are maintained through incumbent campaign spending. No self-correction mechanism.
constraint_indexing:constraint_classification(political_capture_finance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: REFORM-MINDED LEGISLATOR (TANGLED ROPE) — Constrained by campaign finance dependence for re-election viability but retains some coordination role: legislation on campaign finance itself requires legislative action. Benefits from the legislative position (salary, staff, committee influence) while being extracted from through necessity to fundraise. Can exit (retirement, voluntary departure) but at high career cost. Mixed extraction and coordination function.
constraint_indexing:constraint_classification(political_capture_finance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FINANCE SECTOR INCUMBENTS (ROPE) — Primary beneficiary (institutional/arbitrage). Experiences the constraint as coordination: campaign contributions enable access to legislators, shaping regulatory agenda. High effective exit capacity through arbitrage (can shift contributions across districts, candidates, parties; can exit a given jurisdiction). Net beneficiary — extraction flows toward this agent. The constraint solves their collective action problem of accessing policy makers.
constraint_indexing:constraint_classification(political_capture_finance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CAMPAIGN FINANCE TRANSPARENCY THEATER (PITON) — Disclosure requirements and enforcement mechanisms (FEC filings, contribution limits nominally enforced) are substantially performative. Dark money, corporate shell PACs, and conduit structures obscure the actual flow of capital to candidates. The ritual of transparency (filing reports, nominal limits) persists through institutional inertia despite widespread knowledge that the mechanism is degraded. Theater ratio (0.65) reflects that formal transparency infrastructure exists but functional verification of source and influence is minimal.
constraint_indexing:constraint_classification(political_capture_finance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SYSTEMIC PERSPECTIVE (TANGLED ROPE) — From a civilizational scope, political capture finance serves a genuine coordination function: it channels resources from concentrated capital holders to political candidates, enabling candidate access to media and communication infrastructure that would otherwise require direct state funding. This solves the problem of political finance without centralizing state control of candidate communication. However, it simultaneously extracts from voters (who have no equivalent financial access to candidates) and from market entrants (whose regulatory barriers are maintained through capture). The asymmetric extraction and genuine coordination function coexist structurally — this is Tangled Rope at the analytical level, not Snare or Rope alone.
constraint_indexing:constraint_classification(political_capture_finance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(political_capture_finance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(political_capture_finance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(political_capture_finance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(political_capture_finance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(political_capture_finance, TR),
    TR >= 0.70.

:- end_tests(political_capture_finance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The original research shows campaign finance asymmetry has increased substantially: the top 0.1% of donors provide ~25% of campaign funds (2020 data), and finance sector contributes disproportionately to deregulation-friendly candidates. The extraction is not total (some regulatory constraints persist), but it is substantial and directional. The trajectory from 0.35 to 0.68 reflects accumulation: as wealth concentration has increased and legal barriers to campaign finance (Citizens United, etc.) have been removed, the mechanism has become more efficient. Suppression (0.72): High. Voters face multiple barriers: (a) information barriers (finance flows are obscured by dark money and conduit structures), (b) ballot access barriers (well-financed candidates use media access to narrow perceived choices), (c) geographic targeting (campaign advertising is concentrated in swing districts, suppressing turnout in non-competitive areas), (d) resource barriers (grassroots organization requires funding). Theater ratio (0.65): Moderately high. Transparency mechanisms (FEC filings, contribution limits) create the appearance of oversight, but actual source verification is minimal. Dark money (501c4 and 527 organizations) obscures origin. Corporate shell PACs create attribution ambiguity. Conduit structures (passing funds through multiple entities) degrade traceability. The ritual persists (quarterly filings, nominal limits) despite functional opacity.
 *
 * PERSPECTIVAL GAP:
 *   The Snare perspective (voters/trapped) perceives maximum extraction with no coordination benefit. The Rope perspective (incumbents/arbitrage) perceives pure coordination with no extraction cost to themselves. The Tangled Rope perspectives (reform legislator, analytical observer) see the constraint as genuine hybrid: it coordinates candidate financing (solving the real problem of electoral communication without state monopoly) while simultaneously extracting from voters and maintaining entry barriers. The gap reveals that the constraint's classification depends entirely on structural position. From the inside of the capture, it is coordination. From outside, it is extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective follows from power level, exit options, and beneficiary/victim status. Voters have d ≈ 0.95 (maximum target): powerless, trapped, no exit, victim of extraction. Finance incumbents have d ≈ 0.05 (full beneficiary): institutional power, arbitrage exit, beneficiary status. The sigmoid f(d) maps these to effective extraction multipliers: f(0.95) ≈ 1.42 for voters (maximum experienced extraction), f(0.05) ≈ -0.12 for incumbents (negative extraction, net benefit). The scope modifier σ(national) = 1.0 applies to both. The resulting chi (effective extraction) for voters is χ ≈ 0.68 × 1.42 × 1.0 ≈ 0.96 (near-total extraction), while for finance incumbents χ ≈ 0.68 × (-0.12) × 1.0 ≈ -0.08 (net benefit). This is the structural mechanism of capture: the same constraint produces maximum extraction for those without funding capacity and maximum benefit for those with it.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy by distinguishing the coordination function from the extraction mechanism. The coordination function is real: some mechanism must finance candidate communication in a democracy without direct state funding. Campaign finance provides one solution — capital holders fund candidate access to media. The extraction is also real: this solution asymmetrically burdens voters (who have no equivalent funding mechanism) and excludes market entrants (whose regulatory barriers are maintained through incumbent spending). The mandatrophy is resolved by recognizing that Tangled Rope is the correct classification — NOT because 'it's both,' but because the coordination function and extraction mechanism are structurally inseparable in THIS implementation. The constraint could be restructured: public campaign financing would preserve the coordination benefit (candidate communication) while removing the extraction (voter exclusion). But the current constraint embeds both functions in a single asymmetric flow. The Snare classifications from voter perspectives are correct for their structural position; the Rope classification from incumbent perspectives is correct for their position; the Tangled Rope classification at the analytical level is correct for the system as a whole. Each perspective diagnoses accurately from its position. The mandatrophy confirms that no single classification error is being made — the constraint genuinely exhibits all three structural properties (coordination, extraction, asymmetry) from different angles.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_threshold,
    'At what campaign finance asymmetry threshold does coordination transition to pure extraction? Is there a functional coordination benefit that justifies the asymmetry, or is the ''solution to campaign finance'' framing purely post-hoc?',
    'Comparative analysis: (a) counterfactual state-funded candidate access systems (other democracies); (b) timeline analysis of when finance asymmetry exceeded some meaningful coordination-to-extraction threshold; (c) voter preference aggregation data showing whether finance-shaped candidate slates reflect actual voter demand or manufactured consent',
    'If coordination benefit is real: constraint may be a degraded Scaffold (temporary arrangement becoming extractive) rather than pure Tangled Rope. If coordination benefit is post-hoc: constraint is Snare masked as Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_threshold, empirical, 'Threshold distinguishing coordination from extraction in finance-mediated candidate access').

omega_variable(
    suppression_mechanism_identity_vs_structural,
    'Is voter suppression in this constraint primarily structural (ballot access barriers, voter registration barriers, geographic targeting) or internalized (voters have internalized the belief that finance shapes politics as natural/inevitable and do not perceive alternatives)?',
    'Post-intervention measurement: if campaign finance transparency or reform is suddenly implemented, does voter agency/participation increase immediately (suggesting suppression was structural) or persist (suggesting internalized cognitive capture)?',
    'If structural: suppression may decrease with policy intervention. If internalized: the constraint''s suppression will persist even after structural barriers are nominally removed — voters carry the suppression with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_vs_structural, empirical, 'Whether voter suppression is structural or identity-locked').

omega_variable(
    capture_reversibility,
    'Once regulatory capture is established (incumbents have used finance to shape rules to their benefit), can the constraint be reversed through endogenous political reform, or does it require exogenous shock (war, constitutional crisis, generational change)?',
    'Historical case analysis: instances where campaign finance capture was reversed; measurement of legislative willingness to fund their own replacement (low expected willingness); identification of whether reversals required exogenous events',
    'If endogenously reversible: constraint is Scaffold with sunset mechanism. If exogenous-shock-dependent: constraint is Snare or Tangled Rope with no internal exit mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capture_reversibility, empirical, 'Whether capture is reversible through endogenous political reform').

omega_variable(
    coalition_power_formation,
    'Can powerless voters overcome campaign finance suppression through coalition formation (pooled resources, organized primary challenges, ballot initiatives)? If so, at what scale does coalition power become viable?',
    'Historical measurement of successful coalition campaigns: cost comparison (coalition-funded vs incumbent-funded campaigns); coalition size thresholds above which electoral outcomes shift; analysis of whether coalition victories are sustained or systematically reversed in following cycles',
    'If coalition power is viable at moderate scale: powerless agent power may be upgraded to ''organized'' for certain perspectives, shifting Snare toward Tangled Rope. If coalitions are systematically overwhelmed: Snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_power_formation, empirical, 'Coalition power threshold for voter-organized campaign finance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(political_capture_finance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pcf_tr_t0, political_capture_finance, theater_ratio, 0, 0.4).
narrative_ontology:measurement(pcf_tr_t5, political_capture_finance, theater_ratio, 5, 0.52).
narrative_ontology:measurement(pcf_tr_t10, political_capture_finance, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(pcf_be_t0, political_capture_finance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pcf_be_t5, political_capture_finance, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(pcf_be_t10, political_capture_finance, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(political_capture_finance, enforcement_mechanism).
narrative_ontology:affects_constraint(political_capture_finance, regulatory_capture_finance_sector).
narrative_ontology:affects_constraint(political_capture_finance, voter_agency_suppression).
narrative_ontology:affects_constraint(political_capture_finance, market_entry_barriers_finance).

% DUAL FORMULATION NOTE:
% Political capture finance is the upstream constraint affecting sector-specific regulatory capture (financial services deregulation, telecommunications policy, pharmaceutical pricing). The finance sector's campaign funding shapes the regulatory agenda across multiple sectors. Each sector has its own regulatory capture story with its own extractiveness and beneficiary/victim structure, but all are downstream of this political finance constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(political_capture_finance, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
