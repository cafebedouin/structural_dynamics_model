% ============================================================================
% CONSTRAINT STORY: mexican_airline_merger
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mexican_airline_merger, []).

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
 *   constraint_id: mexican_airline_merger
 *   human_readable: Mexican Airline Merger Antitrust Exemption
 *   domain: economic/competition_policy
 *
 * SUMMARY:
 *   The Mexican airline merger antitrust exemption represents a structural
 *   tension between national economic policy (consolidation of fragmented
 *   domestic carriers into a competitive international actor) and consumer
 *   welfare protection (antitrust enforcement against market concentration).
 *   The Mexican government initiated or encouraged merger of domestic
 *   carriers with stated objectives of creating a 'national champion' capable
 *   of competing internationally and serving remote/underserved routes. This
 *   policy required an antitrust exemption from Mexico's Federal Economic
 *   Competition Commission (COFECE). The constraint exhibits a hybrid
 *   coordination-extraction structure: it solves a real coordination problem
 *   (network fragmentation, international competitiveness) while enabling
 *   extraction (price increases, service reduction for captive passengers).
 *   Passengers on domestic Mexican routes experience this as a snare —
 *   trapped by reduced competition. The government and consolidated carrier
 *   experience it as coordination and benefit. The constraint's theater ratio
 *   reflects that regulatory review processes are largely performative —
 *   COFECE conducts formal analysis but within predetermined policy
 *   parameters set by the executive branch, reducing substantive contestation
 *   of merger terms.
 *
 * KEY AGENTS:
 *   - Mexican Government / Ministry of Transportation: Primary policy initiator (institutional/arbitrage) — benefits from national consolidation narrative and tax optimization; can arbitrage by renegotiating exemption terms
 *   - Mexicana de Aviación / Consolidated Carrier: Primary direct beneficiary (powerful/arbitrage) — captures margin expansion, reduced competitive pressure, international alliance capability
 *   - Mexican Airline Passengers: Primary victim (powerless/trapped) — face higher fares and reduced service quality with no exit options within domestic market
 *   - Competing Regional Carriers: Secondary victim (moderate/constrained) — face reduced market share, constrained by regulatory barriers to expansion
 *   - COFECE (Federal Economic Competition Commission): Institutional regulator (institutional/constrained) — maintains review function but constrained by political pressure; theater ratio high
 *   - USMCA Trade Framework / International Regulators: Organized observers (organized/constrained) — face competing mandates between competition principles and national sovereignty; constrained in enforcement action
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mexican_airline_merger, 0.52).
domain_priors:suppression_score(mexican_airline_merger, 0.58).
domain_priors:theater_ratio(mexican_airline_merger, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mexican_airline_merger, extractiveness, 0.52).
narrative_ontology:constraint_metric(mexican_airline_merger, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(mexican_airline_merger, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mexican_airline_merger, tangled_rope).
narrative_ontology:human_readable(mexican_airline_merger, "Mexican Airline Merger Antitrust Exemption").
narrative_ontology:topic_domain(mexican_airline_merger, "economic/competition_policy").

domain_priors:requires_active_enforcement(mexican_airline_merger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mexican_airline_merger, mexicana_de_aviacion).
narrative_ontology:constraint_beneficiary(mexican_airline_merger, consolidated_carrier).
narrative_ontology:constraint_beneficiary(mexican_airline_merger, mexican_government).
narrative_ontology:constraint_victim(mexican_airline_merger, airline_passengers).
narrative_ontology:constraint_victim(mexican_airline_merger, competing_carriers).
narrative_ontology:constraint_victim(mexican_airline_merger, consumer_welfare).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AIRLINE PASSENGER (SNARE) — Trapped within Mexican domestic air market with limited exit options. Cannot exit the constraint through switching to alternative carriers (reduced competition post-merger) or substitutes (geographic coverage, travel time make ground alternatives impractical). Faces maximum extraction through higher fares and reduced service quality. No alternative mobility.
constraint_indexing:constraint_classification(mexican_airline_merger, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPETING REGIONAL CARRIER (TANGLED ROPE) — Constrained by regulatory environment and capacity limitations. Benefits from exemption if able to access consolidated entity's assets or form strategic alliances; bears extraction through reduced market share and pricing pressure. Mixed coordination (alliance possibilities) and extraction (market dominance). Exit is costly but not impossible — shift to international markets or specialized routes.
constraint_indexing:constraint_classification(mexican_airline_merger, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEXICAN GOVERNMENT / AVIATION AUTHORITY (ROPE) — Primary beneficiary with high arbitrage options. Frames merger as coordination solution for national carrier fragmentation and international competitiveness. Extracts political capital through consolidation narrative and tax revenue optimization. Experiences constraint as coordination mechanism enabling national champion strategy. Can arbitrage by suspending exemption or modifying terms.
constraint_indexing:constraint_classification(mexican_airline_merger, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MEXICANA DE AVIACION / CONSOLIDATED CARRIER (ROPE) — Primary direct beneficiary. Experiences exemption as enabling coordination (route network consolidation, fleet optimization, international alliance building). High arbitrage options through contract renegotiation, regulatory exit, or service expansion. Extraction flow runs toward this agent. Net beneficiary from reduced competitive pressure and margin expansion.
constraint_indexing:constraint_classification(mexican_airline_merger, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: USMCA TRADE FRAMEWORK / INTERNATIONAL REGULATORS (TANGLED ROPE) — Organized institutional observers facing conflicting mandates: maintain competitive market principles (USMCA Chapter 21 competition provisions) while accommodating national government priority on carrier consolidation. Constrained by sovereignty doctrine; limited exit through legal challenge or trade retaliation. Benefits from stability in North American air market coordination; bears extraction risk through reduced consumer welfare and antitrust precedent spillover. Mixed: policy coordination function + asymmetric harm.
constraint_indexing:constraint_classification(mexican_airline_merger, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: MEXICAN ANTITRUST AUTHORITY (COFECE) (PITON) — Institutional actor with nominal enforcement mandate but constrained capacity and political pressure. Theater ratio high: formal review processes occur but outcomes predetermined by government policy priority. Original function (competitive market protection) has atrophied; current role is performative legitimation of merger exemption. Maintains institutional structure and review protocols despite reduced real enforcement. Constrained exit — cannot openly defy government policy without institutional reprisal.
constraint_indexing:constraint_classification(mexican_airline_merger, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GLOBAL COMPETITION PRINCIPLE (MOUNTAIN) — From universal competition law perspective, antitrust exemptions for carrier consolidation reflect an immutable tension: concentrated markets reduce competitive pressure, and this effect is structurally inherent regardless of policy framework. However, this naturalizes what is actually a contingent policy choice. The framework reveals this as a false summit: exemptions are not natural laws but institutional artifacts dependent on government enforcement discretion.
constraint_indexing:constraint_classification(mexican_airline_merger, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mexican_airline_merger_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mexican_airline_merger, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mexican_airline_merger, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mexican_airline_merger, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mexican_airline_merger, TR),
    TR >= 0.70.

:- end_tests(mexican_airline_merger_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The consolidated carrier captures increased margins through reduced competition and can extract through higher fares and reduced service frequency on non-competitive routes. However, extractiveness is not extreme (snare level) because: (1) International carriers provide some external competitive pressure, particularly on leisure and international hub routes; (2) Mexican passengers have partial substitutes (ground transportation, travel avoidance) for non-essential trips; (3) Geographic coverage expansion may benefit some passenger segments. The measurement trajectory (0.35→0.52 over 6 years) reflects gradual consolidation of market power post-merger as the carrier realizes pricing power and service reduction opportunities. Suppression (0.58): Moderate-high. Significant structural barriers include limited domestic airline entry capacity (high capital, airport slot scarcity in Mexico City), regulatory licensing delays, and incumbent advantage post-consolidation. Passengers have no meaningful exit options within domestic market. However, suppression is not extreme because international carriers and ground transportation provide partial alternatives for some journey types. Theater ratio (0.64): High. Regulatory review processes by COFECE appear substantive (formal competitive impact analysis, public comments, conditions attached) but occur within predetermined policy parameters set by the Mexican government's national champion strategy. The exemption was effectively decided at the policy level; COFECE's review was legitimation rather than genuine contestation. Theater ratio increases over the measurement interval (0.45→0.64) as the performance monitoring and condition enforcement become increasingly perfunctory.
 *
 * PERSPECTIVAL GAP:
 *   The passenger (powerless/trapped) sees Snare: high extraction, no exit. The government (institutional/arbitrage) sees Rope: coordination benefits, high exit optionality. The carrier (powerful/arbitrage) sees Rope: benefits from merger coordination, arbitrage options for renegotiation. COFECE (institutional/constrained) sees Tangled Rope: coordination function (regulatory legitimacy) mixed with extraction (compromised mandate). International observers (organized/constrained) see Tangled Rope: policy coordination (national sovereignty) mixed with trade rule asymmetry (competition principles overridden). The piton perspective reveals that regulatory review processes maintain institutional theater despite reduced functional enforcement. The mountain perspective risks naturalizing this as inherent to airline markets—but the structural data shows it's contingent on policy choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) is derived from their structural position relative to extraction flow. Mexicana de Aviación as primary beneficiary with arbitrage options (can exit by renegotiating terms, expanding internationally, or shifting focus) derives d≈0.15 (full beneficiary), producing low/negative effective extraction chi—they experience the constraint as enabling, not constraining. Mexican government as policy initiator with institutional power derives d≈0.10 (institutional beneficiary), experiencing the constraint as a coordination tool. Passengers derive d≈0.92 (trapped victims with no exit), producing maximum experienced extraction chi through the sigmoid function—trapped exit + victim status = high f(d). COFECE derives d≈0.55 (symmetric institutional constraint) despite nominal regulatory authority, because constrained exit (political pressure) and mixed victim/beneficiary status (must legitimize policy while maintaining nominal competitive mandate) produce symmetric cost-benefit. The derivation reveals why COFECE classifies as Tangled Rope from an international perspective: it benefits from stability (coordination) but bears extraction through compromised mandate (asymmetric harm).
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL DISTINCTION: This constraint resolves mandatrophy by separating the beneficiary's genuine coordination function (network consolidation, international alliance capability) from the victim's genuine extraction experience (captive market, higher fares). The constraint is truthfully Tangled Rope because BOTH are structurally real: (1) The coordination problem is genuine—fragmented domestic Mexican carriers cannot compete internationally or serve remote routes efficiently; (2) The asymmetric extraction is genuine—passengers on concentrated routes face reduced competition and higher prices. The mandatrophy is avoided by refusing to collapse this to 'really a Rope' (beneficiary framing) or 'really a Snare' (victim framing). Both are partially true. The policy choice is whether the coordination benefits justify the extraction costs—a value question, not a classification question. The analytical perspective's false summit (naturalizing this as inherent) is unmasked by the structural data: the exemption is contingent, not inevitable. If the Mexican government prioritized consumer welfare over international competitiveness, this could be regulated to Rope (coordination without extraction) through price controls, service obligations, and entry barriers reduction. The fact that it remains Tangled Rope reveals a policy choice, not a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    merger_efficiency_gains_realization,
    'Do claimed merger efficiencies (network consolidation, international alliance capability, cost reduction) materialize as consumer benefits or are they captured entirely as producer surplus?',
    'Comparative analysis of fares, service frequency, and consumer complaint rates pre- vs post-merger; international route expansion data; fuel cost pass-through analysis',
    'If consumer benefits emerge: constraint reclassifies toward Rope (coordination function realized). If entirely captured: constraint remains Tangled Rope or shifts toward Snare (pure extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(merger_efficiency_gains_realization, empirical, 'Whether merger efficiency gains benefit consumers or are captured as producer surplus').

omega_variable(
    international_competitive_pressure_sufficiency,
    'Do international carriers and cross-border competition from U.S./Latin American airlines provide sufficient external constraint on consolidated Mexican carrier pricing and service quality?',
    'Cross-border passenger diversion analysis; international carrier pricing relative to Mexican domestic routes; market share trends for international vs domestic routes; elasticity of demand for international substitutes',
    'If international pressure is sufficient: effective extraction is lower than base assessment; constraint reclassifies toward weaker type. If insufficient: Mexican passengers face true captive market; extraction intensifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_competitive_pressure_sufficiency, empirical, 'Whether international competition constrains consolidated carrier pricing').

omega_variable(
    regulatory_oversight_credibility,
    'Is the antitrust exemption truly time-limited with performance conditions, or is it effectively permanent with performative review processes?',
    'Review of exemption terms, performance metrics, and compliance monitoring; historical record of other Mexican antitrust exemptions (revocation rate, condition enforcement); COFECE institutional independence assessment',
    'If credibly time-limited with enforcement: constraint is Scaffold with real sunset. If permanent/performative: constraint is Piton (theatrical review masking institutional inertia) or escalates toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_oversight_credibility, empirical, 'Whether antitrust exemption is genuinely time-limited or effectively permanent').

omega_variable(
    domestic_carrier_entry_barriers,
    'What are the structural barriers (capital, slots, regulatory licensing) preventing new domestic carrier entry or expansion by existing carriers post-merger?',
    'Capital requirement analysis for domestic airline entry; Mexico City airport slot allocation data; licensing approval timelines and criteria; competitor investment decisions',
    'If barriers are low: market contestability constrains consolidated carrier; extraction limited. If barriers are high: consolidated carrier has durable monopoly; extraction can intensify.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domestic_carrier_entry_barriers, empirical, 'Structural barriers to domestic airline entry or expansion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mexican_airline_merger, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mxam_tr_t0, mexican_airline_merger, theater_ratio, 0, 0.45).
narrative_ontology:measurement(mxam_tr_t3, mexican_airline_merger, theater_ratio, 3, 0.55).
narrative_ontology:measurement(mxam_tr_t6, mexican_airline_merger, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(mxam_be_t0, mexican_airline_merger, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mxam_be_t3, mexican_airline_merger, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(mxam_be_t6, mexican_airline_merger, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mexican_airline_merger, resource_allocation).
narrative_ontology:affects_constraint(mexican_airline_merger, mexican_airport_slot_allocation).
narrative_ontology:affects_constraint(mexican_airline_merger, nafta_competition_framework).
narrative_ontology:affects_constraint(mexican_airline_merger, regional_airline_consolidation_pressure).

% DUAL FORMULATION NOTE:
% This constraint is a specific instantiation of the broader tension between national economic policy (industrial consolidation) and regional trade frameworks (competition principles). Upstream: Mexican economic policy drivers and NAFTA/USMCA conflict. Downstream: impacts on regional airline competition and passenger welfare. Network links represent institutional coupling—changes in one constraint cascade through the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mexican_airline_merger, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
