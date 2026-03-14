% ============================================================================
% CONSTRAINT STORY: bail_system_wealth_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bail_system_wealth_extraction, []).

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
 *   constraint_id: bail_system_wealth_extraction
 *   human_readable: Bail System Wealth Extraction and Systemic Debt Trapping
 *   domain: criminal_justice/economic_policy
 *
 * SUMMARY:
 *   The bail system in the United States functions as a wealth extraction
 *   mechanism disguised as pretrial release coordination. Defendants too poor
 *   to post bail face either pretrial detention (destroying employment and
 *   housing) or debt servitude through commercial bail bondsmen charging
 *   non-refundable 10-15% fees. This creates a two-tiered system: wealthy
 *   defendants are released on recognizance or minimal bail; poor defendants
 *   are trapped in pretrial detention or extracted of scarce family
 *   resources. The extractiveness has increased over 20 years as bail amounts
 *   have risen, bondsman fees have become standardized, and risk assessment
 *   instruments have been captured by the industry rather than used to reduce
 *   wealth-dependence. The constraint exhibits the full Snare signature: high
 *   base extractiveness (0.68), high suppression (defendants cannot
 *   negotiate, cannot challenge bail system from inside pretrial detention),
 *   mixed theater (constitutional procedures exist but are performative). The
 *   system persists despite documented harm (innocent defendants pleading
 *   guilty to obtain release, family debt cycles, racial disparities) because
 *   it generates revenue for courts, bail bond companies, and related
 *   industries. Bail reform movements have succeeded in specific
 *   jurisdictions (Washington DC, New Jersey, Connecticut) by eliminating
 *   commercial bail and moving to risk-based release-on-recognizance. These
 *   reforms show that the extraction is not functionally necessary — pretrial
 *   release coordination can occur with minimal wealth extraction. The
 *   persistence of high-extraction bail in other states reflects
 *   institutional path-dependency (judge authority, bail bond industry
 *   lobbying, court revenue dependence) rather than structural necessity.
 *
 * KEY AGENTS:
 *   - Poor defendants: Primary victim (powerless/trapped) — face binary choice between pretrial detention or debt servitude through bail bondsmen. Cannot exit or negotiate.
 *   - Family networks of defendants: Secondary victim (moderate/constrained) — bear financial costs of bail or bonds; also benefit from defendant release. Constrained by resources and information barriers.
 *   - Bail bond companies: Primary beneficiary (institutional/arbitrage) — profit from 10-15% non-refundable fees; experience system as coordination with favorable terms.
 *   - Court systems and judges: Secondary beneficiary (institutional/arbitrage) — benefit from bail revenue stream and reduced detention pressure; arbitrage by setting bail terms.
 *   - Criminal justice reform coalition: Organized victim (organized/constrained) — recognize extraction but face entrenched industry resistance and legislative barriers to reform.
 *   - Constitutional fairness apparatus: Institutional performer (institutional/arbitrage) — 8th Amendment excessive bail provisions exist but are largely unenforced; maintain theater of fair adjudication.
 *   - Analytical observer: Structural analyst (analytical/analytical) — views system as contingent extraction mechanism revealed by comparative analysis with peer democracies using low-extraction pretrial release systems.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bail_system_wealth_extraction, 0.68).
domain_priors:suppression_score(bail_system_wealth_extraction, 0.72).
domain_priors:theater_ratio(bail_system_wealth_extraction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bail_system_wealth_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(bail_system_wealth_extraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bail_system_wealth_extraction, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bail_system_wealth_extraction, snare).
narrative_ontology:human_readable(bail_system_wealth_extraction, "Bail System Wealth Extraction and Systemic Debt Trapping").
narrative_ontology:topic_domain(bail_system_wealth_extraction, "criminal_justice/economic_policy").

domain_priors:requires_active_enforcement(bail_system_wealth_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bail_system_wealth_extraction, bail_bond_companies).
narrative_ontology:constraint_beneficiary(bail_system_wealth_extraction, commercial_bail_industry).
narrative_ontology:constraint_beneficiary(bail_system_wealth_extraction, court_systems_fee_dependent).
narrative_ontology:constraint_victim(bail_system_wealth_extraction, poor_defendants).
narrative_ontology:constraint_victim(bail_system_wealth_extraction, pretrial_detainees).
narrative_ontology:constraint_victim(bail_system_wealth_extraction, family_networks_defendants).
narrative_ontology:constraint_victim(bail_system_wealth_extraction, systemic_justice_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POOR DEFENDANT (SNARE) — Arrested, unable to pay bail, faces pretrial detention or debt servitude through bail bonds. No exit: detention destroys employment and housing; bondsman fees (10-15% non-refundable) are unrecoverable even if acquitted. Maximum experienced extraction. Suppression is total — cannot negotiate terms, cannot appeal bond amount without legal resources they lack, cannot challenge the bail system from inside it.
constraint_indexing:constraint_classification(bail_system_wealth_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FAMILY NETWORK (TANGLED ROPE) — Bears actual bail costs or bondsman fees; also benefits from defendant's release (avoids separation, maintains employment, preserves family income). Constrained by resource limits and social isolation if they cannot pay. Extraction asymmetry: poor families subsidize the bail industry while wealthy defendants post cash or use attorneys to negotiate release conditions. Mixed coordination-extraction: the system does coordinate release/detention decisions, but does so through wealth extraction.
constraint_indexing:constraint_classification(bail_system_wealth_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BAIL BOND COMPANIES (ROPE) — Profit from the bail system (10-15% non-refundable fees, skip-tracing revenue, collateral capture). Experience the constraint as pure coordination: assessing flight risk, managing pretrial release logistics, enforcing conditions. Net beneficiary. Arbitrage: can exit by refusing high-risk clients, renegotiating with courts, or transitioning to surety services. Extraction flows toward bondsman companies — they experience the system as coordination with favorable terms.
constraint_indexing:constraint_classification(bail_system_wealth_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COURT SYSTEMS / JUDGES (ROPE) — Benefit from bail revenue stream (fines, fees, court costs tied to pretrial detention length). Experience the constraint as coordination: bail sets pretrial release terms, manages courtroom docket, reduces jail overcrowding (theoretically). Arbitrage: can set bail parameters, use release on own recognizance, implement bail reform. Extraction flows toward courts through collected fees and reduced detention costs. Judges frame bail as risk management; the extraction is secondary to the coordination narrative.
constraint_indexing:constraint_classification(bail_system_wealth_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CRIMINAL JUSTICE REFORM COALITION (SNARE) — Organized agents (public defenders, civil rights orgs, bail reform advocates) recognize the system as extractive snare. Constrained exit: can push for reform legislation but face entrenched bail bond industry lobbying, political resistance, and judicial inertia. Some jurisdictions (DC, New Jersey) have adopted bail reform, but nationwide system persists. Reform coalition experiences snare classification because the extraction mechanism is deeply integrated into court operations and profit incentives — exit requires systemic change, not individual action.
constraint_indexing:constraint_classification(bail_system_wealth_extraction, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL FAIRNESS APPARATUS (PITON) — The 8th Amendment (excessive bail prohibition) and due process clauses exist but are largely performative. Courts default to bail-setting despite constitutional constraints on excessive bail. The constitutional check on bail extraction is maintained through ritual (bail hearings, appeal processes) but has low functional force — extraction continues because constitutional challenge is expensive and time-consuming. Theater ratio reflects that bail procedures appear to adjudicate fairly but systematically favor wealth. The constitutional apparatus is degraded (piton) because it once functioned (historical bail reform movements in 1960s-80s) but now persists through inertia as extraction mechanisms have adapted.
constraint_indexing:constraint_classification(bail_system_wealth_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational view, the bail system is a pure extraction mechanism disguised as risk management coordination. The structural analysis shows: (1) Non-refundable bondsman fees are not risk compensation but wealth transfer. (2) Bail amounts are not calibrated to individual risk but to extractive capacity (poor defendants set high, wealthy set low or released on recognizance). (3) No bail system comparable in wealth extraction exists in peer democracies (Germany, Canada, UK use bail only for flight risk, not as debt trap). The constraint is civilizationally contingent, not structurally necessary. The analytical view reveals the American bail system as a Snare structured as a Rope.
constraint_indexing:constraint_classification(bail_system_wealth_extraction, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bail_system_wealth_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bail_system_wealth_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bail_system_wealth_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bail_system_wealth_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bail_system_wealth_extraction, TR),
    TR >= 0.70.

:- end_tests(bail_system_wealth_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Non-refundable bondsman fees (10-15% of bail amount) are pure wealth transfer from poor defendants/families to bail companies. A defendant arrested with $5,000 bail pays $500-750 to bondsman regardless of trial outcome; if acquitted, this is extraction without coordination return. Bail amounts themselves are wealth-filtered: poor defendants receive higher bail or detention (implicit wealth extraction through incarceration cost); wealthy defendants receive lower bail or ROR. The extractiveness has increased from ~0.45 (1990s, when bail reform movements had more force) to 0.68 (2020s, as bondsman fees standardized and became unavoidable). Suppression (0.72): High. Defendants in pretrial detention cannot negotiate bail, access attorneys to challenge it, or appeal from inside jail. Bail bond agreements are non-negotiable standard forms. Poor defendants lack information about alternatives (ROR, risk assessment). Career consequences of detention (job loss, housing loss) raise the cost of challenging the system to unaffordable levels. Suppression has remained high across the interval, though criminal justice reform movements have reduced it somewhat in reformed jurisdictions. Theater ratio (0.55): Moderate-high. Bail procedures (bail hearings, written decisions, appeal processes) exist and appear to adjudicate release fairly. Constitutional protections (8th Amendment excessive bail) are invoked but rarely enforced. The procedure theater serves to legitimize the extraction ("bail is just risk management") while the extraction mechanism (wealth-filtering of amounts and fees) operates underneath. Theater has increased as procedural formalism has expanded without reducing extraction.
 *
 * PERSPECTIVAL GAP:
 *   Bail bondsmen experience the constraint as Rope: they solve a coordination problem (managing pretrial release, assessing flight risk) and are compensated fairly for their service (10-15% fee). From their perspective, the system coordinates legitimate risk management. Poor defendants experience the constraint as Snare: they face binary choice (detention + job loss or non-refundable debt + family extraction) with no exit. The system extracts wealth without coordination return from their perspective. Courts experience Rope: bail coordinates release decisions, manages docket, reduces jail pressure, generates revenue. Reform advocates experience Snare: the system systematically extracts from poor defendants while benefiting courts and bondsmen. The analytical observer validates the Snare framing by noting that peer democracies (Germany, Canada, UK) achieve better pretrial release outcomes without bail bondsmen or wealth-dependent bail amounts — the American system's extractiveness is institutional choice, not structural necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for poor defendants: d ≈ 0.95 (trapped victim with no exit). Derived from powerless power, trapped exit options, and victim status in base_properties. Bondsmen extract maximum χ from this agent. Directionality for bail companies: d ≈ 0.05 (full beneficiary with arbitrage exit). Derived from institutional power, arbitrage exit options, beneficiary status. These agents experience negative effective extraction (the system subsidizes them). Directionality for reform coalition: d ≈ 0.70 (victim/constrained). Organized power reduces base d from powerless (0.95) to moderate (0.70), but constrained exit (face lobbying/legislative barriers) keeps d high. Courts and judges: d ≈ 0.25 (beneficiary, but not sole beneficiary; arbitrage exit provides flexibility). The perspectival gap between bondsmen (d=0.05, see Rope) and poor defendants (d=0.95, see Snare) is maximal — the same extraction mechanism is coordination for the beneficiary and snare for the victim.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED via comparative institutional analysis. The bail system resolves mandatrophy (the apparent tension between pure extraction and coordination benefit) by decomposing into genuine coordination (pretrial release assessment) that is coupled with extractive wealth-filtering. The coordination function IS real: courts need mechanism to assess flight risk and manage release. But the extraction mechanism (wealth-dependent bail amounts, non-refundable bondsman fees) is NOT necessary for coordination. Peer democracies achieve superior coordination with minimal extraction. The American bail system is therefore a hybrid Snare-masquerading-as-Rope, where the Rope function (coordination) has been deliberately coupled with Snare mechanisms (extraction). The mandatrophy resolves by recognizing that the two are separable: DC and NJ bail reforms show that coordination can continue with extraction removed. The persistence of high extraction in most US states reflects institutional capture (court revenue dependence, bail bond industry lobbying, judge authority vested in bail-setting discretion) rather than coordination necessity. Classification: Snare (extractive extraction mechanism dominates the coordination function in experienced impact on victims).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bail_necessity_vs_extraction,
    'Is the extractive wealth transfer a necessary mechanism for pretrial release coordination, or is it contingent institutional design?',
    'Comparative analysis: jurisdictions with low-extraction bail systems (DC bail reform, NJ reforms) vs high-extraction jurisdictions; measurement of court outcomes (appearance rates, recidivism) controlling for extraction level. Do low-extraction systems fail at release coordination?',
    'If extraction is necessary: classify as Tangled Rope (coordination + extraction as functional hybrid). If contingent: confirm Snare classification and validate bail reform viability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bail_necessity_vs_extraction, empirical, 'Whether wealth extraction is functionally necessary for pretrial release coordination').

omega_variable(
    bondsman_flight_risk_role,
    'Do commercial bail bondsmen actually reduce flight risk more effectively than risk assessment instruments or supervised release?',
    'RCT or natural experiment comparing bondsman-supervised release vs public supervision vs risk-assessed release-on-recognizance; measurement of appearance rates, fugitive recovery costs, and community safety outcomes.',
    'If bondsmen are more effective: supports Rope classification (genuine coordination benefit). If equivalent or worse: extraction exists with no coordination return — pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bondsman_flight_risk_role, empirical, 'Whether bail bondsmen provide superior flight risk management').

omega_variable(
    constitutional_enforcement_degradation,
    'Has the 8th Amendment''s excessive bail prohibition degraded from historical enforcement (1960s-70s bail reform era) to performative ritual?',
    'Historical analysis of bail appellate decisions, success rates of excessive bail challenges, changes in bail amounts over time controlling for crime severity. Measurement of constitutional doctrine vs observed bail-setting practices.',
    'If degradation confirmed: piton classification is accurate (constitutional machinery is theater). If constitutional enforcement remains functional: the piton perspective overstates degradation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_enforcement_degradation, empirical, 'Degree of 8th Amendment excessive bail enforcement degradation').

omega_variable(
    wealth_filtering_intentionality,
    'Is the systematic wealth-filtering effect of bail amounts intentional extraction policy or unintended consequence of risk-adjustment practices?',
    'Analysis of judicial bail-setting decisions; interviews/analysis of judicial written opinions on bail reasoning; comparison of bail amounts across similar cases stratified by defendant wealth/race. Measurement of residual wealth effect controlling for legitimate risk factors.',
    'If intentional: confirms Snare with deliberate extraction. If unintended: classification shifts toward Tangled Rope (coordination with extraction as side effect). Impacts mandatrophy resolution — intentional extraction requires different policy response than algorithmic bias.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wealth_filtering_intentionality, empirical, 'Whether bail wealth-filtering is intentional extraction or unintended outcome').

omega_variable(
    reform_resistance_mechanisms,
    'What specific institutional/political mechanisms prevent bail reform despite documented harm and public support?',
    'Analysis of failed bail reform legislation; mapping of lobbyist/industry influence on legislative process; interviews with judges/prosecutors on resistance to reform; tracking of bail bond company campaign contributions and legislative testimony.',
    'If resistance is structural (entrenched revenue models, institutional path-dependency): supports Snare with high suppression. If resistance is political choice (industry captured state): same classification but clarifies that system is sustaining mechanism for extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reform_resistance_mechanisms, empirical, 'Institutional mechanisms resisting bail system reform').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bail_system_wealth_extraction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bail_tr_t0, bail_system_wealth_extraction, theater_ratio, 0, 0.4).
narrative_ontology:measurement(bail_tr_t10, bail_system_wealth_extraction, theater_ratio, 10, 0.48).
narrative_ontology:measurement(bail_tr_t20, bail_system_wealth_extraction, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(bail_be_t0, bail_system_wealth_extraction, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(bail_be_t10, bail_system_wealth_extraction, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(bail_be_t20, bail_system_wealth_extraction, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bail_system_wealth_extraction, enforcement_mechanism).
narrative_ontology:affects_constraint(bail_system_wealth_extraction, mass_incarceration_systemic).
narrative_ontology:affects_constraint(bail_system_wealth_extraction, racial_disparities_criminal_justice).
narrative_ontology:affects_constraint(bail_system_wealth_extraction, plea_bargaining_coercion).
narrative_ontology:affects_constraint(bail_system_wealth_extraction, pretrial_detention_employment_loss).

% DUAL FORMULATION NOTE:
% The bail system wealth extraction is upstream of broader mass incarceration dynamics. Poor defendants extracted by bail → forced to plead guilty (constrained by pretrial detention cost) → incarcerated → families disrupted → cyclical poverty. The bail system is a causal mechanism in the broader mass incarceration constraint family. The constraint family also includes plea-bargaining coercion (separate story: defendants coerced to accept guilt by bail pressure) and pretrial detention employment loss (separate story: detention mechanism that operates through bail extraction). Decomposition: bail_system_wealth_extraction (ε=0.68, Snare) → plea_bargaining_coercion (ε=0.72, Snare, downstream) → mass_incarceration_systemic (ε=0.55, Tangled Rope, system-level).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bail_system_wealth_extraction, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
