% ============================================================================
% CONSTRAINT STORY: fiscal_equalization_friction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fiscal_equalization_friction, []).

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
 *   constraint_id: fiscal_equalization_friction
 *   human_readable: The Equalization Conflict (Net Transfer Friction)
 *   domain: economic/political
 *
 * SUMMARY:
 *   Canada's equalization program represents a federal coordination mechanism
 *   designed to ensure provincial governments can deliver comparable public
 *   services despite unequal resource bases. Since 1957, the program has
 *   transferred fiscal capacity from resource-rich and high-income provinces
 *   (Alberta, Ontario, British Columbia) to lower-capacity regions (Quebec,
 *   Atlantic provinces, Manitoba). The constraint exhibits a core tension:
 *   equalization provides genuine coordination benefit (prevents regional
 *   service collapse, maintains national political cohesion) while
 *   simultaneously extracting fiscal autonomy from contributor provinces and
 *   creating fiscal dependency among recipients. This structural duality
 *   explains why the constraint classifies as tangled_rope from most
 *   perspectives and generates persistent political friction. The
 *   theater_ratio (0.58) reflects that increasingly, the technical
 *   equalization formula (five-province standard, fiscal capacity
 *   calculations) justifies political agreements negotiated outside the
 *   formula — notably, Quebec's special status and Atlantic Accord
 *   exemptions. The constraint's extractiveness has risen from 0.22 (1975) to
 *   0.38 (2024) as contributor province grievance has intensified and the
 *   formula has become more opaque.
 *
 * KEY AGENTS:
 *   - Net Contributor Provinces (Alberta, Ontario, British Columbia): Institutional/constrained — locked into transfer obligations via constitutional duty; cannot exit Confederation without massive political cost; suppress grievance through formal federalism channels
 *   - Recipient Provinces (Quebec, Manitoba, Atlantic provinces): Moderate/mobile — benefit from transfers that offset fiscal disadvantage; also constrained by equalization dependency; have partial exit options (resource development, devolution of revenue sources)
 *   - Federal Government (Department of Finance, Parliament): Institutional/arbitrage — operates equalization mechanism; benefits from role as national arbiter; has exit via formula adjustment; experiences coordination problem of balancing contributor grievance against recipient need
 *   - Equalization Formula Ritual (five-province standard, fiscal capacity calculation): Institutional/arbitrage — technical apparatus has become performative; real negotiation happens outside formula; persists through institutional inertia
 *   - Reform Movement (Provincial fiscal federalism critics, think tanks, separatist movements): Organized/mobile — organized agents proposing alternatives (asymmetric federalism, decentralized tax room, resource-sharing); see sunset path via structural reform
 *   - Analytical Observer: Civilizational/analytical — risks naturalizing fiscal disparity as immutable, missing that equalization mechanism design (not disparity itself) is contingent and contested
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fiscal_equalization_friction, 0.38).
domain_priors:suppression_score(fiscal_equalization_friction, 0.48).
domain_priors:theater_ratio(fiscal_equalization_friction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fiscal_equalization_friction, extractiveness, 0.38).
narrative_ontology:constraint_metric(fiscal_equalization_friction, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(fiscal_equalization_friction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fiscal_equalization_friction, tangled_rope).
narrative_ontology:human_readable(fiscal_equalization_friction, "The Equalization Conflict (Net Transfer Friction)").
narrative_ontology:topic_domain(fiscal_equalization_friction, "economic/political").

domain_priors:requires_active_enforcement(fiscal_equalization_friction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fiscal_equalization_friction, equalization_recipient_provinces).
narrative_ontology:constraint_beneficiary(fiscal_equalization_friction, federal_redistributive_mechanism).
narrative_ontology:constraint_victim(fiscal_equalization_friction, net_contributing_provinces).
narrative_ontology:constraint_victim(fiscal_equalization_friction, provincial_fiscal_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NET CONTRIBUTOR PROVINCE (SNARE) — Alberta, Ontario, British Columbia locked into transfer obligations via federal formula. Cannot exit without leaving Confederation. High suppression: constitutional duty to contribute; political penalty for withholding. d≈0.78, f(d)≈1.12, σ=1.0 → χ≈0.43. The constraint extracts fiscal capacity regardless of provincial preference.
constraint_indexing:constraint_classification(fiscal_equalization_friction, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: FEDERAL GOVERNMENT (ROPE) — Operates the equalization mechanism as coordinating institution. Experiences constraint as coordination problem: calibrating transfers to maintain national cohesion without triggering contributor rebellion. d≈0.25, f(d)≈0.15, σ=1.0 → χ≈0.06. Federal government has arbitrage exit (can adjust formula) and benefits from role as redistributive arbiter. Low effective extraction because the federal actor designed the system.
constraint_indexing:constraint_classification(fiscal_equalization_friction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: RECIPIENT PROVINCE (TANGLED ROPE) — Quebec, Manitoba, Atlantic provinces benefit from transfers that partially offset revenue disadvantage (lower resource bases, aging populations). But also constrained: equalization creates fiscal dependency and reduces incentive for own revenue generation. d≈0.45, f(d)≈0.45, σ=1.0 → χ≈0.17. Mixed: coordination benefit (ensures provincial services in lower-capacity regions) + asymmetric extraction (creates dependency, entangles provincial policy autonomy with federal approval).
constraint_indexing:constraint_classification(fiscal_equalization_friction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: EQUALIZATION FORMULA RITUAL (PITON) — The technical formula (five-province standard, fiscal capacity calculations) has become increasingly performative. Real political negotiation happens outside the formula; the formula justifies post-hoc political deals. Theater ratio = 0.58 reflects that half the equalization value derives from formula application; half from political adjustment and special-status provinces (Quebec). The ritual persists because no alternative exists, not because the formula accurately captures fiscal capacity. d≈0.30, f(d)≈0.28, σ=1.0 → χ≈0.11.
constraint_indexing:constraint_classification(fiscal_equalization_friction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM MOVEMENT / EQUALIZATION SKEPTICS (SCAFFOLD) — Organized agents (provincial governments, policy think tanks, fiscal federalism reformers) see equalization as temporary coordination failure solvable via alternative mechanisms: asymmetric federalism, decentralized tax room, resource-revenue sharing. This perspective frames the constraint as having a sunset: if provinces negotiate genuine fiscal autonomy and resource-sharing reforms, the transfer dependency structure dissolves. d≈0.35, f(d)≈0.34, σ=1.0 → χ≈0.13. Low effective extraction because organized agents have agency and a proposed exit path.
constraint_indexing:constraint_classification(fiscal_equalization_friction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, fiscal disparity between regions is inherent to geography and resource distribution. Some form of redistribution is mathematically inevitable in a federation; equalization appears as natural law — the structural necessity of maintaining equal access to public services across unequal resource bases. However, the structural data (ε=0.38, suppression=0.48) contradicts a mountain classification. The engine will identify this as a false summit: what appears natural (regional fiscal disparity) is actually a contingent political choice (how to structure the transfer mechanism, which formulas to use, how much autonomy to grant provinces). d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.44.
constraint_indexing:constraint_classification(fiscal_equalization_friction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fiscal_equalization_friction_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(fiscal_equalization_friction, TR),
    TR >= 0.70.

:- end_tests(fiscal_equalization_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts fiscal capacity from contributor provinces (they contribute ≈1.8% of GDP to equalization transfers annually) without meaningful consent or negotiation power. But extraction is not severe (ε not > 0.46) because: (1) contributor provinces retain substantial fiscal autonomy and resource control; (2) equalization provides measurable coordination benefit (prevents regional fiscal collapse); (3) the constraint operates within constitutional framework, not through coercion. The rising trajectory (0.22 → 0.38) reflects that contributor province grievance is escalating — perceived extraction is increasing even if nominal transfers are stable. Suppression (0.48): Moderate-high. Barriers to exit include constitutional entrenchment of the equalization principle, political penalty for withholding transfers (risks Confederation), and lack of alternative coordination mechanisms. But suppression is not total (≥0.60 for snare) because some provinces have partial exit options (resource development for recipients, asymmetric federalism proposals, negotiated exemptions). Theater ratio (0.58): Moderate. The five-province standard formula appears technical and neutral but increasingly justifies politically negotiated outcomes. Special-status exemptions (Quebec, Atlantic Accord) and periodic formula changes reveal the formula's performative character — it legitimates post-hoc political deals rather than determining them. Theater has increased (0.35 → 0.58) as formula complexity has grown and political negotiation has become more opaque.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a dramatic perspectival gap between contributor and recipient perspectives, mediated by federal and reform perspectives. Net contributor provinces (Alberta, Ontario, BC) classify the constraint as snare: they are locked in, suppressed, experiencing extraction with minimal exit. Recipient provinces classify it as tangled_rope: they benefit from transfers (coordination) but also constrained by dependency (extraction). The federal government sees rope: they are coordinating a legitimate redistribution problem. Reform movements see scaffold: they propose a sunset via structural reform (asymmetric federalism, decentralized tax room). The equalization formula itself appears as degraded ritual (piton): technically sophisticated but increasingly performative. The analytical observer risks seeing mountain (fiscal disparity is natural; equalization is natural response) — but the structural data reveals this is false summit: the magnitude and structure of equalization (not the existence of fiscal disparity) is contingent political choice. The perspectival gap is stable because it reflects genuine structural differences: contributor provinces truly are constrained (constitutional duty); recipient provinces truly benefit (transfers maintain service levels); federal government truly has more flexibility (can adjust formula); reformers truly have agency (can propose alternatives).
 *
 * DIRECTIONALITY LOGIC:
 *   Net contributor provinces: Victim + constrained → d≈0.78, f(d)≈1.12. High effective extraction. They bear substantial transfer obligations without meaningful negotiation power. Exit costs are prohibitive (Confederation is the only alternative). Federal government: Beneficiary + arbitrage → d≈0.25, f(d)≈0.15. Net beneficiary. Federal government designed the system, operates it, and can adjust it. Has multiple exit/adjustment options. Recipient provinces: Mixed (beneficiary and victim) + mobile → d≈0.45, f(d)≈0.45. Moderate extraction. They benefit from transfers but constrained by dependency; have some exit options (resource development, tax room devolution). Reform movement: Organized + mobile → d≈0.35, f(d)≈0.34. Low effective extraction. Organized agents with agency and proposed exit path (structural reform). Equalization formula: Institutional + arbitrage → d≈0.30, f(d)≈0.28. The piton classification emerges from high theater (0.58), not from high directionality. Formula appears to determine allocations but actually justifies political outcomes. Analytical observer: analytical → d≈0.72, f(d)≈1.15. False summit risk: naturalizing contingent institutional choice as immutable law.
 *
 * MANDATROPHY ANALYSIS:
 *   The equalization constraint resolves mandatrophy by disambiguating coordination function from extraction mechanism. The coordination function is real: equalization prevents regional fiscal collapse and maintains national political cohesion. This explains rope and scaffold perspectives. But the extraction mechanism is also real: equalization extracts fiscal autonomy from contributor provinces without proportional negotiation power. This explains snare and piton perspectives. The tangled_rope classification from the recipient province perspective correctly captures both: equalization is genuine coordination (solving fiscal disparity problem) + asymmetric extraction (creating dependency). The mandatrophy resolution: this is NOT a case of mislabeling pure extraction as coordination. It is a case of a single structural phenomenon serving both functions simultaneously. The false summit (mountain perspective) is the risk: naturalizing fiscal disparity as an immutable law misses that equalization's design, magnitude, and structure are contingent political choices. The engine's false natural law detector should flag this: ε=0.38 and suppression=0.48 are inconsistent with mountain classification (which requires ε≤0.25, suppression≤0.05).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_capacity_measurement,
    'What metric should determine provincial fiscal capacity: current tax revenue, potential tax revenue, per-capita resource endowment, demographic adjustment, or forward-looking expected capacity?',
    'Comparative analysis of capacity measurement systems (five-province standard vs ten-province standard vs non-renewable resource exclusion); correlation between capacity metric and actual provincial service delivery outcomes',
    'If capacity measured by current revenue: favors provinces with developed tax bases and penalizes resource-rich provinces with low extraction. If measured by potential capacity: creates strong incentive for provincial resource development, potentially reducing equalization dependence. Different metrics produce 15-25% variance in transfer allocations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_capacity_measurement, empirical, 'Which fiscal capacity metric defines equalization entitlement').

omega_variable(
    contributor_rebellion_threshold,
    'What transfer-to-GDP ratio triggers a contributor province''s political exit threat (separation, asymmetric federalism, or coercive tax resistance)?',
    'Historical analysis of provincial grievance escalation; polling of contributor province taxpayer willingness-to-pay; correlation between transfer burden and separatist sentiment in Alberta and Ontario',
    'If threshold < 2% of GDP: current transfers (≈1.8% for Alberta) are unsustainable, forcing formula renegotiation or structural reform within 10-15 years. If threshold > 3%: status quo persists, but fiscal capacity of contributor provinces declines (aging, out-migration), raising future threshold conflict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contributor_rebellion_threshold, empirical, 'Contributor province exit threshold').

omega_variable(
    equalization_formula_legitimacy,
    'Is the five-province standard a neutral technical calculation or a political choice favoring recipient provinces? Can any formula be perceived as legitimate by all provinces simultaneously?',
    'Comparative analysis of formula design space: simulations of alternative capacity metrics and their distributional consequences; interviews with provincial finance ministers about formula acceptance thresholds',
    'If formula is technically neutral: current friction is manageable through adjustment. If formula is structurally political: equalization is inherently conflictual regardless of metric choice — the constraint is more tangled_rope/snare than rope. This would suggest structural reform (asymmetric federalism, conditional autonomy) is necessary, not just formula tweaking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equalization_formula_legitimacy, conceptual, 'Whether equalization formula can achieve political legitimacy').

omega_variable(
    resource_boom_extraction_asymmetry,
    'When a recipient province develops natural resources and becomes a net contributor, does the equalization mechanism create incentive structures that penalize resource development (perverse incentive) or align provincial and national interests?',
    'Analysis of Newfoundland and Labrador''s offshore oil development trajectory post-Atlantic Accord; modeling of equalization clawback effects on provincial investment decisions; comparative study of provinces under equalization vs. autonomous resource taxation',
    'If perverse: equalization suppresses recipient province growth, locking them in dependency. The constraint is pure snare from recipient perspective. If aligned: equalization provides transitional support while recipient provinces build capacity. The constraint is genuinely mixed coordination-extraction (tangled rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_boom_extraction_asymmetry, empirical, 'Whether equalization creates perverse incentives for resource development').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fiscal_equalization_friction, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fiscal_eq_tr_t0, fiscal_equalization_friction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fiscal_eq_tr_t15, fiscal_equalization_friction, theater_ratio, 15, 0.48).
narrative_ontology:measurement(fiscal_eq_tr_t30, fiscal_equalization_friction, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(fiscal_eq_be_t0, fiscal_equalization_friction, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(fiscal_eq_be_t15, fiscal_equalization_friction, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(fiscal_eq_be_t30, fiscal_equalization_friction, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fiscal_equalization_friction, resource_allocation).
narrative_ontology:affects_constraint(fiscal_equalization_friction, provincial_tax_capacity).
narrative_ontology:affects_constraint(fiscal_equalization_friction, resource_revenue_volatility).
narrative_ontology:affects_constraint(fiscal_equalization_friction, regional_fiscal_autonomy).

% DUAL FORMULATION NOTE:
% Fiscal equalization is downstream of regional fiscal disparity (which is geographic/demographic) but represents a structurally distinct constraint on federal-provincial coordination. The upstream constraint (regional fiscal disparity) has ε≈0.15 (essentially immutable: geographic resources are fixed). Equalization friction (this story) has ε=0.38 because it is a political choice about how to respond to disparity, not the disparity itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fiscal_equalization_friction, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
