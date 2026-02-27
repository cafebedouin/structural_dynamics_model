% ============================================================================
% CONSTRAINT STORY: heglig_oil_field_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_heglig_oil_field_control, []).

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
 *   constraint_id: heglig_oil_field_control
 *   human_readable: Control of Heglig Oil Field as a Strategic War Asset
 *   domain: geopolitical/economic/conflict
 *
 * SUMMARY:
 *   The RSF capture of Heglig, Sudan's largest and most productive oil field,
 *   represents a classic resource capture during state collapse. The
 *   constraint embodies military extraction: the RSF uses control of the
 *   field (1) to fund military operations, (2) to deny resources to competing
 *   factions, and (3) to establish de facto territorial sovereignty. The
 *   civilian population—without exit options, organizational capacity, or
 *   alternative income sources—bears the full cost: fuel scarcity, currency
 *   collapse (lost foreign exchange), and diversion of what limited state
 *   capacity remains toward military operations rather than civilian
 *   services. Suppression is exceptionally high (0.75): the RSF monopolizes
 *   oil infrastructure, controls export corridors, and uses fuel scarcity as
 *   a compliance mechanism. Theater ratio (0.55) reflects the gap between
 *   RSF's occasional claims of state-building legitimacy and the operational
 *   reality of pure resource extraction. The constraint is a
 *   high-extractiveness Snare from most perspectives except the RSF
 *   leadership (Rope/net beneficiary) and global energy markets (Tangled Rope
 *   with both costs and coordination benefits). The international
 *   humanitarian regime sees a Snare but has no enforcement capacity—trapped
 *   in performative statements while suppression persists.
 *
 * KEY AGENTS:
 *   - RSF Leadership: Primary beneficiary (institutional/arbitrage) — captures oil revenue, funds military expansion, establishes geopolitical leverage in factional conflict
 *   - Sudanese Civilian Population: Primary victim (powerless/trapped) — denied fuel access, subjected to currency collapse, forced to bear costs of resource diversion
 *   - Competing Sudanese Factions (SAF, others): Secondary victim (organized/constrained) — lose funding capacity and geopolitical leverage; unable to retake field without major offensive
 *   - Global Oil Markets & Energy Importers: Mixed (powerful/mobile) — experience energy price volatility and reduced supply, but have exit options and derive coordination benefit (price signals)
 *   - Neighboring States (Egypt, Ethiopia, Eritrea): Secondary victim (moderate/constrained) — absorb refugee flows, cross-border spillover, disrupted trade; cannot ignore crisis but have limited capacity
 *   - International Humanitarian/Governance Regime: Organizational victim (analytical/analytical) — sees constraint as violation of norms but lacks enforcement mechanisms; trapped in theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(heglig_oil_field_control, 0.68).
domain_priors:suppression_score(heglig_oil_field_control, 0.75).
domain_priors:theater_ratio(heglig_oil_field_control, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(heglig_oil_field_control, extractiveness, 0.68).
narrative_ontology:constraint_metric(heglig_oil_field_control, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(heglig_oil_field_control, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(heglig_oil_field_control, snare).
narrative_ontology:human_readable(heglig_oil_field_control, "Control of Heglig Oil Field as a Strategic War Asset").
narrative_ontology:topic_domain(heglig_oil_field_control, "geopolitical/economic/conflict").

domain_priors:requires_active_enforcement(heglig_oil_field_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(heglig_oil_field_control, rsf_paramilitary_faction).
narrative_ontology:constraint_victim(heglig_oil_field_control, sudanese_civilian_population).
narrative_ontology:constraint_victim(heglig_oil_field_control, competing_power_factions).
narrative_ontology:constraint_victim(heglig_oil_field_control, regional_energy_security).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUDANESE CIVILIAN POPULATION (SNARE) — Trapped within territory controlled by RSF. Oil revenue diversion funds military operations; civilian access to fuel and foreign exchange collapses. Zero exit options except displacement/refugee status. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.64.
constraint_indexing:constraint_classification(heglig_oil_field_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPETING SUDANESE FACTIONS (SNARE) — Organized military actors unable to retake the field without major offensive (constrained exit). RSF's control of oil wealth + military hardware creates asymmetric extraction — competing factions lose geopolitical leverage and funding capacity. d≈0.70, f(d)≈1.05, σ=1.1 → χ≈0.57.
constraint_indexing:constraint_classification(heglig_oil_field_control, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: GLOBAL OIL MARKETS (TANGLED ROPE) — Sudan's oil production disruption (300k-400k bpd taken offline) affects global energy prices; energy-dependent states bear cost. However, global actors have exit options: alternative suppliers, energy substitution, storage depletion. The constraint provides coordination benefit (price signal forces efficiency investment) alongside extraction (volatility tax on energy importers). d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.38.
constraint_indexing:constraint_classification(heglig_oil_field_control, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: RSF LEADERSHIP (ROPE) — Benefits from control via oil revenue extraction, military funding, geopolitical leverage. Experiences constraint as coordination mechanism: control of resource base enables organization of faction, signaling of commitment to external actors, and negotiating leverage. d≈0.08, f(d)≈-0.10, σ=1.1 → χ≈-0.06. Net beneficiary; negative effective extraction.
constraint_indexing:constraint_classification(heglig_oil_field_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL GOVERNANCE REGIME (SNARE) — From civilizational view, the constraint violates natural resource governance norms (revenue transparency, civilian benefit, environmental protection). The regime cannot enforce; trapped in performative statements. Theater ratio reflects gap between humanitarian law and capacity to prevent military capture of resources. d≈0.85, f(d)≈1.20, σ=1.2 → χ≈0.55.
constraint_indexing:constraint_classification(heglig_oil_field_control, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: NEIGHBORING STATES (SNARE) — Egypt, Ethiopia, Eritrea face refugee flows, cross-border spillover, and disrupted trade. Constrained exit: cannot ignore humanitarian crisis but have limited intervention capacity. Experience extraction via forced burden-sharing and regional destabilization. d≈0.68, f(d)≈1.00, σ=1.1 → χ≈0.50.
constraint_indexing:constraint_classification(heglig_oil_field_control, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(heglig_oil_field_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(heglig_oil_field_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(heglig_oil_field_control, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(heglig_oil_field_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(heglig_oil_field_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. RSF extraction is severe and multi-dimensional: direct revenue capture (oil sales valued at ~$5-8B annually), denial of resources to competitors, military equipment procurement, and forced labor or conscription funded by oil wealth. The extraction has increased from 0.45 to 0.68 over the measurement interval as RSF consolidated control and formalized revenue diversion. This reflects not just initial seizure but systematic institutionalization of extraction. Suppression (0.75): Very high. RSF maintains monopoly control through military force, controls all export infrastructure, restricts civilian fuel access through rationing and price inflation, and prevents alternative supply networks from forming. The only reason suppression is not 0.95+ is that some black-market fuel networks persist at high cost to civilians. Theater ratio (0.55): Moderate. The RSF occasionally makes legitimacy claims (state formation narrative, resource nationalism) but these are not central to the constraint's operation. The primary mechanism is direct coercion and resource denial, not performative claims. The theater ratio reflects periodic international negotiations where RSF signals willingness to 'govern responsibly' while extracting maximum value.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. The RSF leadership sees rope (coordination benefit of resource control). Competing factions see snare (extraction and denial). The civilian population sees snare (pure victimization). Global energy markets see tangled rope (mixed costs and coordination benefit). Neighboring states see snare (forced burden-sharing). The international regime sees snare but with performative layer (theater). This divergence reflects that the same structural mechanism (resource monopoly enforced by military power) distributes costs and benefits entirely asymmetrically: one faction extracts maximally; everyone else bears cost. There is no shared perception that the constraint benefits all parties or solves a collective action problem.
 *
 * DIRECTIONALITY LOGIC:
 *   RSF Leadership: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; negative effective extraction. Sudanese civilians: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction power. Competing factions: Victim + constrained → d≈0.70, f(d)≈1.05. High extraction; cannot exit without military investment. Global energy markets: Mixed + mobile → d≈0.55, f(d)≈0.75. Moderate effective extraction; has exit options (substitution, efficiency gains). Neighboring states: Victim + constrained → d≈0.68, f(d)≈1.00. Moderate-high extraction; cannot ignore neighboring state humanitarian crisis. International regime: Victim + analytical → d≈0.85, f(d)≈1.20. High effective extraction of enforcement legitimacy; trapped by lack of enforcement capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: This constraint does NOT resolve mandatrophy at extractiveness 0.68 < 0.70 threshold. However, if we project forward to consolidation scenarios where extractiveness reaches 0.72+, mandatrophy would require demonstration that (1) the constraint has genuine coordination function (security provision, administrative capacity for civilian goods), or (2) the classification as pure Snare is stable across time. Current evidence supports Snare classification: the RSF provides no civilian coordination benefit; extraction is the sole mechanism. The mandatrophy is prevented by the absence of any legitimate coordination claim. If the RSF were to establish stable civilian administration (schools, healthcare funded by oil revenue), the constraint would shift toward Tangled Rope, and mandatrophy resolution would become necessary. Currently, the constraint is unambiguously extractive, preventing the mandatrophy paradox.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rsf_institutional_durability,
    'Can the RSF consolidate Heglig control into a sustained state-like institution, or will it remain a warlord extraction mechanism subject to military collapse?',
    'Observation of administrative structures, revenue reinvestment in civilian services vs military spending, ability to maintain control against organized international pressure, succession planning and factional coherence',
    'If institutional: constraint evolves toward Tangled Rope (with coordination benefits in security/governance). If extraction-only: remains Snare indefinitely, intensifying civilian cost until military resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rsf_institutional_durability, empirical, 'Sustainability of RSF institutional control over oil field').

omega_variable(
    international_enforcement_threshold,
    'At what level of humanitarian cost or geopolitical pressure does the international community intervene to forcibly restore resource governance?',
    'Analysis of historical precedent (Zimbabwe diamonds, Libya oil, Congo minerals), comparison of civilian casualty thresholds across similar conflicts, measurement of diplomatic pressure intensity',
    'If threshold is low: external intervention could shift constraint to temporary military-managed transition (Scaffold). If threshold is high: RSF control persists, Snare classification holds indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_enforcement_threshold, empirical, 'Threshold for international intervention in resource control').

omega_variable(
    civilian_substitute_energy_access,
    'Can black-market fuel networks and cross-border smuggling provide sufficient civilian energy access to materially reduce suppression, or does RSF monopoly control remain total?',
    'Price tracking of black-market fuel, measurement of informal supply chains, monitoring of electricity access in RSF-controlled regions, comparison with baseline pre-conflict consumption',
    'If substitute networks are robust: suppression may decline from 0.75 to 0.55-0.60, weakening Snare classification. If monopoly holds: suppression remains high, Snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_substitute_energy_access, empirical, 'Effectiveness of black-market fuel networks in mitigating civilian energy suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(heglig_oil_field_control, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(heglig_tr_t0, heglig_oil_field_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(heglig_tr_t12, heglig_oil_field_control, theater_ratio, 12, 0.48).
narrative_ontology:measurement(heglig_tr_t24, heglig_oil_field_control, theater_ratio, 24, 0.55).

% Extraction over time
narrative_ontology:measurement(heglig_be_t0, heglig_oil_field_control, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(heglig_be_t12, heglig_oil_field_control, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(heglig_be_t24, heglig_oil_field_control, base_extractiveness, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(heglig_oil_field_control, resource_allocation).
narrative_ontology:affects_constraint(heglig_oil_field_control, sudanese_currency_collapse).
narrative_ontology:affects_constraint(heglig_oil_field_control, regional_energy_security_east_africa).
narrative_ontology:affects_constraint(heglig_oil_field_control, refugee_flight_horn_of_africa).

% DUAL FORMULATION NOTE:
% Heglig control is a direct cause (upstream) of multiple constraints: currency collapse (oil revenue diversion reduces foreign exchange), regional energy insecurity (Sudan's production offline), and refugee dynamics (economic collapse drives migration). Each downstream constraint has its own ε values reflecting the second-order effects. The upstream constraint (Heglig control, ε=0.68, Snare) provides the extraction mechanism that cascades into these effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(heglig_oil_field_control, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
