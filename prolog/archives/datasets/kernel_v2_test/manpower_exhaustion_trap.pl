% ============================================================================
% CONSTRAINT STORY: manpower_exhaustion_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manpower_exhaustion_trap, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: manpower_exhaustion_trap
 *   human_readable: Manpower Exhaustion Trap in Sustained High-Intensity Conflict
 *   domain: political_economy/regime_stability/military_conflict
 *
 * SUMMARY:
 *   The manpower exhaustion trap describes a structural incompatibility
 *   between casualty rate (1,470/day, approximately 536,550/year) and
 *   recruitment capacity (60-75% of 409,000 annual target, or
 *   245,400-306,750/year). The arithmetic gap is approximately
 *   230,000-291,000 per year, compounded by a desertion rate of 70,000/year.
 *   This constraint is presented as a natural law — an immutable demographic
 *   limit that any state would face at this casualty rate. However, the
 *   structural data reveals identifiable beneficiaries (regime leadership,
 *   military-industrial complex, security apparatus) and victims
 *   (conscript-age population, regional labor markets, frontline units),
 *   suggesting the constraint is maintained by regime choices rather than
 *   demographic inevitability. The regime refuses full mobilization
 *   (politically unacceptable) and refuses negotiated exit (would expose
 *   leadership to accountability), creating a false choice between 'continue
 *   current extraction' and 'demographic collapse.' The constraint's
 *   theater_ratio (0.68) reflects increasing performative mobilization:
 *   recruitment campaigns, patriotic messaging, and enforcement theater that
 *   cannot close the arithmetic gap. The accessibility_collapse score (0.35)
 *   is low because alternatives (full mobilization, negotiated settlement,
 *   unilateral withdrawal) remain structurally available — the regime simply
 *   refuses them. The resistance score (0.78) is high because the constraint
 *   faces substantial opposition: 70,000 desertions/year, draft evasion,
 *   regional resistance to mobilization, and international pressure for
 *   settlement.
 *
 * KEY AGENTS:
 *   - Conscript-Age Population: Primary victim (powerless/trapped) — faces maximum extraction with no exit; casualty rate experienced as pure loss
 *   - Regional Labor Markets: Structural victim (powerless/trapped) — productive workers removed, economic activity contracts, demographic collapse accelerates
 *   - Frontline Units: Mixed victim-beneficiary (moderate/constrained) — experience both coordination (unit cohesion) and extraction (quality degradation, casualty rates exceed replacement)
 *   - Regime Leadership: Primary beneficiary (institutional/arbitrage) — constraint coordinates regime survival by preventing negotiated settlement that would expose leadership to accountability
 *   - Military-Industrial Complex: Beneficiary (institutional/arbitrage) — sustained conflict guarantees demand for defense production
 *   - Security Apparatus: Mixed beneficiary-victim (institutional/constrained) — benefits from expanded authority but bears extraction through impossible enforcement tasks and legitimacy erosion
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing regime choices as demographic inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manpower_exhaustion_trap, 0.85).
domain_priors:suppression_score(manpower_exhaustion_trap, 0.92).
domain_priors:theater_ratio(manpower_exhaustion_trap, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manpower_exhaustion_trap, extractiveness, 0.85).
narrative_ontology:constraint_metric(manpower_exhaustion_trap, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(manpower_exhaustion_trap, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manpower_exhaustion_trap, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(manpower_exhaustion_trap, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manpower_exhaustion_trap, mountain).
narrative_ontology:human_readable(manpower_exhaustion_trap, "Manpower Exhaustion Trap in Sustained High-Intensity Conflict").
narrative_ontology:topic_domain(manpower_exhaustion_trap, "political_economy/regime_stability/military_conflict").

domain_priors:requires_active_enforcement(manpower_exhaustion_trap).
domain_priors:emerges_naturally(manpower_exhaustion_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manpower_exhaustion_trap, regime_leadership).
narrative_ontology:constraint_beneficiary(manpower_exhaustion_trap, military_industrial_complex).
narrative_ontology:constraint_beneficiary(manpower_exhaustion_trap, security_apparatus).
narrative_ontology:constraint_victim(manpower_exhaustion_trap, conscript_age_population).
narrative_ontology:constraint_victim(manpower_exhaustion_trap, regional_labor_markets).
narrative_ontology:constraint_victim(manpower_exhaustion_trap, frontline_units).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSCRIPT-AGE POPULATION (SNARE) — Faces maximum extraction with no exit. Mobilization enforcement prevents legal exit; desertion carries severe penalties; economic dependency and family ties prevent geographic exit. The casualty rate (1,470/day) is experienced as pure extraction with no coordination benefit. Suppression is near-total: border controls, legal penalties, social stigma, and economic coercion all prevent exit.
constraint_indexing:constraint_classification(manpower_exhaustion_trap, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL LABOR MARKETS (SNARE) — Structural victims of manpower extraction. Cannot exit the constraint (labor supply is geographically fixed). Experience pure extraction: productive workers removed, economic activity contracts, demographic collapse accelerates. No coordination function from this perspective — only loss.
constraint_indexing:constraint_classification(manpower_exhaustion_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: FRONTLINE UNITS (TANGLED ROPE) — Experience both coordination (unit cohesion, tactical effectiveness depends on manpower) and extraction (quality degradation as recruitment scrapes bottom of barrel, casualty rates exceed replacement, units hollowed out). Constrained exit: rotation exists in principle but is increasingly delayed or denied. Mixed experience: the constraint coordinates military function while extracting from unit effectiveness.
constraint_indexing:constraint_classification(manpower_exhaustion_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REGIME LEADERSHIP (ROPE) — Primary beneficiary. The constraint coordinates regime survival: continued mobilization signals resolve, maintains territorial control, prevents negotiated settlement that would expose leadership to accountability. Arbitrage exit: leadership can exit the constraint (end the war) at will, though domestic political costs are high. Experiences low effective extraction — the constraint serves leadership goals.
constraint_indexing:constraint_classification(manpower_exhaustion_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: MILITARY-INDUSTRIAL COMPLEX (ROPE) — Beneficiary. The constraint coordinates industrial mobilization and resource allocation toward defense production. Sustained conflict guarantees demand. Arbitrage exit: can shift production or exit the sector if the constraint ends. Low effective extraction — the constraint enables rather than constrains.
constraint_indexing:constraint_classification(manpower_exhaustion_trap, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: SECURITY APPARATUS (TANGLED ROPE) — Mixed position. Benefits from expanded authority and resources (mobilization enforcement, border control, desertion prevention). But also bears extraction: tasked with impossible enforcement (preventing 70,000 desertions/year), faces legitimacy erosion as coercion intensifies, and risks being scapegoated if the regime falls. Constrained exit: cannot simply stop enforcing without regime collapse, but also cannot exit the regime structure.
constraint_indexing:constraint_classification(manpower_exhaustion_trap, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the manpower exhaustion trap appears as an immutable constraint: any state sustaining casualties at 1,470/day against recruitment capacity of 60-75% of target will exhaust its manpower base within a calculable timeframe. The arithmetic is inexorable — this appears as a law of military demography, independent of regime type or ideology. However, the structural data contradicts this: the constraint is maintained by regime choices (continuing the war, refusing full mobilization, suppressing exit options). The 'natural law' framing naturalizes what is actually a political choice structure.
constraint_indexing:constraint_classification(manpower_exhaustion_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manpower_exhaustion_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(manpower_exhaustion_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(manpower_exhaustion_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(manpower_exhaustion_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(manpower_exhaustion_trap, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(manpower_exhaustion_trap, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(manpower_exhaustion_trap, ExtMetricName, E),
    domain_priors:suppression_score(manpower_exhaustion_trap, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(manpower_exhaustion_trap),
    narrative_ontology:constraint_metric(manpower_exhaustion_trap, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(manpower_exhaustion_trap, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(manpower_exhaustion_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.85): Very high. The casualty rate (1,470/day) combined with recruitment shortfall (230,000-291,000/year gap) and desertion (70,000/year) creates severe extraction from conscript-age population and regional labor markets. The regime captures political benefits (continued territorial control, avoided accountability) while the population bears demographic and economic costs. The extraction has increased over the 24-month interval as the recruitment pool degrades (convicts, debtors, disabled) and suppression intensifies. Suppression (0.92): Near-maximum. Border controls prevent geographic exit; legal penalties prevent desertion; economic coercion (debt forgiveness for enlistment) prevents economic exit; social stigma and family pressure prevent social exit. The suppression requirement has increased sharply over the interval as the regime intensifies enforcement to counter rising desertion rates. Theater ratio (0.68): High. Mobilization campaigns, patriotic messaging, and recruitment drives are increasingly performative — they cannot close the arithmetic gap between casualties and recruitment. The theater has increased as the regime substitutes symbolic mobilization for actual capacity expansion (full mobilization remains politically unacceptable). Accessibility collapse (0.35): Low for a claimed mountain. Alternatives remain structurally available: full mobilization would close the manpower gap; negotiated settlement would end the casualty rate; unilateral withdrawal would preserve remaining manpower. The regime refuses these alternatives for political reasons, not because they are impossible. A genuine natural law would show accessibility collapse near 1.0 (no alternatives exist once the constraint is understood). Resistance (0.78): High. The constraint faces substantial opposition: 70,000 desertions/year, draft evasion networks, regional resistance to mobilization, international pressure for settlement, and internal elite dissent. A genuine natural law would show resistance near 0.0 (no one resists arithmetic).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a critical false-summit pattern: what appears as an immutable natural law (mountain) from the analytical perspective is experienced as pure extraction (snare) by the victims and as coordination (rope) by the beneficiaries. The conscript-age population sees a snare: maximum extraction, no exit, no coordination benefit. Regional labor markets see a snare: demographic collapse with no compensation. Frontline units see tangled rope: some coordination (unit cohesion) but severe extraction (quality degradation, unsustainable casualties). Regime leadership sees rope: the constraint coordinates regime survival. The analytical observer sees mountain: the arithmetic of casualties vs recruitment appears inexorable. But the structural data reveals beneficiaries (regime leadership, MIC, security apparatus) who benefit from maintaining the constraint, and the low accessibility_collapse score reveals that alternatives exist but are refused. The perspectival gap is diagnostic: a genuine natural law would show mountain from all perspectives; a false summit shows mountain from the analytical perspective but snare/tangled_rope from victim perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   The conscript-age population is declared as a victim with trapped exit options, producing maximum directionality (d near 1.0) and maximum effective extraction. Regional labor markets are also victims with trapped exit (labor supply is geographically fixed), producing high d. Frontline units are victims but with constrained rather than trapped exit (rotation exists in principle), producing moderate d. Regime leadership, military-industrial complex, and security apparatus are declared as beneficiaries with varying exit options: leadership and MIC have arbitrage exit (can end the war or shift production), producing low or negative d; security apparatus has constrained exit (cannot exit without regime collapse), producing moderate d but still net-beneficiary. The directionality derivation captures the structural asymmetry: extraction flows from the population toward the regime, not the reverse. The analytical observer's mountain classification is perspectival — the engine's false summit detector will identify this as naturalization of regime choices.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint's mandate (sustaining military operations) has diverged from its function (regime survival). The original coordination function (mobilizing manpower to defend territory) has been hollowed out by extraction (regime refuses full mobilization to avoid political cost, refuses negotiated exit to avoid accountability). The theater_ratio (0.68) reflects this: mobilization campaigns are increasingly performative, substituting symbolic action for actual capacity expansion. The constraint persists not because it serves its stated function (territorial defense) but because it serves the regime's survival function (preventing settlement that would expose leadership). This is mandatrophy: the mandate (defense) is maintained theatrically while the actual function (regime survival through conflict continuation) operates extractively. The rising theater_ratio and rising extractiveness over the 24-month interval show the mandatrophy intensifying: as the arithmetic gap widens, the regime increases performative mobilization and suppression rather than addressing the structural incompatibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_regime_choice,
    'Is the manpower exhaustion trap an immutable demographic constraint (mountain) or a regime-maintained extraction mechanism (snare with false-summit framing)?',
    'Counterfactual analysis: if the regime ended the war or accepted full mobilization, would the constraint persist? Historical comparison: do all states at this casualty rate face the same constraint, or only those with specific regime structures?',
    'If natural law: the constraint is a genuine mountain — no regime could sustain this casualty rate indefinitely. If regime choice: the constraint is a false summit — the ''impossibility'' of full mobilization or negotiated exit is political, not demographic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_regime_choice, conceptual, 'Whether manpower exhaustion is natural law or regime-maintained extraction').

omega_variable(
    full_mobilization_threshold,
    'What is the true political cost threshold for full mobilization, and is it genuinely prohibitive or merely claimed to be?',
    'Comparative analysis of historical mobilizations; polling data on public tolerance for expanded conscription; elite defection risk modeling',
    'If full mobilization is politically feasible: the regime is choosing extraction over coordination, and the constraint is a snare. If genuinely prohibitive: the regime faces a real structural limit, and the constraint has mountain characteristics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(full_mobilization_threshold, empirical, 'Political feasibility of full mobilization').

omega_variable(
    desertion_rate_sustainability,
    'At what desertion rate does the enforcement apparatus itself collapse, and how close is the current 70,000/year rate to that threshold?',
    'Historical analysis of military collapse thresholds; modeling of enforcement capacity vs desertion volume; identification of tipping points in other conflicts',
    'If near threshold: the constraint is approaching a phase transition where suppression fails and the snare opens. If far from threshold: the current extraction level is sustainable for years.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(desertion_rate_sustainability, empirical, 'Sustainability of current desertion rate').

omega_variable(
    quality_degradation_impact,
    'Does recruitment quality degradation (convicts, debtors, disabled) reduce military effectiveness enough to force strategic adaptation, or can the regime sustain current operations indefinitely with degraded manpower?',
    'Battlefield performance metrics correlated with recruitment cohort quality; tactical adaptation analysis; comparison with historical precedents of quality-degraded armies',
    'If quality degradation forces adaptation: the constraint has a built-in sunset (the regime must change strategy or collapse). If sustainable: the extraction can continue until demographic exhaustion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_degradation_impact, empirical, 'Impact of recruitment quality degradation on military effectiveness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manpower_exhaustion_trap, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(manpower_theater_t0, manpower_exhaustion_trap, theater_ratio, 0, 0.35).
narrative_ontology:measurement(manpower_theater_t6, manpower_exhaustion_trap, theater_ratio, 6, 0.45).
narrative_ontology:measurement(manpower_theater_t12, manpower_exhaustion_trap, theater_ratio, 12, 0.55).
narrative_ontology:measurement(manpower_theater_t18, manpower_exhaustion_trap, theater_ratio, 18, 0.62).
narrative_ontology:measurement(manpower_theater_t24, manpower_exhaustion_trap, theater_ratio, 24, 0.68).

% Extraction over time
narrative_ontology:measurement(manpower_extract_t0, manpower_exhaustion_trap, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(manpower_extract_t6, manpower_exhaustion_trap, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(manpower_extract_t12, manpower_exhaustion_trap, base_extractiveness, 12, 0.75).
narrative_ontology:measurement(manpower_extract_t18, manpower_exhaustion_trap, base_extractiveness, 18, 0.82).
narrative_ontology:measurement(manpower_extract_t24, manpower_exhaustion_trap, base_extractiveness, 24, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(manpower_suppress_t0, manpower_exhaustion_trap, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(manpower_suppress_t6, manpower_exhaustion_trap, suppression_requirement, 6, 0.72).
narrative_ontology:measurement(manpower_suppress_t12, manpower_exhaustion_trap, suppression_requirement, 12, 0.82).
narrative_ontology:measurement(manpower_suppress_t18, manpower_exhaustion_trap, suppression_requirement, 18, 0.88).
narrative_ontology:measurement(manpower_suppress_t24, manpower_exhaustion_trap, suppression_requirement, 24, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manpower_exhaustion_trap, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is structurally independent but could be linked to constraints on regime legitimacy, economic mobilization capacity, or international sanctions if those stories are authored.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manpower_exhaustion_trap, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
