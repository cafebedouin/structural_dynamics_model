% ============================================================================
% CONSTRAINT STORY: pla_loyalty_purge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pla_loyalty_purge, []).

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
 *   constraint_id: pla_loyalty_purge
 *   human_readable: PLA Loyalty Purge Mechanism
 *   domain: political/military_control
 *
 * SUMMARY:
 *   The PLA loyalty purge mechanism under Xi Jinping represents a structural
 *   constraint combining factional consolidation, institutional reform
 *   rhetoric, and systematic elimination of rival power centers. Beginning
 *   with the removal of Guo Boxiong (2013) and Xu Caihou (2014), the purges
 *   accelerated through the 2015-2020 period, reaching peaks during military
 *   reform initiatives (2015-2016) and intensifying again during the Xi
 *   Jinping consolidation (2017-2025). The constraint operates by creating
 *   career-threatening uncertainty for officers whose factional alignment,
 *   past associations, or perceived disloyalty becomes grounds for
 *   investigation, removal, or criminal prosecution. This mechanism extracts
 *   loyalty compliance, consolidates Xi's factional control, eliminates
 *   institutional counterweights, and destabilizes the military institution
 *   itself. The constraint exhibits snare characteristics: high suppression
 *   (no legitimate exit; investigation criteria opaque), high extractiveness
 *   (officers forced to signal loyalty; institutional autonomy eroded), and
 *   meaningful theater (loyalty investigations rely on denunciations and
 *   retrospective reinterpretation). The theater ratio (0.65) reflects that
 *   investigations function as much as political instruments as institutional
 *   accountability mechanisms.
 *
 * KEY AGENTS:
 *   - Xi Jinping Leadership Faction: Primary beneficiary (institutional/arbitrage) — consolidates control, eliminates rivals, establishes hierarchy of loyalty through purge mechanism
 *   - PLA Officer Corps: Primary victim (powerless/trapped) — subject to loyalty investigations without transparent criteria; exit is career death or defection
 *   - Military Institutional Stability: Secondary victim (moderate/constrained) — loses institutional autonomy, experiences talent flight, faces erosion of meritocratic promotion
 *   - Mid-Ranking Command Structure: Mixed beneficiary-victim (organized/constrained) — benefits from promotion opportunities created by purges while bearing career uncertainty
 *   - Central Commission for Discipline Inspection (CCDI): Enforcement apparatus (institutional/constrained) — maintains investigation and purge machinery; constrained by party loyalty
 *   - Analytical Observer: Global/civilizational perspective (analytical/analytical) — sees pure power consolidation mechanism with institutional destabilization costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pla_loyalty_purge, 0.68).
domain_priors:suppression_score(pla_loyalty_purge, 0.78).
domain_priors:theater_ratio(pla_loyalty_purge, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pla_loyalty_purge, extractiveness, 0.68).
narrative_ontology:constraint_metric(pla_loyalty_purge, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(pla_loyalty_purge, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pla_loyalty_purge, snare).
narrative_ontology:human_readable(pla_loyalty_purge, "PLA Loyalty Purge Mechanism").
narrative_ontology:topic_domain(pla_loyalty_purge, "political/military_control").

domain_priors:requires_active_enforcement(pla_loyalty_purge).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pla_loyalty_purge, xi_leadership_faction).
narrative_ontology:constraint_victim(pla_loyalty_purge, pla_officer_corps).
narrative_ontology:constraint_victim(pla_loyalty_purge, military_institutional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLA OFFICER FACING PURGE RISK (SNARE) — Career military officers cannot exit the PLA without severe personal/career consequences. Subject to loyalty investigations without transparent criteria. Exit options are trapped (defection is political/physical exile, demotion ends military identity). d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.94.
constraint_indexing:constraint_classification(pla_loyalty_purge, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MILITARY INSTITUTIONAL STABILITY (SNARE) — Cannot exit the purge dynamics; bears structural cost of talent flight, reduced institutional autonomy, and erosion of meritocratic promotion. Constrained exit (structural reform is politically impossible). d≈0.85, f(d)≈1.18, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(pla_loyalty_purge, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MID-RANKING COMMAND STRUCTURE (TANGLED ROPE) — Experiences mixed extraction and coordination. Purges create promotion opportunities while simultaneously introducing career uncertainty. Constrained exit (cannot leave military). Benefits from loyalty signaling mechanism (weaker competitors purged) while bearing costs of institutional friction. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.51.
constraint_indexing:constraint_classification(pla_loyalty_purge, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: XI LEADERSHIP FACTION / CENTRAL COMMISSION (ROPE) — Primary beneficiary. Purge mechanism consolidates control, eliminates factional rivals, establishes hierarchy of loyalty. Experiences constraint as coordination: signaling loyalty through purge participation, establishing control mechanisms, and managing factional dynamics. Arbitrage exit (can restructure purge criteria at will). d≈0.08, f(d)≈-0.09, σ=1.0 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(pla_loyalty_purge, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PARTY-MILITARY INSTITUTIONAL APPARATUS (PITON) — Theater ratio 0.65 indicates significant performative content: loyalty investigations rely on denunciations, factional reports, and retrospective reinterpretation of past behavior. The mechanism persists partly through institutional inertia (alternative control systems are more costly politically). Constrained exit (cannot abandon security apparatus). d≈0.40, f(d)≈0.40, σ=1.0 → χ≈0.26.
constraint_indexing:constraint_classification(pla_loyalty_purge, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational/global perspective, the purge mechanism is pure extraction: it consolidates power, eliminates institutional autonomy, and extracts loyalty compliance through fear. No coordination benefit visible to the officers being purged or the institutions destabilized. The mechanism relies entirely on suppression and exit barriers. d≈0.88, f(d)≈1.28, σ=1.2 → χ≈1.10.
constraint_indexing:constraint_classification(pla_loyalty_purge, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pla_loyalty_purge_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pla_loyalty_purge, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pla_loyalty_purge, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pla_loyalty_purge, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pla_loyalty_purge, TR),
    TR >= 0.70.

:- end_tests(pla_loyalty_purge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The purge mechanism extracts significant loyalty compliance from PLA officers who must signal alignment with Xi while managing career uncertainty. The mechanism also extracts institutional autonomy — the military's capacity for independent assessment of operational readiness is constrained by loyalty politics. Extractiveness has increased from 0.48 to 0.68 over the interval as investigation criteria have become more expansive and retroactive. Suppression (0.78): High. Officers cannot exit without severe consequences (defection, demotion, criminal prosecution). Investigation criteria are opaque, often based on denunciations and retrospective interpretation of past behavior. Factional affiliation, regional origins, and past associations become grounds for investigation. The suppression reflects both the absence of formal exit mechanisms and the prevalence of informal political risk. Theater ratio (0.65): Moderate-high. Loyalty investigations function partly as accountability mechanisms (anti-corruption rhetoric) and partly as political instruments (eliminating rivals, consolidating factions). The theater has increased as investigation procedures have become more elaborate while remaining non-transparent. The ratio reflects that performative loyalty signaling (public declarations, participation in purge proceedings, factional affiliation signals) constitutes a significant portion of actual investigation outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The primary beneficiary (Xi faction) sees the purge as a coordination mechanism for establishing control and eliminating institutional rivals — a rational governance tool. The powerless victim (career officers) sees pure extraction: career threat, investigation opacity, and inability to exit. The institutional actor (military apparatus) sees a mixed picture: some aspects are performative purge theater (maintaining appearance of Party discipline), some aspects are genuine factional consolidation, and some aspects impose real institutional costs. The analytical observer sees that the purge mechanism serves primarily factional consolidation (extraction) with secondary institutional reform rhetoric (theater). The perspectival gap reveals that 'loyalty' is experienced very differently: for the beneficiary, loyalty is a coordinating signal; for the victim, loyalty is an extractive demand; for the institutional apparatus, loyalty is a performative display; for the observer, loyalty is a power consolidation mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Xi leadership faction: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary; can restructure purge criteria and investigation procedures at will. PLA officer corps: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction pressure; no legitimate exit mechanism. Military institutional stability: Victim + constrained → d≈0.85, f(d)≈1.18. High extraction; institutional reform is politically impossible. Mid-ranking command: Organized + constrained → d≈0.55, f(d)≈0.75. Mixed: benefits from rival removal but bears career uncertainty. CCDI/institutional apparatus: Constrained + institutional → d≈0.40, f(d)≈0.40. Maintains purge machinery but cannot independently set criteria. Analytical observer: analytical → d≈0.88, f(d)≈1.28. Sees extraction mechanism with institutional destabilization as outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint is classified as SNARE from the primary perspectives (powerless officer, institutional stability, analytical observer) with high confidence (ε=0.68, χ≈0.94 for trapped victim; χ≈1.10 from analytical view). The tangled_rope classification (mid-ranking command) is secondary — representing mixed extraction/coordination experienced by beneficiaries of rival removal while bearing career risk. The rope classification (Xi faction) reflects their perspective that the purge is a coordination mechanism, but this perspective is not analytically primary — the mechanism primarily functions as extraction from officers and institutional destabilization. The piton classification (institutional apparatus) reflects performative elements (theater=0.65), but the piton is not the primary type because extractiveness remains high (0.68 > 0.25) and suppression is severe (0.78 >> 0.05). The mandatrophy is resolved by recognizing that the purge mechanism is legitimately a snare (extraction primary, coordination secondary, performed through theater) rather than a coordination problem misnamed as extraction. The theater (0.65) reflects the gap between purge rhetoric (Party discipline, institutional reform) and actual function (factional consolidation, power extraction), but theater does not degrade the constraint to piton because the extraction mechanism remains active and functional — it is not degraded, it is performing its intended consolidation function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    loyalty_investigation_criteria_transparency,
    'Are loyalty investigation criteria based on explicit policy or retroactively constructed to justify predetermined removal decisions?',
    'Analysis of investigation procedures against written Party guidelines; comparison of stated charges to actual behaviors; longitudinal tracking of criteria changes',
    'If explicit and consistent: purge mechanism is enforceable governance tool (extraction ratio may decrease). If retroactive: purge is pure factional weapon (extraction maximized, institutional predictability collapses).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(loyalty_investigation_criteria_transparency, empirical, 'Whether loyalty criteria are transparent or constructed retroactively').

omega_variable(
    factional_motivation_vs_institutional_reform,
    'To what extent do purges serve Xi faction consolidation versus genuine anti-corruption or institutional modernization objectives?',
    'Pattern analysis of removals (correlation with factional affiliation vs objective performance metrics); post-purge institutional outcomes; comparison with non-purged peer militaries'' modernization rates',
    'If primarily factional: pure snare classification confirmed. If significant reform component: tangled rope classification gains credibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(factional_motivation_vs_institutional_reform, conceptual, 'Whether purges serve factional consolidation or institutional reform').

omega_variable(
    institutional_talent_flight_magnitude,
    'How many capable officers leave PLA service or reduce commitment due to purge uncertainty? What is the institutional cost in capability terms?',
    'Tracking of resignation rates, early retirement, and reduced institutional engagement; comparison to pre-purge baselines; capability assessments of successor appointees',
    'If high talent flight: institutional victim status confirmed, suppression justified as >0.75. If low flight: officers more resilient than assumed, suppression may be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_talent_flight_magnitude, empirical, 'Magnitude of institutional talent loss due to purge uncertainty').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pla_loyalty_purge, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pla_tr_t0, pla_loyalty_purge, theater_ratio, 0, 0.52).
narrative_ontology:measurement(pla_tr_t5, pla_loyalty_purge, theater_ratio, 5, 0.59).
narrative_ontology:measurement(pla_tr_t10, pla_loyalty_purge, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(pla_be_t0, pla_loyalty_purge, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(pla_be_t5, pla_loyalty_purge, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(pla_be_t10, pla_loyalty_purge, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pla_loyalty_purge, enforcement_mechanism).
narrative_ontology:affects_constraint(pla_loyalty_purge, chinese_military_factionalism).
narrative_ontology:affects_constraint(pla_loyalty_purge, xi_factional_consolidation).

% DUAL FORMULATION NOTE:
% The PLA loyalty purge is downstream of broader Xi factional consolidation dynamics and affects military institutional factionalism. Related constraints involve competing power centers within the military and the broader Party-state integration mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pla_loyalty_purge, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
