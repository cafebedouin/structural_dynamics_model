% ============================================================================
% CONSTRAINT STORY: us_isolationism_policy_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_isolationism_policy_2026, []).

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
 *   constraint_id: us_isolationism_policy_2026
 *   human_readable: US Withdrawal from International Organizations under "America First" Doctrine
 *   domain: geopolitical/international_relations
 *
 * SUMMARY:
 *   The US withdrawal from international organizations under an "America
 *   First" doctrine creates a structural constraint on global coordination
 *   that operates differently depending on the observer's position relative
 *   to US power and their dependence on multilateral institutions. This
 *   constraint exhibits the full spectrum of Deferential Realism types: from
 *   snare (for developing nations trapped without alternatives) through
 *   tangled rope (for allied powers forced to coordinate without US
 *   participation) to rope (for the US nationalist constituency experiencing
 *   withdrawal as liberation) and piton (for degraded institutions
 *   maintaining performative function). The constraint's extractiveness
 *   arises not from overt coercion but from the asymmetric consequences of
 *   coordination withdrawal: US actors can opt out; non-US actors cannot
 *   escape the resulting coordination vacuum. Theater ratio of 0.61 reflects
 *   that international institutions continue issuing guidance and maintaining
 *   formal structures even as their enforcement capacity and legitimacy
 *   collapse—the institutional shell persists through inertia rather than
 *   functionality. The temporal measurement shows extractiveness increasing
 *   from 0.35 (pre-withdrawal baseline with intact multilateralism) to 0.58
 *   (five years into withdrawal with alternative mechanisms partially
 *   emerged) and theater ratio rising from 0.42 to 0.61 as institutions
 *   become increasingly performative.
 *
 * KEY AGENTS:
 *   - US Nationalist Coalition: Primary beneficiary (institutional/arbitrage) — experiences withdrawal as regaining sovereignty; captures redirected resource allocation
 *   - Developing Nations (Non-Aligned): Primary victim (powerless/trapped) — depend on WHO, IMF, UN without alternative sources of coordination capacity; trapped by asymmetric dependency
 *   - EU and Allied Powers: Secondary victim (organized/constrained) — forced to maintain coordination mechanisms unilaterally; cannot exit without fragmenting alliance; constrained between loyalty and cost
 *   - Competing Great Powers (China, Russia): Tertiary actor (powerful/mobile) — benefit from coordination vacuum; destabilized by loss of rules-based order; able to shift strategies bilaterally
 *   - US-Dependent Trade Partners/Allies: Secondary victim (moderate/constrained) — dependent on US market access and military guarantees; lose coordination benefits while maintaining trade asymmetries
 *   - International Institutions (UN, WHO, IMF, World Bank): Institutional shell (institutional/arbitrage) — suffer degradation of legitimacy and enforcement capacity; maintain formal function through inertia
 *   - Civilization-Level Analyst: Observational perspective (analytical/analytical) — identifies constraint as temporary scaffold with crisis-driven sunset clause
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_isolationism_policy_2026, 0.58).
domain_priors:suppression_score(us_isolationism_policy_2026, 0.68).
domain_priors:theater_ratio(us_isolationism_policy_2026, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_isolationism_policy_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_isolationism_policy_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_isolationism_policy_2026, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_isolationism_policy_2026, tangled_rope).
narrative_ontology:human_readable(us_isolationism_policy_2026, "US Withdrawal from International Organizations under \"America First\" Doctrine").
narrative_ontology:topic_domain(us_isolationism_policy_2026, "geopolitical/international_relations").

domain_priors:requires_active_enforcement(us_isolationism_policy_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_isolationism_policy_2026, us_nationalist_constituency).
narrative_ontology:constraint_beneficiary(us_isolationism_policy_2026, selective_domestic_industries).
narrative_ontology:constraint_victim(us_isolationism_policy_2026, global_public_health_systems).
narrative_ontology:constraint_victim(us_isolationism_policy_2026, climate_coordination_mechanisms).
narrative_ontology:constraint_victim(us_isolationism_policy_2026, multilateral_development_banks).
narrative_ontology:constraint_victim(us_isolationism_policy_2026, us_soft_power_projection).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING NATIONS (SNARE) — Dependent on WHO disease surveillance, IMF/World Bank financing, and UN coordination for disaster response. US withdrawal removes coordinating capacity without removing the constraint on unilateral action by remaining powers. Trapped: cannot exit multilateral systems; cannot replace US funding. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.95.
constraint_indexing:constraint_classification(us_isolationism_policy_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EU AND ALLIED POWERS (TANGLED ROPE) — Must maintain coordination mechanisms (NATO, climate accords) but now without US participation. Constrained: exit would fragment alliance; staying means bearing coordination costs unilaterally. Simultaneously benefit from reduced US veto power on some regulatory issues. d≈0.62, f(d)≈0.82, σ=1.1 → χ≈0.47.
constraint_indexing:constraint_classification(us_isolationism_policy_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: US NATIONALIST COALITION (ROPE) — Primary beneficiary. Experiences withdrawal as coordination solution: freeing US from 'unfair' burden-sharing, redirecting resources to domestic priorities, reducing regulatory constraints from international bodies. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(us_isolationism_policy_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMPETING GREAT POWERS (TANGLED ROPE) — US withdrawal creates coordination vacuum that these powers exploit (expanding sphere of influence) while simultaneously destabilizing predictable rules. Mobile: can pivot to bilateral agreements. Victim of uncertainty (extraction from rules-based order); beneficiary of reduced US capacity. d≈0.48, f(d)≈0.63, σ=1.2 → χ≈0.37.
constraint_indexing:constraint_classification(us_isolationism_policy_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL INSTITUTIONS (PITON) — Theater ratio 0.61: these organizations become increasingly performative when major power withdraws. WHO issues guidance with reduced enforcement capacity. UN convenes meetings with reduced legitimacy. IMF maintains lending frameworks to irrelevant markets. Original function (global coordination) atrophies; institutional shell persists through inertia and remaining members' loyalty. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(us_isolationism_policy_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: US-DEPENDENT INDUSTRIES AND ALLIED TRADE PARTNERS (TANGLED ROPE) — Constrained: depend on US market access, military alliance guarantees, and technology standards that come bundled with international participation. Withdrawal extracts value from these partners through reduced coordination benefits while maintaining trade asymmetries. Simultaneously benefit from reduced environmental/labor standards coordination. d≈0.71, f(d)≈1.07, σ=1.1 → χ≈0.67.
constraint_indexing:constraint_classification(us_isolationism_policy_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: CIVILIZATION-LEVEL RISK ANALYST (SCAFFOLD) — Views this constraint as a temporary institutional degradation with a civilizational sunset clause. US isolationism provokes crisis (pandemic, climate, financial) that forces reentry into multilateral coordination. Theater ratio 0.61 is artificially high because symbolic commitment still exists even as execution capacity vanishes. Once crisis strikes, either the US returns (constraint dissolved) or develops permanent alternatives (constraint morphs). d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.49. Sunset: 5-15 years until crisis forces recalibration.
constraint_indexing:constraint_classification(us_isolationism_policy_2026, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_isolationism_policy_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_isolationism_policy_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_isolationism_policy_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_isolationism_policy_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_isolationism_policy_2026, TR),
    TR >= 0.70.

:- end_tests(us_isolationism_policy_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits extraction in two dimensions: (1) US actors extract short-term benefits (reduced contributions, redirected spending) while non-US actors bear coordination costs; (2) Non-US dependency on US-led institutions creates asymmetric vulnerability when US withdraws unilaterally. However, extraction is not total (0.70+) because: alternatives are emerging (EU military coordination, BRICS expansion), crisis will likely force US reentry, and some 'extraction' is legitimate geopolitical adjustment rather than pure rent-seeking. Suppression (0.68): High. Developing nations have suppressed alternatives to US-led institutions (no credible competing global health authority replaces WHO in 5-year window; no immediate alternative to dollar-denominated IMF lending). EU and China can establish alternatives but require years to build. Suppression is not total because: alternatives are technically feasible, motivated actors are available, and the timescale is not instantaneous. Theater ratio (0.61): Moderate-high. International organizations continue formal operations—WHO issues disease surveillance alerts, UN holds General Assemblies, IMF releases policy guidance—despite reduced US participation reducing their enforcement capacity and legitimacy. The theater is not extremely high (0.70+) because: institutions retain some legitimacy among non-US members and continue functional activities at reduced scale; the performative aspect is a degradation, not an origination.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same policy produces radically different classifications depending on structural position. The US nationalist sees coordination (Rope) — exit solves a perceived collective action problem (burden-sharing fairness). EU/allies see mixed coordination and extraction (Tangled Rope) — they must maintain systems unilaterally but lose US veto power on some issues. Developing nations see pure extraction (Snare) — they lose coordination capacity without gaining exit options. Competing powers see opportunity in chaos (Tangled Rope) — they benefit from vacuum but destabilized by loss of rules. International institutions see their own atrophy (Piton) — they persist through inertia despite loss of function. The civilizational analyst sees a temporary shock with a sunset clause (Scaffold) — crisis will force reentry. The perspectival gap is driven by exit optionality: institutional actors with alternatives (US, China) see flexibility; trapped actors (developing nations) see constraint; intermediate actors (EU, allied trade partners) experience both.
 *
 * DIRECTIONALITY LOGIC:
 *   US Nationalist Coalition: Beneficiary + arbitrage exit → d≈0.08, f(d)≈-0.10. Negative effective extraction; net beneficiary. Developing Nations: Victims + trapped exit → d≈0.92, f(d)≈1.38. Maximum extraction; no ability to exit or establish alternatives. EU/Allied Powers: Victims of coordination cost + constrained exit → d≈0.62, f(d)≈0.82. Significant extraction; constrained because alliance loyalty prevents full exit. Competing Great Powers: Mixed (beneficiaries of vacuum + destabilized by rules collapse) + mobile exit → d≈0.48, f(d)≈0.63. Intermediate extraction; able to shift strategies but constrained by uncertainty. US-Dependent Trade Partners: Victims of coordination loss + constrained exit → d≈0.71, f(d)≈1.07. High extraction; constrained by US market dependency overrides ability to seek alternatives. International Institutions: Institutional degradation + arbitrage exit (could reform/relocate but inert) → d≈0.10, f(d)≈-0.08. Net neutral; theater ratio (0.61) is the piton signature, not high chi.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing between pure extraction (Snare) and mixed coordination-extraction (Tangled Rope). The critical distinction is whether the constraint has a coordination function that parties depend on (yes: EU/allies depend on NATO coordination, developing nations depend on health surveillance) or whether it is pure extraction (no: developing nations receive coordination value but cannot exit). The constraint is Tangled Rope at base type because: (1) it has a coordination function (multilateral organizations solve collective action problems), (2) it exhibits asymmetric extraction (benefits concentrated in US, costs distributed globally), (3) it requires active enforcement (US policy commitment to withdrawal and non-participation). The snare perspective (developing nations) is real but reflects their structural position, not the constraint's true type. Extractiveness 0.58 and suppression 0.68 confirm tangled rope gates: not low enough for pure rope (ε ≤ 0.45), not high enough for pure snare (ε ≥ 0.66). The scaffold perspective identifies the constraint's sunset clause: global crisis (pandemic, climate, financial) will force US reentry or force alternatives to mature sufficiently, both of which dissolve the constraint within 10-15 years. Therefore: Tangled Rope primary type, with critical omega variables around reentry triggers and alternative formation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    us_reentry_trigger,
    'What crisis event (pandemic, climate disaster, financial collapse) would force US reentry into multilateral coordination, and how binding would reentry commitments be?',
    'Historical precedent analysis (US withdrawal and reentry patterns, 1945-present); identification of decision thresholds for executive reengagement; structural analysis of crisis propagation time vs policy implementation lag',
    'If US reentry occurs within 5 years: constraint is temporary scaffold (sunset clause confirmed). If US remains outside > 10 years and alternatives stabilize: constraint morphs into permanent piton (institutional degradation becomes structural). If reentry occurs but with weaker commitments: constraint becomes snare for non-US parties (extraction without coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_reentry_trigger, empirical, 'Crisis-driven US reentry timeline and commitment binding').

omega_variable(
    alternative_coordination_formation,
    'Do remaining powers (EU, China, India, ASEAN) successfully establish alternative coordination mechanisms that provide genuine alternatives to US-led institutions, or do they fragment into competing blocs?',
    'Quantitative: measure coordination cost reductions in non-US mechanisms vs US mechanisms; assess compliance rates and dispute resolution success rates. Qualitative: identify institutional innovations (new frameworks, novel governance structures)',
    'If successful alternatives emerge: constraint shifts from snare (for non-US actors) toward tangled rope (asymmetric extraction replaced by mutual coordination). If fragmentation occurs: constraint becomes snare for all non-great-powers (trapped between competing blocs).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_coordination_formation, empirical, 'Success of alternative multilateral coordination without US leadership').

omega_variable(
    domestic_cost_realization,
    'As US soft power, scientific leadership, and alliance reliability degrade, do domestic political costs (loss of geopolitical influence, reduced access to international talent/markets) force policy reversal before crisis triggers reentry?',
    'Measurement of US diplomatic capacity metrics (bilateral agreement success rate, allied military deployments in US interest, research collaboration rates). Political feedback (polling on isolationism sustainability; primary/general election outcomes).',
    'If domestic costs realized quickly: constraint becomes temporary (scaffold) with shorter sunset clause (2-5 years). If costs diffuse or are blamed on opponents: constraint persists longer as snare (extraction sustained by manufactured scarcity narratives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_cost_realization, preference, 'Domestic political reversal triggered by soft power/influence loss').

omega_variable(
    china_expansion_pace,
    'How rapidly does China expand its sphere of influence through Belt-and-Road, RCEP, and BRICS mechanisms once US coordination capacity withdraws? Does pace trigger US emergency reengagement or does it stabilize as new equilibrium?',
    'Quantitative: measure Chinese FDI, military presence, and institutional leadership in regions previously under US influence (Southeast Asia, Middle East, Africa). Timeline analysis: correlate US withdrawal announcement with acceleration of Chinese expansion.',
    'If rapid expansion (≥20% annual increase): great-power competition enters unstable phase (snare for smaller powers, tangled rope for mid-powers). If slow expansion (≤5% annual increase): new equilibrium settles (permanent degradation, piton for institutions).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(china_expansion_pace, empirical, 'Pace of Chinese expansion into US-vacated institutional space').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_isolationism_policy_2026, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(isol_tr_t0, us_isolationism_policy_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(isol_tr_t2, us_isolationism_policy_2026, theater_ratio, 2, 0.52).
narrative_ontology:measurement(isol_tr_t5, us_isolationism_policy_2026, theater_ratio, 5, 0.61).

% Extraction over time
narrative_ontology:measurement(isol_be_t0, us_isolationism_policy_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(isol_be_t2, us_isolationism_policy_2026, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(isol_be_t5, us_isolationism_policy_2026, base_extractiveness, 5, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_isolationism_policy_2026, global_infrastructure).
narrative_ontology:affects_constraint(us_isolationism_policy_2026, climate_treaty_enforcement).
narrative_ontology:affects_constraint(us_isolationism_policy_2026, global_pandemic_response_capacity).
narrative_ontology:affects_constraint(us_isolationism_policy_2026, multilateral_development_financing).
narrative_ontology:affects_constraint(us_isolationism_policy_2026, nato_burden_sharing_burden).
narrative_ontology:affects_constraint(us_isolationism_policy_2026, chinese_sphere_expansion).

% DUAL FORMULATION NOTE:
% US isolationism affects multiple downstream constraints by removing coordination capacity and reducing enforcement credibility. Each downstream constraint should be analyzed separately for its own ε, suppression, and perspectives. For example, climate_treaty_enforcement has its own extractiveness profile reflecting the specific mechanisms of carbon markets and regulatory capture, distinct from the general coordination withdrawal modeled here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_isolationism_policy_2026, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
