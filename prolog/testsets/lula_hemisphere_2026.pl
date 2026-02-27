% ============================================================================
% CONSTRAINT STORY: lula_hemisphere_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lula_hemisphere_2026, []).

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
 *   constraint_id: lula_hemisphere_2026
 *   human_readable: The Monroe Doctrine Revival (Unilateral US Hegemony)
 *   domain: political/international_relations
 *
 * SUMMARY:
 *   The Monroe Doctrine revival, as articulated by President Lula in response
 *   to hypothetical 2026 US military intervention in Venezuela, represents a
 *   unilateral reassertion of hemispheric hegemony by the United States. This
 *   constraint models how a single dominant power maintains enforcement
 *   capacity over a region through threat credibility, institutional theater,
 *   and structural power differentials. The extractiveness (0.58) reflects
 *   that while genuine regional coordination interests exist (hemispheric
 *   stability, trade frameworks), the dominant benefit flows to the US
 *   geopolitical position and military-industrial complex. The suppression
 *   (0.72) is high because regional states cannot exit: they are formally
 *   sovereign but practically constrained by US military dominance and the
 *   credible threat of unilateral intervention. The theater ratio (0.48) has
 *   been declining because the doctrine increasingly relies on direct power
 *   demonstration rather than institutional narrative — the Cold War
 *   institutions (OAS, TIAR) that once provided theater are losing
 *   legitimacy, requiring more explicit threat credibility to maintain the
 *   constraint. The constraint exhibits all six types from different
 *   structural positions, making it a diagnostic exemplar for how power
 *   asymmetry generates perspectival pluralism.
 *
 * KEY AGENTS:
 *   - United States Military-Industrial Complex: Primary beneficiary (institutional/arbitrage) — captures enforcement narrative and defense spending justification
 *   - Venezuela: Primary victim (powerless/trapped) — faces unilateral threat with no exit mechanism or security alternatives
 *   - Brazil and Regional States: Secondary victims (moderate/constrained) — formally sovereign but constrained by US power differential; have diplomatic options but no military exit
 *   - US Democratic Institutions: Mixed (powerful/mobile) — benefit from hegemonic narrative but constrained by domestic political costs of interventionism
 *   - Cold War Security Institutions (OAS, TIAR): Institutional actors (institutional/arbitrage) — maintain ceremonial role but have lost functional coordination capacity (piton)
 *   - Emerging Multipolar Coalitions (BRICS, CELAC): Organized actors (organized/constrained) — attempting to build alternative constraint-breaking pathways with potential sunset
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lula_hemisphere_2026, 0.58).
domain_priors:suppression_score(lula_hemisphere_2026, 0.72).
domain_priors:theater_ratio(lula_hemisphere_2026, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lula_hemisphere_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(lula_hemisphere_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(lula_hemisphere_2026, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lula_hemisphere_2026, snare).
narrative_ontology:human_readable(lula_hemisphere_2026, "The Monroe Doctrine Revival (Unilateral US Hegemony)").
narrative_ontology:topic_domain(lula_hemisphere_2026, "political/international_relations").

domain_priors:requires_active_enforcement(lula_hemisphere_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lula_hemisphere_2026, us_military_industrial_complex).
narrative_ontology:constraint_beneficiary(lula_hemisphere_2026, us_geopolitical_dominance).
narrative_ontology:constraint_victim(lula_hemisphere_2026, latin_american_sovereignty).
narrative_ontology:constraint_victim(lula_hemisphere_2026, regional_multipolarity).
narrative_ontology:constraint_victim(lula_hemisphere_2026, venezuela_political_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VENEZUELA (SNARE) — Faces unilateral military threat with no exit mechanism. Cannot organize alternative security arrangements; sovereignty constrained by US capacity to intervene unilaterally. Maximum experienced extraction — regime selection imposed externally.
constraint_indexing:constraint_classification(lula_hemisphere_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: BRAZIL/REGIONAL ACTORS (SNARE) — Formally sovereign but constrained by US military dominance. Regional alternatives (CELAC, ALBA, PROSUR) lack enforcement capacity. Can resist diplomatically but cannot exit the constraint structure. High extraction of regional autonomy through threat credibility.
constraint_indexing:constraint_classification(lula_hemisphere_2026, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: US MILITARY-INDUSTRIAL COMPLEX (ROPE) — Primary beneficiary. Experiences the constraint as coordination: maintaining hemispheric dominance requires threat credibility and occasional demonstration. Intervention threat generates defense contractor demand, justifies military budgets, establishes geostrategic narrative. Net beneficiary through arbitrage — can redirect resources if hegemony is challenged elsewhere.
constraint_indexing:constraint_classification(lula_hemisphere_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: US DEMOCRATIC GOVERNANCE (TANGLED ROPE) — US itself experiences coordination function (hemispheric stability narrative) AND asymmetric extraction (military commitment, opportunity cost, domestic political risk of interventionism). Congress theoretically constrains intervention, but executive power to maintain threat credibility enables hegemonic extraction. Hybrid: genuine coordination (hemispheric framework) plus extraction from US domestic resources and democratic discourse.
constraint_indexing:constraint_classification(lula_hemisphere_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: COLD WAR SECURITY INSTITUTIONS (PITON) — OAS, Inter-American Treaty of Reciprocal Assistance (TIAR), bilateral defense pacts are theater: their institutional forms persist from Cold War logic but the functional security problem has shifted. Invoked ceremonially to justify intervention but lack genuine coordination capacity in multipolar context. Theater ratio high because revival requires performative invocation of anachronistic doctrines.
constraint_indexing:constraint_classification(lula_hemisphere_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EMERGING MULTIPOLAR INSTITUTIONS (SCAFFOLD) — BRICS, Shanghai Cooperation Organization, global South coalitions provide temporary constraint on US unilateralism. These represent a scaffold with potential sunset: if successful, they reduce US enforcement capacity and regional states gain exit options. Currently constrained (members have territorial disputes, economic dependencies on US markets) but provides alternative verification pathway for regional autonomy norms.
constraint_indexing:constraint_classification(lula_hemisphere_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / REALIST VIEW (MOUNTAIN) — From civilizational/universal perspective, hegemonic powers inevitably enforce spheres of influence — this appears as natural law of international politics. Power differentials create structural enforcement without explicit doctrine. However, the base data contradicts mountain classification: the extractiveness (0.58) and suppression (0.72) indicate contingent institutional arrangements (Monroe Doctrine doctrinal revival, threat credibility maintenance) rather than immutable structural limits. False summit detection applies.
constraint_indexing:constraint_classification(lula_hemisphere_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lula_hemisphere_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lula_hemisphere_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lula_hemisphere_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lula_hemisphere_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lula_hemisphere_2026, TR),
    TR >= 0.70.

:- end_tests(lula_hemisphere_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): Moderate-high. The Monroe Doctrine revival serves clear extraction functions: guarantees US military dominance in the region, enables US resource access (energy, minerals, agricultural markets), and provides geopolitical leverage against China and Russia. However, the extraction is not complete (0.75+) because genuine regional coordination interests exist — hemispheric stability benefits all parties, trade frameworks provide mutual gain, security cooperation has real benefits. The constraint is hybrid extraction plus coordination, not pure extraction. Over the interval (0-6 years), extractiveness increases from 0.42 to 0.58 as US reasserts dominance against multipolar challenges. Suppression (0.72): High. Regional states face significant barriers to exit: military capability gap is immense, institutional alternatives (BRICS, CELAC) lack enforcement capacity, economic dependencies tie regional states to US markets, and threat credibility makes defection costly. However, suppression is not total (0.80+) because some diplomatic and economic counter-options exist — Brazil can navigate multiple relationships, Mexico has USMCA leverage, even Venezuela has Chinese and Russian partnerships. Theater ratio (0.48): Moderate-low and declining. The Monroe Doctrine is invoked ceremonially (theater), but the constraint increasingly relies on direct power demonstration rather than institutional narrative. Cold War institutions have lost legitimacy as doctrine revival becomes more explicit. Theater declines from 0.55 to 0.48 because the narrative must back up with credible threat — performance alone no longer suffices.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximum: powerless agents experience snare (pure extraction), institutional beneficiaries experience rope (coordination), moderate actors experience tangled rope (mixed), powerful democratic institutions experience tension between coordination and extraction, anachronistic institutions experience degradation (piton), and emerging coalitions experience a temporary constraint with potential sunset (scaffold). This diversity arises from the same base structural data because directionality values differ — beneficiaries and victims occupy opposite positions in the extraction flow, producing opposite classifications despite identical baseline metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary-victim declarations map directly to directionality derivation. The US military-industrial complex and geopolitical dominance are declared beneficiaries — they experience low d values and thus negative or neutral χ, producing rope classification. Latin American sovereignty, regional multipolarity, and Venezuelan autonomy are declared victims — they experience high d values and thus high χ, producing snare classification. The gradient between beneficiary institutional actors (d ≈ 0.15-0.35) and victim regional states (d ≈ 0.65-0.95) explains the perspectival range. Directionality overrides are not required because the structural beneficiary/victim declarations accurately capture the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves the mandatrophy by showing that the snare classification from Venezuela's perspective is not mislabeled coordination. Venezuela genuinely receives no coordination benefit — the constraint is purely extractive from the victim's view. Simultaneously, the beneficiary's rope classification is not mislabeled extraction — the US genuinely experiences coordination value (hemispheric stability, predictable access, reduced multipolarity friction). The constraint's extractiveness (0.58) prevents it from being pure rope (which requires ε ≤ 0.45) — there is genuine asymmetric extraction, not pure coordination. The suppression (0.72) prevents it from being pure snare (snare typically requires suppression ≥ 0.60 but often closer to 0.80+) because some regional exit options exist. The measured extractiveness and suppression accurately capture the hybrid nature. The mandatrophy is resolved by recognizing that the constraint IS legitimately both coordination (for beneficiaries) and extraction (for victims) from different structural positions — there is no paradox, only perspectival divergence driven by power asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unilateral_intervention_credibility,
    'Does US retain unilateral intervention capacity in 2026, or has multipolar fragmentation made solo intervention prohibitively costly?',
    'Historical assessment: if US intervenes unilaterally and succeeds, credibility confirmed. If costs exceed expected benefits, credibility degrades. If no intervention occurs, counterfactual scenario analysis required.',
    'If credible: snare classification holds. If credibility collapses: constraint reclassifies as piton or degrades to rope. Entire regional extraction mechanism depends on threat believability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_intervention_credibility, empirical, 'Whether US maintains credible unilateral intervention capacity').

omega_variable(
    regional_coalition_counter_capacity,
    'Can BRICS, CELAC, or emerging multilateral coalitions actually constrain US intervention, or are they diplomatic theater without enforcement teeth?',
    'Test case: Brazilian/multilateral response to hypothetical US military action. Measurement of actual economic sanctions, military mobilization, or diplomatic isolation inflicted on US.',
    'If coalitions have teeth: scaffold classification is structural, sunset is real, regional states gain genuine exit options. If purely performative: scaffold is piton (theater without function), regional extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_coalition_counter_capacity, empirical, 'Whether regional coalitions have enforcement capacity against US').

omega_variable(
    domestic_us_political_constraint,
    'Does US domestic democratic discourse actually constrain executive intervention capacity, or has security state authority decoupled from democratic accountability?',
    'Analysis of Congressional authority assertions: do they block interventions (constraint confirmed) or only generate theater of debate while executive acts? Legislative vs executive budget outcomes.',
    'If Congress actually constrains: tangled rope classification confirmed (genuine coordination + extraction tension). If Congress is theater: executive hegemony increases, constraint reclassifies as pure snare from regional view.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domestic_us_political_constraint, conceptual, 'Whether US domestic democracy constrains executive power').

omega_variable(
    ideological_doctrine_necessity,
    'Does unilateral hegemony require Monroe Doctrine ideology to sustain, or does power differential enforce compliance without doctrinal narrative?',
    'Comparative analysis: does power work without the doctrine (power realism)? Or does doctrine legitimacy matter for coalition-building and international law compliance?',
    'If doctrine is necessary: theater ratio correct (extractiveness requires narrative). If power is sufficient: theater is lower, constraint is more purely structural (extractiveness should increase, doctrine is decoration).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_doctrine_necessity, conceptual, 'Whether doctrinal narrative is necessary for hegemonic enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lula_hemisphere_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lula_tr_t0, lula_hemisphere_2026, theater_ratio, 0, 0.55).
narrative_ontology:measurement(lula_tr_t3, lula_hemisphere_2026, theater_ratio, 3, 0.51).
narrative_ontology:measurement(lula_tr_t6, lula_hemisphere_2026, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(lula_be_t0, lula_hemisphere_2026, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lula_be_t3, lula_hemisphere_2026, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(lula_be_t6, lula_hemisphere_2026, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lula_hemisphere_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(lula_hemisphere_2026, brics_multipolarity_challenge).
narrative_ontology:affects_constraint(lula_hemisphere_2026, venezuela_regime_survival).
narrative_ontology:affects_constraint(lula_hemisphere_2026, brazilian_regional_leadership).
narrative_ontology:affects_constraint(lula_hemisphere_2026, us_military_spending_justification).

% DUAL FORMULATION NOTE:
% The Monroe Doctrine constraint can be decomposed into distinct upstream constraints: US military dominance as pure structural power (higher extractiveness, lower theater), and Monroe Doctrine as doctrinal narrative framework (higher theater, lower pure extraction). This story models the hybrid — the doctrine revival requires both structural power and narrative legitimacy. Upstream constraints affect this one through power differential validation and institutional theater provision.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
