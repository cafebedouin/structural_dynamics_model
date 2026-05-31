% ============================================================================
% CONSTRAINT STORY: elite_legitimacy_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_elite_legitimacy_collapse, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: elite_legitimacy_collapse
 *   human_readable: Elite Legitimacy Collapse in Liberal Democracies
 *   domain: political_economy/comparative_politics/democratic_theory
 *
 * SUMMARY:
 *   The elite legitimacy collapse in liberal democracies represents the
 *   convergence of economic frustration (generational wage stagnation,
 *   regional industrial decline) and cultural grievance (immigration anxiety,
 *   secularization backlash, urban-rural divide) into a unified anti-elite
 *   narrative. Mainstream parties — center-left and center-right coalitions
 *   that governed during the neoliberal era — are framed as corrupt
 *   beneficiaries of globalization who betrayed 'the people.' This constraint
 *   exhibits tangled_rope structure: it performs a genuine coordination
 *   function (aggregating diffuse discontent into legible political demand,
 *   signaling representation failure) while simultaneously enabling
 *   asymmetric extraction (populist leadership captures agenda-setting power
 *   and media attention without delivering material improvements to core
 *   voters). The constraint requires active enforcement through media
 *   amplification, coalition discipline, and the maintenance of the
 *   anti-elite narrative regardless of policy outcomes. The theater_ratio
 *   (0.58) reflects that institutional responses — cordon sanitaire norms,
 *   technocratic reassurances, incremental policy adjustments — are
 *   increasingly performative: they maintain the appearance of democratic
 *   responsiveness while the underlying legitimacy crisis deepens. The
 *   suppression trajectory (0.38 → 0.52) models the intensification of
 *   enforcement: as mainstream parties lose vote share, they face increasing
 *   pressure to adopt populist framing or risk irrelevance, while populist
 *   parties enforce message discipline to prevent policy specificity from
 *   fragmenting their coalition.
 *
 * KEY AGENTS:
 *   - Economically Displaced Voter: Primary victim (powerless/trapped) — bears full cost of legitimacy collapse through policy gridlock and scapegoating without material relief
 *   - Mainstream Party Activist: Secondary victim (moderate/constrained) — coalition-building work undermined by legitimacy crisis; retains agency to adapt but faces career risk
 *   - Populist Party Leadership: Primary beneficiary (institutional/arbitrage) — captures vote share, media coverage, and agenda-setting power through anti-elite narrative
 *   - Anti-Establishment Media: Secondary beneficiary (institutional/arbitrage) — gains audience and influence by amplifying legitimacy crisis
 *   - Civil Society Coalition: Mixed position (organized/mobile) — crisis creates reform openings but delegitimizes expertise and deliberative norms
 *   - Liberal Democratic Norms: Abstract victim (powerless/trapped) — deliberation, compromise, and institutional trust eroded without self-correction mechanism
 *   - Cordon Sanitaire Mechanism: Degraded institutional norm (institutional/constrained) — maintains rhetorical exclusion while populist influence operates through alternative channels
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(elite_legitimacy_collapse, 0.48).
domain_priors:suppression_score(elite_legitimacy_collapse, 0.52).
domain_priors:theater_ratio(elite_legitimacy_collapse, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(elite_legitimacy_collapse, extractiveness, 0.48).
narrative_ontology:constraint_metric(elite_legitimacy_collapse, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(elite_legitimacy_collapse, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(elite_legitimacy_collapse, tangled_rope).
narrative_ontology:human_readable(elite_legitimacy_collapse, "Elite Legitimacy Collapse in Liberal Democracies").
narrative_ontology:topic_domain(elite_legitimacy_collapse, "political_economy/comparative_politics/democratic_theory").

domain_priors:requires_active_enforcement(elite_legitimacy_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(elite_legitimacy_collapse, populist_party_leadership).
narrative_ontology:constraint_beneficiary(elite_legitimacy_collapse, anti_establishment_media).
narrative_ontology:constraint_victim(elite_legitimacy_collapse, liberal_democratic_norms).
narrative_ontology:constraint_victim(elite_legitimacy_collapse, mainstream_party_coalitions).
narrative_ontology:constraint_victim(elite_legitimacy_collapse, policy_deliberation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECONOMICALLY DISPLACED VOTER (SNARE) — Trapped in regions with declining industries, no geographic mobility, facing decades of wage stagnation. The anti-elite narrative offers psychological relief but delivers no material improvement. Maximum experienced extraction: the legitimacy collapse channels frustration into electoral cycles that do not address structural economic decline.
constraint_indexing:constraint_classification(elite_legitimacy_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MAINSTREAM PARTY ACTIVIST (TANGLED ROPE) — Constrained by party loyalty and career investment in existing coalitions, but also benefits from the democratic system's deliberative norms and institutional stability. Experiences both coordination (the party system aggregates preferences) and extraction (the legitimacy collapse undermines their coalition-building work). Significant extraction but not maximal — retains agency to adapt messaging and rebuild trust.
constraint_indexing:constraint_classification(elite_legitimacy_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: POPULIST PARTY LEADERSHIP (ROPE) — Primary beneficiary. The legitimacy collapse creates electoral opportunity and media attention. Experiences the constraint as coordination: the anti-elite narrative aggregates diverse grievances into a unified voting bloc. Net beneficiary — extraction runs toward this agent through vote share gains, media coverage, and agenda-setting power.
constraint_indexing:constraint_classification(elite_legitimacy_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL SOCIETY COALITION (TANGLED ROPE) — Organized agents (NGOs, unions, civic groups) see both coordination function (the legitimacy crisis mobilizes citizens to demand accountability) and extraction (the anti-elite narrative delegitimizes their own expertise and institutional role). Mobile across issue domains but constrained by funding and public trust. Mixed experience: the crisis creates openings for reform but also undermines the deliberative norms they depend on.
constraint_indexing:constraint_classification(elite_legitimacy_collapse, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: CORDON SANITAIRE MECHANISM (PITON) — The institutional norm that mainstream parties refuse coalition with populist parties has degraded into theater. Originally a functional firewall, it now persists through inertia while populist parties gain influence through confidence-and-supply arrangements, policy adoption, and normalization. High theater ratio: the cordon is maintained rhetorically but bypassed structurally.
constraint_indexing:constraint_classification(elite_legitimacy_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global civilizational perspective, the legitimacy collapse exhibits both genuine coordination function (aggregating diffuse discontent into legible political demand) and asymmetric extraction (concentrating agenda-setting power in populist leadership while diffusing accountability). The constraint requires active enforcement through media amplification and coalition discipline. Analytical classification matches claimed type.
constraint_indexing:constraint_classification(elite_legitimacy_collapse, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(elite_legitimacy_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(elite_legitimacy_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(elite_legitimacy_collapse, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(elite_legitimacy_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(elite_legitimacy_collapse, TR),
    TR >= 0.70.

:- end_tests(elite_legitimacy_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Populist leadership captures substantial electoral and agenda-setting benefits during the legitimacy crisis, but the extraction is not total — some voters do experience the anti-elite narrative as genuine representation of their grievances, and some mainstream parties successfully adapt. The value reflects that the career and influence asymmetry is real and significant, but the constraint also performs coordination work. Suppression (0.52): Moderate-high. Significant barriers to alternative political expression include media amplification of the anti-elite frame, coalition discipline enforcing message purity, cordon sanitaire norms limiting mainstream-populist cooperation, and the psychological lock-in of identity-based voting. But suppression is not total — civil society retains voice, some voters remain persuadable, and institutional checks persist. Theater ratio (0.58): Moderate-high. Institutional responses to the legitimacy crisis are substantially performative: cordon sanitaire norms are maintained rhetorically while populist influence grows through confidence-and-supply arrangements and policy adoption; technocratic reassurances about economic recovery do not address the structural drivers; incremental policy adjustments signal responsiveness without delivering material change. The theater has increased over the interval as the gap between institutional rhetoric and voter experience has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same legitimacy crisis appears as pure extraction (snare) from the perspective of trapped economically displaced voters, as coordination (rope) from the perspective of populist leadership benefiting from the anti-elite narrative, as degraded ritual (piton) from the perspective of the cordon sanitaire mechanism, and as mixed coordination-extraction (tangled_rope) from the perspectives of mainstream party activists, civil society coalitions, and the analytical observer. The gap is not a measurement error — it reflects real structural differences in how agents experience the constraint based on their power, exit options, and relationship to the extraction flow. The powerless trapped voter cannot exit the legitimacy crisis and bears maximum extraction. The institutional beneficiary with arbitrage options experiences net benefit. The organized agents with mobile exit see both functions. The analytical observer integrates across perspectives and confirms the tangled_rope structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The populist party leadership is the primary beneficiary — they are institutional actors with arbitrage exit options (can shift between opposition and government, between national and European arenas) who capture vote share and agenda-setting power through the legitimacy collapse. The engine derives low d (beneficiary + arbitrage) → low/negative f(d) → low/negative chi, producing the rope classification from their perspective. The economically displaced voter is the primary victim — powerless with trapped exit options (no geographic mobility, no career alternatives, decades-long time horizon) who bears the full cost of policy gridlock and scapegoating. The engine derives high d (victim + trapped) → high f(d) → high chi, producing the snare classification from their perspective. Mainstream party activists are secondary victims with constrained exit (party loyalty, career investment) who experience both coordination (the party system still aggregates preferences) and extraction (the legitimacy collapse undermines their work), producing tangled_rope. Civil society coalitions are organized with mobile exit (can shift issue domains, funding sources) and experience mixed benefits and costs, also producing tangled_rope. The analytical observer sees the full structure: genuine coordination function (signaling representation failure) combined with asymmetric extraction (leadership capture without accountability), confirming the tangled_rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled_rope classification requires BOTH genuine coordination function AND asymmetric extraction, with active enforcement maintaining the hybrid. The coordination function is real: the anti-elite narrative aggregates diffuse economic and cultural grievances into legible political demand, signals representation failure to mainstream parties, and mobilizes previously disengaged voters. The extraction is also real: populist leadership captures agenda-setting power and media attention without delivering material improvements to core voters, while the anti-elite frame delegitimizes policy deliberation and expertise. The constraint requires active enforcement: media amplification maintains the narrative's salience, coalition discipline prevents policy specificity from fragmenting the voting bloc, and cordon sanitaire norms create the outsider status that sustains the anti-elite identity. Without this enforcement, the coordination function would either succeed (mainstream parties would adapt and the crisis would resolve) or fail (voters would defect when material conditions do not improve). The tangled_rope structure persists because the enforcement mechanism prevents both resolution pathways.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    grievance_legitimacy_threshold,
    'At what threshold of economic frustration does the anti-elite narrative shift from legitimate accountability demand to extractive scapegoating?',
    'Longitudinal analysis correlating economic indicators (wage stagnation, unemployment duration, regional decline) with survey measures of institutional trust and populist vote share; identification of inflection points where grievance expression becomes decoupled from policy responsiveness',
    'If threshold is low (minor economic stress triggers delegitimization): the constraint is primarily extractive, exploiting normal democratic friction. If threshold is high (only severe prolonged decline triggers collapse): the constraint is primarily coordinative, signaling genuine representation failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grievance_legitimacy_threshold, empirical, 'Economic threshold distinguishing legitimate grievance from extractive scapegoating').

omega_variable(
    populist_governance_counterfactual,
    'Do populist parties in government deliver material improvements to their core constituencies, or does the anti-elite narrative persist regardless of policy outcomes?',
    'Comparative analysis of economic outcomes (wage growth, employment, regional investment) in regions governed by populist vs mainstream parties; panel data tracking whether populist voters'' material conditions improve post-election',
    'If populist governance improves outcomes: the legitimacy collapse is a coordination mechanism surfacing genuine policy failure. If outcomes do not improve or worsen: the constraint is extractive, with the anti-elite narrative serving leadership interests rather than voter welfare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(populist_governance_counterfactual, empirical, 'Whether populist governance delivers material improvements to core voters').

omega_variable(
    cordon_sanitaire_effectiveness,
    'Does the cordon sanitaire norm actually prevent populist policy influence, or does it merely delay formal coalition while allowing informal agenda-setting?',
    'Policy content analysis comparing mainstream party platforms before and after populist electoral gains; tracking of legislative votes where mainstream parties adopt populist positions; measurement of populist influence through confidence-and-supply arrangements vs formal coalition',
    'If cordon is effective: piton classification confirmed — the norm is degraded but still functional. If cordon is bypassed: the norm is pure theater, and populist influence operates through alternative channels regardless of formal exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cordon_sanitaire_effectiveness, empirical, 'Whether cordon sanitaire prevents or merely delays populist influence').

omega_variable(
    cultural_vs_economic_primacy,
    'Is the legitimacy collapse primarily driven by economic frustration (material grievance) or cultural backlash (identity threat), and does the answer vary by national context?',
    'Regression analysis decomposing populist vote share into economic predictors (wage stagnation, unemployment, trade exposure) vs cultural predictors (immigration rates, secularization, urban-rural divide); cross-national comparison of driver weights',
    'If economic drivers dominate: the constraint is downstream of generational_economic_decline (mountain) and the coordination function is genuine. If cultural drivers dominate: the constraint is downstream of cultural_backlash_mobilization (tangled_rope) and the extraction mechanism is identity-based rather than material.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_vs_economic_primacy, empirical, 'Relative weight of economic vs cultural drivers of legitimacy collapse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(elite_legitimacy_collapse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elite_legit_theater_t0, elite_legitimacy_collapse, theater_ratio, 0, 0.35).
narrative_ontology:measurement(elite_legit_theater_t5, elite_legitimacy_collapse, theater_ratio, 5, 0.48).
narrative_ontology:measurement(elite_legit_theater_t10, elite_legitimacy_collapse, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(elite_legit_extract_t0, elite_legitimacy_collapse, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(elite_legit_extract_t5, elite_legitimacy_collapse, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(elite_legit_extract_t10, elite_legitimacy_collapse, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(elite_legit_suppress_t0, elite_legitimacy_collapse, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(elite_legit_suppress_t5, elite_legitimacy_collapse, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(elite_legit_suppress_t10, elite_legitimacy_collapse, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(elite_legitimacy_collapse, identity_coordination).

% DUAL FORMULATION NOTE:
% The elite legitimacy collapse is downstream of both generational_economic_decline (mountain — structural wage stagnation and regional industrial decline) and cultural_backlash_mobilization (tangled_rope — identity threat and secularization anxiety). The legitimacy collapse has its own extractiveness (0.48) reflecting the political economy of the anti-elite narrative, distinct from the upstream constraints' extractiveness values. The omega variable cultural_vs_economic_primacy addresses the relative weight of these upstream drivers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
