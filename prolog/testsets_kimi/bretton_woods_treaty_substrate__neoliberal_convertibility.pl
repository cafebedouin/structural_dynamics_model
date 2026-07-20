% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__neoliberal_convertibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__neoliberal_convertibility, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bretton_woods_treaty_substrate__neoliberal_convertibility
 *   human_readable: Bretton Woods Neoliberal Convertibility Regime
 *   domain: international_political_economy/monetary_history
 *
 * SUMMARY:
 *   This constraint story instantiates the neoliberal_convertibility reading
 *   of the Bretton Woods treaty substrate. Under this reading, the post-war
 *   monetary architecture is interpreted not as 'embedded liberalism'
 *   protecting domestic policy space, but as a regime that constrains
 *   government intervention to enable free international capital markets. The
 *   International Monetary Fund and World Bank enforce convertibility and
 *   capital account openness through conditionality, while global financial
 *   markets and reserve-currency nations capture the coordination gains.
 *   National governments in the periphery lose fiscal and monetary autonomy,
 *   and domestic populations bear the costs of austerity and structural
 *   adjustment. The constraint is a contested reading: its beneficiaries
 *   (international finance, reserve currency issuers) experience a
 *   rules-based order that subsidizes their mobility, while its victims
 *   (debtor governments and domestic populations) experience an external
 *   disciplinary apparatus that removes their policy levers.
 *
 * KEY AGENTS:
 *   - International finance: Primary beneficiary (institutional/arbitrage/global) â gains from liberalized capital flows and market discipline.
 *   - Reserve currency issuers: Secondary beneficiary (institutional/arbitrage/global) â gains seigniorage and asymmetric adjustment privileges.
 *   - Multilateral creditors (IMF/World Bank): Agenda-setter (institutional/arbitrage/global) â administers conditionality and interprets the treaty as mandating openness.
 *   - Debtor nation governments: Primary target (moderate/constrained/national) â loses policy autonomy to retain market access.
 *   - Domestic populations: Secondary target (powerless/trapped/national) â bears austerity and welfare costs without consent.
 *   - Capital control advocates: Excluded voice (moderate/constrained/global) â policy preferences treated as regime violations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.68).
domain_priors:suppression_score(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.72).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__neoliberal_convertibility, "Bretton Woods Neoliberal Convertibility Regime").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__neoliberal_convertibility, "international_political_economy/monetary_history").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__neoliberal_convertibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__neoliberal_convertibility, '6ce61ace-82b3-412c-b60f-f17dc275c1f2').
narrative_ontology:cs_kernel_codification('6ce61ace-82b3-412c-b60f-f17dc275c1f2', fixed_text).
narrative_ontology:cs_authority_grounding('6ce61ace-82b3-412c-b60f-f17dc275c1f2', lineage).
narrative_ontology:cs_interpretation_layer_present('6ce61ace-82b3-412c-b60f-f17dc275c1f2').
narrative_ontology:cs_reading_relation('6ce61ace-82b3-412c-b60f-f17dc275c1f2', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, coexists_with).
narrative_ontology:cs_reading_relation('6ce61ace-82b3-412c-b60f-f17dc275c1f2', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('6ce61ace-82b3-412c-b60f-f17dc275c1f2', foundational, capital_account_liberalization_as_efficiency_imperative).
narrative_ontology:cs_axiom_status(capital_account_liberalization_as_efficiency_imperative, holdable).
narrative_ontology:cs_axiom_grounding('6ce61ace-82b3-412c-b60f-f17dc275c1f2', capital_account_liberalization_as_efficiency_imperative, instrumental).
narrative_ontology:cs_axiom('6ce61ace-82b3-412c-b60f-f17dc275c1f2', foundational, government_intervention_as_distortion).
narrative_ontology:cs_axiom_status(government_intervention_as_distortion, holdable).
narrative_ontology:cs_axiom_grounding('6ce61ace-82b3-412c-b60f-f17dc275c1f2', government_intervention_as_distortion, empirically_contingent).
narrative_ontology:cs_reference_frame('6ce61ace-82b3-412c-b60f-f17dc275c1f2', liberalized_global_capital_order).
narrative_ontology:cs_drift_state('6ce61ace-82b3-412c-b60f-f17dc275c1f2', post_2008_crisis, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('6ce61ace-82b3-412c-b60f-f17dc275c1f2', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, international_finance).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, reserve_currency_issuers).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, debtor_nation_governments).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from unrestricted cross-border capital flows, convertible currencies, and the elimination of capital controls. Gains access to debtor nation assets and policy influence through market discipline. Can exit any national jurisdiction instantly if conditions worsen.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, international_finance, beneficiary,
    institutional, generational, arbitrage, global).

% Derives seigniorage and structural advantage from reserve currency status embedded in the regime. Benefits from persistent demand for their currency as international reserves and from asymmetric adjustment burdens that fall on debtor nations.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, reserve_currency_issuers, beneficiary,
    institutional, generational, arbitrage, global).

% Administers conditionality and surveillance through the IMF and World Bank. Sets the interpretive frame that treats capital account liberalization as normative and capital controls as backward violations. Derives institutional mandate, employment, and operational authority from enforcing the convertibility regime.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, multilateral_creditors, agenda_setter,
    institutional, generational, arbitrage, global).

% Must maintain currency convertibility and open capital accounts to retain market access and avoid punitive spreads or exclusion. Lose fiscal and monetary autonomy; austerity, privatization, and deregulation are imposed as conditions. Formal sovereignty masks operational subordination to international financial benchmarks.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, debtor_nation_governments, payer,
    moderate, biographical, constrained, national).

% Bear the downstream costs of lost policy autonomy: austerity cuts to public services, wage suppression, and reduced development spending. Did not consent to the treaty reinterpretation and lack institutional channels to reverse it. Exit requires migration, which is itself restricted by immigration regimes.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_populations, payer,
    powerless, biographical, trapped, national).

% Keynesian and developmentalist economists who argue for capital controls and managed exchange rates. Their preferred policies are treated as violations of the regime rather than legitimate alternatives. Excluded from IMF policymaking consensus, mainstream economics curricula, and Bretton Woods institutional histories authored by the beneficiary camp.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_control_advocates, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__neoliberal_convertibility, diffuse).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__neoliberal_convertibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a global monetary order with currency convertibility and reduced capital barriers, ostensibly to lower transaction costs, enable cross-border price discovery, and channel savings toward productive investment.
% TRANSFER_FUNCTION: Moves fiscal and monetary policy autonomy from national governments to international financial markets and multilateral institutions, converting domestic policy space into market access rights for mobile capital.
% ABSENT_VOICES: Domestic populations in debtor nations, Keynesian economists defending capital controls, and import-substituting industrialists are structurally excluded; their policy preferences are coded as violations of convertibility rather than legitimate alternatives.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, debtor nations would reimpose capital controls, exchange rates would disconnect from market-liberalization benchmarks, the IMF would lose conditionality leverage, and global capital flows would fragment into regional or national circuits â the post-war monetary architecture would reorganize around policy autonomy rather than convertibility.
% FOUNDING_PROBLEM: Post-war monetary disorder, competitive devaluations, and the collapse of international trade and investment in the 1930s required a cooperative framework to stabilize exchange rates and rebuild liquidity.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (e.g., Helleiner, Ruggie) and archival records from Bretton Woods attest the original design aimed at 'embedded liberalism' â capital controls were built in as legitimate tools. The neoliberal reading's claim that the system was designed to free capital markets is contested by these external sources; the original problem (post-war reconstruction) was solved by the 1960s, yet the institutional shell was repurposed.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__neoliberal_convertibility, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__neoliberal_convertibility, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__neoliberal_convertibility, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) is high because the constraint systematically transfers policy autonomy from national governments to international markets. Suppression (0.72) is high because the regime depends on IMF conditionality, structural adjustment enforcement, and the threat of capital flight and exclusion to maintain compliance. Theater ratio (0.48) is moderate-to-high: the discourse of 'development', 'good governance', and 'poverty reduction' performs substantial maintenance work that masks the market-discipline function. Accessibility collapse (0.75) is high because, after the Washington Consensus, alternatives such as capital controls and autonomous industrial policy were rendered institutionally unthinkable in mainstream policymaking. Resistance (0.60) reflects recurring but fragmented opposition from debtor nations and populist movements, insufficient to reverse the regime. The temporal series shows the neoliberal reading layering onto the treaty substrate between 1970 and 1990, peaking in extractiveness and enforcement around the turn of the millennium, with modest post-2008 retrenchment.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of international finance and reserve currency issuers, the constraint appears as a neutral global infrastructure that reduces risk and enables efficient allocation. From the seat of debtor governments and domestic populations, the same structure operates as an external disciplinary apparatus that removes democratic economic tools. The engine computes this divergence from the structural data: identical institutional facts read as coordination by mobile capital and as extraction by trapped populations.
 *
 * DIRECTIONALITY LOGIC:
 *   International finance and reserve currency issuers are structural beneficiaries: the constraint subsidizes their liquidity and scope, yielding directionality near the beneficiary pole. Multilateral creditors sit near the beneficiary pole as well, though their gain is authority and mandate rather than direct rent. Debtor nation governments are targets: they pay in policy autonomy and bear conditionality costs, with constrained exit keeping directionality near the target pole. Domestic populations are the most trapped target: identity-locked by citizenship and immobility, they sit closest to the full-target end. Capital control advocates are excluded, not coordinated â their exclusion is constitutive of the regime's interpretive closure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â post-war monetary chaos and the need for reconstruction â was solved by the 1960s. The neoliberal reading represents a mandatrophy: the institutional shell (IMF, World Bank, convertibility discourse) persisted after its original coordination function expired, and was reinterpreted to enforce market discipline rather than to coordinate shared recovery. The constraint prevents mislabeling by distinguishing the original embedded-liberalism kernel from this later extraction-heavy reading. Without this decomposition, one might classify the Bretton Woods substrate as a scaffold or rope; the reading-specific analysis reveals it as a tangled rope under the neoliberal interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neoliberal_reading_anachronism,
    'Does the neoliberal convertibility reading accurately describe the Bretton Woods treaty substrate, or is it a retrospective reinterpretation that layers extraction onto an originally cooperative framework?',
    'Archival analysis of negotiating records (White and Keynes drafts) and the evolution of IMF Articles of Agreement; linguistic analysis of whether ''convertibility'' in the original text meant current-account or capital-account openness.',
    'If anachronistic, the constraint''s high extractiveness derives from institutional drift and capture rather than treaty design, mandating reclassification toward piton or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neoliberal_reading_anachronism, conceptual, 'Whether the neoliberal reading is textually faithful or a layered reinterpretation.').

omega_variable(
    sibling_reading_relationship,
    'Can the Keynesian embedded liberalism reading and the neoliberal convertibility reading coexist as live interpretive options within the same institutional framework, or does the ascendance of one foreclose the other?',
    'Legal analysis of IMF Article VI and subsequent amendments; institutional history of whether the IMF board has recognized capital controls as legitimate within the same treaty framework.',
    'If they cannot coexist, the regime is committed to a single partisan reading; if they can, the current dominance of the neoliberal reading is a political achievement, not a textual necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_relationship, conceptual, 'Whether sibling readings are mutually exclusive or competing live options.').

omega_variable(
    conditionality_as_consent,
    'Is IMF conditionality a form of voluntary contractual adhesion or coercive structural enforcement?',
    'Comparative analysis of bargaining power asymmetries, the availability of non-IMF financing alternatives for program countries, and the differential cost of exit.',
    'If coercive, the constraint''s suppression and extractiveness are structurally higher than a voluntary coordination mechanism; if voluntary, the constraint approaches a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_as_consent, empirical, 'Whether enforcement is consensual or coercive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__neoliberal_convertibility, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t0, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bret_tr_t16, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 16, 0.15).
narrative_ontology:measurement(bret_tr_t32, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 32, 0.25).
narrative_ontology:measurement(bret_tr_t48, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 48, 0.45).
narrative_ontology:measurement(bret_tr_t64, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 64, 0.5).
narrative_ontology:measurement(bret_tr_t80, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 80, 0.48).

% Extraction over time
narrative_ontology:measurement(bret_be_t0, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(bret_be_t16, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 16, 0.3).
narrative_ontology:measurement(bret_be_t32, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(bret_be_t48, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 48, 0.72).
narrative_ontology:measurement(bret_be_t64, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 64, 0.7).
narrative_ontology:measurement(bret_be_t80, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 80, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t0, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(bret_su_t16, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 16, 0.25).
narrative_ontology:measurement(bret_su_t32, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(bret_su_t48, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 48, 0.78).
narrative_ontology:measurement(bret_su_t64, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 64, 0.75).
narrative_ontology:measurement(bret_su_t80, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 80, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__neoliberal_convertibility, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
