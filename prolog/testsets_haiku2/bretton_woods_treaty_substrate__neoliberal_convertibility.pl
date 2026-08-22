% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__neoliberal_convertibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_neoliberal_convertibility, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Bretton Woods Convertibility Constraint (Neoliberal Reading)
 *   domain: international_political_economy/monetary_governance
 *
 * SUMMARY:
 *   This constraint embodies the neoliberal reading of the Bretton Woods
 *   framework: the treaty establishes rules that subordinate national
 *   government policy autonomy to the requirements of convertible-currency
 *   capital markets. Under this reading, the Articles of Agreement and
 *   subsequent institutional evolution toward capital account liberalization
 *   function to ensure that governments cannot use capital controls, directed
 *   lending, exchange rate management, or strategic investment to deviate
 *   from paths that international finance capitals prefer. The constraint
 *   benefits capital exporters and international finance; it harms
 *   capital-importing states and their capacity for autonomous development
 *   policy. The constraint is claimed as tangled_rope (genuine coordination
 *   of exchange-rate stability + asymmetric extraction of policy autonomy)
 *   and the metrics reflect substantially extractive, actively enforced
 *   operation through IMF conditionality. This reading contrasts with
 *   keynesian_embedded_liberalism (which emphasizes that Bretton Woods was
 *   designed to PROTECT domestic policy space) and sovereignty_defense (which
 *   emphasizes that constraints are imposed on external discipline to
 *   preserve monetary sovereignty). Each reading fixes the same treaty text
 *   as referent; they diverge on what it is ABOUT and what its operation
 *   DOES.
 *
 * KEY AGENTS:
 *   - International Finance Capital: Benefits from predictable convertibility; collects investment returns and capital repatriation rents
 *   - Capital-Exporting States (US, UK, wealthy industrialized): Beneficiaries; set enforcement rules via IMF voting and capital access
 *   - Capital-Importing States (developing, middle-income): Victims; constrained policy autonomy, subordinated to external discipline
 *   - IMF/World Bank Governance: Agenda-setter; enforces convertibility through conditionality and surveillance
 *   - Labor & Domestic Producers (in capital-importing states): Diffuse victims; harmed by deindustrialization, exchange appreciation, inability of government to manage transitions
 *   - Heterodox Economists & Policy Advocates: Excluded voices; advocates for capital controls and industrial policy are systematically marginalized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.68).
domain_priors:suppression_score(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.72).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__neoliberal_convertibility, "Bretton Woods Convertibility Constraint (Neoliberal Reading)").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__neoliberal_convertibility, "international_political_economy/monetary_governance").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__neoliberal_convertibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__neoliberal_convertibility, '2539bb1a-51c0-4a49-86c9-31afa248f1b5').
narrative_ontology:cs_kernel_codification('2539bb1a-51c0-4a49-86c9-31afa248f1b5', formalized).
narrative_ontology:cs_authority_grounding('2539bb1a-51c0-4a49-86c9-31afa248f1b5', extraction).
narrative_ontology:cs_interpretation_layer_present('2539bb1a-51c0-4a49-86c9-31afa248f1b5').
narrative_ontology:cs_reading_relation('2539bb1a-51c0-4a49-86c9-31afa248f1b5', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, coexists_with).
narrative_ontology:cs_reading_relation('2539bb1a-51c0-4a49-86c9-31afa248f1b5', bretton_woods_treaty_substrate__sovereignty_defense, influences).
narrative_ontology:cs_axiom('2539bb1a-51c0-4a49-86c9-31afa248f1b5', foundational, capital_convertibility_core_objective).
narrative_ontology:cs_axiom_status(capital_convertibility_core_objective, holdable).
narrative_ontology:cs_axiom_grounding('2539bb1a-51c0-4a49-86c9-31afa248f1b5', capital_convertibility_core_objective, instrumental).
narrative_ontology:cs_axiom('2539bb1a-51c0-4a49-86c9-31afa248f1b5', foundational, policy_autonomy_subordinate_to_market_discipline).
narrative_ontology:cs_axiom_status(policy_autonomy_subordinate_to_market_discipline, holdable).
narrative_ontology:cs_axiom_grounding('2539bb1a-51c0-4a49-86c9-31afa248f1b5', policy_autonomy_subordinate_to_market_discipline, empirically_contingent).
narrative_ontology:cs_reference_frame('2539bb1a-51c0-4a49-86c9-31afa248f1b5', progressive_capital_liberalization).
narrative_ontology:cs_drift_state('2539bb1a-51c0-4a49-86c9-31afa248f1b5', contemporary_post_2008_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2539bb1a-51c0-4a49-86c9-31afa248f1b5', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, international_finance_capital).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_exporting_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_importing_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_policy_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, labor_domestic_producers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% International financial institutions and private capital markets benefit from convertibility rules that guarantee capital can move freely across borders and exit any national jurisdiction. The convertibility constraint prevents governments from using capital controls as a policy tool, ensuring predictable return on foreign investment and exit liquidity. Collects rents through investment returns and liquidity premiums.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, international_finance_capital, beneficiary,
    institutional, generational, arbitrage, global).

% Wealthy industrialized nations with large pools of capital benefit from rules that prevent other governments from restricting foreign investment. Their corporations and investment funds gain guaranteed access to markets and assured capital repatriation. They set the enforcement rules through IMF voting structures and veto power over lending conditions.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_exporting_states, beneficiary,
    institutional, generational, arbitrage, global).

% Developing and middle-income nations dependent on foreign capital for growth pay the cost of convertibility rules. They cannot use capital controls to manage currency crises, protect nascent industries, or retain savings during external shocks. Their policy autonomy is constrained by the requirement to keep their currencies convertible and borders open to capital flows. Conditionality attached to IMF/World Bank lending enforces compliance.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_importing_states, payer,
    moderate, generational, constrained, national).

% The capacity of national governments to use capital controls, directed credit, exchange rate management, and sectoral investment strategy as tools for development and stabilization is directly subordinated to convertibility requirements. This is not an agent but the analytical object—the thing that bears the cost of the constraint. Governments that attempt to exercise this autonomy face capital flight, currency crisis, and IMF intervention.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_policy_autonomy, payer,
    analytical, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_policy_autonomy).

% The IMF and World Bank enforce convertibility through lending conditions (structural adjustment programs), surveillance, and veto power over capital account liberalization sequencing. They adjudicate what counts as a permissible constraint on capital flows and condition development financing on acceptance of convertibility rules. They are the administered enforcement apparatus.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, imf_world_bank_governance, agenda_setter,
    institutional, generational, analytical, global).

% Workers and domestic firms in capital-importing states bear diffuse costs: exchange rate appreciation from capital inflows, deindustrialization from import competition, inability of government to protect infant industries or manage sectoral transitions. They are diffusely harmed by constraints they did not consent to and cannot exit.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, labor_domestic_producers, payer,
    powerless, biographical, trapped, national).

% Development economists, structuralists, and heterodox theorists who advocate capital controls, directed credit, and industrial policy are systematically excluded from IMF governance and policy debate. Their arguments that convertibility constraints harm development are treated as technically unsound rather than as legitimate policy positions. Professional advancement in mainstream institutions requires accepting the convertibility framework.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, heterodox_economists, excluded,
    moderate, biographical, constrained, global).

% The original Articles of Agreement (Article VI, transitional provisions, Article XIX revision procedures) are the written kernel that grounds the interpretive dispute. This reading treats the treaty as requiring progressive capital account liberalization and convertibility as a core objective; sibling readings emphasize transitional safeguards and embedded liberalism. The text itself is not an actor but the basis of the contested authority.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_text, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_text).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__neoliberal_convertibility, international_finance_capital).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__neoliberal_convertibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Convertibility rules solve the problem of predictable, universal currency exchange: traders and investors know they can move capital across borders and convert currencies at stable rates, enabling integrated global capital markets and reducing transaction costs for international commerce.
% TRANSFER_FUNCTION: Moves policy autonomy from national governments to international financial markets and institutions. Transfers the capacity to use capital controls, directed credit, and exchange rate management from national policy toolkits to the IMF's conditional lending regime. Moves investment returns and capital flows from capital-importing states to capital exporters and their investors.
% ABSENT_VOICES: Labor unions and domestic producer associations in capital-importing states are structurally excluded from governance; they would argue that convertibility constraints harm development and worker welfare. Heterodox economists and development theorists advocating alternative frameworks are excluded from policy debate. Debtor-country governments and their citizens have limited formal voice in IMF decision-making (weighted voting favors capital exporters).
% DISAPPEARANCE_RATIONALE: If convertibility constraints disappeared, capital-importing states would immediately re-implement capital controls, directed lending, and managed exchange rates. Global capital markets would fragment along national and regional lines. Development strategies would shift from export-led growth models toward domestic-market and industrial-policy approaches. The distribution of policy autonomy, investment returns, and developmental capacity would fundamentally reorganize.
% FOUNDING_PROBLEM: Post-WWII, the global economy needed to rebuild without reverting to the protectionist, competitive devaluations, and capital hoarding that characterized the 1930s. Capital account convertibility was designed to create a stable framework for post-war reconstruction and trade. The constraint was built to prevent competitive currency manipulation and enable coordinated recovery.
% FOUNDING_PROBLEM_CORROBORATION: Capital exporters and international finance institutions attest the founding problem remains live—without convertibility discipline, governments would revert to capital controls and competitive devaluation, destabilizing global finance. Development economists, debtor-country policymakers, and heterodox theorists attest the founding problem is solved (post-war reconstruction is complete) and convertibility now serves rent extraction rather than stability. Legislative hearings in developing countries and academic critiques from outside capital-exporting institutions support the shifted-function reading.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__neoliberal_convertibility, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__neoliberal_convertibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__neoliberal_convertibility, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68) and rising over the interval (from 0.35 at Bretton Woods 1944 to 0.68 by 2024) because the constraint's function has shifted: early Bretton Woods balanced post-war reconstruction with some protection for national policy (the transition provisions, Article XIX revision clauses); by the 1980s-2000s, IMF conditionality and capital account liberalization ideology had locked in permanent constraints on policy autonomy. Suppression is high (0.72) because enforcement is active: governments that violate convertibility rules face capital flight, currency crises, and IMF intervention; the threat is credible and frequently actualized. Theater is moderate (0.42) and rising: the rhetoric of 'development assistance' and 'financial stability' masks the policy subordination, but a growing share of visible IMF activity is defending convertibility rather than addressing genuine coordination problems. Accessibility collapse is moderate (0.58) because alternatives (capital controls, directed credit, managed exchange rates) remain theoretically available and are occasionally practiced (China's managed capital account, India's sectoral protections), but the cost of doing so (capital flight, loss of IMF access, market discipline) is so high that the constraint appears structurally inevitable. Resistance is high (0.71) and persistent: debtor-country governments repeatedly resist conditionality; developing-country coalitions advocate alternative frameworks; but resistance has not broken the constraint because capital exporters and the IMF apparatus maintain unified enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of international finance and capital-exporting states, this is genuine coordination: creating a stable, predictable global currency and capital system from which all benefit. From the seat of a capital-importing state like Brazil or Indonesia, the same structure is enforced extraction: the government's capacity to manage its own development is subordinated to the preferences of foreign investors and IMF staff. From the labor/domestic-producer seat in a capital-importing state, the constraint is pure harm: policy tools that could protect jobs and local industry are forbidden. The engine computes these divergences from the structural data (who benefits, who is constrained, what exit options each has) rather than from any claimed type.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for capital exporters is near beneficiary (d ≈ 0.15): they collect from the constraint, have arbitrage-level exit (if one market closes, capital moves to another), and sit at institutional power. Directionality for capital-importing states is near target (d ≈ 0.85): they pay through policy subordination, have constrained exit (violating convertibility brings IMF/market punishment), and sit at moderate institutional power. Directionality for IMF governance is near beneficiary (d ≈ 0.20): it administers the constraint and collects institutional authority and lending fees, though technically subject to capital-exporting-state veto. Directionality for labor is deeply target (d ≈ 0.95): powerless, trapped exit, bearing diffuse costs. No override needed; the derivation chain (beneficiary/victim declarations + power + exit) produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does show mandatrophy features: the founding problem (preventing competitive devaluation and beggar-thy-neighbor policies of the 1930s) was solved by the 1960s; the constraint persists not because it solves an ongoing coordination problem but because it serves the extractive interest of capital exporters and international finance. The theater_ratio rising from 0.15 to 0.42 signals performative maintenance: IMF staff invoke 'financial stability' and 'development' rationales for conditionality that primarily defends convertibility and capital mobility. The classification as tangled_rope rather than snare reflects that some coordination function (unified exchange-rate framework, reduction in transaction costs for international trade) is real; but the dominant function (preventing policy autonomy in debtor states) is extractive. If the coordination had fully atrophied, the type would be piton; the remaining, attenuated coordination function prevents that classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordinate_vs_extract_function_shift,
    'Did the Bretton Woods framework''s primary function shift from post-war reconstruction coordination (1944-1960s) to capital-market extraction (1980s-present), or did it always embody both?',
    'Historical analysis of IMF conditionality patterns, voting-record changes, and stated policy rationales across decades. Comparison of early-period lending conditions (infrastructure-focused, tolerant of capital controls) versus late-period conditions (capital-account liberalization, privatization, fiscal austerity).',
    'If function shifted, the constraint transitions from tangled_rope (mixed coordination and extraction) to snare (pure extraction with coordination cover). If always both, the tangled_rope classification holds throughout. This determines whether mandatrophy emerges as the founding problem outlives its solution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordinate_vs_extract_function_shift, empirical, 'Whether Bretton Woods function changed from coordination to extraction over its lifecycle.').

omega_variable(
    necessity_of_convertibility_for_trade,
    'Is universal convertibility structurally necessary for stable international trade and finance, or can trade and investment occur with managed capital accounts and regional currency arrangements?',
    'Natural experiments: the success of regional trade blocs (ASEAN, Mercosur) and managed capital-account regimes (China''s Belt and Road, India''s development model) in achieving growth and trade integration despite not conforming to full convertibility.',
    'If convertibility is necessary, the constraint solves a genuine coordination problem and justifies tangled_rope. If trade flourishes without it, the constraint is pure extraction defending a particular financial architecture rather than a necessary function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_of_convertibility_for_trade, empirical, 'Whether capital-account convertibility is necessary for international commerce or contingent on institutional preference.').

omega_variable(
    reading_interpretive_authority,
    'Within the same treaty text, is the neoliberal reading (capital liberalization as core objective) or the keynesian reading (domestic policy protection as core objective) the more coherent interpretation of the Articles of Agreement and their legislative history?',
    'Textual analysis of the Articles, contemporaneous legislative records from the 1944 Bretton Woods Conference, statements by Keynes and White (the framers), and analysis of what amendments were adopted versus what were proposed.',
    'A finding that the keynesian reading is more coherent with the treaty''s original text and intent would support reclassification of this constraint from the neoliberal reading to a different category or omega-variable foregrounding interpretive contestation. This is a reading_relations question: does the neoliberal reading FORECLOSE the keynesian reading (they cannot coexist in one coherent framework), or do they COEXIST (different parties hold both, both are live options)?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_interpretive_authority, conceptual, 'Whether the neoliberal and keynesian readings are logically foreclosed or interpretively coexistent.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.72) of capital-importing-state policy autonomy primarily structural (external punishment for violation: capital flight, IMF intervention) or internalized (governments believe convertibility is necessary and desirable)?',
    'Comparison of governments that maintain capital controls despite IMF pressure (Malaysia 1997, China ongoing, India sectoral) versus those that liberalize and report satisfaction. Post-exit trajectories: do debtor-country governments that exit the IMF/convertibility regime restore capital controls, suggesting suppression was structural?',
    'If suppression is primarily structural, the constraint''s effective suppression is as authored (0.72), and it should remain classified as extractive. If suppression is substantially internalized, the constraint may be more culturally embedded than the structural reading suggests, affecting omega uncertainty about whether alternatives are genuinely available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of policy autonomy is imposed externally or accepted as legitimate by target states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__neoliberal_convertibility, 1944, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1944, 0.15).
narrative_ontology:measurement_basis(bret_tr_t1944, observed).
narrative_ontology:measurement(bret_tr_t1960, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1960, 0.22).
narrative_ontology:measurement_basis(bret_tr_t1960, observed).
narrative_ontology:measurement(bret_tr_t1973, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1973, 0.28).
narrative_ontology:measurement_basis(bret_tr_t1973, observed).
narrative_ontology:measurement(bret_tr_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1985, 0.35).
narrative_ontology:measurement_basis(bret_tr_t1985, observed).
narrative_ontology:measurement(bret_tr_t2000, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2000, 0.4).
narrative_ontology:measurement_basis(bret_tr_t2000, observed).
narrative_ontology:measurement(bret_tr_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(bret_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1944, 0.35).
narrative_ontology:measurement_basis(bret_be_t1944, observed).
narrative_ontology:measurement(bret_be_t1960, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1960, 0.42).
narrative_ontology:measurement_basis(bret_be_t1960, observed).
narrative_ontology:measurement(bret_be_t1973, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1973, 0.51).
narrative_ontology:measurement_basis(bret_be_t1973, observed).
narrative_ontology:measurement(bret_be_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement_basis(bret_be_t1985, observed).
narrative_ontology:measurement(bret_be_t2000, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement_basis(bret_be_t2000, observed).
narrative_ontology:measurement(bret_be_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(bret_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1944, 0.48).
narrative_ontology:measurement_basis(bret_su_t1944, observed).
narrative_ontology:measurement(bret_su_t1960, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement_basis(bret_su_t1960, observed).
narrative_ontology:measurement(bret_su_t1973, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1973, 0.62).
narrative_ontology:measurement_basis(bret_su_t1973, observed).
narrative_ontology:measurement(bret_su_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1985, 0.68).
narrative_ontology:measurement_basis(bret_su_t1985, observed).
narrative_ontology:measurement(bret_su_t2000, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement_basis(bret_su_t2000, observed).
narrative_ontology:measurement(bret_su_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2024, 0.72).
narrative_ontology:measurement_basis(bret_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__neoliberal_convertibility, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.12).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate__keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate__sovereignty_defense).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, imf_conditionality_lending_regime).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_flight_currency_discipline).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, development_state_policy_constraints).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel bretton_woods_treaty_substrate. The same treaty text generates different constraints depending on which clauses and historical interpretations are centered: this neoliberal reading emphasizes capital liberalization; the keynesian reading emphasizes domestic policy protection; the sovereignty reading emphasizes external discipline. All three have the same referent (Bretton Woods, 1944); they differ in what function the constraint performs and who benefits. Each has its own ε, beneficiary/victim structure, and type. They are linked via network.affects_constraints because they are readings of the same kernel and each reading's interpretive success influences whether the others remain live options.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bretton_woods_treaty_substrate__neoliberal_convertibility, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
