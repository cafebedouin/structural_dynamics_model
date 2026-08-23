% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__neoliberal_convertibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   human_readable: Bretton Woods Neoliberal Convertibility Reading
 *   domain: international_political_economy
 *
 * SUMMARY:
 *   The Bretton Woods treaty substrate is read here through the neoliberal
 *   convertibility lens: the Articles of Agreement and subsequent IMF
 *   practice are interpreted as creating binding constraints on national
 *   macroeconomic interventionâparticularly capital controls and activist
 *   monetary policyâin order to secure free international capital markets.
 *   Under this reading, the IMF's surveillance and conditionality apparatus
 *   enforces government discipline, treating policy autonomy as a source of
 *   distortion rather than a legitimate public good. The constraint
 *   coordinates global capital allocation but asymmetrically extracts policy
 *   space from debtor and peripheral governments while transnational finance
 *   captures the mobility premium. This is a kernel reading: the same treaty
 *   substrate supports rival interpretations (embedded liberalism,
 *   sovereignty defense) with diametrically opposed structural mappings.
 *
 * KEY AGENTS:
 *   - imf: Agenda-setter (institutional/global/constrained) â interprets and enforces treaty obligations as capital-account liberalization.
 *   - transnational_finance: Beneficiary (powerful/global/arbitrage) â collects mobility rents and arbitrage gains from unconstrained flows.
 *   - debtor_governments: Payer (moderate/national/constrained) â surrender capital controls and fiscal tools to maintain market access and IMF eligibility.
 *   - domestic_workers: Payer (powerless/national/trapped) â absorb austerity and wage compression under adjustment programs.
 *   - heterodox_economists: Excluded (organized/global/mobile) â advocate capital controls but are marginalized from institutional policy design.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.72).
domain_priors:suppression_score(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.78).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, extractiveness, 0.72).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__neoliberal_convertibility, "Bretton Woods Neoliberal Convertibility Reading").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__neoliberal_convertibility, "international_political_economy").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__neoliberal_convertibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__neoliberal_convertibility, '0512150b-d00e-4a3c-b503-111951a5b92c').
narrative_ontology:cs_kernel_codification('0512150b-d00e-4a3c-b503-111951a5b92c', formalized).
narrative_ontology:cs_authority_grounding('0512150b-d00e-4a3c-b503-111951a5b92c', lineage).
narrative_ontology:cs_interpretation_layer_present('0512150b-d00e-4a3c-b503-111951a5b92c').
narrative_ontology:cs_reading_relation('0512150b-d00e-4a3c-b503-111951a5b92c', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, forecloses).
narrative_ontology:cs_reading_relation('0512150b-d00e-4a3c-b503-111951a5b92c', bretton_woods_treaty_substrate__sovereignty_defense, forecloses).
narrative_ontology:cs_axiom('0512150b-d00e-4a3c-b503-111951a5b92c', foundational, capital_account_liberalization_as_norm).
narrative_ontology:cs_axiom_status(capital_account_liberalization_as_norm, holdable).
narrative_ontology:cs_axiom_grounding('0512150b-d00e-4a3c-b503-111951a5b92c', capital_account_liberalization_as_norm, conventional).
narrative_ontology:cs_axiom('0512150b-d00e-4a3c-b503-111951a5b92c', foundational, domestic_intervention_as_distortion).
narrative_ontology:cs_axiom_status(domestic_intervention_as_distortion, holdable).
narrative_ontology:cs_axiom_grounding('0512150b-d00e-4a3c-b503-111951a5b92c', domestic_intervention_as_distortion, instrumental).
narrative_ontology:cs_reference_frame('0512150b-d00e-4a3c-b503-111951a5b92c', liberalized_capital_order).
narrative_ontology:cs_drift_state('0512150b-d00e-4a3c-b503-111951a5b92c', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0512150b-d00e-4a3c-b503-111951a5b92c', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, transnational_finance).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, debtor_governments).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers Article VIII surveillance and lending conditionality, interpreting the treaty as mandating capital-account liberalization and fiscal restraint. Its institutional survival and budget depend on continued demand for enforcement and technical assistance.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, imf, agenda_setter,
    institutional, generational, constrained, global).

% Deploys capital across borders without regulatory segmentation, capturing arbitrage spreads and liquidity premiums. Lobbies for continued openness and opposes sovereign capital controls that would segment markets.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, transnational_finance, beneficiary,
    powerful, biographical, arbitrage, global).

% Must maintain IMF compliance and market confidence to retain borrowing access, which requires surrendering capital controls, activist industrial policy, and counter-cyclical fiscal tools. Default is possible but carries catastrophic exclusion costs.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, debtor_governments, payer,
    moderate, biographical, constrained, national).

% Bear the downstream costs of austerity and structural adjustment: wage compression, public-sector retrenchment, and reduced social insurance when governments lose macroeconomic stabilization tools. Cannot exit the national labor market without prohibitive cost.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_workers, payer,
    powerless, biographical, trapped, national).

% Produce research demonstrating the viability of capital controls and developmental states, but are structurally excluded from IMF Article IV consultation rooms, program design teams, and mainstream policy journals that set the discourse.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, heterodox_economists, excluded,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__neoliberal_convertibility, transnational_finance).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__neoliberal_convertibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a multilateral payments framework intended to reduce currency risk and enable cross-border capital allocation by harmonizing monetary rules among signatories.
% TRANSFER_FUNCTION: Moves macroeconomic policy autonomy and crisis-buffering capacity from debtor governments and domestic populations to transnational investors and creditor institutions, by constraining the tools governments may use to manage external shocks.
% ABSENT_VOICES: Keynesian and developmentalist economists who would argue for capital controls, counter-cyclical fiscal policy, and autonomous industrial strategy are excluded from the institutional rooms where conditionality is designed.
% DISAPPEARANCE_RATIONALE: If the constraint on government intervention vanished, debtor governments would reimpose capital controls, monetary autonomy would return to national treasuries, transnational finance would face segmented national markets, and the IMF's surveillance and conditionality apparatus would lose its primary lever.
% FOUNDING_PROBLEM: Prevent the competitive devaluations, protectionist bilateralism, and payments chaos of the 1930s by creating a multilateral framework for stable exchange rates and orderly international settlements.
% FOUNDING_PROBLEM_CORROBORATION: The neoliberal reading is primarily attested by IMF management and advanced-economy finance ministries from the 1980s onward. Original treaty architects and Keynesian historians from outside the contemporary beneficiary set attest that the founding problem was embedded liberalismâprotecting domestic policy spaceânot capital mobility. Corroboration for THIS reading's founding claim from outside the benefiting parties is weak.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__neoliberal_convertibility, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__neoliberal_convertibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__neoliberal_convertibility, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constraint systematically strips policy tools from governments during crises, transferring adjustment costs to domestic populations. Suppression (0.78) is higher still because IMF conditionality, surveillance, and market-discipline mechanisms actively suppress alternatives such as capital controls and developmental states. Theater ratio (0.40) is moderate: a growing share of conditionality is performative (structural adjustment as credibility signaling to markets) rather than functionally tied to payments balance. Accessibility collapse (0.75) is high because the norm of liberalization has made capital controls unthinkable for policy elites even after empirical challenges. Resistance (0.60) reflects periodic debtor defaults and anti-austerity movements, but remains insufficient to unwind the constraint. All metrics are authored on a shared time grid to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   From the IMF seat, the constraint is a necessary coordination mechanism preventing the beggar-thy-neighbor policies of the 1930s. From the debtor government and worker seats, it is an externally enforced stripping of macroeconomic tools that transfers adjustment costs downward. The engine computes this divergence: the agenda-setter seat may compute toward rope or tangled rope, while the powerless trapped payer seat computes strongly toward snare.
 *
 * DIRECTIONALITY LOGIC:
 *   The IMF sits near symmetric but slightly toward beneficiary as the administrator extracting institutional relevance from the arrangement. Transnational finance is a clear beneficiary with mobile, globally scoped exit, yielding low directionality and damped effective extraction. Debtor governments are targeted payers with constrained exit and national scope, yielding high directionality and amplified extraction. Domestic workers are the deepest targets: powerless, trapped, and nationally scoped, so their effective extraction is maximized. Heterodox economists are excluded from the cost/benefit flow and function as analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The neoliberal reading risks mislabeling the constraint as pure coordination (rope) because it genuinely enables cross-border investment flows. However, the presence of identifiable victimsâdebtor governments stripped of capital controls, domestic workers absorbing austerityâestablishes asymmetric extraction. Active enforcement via conditionality prevents exit, distinguishing it from a spontaneous market equilibrium. The founding problem (1930s disorder) is contested: the neoliberal reading claims continuity, but historical corroboration from the original architects supports embedded liberalism, suggesting the arrangement may persist to serve a different function than the one it was built for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the Bretton Woods treaty substrate structurally encode capital-account liberalization as a binding norm, or is the neoliberal convertibility reading an ex post interpretive graft onto a kernel originally designed for embedded liberalism?',
    'Archival analysis of original treaty negotiation records (White vs. Keynes drafts, Article VIII vs. Article XIV distinctions) and subsequent amendment history (1976 Jamaica, 1990s Article VIII push).',
    'If the kernel was originally embedded-liberal, this reading is a false summit or extraction-through-interpretation; if the kernel genuinely encoded liberalization, the reading is closer to a rope or mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the neoliberal reading is inscribed in the kernel or projected onto it.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of capital controls and interventionist policy structural (IMF conditionality, market discipline) or internalized (policy elites believe liberalization is economically virtuous regardless of external enforcement)?',
    'Policy trajectory analysis after sovereign defaults or IMF program exits: do governments reimpose controls when external enforcement lapses, or do they maintain liberalization because of internalized belief?',
    'If internalized, effective suppression exceeds structural measures and the constraint operates more like cognitive capture; if purely structural, it is a conventional enforcement mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of interventionist policy.').

omega_variable(
    coordination_extraction_boundary,
    'Does the constraint''s coordination of international capital flows represent a genuine collective-action solution to payments imbalance, or does the coordination function serve primarily to legitimize extraction of policy space?',
    'Counterfactual analysis of growth and stability outcomes under closed capital accounts versus open accounts for structurally similar economies.',
    'If open accounts produce no superior coordination outcome, the constraint is largely extractionary; if they do, the tangled-rope classification is sustained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the coordination function is genuine or cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__neoliberal_convertibility, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t0, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bret_tr_t10, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 10, 0.12).
narrative_ontology:measurement(bret_tr_t20, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 20, 0.18).
narrative_ontology:measurement(bret_tr_t30, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 30, 0.25).
narrative_ontology:measurement(bret_tr_t40, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 40, 0.3).
narrative_ontology:measurement(bret_tr_t50, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 50, 0.34).
narrative_ontology:measurement(bret_tr_t60, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 60, 0.37).
narrative_ontology:measurement(bret_tr_t70, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 70, 0.39).
narrative_ontology:measurement(bret_tr_t80, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 80, 0.4).

% Extraction over time
narrative_ontology:measurement(bret_be_t0, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(bret_be_t10, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(bret_be_t20, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(bret_be_t30, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(bret_be_t40, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(bret_be_t50, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 50, 0.66).
narrative_ontology:measurement(bret_be_t60, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(bret_be_t70, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 70, 0.71).
narrative_ontology:measurement(bret_be_t80, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 80, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t0, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(bret_su_t10, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(bret_su_t20, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(bret_su_t30, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(bret_su_t40, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(bret_su_t50, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 50, 0.75).
narrative_ontology:measurement(bret_su_t60, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 60, 0.77).
narrative_ontology:measurement(bret_su_t70, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 70, 0.78).
narrative_ontology:measurement(bret_su_t80, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 80, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__neoliberal_convertibility, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
