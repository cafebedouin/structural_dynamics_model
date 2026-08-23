% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__neoliberal_convertibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Neoliberal Convertibility Reading of Bretton Woods Treaty Substrate
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This constraint story captures the neoliberal_convertibility reading of
 *   the Bretton Woods treaty substrate: the claim that the treaty's core
 *   logic constrains government intervention to enable free capital markets.
 *   Historically, the 1944 Articles of Agreement permitted capital controls
 *   (Article VI) and envisioned embedded liberalism (Keynes/White). The
 *   neoliberal reading emerges through institutional reinterpretation — the
 *   1978 Second Amendment, the 1990s capital account liberalization push, and
 *   IMF conditionality — converting a coordination framework for current
 *   account convertibility into a constraint on capital account management.
 *   The structural delta: national policy autonomy enters the victim set;
 *   international finance becomes the primary beneficiary; capital controls
 *   are redefined from legitimate tools to treaty violations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.72).
domain_priors:suppression_score(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.68).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, extractiveness, 0.72).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__neoliberal_convertibility, "Neoliberal Convertibility Reading of Bretton Woods Treaty Substrate").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__neoliberal_convertibility, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__neoliberal_convertibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__neoliberal_convertibility, '1fe5febc-8398-40fb-adee-58ad56d4a8c2').
narrative_ontology:cs_kernel_codification('1fe5febc-8398-40fb-adee-58ad56d4a8c2', formalized).
narrative_ontology:cs_authority_grounding('1fe5febc-8398-40fb-adee-58ad56d4a8c2', lineage).
narrative_ontology:cs_interpretation_layer_present('1fe5febc-8398-40fb-adee-58ad56d4a8c2').
narrative_ontology:cs_reading_relation('1fe5febc-8398-40fb-adee-58ad56d4a8c2', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, coexists_with).
narrative_ontology:cs_reading_relation('1fe5febc-8398-40fb-adee-58ad56d4a8c2', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('1fe5febc-8398-40fb-adee-58ad56d4a8c2', foundational, capital_mobility_as_freedom).
narrative_ontology:cs_axiom_status(capital_mobility_as_freedom, holdable).
narrative_ontology:cs_axiom_grounding('1fe5febc-8398-40fb-adee-58ad56d4a8c2', capital_mobility_as_freedom, deontological).
narrative_ontology:cs_axiom('1fe5febc-8398-40fb-adee-58ad56d4a8c2', foundational, capital_controls_as_market_distortion).
narrative_ontology:cs_axiom_status(capital_controls_as_market_distortion, holdable).
narrative_ontology:cs_axiom_grounding('1fe5febc-8398-40fb-adee-58ad56d4a8c2', capital_controls_as_market_distortion, empirically_contingent).
narrative_ontology:cs_axiom('1fe5febc-8398-40fb-adee-58ad56d4a8c2', secondary, market_discipline_improves_policy_outcomes).
narrative_ontology:cs_axiom_status(market_discipline_improves_policy_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('1fe5febc-8398-40fb-adee-58ad56d4a8c2', market_discipline_improves_policy_outcomes, instrumental).
narrative_ontology:cs_reference_frame('1fe5febc-8398-40fb-adee-58ad56d4a8c2', bretton_woods_original_design).
narrative_ontology:cs_drift_state('1fe5febc-8398-40fb-adee-58ad56d4a8c2', neoliberal_turn_1970s_80s, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1fe5febc-8398-40fb-adee-58ad56d4a8c2', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, international_finance).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, global_capital_markets).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, multinational_banks).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, national_governments).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_policy_autonomy).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_country_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_populations).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_mobility_as_efficiency).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__neoliberal_convertibility, market_discipline_over_political_discretion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Global financial institutions and capital markets gain unrestricted cross-border movement, reduced transaction costs, and the ability to discipline national policies through capital flight. They capture the gains from convertibility while externalizing crisis costs to states.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, international_finance, beneficiary,
    institutional, generational, arbitrage, global).

% Deepening and integration of global capital markets is the structural outcome; market infrastructure providers (clearing, settlement, rating agencies) expand their domain. They shape the rules through standard-setting bodies and lobbying.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, global_capital_markets, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__neoliberal_convertibility, global_capital_markets, agenda_setter).

% Large commercial and investment banks operate across borders with minimal regulatory friction, accessing cheaper funding and broader asset pools. Their business models depend on the convertibility regime.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, multinational_banks, beneficiary,
    powerful, biographical, mobile, global).

% Governments lose autonomous control over capital accounts, exchange rates, and domestic monetary policy. Capital controls are treated as treaty violations; policy space is constrained by market discipline and IMF conditionality. Exit means leaving the global financial system — prohibitively costly for most.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, national_governments, payer,
    institutional, biographical, constrained, national).

% The capacity of democratic polities to set economic policy — capital controls, industrial policy, full employment targeting, financial regulation — is structurally eroded. This is not an actor but a capability that the constraint extracts from the polity.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_policy_autonomy, payer,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_policy_autonomy).

% Face asymmetric enforcement: capital account liberalization demanded as condition for crisis lending, while reserve currency issuers retain policy space. Structural adjustment programs enforce the reading's logic. Exit is blocked by debt dependence and market access needs.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_country_governments, payer,
    moderate, biographical, trapped, regional).

% The IMF evolves from Bretton Woods' fixed-rate guardian to the enforcement arm of capital account liberalization. Article IV consultations and conditionality operationalize the neoliberal reading. It administers the constraint and could change its interpretation but is incentivized to maintain it.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, imf_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Workers, farmers, and vulnerable groups bear adjustment costs (austerity, devaluation, privatization) when capital flight disciplines policy. They have no voice in treaty interpretation or IMF governance. Their opposition is channeled through domestic politics that the constraint structures.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_populations, excluded,
    organized, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_populations, payer).

% Produce the theoretical frameworks (efficient markets, original sin, trilemma) that legitimize or contest the reading. Their models become policy instruments. They observe the full structure but their influence is mediated through institutional adoption.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, academic_economists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, rule-based framework for cross-border payments and investment by committing states to current account convertibility and fixed (later managed) exchange rates, reducing transaction uncertainty for trade and long-term capital flows.
% TRANSFER_FUNCTION: Transfers policy autonomy — specifically the right to impose capital controls, set independent monetary policy, and manage the capital account — from national governments to international financial markets. The gains (seigniorage from reserve status, seigniorage from financial deepening) accrue to financial centers and reserve currency issuers.
% ABSENT_VOICES: Post-colonial states at the 1944 conference (India, Philippines, etc.) were present but structurally marginalized; labor movements and peasant organizations were excluded entirely. Later, UNCTAD and the Group of 77 articulated the developing country critique but were excluded from IMF governance reform. Their objection: the system encodes core-periphery asymmetry.
% DISAPPEARANCE_RATIONALE: If the neoliberal convertibility constraint vanished overnight, capital controls would be re-legitimized as policy tools, IMF conditionality would lose its liberalization mandate, exchange rate regimes would diversify, and the global financial safety net would need redesign. The 2008 and 2020 crises showed that even partial suspension (swap lines, capital flow management measures) triggers immediate rearrangement.
% FOUNDING_PROBLEM: Post-war monetary chaos: competitive devaluations, trade collapse, balance-of-payments crises, and the absence of a multilateral payments system. The founding problem was coordination — how to restore stable exchange rates and current account convertibility without reproducing the gold standard's deflationary bias.
% FOUNDING_PROBLEM_CORROBORATION: Keynes and White's archival records confirm the embedded liberalism intent (capital controls permitted, policy autonomy protected). The Triffin dilemma (1960) and the 1971 Nixon shock corroborate that the founding problem (fixed rates with convertibility) was structurally unstable. Developing country testimonies at UNCTAD (1964-present) and the 1997 Asian Financial Crisis post-mortems corroborate that the neoliberal reading transformed the solution into a new extraction mechanism. No independent corroboration supports the claim that capital account liberalization was the original founding problem.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__neoliberal_convertibility, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__neoliberal_convertibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__neoliberal_convertibility, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constraint transfers substantial policy autonomy (capital account, monetary sovereignty, crisis management tools) from states to markets, with crisis costs socialized. Suppression (0.68) is substantial: capital controls are delegitimized, IMF conditionality enforces liberalization, and market discipline operates as structural coercion. Theater ratio (0.45) reflects that the coordination function (payments stability) is real but increasingly decoupled from the capital account liberalization that generates extraction. The measurement series shows the neoliberal turn (post-1971, accelerating post-1980) as extraction accumulation and enforcement intensification — the constraint's type shifts from rope (embedded liberalism era) to tangled_rope (neoliberal era).
 *
 * PERSPECTIVAL GAP:
 *   From the IMF/financial center seat, the constraint appears as rope — genuine coordination solving the payments problem. From the developing country government seat, it appears as snare — asymmetric extraction with suppressed exits. From the analytical seat, the type transition (rope → tangled_rope) is visible as institutional drift. The engine computes this seat divergence from the structural data; the claimed_type (tangled_rope) represents the analytical seat's judgment that the extraction component is now structural, not incidental.
 *
 * DIRECTIONALITY LOGIC:
 *   International finance and global capital markets are structural beneficiaries (d near 0): they gain mobility, discipline states, capture seigniorage. National governments are targets (d near 1): they lose policy tools, face market discipline, bear crisis costs. Developing country governments are more intensely targeted (d → 1) due to trapped exit and asymmetric enforcement. IMF institutions are agenda_setters with analytical exit but institutional incentives align them with the reading. Domestic populations are excluded payers — they bear costs without voice. The directionality derivation from beneficiary/victim declarations plus exit options produces this gradient; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war monetary coordination) was real and the original design (embedded liberalism) solved it. The neoliberal reading constitutes mandatrophy: the mandate (stable convertible system) has been reinterpreted to serve a different function (capital mobility as discipline). The constraint persists because the agenda_setter (IMF/financial centers) benefits from the reinterpretation, while the payers (national governments, especially developing) lack coordinated exit. The founding_problem_status = contested captures this: the original problem is solved, but the arrangement persists in a form that extracts from its putative beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the Bretton Woods kernel, or does it describe the kernel itself?',
    'Comparative analysis of the three declared readings'' structural profiles (beneficiary/victim sets, enforcement mechanisms, coordination claims). If each reading produces a stable, distinct constraint classification with non-overlapping ε-invariant profiles, the kernel decomposition is validated.',
    'If validated, the neoliberal_convertibility constraint is one of three in a constraint family linked by network.affects_constraints. If not, the kernel frame is a category error and this story should be re-authored as a flat constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel/reading decomposition correctly models the Bretton Woods interpretive contest.').

omega_variable(
    structural_delta_from_siblings,
    'Does the declared structural delta (national policy autonomy → victim; international finance → beneficiary; capital controls → violations) accurately distinguish this reading from its siblings?',
    'Map each sibling''s beneficiary/victim declarations and enforcement logic. The keynesian reading should have finance as constrained, policy autonomy as beneficiary. The sovereignty reading should have external discipline as victim, national sovereignty as beneficiary. Verify non-overlap.',
    'If deltas overlap, the readings are not structurally distinct and the kernel frame collapses. If distinct, the family structure holds and cross-reading contamination analysis becomes possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_delta_from_siblings, conceptual, 'Whether the three readings'' structural profiles are genuinely non-overlapping.').

omega_variable(
    coordination_extraction_boundary,
    'Is the coordination function (payments stability, trade facilitation) genuinely separable from the extraction function (capital account liberalization, policy discipline), or are they inextricably fused?',
    'Historical counterfactual: did the 1944-1971 embedded liberalism era deliver coordination without the neoliberal extraction? Post-1971, did coordination degrade as extraction rose? The measurement series (extractiveness rising, theater rising) suggests fusion — but the 1944-1971 period had low extractiveness and functional coordination.',
    'If separable, the tangled_rope classification is correct (coordination + extraction). If fused, the constraint may be a snare with coordination as cover. The 1944-1971 low-extraction period is evidence for separability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the constraint''s coordination and extraction components are structurally separable or fused.').

omega_variable(
    imf_institutional_incentive_structure,
    'Does the IMF''s role as agenda_setter reflect genuine coordination necessity, or has its institutional survival become dependent on enforcing the neoliberal reading?',
    'Analyze IMF resource dependence (quota vs. borrowing), staff career incentives, and governance reform resistance. If the institution''s material reproduction requires the neoliberal reading''s enforcement, the agenda_setter seat is captured.',
    'If captured, the IMF''s agenda_setter role is performative maintenance of a piton-like structure. If not, it remains a genuine coordination enforcer. Affects the theater_ratio interpretation and piton detection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(imf_institutional_incentive_structure, empirical, 'Whether the agenda_setter''s incentives align with coordination or extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__neoliberal_convertibility, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bw_neolib_tr_t0, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bw_neolib_tr_t10, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 10, 0.12).
narrative_ontology:measurement(bw_neolib_tr_t20, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 20, 0.18).
narrative_ontology:measurement(bw_neolib_tr_t27, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 27, 0.28).
narrative_ontology:measurement(bw_neolib_tr_t35, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 35, 0.38).
narrative_ontology:measurement(bw_neolib_tr_t45, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 45, 0.42).
narrative_ontology:measurement(bw_neolib_tr_t55, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 55, 0.44).
narrative_ontology:measurement(bw_neolib_tr_t65, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 65, 0.45).
narrative_ontology:measurement(bw_neolib_tr_t80, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 80, 0.45).

% Extraction over time
narrative_ontology:measurement(bw_neolib_be_t0, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(bw_neolib_be_t10, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(bw_neolib_be_t20, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(bw_neolib_be_t27, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 27, 0.45).
narrative_ontology:measurement(bw_neolib_be_t35, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 35, 0.58).
narrative_ontology:measurement(bw_neolib_be_t45, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 45, 0.68).
narrative_ontology:measurement(bw_neolib_be_t55, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 55, 0.71).
narrative_ontology:measurement(bw_neolib_be_t65, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 65, 0.72).
narrative_ontology:measurement(bw_neolib_be_t80, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 80, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(bw_neolib_su_t0, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(bw_neolib_su_t10, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(bw_neolib_su_t20, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(bw_neolib_su_t27, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 27, 0.5).
narrative_ontology:measurement(bw_neolib_su_t35, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 35, 0.6).
narrative_ontology:measurement(bw_neolib_su_t45, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 45, 0.65).
narrative_ontology:measurement(bw_neolib_su_t55, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 55, 0.67).
narrative_ontology:measurement(bw_neolib_su_t65, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 65, 0.68).
narrative_ontology:measurement(bw_neolib_su_t80, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 80, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__neoliberal_convertibility, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.12).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, imf_conditionality_regime).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_account_liberalization_pressure).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, reserve_currency_seigniorage).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, financial_crisis_management_framework).

% DUAL FORMULATION NOTE:
% This constraint is the neoliberal_convertibility reading of the bretton_woods_treaty_substrate kernel. Its sibling readings are keynesian_embedded_liberalism (constraint_id: bretton_woods_treaty_substrate__keynesian_embedded_liberalism) and sovereignty_defense (constraint_id: bretton_woods_treaty_substrate__sovereignty_defense). The ε values differ substantially: keynesian reading has low extractiveness (capital controls permitted, policy autonomy protected); sovereignty reading has moderate extractiveness (external discipline constrained, but national policy space preserved); this reading has high extractiveness (policy autonomy extracted for finance). All three share the treaty substrate but instantiate different constraints. Network edges reflect downstream institutional consequences of this reading's dominance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bretton_woods_treaty_substrate__neoliberal_convertibility, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
