% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__keynesian_embedded_liberalism, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
 *   human_readable: Bretton Woods: Keynesian Embedded Liberalism Reading (Capital Controls for Policy Space)
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This constraint story represents the 'Keynesian Embedded Liberalism'
 *   reading of the Bretton Woods system, which posits that the system was
 *   designed to constrain international capital flows to safeguard national
 *   policy space for full employment and social welfare. This reading
 *   emphasizes the legitimacy of capital controls as tools for domestic
 *   stability, rather than as market distortions. The constraint is
 *   classified as a Rope because it genuinely coordinated national economic
 *   policies for collective benefit, even while imposing costs on
 *   international capital.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.45).
domain_priors:suppression_score(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.6).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, extractiveness, 0.45).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "Bretton Woods: Keynesian Embedded Liberalism Reading (Capital Controls for Policy Space)").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__keynesian_embedded_liberalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, '7dc83640-1204-4a35-92d6-aee633ba68ab').
narrative_ontology:cs_kernel_codification('7dc83640-1204-4a35-92d6-aee633ba68ab', formalized).
narrative_ontology:cs_authority_grounding('7dc83640-1204-4a35-92d6-aee633ba68ab', lineage).
narrative_ontology:cs_interpretation_layer_present('7dc83640-1204-4a35-92d6-aee633ba68ab').
narrative_ontology:cs_reading_relation('7dc83640-1204-4a35-92d6-aee633ba68ab', bretton_woods_treaty_substrate__neoliberal_convertibility, coexists_with).
narrative_ontology:cs_reading_relation('7dc83640-1204-4a35-92d6-aee633ba68ab', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('7dc83640-1204-4a35-92d6-aee633ba68ab', foundational, capital_controls_legitimate_policy_tool).
narrative_ontology:cs_axiom_status(capital_controls_legitimate_policy_tool, holdable).
narrative_ontology:cs_axiom_grounding('7dc83640-1204-4a35-92d6-aee633ba68ab', capital_controls_legitimate_policy_tool, conventional).
narrative_ontology:cs_axiom('7dc83640-1204-4a35-92d6-aee633ba68ab', foundational, domestic_policy_space_priority).
narrative_ontology:cs_axiom_status(domestic_policy_space_priority, holdable).
narrative_ontology:cs_axiom_grounding('7dc83640-1204-4a35-92d6-aee633ba68ab', domestic_policy_space_priority, instrumental).
narrative_ontology:cs_reference_frame('7dc83640-1204-4a35-92d6-aee633ba68ab', post_war_embedded_liberal_consensus).
narrative_ontology:cs_drift_state('7dc83640-1204-4a35-92d6-aee633ba68ab', post_1971_collapse, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('7dc83640-1204-4a35-92d6-aee633ba68ab', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_labor_markets).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_capital_flows).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, speculative_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefited from the ability to implement independent monetary and fiscal policies without being undermined by capital flight or speculative attacks. They could prioritize full employment and social welfare programs.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, beneficiary,
    institutional, generational, constrained, national).

% Benefited from stable domestic economies and policies aimed at full employment, protected from the volatility of global capital. Their bargaining power was enhanced by reduced external pressure.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_labor_markets, beneficiary,
    organized, biographical, constrained, national).

% Were constrained by capital controls, limiting their free movement across borders and reducing opportunities for arbitrage and speculative gains. This imposed a cost on their mobility and profit potential.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_capital_flows, payer,
    powerful, immediate, constrained, global).

% Faced restrictions on their ability to move capital quickly to exploit interest rate differentials or currency fluctuations, reducing their profit opportunities and leverage over national economies.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, speculative_investors, payer,
    moderate, immediate, constrained, global).

% Administered the Bretton Woods system, overseeing fixed exchange rates and providing short-term financing. Its role was to ensure stability and facilitate the operation of capital controls, not to dismantle them.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_monetary_fund, agenda_setter,
    institutional, generational, constrained, global).

% Argued against capital controls and fixed exchange rates, advocating for free markets and minimal government intervention. Their views were largely excluded from the initial design and early operation of the Bretton Woods system.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, neoliberal_economists, excluded,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a system of fixed but adjustable exchange rates and legitimate capital controls to prevent competitive devaluations and allow national governments to pursue independent macroeconomic policies, fostering global economic stability.
% TRANSFER_FUNCTION: Transferred a degree of autonomy over domestic economic policy to national governments by limiting the power of international capital flows, effectively transferring potential speculative gains from investors to national policy space.
% ABSENT_VOICES: Advocates for unrestricted capital mobility and purely market-driven exchange rates were largely absent from the foundational discussions and early implementation, as the system was designed to explicitly constrain their preferred mechanisms.
% DISAPPEARANCE_RATIONALE: If the Bretton Woods system (as understood by this reading) had never existed, national governments would have faced much greater pressure from international capital, leading to different domestic policy choices, increased financial volatility, and potentially less robust social welfare states. The global financial architecture would have developed along a different, more market-liberal path from the outset.
% FOUNDING_PROBLEM: The interwar period was characterized by competitive currency devaluations, protectionism, and volatile capital flows that undermined national economic stability and contributed to global depression.
% FOUNDING_PROBLEM_CORROBORATION: Historians of economic thought and international relations widely corroborate the problem of interwar financial instability. Contemporary economists and policymakers, particularly those advocating for 'macroprudential' capital controls, attest that similar problems of financial instability and loss of policy space persist in the absence of such constraints, indicating the founding problem is still live.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).
:- end_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, reflecting the costs imposed on international capital mobility, but balanced by the benefits of domestic policy autonomy. Suppression (0.6) is also moderate, as capital controls required active enforcement but were generally accepted as legitimate tools. Theater ratio is low (0.1) because the system's functions were largely genuine and effective during its operational period. The system's design explicitly aimed to collapse the accessibility of unrestricted capital movement (0.7) to reduce resistance from domestic policy goals (0.3).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of national governments, the system was a beneficial coordination mechanism. From the perspective of international capital, it was a restrictive and extractive regime. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing a Rope-like constraint and payers experiencing a more Snare-like one.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments and domestic labor markets are clear beneficiaries, gaining policy autonomy and stability. International capital flows and speculative investors are the primary payers, bearing the costs of restricted mobility. The IMF, as the agenda-setter, facilitated the system's operation, balancing coordination with enforcement. Neoliberal economists are 'excluded' as their preferred mechanisms were explicitly constrained.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_control_effectiveness,
    'To what extent were capital controls truly effective in insulating domestic policy space, given the rise of offshore markets and financial innovation?',
    'Empirical studies analyzing the leakage and circumvention of capital controls over the Bretton Woods period, and their impact on national policy autonomy.',
    'If controls were largely ineffective, the constraint''s actual extractiveness from capital would be lower, and its coordination function for national policy space would be weaker, potentially shifting its classification towards a Piton or a less effective Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_control_effectiveness, empirical, 'The actual efficacy of capital controls in achieving their stated goals.').

omega_variable(
    legitimacy_of_capital_controls,
    'Is the constraint on international capital flows a legitimate tool for national policy autonomy, or an illegitimate interference with economic freedom?',
    'This is a preference-based question, resolvable only through normative debate and political choice regarding the prioritization of national sovereignty and social welfare versus global economic efficiency and individual economic liberty.',
    'A resolution favoring economic freedom would reframe capital controls as pure extraction, shifting the constraint towards a Snare. A resolution favoring policy autonomy would reinforce its Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_capital_controls, preference, 'Normative debate over the legitimacy of capital controls.').

omega_variable(
    embedded_liberalism_vs_neoliberalism,
    'Is the Bretton Woods system fundamentally a ''Keynesian embedded liberalism'' project, or was it always intended to transition towards ''neoliberal convertibility''?',
    'Conceptual analysis of founding documents, statements by key architects, and historical trajectories of policy debates. This is a debate about the ''true'' intent and telos of the system.',
    'If the ''neoliberal convertibility'' reading is adopted, the constraint''s extractiveness from capital would be re-evaluated as a temporary, undesirable friction, and its classification would shift towards a Scaffold (transitional) or even a Snare (if the coordination story is seen as cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embedded_liberalism_vs_neoliberalism, conceptual, 'Debate over the core ideological framing of Bretton Woods.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1944, 0.08).
narrative_ontology:measurement(bret_tr_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1950, 0.09).
narrative_ontology:measurement(bret_tr_t1958, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1958, 0.09).
narrative_ontology:measurement(bret_tr_t1965, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1971, 0.1).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1944, 0.4).
narrative_ontology:measurement(bret_be_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement(bret_be_t1958, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1958, 0.43).
narrative_ontology:measurement(bret_be_t1965, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1965, 0.44).
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1971, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1944, 0.55).
narrative_ontology:measurement(bret_su_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1950, 0.57).
narrative_ontology:measurement(bret_su_t1958, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1958, 0.58).
narrative_ontology:measurement(bret_su_t1965, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1965, 0.59).
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1971, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, enforcement_mechanism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, post_bretton_woods_floating_exchange_rates).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, washington_consensus_fiscal_discipline).

% DUAL FORMULATION NOTE:
% This is one of three readings of the Bretton Woods treaty substrate. This 'Keynesian Embedded Liberalism' reading emphasizes capital controls for domestic policy space. Sibling readings ('neoliberal_convertibility' and 'sovereignty_defense') offer alternative interpretations of the system's core purpose and mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
