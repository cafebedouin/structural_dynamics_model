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
 *   human_readable: Bretton Woods: Keynesian Embedded Liberalism
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the 'Keynesian embedded liberalism'
 *   reading of the Bretton Woods treaty substrate. This reading posits that
 *   the Bretton Woods system was designed to constrain international capital
 *   flows to protect domestic policy space, allowing national governments to
 *   pursue full employment and social welfare policies. Capital controls were
 *   seen as legitimate tools, not violations. The system aimed to balance
 *   international economic cooperation with national autonomy, a 'tangled
 *   rope' that coordinated states while extracting from financial capital.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.65).
domain_priors:suppression_score(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.75).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, extractiveness, 0.65).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "Bretton Woods: Keynesian Embedded Liberalism").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__keynesian_embedded_liberalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'f271f3d7-530f-4e54-99f4-7c7f36af2e43').
narrative_ontology:cs_kernel_codification('f271f3d7-530f-4e54-99f4-7c7f36af2e43', fixed_text).
narrative_ontology:cs_authority_grounding('f271f3d7-530f-4e54-99f4-7c7f36af2e43', lineage).
narrative_ontology:cs_interpretation_layer_present('f271f3d7-530f-4e54-99f4-7c7f36af2e43').
narrative_ontology:cs_reading_relation('f271f3d7-530f-4e54-99f4-7c7f36af2e43', bretton_woods_treaty_substrate__neoliberal_convertibility, forecloses).
narrative_ontology:cs_reading_relation('f271f3d7-530f-4e54-99f4-7c7f36af2e43', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('f271f3d7-530f-4e54-99f4-7c7f36af2e43', foundational, capital_controls_essential_for_stability).
narrative_ontology:cs_axiom_status(capital_controls_essential_for_stability, holdable).
narrative_ontology:cs_axiom_grounding('f271f3d7-530f-4e54-99f4-7c7f36af2e43', capital_controls_essential_for_stability, conventional).
narrative_ontology:cs_axiom('f271f3d7-530f-4e54-99f4-7c7f36af2e43', foundational, domestic_policy_autonomy_priority).
narrative_ontology:cs_axiom_status(domestic_policy_autonomy_priority, holdable).
narrative_ontology:cs_axiom_grounding('f271f3d7-530f-4e54-99f4-7c7f36af2e43', domestic_policy_autonomy_priority, instrumental).
narrative_ontology:cs_reference_frame('f271f3d7-530f-4e54-99f4-7c7f36af2e43', embedded_liberalism_consensus).
narrative_ontology:cs_drift_state('f271f3d7-530f-4e54-99f4-7c7f36af2e43', post_bretton_woods_collapse, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('f271f3d7-530f-4e54-99f4-7c7f36af2e43', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_industries).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, labor_unions).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance_capital).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, speculators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefited from the ability to manage domestic economies for full employment and social welfare, protected from external capital flight by capital controls. They actively participated in setting and enforcing the rules of the Bretton Woods system.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, agenda_setter,
    institutional, generational, constrained, national).

% Subject to capital controls and fixed exchange rates, limiting their ability to move capital freely across borders for speculative gains. This constrained their profit-making opportunities compared to a free capital market.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance_capital, payer,
    powerful, immediate, constrained, global).

% Benefited from stable exchange rates and national economic policies that prioritized domestic growth and employment, shielded from the volatility of international capital flows.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_industries, beneficiary,
    organized, biographical, constrained, national).

% Benefited from national policies aimed at full employment and social welfare, which were made possible by the protection afforded by capital controls against external economic pressures.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, labor_unions, beneficiary,
    organized, biographical, constrained, national).

% Their ability to profit from short-term capital movements and currency fluctuations was severely curtailed by the fixed exchange rate system and capital controls, making their traditional activities largely impossible.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, speculators, payer,
    moderate, immediate, trapped, global).

% Administered the rules of the Bretton Woods system, including overseeing exchange rate parities and providing short-term liquidity to countries facing balance-of-payments difficulties, thereby enforcing the embedded liberal order.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_monetary_fund, agenda_setter,
    institutional, generational, analytical, global).

% Their intellectual framework, which advocated for free capital mobility and minimal state intervention, was largely excluded from the foundational design and initial operational principles of the Bretton Woods system.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, neoliberal_economists, excluded,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a stable international monetary system with fixed but adjustable exchange rates, prevent competitive devaluations, and allow national governments to pursue independent domestic economic policies (e.g., full employment) without being undermined by capital flight.
% TRANSFER_FUNCTION: Transfers control over international capital flows from private financial actors to national governments, enabling the latter to direct capital for domestic policy goals and stabilize their economies.
% ABSENT_VOICES: Advocates for unrestricted capital mobility and financial liberalization were structurally excluded from the initial design and operational principles of the system, as their views were seen as contributing to the interwar instability the system sought to prevent.
% DISAPPEARANCE_RATIONALE: If the Bretton Woods system, as interpreted by Keynesian embedded liberalism, had not been established, the post-WWII international economic order would have been fundamentally different, likely characterized by a return to competitive devaluations, capital flight, and greater global economic instability, profoundly altering national policy choices and international cooperation.
% FOUNDING_PROBLEM: The economic instability of the interwar period, characterized by competitive currency devaluations, protectionism, and uncontrolled capital flight, which undermined national economic sovereignty and contributed to the Great Depression.
% FOUNDING_PROBLEM_CORROBORATION: Historians of economic thought, international relations scholars, and many contemporary policymakers (especially in developing nations) corroborate the original problem and the system's effectiveness in addressing it. Legislative hearing testimony and independent economic analyses from outside the benefiting parties support the historical account of the problem and the system's initial efficacy.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the significant limitations placed on international finance and speculators, who bore the costs of restricted capital mobility. Suppression (0.75) was high due to the active enforcement of capital controls and fixed exchange rates by national governments and the IMF. The theater ratio (0.15) was low, as the system was largely functional and achieved its stated goals for a significant period. The claimed type is 'tangled_rope' because it provided genuine coordination (exchange rate stability, policy space) but also involved asymmetric extraction from specific financial actors.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of national governments and domestic stakeholders, the system was a beneficial coordination mechanism. From the perspective of international finance, it was a highly extractive and suppressive regime. The engine's per-seat classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments, domestic industries, and labor unions were the primary beneficiaries, gaining policy autonomy and stability. International finance capital and speculators were the primary targets, facing restrictions on their activities. The IMF acted as an agenda-setter, administering the system. Neoliberal economists, advocating for free capital, were structurally excluded from the system's design.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''keynesian_embedded_liberalism'' reading of the Bretton Woods kernel, distinct from ''neoliberal_convertibility'' or ''sovereignty_defense''?',
    'Analysis of primary historical documents, policy debates, and economic outcomes, focusing on the explicit intent and operational mechanisms of the system as understood by its architects and early practitioners.',
    'If misidentified, the classification of extractiveness, beneficiaries, and victims would shift dramatically to align with the correct reading''s structural claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Distinguishing this specific reading of the Bretton Woods kernel.').

omega_variable(
    capital_control_effectiveness,
    'To what extent were capital controls genuinely effective in protecting domestic policy space, and how much did they merely redirect or delay capital flows?',
    'Empirical studies comparing economic outcomes in countries with varying degrees of capital control enforcement during the Bretton Woods era, and analysis of capital flight data.',
    'If controls were less effective than assumed, the ''suppression'' and ''extractiveness'' metrics might be overstated, potentially shifting the classification towards a ''piton'' or ''rope'' if the coordination function was less robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_control_effectiveness, empirical, 'The actual efficacy of capital controls in achieving their stated goals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1944, 0.1).
narrative_ontology:measurement(bret_tr_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(bret_tr_t1957, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1957, 0.14).
narrative_ontology:measurement(bret_tr_t1964, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1964, 0.16).
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1971, 0.15).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1944, 0.6).
narrative_ontology:measurement(bret_be_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1950, 0.62).
narrative_ontology:measurement(bret_be_t1957, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1957, 0.64).
narrative_ontology:measurement(bret_be_t1964, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1964, 0.66).
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1971, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1944, 0.7).
narrative_ontology:measurement(bret_su_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1950, 0.72).
narrative_ontology:measurement(bret_su_t1957, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1957, 0.74).
narrative_ontology:measurement(bret_su_t1964, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1964, 0.76).
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1971, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
