% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__neoliberal_convertibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   human_readable: Bretton Woods: Neoliberal Convertibility Reading
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the 'neoliberal convertibility'
 *   reading of the Bretton Woods treaty substrate. This reading interprets
 *   the original agreements, particularly after the collapse of fixed
 *   exchange rates in the early 1970s, as fundamentally establishing a
 *   framework for free capital markets and limiting government intervention
 *   in favor of market discipline. It views capital controls as violations
 *   and prioritizes the interests of international finance over national
 *   policy autonomy. The constraint is claimed as a Tangled Rope, reflecting
 *   its dual function of coordinating global capital flows while extracting
 *   policy space from national actors.
 *
 * KEY AGENTS:
 *   - international_financial_institutions: Primary agenda_setter and beneficiary (institutional/arbitrage)
 *   - global_capital_holders: Primary beneficiary (powerful/arbitrage)
 *   - national_governments: Primary payer (institutional/constrained)
 *   - domestic_labor_markets: Primary payer (powerless/trapped)
 *   - keynesian_economists: Excluded voice (analytical/analytical)
 *   - sovereignty_advocates: Excluded voice (organized/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.85).
domain_priors:suppression_score(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.78).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, extractiveness, 0.85).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__neoliberal_convertibility, "Bretton Woods: Neoliberal Convertibility Reading").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__neoliberal_convertibility, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__neoliberal_convertibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__neoliberal_convertibility, '782273a5-afdb-478a-b5df-3ebc84d7049a').
narrative_ontology:cs_kernel_codification('782273a5-afdb-478a-b5df-3ebc84d7049a', formalized).
narrative_ontology:cs_authority_grounding('782273a5-afdb-478a-b5df-3ebc84d7049a', lineage).
narrative_ontology:cs_interpretation_layer_present('782273a5-afdb-478a-b5df-3ebc84d7049a').
narrative_ontology:cs_reading_relation('782273a5-afdb-478a-b5df-3ebc84d7049a', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, forecloses).
narrative_ontology:cs_reading_relation('782273a5-afdb-478a-b5df-3ebc84d7049a', bretton_woods_treaty_substrate__sovereignty_defense, forecloses).
narrative_ontology:cs_axiom('782273a5-afdb-478a-b5df-3ebc84d7049a', foundational, capital_mobility_is_efficient).
narrative_ontology:cs_axiom_status(capital_mobility_is_efficient, holdable).
narrative_ontology:cs_axiom_grounding('782273a5-afdb-478a-b5df-3ebc84d7049a', capital_mobility_is_efficient, empirically_contingent).
narrative_ontology:cs_axiom('782273a5-afdb-478a-b5df-3ebc84d7049a', foundational, government_intervention_distorts_markets).
narrative_ontology:cs_axiom_status(government_intervention_distorts_markets, holdable).
narrative_ontology:cs_axiom_grounding('782273a5-afdb-478a-b5df-3ebc84d7049a', government_intervention_distorts_markets, empirically_contingent).
narrative_ontology:cs_reference_frame('782273a5-afdb-478a-b5df-3ebc84d7049a', washington_consensus_era).
narrative_ontology:cs_drift_state('782273a5-afdb-478a-b5df-3ebc84d7049a', post_2008_financial_crisis, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('782273a5-afdb-478a-b5df-3ebc84d7049a', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, international_financial_institutions).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, global_capital_holders).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, national_governments).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_labor_markets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote and enforce policies that prioritize capital mobility and convertibility, interpreting the Bretton Woods agreements as foundational for this order. They benefit from the stability and growth of global financial markets.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, international_financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit directly from the free movement of capital across borders, allowing them to seek the highest returns and diversify investments without significant national restrictions. They lobby for the maintenance of these rules.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, global_capital_holders, beneficiary,
    powerful, biographical, arbitrage, global).

% Bear the cost of reduced policy autonomy, particularly in monetary and fiscal policy, as they must prioritize capital market stability and avoid measures that could trigger capital flight. Their ability to implement counter-cyclical policies is constrained.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, national_governments, payer,
    institutional, biographical, constrained, national).

% Are vulnerable to the pressures of global capital mobility, facing downward pressure on wages, reduced social protections, and increased precarity as governments compete for investment and avoid capital flight. They have little direct influence on these international rules.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_labor_markets, payer,
    powerless, immediate, trapped, national).

% Argue that the emphasis on free capital markets undermines domestic policy space and leads to financial instability, advocating for capital controls and greater state intervention. Their views are often marginalized in dominant policy circles.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, keynesian_economists, excluded,
    analytical, generational, analytical, global).

% Contend that the Bretton Woods framework, as interpreted, erodes national monetary sovereignty and democratic control over economic policy. They seek to restore the ability of states to manage their own currencies and economies free from external financial discipline.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, sovereignty_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__neoliberal_convertibility, global_capital_holders).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__neoliberal_convertibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for international monetary cooperation that facilitates global trade and investment by ensuring currency convertibility and discouraging competitive devaluations, thereby enabling efficient allocation of capital.
% TRANSFER_FUNCTION: Transfers policy autonomy from national governments to the imperatives of global capital markets, allowing global capital holders to move funds freely and extract returns, while imposing discipline on national economic policies.
% ABSENT_VOICES: Advocates for capital controls, proponents of robust domestic industrial policy, and those prioritizing national economic sovereignty are largely excluded from the interpretive and enforcement mechanisms of this reading, as their positions are framed as violations of the established order.
% DISAPPEARANCE_RATIONALE: If the neoliberal convertibility interpretation of Bretton Woods vanished, the global financial architecture would undergo a profound reorganization. National governments would regain significant policy space, potentially reintroducing capital controls, leading to a fragmentation of global capital markets and a re-evaluation of international trade and investment flows.
% FOUNDING_PROBLEM: The original Bretton Woods agreements aimed to prevent a return to the economic nationalism and competitive devaluations of the interwar period, which had destabilized global trade and led to conflict. This reading emphasizes the need for open capital markets to achieve global prosperity.
% FOUNDING_PROBLEM_CORROBORATION: International financial institutions and many mainstream economists attest that the problem of financial instability and inefficient capital allocation remains live, requiring continued adherence to free capital markets. Critics (e.g., Keynesian economists, development economists) argue that the problem has evolved, and the current interpretation exacerbates new forms of instability and inequality; their testimony and historical analysis from outside the benefiting parties challenge the 'live' status.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__neoliberal_convertibility, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__neoliberal_convertibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__neoliberal_convertibility, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because the interpretation systematically prioritizes global capital mobility, imposing significant costs on national policy autonomy and domestic welfare. Suppression (0.78) is also high, as the framework actively discourages and penalizes capital controls or other forms of state intervention that would impede capital flows, often through conditional lending and policy advice from IFIs. Theater ratio is low (0.1) because the enforcement of capital mobility is a core, active function of the global financial system, not a performative relic. Accessibility collapse is high (0.7) because the dominant discourse frames alternatives like capital controls as economically unsound or illegitimate. Resistance (0.6) is moderate, reflecting ongoing debates and occasional attempts by states to reassert policy space, especially after financial crises.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of international financial institutions and global capital holders, this interpretation of Bretton Woods is a necessary coordination mechanism for global prosperity, with any 'extraction' being a legitimate cost of market efficiency. From the perspective of national governments and domestic labor markets, it is a system that extracts policy autonomy and welfare in favor of financial interests. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   International financial institutions and global capital holders are clear beneficiaries, as the constraint directly enables their operations and profit-seeking (low d). National governments and domestic labor markets are targets, bearing the costs of reduced policy space and increased vulnerability (high d). Keynesian economists and sovereignty advocates are excluded, as their alternative framings are suppressed by the dominant interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of Bretton Woods has arguably undergone a form of mandatrophy where its original mandate (preventing interwar chaos, protecting domestic policy space) has been reinterpreted to serve a different, more extractive function (enabling free capital markets). The coordination story of 'global financial stability' persists, but the mechanism for achieving it has shifted from managed exchange rates and capital controls to capital liberalization, leading to asymmetric extraction. The classification as Tangled Rope captures this hybridity, preventing mislabeling it as pure coordination or pure extraction without acknowledging its historical evolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_vs_neoliberal_interpretation,
    'To what extent does the ''neoliberal convertibility'' reading align with the original intent of the Bretton Woods agreements, particularly regarding capital controls and national policy autonomy?',
    'Historical analysis of primary source documents, diplomatic correspondence, and early policy debates from the 1940s-1960s, comparing stated goals with later interpretations.',
    'If the original intent strongly favored capital controls and policy autonomy (as argued by the Keynesian embedded liberalism reading), then the neoliberal convertibility reading is a significant reinterpretation that masks extraction under a historical claim. If the original intent was more ambiguous or already leaned towards capital mobility, the current reading is less of a distortion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_vs_neoliberal_interpretation, empirical, 'Alignment of current interpretation with historical founding intent.').

omega_variable(
    structural_necessity_of_capital_mobility,
    'Is the high degree of capital mobility enforced by this reading a structural necessity for global economic stability and growth, or does it primarily serve the interests of global capital holders?',
    'Comparative economic analysis of countries that have successfully implemented capital controls (e.g., Malaysia in 1998, China) without severe economic collapse, or studies on the relationship between capital mobility and financial crises.',
    'If capital mobility is not a structural necessity, then the suppression of capital controls is pure extraction. If it is, then a portion of the measured extraction is a legitimate cost of global coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_necessity_of_capital_mobility, empirical, 'Whether capital mobility is a structural necessity or an extractive preference.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of national policy autonomy structural (external market pressures, IFI conditionalities) or internalized (governments'' belief in neoliberal orthodoxy)?',
    'Analysis of policy choices in moments of crisis: if governments revert to capital controls despite external pressure, the internalized component is weaker. If they resist even when external pressure lessens, the internalized component is stronger.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as governments carry the suppression with them even when external barriers are reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for national policy autonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__neoliberal_convertibility, 1971, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1971, 0.05).
narrative_ontology:measurement(bret_tr_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(bret_tr_t1995, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(bret_tr_t2008, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2008, 0.12).
narrative_ontology:measurement(bret_tr_t2015, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2015, 0.11).
narrative_ontology:measurement(bret_tr_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1971, 0.6).
narrative_ontology:measurement(bret_be_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1985, 0.75).
narrative_ontology:measurement(bret_be_t1995, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1995, 0.85).
narrative_ontology:measurement(bret_be_t2008, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2008, 0.88).
narrative_ontology:measurement(bret_be_t2015, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2015, 0.86).
narrative_ontology:measurement(bret_be_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1971, 0.5).
narrative_ontology:measurement(bret_su_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement(bret_su_t1995, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1995, 0.78).
narrative_ontology:measurement(bret_su_t2008, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2008, 0.8).
narrative_ontology:measurement(bret_su_t2015, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2015, 0.79).
narrative_ontology:measurement(bret_su_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__neoliberal_convertibility, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'bretton_woods_treaty_substrate' kernel. Other readings include 'keynesian_embedded_liberalism' and 'sovereignty_defense', which offer alternative interpretations of the treaty's purpose and mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
