% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__capital_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__capital_supremacy_reading, []).

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
 *   constraint_id: nafta_jurisdictional_boundary__capital_supremacy_reading
 *   human_readable: NAFTA Jurisdictional Boundary: Capital Supremacy Reading
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This constraint represents the 'capital supremacy' reading of NAFTA's
 *   jurisdictional boundary, where the trade agreement's text is interpreted
 *   as supreme law overriding domestic regulatory standards, and capital
 *   mobility and regulatory harmonization are treated as mandatory treaty
 *   obligations. This reading prioritizes the interests of multinational
 *   corporations and financial capital holders, often at the expense of
 *   domestic labor, environmental protections, and national regulatory
 *   autonomy. The constraint is classified as a Snare due to its high
 *   extractiveness and suppression, with identifiable victims and a
 *   coordination story that serves as cover for asymmetric extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.85).
domain_priors:suppression_score(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.9).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__capital_supremacy_reading, snare).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__capital_supremacy_reading, "NAFTA Jurisdictional Boundary: Capital Supremacy Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__capital_supremacy_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__capital_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__capital_supremacy_reading, '4397ae7c-fd95-46ee-8f67-a21155dc830c').
narrative_ontology:cs_kernel_codification('4397ae7c-fd95-46ee-8f67-a21155dc830c', fixed_text).
narrative_ontology:cs_authority_grounding('4397ae7c-fd95-46ee-8f67-a21155dc830c', extraction).
narrative_ontology:cs_interpretation_layer_present('4397ae7c-fd95-46ee-8f67-a21155dc830c').
narrative_ontology:cs_reading_relation('4397ae7c-fd95-46ee-8f67-a21155dc830c', nafta_jurisdictional_boundary__embedded_liberalism_reading, influences).
narrative_ontology:cs_reading_relation('4397ae7c-fd95-46ee-8f67-a21155dc830c', nafta_jurisdictional_boundary__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('4397ae7c-fd95-46ee-8f67-a21155dc830c', foundational, trade_law_supremacy).
narrative_ontology:cs_axiom_status(trade_law_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('4397ae7c-fd95-46ee-8f67-a21155dc830c', trade_law_supremacy, conventional).
narrative_ontology:cs_axiom('4397ae7c-fd95-46ee-8f67-a21155dc830c', foundational, unrestricted_capital_mobility).
narrative_ontology:cs_axiom_status(unrestricted_capital_mobility, holdable).
narrative_ontology:cs_axiom_grounding('4397ae7c-fd95-46ee-8f67-a21155dc830c', unrestricted_capital_mobility, instrumental).
narrative_ontology:cs_reference_frame('4397ae7c-fd95-46ee-8f67-a21155dc830c', neoliberal_trade_orthodoxy).
narrative_ontology:cs_drift_state('4397ae7c-fd95-46ee-8f67-a21155dc830c', contemporary_populist_backlash, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4397ae7c-fd95-46ee-8f67-a21155dc830c', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, financial_capital_holders).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_unions).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, environmental_advocacy_groups).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, national_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, local_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from reduced regulatory hurdles and the ability to challenge domestic standards as trade barriers. They leverage the agreement's provisions to optimize production and supply chains across borders, minimizing compliance costs.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from unrestricted capital mobility and protections for foreign investment, allowing them to seek the highest returns with minimal regulatory friction. They can rapidly reallocate capital in response to perceived regulatory threats.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, financial_capital_holders, beneficiary,
    powerful, immediate, arbitrage, global).

% Bear the costs of downward pressure on wages and working conditions due to competition from lower-standard jurisdictions. Their ability to advocate for stronger domestic labor protections is constrained by the threat of capital flight and trade challenges.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_unions, payer,
    organized, biographical, constrained, national).

% Face challenges to domestic environmental regulations that are deemed non-tariff barriers to trade. Their efforts to strengthen environmental protections are often met with legal challenges under the trade agreement, leading to a chilling effect on new regulations.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, environmental_advocacy_groups, payer,
    moderate, generational, constrained, national).

% Experience a loss of jurisdictional authority as their domestic regulatory standards become subject to review and potential override by international trade tribunals. They must harmonize regulations with treaty obligations, often leading to a 'race to the bottom'.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, national_regulatory_agencies, payer,
    institutional, biographical, constrained, national).

% Suffer the direct impacts of weakened labor and environmental standards, including job losses, environmental degradation, and reduced public health protections. They have minimal recourse against decisions made at the international trade level.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, local_communities, payer,
    powerless, biographical, trapped, local).

% Are nominally the parties to the agreement, but under this reading, their sovereign power to regulate in the public interest is significantly curtailed by the supremacy of trade law. They enforce the agreement's provisions, often against their own domestic policy preferences.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, sovereign_states, agenda_setter,
    institutional, civilizational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates cross-border trade and investment by harmonizing regulatory environments and providing dispute resolution mechanisms, reducing transaction costs for international commerce.
% TRANSFER_FUNCTION: Transfers regulatory authority and policy space from domestic governments and their citizens to international trade tribunals and multinational corporations, enabling the latter to externalize social and environmental costs.
% ABSENT_VOICES: Indigenous communities, small and medium-sized enterprises (SMEs) without international reach, and future generations (who will inherit environmental degradation) are largely excluded from the negotiation and enforcement processes, and would advocate for stronger domestic protections and a rebalancing of trade priorities.
% DISAPPEARANCE_RATIONALE: If this reading of NAFTA vanished overnight, domestic regulatory agencies would immediately reassert their authority, labor and environmental standards would likely strengthen, and multinational corporations would face increased compliance costs, leading to a significant re-evaluation of their cross-border investment strategies. The balance of power between capital and states would shift dramatically.
% FOUNDING_PROBLEM: To eliminate barriers to trade and investment between member countries, promote economic integration, and create a predictable legal framework for international commerce.
% FOUNDING_PROBLEM_CORROBORATION: Multinational corporations and proponents of this reading attest that the founding problem of trade barriers remains live. Domestic labor unions, environmental groups, and some economists argue that the problem of trade barriers has been largely solved, and the agreement's persistence under this reading primarily serves to protect corporate profits at the expense of public welfare; academic legal analysis and civil society reports corroborate this shifted-function reading.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__capital_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__capital_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__capital_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because this reading systematically transfers wealth and regulatory power from domestic public interests to international capital. Suppression is very high (0.90) as it relies on active enforcement through international tribunals and the threat of trade sanctions to prevent states from enacting or maintaining regulations that might impede capital. Theater ratio is low (0.10) because the enforcement mechanisms are highly effective and directly serve the extractive function, with little performative overhead. The increasing extractiveness and suppression over time reflect the hardening of this interpretation and the growing power of international capital relative to domestic regulatory bodies.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiaries (capital holders) perceive this as a necessary framework for efficient global markets, a 'Rope' that coordinates trade. The victims (labor, environmental groups, regulatory agencies) experience it as a 'Snare' that systematically extracts from them. The engine's classification as Snare reflects the structural reality of high extraction and suppression, regardless of the claimed coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational corporations and financial capital holders are clear beneficiaries (d near 0.0), as the constraint directly subsidizes their operations by reducing regulatory costs and ensuring capital mobility. Domestic labor unions, environmental groups, national regulatory agencies, and local communities are direct targets/victims (d near 1.0), bearing the costs of weakened protections and loss of autonomy. Sovereign states, while nominally agenda-setters, are also constrained by this reading, as their ability to act in the domestic public interest is curtailed.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a highly extractive arrangement as mere coordination. The 'capital supremacy' reading has evolved beyond its initial coordination mandate (reducing trade barriers) to become a mechanism for systematic extraction, where the 'founding problem' is now contested. The high extractiveness and suppression, coupled with the contested status of the founding problem, indicate a clear shift from coordination to extraction, characteristic of a Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretation_vs_textual_basis,
    'To what extent is this ''capital supremacy'' reading an inherent feature of the NAFTA text, versus an interpretive choice by tribunals and powerful actors?',
    'Comparative legal analysis of similar trade agreements with different interpretive histories, or a re-negotiation of the treaty text to explicitly clarify the balance between trade and domestic regulatory autonomy.',
    'If primarily an interpretive choice, it suggests the constraint is more amenable to change through judicial re-interpretation or political will; if inherent in the text, it implies a more fundamental structural problem requiring treaty amendment or withdrawal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_vs_textual_basis, conceptual, 'Ambiguity between textual determinism and interpretive agency in shaping the constraint.').

omega_variable(
    regulatory_harmonization_vs_race_to_bottom,
    'Does the mandatory regulatory harmonization under this reading lead to genuine upward convergence of standards, or a ''race to the bottom'' where standards are lowered to attract capital?',
    'Empirical study comparing regulatory outcomes in NAFTA member states across various sectors (labor, environment, health) before and after the agreement, and in comparison to non-member states.',
    'Evidence of a ''race to the bottom'' would further strengthen the Snare classification by demonstrating direct negative externalities on victims; evidence of upward convergence would suggest a more complex, potentially less extractive, coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_harmonization_vs_race_to_bottom, empirical, 'Empirical outcome of regulatory harmonization: convergence or degradation of standards.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the ''nafta_jurisdictional_boundary'' kernel, or does it represent a fundamentally different constraint?',
    'Analysis of whether the core commitment (the NAFTA text) remains the referent for all readings, or if this reading has diverged so far as to constitute a new, independent constraint.',
    'If it''s a distinct reading, the kernel framework is appropriate. If it''s a new constraint, it should be re-classified as an independent entity, potentially linked to the original kernel via ''affects_constraints'' rather than being a reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''capital_supremacy_reading'' of the ''nafta_jurisdictional_boundary'' kernel. Sibling readings include ''embedded_liberalism_reading'' and ''sovereignty_primacy_reading''. This reading emphasizes the supremacy of trade law and capital mobility, leading to a victim set that includes domestic labor and environmental standards, and a loss of jurisdictional authority for regulatory agencies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__capital_supremacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(naft_tr_t5, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(naft_tr_t10, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(naft_tr_t15, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(naft_tr_t20, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(naft_tr_t25, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 25, 0.1).

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(naft_be_t5, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(naft_be_t10, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(naft_be_t15, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 15, 0.83).
narrative_ontology:measurement(naft_be_t20, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 20, 0.84).
narrative_ontology:measurement(naft_be_t25, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 25, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(naft_su_t5, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 5, 0.8).
narrative_ontology:measurement(naft_su_t10, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(naft_su_t15, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 15, 0.88).
narrative_ontology:measurement(naft_su_t20, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 20, 0.89).
narrative_ontology:measurement(naft_su_t25, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 25, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__capital_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, global_supply_chain_labor_standards).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, national_environmental_regulations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
