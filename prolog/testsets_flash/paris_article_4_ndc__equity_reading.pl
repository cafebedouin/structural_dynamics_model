% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__equity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__equity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: paris_article_4_ndc__equity_reading
 *   human_readable: Paris Agreement Article 4 NDC Interpretation: Equity Reading (CBDR-RC)
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'equity reading' of the Paris Agreement's
 *   Article 4, which mandates Nationally Determined Contributions (NDCs).
 *   This reading emphasizes Common But Differentiated Responsibilities and
 *   Respective Capabilities (CBDR-RC), requiring structural distinctions
 *   between developed and developing states in their climate obligations. It
 *   leads to an asymmetric distribution of burdens and benefits, with
 *   developed states facing greater pressure for emissions reductions and
 *   financial transfers, while developing states retain more policy space.
 *   This interpretation is a Tangled Rope: it coordinates global
 *   participation by acknowledging historical inequities, but it also
 *   extracts from developed states and high-emitting industries to benefit
 *   developing states and equity coalitions, requiring active enforcement
 *   through diplomatic pressure and negotiation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, 0.45).
domain_priors:suppression_score(paris_article_4_ndc__equity_reading, 0.3).
domain_priors:theater_ratio(paris_article_4_ndc__equity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__equity_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__equity_reading, "Paris Agreement Article 4 NDC Interpretation: Equity Reading (CBDR-RC)").
narrative_ontology:topic_domain(paris_article_4_ndc__equity_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__equity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__equity_reading, 'f37b6ad7-8223-4895-9a34-2e1b844e5eeb').
narrative_ontology:cs_kernel_codification('f37b6ad7-8223-4895-9a34-2e1b844e5eeb', formalized).
narrative_ontology:cs_authority_grounding('f37b6ad7-8223-4895-9a34-2e1b844e5eeb', lineage).
narrative_ontology:cs_interpretation_layer_present('f37b6ad7-8223-4895-9a34-2e1b844e5eeb').
narrative_ontology:cs_reading_relation('f37b6ad7-8223-4895-9a34-2e1b844e5eeb', paris_article_4_ndc__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f37b6ad7-8223-4895-9a34-2e1b844e5eeb', paris_article_4_ndc__supranational_reading, coexists_with).
narrative_ontology:cs_axiom('f37b6ad7-8223-4895-9a34-2e1b844e5eeb', foundational, historical_responsibility_for_emissions).
narrative_ontology:cs_axiom_status(historical_responsibility_for_emissions, holdable).
narrative_ontology:cs_axiom_grounding('f37b6ad7-8223-4895-9a34-2e1b844e5eeb', historical_responsibility_for_emissions, deontological).
narrative_ontology:cs_axiom('f37b6ad7-8223-4895-9a34-2e1b844e5eeb', foundational, differentiated_capabilities_for_action).
narrative_ontology:cs_axiom_status(differentiated_capabilities_for_action, holdable).
narrative_ontology:cs_axiom_grounding('f37b6ad7-8223-4895-9a34-2e1b844e5eeb', differentiated_capabilities_for_action, empirically_contingent).
narrative_ontology:cs_reference_frame('f37b6ad7-8223-4895-9a34-2e1b844e5eeb', unfccc_cbdr_rc_framework).
narrative_ontology:cs_drift_state('f37b6ad7-8223-4895-9a34-2e1b844e5eeb', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f37b6ad7-8223-4895-9a34-2e1b844e5eeb', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__equity_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, developing_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, equity_coalitions).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, high_emitting_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, climate_vulnerable_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Expected to bear a greater burden in emissions reductions and provide financial/technological support to developing states, reflecting historical responsibility. They face pressure to increase ambition and transfer resources, but resist legally binding targets that could harm their economies.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_states, payer,
    institutional, generational, constrained, global).

% Benefit from the principle of Common But Differentiated Responsibilities and Respective Capabilities (CBDR-RC), which grants them more policy space for economic development and access to climate finance. They advocate for greater ambition from developed states and resist uniform obligations.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developing_states, beneficiary,
    organized, generational, mobile, global).

% Groups of developing states and civil society organizations that actively champion the CBDR-RC principle, pushing for its robust interpretation and implementation. They exert significant diplomatic pressure and can veto consensus on stronger, undifferentiated commitments.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, equity_coalitions, agenda_setter,
    organized, generational, constrained, global).

% Face increasing regulatory and financial pressure to decarbonize, particularly in developed states. They bear the costs of emissions reductions and carbon pricing, often lobbying against stricter interpretations of NDCs.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, high_emitting_industries, payer,
    powerful, biographical, constrained, global).

% Would ideally enforce a more uniform, ratcheting mechanism for NDCs, but their authority is constrained by the CBDR-RC principle as interpreted by the equity reading. They lack the power to impose binding, undifferentiated targets or sanctions.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, supranational_climate_institutions, excluded,
    institutional, generational, trapped, global).

% Are the ultimate beneficiaries of effective climate action, but their direct influence on NDC interpretation is limited. They rely on developing states and equity coalitions to advocate for their interests and secure climate finance for adaptation and loss and damage.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, climate_vulnerable_communities, beneficiary,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global climate action by acknowledging historical responsibility and differing capabilities, aiming to build trust and facilitate participation from all states by not imposing uniform burdens.
% TRANSFER_FUNCTION: Mandates a transfer of responsibility for emissions reductions and climate finance from developing to developed states, while also transferring policy space and development flexibility to developing states.
% ABSENT_VOICES: A strong, independent supranational enforcement body capable of imposing uniform, binding emissions targets would object, arguing that differentiated responsibilities hinder the necessary pace and scale of global decarbonization. Their voice is muted by the emphasis on national sovereignty and differentiated capabilities.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the entire framework of international climate negotiations would collapse. Developing states would likely withdraw or refuse to participate in new agreements without the CBDR-RC principle, leading to a fragmented and less effective global response to climate change.
% FOUNDING_PROBLEM: The historical inequity of climate change, where developed nations contributed most to emissions but developing nations face disproportionate impacts, leading to a lack of trust and willingness to participate in global climate agreements without differentiated responsibilities.
% FOUNDING_PROBLEM_CORROBORATION: The G77+China bloc, the Alliance of Small Island States (AOSIS), and numerous academic studies on climate justice consistently corroborate that the problem of historical inequity and differentiated capabilities remains central to international climate negotiations. Independent analyses from UN bodies and NGOs also support the ongoing relevance of this founding problem.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__equity_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__equity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__equity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(paris_article_4_ndc__equity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__equity_reading_tests).
:- end_tests(paris_article_4_ndc__equity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the transfer of responsibility and resources from developed to developing states. Suppression (0.30) is relatively low, as the constraint's persistence relies more on diplomatic consensus and the political power of equity coalitions than overt coercion. Theater ratio (0.20) is also low, as the CBDR-RC principle is genuinely invoked and shapes policy, though some performative aspects exist in negotiations. The trend shows a slight increase in extractiveness and suppression as the demands for climate finance and differentiated action intensify.
 *
 * PERSPECTIVAL GAP:
 *   Developed states perceive this as a constraint that unfairly burdens them, while developing states see it as a necessary and just coordination mechanism. The engine will compute different classifications for these seats based on their declared power, exit options, and beneficiary/victim status. The equity coalitions, as agenda-setters, experience it as a tool for justice and leverage.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed states and high-emitting industries are primary payers (d near 1.0) as they bear the costs of emissions reductions and transfers. Developing states and climate-vulnerable communities are beneficiaries (d near 0.0) due to increased policy space and access to climate finance. Equity coalitions act as agenda-setters, actively shaping and enforcing this interpretation, benefiting from its persistence. Supranational climate institutions are excluded from a more powerful enforcement role by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cbdr_rc_scope_ambiguity,
    'What is the precise scope and application of ''Common But Differentiated Responsibilities and Respective Capabilities'' (CBDR-RC) in the context of NDCs, particularly regarding financial and technological transfers?',
    'Further negotiation and agreement on specific metrics and targets for ''differentiated responsibilities'' in finance and technology, or a landmark legal ruling clarifying its application.',
    'A narrower interpretation could increase the burden on developing states and reduce transfers, shifting the constraint towards a more uniform, less extractive (for developed states) model. A broader interpretation would solidify the current asymmetric distribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cbdr_rc_scope_ambiguity, conceptual, 'Ambiguity in the operational definition of CBDR-RC.').

omega_variable(
    equity_vs_effectiveness_tradeoff,
    'Does the emphasis on equity and differentiated responsibilities sufficiently enable the global collective action required to meet the Paris Agreement''s temperature goals, or does it hinder overall effectiveness?',
    'Empirical analysis of global emissions trajectories and climate impacts under the current CBDR-RC interpretation versus counterfactuals with more uniform, binding commitments.',
    'If found to significantly hinder effectiveness, pressure would mount to re-interpret NDCs towards a more supranational, less differentiated model, potentially shifting the constraint''s classification for developing states towards a payer role. If found to be effective, the equity reading would be further entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_vs_effectiveness_tradeoff, empirical, 'Trade-off between equity principles and global climate action effectiveness.').

omega_variable(
    mandate_drift_from_founding_problem,
    'Has the ''founding problem'' of historical inequity, which justified CBDR-RC, been sufficiently addressed such that the principle now primarily serves to maintain policy space for developing states rather than redress past harms?',
    'Independent historical and economic analysis of global emissions and development trajectories, assessing the extent to which historical responsibilities have been ''paid down'' through past transfers and differentiated action.',
    'If the founding problem is deemed ''dead'' or substantially resolved, the continued strong emphasis on CBDR-RC could be reclassified as a Snare for developed states, as its coordination function (addressing historical inequity) would have atrophied, leaving primarily extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_drift_from_founding_problem, empirical, 'Whether the original justification for CBDR-RC remains fully valid.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__equity_reading, 2015, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t2015, paris_article_4_ndc__equity_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(pari_tr_t2018, paris_article_4_ndc__equity_reading, theater_ratio, 2018, 0.17).
narrative_ontology:measurement(pari_tr_t2021, paris_article_4_ndc__equity_reading, theater_ratio, 2021, 0.19).
narrative_ontology:measurement(pari_tr_t2024, paris_article_4_ndc__equity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(pari_be_t2015, paris_article_4_ndc__equity_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(pari_be_t2018, paris_article_4_ndc__equity_reading, base_extractiveness, 2018, 0.42).
narrative_ontology:measurement(pari_be_t2021, paris_article_4_ndc__equity_reading, base_extractiveness, 2021, 0.44).
narrative_ontology:measurement(pari_be_t2024, paris_article_4_ndc__equity_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t2015, paris_article_4_ndc__equity_reading, suppression_requirement, 2015, 0.25).
narrative_ontology:measurement(pari_su_t2018, paris_article_4_ndc__equity_reading, suppression_requirement, 2018, 0.27).
narrative_ontology:measurement(pari_su_t2021, paris_article_4_ndc__equity_reading, suppression_requirement, 2021, 0.29).
narrative_ontology:measurement(pari_su_t2024, paris_article_4_ndc__equity_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__equity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_agreement_finance_mechanisms).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, global_carbon_markets).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'paris_article_4_ndc' kernel. The 'equity_reading' emphasizes Common But Differentiated Responsibilities and Respective Capabilities (CBDR-RC), leading to asymmetric burdens. The 'sovereigntist_reading' (a Rope) views NDCs as voluntary pledges, while the 'supranational_reading' (a Snare) interprets them as binding, ratcheting commitments with international accountability. Each reading has distinct ε values and stakeholder dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
