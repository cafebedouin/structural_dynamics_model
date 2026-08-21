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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: paris_article_4_ndc__equity_reading
 *   human_readable: Paris Agreement Article 4 NDC: Equity Reading (CBDR)
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'equity reading' of Article 4 of the Paris
 *   Agreement, which mandates that Nationally Determined Contributions (NDCs)
 *   must be interpreted through the principle of Common But Differentiated
 *   Responsibilities and Respective Capabilities (CBDR-RC). This reading
 *   requires structural distinctions between developed and developing states,
 *   imposing greater obligations on developed states for emissions reductions
 *   and financial transfers, while preserving policy space for developing
 *   states. It is a contested interpretation, with other readings emphasizing
 *   national sovereignty or supranational enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, 0.45).
domain_priors:suppression_score(paris_article_4_ndc__equity_reading, 0.55).
domain_priors:theater_ratio(paris_article_4_ndc__equity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__equity_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__equity_reading, "Paris Agreement Article 4 NDC: Equity Reading (CBDR)").
narrative_ontology:topic_domain(paris_article_4_ndc__equity_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__equity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__equity_reading, '687f82e9-3654-451a-a3c6-d229e0625e23').
narrative_ontology:cs_kernel_codification('687f82e9-3654-451a-a3c6-d229e0625e23', fixed_text).
narrative_ontology:cs_authority_grounding('687f82e9-3654-451a-a3c6-d229e0625e23', lineage).
narrative_ontology:cs_interpretation_layer_present('687f82e9-3654-451a-a3c6-d229e0625e23').
narrative_ontology:cs_reading_relation('687f82e9-3654-451a-a3c6-d229e0625e23', paris_article_4_ndc__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('687f82e9-3654-451a-a3c6-d229e0625e23', paris_article_4_ndc__supranational_reading, coexists_with).
narrative_ontology:cs_axiom('687f82e9-3654-451a-a3c6-d229e0625e23', foundational, historical_responsibility_for_emissions).
narrative_ontology:cs_axiom_status(historical_responsibility_for_emissions, holdable).
narrative_ontology:cs_axiom_grounding('687f82e9-3654-451a-a3c6-d229e0625e23', historical_responsibility_for_emissions, deontological).
narrative_ontology:cs_axiom('687f82e9-3654-451a-a3c6-d229e0625e23', foundational, differentiated_capabilities_for_action).
narrative_ontology:cs_axiom_status(differentiated_capabilities_for_action, holdable).
narrative_ontology:cs_axiom_grounding('687f82e9-3654-451a-a3c6-d229e0625e23', differentiated_capabilities_for_action, empirically_contingent).
narrative_ontology:cs_reference_frame('687f82e9-3654-451a-a3c6-d229e0625e23', unfccc_equity_framework_1992).
narrative_ontology:cs_drift_state('687f82e9-3654-451a-a3c6-d229e0625e23', contemporary_paris_agreement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('687f82e9-3654-451a-a3c6-d229e0625e23', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__equity_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, developing_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, equity_coalitions).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the primary burden of emissions reductions and financial transfers, often resisting these obligations due to perceived economic costs and sovereignty concerns. Their policy space is constrained by historical responsibility.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_states, payer,
    powerful, generational, constrained, global).

% Benefit from policy space for economic development and are entitled to financial and technological support from developed states. They actively defend the CBDR principle to ensure equitable burden-sharing.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developing_states, beneficiary,
    organized, generational, constrained, global).

% Composed of developing states and civil society organizations, these coalitions advocate for and actively enforce the CBDR principle, gaining influence in international negotiations and ensuring that transfers and policy space are maintained for their constituents.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, equity_coalitions, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__equity_reading, equity_coalitions, beneficiary).

% Administers the Paris Agreement, including NDC submissions and interpretations of CBDR. It facilitates negotiations and reporting but lacks direct enforcement power, relying on state compliance and peer pressure.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, unfccc_secretariat, agenda_setter,
    institutional, generational, analytical, global).

% The ultimate referent for climate action. Its stability is the overarching goal, but it is a non-agent entity that cannot directly participate in the constraint's operation.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, global_climate_system, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(paris_article_4_ndc__equity_reading, global_climate_system).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__equity_reading, developing_states).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__equity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global climate action by acknowledging historical responsibility and differing national capabilities, thereby enabling broader and more equitable participation from all states in addressing climate change.
% TRANSFER_FUNCTION: Moves financial and technological resources from developed to developing states, and grants policy space to developing states, in exchange for their participation in the global climate regime. It also transfers political legitimacy to equity-focused interpretations of climate action.
% ABSENT_VOICES: Future generations and non-human species, who bear the long-term consequences of climate inaction, lack direct representation. Their interests are partially articulated by some equity coalitions and scientific bodies, but often diluted in state-centric negotiations.
% DISAPPEARANCE_RATIONALE: If the CBDR principle vanished overnight, the Paris Agreement's framework for NDCs would likely collapse. Developing states would withdraw or refuse to enhance their commitments without equity considerations, leading to a breakdown of global climate cooperation and a fragmented, ineffective response to climate change.
% FOUNDING_PROBLEM: How to achieve universal participation in climate action while addressing historical injustices, vast disparities in development, and differing capacities among states to mitigate and adapt to climate change.
% FOUNDING_PROBLEM_CORROBORATION: Developing states, numerous academic analyses, and reports from UN bodies consistently corroborate the ongoing relevance of historical responsibility and capacity gaps. Developed states often contest the extent of their current obligations, but the underlying problem of differentiated responsibility remains central to negotiations.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__equity_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__equity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__equity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(paris_article_4_ndc__equity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__equity_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates global climate action (a collective-action problem) but does so with asymmetric extraction. Developed states (victims) bear a disproportionate burden of mitigation and finance, while developing states (beneficiaries) gain policy space and resources. Extractiveness is moderate (0.45) because while developed states pay, the overall goal is global benefit, and developing states retain agency. Suppression is moderate (0.55) as equity coalitions actively enforce this interpretation, and developed states face diplomatic pressure and reputational costs for non-compliance. Theater ratio is low (0.20) as the CBDR principle is a live, actively debated, and implemented aspect of climate governance, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   Developed states often perceive this interpretation as an unfair burden, hindering their economic competitiveness. Developing states, conversely, view it as a fundamental principle of climate justice, essential for addressing historical emissions and enabling their sustainable development. The engine's per-seat classification will reflect this divergence, with developed states experiencing higher effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed states are targets (payers) as they bear the primary costs and constraints. Developing states are beneficiaries, receiving policy space and financial support. Equity coalitions act as agenda-setters, actively shaping and enforcing this interpretation, thus benefiting from its operation and gaining political leverage. The UNFCCC Secretariat, while an agenda-setter, operates more as an analytical observer in this specific reading, facilitating rather than directly benefiting from the asymmetric transfers.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the CBDR principle as pure extraction (Snare) by acknowledging its genuine coordination function in enabling universal participation in climate action. Conversely, it avoids mislabeling it as pure coordination (Rope) by recognizing the active enforcement and asymmetric extraction it imposes on developed states, which is often resisted. The 'live' status of the founding problem, corroborated by developing states, further supports its ongoing relevance, preventing a Piton classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equity_vs_responsibility_ambiguity,
    'Is the CBDR principle primarily a mechanism for genuine climate equity and justice, or is it increasingly used by some developing states to avoid responsibility for their own rising emissions?',
    'Empirical analysis of NDC ambition and implementation in developing states, correlated with their economic development and historical emissions trajectories, alongside a review of financial and technological transfer effectiveness.',
    'If the latter, the effective extraction from developed states might be higher than justified by pure equity, potentially shifting the constraint''s classification closer to a Snare for developed states, or indicating a drift towards a less functional Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_vs_responsibility_ambiguity, conceptual, 'Ambiguity regarding the primary function of CBDR-RC in contemporary climate governance.').

omega_variable(
    enforcement_sufficiency_ambiguity,
    'Are the current enforcement mechanisms (diplomatic pressure, peer review, reputational costs) sufficient to ensure developed states meet their CBDR-RC obligations for mitigation and finance, or are they largely symbolic?',
    'Tracking of actual financial transfers, technology diffusion, and developed state NDC achievement against their historical responsibility and capacity, coupled with analysis of compliance mechanisms'' effectiveness.',
    'If enforcement is largely symbolic, the constraint''s effective suppression is lower than measured, and its ''tangled_rope'' classification might drift towards a ''piton'' or a ''snare'' (if the coordination story is cover for inaction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_sufficiency_ambiguity, empirical, 'Effectiveness of enforcement mechanisms for CBDR-RC obligations.').

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading of the ''paris_article_4_ndc'' kernel. What would change structurally if a sibling reading (e.g., ''sovereigntist_reading'' or ''supranational_reading'') were adopted?',
    'Analysis of policy outcomes and state behavior under alternative interpretive frameworks, or a shift in the dominant legal/political interpretation of Article 4.',
    'Adoption of the ''sovereigntist_reading'' would likely reduce extractiveness and suppression on developed states, potentially shifting this constraint towards a ''rope'' or even ''piton'' due to reduced enforcement. Adoption of the ''supranational_reading'' would likely increase extractiveness and suppression on all states, potentially shifting this constraint towards a ''snare'' or a more coercive ''tangled_rope'' for all parties.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural implications of alternative readings of the Paris Agreement''s NDC kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__equity_reading, 1992, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t1992, paris_article_4_ndc__equity_reading, theater_ratio, 1992, 0.3).
narrative_ontology:measurement(pari_tr_t2005, paris_article_4_ndc__equity_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(pari_tr_t2015, paris_article_4_ndc__equity_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(pari_tr_t2025, paris_article_4_ndc__equity_reading, theater_ratio, 2025, 0.2).
narrative_ontology:measurement(pari_tr_t2035, paris_article_4_ndc__equity_reading, theater_ratio, 2035, 0.2).
narrative_ontology:measurement(pari_tr_t2050, paris_article_4_ndc__equity_reading, theater_ratio, 2050, 0.2).

% Extraction over time
narrative_ontology:measurement(pari_be_t1992, paris_article_4_ndc__equity_reading, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement(pari_be_t2005, paris_article_4_ndc__equity_reading, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement(pari_be_t2015, paris_article_4_ndc__equity_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(pari_be_t2025, paris_article_4_ndc__equity_reading, base_extractiveness, 2025, 0.44).
narrative_ontology:measurement(pari_be_t2035, paris_article_4_ndc__equity_reading, base_extractiveness, 2035, 0.45).
narrative_ontology:measurement(pari_be_t2050, paris_article_4_ndc__equity_reading, base_extractiveness, 2050, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t1992, paris_article_4_ndc__equity_reading, suppression_requirement, 1992, 0.4).
narrative_ontology:measurement(pari_su_t2005, paris_article_4_ndc__equity_reading, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(pari_su_t2015, paris_article_4_ndc__equity_reading, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(pari_su_t2025, paris_article_4_ndc__equity_reading, suppression_requirement, 2025, 0.53).
narrative_ontology:measurement(pari_su_t2035, paris_article_4_ndc__equity_reading, suppression_requirement, 2035, 0.55).
narrative_ontology:measurement(pari_su_t2050, paris_article_4_ndc__equity_reading, suppression_requirement, 2050, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__equity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_agreement_ndc_ratcheting_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, global_climate_finance_flows).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__supranational_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'paris_article_4_ndc' kernel. Each reading has a unique structural profile and ε value, and they are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
