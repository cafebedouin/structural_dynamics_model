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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: paris_article_4_ndc__equity_reading
 *   human_readable: Paris Agreement Article 4 NDC Equity Reading (CBDR)
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'equity reading' of Article 4 of the Paris
 *   Agreement, which mandates that Nationally Determined Contributions (NDCs)
 *   must be interpreted through the principle of Common But Differentiated
 *   Responsibilities and Respective Capabilities (CBDR-RC). This reading
 *   requires structural distinctions between developed and developing states,
 *   leading to asymmetric obligations where developed states bear greater
 *   burdens for emissions reductions and financial transfers, while
 *   developing states retain policy space for growth. This reading is
 *   actively championed by equity coalitions and developing nations, often in
 *   contest with sovereigntist and supranational interpretations.
 *
 * KEY AGENTS:
 *   - Developed States: Primary targets/payers, constrained by obligations.
 *   - Developing States: Primary beneficiaries, retain policy space.
 *   - Equity Coalitions: Agenda-setters, actively enforce CBDR.
 *   - UNFCCC Secretariat: Institutional agenda-setter, administers the treaty.
 *   - Global Climate System: Analytical observer, ultimate referent.
 *   - Fossil Fuel Industry: Excluded, would prefer weaker NDCs.
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
narrative_ontology:human_readable(paris_article_4_ndc__equity_reading, "Paris Agreement Article 4 NDC Equity Reading (CBDR)").
narrative_ontology:topic_domain(paris_article_4_ndc__equity_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__equity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__equity_reading, '42c7f7c5-384b-406d-a690-2995f0612afd').
narrative_ontology:cs_kernel_codification('42c7f7c5-384b-406d-a690-2995f0612afd', fixed_text).
narrative_ontology:cs_authority_grounding('42c7f7c5-384b-406d-a690-2995f0612afd', lineage).
narrative_ontology:cs_interpretation_layer_present('42c7f7c5-384b-406d-a690-2995f0612afd').
narrative_ontology:cs_reading_relation('42c7f7c5-384b-406d-a690-2995f0612afd', paris_article_4_ndc__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('42c7f7c5-384b-406d-a690-2995f0612afd', paris_article_4_ndc__supranational_reading, coexists_with).
narrative_ontology:cs_axiom('42c7f7c5-384b-406d-a690-2995f0612afd', foundational, historical_responsibility_for_emissions).
narrative_ontology:cs_axiom_status(historical_responsibility_for_emissions, holdable).
narrative_ontology:cs_axiom_grounding('42c7f7c5-384b-406d-a690-2995f0612afd', historical_responsibility_for_emissions, deontological).
narrative_ontology:cs_axiom('42c7f7c5-384b-406d-a690-2995f0612afd', foundational, equitable_burden_sharing_principle).
narrative_ontology:cs_axiom_status(equitable_burden_sharing_principle, holdable).
narrative_ontology:cs_axiom_grounding('42c7f7c5-384b-406d-a690-2995f0612afd', equitable_burden_sharing_principle, deontological).
narrative_ontology:cs_reference_frame('42c7f7c5-384b-406d-a690-2995f0612afd', unfccc_equity_framework_1992).
narrative_ontology:cs_drift_state('42c7f7c5-384b-406d-a690-2995f0612afd', contemporary_climate_crisis, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('42c7f7c5-384b-406d-a690-2995f0612afd', '').
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

% Bear the primary burden of emissions reductions, financial transfers, and technology sharing under the CBDR principle. They often resist these obligations, viewing them as hindering economic competitiveness or infringing on sovereignty, but are bound by treaty and political pressure.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_states, payer,
    institutional, generational, constrained, global).

% Retain policy space for economic development, receive financial and technological support, and have less stringent emissions reduction targets. They actively defend the CBDR principle as essential for climate justice and their right to develop.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developing_states, beneficiary,
    organized, generational, mobile, global).

% Composed of developing states, civil society organizations, and climate justice advocates. They actively advocate for and enforce the CBDR principle, using their collective bargaining and veto power within international negotiations to ensure differentiated responsibilities are upheld.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, equity_coalitions, agenda_setter,
    organized, generational, constrained, global).

% Administers the Paris Agreement and facilitates negotiations. Its role is to implement the treaty, but its interpretation of CBDR is influenced by the political power dynamics of member states and equity coalitions.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, unfccc_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% The ultimate referent for climate action. Its stability is the goal of the constraint, but it is not an active agent in its enforcement or interpretation.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, global_climate_system, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(paris_article_4_ndc__equity_reading, global_climate_system).

% Would prefer weaker NDCs and no CBDR to avoid stranded assets and maintain demand for their products. They exert influence through lobbying and political donations but are not directly part of the treaty interpretation process.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, fossil_fuel_industry, excluded,
    powerful, biographical, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global climate action by assigning differentiated responsibilities and capacities for emissions reductions, financial transfers, and technology sharing, based on historical emissions and development status, to achieve collective climate goals.
% TRANSFER_FUNCTION: Transfers financial resources and technological support from developed to developing states, and policy space (flexibility in emissions targets) to developing states, while requiring more stringent action from developed states.
% ABSENT_VOICES: Future generations and non-human species, who bear the long-term consequences of climate inaction but lack direct representation in current international climate governance. Their interests are mediated through advocacy groups and scientific consensus.
% DISAPPEARANCE_RATIONALE: If the CBDR principle vanished overnight, the fragile consensus on global climate action would likely collapse. Developing states would reject undifferentiated obligations, leading to a breakdown in negotiations, increased blame-shifting, and a significant reduction in collective ambition, as the perceived injustice would undermine cooperation.
% FOUNDING_PROBLEM: The historical injustice of disproportionate greenhouse gas emissions by developed nations, leading to climate change, and the need for equitable burden-sharing in global climate action while allowing developing nations to pursue economic growth and poverty eradication.
% FOUNDING_PROBLEM_CORROBORATION: Developing states, climate justice organizations, and numerous academic analyses consistently corroborate the ongoing relevance of historical responsibility, differentiated capacity, and the need for equitable climate action. This is evidenced in UN reports, civil society advocacy, and scientific literature.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__equity_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__equity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__equity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) is moderate because while developed states face significant obligations, the overall framework aims for global coordination, not pure extraction. Suppression (0.55) is moderate, reflecting the active political and diplomatic pressure exerted by equity coalitions to uphold CBDR, which constrains developed states' policy choices. Theater ratio (0.20) is low, as the CBDR principle is a core, actively debated, and often enforced aspect of climate negotiations, not merely performative. Accessibility collapse (0.40) is moderate; developed states have constrained exit from their obligations, but developing states gain policy space, so alternatives are not fully collapsed for all parties. Resistance (0.60) is high, reflecting the ongoing contestation from developed states against these obligations and the strong advocacy from equity coalitions.
 *
 * PERSPECTIVAL GAP:
 *   Developed states experience this as a moderately extractive constraint, imposing costs and limiting policy options. Developing states, conversely, experience it as a beneficial coordination mechanism that provides necessary policy space and resources. Equity coalitions view it as a just and necessary framework for global cooperation. The engine's per-seat classification will reflect these divergent experiences based on the declared roles and positional atoms.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed states are the primary targets (payers) due to their obligations for emissions reductions and financial transfers. Developing states are beneficiaries, gaining policy space and resources. Equity coalitions act as agenda-setters, actively shaping and enforcing this interpretation. The UNFCCC Secretariat, while an agenda-setter, operates within the contested interpretations. The fossil fuel industry is excluded, as their interests are directly opposed to the constraint's aims.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling the CBDR principle as either pure coordination (Rope) or pure extraction (Snare). It acknowledges the genuine coordination function of enabling global climate action by addressing historical inequities, while also recognizing the asymmetric extraction from developed states. The active enforcement by equity coalitions and the ongoing resistance from developed states confirm its 'tangled' nature, where coordination and extraction are intertwined and require continuous maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cbdr_interpretation_ambiguity,
    'How precisely are ''common but differentiated responsibilities'' defined and applied in practice, particularly regarding the thresholds for ''developed'' vs. ''developing'' status and the scope of historical responsibility?',
    'Further international legal clarification, binding arbitration on specific cases, or a new global agreement that redefines differentiation criteria.',
    'A stricter definition could increase extraction from developed states and solidify policy space for developing states, potentially shifting the constraint closer to a Snare for the former. A looser definition could reduce extraction, making it more of a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cbdr_interpretation_ambiguity, conceptual, 'Ambiguity in the operational definition of CBDR-RC.').

omega_variable(
    transfer_sufficiency_empirical,
    'Are the financial and technological transfers from developed to developing states, as mandated by this reading, empirically sufficient to enable developing states to meet their climate goals without hindering their development?',
    'Independent audits of climate finance flows, empirical studies on technology transfer efficacy, and assessments of developing states'' capacity-building needs versus actual support received.',
    'If transfers are found to be insufficient, the ''beneficiary'' aspect for developing states is weakened, potentially increasing their effective extraction and making the constraint more purely extractive for developed states without achieving its coordination goal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_sufficiency_empirical, empirical, 'Empirical sufficiency of climate finance and technology transfers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__equity_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t1992, paris_article_4_ndc__equity_reading, theater_ratio, 1992, 0.3).
narrative_ontology:measurement(pari_tr_t2000, paris_article_4_ndc__equity_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(pari_tr_t2008, paris_article_4_ndc__equity_reading, theater_ratio, 2008, 0.25).
narrative_ontology:measurement(pari_tr_t2015, paris_article_4_ndc__equity_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(pari_tr_t2020, paris_article_4_ndc__equity_reading, theater_ratio, 2020, 0.21).
narrative_ontology:measurement(pari_tr_t2024, paris_article_4_ndc__equity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(pari_be_t1992, paris_article_4_ndc__equity_reading, base_extractiveness, 1992, 0.3).
narrative_ontology:measurement(pari_be_t2000, paris_article_4_ndc__equity_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(pari_be_t2008, paris_article_4_ndc__equity_reading, base_extractiveness, 2008, 0.4).
narrative_ontology:measurement(pari_be_t2015, paris_article_4_ndc__equity_reading, base_extractiveness, 2015, 0.43).
narrative_ontology:measurement(pari_be_t2020, paris_article_4_ndc__equity_reading, base_extractiveness, 2020, 0.44).
narrative_ontology:measurement(pari_be_t2024, paris_article_4_ndc__equity_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t1992, paris_article_4_ndc__equity_reading, suppression_requirement, 1992, 0.4).
narrative_ontology:measurement(pari_su_t2000, paris_article_4_ndc__equity_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(pari_su_t2008, paris_article_4_ndc__equity_reading, suppression_requirement, 2008, 0.5).
narrative_ontology:measurement(pari_su_t2015, paris_article_4_ndc__equity_reading, suppression_requirement, 2015, 0.53).
narrative_ontology:measurement(pari_su_t2020, paris_article_4_ndc__equity_reading, suppression_requirement, 2020, 0.54).
narrative_ontology:measurement(pari_su_t2024, paris_article_4_ndc__equity_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__equity_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'paris_article_4_ndc' kernel, focusing on the equity and CBDR-RC aspects. It is linked to the 'sovereigntist_reading' and 'supranational_reading' as part of a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
