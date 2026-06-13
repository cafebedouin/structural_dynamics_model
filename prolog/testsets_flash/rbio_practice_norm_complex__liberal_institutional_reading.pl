% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__liberal_institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__liberal_institutional_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rbio_practice_norm_complex__liberal_institutional_reading
 *   human_readable: Liberal Institutional Reading of RBIO Practice Norms
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'liberal institutional' reading of norms
 *   governing the Responsibility to Protect (R2P), humanitarian intervention,
 *   and economic conditionality in international relations. It posits that
 *   these norms are universal, derive legitimacy from state consent and
 *   multilateral processes (e.g., UNSC authorization), and are genuinely
 *   revisable. Selective enforcement is attributed to capacity limitations,
 *   not a fundamental flaw in legitimacy or an extractive agenda.
 *   Intervention is justified when authorized by the UNSC or in cases of
 *   grave atrocities; economic conditionality is seen as acceptable
 *   contractual terms. Beneficiaries include intervening states,
 *   international contractors, and international organizations, while
 *   targeted states and civilian populations under sanctions are victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, 0.45).
domain_priors:suppression_score(rbio_practice_norm_complex__liberal_institutional_reading, 0.3).
domain_priors:theater_ratio(rbio_practice_norm_complex__liberal_institutional_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__liberal_institutional_reading, rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__liberal_institutional_reading, "Liberal Institutional Reading of RBIO Practice Norms").
narrative_ontology:topic_domain(rbio_practice_norm_complex__liberal_institutional_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__liberal_institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__liberal_institutional_reading, '831d737a-d35d-4379-a2c1-bd11754d5a8e').
narrative_ontology:cs_kernel_codification('831d737a-d35d-4379-a2c1-bd11754d5a8e', formalized).
narrative_ontology:cs_authority_grounding('831d737a-d35d-4379-a2c1-bd11754d5a8e', lineage).
narrative_ontology:cs_interpretation_layer_present('831d737a-d35d-4379-a2c1-bd11754d5a8e').
narrative_ontology:cs_reading_relation('831d737a-d35d-4379-a2c1-bd11754d5a8e', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('831d737a-d35d-4379-a2c1-bd11754d5a8e', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('831d737a-d35d-4379-a2c1-bd11754d5a8e', foundational, universal_human_rights_transcend_sovereignty).
narrative_ontology:cs_axiom_status(universal_human_rights_transcend_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('831d737a-d35d-4379-a2c1-bd11754d5a8e', universal_human_rights_transcend_sovereignty, deontological).
narrative_ontology:cs_axiom('831d737a-d35d-4379-a2c1-bd11754d5a8e', foundational, multilateral_consent_confers_legitimacy).
narrative_ontology:cs_axiom_status(multilateral_consent_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('831d737a-d35d-4379-a2c1-bd11754d5a8e', multilateral_consent_confers_legitimacy, conventional).
narrative_ontology:cs_reference_frame('831d737a-d35d-4379-a2c1-bd11754d5a8e', post_cold_war_liberal_order).
narrative_ontology:cs_drift_state('831d737a-d35d-4379-a2c1-bd11754d5a8e', contemporary_multipolar_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('831d737a-d35d-4379-a2c1-bd11754d5a8e', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, international_contractors).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, international_organizations).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_under_sanctions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that participate in or authorize interventions, often citing humanitarian or security justifications. They benefit from upholding international order, projecting influence, and sometimes from post-intervention contracts or resource access. They operate within the framework of international law but can choose to engage or not.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states, beneficiary,
    institutional, generational, mobile, global).

% Bodies like the UN Security Council, IMF, or World Bank that authorize, implement, or oversee RBIO norms and interventions. They benefit from their expanded mandate and legitimacy in global governance, but are constrained by member state interests and veto powers.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, international_organizations, agenda_setter,
    institutional, civilizational, constrained, global).

% States that are subject to interventions, sanctions, or economic conditionalities. They bear the direct costs of these actions, including loss of sovereignty, economic disruption, and internal instability. Their exit options are severely limited, often to compliance or collapse.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states, payer,
    powerless, generational, trapped, national).

% Citizens within targeted states who suffer the humanitarian consequences of sanctions or conflict, including poverty, lack of access to essential goods, and displacement. They have virtually no exit options from the effects of these international actions.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_under_sanctions, payer,
    powerless, biographical, trapped, national).

% Private companies that secure contracts for reconstruction, security, or resource extraction in post-intervention or sanction-affected areas. They benefit directly from the economic opportunities created by RBIO actions and have high mobility to pursue such opportunities.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, international_contractors, beneficiary,
    organized, immediate, arbitrage, global).

% Non-governmental organizations and activists who monitor compliance with human rights and international law. They advocate for interventions in cases of grave atrocities and scrutinize the conduct of intervening powers, influencing public opinion and policy debates.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, human_rights_advocates, observer,
    moderate, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international responses to humanitarian crises and threats to international peace and security, establishing a framework for collective action and legitimate intervention where state sovereignty is deemed to have failed its responsibility.
% TRANSFER_FUNCTION: Transfers authority for intervention from individual states to multilateral bodies (e.g., UNSC) in exchange for legitimacy; transfers resources and influence to intervening states and international organizations; transfers costs (sovereignty, economic stability, human suffering) to targeted states and their populations.
% ABSENT_VOICES: States and non-state actors who view these norms as illegitimate infringements on sovereignty or as tools of neo-colonialism are often marginalized in multilateral forums where these norms are debated and applied. They would argue for strict non-interference and self-determination.
% DISAPPEARANCE_RATIONALE: If these norms vanished, the international system would revert to a more purely Westphalian model, with less justification for humanitarian intervention or economic conditionality. States would face fewer external constraints on internal affairs, but also fewer mechanisms for collective security and human rights protection. The roles of international organizations would diminish significantly.
% FOUNDING_PROBLEM: The problem of states committing grave atrocities against their own populations, and the inability of the international community to respond effectively without violating traditional notions of sovereignty, leading to widespread human suffering and regional instability.
% FOUNDING_PROBLEM_CORROBORATION: The problem is widely attested by human rights organizations, international legal scholars, and many states, who continue to document ongoing atrocities and advocate for effective international responses. While the specific mechanisms are contested, the underlying problem of state failure to protect its population remains live.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__liberal_institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__liberal_institutional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__liberal_institutional_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rbio_practice_norm_complex__liberal_institutional_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).
:- end_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the costs borne by targeted states and populations, even when interventions are deemed legitimate. Suppression (0.30) is moderate, as states retain formal sovereignty and can resist, though often at high cost. The theater ratio (0.20) is low, indicating that the stated multilateral processes and justifications for intervention are largely believed to be genuine within this reading, with minimal performative cover. Accessibility collapse (0.60) is moderate, as alternatives to compliance exist but are often costly. Resistance (0.40) is present from targeted states but is not seen as undermining the overall legitimacy of the norms within this framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of intervening states and international organizations (beneficiaries), these norms are a 'rope' that facilitates collective action for global stability and human rights. From the perspective of targeted states and their civilian populations (victims), the same norms, even when framed as legitimate, can feel extractive and coercive, potentially shifting towards a 'tangled_rope' or 'snare' depending on the specific context and perceived fairness of enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states and international contractors are beneficiaries (d near 0.0) as they gain influence, resources, or contracts from interventions and conditionality. Targeted states and civilian populations under sanctions are victims (d near 1.0) as they bear the direct costs of intervention, sanctions, or imposed conditionalities. International organizations are also beneficiaries (d near 0.0-0.2) as their mandate and influence are expanded by these norms. The 'liberal institutional' framing emphasizes the coordination function of these norms, even while acknowledging the costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling coordination as pure extraction by emphasizing the genuine multilateral processes and consent-based nature of the norms. It acknowledges that costs are borne, but frames them as necessary for upholding universal values or maintaining international order, rather than as purely extractive. The challenge is to ensure that the 'capacity problem' explanation for selective enforcement is not a cover for a 'legitimacy problem' that would indicate mandatrophy or a shift to a more extractive type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_vs_capacity_ambiguity,
    'Is enforcement selectivity of RBIO norms genuinely a capacity problem, or does it reflect a deeper legitimacy problem rooted in power asymmetries?',
    'Empirical analysis of enforcement patterns across different power configurations and resource levels; if selectivity persists even with ample capacity, reclassify as legitimacy-driven.',
    'If primarily a legitimacy problem, the constraint''s claimed ''rope'' nature is a cover for ''tangled_rope'' or ''snare'' from the perspective of targeted states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_vs_capacity_ambiguity, empirical, 'Ambiguity between capacity and legitimacy as drivers of enforcement selectivity.').

omega_variable(
    liberal_institutional_vs_hegemonic_extraction,
    'Is this constraint a genuine liberal institutional norm, or is it a reading that serves as a cover for hegemonic extraction?',
    'Analysis of the distribution of benefits and costs, and the actual revisability of norms in practice, especially concerning P5 veto power. If revision is systematically blocked for non-hegemonic interests, the hegemonic_extraction_reading is more accurate.',
    'If the hegemonic_extraction_reading is more accurate, the constraint shifts from ''rope'' to ''tangled_rope'' or ''snare'' for many actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liberal_institutional_vs_hegemonic_extraction, conceptual, 'This constraint is the ''liberal_institutional_reading'' of the ''rbio_practice_norm_complex'' kernel. The ''hegemonic_extraction_reading'' would emphasize the frozen nature of norms due to P5 veto and institutional path-dependency, and interpret enforcement selectivity as revealing extractive intent.').

omega_variable(
    liberal_institutional_vs_sovereignty_maximalist,
    'Is this constraint''s justification for intervention (UNSC authorization, grave atrocities) genuinely multilateral and consent-based, or does it infringe on state sovereignty as claimed by sovereignty maximalists?',
    'Legal and political analysis of state consent mechanisms and the interpretation of ''grave atrocities'' in practice. If humanitarian exceptions are consistently applied to achieve regime change, the sovereignty_maximalist_reading gains traction.',
    'If the sovereignty_maximalist_reading is more accurate, the constraint shifts from ''rope'' to ''snare'' for targeted states, as it would be seen as a pretext for interference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liberal_institutional_vs_sovereignty_maximalist, conceptual, 'This constraint is the ''liberal_institutional_reading'' of the ''rbio_practice_norm_complex'' kernel. The ''sovereignty_maximalist_reading'' would assert state sovereignty as absolute and view humanitarian exceptions as pretexts for regime change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__liberal_institutional_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t0, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(rbio_tr_t10, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(rbio_tr_t20, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(rbio_tr_t30, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(rbio_be_t0, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rbio_be_t10, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(rbio_be_t20, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 20, 0.43).
narrative_ontology:measurement(rbio_be_t30, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t0, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(rbio_su_t10, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(rbio_su_t20, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 20, 0.29).
narrative_ontology:measurement(rbio_su_t30, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 30, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__liberal_institutional_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'rbio_practice_norm_complex' kernel. The other readings are 'hegemonic_extraction_reading' and 'sovereignty_maximalist_reading', which offer alternative interpretations of the same international norms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
