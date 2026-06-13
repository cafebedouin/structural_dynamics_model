% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__conditional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__conditional_sovereignty, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: westphalian_sovereignty__conditional_sovereignty
 *   human_readable: Conditional Sovereignty Doctrine (Responsibility to Protect)
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This constraint represents the 'conditional sovereignty' reading of the
 *   Westphalian sovereignty kernel, often associated with the Responsibility
 *   to Protect (R2P) doctrine. It posits that state sovereignty is not
 *   absolute but conditional on a state's fulfillment of its responsibility
 *   to protect its own population from mass atrocities. Failure to do so
 *   triggers a legitimate right, and even obligation, for external
 *   intervention. This reading emerged in response to the failures of
 *   non-intervention in the late 20th century. It is claimed as a snare
 *   because it extracts autonomy from states that violate human rights,
 *   enforced by the threat of international action.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__conditional_sovereignty, 0.4).
domain_priors:suppression_score(westphalian_sovereignty__conditional_sovereignty, 0.6).
domain_priors:theater_ratio(westphalian_sovereignty__conditional_sovereignty, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, extractiveness, 0.4).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__conditional_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__conditional_sovereignty, "Conditional Sovereignty Doctrine (Responsibility to Protect)").
narrative_ontology:topic_domain(westphalian_sovereignty__conditional_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__conditional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__conditional_sovereignty, '1608fced-acf8-4241-9ba3-1a9c314fcada').
narrative_ontology:cs_kernel_codification('1608fced-acf8-4241-9ba3-1a9c314fcada', formalized).
narrative_ontology:cs_authority_grounding('1608fced-acf8-4241-9ba3-1a9c314fcada', lineage).
narrative_ontology:cs_interpretation_layer_present('1608fced-acf8-4241-9ba3-1a9c314fcada').
narrative_ontology:cs_reading_relation('1608fced-acf8-4241-9ba3-1a9c314fcada', westphalian_sovereignty__absolute_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('1608fced-acf8-4241-9ba3-1a9c314fcada', westphalian_sovereignty__graduated_sovereignty, coexists_with).
narrative_ontology:cs_axiom('1608fced-acf8-4241-9ba3-1a9c314fcada', foundational, sovereignty_entails_responsibility).
narrative_ontology:cs_axiom_status(sovereignty_entails_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('1608fced-acf8-4241-9ba3-1a9c314fcada', sovereignty_entails_responsibility, deontological).
narrative_ontology:cs_axiom('1608fced-acf8-4241-9ba3-1a9c314fcada', foundational, international_community_has_right_to_intervene).
narrative_ontology:cs_axiom_status(international_community_has_right_to_intervene, holdable).
narrative_ontology:cs_axiom_grounding('1608fced-acf8-4241-9ba3-1a9c314fcada', international_community_has_right_to_intervene, conventional).
narrative_ontology:cs_reference_frame('1608fced-acf8-4241-9ba3-1a9c314fcada', post_cold_war_humanitarian_intervention_consensus).
narrative_ontology:cs_drift_state('1608fced-acf8-4241-9ba3-1a9c314fcada', contemporary_multipolar_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('1608fced-acf8-4241-9ba3-1a9c314fcada', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, international_intervention_advocates).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, human_rights_organizations).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, sovereign_states_committing_violations).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, populations_under_intervention).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, populations_under_intervention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These actors (e.g., certain UN bodies, NGOs, academic proponents of R2P) champion the doctrine, define the thresholds for intervention, and lobby for its enforcement. They benefit from the expanded legitimacy for external action.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, international_intervention_advocates, agenda_setter,
    institutional, generational, mobile, global).

% States whose domestic actions are deemed to violate human rights systematically face the threat or reality of external intervention, losing autonomy and potentially facing regime change. Their options are to cease violations or resist intervention.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, sovereign_states_committing_violations, payer,
    powerful, immediate, trapped, national).

% These organizations benefit from the doctrine as it provides a legal and moral framework to advocate for the protection of populations and to hold states accountable. Their reports and advocacy often serve as triggers for intervention discussions.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, human_rights_organizations, beneficiary,
    organized, biographical, mobile, global).

% While ostensibly the beneficiaries of protection, these populations often bear the immediate costs of intervention, including violence, displacement, and disruption of social order. Their long-term benefit is contingent on the success and nature of the intervention.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, populations_under_intervention, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, populations_under_intervention, beneficiary).

% These states (often those prioritizing state sovereignty or fearing precedent for their own domestic affairs) resist the doctrine's application, arguing it undermines international law and can be selectively applied. They are often outvoted or bypassed in international forums.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, states_opposed_to_intervention, excluded,
    institutional, generational, constrained, global).

% Analyze the legal basis, implementation, and consequences of conditional sovereignty, debating its consistency with existing international law and its effectiveness in practice. They provide critical commentary and shape the intellectual discourse.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for international actors to coordinate responses to mass atrocities, aiming to prevent or halt systematic human rights violations when individual states fail to protect their own populations.
% TRANSFER_FUNCTION: Transfers the right to intervene (and the associated costs and risks) from individual states to the international community, and transfers a portion of state autonomy from violating states to external actors. It also transfers resources (military, humanitarian) into intervention zones.
% ABSENT_VOICES: Populations in states that fear intervention, or those who believe that external intervention is a greater evil than domestic oppression, are often marginalized. Their voices would highlight the potential for abuse, neo-colonialism, or unintended consequences of intervention.
% DISAPPEARANCE_RATIONALE: If conditional sovereignty vanished, the international community would revert to a stricter interpretation of non-interference, making coordinated responses to mass atrocities much harder. States would regain absolute domestic autonomy, but populations facing severe human rights abuses would lose a potential (albeit imperfect) avenue for protection.
% FOUNDING_PROBLEM: The failure of the international community to prevent or respond effectively to genocides and mass atrocities (e.g., Rwanda, Srebrenica) in the late 20th century, leading to calls for a redefinition of sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: The problem is widely attested by human rights organizations, international legal bodies, and historical commissions. While its application remains contested, the underlying problem of state failure to protect populations from mass atrocities is still a live concern, corroborated by ongoing conflicts and humanitarian crises.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__conditional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__conditional_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__conditional_sovereignty, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(westphalian_sovereignty__conditional_sovereignty, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__conditional_sovereignty_tests).
:- end_tests(westphalian_sovereignty__conditional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.4) because it imposes a significant, non-negotiable duty on states, backed by the threat of force, but only under specific, high-threshold conditions. Suppression is moderate (0.6) as it requires active enforcement (diplomatic pressure, sanctions, military intervention) against resistant states. Theater ratio is low (0.2) as the doctrine's application, while often debated, is generally a serious, functional attempt to address atrocities, not mere performance. Resistance is high (0.7) due to strong opposition from states prioritizing traditional notions of absolute sovereignty.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states committing violations, this is a clear snare, an illegitimate infringement on their sovereignty. From the perspective of intervention advocates, it is a necessary, albeit difficult, coordination mechanism to uphold universal human rights. The engine's classification as a snare reflects the coercive nature of the constraint on the target states.
 *
 * DIRECTIONALITY LOGIC:
 *   International intervention advocates and human rights organizations are beneficiaries, as the doctrine legitimizes their calls for action and expands their influence (d near 0.0-0.2). Sovereign states committing violations are clear targets, facing loss of autonomy and potential intervention (d near 0.8-1.0). Populations under intervention are complex: they are victims of the initial violations and often bear the immediate costs of intervention, but are also the ultimate intended beneficiaries of protection (d near 0.5-0.7, depending on the outcome). States opposed to intervention are excluded, as their arguments for absolute sovereignty are structurally sidelined by this doctrine.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intervention_selectivity_bias,
    'Is the application of conditional sovereignty and subsequent intervention consistently applied based on human rights violations, or is it selectively applied based on geopolitical interests?',
    'Empirical analysis of all cases of systematic human rights violations over a decade, comparing those that triggered intervention with those that did not, controlling for severity and capacity.',
    'If highly selective, the effective extractiveness on target states is amplified by perceived injustice and the doctrine''s legitimacy is undermined, potentially shifting its classification towards a more purely extractive snare or even a piton if its original mandate is seen as theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_selectivity_bias, empirical, 'Bias in intervention decisions.').

omega_variable(
    long_term_impact_on_populations,
    'Do external interventions consistently lead to improved long-term human rights outcomes and stability for the populations they aim to protect, or do they often result in prolonged instability and unintended negative consequences?',
    'Longitudinal studies comparing post-intervention states with non-intervened states facing similar initial conditions, across multiple metrics of human rights, governance, and economic stability.',
    'If interventions consistently fail to improve long-term outcomes, the ''beneficiary'' role of populations becomes highly questionable, increasing the effective extractiveness on them and potentially reclassifying the constraint as a more severe snare for all involved, as the coordination function''s justification collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_impact_on_populations, empirical, 'Effectiveness of intervention in achieving its stated goals.').

omega_variable(
    absolute_vs_conditional_sovereignty_framing,
    'Is the concept of sovereignty fundamentally absolute (as per traditional Westphalian principles) or inherently conditional on state behavior (as per R2P)?',
    'This is a conceptual debate rooted in different philosophical and legal traditions; it cannot be resolved empirically. Resolution depends on which foundational principles of international order are prioritized.',
    'If an absolute sovereignty framing were adopted, this constraint would be reclassified as an illegitimate snare from all state perspectives, as its very premise would be rejected. If the conditional framing is universally accepted, the constraint might move towards a tangled rope, as the ''extraction'' becomes a universally accepted cost of membership in the international system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolute_vs_conditional_sovereignty_framing, conceptual, 'Conceptual debate over the nature of state sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__conditional_sovereignty, 1999, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1999, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 1999, 0.1).
narrative_ontology:measurement(west_tr_t2004, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2004, 0.15).
narrative_ontology:measurement(west_tr_t2009, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2009, 0.2).
narrative_ontology:measurement(west_tr_t2014, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2014, 0.25).
narrative_ontology:measurement(west_tr_t2019, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2019, 0.2).
narrative_ontology:measurement(west_tr_t2024, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(west_be_t1999, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 1999, 0.3).
narrative_ontology:measurement(west_be_t2004, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2004, 0.35).
narrative_ontology:measurement(west_be_t2009, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2009, 0.4).
narrative_ontology:measurement(west_be_t2014, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2014, 0.42).
narrative_ontology:measurement(west_be_t2019, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2019, 0.4).
narrative_ontology:measurement(west_be_t2024, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1999, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 1999, 0.5).
narrative_ontology:measurement(west_su_t2004, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2004, 0.55).
narrative_ontology:measurement(west_su_t2009, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2009, 0.6).
narrative_ontology:measurement(west_su_t2014, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2014, 0.65).
narrative_ontology:measurement(west_su_t2019, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement(west_su_t2024, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__conditional_sovereignty, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'westphalian_sovereignty' kernel. Other readings include 'absolute_sovereignty' and 'graduated_sovereignty'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
