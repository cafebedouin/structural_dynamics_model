% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__hybrid_effective_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__hybrid_effective_control_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__hybrid_effective_control_reading
 *   human_readable: UNCLOS Maritime Sovereignty: Hybrid Effective Control Reading
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint represents a 'hybrid' reading of UNCLOS maritime
 *   sovereignty, where natural features grant full maritime zones, but
 *   artificial features, while initially only having safety zones, can mature
 *   into territorial claims through prolonged, unchallenged effective
 *   control. This interpretation is a departure from a strict geographic
 *   reading and is often advanced by states with significant construction and
 *   naval capabilities. It sits between the strict geographic interpretation
 *   and a fully expansive view of artificial island claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.58).
domain_priors:suppression_score(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.65).
domain_priors:theater_ratio(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__hybrid_effective_control_reading, "UNCLOS Maritime Sovereignty: Hybrid Effective Control Reading").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__hybrid_effective_control_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__hybrid_effective_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__hybrid_effective_control_reading, '6ea2963a-b387-49fe-ad56-d2f9646a2562').
narrative_ontology:cs_kernel_codification('6ea2963a-b387-49fe-ad56-d2f9646a2562', fixed_text).
narrative_ontology:cs_authority_grounding('6ea2963a-b387-49fe-ad56-d2f9646a2562', lineage).
narrative_ontology:cs_interpretation_layer_present('6ea2963a-b387-49fe-ad56-d2f9646a2562').
narrative_ontology:cs_reading_relation('6ea2963a-b387-49fe-ad56-d2f9646a2562', unclos_maritime_sovereignty__strict_geographic_reading, coexists_with).
narrative_ontology:cs_reading_relation('6ea2963a-b387-49fe-ad56-d2f9646a2562', unclos_maritime_sovereignty__expansive_construction_reading, coexists_with).
narrative_ontology:cs_axiom('6ea2963a-b387-49fe-ad56-d2f9646a2562', foundational, effective_control_as_legitimacy_factor).
narrative_ontology:cs_axiom_status(effective_control_as_legitimacy_factor, holdable).
narrative_ontology:cs_axiom_grounding('6ea2963a-b387-49fe-ad56-d2f9646a2562', effective_control_as_legitimacy_factor, conventional).
narrative_ontology:cs_axiom('6ea2963a-b387-49fe-ad56-d2f9646a2562', foundational, graduated_maritime_rights).
narrative_ontology:cs_axiom_status(graduated_maritime_rights, holdable).
narrative_ontology:cs_axiom_grounding('6ea2963a-b387-49fe-ad56-d2f9646a2562', graduated_maritime_rights, conventional).
narrative_ontology:cs_reference_frame('6ea2963a-b387-49fe-ad56-d2f9646a2562', unclos_1982_framework).
narrative_ontology:cs_drift_state('6ea2963a-b387-49fe-ad56-d2f9646a2562', contemporary_south_china_sea_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6ea2963a-b387-49fe-ad56-d2f9646a2562', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_regional_power_projection).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_competing_claims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_signatories).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_signatories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states possess the technological and financial means to construct artificial features in disputed maritime areas, leveraging this reading to potentially expand their territorial claims and control over resources.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity, beneficiary,
    institutional, generational, mobile, global).

% These states can enforce 'prolonged effective control absent challenge' through naval presence and diplomatic pressure, effectively shaping the interpretation and application of maritime law in their favor.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_regional_power_projection, agenda_setter,
    institutional, generational, arbitrage, global).

% These states lack the military or diplomatic capacity to effectively challenge the claims made by more powerful states based on artificial features and effective control, leading to a loss of potential maritime rights.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants, payer,
    powerless, generational, trapped, regional).

% These states have legitimate claims to maritime areas but find their positions undermined by the 'effective control' principle, which favors those who can physically occupy and defend features, regardless of their natural origin.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_competing_claims, payer,
    moderate, generational, constrained, regional).

% These experts analyze state practice and legal precedents, often highlighting the divergence between strict UNCLOS interpretation and the evolving 'effective control' doctrine, but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% All states benefit from the general framework of UNCLOS for maritime order, but this specific reading introduces ambiguities that can lead to disputes and undermine the predictability of the law for many.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_signatories, beneficiary,
    organized, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_signatories, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a flexible, albeit contested, framework for states to assert and manage maritime zones, attempting to balance traditional geographic principles with the realities of state practice and technological capabilities.
% TRANSFER_FUNCTION: Transfers potential maritime sovereignty, resource rights, and strategic control from international waters or contested zones to states capable of constructing and maintaining prolonged effective control over features, even artificial ones.
% ABSENT_VOICES: Small island developing states, indigenous communities whose traditional fishing grounds are impacted by expanding claims, and environmental organizations advocating for the protection of marine ecosystems from artificial construction and militarization.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal status of numerous artificial features and the claims based on them would be immediately invalidated. This would lead to a massive re-evaluation of maritime boundaries, increased disputes, and potentially military confrontations as states revert to stricter interpretations or attempt to re-assert claims under different pretexts.
% FOUNDING_PROBLEM: The original UNCLOS framework sought to establish a comprehensive and stable legal order for the oceans, resolving disputes over maritime boundaries and resource allocation based on clear geographic principles.
% FOUNDING_PROBLEM_CORROBORATION: While the overarching problem of maritime order remains live, this specific reading's status is contested. Powerful states and some legal scholars argue it's a necessary evolution of international law to accommodate modern realities. Weaker states and other scholars argue it undermines the original intent of UNCLOS and exacerbates power imbalances; this is corroborated by numerous diplomatic protests and international legal analyses from non-benefiting parties.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__hybrid_effective_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__hybrid_effective_control_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.58, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.58) because it allows powerful states to gain significant maritime advantages through construction and control, but it's not pure extraction as it still acknowledges the baseline of natural features and the initial limited status of artificial ones. Suppression is high (0.65) because the 'absent challenge' clause inherently relies on the ability to deter or overcome opposition, often through military or diplomatic means. The theater ratio is moderate (0.25) as there are genuine legal arguments and state practices involved, but these often serve to legitimize underlying power projection. The slight dip in extractiveness and suppression towards the end of the interval reflects increased international scrutiny and diplomatic pushback against the most aggressive interpretations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of powerful states, this reading is a pragmatic evolution of international law, reflecting modern capabilities and security needs. From the perspective of weaker states, it's an opportunistic interpretation that undermines the equitable principles of UNCLOS and legitimizes 'might makes right' in maritime governance.
 *
 * DIRECTIONALITY LOGIC:
 *   States with construction capacity and regional power projection are clear beneficiaries, as this reading provides a pathway for them to expand their maritime influence. Militarily weaker claimants and states with competing claims are victims, as their ability to contest these claims is limited. UNCLOS signatories are both beneficiaries (from the overall framework) and payers (from the ambiguities this reading introduces).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_effective_control,
    'What specific actions and duration constitute ''prolonged effective control'' sufficient to mature a claim, and what constitutes an ''effective challenge'' that prevents maturation?',
    'Analysis of state practice, international court rulings, and diplomatic responses to specific cases. A clear consensus on thresholds for control and challenge would resolve this ambiguity.',
    'If thresholds are high, fewer artificial features would mature into claims, reducing extraction. If thresholds are low or vague, more claims would mature, increasing extraction and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_effective_control, empirical, 'Ambiguity in the criteria for ''effective control'' and ''challenge'' in maritime claims.').

omega_variable(
    power_vs_law_legitimacy,
    'To what extent does this reading legitimize claims based on military and economic power projection rather than on universally accepted legal principles?',
    'Comparative analysis of claims made by states with varying power levels, and the outcomes of international arbitration or diplomatic negotiations. If outcomes consistently favor powerful states regardless of strict legal merit, it suggests power is the primary driver.',
    'If power is the primary driver, the constraint''s effective extractiveness and suppression are higher than the legal framing suggests, indicating a Snare-like operation under the guise of a legal framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(power_vs_law_legitimacy, conceptual, 'The role of power projection in shaping the legal interpretation of maritime claims.').

omega_variable(
    natural_vs_artificial_boundary,
    'Where is the precise legal boundary between a ''natural feature'' and an ''artificial feature'' that has been significantly modified or enhanced by human activity?',
    'Development of clearer international legal guidelines or precedents on the degree of human modification that alters a feature''s fundamental legal classification.',
    'A stricter definition would limit the scope for artificial features to gain territorial claims, reducing extraction. A looser definition would expand it, increasing extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_artificial_boundary, conceptual, 'Ambiguity in distinguishing natural from significantly modified artificial maritime features.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__hybrid_effective_control_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(uncl_tr_t1995, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(uncl_tr_t2005, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(uncl_tr_t2015, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(uncl_tr_t2024, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 1982, 0.45).
narrative_ontology:measurement(uncl_be_t1995, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement(uncl_be_t2005, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(uncl_be_t2015, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(uncl_be_t2024, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 1982, 0.5).
narrative_ontology:measurement(uncl_su_t1995, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(uncl_su_t2005, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement(uncl_su_t2015, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(uncl_su_t2024, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__hybrid_effective_control_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, south_china_sea_freedom_of_navigation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'unclos_maritime_sovereignty' kernel, focusing on a hybrid approach to artificial features and effective control. It influences and coexists with stricter and more expansive interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
