% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__normative_reading_drop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__normative_reading_drop, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: total_war_winnability_post1945__normative_reading_drop
 *   human_readable: Normative Prohibition of Total War (post-1945)
 *   domain: international_relations/strategic_studies/commitment_system
 *
 * SUMMARY:
 *   The constraint describes the post-1945 normative order that renders total
 *   war illegitimate through Article 2(4) of the UN Charter and the
 *   development of international humanitarian law. While total war remains
 *   physically possible (unlike the structural contraction reading), this
 *   reading holds that a coordination regime — treaties, customary law, and
 *   institutional practice — has successfully stigmatized and legally
 *   prohibited it. The constraint is claimed as a rope: a genuine
 *   coordination mechanism with minimal coercive overhead, benefiting global
 *   civilian populations and constraining revisionist powers. The engine will
 *   compute per-seat classifications from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__normative_reading_drop, 0.15).
domain_priors:suppression_score(total_war_winnability_post1945__normative_reading_drop, 0.1).
domain_priors:theater_ratio(total_war_winnability_post1945__normative_reading_drop, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, extractiveness, 0.15).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__normative_reading_drop, rope).
narrative_ontology:human_readable(total_war_winnability_post1945__normative_reading_drop, "Normative Prohibition of Total War (post-1945)").
narrative_ontology:topic_domain(total_war_winnability_post1945__normative_reading_drop, "international_relations/strategic_studies/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__normative_reading_drop, '2df317b2-615c-46dd-ad19-bdf9e70369ce').
narrative_ontology:cs_kernel_codification('2df317b2-615c-46dd-ad19-bdf9e70369ce', formalized).
narrative_ontology:cs_authority_grounding('2df317b2-615c-46dd-ad19-bdf9e70369ce', lineage).
narrative_ontology:cs_interpretation_layer_present('2df317b2-615c-46dd-ad19-bdf9e70369ce').
narrative_ontology:cs_reading_relation('2df317b2-615c-46dd-ad19-bdf9e70369ce', total_war_winnability_post1945__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('2df317b2-615c-46dd-ad19-bdf9e70369ce', total_war_winnability_post1945__strategic_culture_drift, coexists_with).
narrative_ontology:cs_axiom('2df317b2-615c-46dd-ad19-bdf9e70369ce', foundational, total_war_normatively_prohibited).
narrative_ontology:cs_axiom_status(total_war_normatively_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('2df317b2-615c-46dd-ad19-bdf9e70369ce', total_war_normatively_prohibited, conventional).
narrative_ontology:cs_axiom('2df317b2-615c-46dd-ad19-bdf9e70369ce', secondary, humanitarian_law_binds_all_parties).
narrative_ontology:cs_axiom_status(humanitarian_law_binds_all_parties, holdable).
narrative_ontology:cs_axiom_grounding('2df317b2-615c-46dd-ad19-bdf9e70369ce', humanitarian_law_binds_all_parties, conventional).
narrative_ontology:cs_reference_frame('2df317b2-615c-46dd-ad19-bdf9e70369ce', post_war_legal_order).
narrative_ontology:cs_drift_state('2df317b2-615c-46dd-ad19-bdf9e70369ce', contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2df317b2-615c-46dd-ad19-bdf9e70369ce', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, revisionist_powers).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__normative_reading_drop, un_charter_article_2_4).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__normative_reading_drop, humanitarian_law_normative_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Civilian populations worldwide benefit from the normative prohibition of total war, which reduces the likelihood of catastrophic warfare affecting them. They have no direct exit from the international system and no organized representation in the treaty regime.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations, beneficiary,
    powerless, generational, trapped, global).

% States that seek to pursue total war for strategic objectives find their options constrained by the normative framework. They bear the cost of forgone military options and face diplomatic and legal sanctions if they violate the norm.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, revisionist_powers, payer,
    powerful, biographical, constrained, global).

% The major powers that drafted the UN Charter and subsequent humanitarian law instruments set the agenda for the normative prohibition of total war. They continue to shape interpretation and enforcement through the UN Security Council and other bodies.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, great_powers, agenda_setter,
    institutional, generational, arbitrage, global).

% International institutions (UN, ICJ, ICC) monitor compliance, adjudicate disputes, and provide forums for norm enforcement. They do not directly benefit or pay but facilitate the coordination function.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, international_institutions, observer,
    institutional, generational, analytical, global).

% Non-state armed groups are bound by humanitarian law but were not parties to the treaty-making process. They are structurally excluded from the norm-setting but subject to its constraints.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, non_state_armed_groups, excluded,
    moderate, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The normative prohibition of total war solves the coordination problem of mutual restraint among states: each state refrains from total war because it expects others to do the same, avoiding a catastrophic security dilemma.
% TRANSFER_FUNCTION: The arrangement transfers the option of waging total war from revisionist powers (who lose the ability to use total war as a strategic tool) to global civilian populations (who gain reduced risk of catastrophic war).
% ABSENT_VOICES: Populations in conflict zones who experience war despite the norm, and future generations who would bear the costs of total war, are not directly represented in the treaty regime.
% DISAPPEARANCE_RATIONALE: If the normative prohibition vanished, the taboo against total war would erode, likely leading to increased militarization, lowered thresholds for catastrophic warfare, and a reorganization of international security around unrestricted force.
% FOUNDING_PROBLEM: The founding problem was the catastrophic experience of two world wars, particularly the unbounded destruction of total war, which motivated the creation of a normative and legal framework to prevent its recurrence.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the historical record of WWII and the explicit preamble of the UN Charter, corroborated by historians and international lawyers outside the beneficiary set.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__normative_reading_drop, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__normative_reading_drop, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__normative_reading_drop, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_winnability_post1945__normative_reading_drop, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__normative_reading_drop, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__normative_reading_drop_tests).
:- end_tests(total_war_winnability_post1945__normative_reading_drop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the norm primarily coordinates mutual restraint rather than extracting resources. Suppression is low (0.10) because compliance is largely voluntary and reputational, not enforced by a central coercive apparatus. Theater ratio is low (0.12) because the legal and diplomatic machinery serves the coordination function rather than performing a facade. Accessibility collapse is moderate (0.40) because alternatives (total war) remain physically available but are normatively closed off. Resistance is moderate (0.30) because revisionist powers periodically challenge the norm.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (great powers), the constraint is a successful coordination achievement they authored. From the payer seat (revisionist powers), it is a restriction on strategic autonomy. From the beneficiary seat (global civilians), it is a protective norm. The engine computes these divergences from the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Great powers (agenda_setter) are near the beneficiary end (d ~ 0.1) because they designed the regime and retain interpretive authority. Revisionist powers (payer) are near the target end (d ~ 0.8) because they bear the forgone-option costs. Global civilians (beneficiary) are at the beneficiary end (d ~ 0.0) with no exit. International institutions (observer) are analytical (d = 0.5). Non-state armed groups (excluded) are trapped (d ~ 0.9) but not direct parties to the treaty.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing total war) remains live, so mandatrophy is not resolved. The coordination function persists; the norm has not atrophied into a piton. The constraint remains a functional rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the total_war_winnability_post1945 kernel, or does it collapse into the structural contraction reading?',
    'Compare the empirical claims about physical possibility of total war; if nuclear weapons have made total war physically impossible, the normative reading''s premise is false.',
    'If the structural contraction reading is correct, this constraint''s ε would be near zero (mountain) rather than rope-level coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the normative reading''s claim of physical possibility holds against the structural contraction reading.').

omega_variable(
    normative_vs_material_causation,
    'Is the decline of total war primarily driven by normative illegitimacy or by material impossibility (nuclear deterrence)?',
    'Counterfactual analysis: would total war have returned absent nuclear weapons but with the normative framework intact?',
    'If material factors dominate, the coordination function of the norm is overstated; if normative factors dominate, the rope classification is robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(normative_vs_material_causation, empirical, 'Causal weight of normative vs. material factors in the decline of total war.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__normative_reading_drop, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(tota_tr_t1960, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(tota_tr_t1975, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1975, 0.05).
narrative_ontology:measurement(tota_tr_t1990, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1990, 0.07).
narrative_ontology:measurement(tota_tr_t2005, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(tota_tr_t2025, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(tota_be_t1960, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1960, 0.12).
narrative_ontology:measurement(tota_be_t1975, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1975, 0.1).
narrative_ontology:measurement(tota_be_t1990, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1990, 0.12).
narrative_ontology:measurement(tota_be_t2005, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2005, 0.15).
narrative_ontology:measurement(tota_be_t2025, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2025, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1945, 0.1).
narrative_ontology:measurement(tota_su_t1960, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1960, 0.08).
narrative_ontology:measurement(tota_su_t1975, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1975, 0.07).
narrative_ontology:measurement(tota_su_t1990, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1990, 0.08).
narrative_ontology:measurement(tota_su_t2005, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2005, 0.1).
narrative_ontology:measurement(tota_su_t2025, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2025, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__normative_reading_drop, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945__structural_contraction_reading).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945__strategic_culture_drift).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the kernel 'total_war_winnability_post1945' into three structurally distinct readings: normative prohibition (rope), structural contraction (mountain), and strategic culture drift (scaffold/tangled_rope). They share the same referent (the post-1945 decline of total war) but differ on the causal mechanism and thus on ε and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
