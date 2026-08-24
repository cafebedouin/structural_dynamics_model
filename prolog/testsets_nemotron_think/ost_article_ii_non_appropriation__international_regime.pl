% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__international_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__international_regime, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: ost_article_ii_non_appropriation__international_regime
 *   human_readable: OST Article II Non-Appropriation Deferral to International Regime
 *   domain: international_space_law/treaty_interpretation/commons_governance
 *
 * SUMMARY:
 *   Article II of the Outer Space Treaty (1967) prohibits national
 *   appropriation of celestial bodies 'by claim of sovereignty, by means of
 *   use or occupation, or by any other means.' Article XI contemplates an
 *   international regime to govern resource exploitation. This reading
 *   (international_regime) holds that Article II defers the appropriation
 *   question to that future regime: neither the extraction-permissive reading
 *   (private ownership allowed) nor the conservation reading (all
 *   appropriation prohibited) has treaty authority absent the multilateral
 *   framework. The constraint is the deferral itself — a scaffold meant to be
 *   temporary, whose sunset clause (the regime) has not triggered after 57
 *   years. The grey zone enables first-mover firms and spacefaring states to
 *   operate without clear authorization or prohibition, while developing
 *   nations and the common heritage principle bear the cost of the regime's
 *   absence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__international_regime, 0.35).
domain_priors:suppression_score(ost_article_ii_non_appropriation__international_regime, 0.2).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__international_regime, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, extractiveness, 0.35).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__international_regime, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__international_regime, scaffold).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__international_regime, "OST Article II Non-Appropriation Deferral to International Regime").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__international_regime, "international_space_law/treaty_interpretation/commons_governance").

narrative_ontology:has_sunset_clause(ost_article_ii_non_appropriation__international_regime).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__international_regime, 'c16354af-8946-42e0-8b38-96b141117c6e').
narrative_ontology:cs_kernel_codification('c16354af-8946-42e0-8b38-96b141117c6e', formalized).
narrative_ontology:cs_authority_grounding('c16354af-8946-42e0-8b38-96b141117c6e', lineage).
narrative_ontology:cs_interpretation_layer_present('c16354af-8946-42e0-8b38-96b141117c6e').
narrative_ontology:cs_reading_relation('c16354af-8946-42e0-8b38-96b141117c6e', ost_article_ii_non_appropriation__extraction_permissive, coexists_with).
narrative_ontology:cs_reading_relation('c16354af-8946-42e0-8b38-96b141117c6e', ost_article_ii_non_appropriation__commons_conservation, coexists_with).
narrative_ontology:cs_axiom('c16354af-8946-42e0-8b38-96b141117c6e', foundational, appropriation_question_deferred_to_multilateral_regime).
narrative_ontology:cs_axiom_status(appropriation_question_deferred_to_multilateral_regime, holdable).
narrative_ontology:cs_axiom_grounding('c16354af-8946-42e0-8b38-96b141117c6e', appropriation_question_deferred_to_multilateral_regime, conventional).
narrative_ontology:cs_reference_frame('c16354af-8946-42e0-8b38-96b141117c6e', ost_1967_deferral_framework).
narrative_ontology:cs_drift_state('c16354af-8946-42e0-8b38-96b141117c6e', contemporary_commercial_space_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('c16354af-8946-42e0-8b38-96b141117c6e', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, first_mover_space_mining_firms).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__international_regime, spacefaring_states_with_commercial_ambitions).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, developing_nations_common_heritage_advocates).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__international_regime, future_generations).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__international_regime, appropriation_requires_multilateral_authorization).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__international_regime, common_heritage_of_mankind_pending_regime).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate in the regulatory grey zone created by the absent regime. They advance extraction technologies and mission plans (e.g., lunar water ice, asteroid metals) without clear legal authorization or prohibition, benefiting from first-mover advantage while the regime negotiation stalls. Their exit is mobile: they can pivot to terrestrial markets or await regulatory clarity.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, first_mover_space_mining_firms, beneficiary,
    organized, biographical, mobile, global).

% States with launch capability and domestic space commerce legislation (e.g., US, Luxembourg, UAE, Japan) that authorize private resource extraction. They set the agenda by advancing national frameworks while blocking or delaying the multilateral regime that would constrain unilateral action. Their exit is arbitrage-grade: they can forum-shop between national and international legal orders.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, spacefaring_states_with_commercial_ambitions, agenda_setter,
    institutional, generational, arbitrage, global).

% States without independent space access that rely on the 'common heritage of mankind' principle (Moon Agreement, UNGA resolutions) to claim equitable benefit sharing. They bear the cost of the stalled regime: first movers lock in claims and infrastructure while the benefit-sharing mechanism never materializes. Their exit is trapped: they cannot develop space capabilities independently and have no leverage to force regime conclusion.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, developing_nations_common_heritage_advocates, payer,
    moderate, generational, trapped, global).

% The majority of UN member states with no space program and no prospect of acquiring one. They are structurally excluded from the regime negotiation (which occurs among spacefaring states) and from the grey-zone extraction. They would object to both extraction-permissive and conservation readings that don't guarantee equitable sharing, but have no forum to be heard.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, non_spacefaring_states, excluded,
    powerless, generational, trapped, global).

% The UN Committee on the Peaceful Uses of Outer Space, the designated forum for the deferred regime negotiation. It hosts the stalled Working Group on Legal Aspects of Space Resource Activities. It observes the constraint's operation (the deferral's persistence, the grey zone's expansion) but lacks enforcement power to conclude the regime or authorize extraction.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, un_copuos, observer,
    institutional, generational, analytical, global).

% The interpretive community that produces the competing readings (extraction_permissive, commons_conservation, international_regime). They analyze the constraint's structure but do not bear its costs or collect its benefits. Their analytical seat is the only one that sees all three readings simultaneously.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__international_regime, legal_scholars_international_lawyers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defers the appropriation question to a future multilateral regime, avoiding premature closure on either extraction or conservation while technological capability and geopolitical consensus are both absent. The coordination is negative: it coordinates by NOT deciding, preserving the status quo of non-appropriation until a positive regime emerges.
% TRANSFER_FUNCTION: Transfers regulatory authority from the treaty text (Article II/Article XI) to a future regime that has not materialized after 50+ years. This creates a grey zone where first movers operate without clear authorization or prohibition, effectively transferring the power to set precedents from the multilateral process to unilateral actors.
% ABSENT_VOICES: Non-spacefaring nations (the majority of UN members), indigenous peoples with cosmological relationships to celestial bodies, future generations who will inherit the regime's terms or the extraction's consequences. They are not at the negotiating table for the deferred regime; the Working Group comprises spacefaring states and invited observers only.
% DISAPPEARANCE_RATIONALE: If the deferral language vanished overnight, the treaty would require immediate authoritative interpretation. Either the extraction_permissive reading (private ownership permitted) or the commons_conservation reading (all appropriation prohibited) would become the default legal baseline, forcing a resolution the deferral was designed to avoid. The grey zone would collapse into one of the two contested readings.
% FOUNDING_PROBLEM: How to govern celestial resource appropriation when technological capability to extract did not yet exist (1967) and geopolitical consensus between Cold War blocs was impossible. The deferral was a temporal bridge: 'we will decide later when we can actually do it and when we agree on how.'
% FOUNDING_PROBLEM_CORROBORATION: UN COPUOS Working Group reports (2018-present) document the persistent deadlock. Developing nation position papers (G77 statements, Moon Agreement parties) attest the benefit-sharing problem remains unsolved. Scholarly commentary from outside spacefaring states (e.g., Basler, Lee, Gabrynowicz) corroborates that the technological capability now exists but geopolitical consensus remains impossible. Spacefaring states' national legislation (US SPACE Act 2015, Luxembourg 2017) demonstrates they treat the problem as live but solve it unilaterally.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__international_regime, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__international_regime, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__international_regime, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__international_regime, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__international_regime, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__international_regime_tests).
:- end_tests(ost_article_ii_non_appropriation__international_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) because the grey zone enables de facto extraction without treaty authorization — the constraint extracts by omission, not commission. Suppression is low (0.20) because no reading is actively suppressed; all three remain live in discourse. Theater ratio is high (0.65) because the deferral language performs the appearance of a governed commons while the regime negotiation performs the appearance of progress — the Working Group meets annually but the zero-sum distributional conflict (benefit sharing vs. non-interference) prevents conclusion. Accessibility collapse is low (0.30) because both sibling readings remain structurally available; the constraint has not foreclosed alternatives. Resistance is low (0.25) because there is no authoritative reading to resist — the constraint is the absence of authority.
 *
 * PERSPECTIVAL GAP:
 *   From the spacefaring state seat, the deferral is a legitimate coordination mechanism preserving flexibility. From the developing nation seat, the same deferral is a snare: it permits extraction by the powerful while the promised benefit-sharing regime never arrives. From the first-mover firm seat, the grey zone is a rope-like opportunity (coordination via national licensing). The engine computes this divergence from the structural data — the authored claim (scaffold) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   First-mover firms and spacefaring states are structural beneficiaries: they operate in the grey zone with mobile/arbitrage exit, collecting first-mover advantages. Developing nations and future generations are payers: they bear the cost of benefit-sharing never materializing while precedents lock in, with trapped exit. UN COPUOS and legal scholars are observers: they see the structure but neither collect nor pay. The agenda_setter role belongs to spacefaring states — they control the regime negotiation's pace and can advance national frameworks unilaterally.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (deferral to a regime) has outlived its function: the technological capability the deferral anticipated now exists, but the geopolitical consensus it required remains impossible. The constraint persists as a scaffold whose sunset failed to trigger — a zombie transitional arrangement. The classification prevents mislabeling this as pure extraction (snare) because no actor designed the grey zone for extraction; it emerged from the regime's failure to materialize. It also prevents mislabeling as pure coordination (rope) because the coordination function (regime negotiation) has atrophied into theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deferral_as_constraint_or_absence,
    'Does the deferral language itself constitute a constraint (a scaffold with a failed sunset), or is it merely the absence of a constraint (a gap the siblings fight to fill)?',
    'Treaty interpretation analysis: does Article II + XI create a positive obligation to conclude a regime before appropriation, or a negative liberty to appropriate until a regime says otherwise? The Vienna Convention on the Law of Treaties (object and purpose, subsequent practice) provides the interpretive framework.',
    'If the deferral is a constraint, it classifies as scaffold (or piton if the regime is permanently dead). If it is merely an absence, the constraint story should be authored for the sibling that fills the gap (extraction_permissive or commons_conservation), not for the gap itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deferral_as_constraint_or_absence, conceptual, 'Whether the deferral language has independent constraining force or is merely a placeholder.').

omega_variable(
    grey_zone_extraction_attribution,
    'Is the de facto extraction by first movers in the grey zone properly attributed to THIS constraint (the deferral), or to the ABSENCE of the regime (which would be a different constraint: the failed negotiation)?',
    'Counterfactual: if the regime had concluded on schedule (e.g., 1980s), would extraction have proceeded under its terms? If yes, the deferral constrained extraction; if no, the deferral enabled it by not prohibiting it.',
    'If attributed to the deferral, extractiveness rises and the scaffold classification holds. If attributed to the failed negotiation, the deferral''s extractiveness is near-zero and the real constraint is the regime''s absence — a different story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(grey_zone_extraction_attribution, conceptual, 'Attribution of grey-zone extraction to the deferral vs. the failed regime.').

omega_variable(
    sunset_clause_permanently_deferred,
    'At what point does a scaffold whose sunset clause has not triggered for 57 years become a piton (degraded/inertial) rather than a scaffold (transitional)?',
    'Measure theater_ratio trajectory: if it crosses 0.7 and founding_problem_status remains ''live'' while the regime negotiation shows no progress over a generation, reclassify as piton. The T17 abductive trigger (mountain_extraction_accumulation analogue for scaffolds) would fire.',
    'If piton, the constraint persists by theatrical maintenance (annual Working Group meetings, ritualized negotiations) with no party benefiting enough to maintain it and no party hurt enough to fix it. The agenda_setter (spacefaring states) could change it but the cost to fix (concluding a regime with benefit-sharing) exceeds what they bear.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_permanently_deferred, empirical, 'The scaffold-to-piton transition threshold for permanently deferred sunsets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__international_regime, 0, 57).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost_art2_intl_regime_tr_t0, ost_article_ii_non_appropriation__international_regime, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ost_art2_intl_regime_tr_t15, ost_article_ii_non_appropriation__international_regime, theater_ratio, 15, 0.3).
narrative_ontology:measurement(ost_art2_intl_regime_tr_t33, ost_article_ii_non_appropriation__international_regime, theater_ratio, 33, 0.45).
narrative_ontology:measurement(ost_art2_intl_regime_tr_t45, ost_article_ii_non_appropriation__international_regime, theater_ratio, 45, 0.55).
narrative_ontology:measurement(ost_art2_intl_regime_tr_t57, ost_article_ii_non_appropriation__international_regime, theater_ratio, 57, 0.65).

% Extraction over time
narrative_ontology:measurement(ost_art2_intl_regime_be_t0, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ost_art2_intl_regime_be_t15, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 15, 0.18).
narrative_ontology:measurement(ost_art2_intl_regime_be_t33, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 33, 0.25).
narrative_ontology:measurement(ost_art2_intl_regime_be_t45, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 45, 0.3).
narrative_ontology:measurement(ost_art2_intl_regime_be_t57, ost_article_ii_non_appropriation__international_regime, base_extractiveness, 57, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(ost_art2_intl_regime_su_t0, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(ost_art2_intl_regime_su_t15, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 15, 0.12).
narrative_ontology:measurement(ost_art2_intl_regime_su_t33, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 33, 0.15).
narrative_ontology:measurement(ost_art2_intl_regime_su_t45, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 45, 0.18).
narrative_ontology:measurement(ost_art2_intl_regime_su_t57, ost_article_ii_non_appropriation__international_regime, suppression_requirement, 57, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__international_regime, global_infrastructure).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__international_regime, 0.15).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__international_regime, ost_article_ii_non_appropriation__commons_conservation).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial label 'OST Article II non-appropriation' into three structurally distinct claims with different ε values, beneficiary structures, and regime dependencies. The international_regime reading (this story) has ε=0.35 (moderate, grey-zone extraction). The extraction_permissive reading would have higher ε (active extraction authorized by national law). The commons_conservation reading would have ε≈0 (prohibition). They share the kernel but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ost_article_ii_non_appropriation__international_regime, institutional, 0.15).
constraint_indexing:directionality_override(ost_article_ii_non_appropriation__international_regime, organized, 0.2).
constraint_indexing:directionality_override(ost_article_ii_non_appropriation__international_regime, moderate, 0.75).
constraint_indexing:directionality_override(ost_article_ii_non_appropriation__international_regime, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
