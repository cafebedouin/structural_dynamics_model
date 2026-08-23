% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__conditional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-24
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: westphalian_sovereignty__conditional_sovereignty
 *   human_readable: Conditional Sovereignty: R2P Intervention Trigger
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   The conditional_sovereignty reading of the westphalian_sovereignty kernel
 *   holds that sovereignty is not a shield but a conditional entitlement:
 *   states that systematically violate human rights forfeit the protection of
 *   non-interference. This reading crystallized in the Responsibility to
 *   Protect (R2P) doctrine (2001 ICISS report, 2005 World Summit endorsement)
 *   and has been invoked for interventions in Kosovo (1999, pre-dating formal
 *   R2P), Libya (2011), and cited in Syria debates. The constraint operates
 *   as a snare from the perspective of targeted states — it extracts autonomy
 *   and imposes external decision-making under a humanitarian veneer. From
 *   the intervener seat, it appears as coordination (preventing atrocities).
 *   The extraction is moderate (0.38) because intervention is rare,
 *   threshold-gated, and requires UNSC authorization — but when triggered,
 *   the extraction of sovereign authority is near-total. Theater ratio (0.28)
 *   reflects that humanitarian justification is often genuine but
 *   increasingly performs cover for regime change. Suppression (0.42) is
 *   structural: targeted states face military intervention, sanctions, and
 *   ICC referral with limited exit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__conditional_sovereignty, 0.38).
domain_priors:suppression_score(westphalian_sovereignty__conditional_sovereignty, 0.42).
domain_priors:theater_ratio(westphalian_sovereignty__conditional_sovereignty, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, extractiveness, 0.38).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__conditional_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__conditional_sovereignty, "Conditional Sovereignty: R2P Intervention Trigger").
narrative_ontology:topic_domain(westphalian_sovereignty__conditional_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__conditional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__conditional_sovereignty, '6a1f05cc-9025-430d-af79-9acd2a0dcf55').
narrative_ontology:cs_kernel_codification('6a1f05cc-9025-430d-af79-9acd2a0dcf55', fixed_text).
narrative_ontology:cs_authority_grounding('6a1f05cc-9025-430d-af79-9acd2a0dcf55', lineage).
narrative_ontology:cs_interpretation_layer_present('6a1f05cc-9025-430d-af79-9acd2a0dcf55').
narrative_ontology:cs_reading_relation('6a1f05cc-9025-430d-af79-9acd2a0dcf55', westphalian_sovereignty__absolute_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('6a1f05cc-9025-430d-af79-9acd2a0dcf55', westphalian_sovereignty__graduated_sovereignty, influences).
narrative_ontology:cs_axiom('6a1f05cc-9025-430d-af79-9acd2a0dcf55', foundational, sovereignty_entails_responsibility_to_protect).
narrative_ontology:cs_axiom_status(sovereignty_entails_responsibility_to_protect, holdable).
narrative_ontology:cs_axiom_grounding('6a1f05cc-9025-430d-af79-9acd2a0dcf55', sovereignty_entails_responsibility_to_protect, deontological).
narrative_ontology:cs_axiom('6a1f05cc-9025-430d-af79-9acd2a0dcf55', foundational, systematic_violations_trigger_external_intervention_authority).
narrative_ontology:cs_axiom_status(systematic_violations_trigger_external_intervention_authority, holdable).
narrative_ontology:cs_axiom_grounding('6a1f05cc-9025-430d-af79-9acd2a0dcf55', systematic_violations_trigger_external_intervention_authority, conventional).
narrative_ontology:cs_reference_frame('6a1f05cc-9025-430d-af79-9acd2a0dcf55', westphalian_non_interference_1945).
narrative_ontology:cs_drift_state('6a1f05cc-9025-430d-af79-9acd2a0dcf55', post_r2p_endorsement_2005, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6a1f05cc-9025-430d-af79-9acd2a0dcf55', '2026-08-24T14:30:00Z').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, international_intervention_advocates).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, atrocity_prevention_ngos).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, un_security_council_permanent_members).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, targeted_sovereign_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, non_intervening_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, civilian_populations_in_target_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, civilian_populations_in_target_states).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, responsibility_to_protect_doctrine).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, human_rights_universality_claim).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__conditional_sovereignty, international_legal_order_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% NGOs, academics, and diplomats who champion R2P. They gain institutional relevance, funding, and normative authority from the doctrine's activation. Their exit is mobile — they can shift advocacy to other norms if R2P loses traction. They do not bear the costs of intervention.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, international_intervention_advocates, beneficiary,
    organized, generational, mobile, global).

% Organizations like Human Rights Watch, Amnesty International, Global Centre for R2P. They benefit from the doctrinal framework that legitimizes their monitoring and advocacy work. Funding and access depend on the R2P architecture. Exit is mobile — they can pivot to other human rights frameworks.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, atrocity_prevention_ngos, beneficiary,
    organized, biographical, mobile, global).

% P5 (US, UK, France, Russia, China) control the authorization gate. P3 (US/UK/France) benefit from intervention legitimacy for strategic objectives; Russia and China benefit from veto power that blocks unwanted interventions. They set the agenda, collect legitimacy rents, and face no exit pressure — they ARE the enforcement mechanism.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, un_security_council_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, un_security_council_permanent_members, beneficiary).

% States accused of systematic violations (e.g., Libya 2011, Syria, Sudan). They bear the full extraction: loss of sovereign control, regime change risk, military intervention, sanctions, ICC referral. Exit is constrained — they must either cease violations (which may threaten regime survival) or endure intervention. Their power is high globally but constrained within the R2P framework.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, targeted_sovereign_states, payer,
    powerful, biographical, constrained, national).

% States not targeted but affected by intervention spillover: refugees, economic disruption, regional destabilization, precedent erosion of non-interference. They pay costs without controlling the trigger. Exit is constrained — they cannot opt out of regional consequences or the precedent effect on their own sovereignty.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, non_intervening_states, payer,
    moderate, biographical, constrained, regional).

% Populations in states facing R2P intervention. They are the nominal beneficiaries (protection from atrocities) but often bear severe costs: collateral damage, post-intervention chaos, displacement, sectarian violence. They are trapped — cannot exit the territory, cannot influence the intervention decision. Their dual role reflects genuine ambiguity: some are saved, others harmed by the intervention itself.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, civilian_populations_in_target_states, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__conditional_sovereignty, civilian_populations_in_target_states, beneficiary).

% 120+ states that reject R2P as Western regime-change tool. They object to the doctrine in UNGA and NAM summits but are structurally excluded from the UNSC authorization gate. Their exit is constrained — they cannot leave the UN system but can build alternative normative frameworks (e.g., 'responsibility while protecting' Brazilian proposal).
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, non_aligned_movement_states, excluded,
    organized, generational, constrained, global).

% Academics analyzing R2P's legal status, state practice, and doctrinal coherence. They neither collect nor pay — they map the constraint's operation across seats. Their analytical exit is absolute.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__conditional_sovereignty, international_legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of atrocity prevention: creates a recognized trigger and authorization pathway for military intervention when a state fails to protect its population, overcoming the paralysis of pure non-interference.
% TRANSFER_FUNCTION: Transfers decision-making authority over military intervention from the target state to the UN Security Council (P5), and transfers the costs of intervention (military, humanitarian, reconstruction) from the target state to intervening states and the international community — while the target state loses sovereign autonomy.
% ABSENT_VOICES: The populations most affected by intervention decisions (civilian_populations_in_target_states) have no voice in the UNSC authorization process. Non-Aligned Movement states are excluded from the veto gate. Would-be interveners without UNSC seats (regional powers, EU) are excluded from the authorization decision despite bearing operational burdens.
% DISAPPEARANCE_RATIONALE: If R2P/conditional sovereignty vanished overnight, the legal and normative barrier to unilateral intervention would drop — powerful states would intervene more freely (Kosovo 1999 precedent), weaker states would lose even the procedural protection of UNSC authorization, and the atrocity prevention framework would collapse into ad hoc power politics. The world would rearrange toward less constrained, less legitimate intervention.
% FOUNDING_PROBLEM: The international community's failure to prevent genocide in Rwanda (1994) and Srebrenica (1995) despite UN peacekeeping presence — the collective action problem of knowing atrocities were occurring but lacking a recognized trigger and authorization mechanism to override sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: The ICISS report (2001), 2005 World Summit Outcome Document, and UN Secretary-General reports 2009-2024 attest the problem is live — atrocities continue (Syria, Myanmar, Yemen, Ethiopia). China and Russia attest the problem is live but argue R2P is the wrong solution (corroboration from outside the primary beneficiary set). No serious actor claims mass atrocity prevention is a solved problem.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__conditional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__conditional_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__conditional_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(westphalian_sovereignty__conditional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__conditional_sovereignty, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness 0.38: intervention is infrequent but when triggered, strips sovereign control entirely. The conditional duty is real — atrocities do trigger pressure — but the threshold is manipulable. Suppression 0.42: targeted states cannot opt out of the R2P framework; exit requires not committing atrocities, which for some regimes is existential. Theater 0.28: the humanitarian coordination function is real but a growing share of enforcement serves strategic interests (Libya 2011 pivot from protection to regime change). Accessibility collapse 0.35: alternatives (regional mediation, sanctions, referral) exist but are often bypassed. Resistance 0.55: strong pushback from Non-Aligned Movement, China, Russia, and targeted states — the constraint is actively contested.
 *
 * PERSPECTIVAL GAP:
 *   From the intervener/advocate seat (international_intervention_advocates, un_security_council_permanent_members): the constraint is a rope — genuine coordination solving the collective action problem of atrocity prevention. From the targeted_sovereign_states seat: it is a snare — conditional autonomy extracted under a humanitarian pretext that is selectively enforced. From civilian_populations_in_target_states: deeply ambivalent — potential beneficiaries of protection but often victims of intervention's collateral effects. The engine computes this divergence from structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: international_intervention_advocates (normative capital, institutional relevance), atrocity_prevention_ngOs (mandate, funding), un_security_council_permanent_members (legitimacy for intervention decisions, strategic leverage). Victims: targeted_sovereign_states (loss of autonomy, regime survival threat), non_intervening_states (drawn into conflicts, precedent risk), civilian_populations_in_target_states (collateral harm, destabilization). Non_intervening_states and civilian_populations are payers because they bear costs without controlling the trigger. Directionality: interveners sit at d≈0.15 (beneficiary), targeted states at d≈0.85 (full target), civilians at d≈0.5 (ambivalent).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing another Rwanda/Srebrenica) remains live — atrocities persist. But the arrangement has drifted: R2P was built for consensus intervention; in practice it has become a tool of P3 (US/UK/France) foreign policy with Russian/Chinese vetoes blocking symmetry. The mandate has not atrophied — the problem is live — but the constraint has mutated into asymmetric extraction. This is not mandatrophy (dead problem, live arrangement); it is capture of a live mandate by powerful beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is conditional_sovereignty a genuine coordinate reading of the westphalian_sovereignty kernel, or does it structurally foreclose the absolute_sovereignty reading within any single state''s commitment framework?',
    'Examine whether states that invoke conditional sovereignty for intervention simultaneously claim absolute sovereignty for their own domestic affairs — a structural contradiction that would indicate foreclosure rather than coexistence.',
    'If conditional_sovereignty forecloses absolute_sovereignty within a single framework, the kernel is not genuinely shared — the readings are different constraints masquerading as a shared commitment. This would require reclassifying the kernel relationship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel is genuinely shared or the readings occupy different constraint spaces.').

omega_variable(
    intervention_selectivity_bias,
    'Does the threshold for ''systematic human rights violations'' that triggers intervention operate uniformly, or is it selectively applied to serve the strategic interests of powerful interveners?',
    'Comparative case analysis: measure intervention frequency and latency against violation severity across states with different geopolitical alignments, controlling for UNSC veto dynamics.',
    'If selectively applied, the constraint''s extraction is higher than the doctrinal threshold suggests — the conditional duty is a cover for strategic extraction. This would push classification toward snare from any seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intervention_selectivity_bias, empirical, 'Whether the trigger condition is applied uniformly or as a selective instrument.').

omega_variable(
    extraction_referent_stability,
    'Does the ε=0.38 refer to the standing Westphalian order (conditional_sovereignty reading''s lights) or to a hypothetical absolute_sovereignty baseline?',
    'Clarify the referent per OQ-26: for a kernel-reading story, ε''s referent is the standing arrangement under contest assessed by THIS reading''s lights. The referent is the post-1945 sovereign order as conditional_sovereignty reads it — not the absolute_sovereignty reading''s preferred order.',
    'Misidentifying the referent inflates or deflates ε by up to 0.2. This omega locks the referent to the conditional reading''s own assessment of the standing order.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_referent_stability, conceptual, 'Referent discipline for kernel-reading ε values.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__conditional_sovereignty, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(westphalian_sovereignty__conditional_sovereignty_tr_t2001, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2001, 0.15).
narrative_ontology:measurement(westphalian_sovereignty__conditional_sovereignty_tr_t2005, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(westphalian_sovereignty__conditional_sovereignty_tr_t2011, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2011, 0.35).
narrative_ontology:measurement(westphalian_sovereignty__conditional_sovereignty_tr_t2015, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2015, 0.31).
narrative_ontology:measurement(westphalian_sovereignty__conditional_sovereignty_tr_t2020, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(westphalian_sovereignty__conditional_sovereignty_tr_t2024, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(westphalian_sovereignty__conditional_sovereignty_be_t2001, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2001, 0.22).
narrative_ontology:measurement(westphalian_sovereignty__conditional_sovereignty_be_t2005, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2005, 0.28).
narrative_ontology:measurement(westphalian_sovereignty__conditional_sovereignty_be_t2011, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2011, 0.41).
narrative_ontology:measurement(westphalian_sovereignty__conditional_sovereignty_be_t2015, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2015, 0.39).
narrative_ontology:measurement(westphalian_sovereignty__conditional_sovereignty_be_t2020, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2020, 0.36).
narrative_ontology:measurement(westphalian_sovereignty__conditional_sovereignty_be_t2024, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(westphalian_sovereignty__conditional_sovereignty_su_t2001, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2001, 0.25).
narrative_ontology:measurement(westphalian_sovereignty__conditional_sovereignty_su_t2005, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2005, 0.35).
narrative_ontology:measurement(westphalian_sovereignty__conditional_sovereignty_su_t2011, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2011, 0.55).
narrative_ontology:measurement(westphalian_sovereignty__conditional_sovereignty_su_t2015, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement(westphalian_sovereignty__conditional_sovereignty_su_t2020, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2020, 0.42).
narrative_ontology:measurement(westphalian_sovereignty__conditional_sovereignty_su_t2024, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__conditional_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalian_sovereignty__conditional_sovereignty, 0.12).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__graduated_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, un_security_council_veto_power).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, icc_jurisdiction_complementarity).

% DUAL FORMULATION NOTE:
% This constraint is one member of the westphalian_sovereignty constraint family. The three readings (absolute, conditional, graduated) instantiate different constraints from the same kernel label. They are linked via affects_constraints. ε differs substantially: absolute_sovereignty reads the standing order as mountain (ε≈0.05), conditional_sovereignty as snare (ε≈0.38), graduated_sovereignty as tangled_rope (ε≈0.25). The decomposition follows the BGS pattern: same label, different structural claims, different ε, linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__conditional_sovereignty, institutional, 0.15).
constraint_indexing:directionality_override(westphalian_sovereignty__conditional_sovereignty, powerful, 0.85).
constraint_indexing:directionality_override(westphalian_sovereignty__conditional_sovereignty, organized, 0.5).
constraint_indexing:directionality_override(westphalian_sovereignty__conditional_sovereignty, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
