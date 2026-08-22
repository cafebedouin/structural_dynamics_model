% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__deterrence_unthinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__deterrence_unthinkable, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: war_winnability_post_1945__deterrence_unthinkable
 *   human_readable: Nuclear Deterrence as Categorical Operational Contraction
 *   domain: strategic/international_relations
 *
 * SUMMARY:
 *   This constraint instantiates the deterrence_unthinkable reading of the
 *   war_winnability_post_1945 kernel. The reading holds that the introduction
 *   of survivable, large-scale nuclear arsenals made great-power total war
 *   categorically unwinnable, producing an operational contraction in which
 *   strategic planning shifts irretrievably from victory to prevention. The
 *   constraint is presented in strategic discourse as a structural feature of
 *   the post-1945 security environmentâeffectively a natural law of
 *   great-power politics. However, the arrangement asymmetrically benefits
 *   civilian populations (through the prevention of total war) while imposing
 *   concentrated costs on military establishments (through mission
 *   incoherence and the loss of victory-oriented institutional identity).
 *   This beneficiary-victim asymmetry triggers false-summit detection even as
 *   the reading claims mountain status.
 *
 * KEY AGENTS:
 *   - Civilian populations (beneficiary/organized): Receive prevention of total war but cannot exit the deterrent system.
 *   - Military establishments (payer/institutional): Bear mission incoherence and identity-lock as warfighting institutions in a war-prevention world.
 *   - Counterforce strategists (excluded/moderate): Excluded from the doctrinal framework; believe winnability persists.
 *   - Strategic studies community (observer/analytical): Produces the epistemic lens for evaluating deterrence stability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, 0.32).
domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, 0.4).
domain_priors:theater_ratio(war_winnability_post_1945__deterrence_unthinkable, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, extractiveness, 0.32).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__deterrence_unthinkable, mountain).
narrative_ontology:human_readable(war_winnability_post_1945__deterrence_unthinkable, "Nuclear Deterrence as Categorical Operational Contraction").
narrative_ontology:topic_domain(war_winnability_post_1945__deterrence_unthinkable, "strategic/international_relations").

domain_priors:emerges_naturally(war_winnability_post_1945__deterrence_unthinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__deterrence_unthinkable, 'a7956187-0d63-437f-b6b4-e1c9aa98a882').
narrative_ontology:cs_kernel_codification('a7956187-0d63-437f-b6b4-e1c9aa98a882', distributed).
narrative_ontology:cs_authority_grounding('a7956187-0d63-437f-b6b4-e1c9aa98a882', distributed).
narrative_ontology:cs_reading_relation('a7956187-0d63-437f-b6b4-e1c9aa98a882', war_winnability_post_1945__countervailing_thinkable, forecloses).
narrative_ontology:cs_reading_relation('a7956187-0d63-437f-b6b4-e1c9aa98a882', war_winnability_post_1945__rhetorical_contraction, coexists_with).
narrative_ontology:cs_axiom('a7956187-0d63-437f-b6b4-e1c9aa98a882', foundational, existential_deterrence_as_structural_constant).
narrative_ontology:cs_axiom_status(existential_deterrence_as_structural_constant, holdable).
narrative_ontology:cs_axiom_grounding('a7956187-0d63-437f-b6b4-e1c9aa98a882', existential_deterrence_as_structural_constant, empirically_contingent).
narrative_ontology:cs_axiom('a7956187-0d63-437f-b6b4-e1c9aa98a882', foundational, warfighting_victory_incoherent_under_mad).
narrative_ontology:cs_axiom_status(warfighting_victory_incoherent_under_mad, holdable).
narrative_ontology:cs_axiom_grounding('a7956187-0d63-437f-b6b4-e1c9aa98a882', warfighting_victory_incoherent_under_mad, conventional).
narrative_ontology:cs_reference_frame('a7956187-0d63-437f-b6b4-e1c9aa98a882', existential_deterrence_equilibrium).
narrative_ontology:cs_drift_state('a7956187-0d63-437f-b6b4-e1c9aa98a882', contemporary_multipolar_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a7956187-0d63-437f-b6b4-e1c9aa98a882', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, civilian_populations).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, military_establishments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Diffuse global populations residing under nuclear-armed states. They benefit from the absence of great-power total war but cannot opt out of the deterrent relationship or the risk of accidental escalation. They exercise influence indirectly through democratic politics but remain structurally exposed to the strategic choices of nuclear command authorities.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, civilian_populations, beneficiary,
    organized, generational, constrained, global).

% Great-power military institutions organized around warfighting victory. Under the deterrence-unthinkable arrangement, their core mission coherence is eroded because planning for victory in total war is treated as incoherent. Professional identity, career structures, and doctrinal traditions are locked to victory-oriented warfare, creating persistent institutional strain as strategy shifts to war prevention and risk management.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, military_establishments, payer,
    institutional, generational, identity_locked, global).

% Strategists and defense analysts who argue that limited nuclear victory or meaningful counterforce options remain achievable. They are doctrinally excluded from the deterrence-unthinkable framework as operationally delusional or as dangerous destabilizers; their policy influence is constrained by the taboo against winnability discourse.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, counterforce_strategists, excluded,
    moderate, biographical, constrained, national).

% Academic and policy-analytical observers who model deterrence stability, arms racing, and strategic posture. They do not collect from or pay into the constraint directly but produce the epistemic framework through which the operational contraction is understood, contested, or legitimized.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, strategic_studies_community, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents great-power total war by making the costs of victory exceed any possible political gain; coordinates mutual survival through the credible threat of mutual destruction, aligning expectations away from warfighting and toward war prevention.
% TRANSFER_FUNCTION: Moves the strategic burden of adaptation from civilian populations (who receive relative survival from the prevention of total war) to military establishments (who bear the loss of victory-oriented mission coherence and traditional warfighting institutional identity).
% ABSENT_VOICES: Counterforce strategists and limited-war theorists who believe nuclear victory remains operationally achievable; they are excluded from the deterrence-unthinkable framework as doctrinally illegitimate or strategically destabilizing, and their policy channels are constrained by the winnability taboo.
% DISAPPEARANCE_RATIONALE: If the constraint vanished and total war became winnable and thinkable again, strategic force postures would revert to warfighting planning, alliance structures would shift toward preemptive or counterforce doctrines, military institutional missions would reconfigure around victory, and the post-1945 division between war prevention and warfighting would collapse.
% FOUNDING_PROBLEM: The industrialization of warfare in the early twentieth century created the material possibility of annihilating great-power adversaries, raising the prospect that the next great-power war would amount to civilizational suicide.
% FOUNDING_PROBLEM_CORROBORATION: Civilian strategic studies institutions and independent historians attest to the prevention function from outside the benefiting parties; corroboration is contested by counterforce strategists who argue the problem was never total war itself but inadequate warfighting precision.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__deterrence_unthinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__deterrence_unthinkable, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__deterrence_unthinkable, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_winnability_post_1945__deterrence_unthinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__deterrence_unthinkable, 0.32, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, ExtMetricName, E),
    domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(war_winnability_post_1945__deterrence_unthinkable),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.32 because the operational contraction imposes real, asymmetric institutional costs on military establishments without a corresponding capturable benefitâdiffuse civilian survival is a coordination outcome, not a rent. Suppression at 0.40 captures the moderate but persistent epistemic and institutional suppression of counterforce alternatives through doctrinal taboo and professional gatekeeping. Theater ratio at 0.30 reflects that much nuclear posture is performative (signaling, extended deterrence theater) but rests on a real physical base of secure second-strike capability. Accessibility collapse is very high (0.88) because once mutual vulnerability is understood, alternative strategic frameworks genuinely collapse intellectually. Resistance is moderate (0.45) because military establishments and counterforce schools actively resist the doctrine through modernization advocacy and warfighting planning, signaling that this 'mountain' is actively contested rather than passively accepted like a physical law.
 *
 * PERSPECTIVAL GAP:
 *   The civilian population seat and the military establishment seat should compute very differently. Civilians experience a low-directionality, low-extraction relationship: the constraint subsidizes their survival. Military establishments experience high directionality and concentrated extraction: their professional identity is rendered incoherent by the same structure. The strategic studies observer seat should compute near the analytical middle, seeing the structural asymmetry without bearing its costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations are declared beneficiaries because the prevention of total war flows to them as a diffuse public good; their directionality sits near the beneficiary end. Military establishments are declared payers because the constraint extracts mission coherence and institutional identity from them; their directionality sits near the target end, amplified by identity_locked exit options. The absence of a concentrated agenda_setter or capturer is consistent with a claimed mountain but is precisely what the false-summit machinery interrogates.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreventing industrialized civilizational suicideâremains live, so mandatrophy is not declared. The constraint is not a piton because there is real functional content beneath the theater (secure second-strike physics is not purely performative). Classification as mountain is claimed because the reading treats mutual vulnerability as unchangeable; the engine will evaluate whether the presence of beneficiaries, moderate resistance, and non-zero extraction indicate a false summit (constructed doctrine benefiting civilian stability at institutional expense) rather than a genuine natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is the unwinnability of nuclear war a genuine physical-strategic law (mountain), or a constructed doctrinal consensus that stabilizes civilian life at the expense of military institutional identity?',
    'Comparative historical analysis across nuclear and pre-nuclear eras; examination of whether deterrence stability persists independently of elite belief in it (natural experiment from leadership transitions or crisis behavior).',
    'If the constraint is constructed rather than natural, reclassification from mountain to tangled_rope or rope is warranted; the beneficiary-victim asymmetry would shift from side-effect to structural extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, conceptual, 'Natural-law versus constructed-doctrine ambiguity for deterrence stability').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of counterforce victory theory epistemic (genuine recognition of physical limits) or institutional (professional gatekeeping and budgetary identity-protection by civilian deterrence theorists)?',
    'Trace the career and funding incentives of deterrence institutions versus physics laboratories; measure whether counterforce arguments are excluded on empirical or on professional-normative grounds.',
    'If institutional, effective suppression is higher than structural measures suggest, and the constraint''s coordination function is partially cover for identity-coordination extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Epistemic versus institutional suppression of counterforce alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__deterrence_unthinkable, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war_winnability_det_unthinkable_tr_t0, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0, 0.05).
narrative_ontology:measurement(war_winnability_det_unthinkable_tr_t20, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 20, 0.2).
narrative_ontology:measurement(war_winnability_det_unthinkable_tr_t40, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 40, 0.42).
narrative_ontology:measurement(war_winnability_det_unthinkable_tr_t60, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 60, 0.3).
narrative_ontology:measurement(war_winnability_det_unthinkable_tr_t80, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 80, 0.35).

% Extraction over time
narrative_ontology:measurement(war_winnability_det_unthinkable_be_t0, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(war_winnability_det_unthinkable_be_t20, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(war_winnability_det_unthinkable_be_t40, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(war_winnability_det_unthinkable_be_t60, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 60, 0.28).
narrative_ontology:measurement(war_winnability_det_unthinkable_be_t80, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 80, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(war_winnability_post_1945__deterrence_unthinkable, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__deterrence_unthinkable, global_infrastructure).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__countervailing_thinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__rhetorical_contraction).

% DUAL FORMULATION NOTE:
% The war_winnability_post_1945 kernel decomposes into three structurally distinct constraints because the epsilon values and stakeholder directionalities differ across readings. This reading (deterrence_unthinkable) claims negligible epsilon and mountain status; countervailing_thinkable would claim substantially higher epsilon with military establishments as beneficiaries of warfighting planning; rhetorical_contraction would claim high theater_ratio with operational planning covertly continuing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
