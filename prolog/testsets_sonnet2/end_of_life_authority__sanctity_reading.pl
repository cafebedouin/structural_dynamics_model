% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__sanctity_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: end_of_life_authority__sanctity_reading
 *   human_readable: Categorical Prohibition on Intentional Life-Ending (Sanctity-of-Life Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint instantiates the sanctity-of-life reading of the
 *   end-of-life authority kernel: it treats the intrinsic value of human life
 *   as prohibiting intentional life-ending regardless of the individual's
 *   competent, informed preference. It is authored as a distinct constraint
 *   from the autonomy reading (which grounds a right to control the timing of
 *   death) and from the slippery-slope mechanism (which is an empirical claim
 *   about how autonomy-based frameworks expand over time). This reading's ε
 *   is assessed on the standing categorical-prohibition arrangement as this
 *   reading's own proponents understand it — a protective coordination
 *   structure with a genuine coercion-prevention function, but one that also
 *   imposes real, uncompensated costs on competent patients whose considered
 *   wishes it categorically overrides. The extractiveness figure reflects
 *   those costs, not any endorsed alternative.
 *
 * KEY AGENTS:
 *   - medical_licensing_authorities: agenda_setter (institutional/analytical) — enforces the categorical rule
 *   - vulnerable_populations_at_coercion_risk: beneficiary (powerless/trapped) — shielded from coercion pathway
 *   - competent_terminally_ill_patients_seeking_control: payer (powerless/trapped) — denied assistance regardless of competence
 *   - religious_and_traditional_medical_institutions: beneficiary/agenda_setter (organized/arbitrage) — institutional legitimacy tied to the prohibition
 *   - disability_rights_advocacy_organizations: beneficiary (organized/constrained) — organized protective interest
 *   - attending_physicians: payer/agenda_setter (moderate/constrained) — bound by the rule they help enforce
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, 0.58).
domain_priors:suppression_score(end_of_life_authority__sanctity_reading, 0.71).
domain_priors:theater_ratio(end_of_life_authority__sanctity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__sanctity_reading, "Categorical Prohibition on Intentional Life-Ending (Sanctity-of-Life Reading)").
narrative_ontology:topic_domain(end_of_life_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__sanctity_reading, '59a2aee0-d05b-41f7-8398-fef8fc9cf3b8').
narrative_ontology:cs_kernel_codification('59a2aee0-d05b-41f7-8398-fef8fc9cf3b8', formalized).
narrative_ontology:cs_authority_grounding('59a2aee0-d05b-41f7-8398-fef8fc9cf3b8', lineage).
narrative_ontology:cs_interpretation_layer_present('59a2aee0-d05b-41f7-8398-fef8fc9cf3b8').
narrative_ontology:cs_reading_relation('59a2aee0-d05b-41f7-8398-fef8fc9cf3b8', end_of_life_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('59a2aee0-d05b-41f7-8398-fef8fc9cf3b8', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('59a2aee0-d05b-41f7-8398-fef8fc9cf3b8', foundational, human_life_possesses_intrinsic_inviolable_value).
narrative_ontology:cs_axiom_status(human_life_possesses_intrinsic_inviolable_value, holdable).
narrative_ontology:cs_axiom_grounding('59a2aee0-d05b-41f7-8398-fef8fc9cf3b8', human_life_possesses_intrinsic_inviolable_value, deontological).
narrative_ontology:cs_axiom('59a2aee0-d05b-41f7-8398-fef8fc9cf3b8', secondary, physician_role_limited_to_preservation_and_palliation).
narrative_ontology:cs_axiom_status(physician_role_limited_to_preservation_and_palliation, holdable).
narrative_ontology:cs_axiom_grounding('59a2aee0-d05b-41f7-8398-fef8fc9cf3b8', physician_role_limited_to_preservation_and_palliation, conventional).
narrative_ontology:cs_reference_frame('59a2aee0-d05b-41f7-8398-fef8fc9cf3b8', hippocratic_life_preservation_mandate).
narrative_ontology:cs_drift_state('59a2aee0-d05b-41f7-8398-fef8fc9cf3b8', contemporary_assisted_dying_legalization_wave, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('59a2aee0-d05b-41f7-8398-fef8fc9cf3b8', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__sanctity_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, vulnerable_populations_at_coercion_risk).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, religious_and_traditional_medical_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, disability_rights_advocacy_organizations).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, competent_terminally_ill_patients_seeking_control).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, patients_with_unbearable_refractory_suffering).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, families_bearing_prolonged_dying_processes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, attending_physicians).
narrative_ontology:constraint_vindicates(end_of_life_authority__sanctity_reading, intrinsic_human_dignity_doctrine).
narrative_ontology:constraint_vindicates(end_of_life_authority__sanctity_reading, physician_non_maleficence_absolutism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce professional standards that categorically bar physicians from intentionally ending patient life, treating any deviation as a licensing and criminal matter. Frame the rule as protecting the profession's healing mandate and preventing coercion of the vulnerable. Administer disciplinary machinery that makes the prohibition operative in practice.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, medical_licensing_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Elderly, disabled, and economically disadvantaged people who might face subtle or overt pressure to end their lives to relieve family burden or system costs if intentional life-ending were available. The prohibition removes that pathway entirely, shielding them from a pressure they could not reliably resist given their structural dependency.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, vulnerable_populations_at_coercion_risk, beneficiary,
    powerless, biographical, trapped, national).

% Patients with decision-making capacity and a terminal diagnosis who want assistance ending their lives on their own terms. The categorical rule denies this option regardless of their stated preference, competence, or the severity of their suffering; their only lawful exits are palliative sedation, refusal of treatment, or travel to a jurisdiction with a different rule, if they can afford and manage it.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, competent_terminally_ill_patients_seeking_control, payer,
    powerless, immediate, trapped, national).

% Patients whose suffering resists palliative management but who may not qualify as strictly terminal. The prohibition applies to them with the same force as to terminal patients; the rule does not admit degree of suffering as a variable, only the categorical prohibition on the intentional act.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, patients_with_unbearable_refractory_suffering, payer,
    powerless, immediate, trapped, national).

% Faith-based hospital systems and traditional medical bodies whose institutional identity and moral authority rest on an absolute prohibition against intentionally ending life. They actively lobby for and help enforce the rule, and their continued legitimacy as moral authorities depends on the categorical frame holding.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, religious_and_traditional_medical_institutions, beneficiary,
    organized, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, religious_and_traditional_medical_institutions, agenda_setter).

% Advocacy groups representing disabled people who argue that any legal pathway to assisted death inevitably signals that some lives are less worth living, increasing social and medical pressure on disabled people generally. They support the categorical prohibition as protective even though it also forecloses options some of their own members might individually want.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, disability_rights_advocacy_organizations, beneficiary,
    organized, generational, constrained, national).

% Family members who watch a loved one endure a prolonged dying process that the patient wished to shorten. They bear emotional and often financial costs of extended terminal care mandated by the prohibition, with no lawful mechanism to honor the patient's expressed wish to end suffering intentionally.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, families_bearing_prolonged_dying_processes, payer,
    moderate, biographical, constrained, local).

% Physicians who must refuse patient requests for assistance in dying regardless of their own clinical judgment about the patient's suffering, redirecting all care toward life-preservation and palliation. Non-compliance risks license revocation and criminal prosecution; a physician who privately agrees with a patient's wish has no lawful avenue to act on it.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, attending_physicians, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, attending_physicians, agenda_setter).

% Provide the alternative the prohibition channels patients toward — comfort care without hastening death. Some see the categorical rule as protecting the integrity of their practice from being redefined as a death-hastening service; others see patients they cannot fully help within the rule's boundaries.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, hospice_and_palliative_care_providers, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__sanctity_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, uniform standard that no physician, institution, or family can be pressured or induced to cross the line into intentionally ending a patient's life, removing intentional killing as a permissible clinical or legal option under any circumstance.
% TRANSFER_FUNCTION: Moves decisional authority over the timing and manner of death away from the individual patient (regardless of competence or suffering) and vests it in the collective judgment of medical, legal, and religious institutions that the categorical prohibition should hold; protective benefit flows to vulnerable populations at coercion risk, and the cost of forgone control is borne by competent patients who would have chosen otherwise.
% ABSENT_VOICES: Competent patients currently suffering under the prohibition rarely appear as organized political actors — they are often too ill, too close to death, or too few in any given jurisdiction at any moment to form a durable advocacy bloc; their voice enters mainly through individual court challenges and family testimony after the fact, not through standing representation in the policymaking process.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished overnight, physicians and institutions would need new protocols to distinguish permissible from impermissible life-ending assistance, insurers and hospice systems would need to reconfigure end-of-life care pathways, and the disability-rights and religious institutional coalitions that currently organize around defending the prohibition would lose their primary policy objective — the entire end-of-life regulatory apparatus would need to be rebuilt around a different governing principle.
% FOUNDING_PROBLEM: Historically, physicians held substantial unilateral power over dying patients with weak oversight, and legalizing or normalizing physician-assisted death risked being used (or coerced) to end the lives of people who did not clearly and freely choose it — particularly the poor, disabled, and dependent elderly who could be framed as burdensome.
% FOUNDING_PROBLEM_CORROBORATION: Disability-rights organizations and geriatric-care researchers outside the religious institutional coalition independently corroborate that coercion risk for dependent populations remains empirically live in jurisdictions with looser eligibility rules (cited in comparative studies of assisted-dying regimes). Autonomy-reading advocates and some palliative-care clinicians contend the problem is now addressable through procedural safeguards (capacity assessment, waiting periods, independent witness requirements) rather than categorical prohibition, making the founding problem's continued need for a total ban, rather than regulated access, the actively contested point.
narrative_ontology:disappearance_verdict(end_of_life_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__sanctity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__sanctity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the categorical rule's real cost to competent, informed patients whose autonomous wish to end suffering is overridden without exception or individualized assessment — this is a genuine transfer of decisional authority away from the person most affected. Suppression (0.71) is high because the prohibition is maintained through licensing discipline and criminal law, not persuasion, and admits no case-by-case override even where coercion risk is demonstrably absent (e.g., a competent patient with strong family support and no economic vulnerability). Theater ratio is comparatively low (0.28) because the enforcement machinery is substantially functional — real disciplinary and criminal consequences attach to violations — rather than symbolic. Accessibility collapse (0.62) is moderate-high: the prohibition closes off the assisted-dying pathway entirely but does not collapse all end-of-life alternatives (palliative sedation, treatment refusal remain open). Resistance (0.66) is substantial and growing, driven by patient advocacy, comparative-jurisdiction evidence, and physician dissent.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable populations at coercion risk and the organized institutions that speak for protective values (religious institutions, disability rights groups) sit near the beneficiary end: the rule's categorical character is precisely what removes the coercion pathway they fear, and organized advocacy groups gain standing and legitimacy from the rule's persistence. Competent terminally ill patients and those with refractory suffering sit near the full-target end: trapped exit, immediate time horizon, and a rule that overrides their stated preference without exception. Physicians and families occupy an intermediate position — structurally bound to enforce or endure the rule, bearing real costs, but not benefiting from its persistence the way the organized advocacy coalitions do.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) reflects that this reading has a genuine, non-fabricated coordination function: preventing coercion of dependent people is a real problem, not merely a cover story, and the beneficiary set (vulnerable populations, disability advocates) is not illusory. But the same categorical structure that prevents coercion also extracts from a distinct population — competent patients with no coercion risk at all — for whom the rule provides no protective benefit and only imposes cost. Classifying this as a mountain would hide the constructed, actively-enforced, and contested nature of the categorical line; classifying it as a pure snare would deny the real protective function the sanctity reading's proponents can point to. The tangled_rope frame holds both facts without collapsing them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_risk_generalization_vs_individualization,
    'Is the coercion risk to vulnerable populations sufficiently general and undetectable to justify a categorical prohibition applying even to patients who are individually assessed as free from coercion and fully competent?',
    'Comparative empirical study of jurisdictions with individualized-assessment assisted-dying regimes (e.g. capacity evaluation, waiting periods, independent witnesses) versus categorical-prohibition jurisdictions: does the individualized-assessment model produce a measurably higher rate of coerced or non-autonomous deaths?',
    'If individualized assessment reliably screens out coercion, the categorical prohibition''s extraction from clearly non-coerced competent patients loses its protective justification and the constraint looks more purely extractive toward that subgroup; if assessment cannot reliably screen out subtle coercion, the categorical rule''s coordination function is stronger than the extractiveness score here suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_risk_generalization_vs_individualization, empirical, 'Whether coercion risk can be individually assessed or requires a blanket rule.').

omega_variable(
    intrinsic_value_doctrine_naturalness,
    'Is the intrinsic-value-of-life premise a discoverable moral truth this reading is correctly tracking, or a contingent doctrinal commitment (religious or philosophical) that organized institutions have an interest in maintaining as authoritative?',
    'Cannot be empirically resolved; the question can be narrowed by examining whether institutions that benefit from the doctrine''s authority (religious hospital systems, licensing bodies) would revise their position if the underlying moral claim were shown to lack the philosophical grounding they assert.',
    'If the doctrine is contingent rather than discovered, the beneficiary institutions'' embrace of it looks more like interest-preservation than principled defense of a moral truth, strengthening the tangled_rope reading; if genuinely discoverable, the coordination function is more robust and less reducible to institutional interest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intrinsic_value_doctrine_naturalness, conceptual, 'Whether the sanctity premise is a moral discovery or an institutionally-serving doctrinal commitment.').

omega_variable(
    framing_choice_institution_vs_doctrine,
    'Should this reading''s kernel be framed as the institutional authority (licensing bodies, hospitals) that administers the prohibition, or as the doctrinal claim itself (intrinsic human dignity) that those institutions invoke to legitimate their authority?',
    'Trace whether removing the doctrinal claim (e.g. through a purely secular harm-reduction justification) would leave the institutional enforcement structure intact — if enforcement persists unchanged, the doctrine is legitimating cover rather than the true kernel.',
    'Framing the kernel as the institution would emphasize agenda_setter power and enforcement machinery as the CS structure''s authority_grounding; framing it as the doctrine would emphasize lineage-based authority grounding through religious/philosophical tradition. This story adopts the doctrine-as-legitimating-claim framing (authority_grounding: lineage) because the sanctity premise is explicitly invoked as the justificatory ground, but the institutional-administration framing remains a defensible alternative that would shift authority_grounding toward extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_choice_institution_vs_doctrine, conceptual, 'Alternative framings of the kernel as institution versus doctrine, and their effect on cs_structure classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__sanctity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__sanctity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(end__tr_t8, end_of_life_authority__sanctity_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(end__tr_t16, end_of_life_authority__sanctity_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(end__tr_t24, end_of_life_authority__sanctity_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(end__tr_t32, end_of_life_authority__sanctity_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(end__tr_t40, end_of_life_authority__sanctity_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__sanctity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(end__be_t8, end_of_life_authority__sanctity_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(end__be_t16, end_of_life_authority__sanctity_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(end__be_t24, end_of_life_authority__sanctity_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(end__be_t32, end_of_life_authority__sanctity_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(end__be_t40, end_of_life_authority__sanctity_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__sanctity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(end__su_t8, end_of_life_authority__sanctity_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(end__su_t16, end_of_life_authority__sanctity_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(end__su_t24, end_of_life_authority__sanctity_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(end__su_t32, end_of_life_authority__sanctity_reading, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(end__su_t40, end_of_life_authority__sanctity_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__sanctity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the end_of_life_authority kernel. The autonomy_reading names a largely disjoint victim set (coerced-vulnerable populations under a permissive regime) and a different governing principle (individual autonomy over death). The slippery_slope_mechanism is an empirical claim about the trajectory of autonomy-based frameworks over time and does not itself carry a categorical prohibition; it functions as an evidentiary input this reading's proponents cite in support of maintaining the categorical line. All three share the same underlying contested kernel but instantiate structurally distinct constraints with different ε, different beneficiary/victim sets, and different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
