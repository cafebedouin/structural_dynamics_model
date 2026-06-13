% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__marketplace_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__marketplace_reading, []).

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
 *   constraint_id: speech_protection_kernel__marketplace_reading
 *   human_readable: Speech Protection: Marketplace of Ideas Reading
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint represents the 'marketplace of ideas' reading of speech
 *   protection, where the primary justification for free speech is its role
 *   in truth-discovery. It posits that even false or harmful speech should be
 *   allowed, as it can be countered by 'more speech,' and that truth will
 *   ultimately emerge from the competition of ideas. This reading rejects
 *   content-based restrictions as distorting the truth-discovery process. It
 *   is one of several competing interpretations of the broader
 *   'speech_protection_kernel'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, 0.3).
domain_priors:suppression_score(speech_protection_kernel__marketplace_reading, 0.2).
domain_priors:theater_ratio(speech_protection_kernel__marketplace_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__marketplace_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__marketplace_reading, "Speech Protection: Marketplace of Ideas Reading").
narrative_ontology:topic_domain(speech_protection_kernel__marketplace_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__marketplace_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__marketplace_reading, 'b5809ca4-6da5-4a59-befc-452ed8d86abe').
narrative_ontology:cs_kernel_codification('b5809ca4-6da5-4a59-befc-452ed8d86abe', fixed_text).
narrative_ontology:cs_authority_grounding('b5809ca4-6da5-4a59-befc-452ed8d86abe', lineage).
narrative_ontology:cs_interpretation_layer_present('b5809ca4-6da5-4a59-befc-452ed8d86abe').
narrative_ontology:cs_reading_relation('b5809ca4-6da5-4a59-befc-452ed8d86abe', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b5809ca4-6da5-4a59-befc-452ed8d86abe', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('b5809ca4-6da5-4a59-befc-452ed8d86abe', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_reading_relation('b5809ca4-6da5-4a59-befc-452ed8d86abe', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('b5809ca4-6da5-4a59-befc-452ed8d86abe', foundational, truth_emerges_from_free_exchange).
narrative_ontology:cs_axiom_status(truth_emerges_from_free_exchange, holdable).
narrative_ontology:cs_axiom_grounding('b5809ca4-6da5-4a59-befc-452ed8d86abe', truth_emerges_from_free_exchange, empirically_contingent).
narrative_ontology:cs_axiom('b5809ca4-6da5-4a59-befc-452ed8d86abe', foundational, content_based_restrictions_distort_truth).
narrative_ontology:cs_axiom_status(content_based_restrictions_distort_truth, holdable).
narrative_ontology:cs_axiom_grounding('b5809ca4-6da5-4a59-befc-452ed8d86abe', content_based_restrictions_distort_truth, instrumental).
narrative_ontology:cs_reference_frame('b5809ca4-6da5-4a59-befc-452ed8d86abe', millian_epistemic_optimism).
narrative_ontology:cs_drift_state('b5809ca4-6da5-4a59-befc-452ed8d86abe', contemporary_digital_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b5809ca4-6da5-4a59-befc-452ed8d86abe', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__marketplace_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, public_discourse).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, truth_seekers).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, targets_of_false_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, vulnerable_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the free flow of ideas, which is believed to lead to the discovery of truth and the robust exchange of opinions. The marketplace model assumes that open competition among ideas will result in the best ones prevailing.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, public_discourse, beneficiary,
    institutional, generational, analytical, national).

% Individuals and groups who actively engage in the process of evaluating diverse viewpoints to arrive at informed conclusions. They benefit from the wide array of information and arguments available in an open marketplace of ideas.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, truth_seekers, beneficiary,
    moderate, biographical, mobile, global).

% Individuals or groups who express ideas, opinions, or information. Under this reading, their right to speak is protected primarily because it contributes to the collective good of truth-discovery, rather than as an end in itself.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, speakers, agenda_setter,
    moderate, immediate, mobile, local).

% Individuals or groups who are subjected to false or harmful speech. This reading expects them to counter such speech with more speech, placing the burden of refutation on them rather than restricting the initial harmful expression.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, targets_of_false_speech, payer,
    powerless, immediate, constrained, local).

% Groups historically marginalized or targeted by hate speech, who may experience disproportionate harm from the 'more speech' remedy. The marketplace model often fails to account for power asymmetries in the ability to counter speech effectively.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, vulnerable_groups, payer,
    powerless, generational, identity_locked, national).

% Interprets and enforces the scope of speech protection, often balancing it against other constitutional values. This reading guides judicial decisions to favor broad protection and disfavor content-based restrictions, assuming the market will self-correct.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, judicial_system, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective search for truth and informed public opinion by ensuring a wide range of ideas, even those considered false or harmful, can compete openly, with the expectation that truth will ultimately prevail.
% TRANSFER_FUNCTION: Transfers the burden of refuting false or harmful speech from the state (via content-based restrictions) to individuals and the public (via 'more speech'), in exchange for a maximally open forum for ideas.
% ABSENT_VOICES: Those who advocate for content-based restrictions on speech that causes demonstrable harm, particularly to vulnerable groups, are often marginalized in this framework, as their concerns are seen as undermining the core truth-seeking function.
% DISAPPEARANCE_RATIONALE: If this reading of speech protection vanished, the legal and philosophical landscape of free expression would fundamentally shift. Content-based restrictions would become more permissible, the burden of countering harmful speech would be reallocated, and the very justification for free speech would need to be re-articulated, leading to a significant rearrangement of legal doctrine and public expectations.
% FOUNDING_PROBLEM: The problem of how to ensure a society arrives at truth and makes sound collective decisions, avoiding censorship and the suppression of dissenting or unpopular ideas.
% FOUNDING_PROBLEM_CORROBORATION: Philosophers and legal scholars from diverse perspectives, including those critical of the marketplace model, generally acknowledge the historical problem of censorship and the value of open inquiry. However, the effectiveness of the 'marketplace' as a solution is contested by many, particularly in the digital age, as noted by communication rights advocates and social scientists.
narrative_ontology:disappearance_verdict(speech_protection_kernel__marketplace_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__marketplace_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__marketplace_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_protection_kernel__marketplace_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__marketplace_reading_tests).
:- end_tests(speech_protection_kernel__marketplace_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Rope because it aims to coordinate the collective search for truth, with a relatively low base extractiveness (0.3) and suppression (0.2) compared to other readings that might impose more direct costs or restrictions. However, it does impose costs on targets of false speech and vulnerable groups, who bear the burden of refutation. The 'more speech' remedy, while theoretically coordinative, can be extractive in practice due to power asymmetries. The theater ratio is low (0.1) as the commitment to open discourse is generally genuine, though its effectiveness is debated.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'public_discourse' and 'truth_seekers', this is a beneficial coordination mechanism. From the perspective of 'targets_of_false_speech' and 'vulnerable_groups', it can be highly extractive, as they bear the costs of harm without adequate protection or effective means to counter powerful disinformation campaigns. The 'judicial_system' attempts to balance these perspectives, but this reading prioritizes the collective epistemic benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'public_discourse' and 'truth_seekers' are clear beneficiaries (low d) as the constraint is designed to serve their collective interest. 'Speakers' are also beneficiaries as their expression is broadly protected. 'Targets_of_false_speech' and 'vulnerable_groups' are victims/payers (high d) as they bear the direct costs of harmful speech and the burden of countering it. The 'judicial_system' acts as an agenda-setter, enforcing the framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling coordination as pure extraction by emphasizing the genuine, if often imperfect, coordination function of truth-discovery. However, it risks mislabeling extraction as coordination by downplaying the real costs borne by those unable to effectively participate in the 'marketplace' due to power imbalances or the nature of the harmful speech. The persistence of the 'founding_problem' as 'contested' suggests a potential for mandatrophy, where the original coordination function is less effective in modern contexts (e.g., digital disinformation) but the constraint persists due to institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marketplace_efficacy_in_digital_age,
    'Does the ''marketplace of ideas'' effectively lead to truth-discovery in the contemporary digital information environment, characterized by algorithmic amplification, echo chambers, and disinformation campaigns?',
    'Empirical studies on the spread of truth vs. falsehood in online environments, and the effectiveness of ''more speech'' as a countermeasure against organized disinformation.',
    'If the marketplace is found to be ineffective or counterproductive in the digital age, the justification for this reading would weaken, potentially leading to a re-evaluation of content-based restrictions or a shift towards harm-threshold or dignity-based readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(marketplace_efficacy_in_digital_age, empirical, 'The effectiveness of the marketplace of ideas in modern information ecosystems.').

omega_variable(
    power_asymmetry_in_speech_countering,
    'Does the ''more speech'' remedy adequately address power asymmetries, where marginalized groups or individuals may lack the resources or platform to effectively counter harmful speech from powerful actors?',
    'Sociological and communication studies on the differential capacity of various groups to engage in public discourse and the impact of such disparities on the ''truth-discovery'' process.',
    'If power asymmetries are found to systematically undermine the ''more speech'' remedy, the ''extractiveness'' metric for ''targets_of_false_speech'' and ''vulnerable_groups'' would be re-evaluated as higher, potentially shifting the overall classification towards a Tangled Rope or Snare for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_asymmetry_in_speech_countering, empirical, 'The impact of power imbalances on the effectiveness of countering harmful speech.').

omega_variable(
    marketplace_vs_other_justifications,
    'Is the collective epistemic benefit (truth-discovery) the sole or primary justification for speech protection, or are individual autonomy, democratic participation, or dignity equally foundational?',
    'Conceptual analysis and philosophical debate on the foundational values underlying free speech. This is a matter of normative priority rather than empirical fact.',
    'If other justifications are deemed equally foundational, the ''marketplace_reading'' would be seen as incomplete or overly narrow, potentially leading to a synthesis with elements from ''absolutist_reading'' (autonomy), ''democratic_participation_reading'', or ''dignity_reading''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(marketplace_vs_other_justifications, conceptual, 'The foundational justification for speech protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__marketplace_reading, 1919, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1919, speech_protection_kernel__marketplace_reading, theater_ratio, 1919, 0.05).
narrative_ontology:measurement(spee_tr_t1945, speech_protection_kernel__marketplace_reading, theater_ratio, 1945, 0.07).
narrative_ontology:measurement(spee_tr_t1970, speech_protection_kernel__marketplace_reading, theater_ratio, 1970, 0.09).
narrative_ontology:measurement(spee_tr_t1995, speech_protection_kernel__marketplace_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(spee_tr_t2010, speech_protection_kernel__marketplace_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_kernel__marketplace_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(spee_be_t1919, speech_protection_kernel__marketplace_reading, base_extractiveness, 1919, 0.2).
narrative_ontology:measurement(spee_be_t1945, speech_protection_kernel__marketplace_reading, base_extractiveness, 1945, 0.25).
narrative_ontology:measurement(spee_be_t1970, speech_protection_kernel__marketplace_reading, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement(spee_be_t1995, speech_protection_kernel__marketplace_reading, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement(spee_be_t2010, speech_protection_kernel__marketplace_reading, base_extractiveness, 2010, 0.32).
narrative_ontology:measurement(spee_be_t2024, speech_protection_kernel__marketplace_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1919, speech_protection_kernel__marketplace_reading, suppression_requirement, 1919, 0.15).
narrative_ontology:measurement(spee_su_t1945, speech_protection_kernel__marketplace_reading, suppression_requirement, 1945, 0.18).
narrative_ontology:measurement(spee_su_t1970, speech_protection_kernel__marketplace_reading, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(spee_su_t1995, speech_protection_kernel__marketplace_reading, suppression_requirement, 1995, 0.22).
narrative_ontology:measurement(spee_su_t2010, speech_protection_kernel__marketplace_reading, suppression_requirement, 2010, 0.25).
narrative_ontology:measurement(spee_su_t2024, speech_protection_kernel__marketplace_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__marketplace_reading, information_standard).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'speech_protection_kernel'. Each reading offers a distinct justification and scope for free speech, leading to different structural properties and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
