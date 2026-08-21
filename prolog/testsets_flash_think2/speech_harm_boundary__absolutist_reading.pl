% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__absolutist_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: speech_harm_boundary__absolutist_reading
 *   human_readable: Absolutist Reading of Speech Harm Boundary
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'absolutist reading' of the speech
 *   harm boundary kernel, where speech protection operates near-absolutely,
 *   and the harm override threshold is extremely high. This reading narrowly
 *   defines unprotected categories (incitement, true threats, defamation,
 *   obscenity) and prioritizes speaker autonomy, often at the cost of those
 *   harmed by speech. The claimed type is 'rope' from the perspective of
 *   coordinating free expression, but the metrics reflect the substantial
 *   extraction of harm from targets.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, 0.7).
domain_priors:suppression_score(speech_harm_boundary__absolutist_reading, 0.2).
domain_priors:theater_ratio(speech_harm_boundary__absolutist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__absolutist_reading, rope).
narrative_ontology:human_readable(speech_harm_boundary__absolutist_reading, "Absolutist Reading of Speech Harm Boundary").
narrative_ontology:topic_domain(speech_harm_boundary__absolutist_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__absolutist_reading, '5e3e57d9-d048-450d-978c-79c115a34db6').
narrative_ontology:cs_kernel_codification('5e3e57d9-d048-450d-978c-79c115a34db6', fixed_text).
narrative_ontology:cs_authority_grounding('5e3e57d9-d048-450d-978c-79c115a34db6', lineage).
narrative_ontology:cs_interpretation_layer_present('5e3e57d9-d048-450d-978c-79c115a34db6').
narrative_ontology:cs_reading_relation('5e3e57d9-d048-450d-978c-79c115a34db6', speech_harm_boundary__dignity_reading, forecloses).
narrative_ontology:cs_reading_relation('5e3e57d9-d048-450d-978c-79c115a34db6', speech_harm_boundary__harm_balancing_reading, forecloses).
narrative_ontology:cs_axiom('5e3e57d9-d048-450d-978c-79c115a34db6', foundational, free_speech_is_paramount).
narrative_ontology:cs_axiom_status(free_speech_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('5e3e57d9-d048-450d-978c-79c115a34db6', free_speech_is_paramount, deontological).
narrative_ontology:cs_axiom('5e3e57d9-d048-450d-978c-79c115a34db6', foundational, truth_emerges_from_open_debate).
narrative_ontology:cs_axiom_status(truth_emerges_from_open_debate, holdable).
narrative_ontology:cs_axiom_grounding('5e3e57d9-d048-450d-978c-79c115a34db6', truth_emerges_from_open_debate, empirically_contingent).
narrative_ontology:cs_reference_frame('5e3e57d9-d048-450d-978c-79c115a34db6', marketplace_of_ideas_framework).
narrative_ontology:cs_drift_state('5e3e57d9-d048-450d-978c-79c115a34db6', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5e3e57d9-d048-450d-978c-79c115a34db6', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__absolutist_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, speakers).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, public_discourse).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, targets_of_harmful_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, vulnerable_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and groups who wish to express themselves freely, benefiting from broad protection against censorship and a high threshold for speech to be deemed unprotected. They bear minimal costs and enjoy significant autonomy.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, speakers, beneficiary,
    moderate, biographical, mobile, global).

% Individuals directly subjected to speech that causes emotional, reputational, or physical harm, but which falls below the extremely high threshold for legal intervention. They bear the direct costs of such speech with limited recourse.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, targets_of_harmful_speech, payer,
    powerless, immediate, trapped, local).

% Collectives historically marginalized or targeted by hate speech, misinformation, or incitement that, under this reading, remains protected. They experience systemic harm and find their identity and safety challenged by the broad scope of protected speech.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, vulnerable_groups, payer,
    powerless, generational, identity_locked, global).

% The institutions responsible for interpreting and enforcing speech protections, consistently applying a high bar for restricting speech and narrowly defining unprotected categories. They uphold the absolutist framework through precedent.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, courts_legal_system, agenda_setter,
    institutional, civilizational, analytical, national).

% The broader societal conversation, which benefits from a wide range of ideas and viewpoints being expressed without fear of censorship. The assumption is that more speech, even harmful speech, ultimately leads to a more informed public.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, public_discourse, beneficiary,
    moderate, generational, mobile, universal).

% Groups and scholars who argue for speech protections to be subordinate to human dignity, advocating for categorical exclusion of personhood-denying speech. Their arguments are largely outside the prevailing absolutist legal framework.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, dignity_advocates, excluded,
    organized, biographical, constrained, global).

% Groups and scholars who propose a proportionality balancing test for speech, where presumptive protection yields to demonstrated harm. Their approach is fundamentally at odds with the absolutist reading's high threshold.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, harm_balancing_advocates, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a broad sphere of free expression, ensuring speakers can articulate diverse views without prior restraint or fear of reprisal for most content, fostering a 'marketplace of ideas'.
% TRANSFER_FUNCTION: Transfers the burden of harm from speakers to targets of speech and vulnerable groups, in exchange for a broadly open public discourse and the perceived societal benefit of unrestricted expression.
% ABSENT_VOICES: Advocates for stronger protections against hate speech, misinformation, and harassment, particularly from vulnerable groups, are structurally excluded from shaping the core legal framework. They would argue the current threshold for harm is too high and leaves them unprotected.
% DISAPPEARANCE_RATIONALE: If this absolutist reading vanished overnight, the legal landscape for speech would immediately shift. Courts would likely adopt more restrictive interpretations, leading to increased regulation of harmful speech, a rebalancing of rights, and significant reorganization of online platforms and public forums to comply with new standards.
% FOUNDING_PROBLEM: To prevent government censorship and ensure a robust marketplace of ideas, believing that truth will emerge from open debate and that restrictions on speech are inherently dangerous.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (speakers, civil liberties groups) argue the problem of potential government overreach and chilling effects is still live. Critics (vulnerable groups, some legal scholars) argue the founding problem has substantially shifted from government censorship to private harms and that the original solution is now causing new problems; independent social science research on speech harms supports the critics' view of shifted function.
narrative_ontology:disappearance_verdict(speech_harm_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(speech_harm_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__absolutist_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) is high because this reading structurally allows significant harm to be borne by targets of speech, effectively transferring the cost of broad expression. Suppression (0.2) is low, as the constraint's primary function is to prevent suppression of speech. Theater ratio (0.1) is low, indicating that the protection afforded is genuinely functional, not merely performative. Resistance (0.5) is moderate, reflecting ongoing legal and social challenges to the scope of protected speech, particularly in the digital age.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of speakers, this constraint is a robust 'rope' that coordinates free expression and prevents censorship. From the perspective of targets and vulnerable groups, the same constraint operates as a 'snare' or 'tangled rope', extracting harm and denying recourse. The engine will compute this divergence from the structural data, highlighting the different experiences of the constraint's operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers and the abstract concept of 'public discourse' are the primary beneficiaries, enjoying extensive freedom of expression. Targets of harmful speech and vulnerable groups are the primary payers, bearing the costs of speech that causes harm but remains protected. The courts and legal system act as agenda-setters, enforcing this high-threshold framework. Dignity and harm-balancing advocates are excluded, as their frameworks are fundamentally incompatible with this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_threshold_justification,
    'Is the extremely high harm override threshold genuinely necessary to prevent a chilling effect on legitimate speech, or does it primarily serve to protect harmful speech?',
    'Empirical studies comparing speech environments and innovation in jurisdictions with different harm thresholds, or detailed analysis of historical cases where speech was restricted and its impact on public discourse.',
    'If the threshold is found to primarily protect harmful speech without significant benefit to legitimate discourse, it would weaken the coordination claim and strengthen the extraction claim, potentially reclassifying the constraint for targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_threshold_justification, empirical, 'Whether the high harm threshold serves its stated purpose or enables undue harm.').

omega_variable(
    digital_era_applicability,
    'Does the absolutist reading, developed largely in a pre-digital context, adequately address the scale and speed of harm dissemination in the contemporary digital era?',
    'Analysis of the efficacy of existing legal remedies for digital harms under this reading, and comparative studies of societal impacts of online speech in different regulatory environments.',
    'If the reading is found to be inadequate for the digital era, it would suggest a significant ''practice_drift'' and potentially lead to calls for re-evaluation of the foundational axioms, pushing towards a ''contested'' status for the reading itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(digital_era_applicability, empirical, 'Relevance of absolutist reading in the digital age.').

omega_variable(
    reading_structural_delta,
    'How would the classification of this constraint change if a sibling reading, such as the ''dignity_reading'' or ''harm_balancing_reading'', were adopted as the prevailing legal framework?',
    'Counterfactual legal analysis and re-evaluation of metrics under the premises of the alternative reading, focusing on shifts in beneficiary/victim sets and extractiveness.',
    'Adoption of a dignity or harm-balancing reading would likely decrease extractiveness for targets/vulnerable groups, increase suppression for certain speakers, and shift the overall classification towards a ''rope'' or ''scaffold'' for a broader set of participants, while potentially creating new victim sets among speakers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_structural_delta, conceptual, 'Impact of adopting a sibling reading on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__absolutist_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1960, speech_harm_boundary__absolutist_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(spee_tr_t1975, speech_harm_boundary__absolutist_reading, theater_ratio, 1975, 0.07).
narrative_ontology:measurement(spee_tr_t1990, speech_harm_boundary__absolutist_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(spee_tr_t2005, speech_harm_boundary__absolutist_reading, theater_ratio, 2005, 0.09).
narrative_ontology:measurement(spee_tr_t2024, speech_harm_boundary__absolutist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(spee_be_t1960, speech_harm_boundary__absolutist_reading, base_extractiveness, 1960, 0.5).
narrative_ontology:measurement(spee_be_t1975, speech_harm_boundary__absolutist_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(spee_be_t1990, speech_harm_boundary__absolutist_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(spee_be_t2005, speech_harm_boundary__absolutist_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(spee_be_t2024, speech_harm_boundary__absolutist_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1960, speech_harm_boundary__absolutist_reading, suppression_requirement, 1960, 0.15).
narrative_ontology:measurement(spee_su_t1975, speech_harm_boundary__absolutist_reading, suppression_requirement, 1975, 0.17).
narrative_ontology:measurement(spee_su_t1990, speech_harm_boundary__absolutist_reading, suppression_requirement, 1990, 0.19).
narrative_ontology:measurement(spee_su_t2005, speech_harm_boundary__absolutist_reading, suppression_requirement, 2005, 0.2).
narrative_ontology:measurement(spee_su_t2024, speech_harm_boundary__absolutist_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__absolutist_reading, information_standard).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__dignity_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__harm_balancing_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, online_platform_content_moderation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'speech_harm_boundary' kernel. Its structural properties and classification are distinct from its sibling readings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
