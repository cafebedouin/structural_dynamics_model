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
 *   constraint_id: speech_harm_boundary__absolutist_reading
 *   human_readable: Near-Absolute Speech Protection (Absolutist Reading)
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'absolutist reading' of the speech harm
 *   boundary kernel, which posits that speech protection operates
 *   near-absolutely, with an extremely high threshold for any harm override.
 *   This interpretation prioritizes speaker autonomy and a robust marketplace
 *   of ideas, narrowly defining categories of unprotected speech (e.g.,
 *   incitement, true threats, defamation, obscenity). The structural
 *   consequence is that targets of harmful speech bear significant costs, as
 *   most speech, even if offensive or distressing, remains protected. Sibling
 *   readings include the 'harm_balancing_reading' and the 'dignity_reading',
 *   which propose different thresholds or categorical exclusions for harmful
 *   speech.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, 0.7).
domain_priors:suppression_score(speech_harm_boundary__absolutist_reading, 0.8).
domain_priors:theater_ratio(speech_harm_boundary__absolutist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__absolutist_reading, rope).
narrative_ontology:human_readable(speech_harm_boundary__absolutist_reading, "Near-Absolute Speech Protection (Absolutist Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__absolutist_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__absolutist_reading, '7ffc684e-fcb1-4b3c-a830-d7d8d05c6308').
narrative_ontology:cs_kernel_codification('7ffc684e-fcb1-4b3c-a830-d7d8d05c6308', fixed_text).
narrative_ontology:cs_authority_grounding('7ffc684e-fcb1-4b3c-a830-d7d8d05c6308', lineage).
narrative_ontology:cs_interpretation_layer_present('7ffc684e-fcb1-4b3c-a830-d7d8d05c6308').
narrative_ontology:cs_reading_relation('7ffc684e-fcb1-4b3c-a830-d7d8d05c6308', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ffc684e-fcb1-4b3c-a830-d7d8d05c6308', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('7ffc684e-fcb1-4b3c-a830-d7d8d05c6308', foundational, speaker_autonomy_is_paramount).
narrative_ontology:cs_axiom_status(speaker_autonomy_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('7ffc684e-fcb1-4b3c-a830-d7d8d05c6308', speaker_autonomy_is_paramount, deontological).
narrative_ontology:cs_axiom('7ffc684e-fcb1-4b3c-a830-d7d8d05c6308', foundational, unprotected_speech_categories_are_narrowly_defined).
narrative_ontology:cs_axiom_status(unprotected_speech_categories_are_narrowly_defined, holdable).
narrative_ontology:cs_axiom_grounding('7ffc684e-fcb1-4b3c-a830-d7d8d05c6308', unprotected_speech_categories_are_narrowly_defined, conventional).
narrative_ontology:cs_reference_frame('7ffc684e-fcb1-4b3c-a830-d7d8d05c6308', marketplace_of_ideas_paradigm).
narrative_ontology:cs_drift_state('7ffc684e-fcb1-4b3c-a830-d7d8d05c6308', contemporary_digital_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7ffc684e-fcb1-4b3c-a830-d7d8d05c6308', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__absolutist_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, speakers).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, public_discourse).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, targets_of_harmful_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, communities_seeking_regulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enjoy broad protection for their expression, with a very high bar for any regulation or legal consequence. They benefit from the constraint's high harm override threshold, allowing them to express controversial or potentially offensive views without significant legal risk.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, speakers, beneficiary,
    powerful, biographical, mobile, global).

% Bear the direct costs of speech-related harms (e.g., harassment, defamation, incitement to violence) that do not meet the extremely high threshold for unprotected speech. Their avenues for legal redress or social protection are severely limited by the constraint.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, targets_of_harmful_speech, payer,
    powerless, immediate, trapped, local).

% Benefits from the robust exchange of ideas, even controversial ones, which is fostered by near-absolute speech protection. The marketplace of ideas is theoretically enriched, though it may also become more polluted by harmful content.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, public_discourse, beneficiary,
    institutional, civilizational, analytical, universal).

% Seek to regulate speech that they perceive as harmful to their members or social cohesion (e.g., hate speech, misinformation). They face significant legal and political barriers due to the high threshold for speech regulation, effectively bearing the cost of unmitigated harms.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, communities_seeking_regulation, payer,
    organized, generational, constrained, national).

% Interpret and enforce the constitutional principles of speech protection, setting the high bar for harm overrides. They are the primary arbiters of what speech is protected and what falls into the narrowly defined unprotected categories.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Are constrained in their ability to pass laws regulating speech, as such laws are subject to strict judicial scrutiny under this absolutist reading. They can propose regulations but often see them struck down if they impinge on protected speech.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Analyze and critique the legal framework of speech protection, debating its philosophical underpinnings, practical effects, and consistency with other constitutional values. They provide the intellectual context for ongoing legal and political contests.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__absolutist_reading, speakers).
narrative_ontology:fixing_cost_class(speech_harm_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, high bar for speech regulation, promoting open debate and preventing chilling effects on expression, thereby coordinating expectations around expressive freedom.
% TRANSFER_FUNCTION: Transfers the burden of speech-related harm from speakers to targets, in exchange for broad expressive freedom and a robust (though potentially chaotic) public discourse.
% ABSENT_VOICES: Those whose speech is effectively silenced by the overwhelming volume or vitriol of protected harmful speech, or those who lack the resources to counter powerful speakers. Their concerns are often marginalized in a framework prioritizing speaker autonomy.
% DISAPPEARANCE_RATIONALE: If near-absolute speech protection vanished, the legal landscape for communication would fundamentally shift. Legislatures would immediately attempt to regulate various forms of speech, leading to a chilling effect on controversial expression and a complete reorganization of legal and social norms around communication.
% FOUNDING_PROBLEM: To prevent government censorship and ensure a robust marketplace of ideas, protecting individual autonomy in expression from state overreach.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and historical analysis corroborate the intent to prevent government censorship. However, targets of harmful speech and some social scientists argue the founding problem has shifted to managing private harms in a digital age, which the current framework fails to address, leading to a contested status.
narrative_ontology:disappearance_verdict(speech_harm_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The 'claimed_type' is 'rope' because this reading frames speech protection as a fundamental coordination mechanism for a free society. However, the metrics reflect a different reality: 'extractiveness' is high (0.7) because the system extracts the right to be free from certain harms from targets of speech. 'Suppression' is high (0.8) for any attempts to regulate speech, as the legal framework actively suppresses such efforts. 'Accessibility collapse' is high for those seeking redress, while 'resistance' comes from those harmed. The 'theater_ratio' is low (0.1) as the enforcement of broad speech protection is genuine, not performative. The measurement series show relative stability over the period, indicating a consistent application of this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of speakers, this constraint is a 'rope' or even a 'mountain'—a fundamental right enabling free expression. From the perspective of targets of harmful speech, it operates more like a 'snare' or 'tangled_rope', extracting their right to safety and dignity in favor of speaker autonomy. The engine's computation will highlight this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers and the abstract 'public_discourse' are the primary beneficiaries, experiencing low directionality as the constraint subsidizes their expressive freedom. Targets of harmful speech and communities seeking regulation are the primary payers/victims, experiencing high directionality as they bear the unmitigated costs of protected harmful speech. Courts and legislatures act as agenda-setters, enforcing the high threshold for regulation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by distinguishing the ideal (a 'rope' for free expression) from the practical reality of high extraction from targets. It highlights that while the founding problem of preventing government censorship remains relevant, the constraint's operation has shifted to impose significant unmitigated harms on certain populations, suggesting a potential for mandatrophy if the original mandate is over-applied to new contexts (like digital harms) without adaptation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identity,
    'Is this constraint a genuine ''absolutist_reading'' of speech protection, or does it implicitly incorporate elements of harm balancing?',
    'Analysis of judicial decisions: if courts consistently apply a strict, narrow set of unprotected categories without explicit balancing, it supports the absolutist claim. If implicit balancing occurs, it suggests a drift towards the harm_balancing_reading.',
    'If it''s a pure absolutist reading, the extraction from targets is inherent to the framework. If it implicitly balances, the framework is less extractive than claimed, and the classification might shift towards a ''tangled_rope'' with a lower effective extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity, conceptual, 'Distinguishing the absolutist reading from implicit harm balancing.').

omega_variable(
    harm_threshold_justification,
    'Is the extremely high harm override threshold genuinely necessary for a robust marketplace of ideas, or does it primarily serve to protect powerful speakers?',
    'Empirical studies on the effects of speech regulation in different jurisdictions, and analysis of power dynamics in public discourse. If robust discourse can exist with lower thresholds, the justification is weakened.',
    'If the high threshold is not empirically necessary for robust discourse, the ''rope'' claim is undermined, and the constraint''s extractive nature from targets becomes more salient, pushing classification towards ''snare'' or ''tangled_rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_threshold_justification, empirical, 'Justification for the high harm override threshold.').

omega_variable(
    sibling_harm_balancing_impact,
    'How would adopting the ''harm_balancing_reading'' structurally alter this constraint''s operation?',
    'Counterfactual legal analysis: modeling judicial decisions under a harm-balancing framework, examining how the scope of unprotected speech and the burden on targets would shift.',
    'The ''harm_balancing_reading'' would likely lower the extractiveness from targets of harmful speech by expanding the scope of regulable speech, potentially shifting the classification towards a ''tangled_rope'' with more symmetric costs and benefits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_harm_balancing_impact, conceptual, 'Impact of the harm_balancing_reading on this constraint.').

omega_variable(
    sibling_dignity_impact,
    'How would adopting the ''dignity_reading'' structurally alter this constraint''s operation?',
    'Counterfactual legal analysis: modeling judicial decisions under a dignity-based framework, examining how categorical exclusions for personhood-denying speech would be defined and applied.',
    'The ''dignity_reading'' would introduce new categorical exclusions for speech deemed to violate human dignity, significantly reducing the extractiveness from targets of such speech and potentially shifting the classification towards a ''tangled_rope'' or even a ''rope'' for a more inclusive public sphere, but with higher suppression for certain types of expression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_dignity_impact, conceptual, 'Impact of the dignity_reading on this constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__absolutist_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1970, speech_harm_boundary__absolutist_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(spee_tr_t1980, speech_harm_boundary__absolutist_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(spee_tr_t1990, speech_harm_boundary__absolutist_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(spee_tr_t2000, speech_harm_boundary__absolutist_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(spee_tr_t2010, speech_harm_boundary__absolutist_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(spee_tr_t2020, speech_harm_boundary__absolutist_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(spee_be_t1970, speech_harm_boundary__absolutist_reading, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(spee_be_t1980, speech_harm_boundary__absolutist_reading, base_extractiveness, 1980, 0.67).
narrative_ontology:measurement(spee_be_t1990, speech_harm_boundary__absolutist_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(spee_be_t2000, speech_harm_boundary__absolutist_reading, base_extractiveness, 2000, 0.69).
narrative_ontology:measurement(spee_be_t2010, speech_harm_boundary__absolutist_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(spee_be_t2020, speech_harm_boundary__absolutist_reading, base_extractiveness, 2020, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1970, speech_harm_boundary__absolutist_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(spee_su_t1980, speech_harm_boundary__absolutist_reading, suppression_requirement, 1980, 0.77).
narrative_ontology:measurement(spee_su_t1990, speech_harm_boundary__absolutist_reading, suppression_requirement, 1990, 0.78).
narrative_ontology:measurement(spee_su_t2000, speech_harm_boundary__absolutist_reading, suppression_requirement, 2000, 0.79).
narrative_ontology:measurement(spee_su_t2010, speech_harm_boundary__absolutist_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(spee_su_t2020, speech_harm_boundary__absolutist_reading, suppression_requirement, 2020, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__harm_balancing_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'speech_harm_boundary' kernel. Its absolutist interpretation structurally influences and is influenced by alternative readings, such as the harm-balancing and dignity-based approaches, which propose different thresholds and categorical exclusions for harmful speech.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
