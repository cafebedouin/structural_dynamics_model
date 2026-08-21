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
 *   constraint_id: speech_protection_kernel__marketplace_reading
 *   human_readable: Speech as Marketplace of Ideas
 *   domain: Constitutional Law / Political Philosophy / Communication Rights
 *
 * SUMMARY:
 *   This constraint story instantiates the 'marketplace of ideas' reading of
 *   the broader speech protection kernel. It posits that the primary purpose
 *   of free speech is to facilitate truth-discovery, with false or harmful
 *   speech best countered by more speech, rather than by state intervention.
 *   This reading emphasizes collective epistemic benefit over individual
 *   autonomy and generally rejects content-based restrictions as distorting
 *   the truth-discovery process. While ideally a 'rope' for coordinating
 *   public discourse, its real-world operation, especially in the digital
 *   age, faces significant challenges regarding its efficacy and fairness.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, 0.45).
domain_priors:suppression_score(speech_protection_kernel__marketplace_reading, 0.15).
domain_priors:theater_ratio(speech_protection_kernel__marketplace_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__marketplace_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__marketplace_reading, "Speech as Marketplace of Ideas").
narrative_ontology:topic_domain(speech_protection_kernel__marketplace_reading, "Constitutional Law / Political Philosophy / Communication Rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__marketplace_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__marketplace_reading, '0a3ad033-1222-414f-b5d7-16d0e0b61d1b').
narrative_ontology:cs_kernel_codification('0a3ad033-1222-414f-b5d7-16d0e0b61d1b', fixed_text).
narrative_ontology:cs_authority_grounding('0a3ad033-1222-414f-b5d7-16d0e0b61d1b', lineage).
narrative_ontology:cs_interpretation_layer_present('0a3ad033-1222-414f-b5d7-16d0e0b61d1b').
narrative_ontology:cs_reading_relation('0a3ad033-1222-414f-b5d7-16d0e0b61d1b', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a3ad033-1222-414f-b5d7-16d0e0b61d1b', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a3ad033-1222-414f-b5d7-16d0e0b61d1b', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a3ad033-1222-414f-b5d7-16d0e0b61d1b', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_axiom('0a3ad033-1222-414f-b5d7-16d0e0b61d1b', foundational, truth_emerges_from_free_exchange).
narrative_ontology:cs_axiom_status(truth_emerges_from_free_exchange, holdable).
narrative_ontology:cs_axiom_grounding('0a3ad033-1222-414f-b5d7-16d0e0b61d1b', truth_emerges_from_free_exchange, empirically_contingent).
narrative_ontology:cs_axiom('0a3ad033-1222-414f-b5d7-16d0e0b61d1b', foundational, state_is_unfit_to_regulate_content).
narrative_ontology:cs_axiom_status(state_is_unfit_to_regulate_content, holdable).
narrative_ontology:cs_axiom_grounding('0a3ad033-1222-414f-b5d7-16d0e0b61d1b', state_is_unfit_to_regulate_content, deontological).
narrative_ontology:cs_axiom('0a3ad033-1222-414f-b5d7-16d0e0b61d1b', secondary, more_speech_is_the_remedy).
narrative_ontology:cs_axiom_status(more_speech_is_the_remedy, holdable).
narrative_ontology:cs_axiom_grounding('0a3ad033-1222-414f-b5d7-16d0e0b61d1b', more_speech_is_the_remedy, instrumental).
narrative_ontology:cs_reference_frame('0a3ad033-1222-414f-b5d7-16d0e0b61d1b', unfettered_discourse_ideal).
narrative_ontology:cs_drift_state('0a3ad033-1222-414f-b5d7-16d0e0b61d1b', digital_age_disinformation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0a3ad033-1222-414f-b5d7-16d0e0b61d1b', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__marketplace_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, truth_seekers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, speakers_advocates).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, social_media_platforms).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, targets_of_false_harmful_speech).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, disinformation_agents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and groups who rely on open discourse to discern facts, evaluate arguments, and form informed opinions. They benefit from the ideal of a self-correcting marketplace but bear the cost of sifting through misinformation.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, truth_seekers, beneficiary,
    moderate, biographical, mobile, global).

% Individuals and organizations who champion the right to speak freely, believing that more speech is always the answer to bad speech. They benefit from broad protection but may overlook the practical challenges of countering harmful narratives.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, speakers_advocates, beneficiary,
    moderate, immediate, mobile, local).

% The primary interpreters and enforcers of speech protection, often grounding their decisions in the marketplace of ideas rationale. They define the boundaries of protected speech and the remedies for its abuse.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, courts_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Individuals or groups who are subjected to false, defamatory, or harmful speech, and who often lack the resources or platform to effectively counter it with 'more speech'. They bear the direct costs of the marketplace's failures.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, targets_of_false_harmful_speech, payer,
    powerless, immediate, trapped, local).

% Elected representatives who consider policy responses to speech-related harms, often balancing the marketplace ideal with other societal values. They observe the effects of the current framework and may propose legislative changes.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, legislators, observer,
    institutional, biographical, constrained, national).

% Private entities that host and mediate a vast amount of public discourse. While benefiting from the broad protection of speech, they also face pressure to moderate harmful content, often struggling to reconcile the marketplace ideal with practical enforcement.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, social_media_platforms, agenda_setter,
    institutional, biographical, constrained, global).

% Actors who intentionally spread false or misleading information, exploiting the open nature of the marketplace of ideas. They benefit from the principle's resistance to content-based regulation, making their activities harder to curb.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, disinformation_agents, beneficiary,
    powerful, immediate, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__marketplace_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__marketplace_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a framework for society to collectively identify and validate ideas, ensuring a robust and open exchange where truth can ultimately prevail over falsehood through rational debate.
% TRANSFER_FUNCTION: Transfers the primary burden of discerning truth and countering harmful speech from state censorship or regulation to individual citizens and the broader public sphere. It also transfers the risk of harm from false speech to its targets, expecting them to use 'more speech' as a remedy.
% ABSENT_VOICES: Those harmed by speech who lack the resources, platform, or social capital to effectively counter it. This includes marginalized communities, individuals targeted by coordinated harassment campaigns, and victims of sophisticated disinformation operations.
% DISAPPEARANCE_RATIONALE: If the marketplace of ideas principle vanished overnight, the legal and philosophical underpinnings of free speech in many democracies would collapse. This would likely lead to a rapid increase in content-based speech regulations, a redefinition of permissible expression, and a fundamental shift in how societies manage public discourse and information.
% FOUNDING_PROBLEM: To prevent state censorship and ensure that society has the best chance of discovering truth and making informed decisions by allowing all ideas, even unpopular or offensive ones, to be aired and debated.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and civil liberties advocates (pro-marketplace) argue the problem of state censorship and the need for truth-discovery remain live. Critical theorists, social justice advocates, and some communication scholars (anti-marketplace) argue that the marketplace has failed to deliver truth and instead amplifies harm, rendering the founding problem 'dead' in its original formulation, or at least substantially altered by new communication technologies. Legislative hearings and independent academic studies provide corroborating evidence for both perspectives.
narrative_ontology:disappearance_verdict(speech_protection_kernel__marketplace_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__marketplace_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__marketplace_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(speech_protection_kernel__marketplace_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__marketplace_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   The claimed type is 'rope' because, in its ideal form, the marketplace of ideas is a coordination mechanism for collective truth-seeking, benefiting all participants. However, the 'extractiveness' is moderate (0.45) and rising, reflecting the real-world costs borne by targets of harmful speech and the exploitation by disinformation agents when the 'more speech' remedy fails. 'Suppression' is low (0.15) as the principle actively resists censorship. 'Theater ratio' is moderate (0.3) as the ideal is often invoked performatively even when the mechanism is struggling. 'Resistance' is high (0.7) due to ongoing debates about its effectiveness and equity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of truth seekers and speakers' advocates, the marketplace of ideas is a vital, if imperfect, 'rope' for societal progress. From the perspective of targets of false/harmful speech, it can feel like a 'snare' or 'tangled rope', where they bear significant costs with inadequate recourse. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Truth seekers and speakers' advocates are beneficiaries, as the principle aims to foster an environment conducive to their goals. Courts and social media platforms act as agenda setters, interpreting and enforcing the rules of this marketplace. Targets of false/harmful speech are payers/victims, bearing the direct costs when the self-correction mechanism is insufficient. Disinformation agents are also beneficiaries, as the broad protection allows them to operate with less state interference.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marketplace_efficacy_ambiguity,
    'Does the ''marketplace of ideas'' mechanism reliably lead to truth-discovery in practice, especially in the digital information environment?',
    'Empirical studies on the spread and correction of misinformation across different platforms and social contexts; analysis of public discourse quality and epistemic outcomes.',
    'If efficacy is low, the constraint''s actual extractiveness and theater ratio are higher than its ideal, potentially reclassifying it from ''rope'' to ''tangled_rope'' or ''snare'' for certain seats. If efficacy is high, it reinforces the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marketplace_efficacy_ambiguity, empirical, 'Whether the marketplace of ideas effectively self-corrects.').

omega_variable(
    power_imbalance_ambiguity,
    'Can ''more speech'' effectively counter harmful speech when there are significant power imbalances between speakers and targets, or when platforms amplify certain voices?',
    'Sociological and communication studies analyzing the impact of power dynamics on the effectiveness of counter-speech, and the role of platform algorithms in shaping information flow.',
    'If power imbalances systematically undermine counter-speech, the ''targets_of_false_harmful_speech'' seat experiences higher effective extraction and suppression, pushing their classification towards ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_imbalance_ambiguity, empirical, 'Impact of power imbalances on counter-speech effectiveness.').

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading of the ''speech_protection_kernel''. What structural elements would change if a sibling reading (e.g., ''dignity_reading'' or ''harm_threshold_reading'') were adopted?',
    'Comparative legal analysis of jurisdictions that prioritize dignity or harm thresholds, examining their speech regulations and judicial outcomes.',
    'Adopting a ''dignity_reading'' would likely increase suppression for certain categories of speech and decrease extractiveness for vulnerable groups. A ''harm_threshold_reading'' would introduce content-based restrictions, altering the balance between free expression and protection from demonstrable harm.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural changes under alternative kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__marketplace_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__marketplace_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__marketplace_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__marketplace_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(spee_tr_t60, speech_protection_kernel__marketplace_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(spee_tr_t80, speech_protection_kernel__marketplace_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement(spee_tr_t100, speech_protection_kernel__marketplace_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__marketplace_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__marketplace_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__marketplace_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(spee_be_t60, speech_protection_kernel__marketplace_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(spee_be_t80, speech_protection_kernel__marketplace_reading, base_extractiveness, 80, 0.43).
narrative_ontology:measurement(spee_be_t100, speech_protection_kernel__marketplace_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__marketplace_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__marketplace_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__marketplace_reading, suppression_requirement, 40, 0.13).
narrative_ontology:measurement(spee_su_t60, speech_protection_kernel__marketplace_reading, suppression_requirement, 60, 0.14).
narrative_ontology:measurement(spee_su_t80, speech_protection_kernel__marketplace_reading, suppression_requirement, 80, 0.15).
narrative_ontology:measurement(spee_su_t100, speech_protection_kernel__marketplace_reading, suppression_requirement, 100, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__marketplace_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'speech_protection_kernel', each representing a distinct structural claim about the purpose and limits of free speech. Other readings include 'absolutist_reading', 'democratic_participation_reading', 'dignity_reading', and 'harm_threshold_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
