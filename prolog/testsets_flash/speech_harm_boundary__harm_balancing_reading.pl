% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__harm_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__harm_balancing_reading, []).

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
 *   constraint_id: speech_harm_boundary__harm_balancing_reading
 *   human_readable: Speech Harm Boundary (Proportionality Balancing Reading)
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'harm balancing' reading of speech
 *   protection, where free speech is presumed protected but can be restricted
 *   if it demonstrably causes harm, subject to a proportionality test. This
 *   reading acknowledges the social costs of unbridled speech and seeks to
 *   mitigate them through active enforcement. It is one of several competing
 *   interpretations of the fundamental 'speech harm boundary' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, 0.45).
domain_priors:suppression_score(speech_harm_boundary__harm_balancing_reading, 0.3).
domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__harm_balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__harm_balancing_reading, "Speech Harm Boundary (Proportionality Balancing Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__harm_balancing_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__harm_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__harm_balancing_reading, 'e8aac706-379d-4df5-b3a1-dc13e0240800').
narrative_ontology:cs_kernel_codification('e8aac706-379d-4df5-b3a1-dc13e0240800', formalized).
narrative_ontology:cs_authority_grounding('e8aac706-379d-4df5-b3a1-dc13e0240800', lineage).
narrative_ontology:cs_interpretation_layer_present('e8aac706-379d-4df5-b3a1-dc13e0240800').
narrative_ontology:cs_reading_relation('e8aac706-379d-4df5-b3a1-dc13e0240800', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e8aac706-379d-4df5-b3a1-dc13e0240800', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('e8aac706-379d-4df5-b3a1-dc13e0240800', foundational, speech_presumptively_protected).
narrative_ontology:cs_axiom_status(speech_presumptively_protected, holdable).
narrative_ontology:cs_axiom_grounding('e8aac706-379d-4df5-b3a1-dc13e0240800', speech_presumptively_protected, deontological).
narrative_ontology:cs_axiom('e8aac706-379d-4df5-b3a1-dc13e0240800', foundational, demonstrable_harm_justifies_restriction).
narrative_ontology:cs_axiom_status(demonstrable_harm_justifies_restriction, holdable).
narrative_ontology:cs_axiom_grounding('e8aac706-379d-4df5-b3a1-dc13e0240800', demonstrable_harm_justifies_restriction, empirically_contingent).
narrative_ontology:cs_reference_frame('e8aac706-379d-4df5-b3a1-dc13e0240800', liberal_democratic_balancing_framework).
narrative_ontology:cs_drift_state('e8aac706-379d-4df5-b3a1-dc13e0240800', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e8aac706-379d-4df5-b3a1-dc13e0240800', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, society_at_large).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, vulnerable_groups).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, speakers_of_harmful_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, speech_platforms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a more orderly public discourse and reduced social friction, as speech that demonstrably causes harm can be restricted. This contributes to social cohesion and the protection of individual and group rights, but also accepts some limits on expressive freedom.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, society_at_large, beneficiary,
    organized, generational, constrained, national).

% Receives protection from speech that directly incites violence, harassment, or discrimination, allowing for greater participation in public life without fear of targeted abuse. This protection is contingent on demonstrating harm, which can be an onerous burden.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, vulnerable_groups, beneficiary,
    moderate, biographical, constrained, local).

% Bears the cost of speech restrictions when their expression is deemed to cause demonstrable harm, leading to censorship, fines, or other penalties. Their ability to express themselves is curtailed, even if they believe their speech is legitimate or necessary.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, speakers_of_harmful_speech, payer,
    powerless, immediate, constrained, local).

% Incurs costs for content moderation, legal defense against claims of harm, and potential regulatory penalties. They are tasked with balancing free expression with harm prevention, often leading to difficult and contested decisions about what speech to allow or remove.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, speech_platforms, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__harm_balancing_reading, speech_platforms, agenda_setter).

% Define and enforce the boundaries of protected speech, applying proportionality tests to balance expressive freedom against potential harm. They adjudicate disputes and set precedents, shaping the practical application of this constraint.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, courts_and_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Argue for near-absolute protection of speech, believing that any restriction, even for demonstrable harm, opens the door to censorship and tyranny. Their arguments are often considered but rarely fully adopted in this balancing framework.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, absolutist_advocates, excluded,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of free speech with the protection of individuals and groups from demonstrable harm, aiming for a public sphere where expression can flourish without causing undue injury.
% TRANSFER_FUNCTION: Transfers the burden of demonstrating harm from the speaker to the affected parties or the state, and transfers the cost of restriction from society to speakers of harmful speech and platforms that host it.
% ABSENT_VOICES: Those who believe that speech should be absolutely protected, regardless of harm, are often marginalized in this framework, as are those who believe that dignity alone should be sufficient to restrict speech without needing to demonstrate harm.
% DISAPPEARANCE_RATIONALE: If the principle of balancing speech against harm vanished, public discourse would likely become more chaotic and potentially abusive, with increased instances of hate speech, harassment, and incitement to violence. Vulnerable groups would face greater threats, and platforms would struggle with unchecked harmful content, leading to a breakdown in social trust and order.
% FOUNDING_PROBLEM: The problem of how to reconcile the fundamental right to free expression with the equally fundamental need to protect individuals and groups from speech that causes direct, demonstrable harm, particularly in diverse and pluralistic societies.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, human rights organizations, and victims' advocacy groups consistently attest to the ongoing challenge of balancing these competing values. International human rights law also reflects this ongoing tension, corroborating the live status of the founding problem.
narrative_ontology:disappearance_verdict(speech_harm_boundary__harm_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__harm_balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__harm_balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_harm_boundary__harm_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__harm_balancing_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__harm_balancing_reading_tests).
:- end_tests(speech_harm_boundary__harm_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the costs borne by speakers whose speech is restricted and by platforms that must moderate content. Suppression (0.30) is also moderate, as active enforcement is required to identify and restrict harmful speech. The theater ratio (0.10) is low, indicating that the balancing act is a genuine, ongoing effort, not merely performative. Accessibility collapse (0.40) is moderate, as alternatives to harmful speech exist, but the option to express certain types of speech is curtailed. Resistance (0.25) is present from those who advocate for broader speech protections.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'vulnerable_groups' and 'society_at_large', this constraint functions as a necessary 'rope' or 'scaffold' for a more inclusive public sphere. However, from the perspective of 'speakers_of_harmful_speech' and 'absolutist_advocates', it can be perceived as a 'snare' that unduly restricts fundamental freedoms. The 'speech_platforms' experience it as a 'tangled_rope', balancing coordination benefits with significant enforcement costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Society and vulnerable groups are beneficiaries, as they gain protection from harm. Speakers of harmful speech and speech platforms are payers, bearing the costs of restriction and moderation. Courts and regulators are agenda-setters, defining and enforcing the boundaries. Absolutist advocates are excluded, as their position is not fully integrated into this balancing framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is actively maintained and its founding problem (balancing speech and harm) is still live, so mandatrophy is not resolved. The increasing extractiveness and suppression over time reflect the growing complexity of managing harmful speech in digital environments, rather than an atrophy of function. The constraint's persistence is driven by the ongoing need to manage the social costs of speech, not by inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_demonstrable_harm,
    'What constitutes ''demonstrable harm'' in practice, and how consistently is this standard applied across different contexts and jurisdictions?',
    'Empirical analysis of court rulings and regulatory decisions, cross-jurisdictional comparative studies, and public discourse analysis to identify convergence or divergence in harm definitions.',
    'If ''demonstrable harm'' is inconsistently or arbitrarily defined, the constraint''s extractiveness and suppression could be higher than measured, functioning more like a ''snare'' for disfavored speech. If consistently and narrowly defined, it reinforces the ''tangled_rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_demonstrable_harm, empirical, 'Ambiguity in the operational definition and application of ''demonstrable harm''.').

omega_variable(
    proportionality_test_efficacy,
    'Is the proportionality balancing test genuinely effective in minimizing speech restrictions while maximizing harm prevention, or does it systematically favor one side?',
    'Longitudinal studies of speech restriction outcomes, comparing cases where the test was applied versus alternative frameworks. Analysis of judicial reasoning for bias or consistent patterns of outcome.',
    'If the test systematically favors harm prevention over speech, the constraint leans towards a ''snare'' for speakers. If it genuinely balances, it supports the ''tangled_rope'' classification. If it systematically favors speech, it leans towards a ''rope'' for speakers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_test_efficacy, empirical, 'Effectiveness and fairness of the proportionality balancing test.').

omega_variable(
    kernel_reading_identity,
    'Is this ''harm_balancing_reading'' truly distinct from the ''dignity_reading'' or ''absolutist_reading'', or do they represent points on a continuum rather than discrete structural claims?',
    'Conceptual analysis of the foundational axioms and their logical implications. If a single framework could consistently integrate all three without contradiction, they are not distinct constraints.',
    'If not truly distinct, the classification of this constraint as a ''tangled_rope'' might be an artifact of an artificial decomposition, and a broader, more complex constraint might be at play. If distinct, the current classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of the ''speech_harm_boundary'' kernel. Sibling readings include ''absolutist_reading'' (speech protection near-absolute) and ''dignity_reading'' (speech subordinate to human dignity). This omega documents the conceptual boundary between these readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__harm_balancing_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1960, speech_harm_boundary__harm_balancing_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(spee_tr_t1975, speech_harm_boundary__harm_balancing_reading, theater_ratio, 1975, 0.07).
narrative_ontology:measurement(spee_tr_t1990, speech_harm_boundary__harm_balancing_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(spee_tr_t2005, speech_harm_boundary__harm_balancing_reading, theater_ratio, 2005, 0.09).
narrative_ontology:measurement(spee_tr_t2024, speech_harm_boundary__harm_balancing_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(spee_be_t1960, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 1960, 0.35).
narrative_ontology:measurement(spee_be_t1975, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 1975, 0.38).
narrative_ontology:measurement(spee_be_t1990, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(spee_be_t2005, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(spee_be_t2024, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1960, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 1960, 0.2).
narrative_ontology:measurement(spee_su_t1975, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 1975, 0.23).
narrative_ontology:measurement(spee_su_t1990, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(spee_su_t2005, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 2005, 0.28).
narrative_ontology:measurement(spee_su_t2024, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__harm_balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'speech_harm_boundary' kernel. Its extractiveness and suppression metrics reflect the costs and enforcement associated with a proportionality balancing approach to speech restrictions, which differs significantly from both absolutist and dignity-first readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
