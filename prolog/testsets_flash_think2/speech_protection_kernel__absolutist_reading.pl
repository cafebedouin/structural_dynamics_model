% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__absolutist_reading, []).

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
 *   constraint_id: speech_protection_kernel__absolutist_reading
 *   human_readable: Absolutist Speech Protection (Listener Harm Not Grounds for Restriction)
 *   domain: Constitutional Law / Political Philosophy / Communication Rights
 *
 * SUMMARY:
 *   This constraint represents an 'absolutist' reading of speech protection,
 *   where speech is protected near-categorically, and listener harm is
 *   explicitly not considered a valid ground for restriction. This reading
 *   prioritizes speaker autonomy and the prevention of state censorship above
 *   all else. It is one specific interpretation of the broader
 *   'speech_protection_kernel' within constitutional law and political
 *   philosophy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, 0.85).
domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, 0.9).
domain_priors:theater_ratio(speech_protection_kernel__absolutist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__absolutist_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__absolutist_reading, "Absolutist Speech Protection (Listener Harm Not Grounds for Restriction)").
narrative_ontology:topic_domain(speech_protection_kernel__absolutist_reading, "Constitutional Law / Political Philosophy / Communication Rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__absolutist_reading, 'b977ddb2-2dbc-4d31-983f-97d526abc79d').
narrative_ontology:cs_kernel_codification('b977ddb2-2dbc-4d31-983f-97d526abc79d', fixed_text).
narrative_ontology:cs_authority_grounding('b977ddb2-2dbc-4d31-983f-97d526abc79d', lineage).
narrative_ontology:cs_interpretation_layer_present('b977ddb2-2dbc-4d31-983f-97d526abc79d').
narrative_ontology:cs_reading_relation('b977ddb2-2dbc-4d31-983f-97d526abc79d', speech_protection_kernel__harm_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('b977ddb2-2dbc-4d31-983f-97d526abc79d', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('b977ddb2-2dbc-4d31-983f-97d526abc79d', speech_protection_kernel__dignity_reading, forecloses).
narrative_ontology:cs_reading_relation('b977ddb2-2dbc-4d31-983f-97d526abc79d', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('b977ddb2-2dbc-4d31-983f-97d526abc79d', foundational, speaker_autonomy_is_primary).
narrative_ontology:cs_axiom_status(speaker_autonomy_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('b977ddb2-2dbc-4d31-983f-97d526abc79d', speaker_autonomy_is_primary, deontological).
narrative_ontology:cs_axiom('b977ddb2-2dbc-4d31-983f-97d526abc79d', foundational, content_neutrality_is_absolute).
narrative_ontology:cs_axiom_status(content_neutrality_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('b977ddb2-2dbc-4d31-983f-97d526abc79d', content_neutrality_is_absolute, conventional).
narrative_ontology:cs_reference_frame('b977ddb2-2dbc-4d31-983f-97d526abc79d', unfettered_expression_paradigm).
narrative_ontology:cs_drift_state('b977ddb2-2dbc-4d31-983f-97d526abc79d', contemporary_digital_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b977ddb2-2dbc-4d31-983f-97d526abc79d', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__absolutist_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, free_speech_advocates).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, targets_of_harmful_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, advocates_for_speech_regulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals or groups who express themselves publicly, benefiting from broad protection against restriction based on the content or potential impact of their speech on listeners. They face minimal legal risk for speech unless it falls into very narrow, historically unprotected categories.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, speakers, beneficiary,
    moderate, biographical, mobile, global).

% Individuals or groups who experience direct harm (e.g., harassment, defamation, incitement to violence) from speech that is protected under this absolutist reading. They bear the costs of such speech with little to no legal recourse, as their harm is not considered a valid ground for restriction.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, targets_of_harmful_speech, payer,
    powerless, immediate, trapped, local).

% Organizations and legal scholars who champion the broadest possible interpretation of speech protection, viewing any restriction based on listener harm as a dangerous precedent that could lead to censorship. They benefit from the legal framework aligning with their ideological position.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, free_speech_advocates, beneficiary,
    organized, generational, analytical, national).

% Organizations and legal scholars who argue for greater consideration of listener harm and the social impact of speech, seeking to expand the grounds for restriction beyond the narrow categorical exclusions. They bear the cost of their policy preferences being rejected by the prevailing legal interpretation.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, advocates_for_speech_regulation, payer,
    organized, generational, constrained, national).

% The primary enforcers and interpreters of speech protection, who apply the absolutist reading by consistently rejecting listener harm as a basis for restricting speech. Their institutional role is to uphold established precedent, which under this reading, favors broad protection.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, courts_and_judiciary, agenda_setter,
    institutional, civilizational, constrained, national).

% The legislative body that could theoretically pass laws restricting speech, but whose power is constrained by judicial interpretation of constitutional speech protections. They observe the effects of the absolutist reading and may attempt to legislate within its bounds or challenge it through constitutional amendment.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, legislature, observer,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, high bar for speech restriction, reducing uncertainty for speakers about the permissible scope of their expression and fostering a climate of open discourse.
% TRANSFER_FUNCTION: Transfers the burden of potential harm from speakers to listeners, who must tolerate speech even if it causes distress or contributes to systemic disadvantage. It also transfers autonomy and expressive freedom to speakers.
% ABSENT_VOICES: Those whose identities are systematically targeted by harmful speech (e.g., hate speech, harassment), who are often marginalized and whose experiences of harm are discounted by the absolutist framework. They would argue for a rebalancing of rights to include protection from speech-inflicted harm.
% DISAPPEARANCE_RATIONALE: If this absolutist protection vanished overnight, there would be immediate and intense pressure to regulate speech based on listener harm, leading to a significant re-evaluation of communication norms, legal frameworks, and public discourse. The legal landscape of free expression would be fundamentally reshaped.
% FOUNDING_PROBLEM: To prevent government censorship and ensure a robust public discourse, protecting even unpopular, offensive, or potentially harmful speech from state interference, thereby fostering a marketplace of ideas.
% FOUNDING_PROBLEM_CORROBORATION: Historical legal scholars and civil liberties organizations attest to the original intent of preventing state censorship. Opponents (e.g., critical race theorists, feminist legal scholars) attest that the founding problem has shifted to address harms from private power and systemic disadvantage, which the absolutist reading fails to address, thus rendering the original solution over-applied.
narrative_ontology:disappearance_verdict(speech_protection_kernel__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(speech_protection_kernel__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__absolutist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because this reading systematically denies recourse to those harmed by speech, effectively extracting their right to be free from such harm. Suppression is also very high (0.90) as it actively suppresses any attempts to introduce harm-based restrictions on speech. Resistance is substantial (0.70) from groups advocating for greater speech regulation. The claimed type is 'rope' because proponents frame it as a beneficial coordination mechanism for free expression, despite its extractive effects on victims.
 *
 * PERSPECTIVAL GAP:
 *   Speakers and free speech advocates experience this constraint as a robust 'rope' that coordinates broad expressive freedom. Targets of harmful speech and advocates for speech regulation, however, experience it as a 'snare' or 'tangled_rope' that extracts their safety and dignity, while coordinating the speech of others. The engine will compute this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers and free speech advocates are clear beneficiaries, as the constraint maximizes their expressive freedom and minimizes their liability. Targets of harmful speech and advocates for speech regulation are victims, bearing the costs of unaddressed harm and the frustration of their policy goals. Courts act as agenda-setters, enforcing this particular interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, standalone principle, or is it merely one reading of the broader ''speech_protection_kernel''?',
    'Analysis of legal and philosophical discourse: if the ''absolutist'' position is consistently debated in relation to other balancing tests, it confirms its status as a reading rather than an independent principle.',
    'If confirmed as a reading, its classification is understood as perspectival within a contested kernel; if standalone, its classification is universal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint''s identity as a specific reading of a contested kernel.').

omega_variable(
    harm_definition_ambiguity,
    'What constitutes ''harm'' from speech, and is the absolutist rejection of it based on a narrow or expansive definition of harm?',
    'Empirical studies on the psychological and social impacts of different types of speech, combined with legal analysis of how ''harm'' is defined in other areas of law.',
    'If ''harm'' is narrowly defined, the absolutist position might be more defensible; if broadly defined (e.g., including systemic subordination), the absolutist position''s extractiveness from victims is amplified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_definition_ambiguity, empirical, 'Ambiguity in the definition of ''listener harm'' and its implications for speech restriction.').

omega_variable(
    absolutism_sustainability,
    'Is the absolutist stance on speech protection sustainable in an era of pervasive digital communication and amplified harms?',
    'Longitudinal analysis of societal cohesion, mental health outcomes, and democratic stability in jurisdictions with absolutist vs. more balanced speech regimes.',
    'If unsustainable, the constraint''s long-term viability as a ''rope'' is challenged, potentially reclassifying it as a ''piton'' (maintained by inertia) or ''snare'' (actively harmful).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(absolutism_sustainability, empirical, 'The long-term viability of an absolutist speech protection framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__absolutist_reading, 1940, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1940, speech_protection_kernel__absolutist_reading, theater_ratio, 1940, 0.15).
narrative_ontology:measurement(spee_tr_t1960, speech_protection_kernel__absolutist_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(spee_tr_t1980, speech_protection_kernel__absolutist_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_kernel__absolutist_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_kernel__absolutist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(spee_be_t1940, speech_protection_kernel__absolutist_reading, base_extractiveness, 1940, 0.7).
narrative_ontology:measurement(spee_be_t1960, speech_protection_kernel__absolutist_reading, base_extractiveness, 1960, 0.75).
narrative_ontology:measurement(spee_be_t1980, speech_protection_kernel__absolutist_reading, base_extractiveness, 1980, 0.8).
narrative_ontology:measurement(spee_be_t2000, speech_protection_kernel__absolutist_reading, base_extractiveness, 2000, 0.83).
narrative_ontology:measurement(spee_be_t2024, speech_protection_kernel__absolutist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1940, speech_protection_kernel__absolutist_reading, suppression_requirement, 1940, 0.75).
narrative_ontology:measurement(spee_su_t1960, speech_protection_kernel__absolutist_reading, suppression_requirement, 1960, 0.8).
narrative_ontology:measurement(spee_su_t1980, speech_protection_kernel__absolutist_reading, suppression_requirement, 1980, 0.85).
narrative_ontology:measurement(spee_su_t2000, speech_protection_kernel__absolutist_reading, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(spee_su_t2024, speech_protection_kernel__absolutist_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'speech_protection_kernel'. Its siblings represent alternative interpretations of speech rights and their limits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
