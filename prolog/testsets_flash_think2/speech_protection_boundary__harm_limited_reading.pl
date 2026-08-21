% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__harm_limited_reading, []).

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
 *   constraint_id: speech_protection_boundary__harm_limited_reading
 *   human_readable: Speech Protection Boundary: Harm-Limited Reading
 *   domain: Constitutional Law / Political Philosophy / Speech Regulation
 *
 * SUMMARY:
 *   This constraint represents the 'harm-limited' reading of speech
 *   protection, where the exercise of free speech is conditional on the
 *   absence of significant harm to dignity, equality, and freedom from
 *   harassment. This reading narrows the set of protected speech, including
 *   categories like hate speech and harassment within the unprotected domain.
 *   It empowers the state to act as a gatekeeper, defining and enforcing
 *   these harm boundaries, which carries inherent risks of abuse and
 *   overreach.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, 0.65).
domain_priors:suppression_score(speech_protection_boundary__harm_limited_reading, 0.75).
domain_priors:theater_ratio(speech_protection_boundary__harm_limited_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__harm_limited_reading, "Speech Protection Boundary: Harm-Limited Reading").
narrative_ontology:topic_domain(speech_protection_boundary__harm_limited_reading, "Constitutional Law / Political Philosophy / Speech Regulation").

domain_priors:requires_active_enforcement(speech_protection_boundary__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__harm_limited_reading, '83f41b96-385b-4521-b098-405df8d7a3f7').
narrative_ontology:cs_kernel_codification('83f41b96-385b-4521-b098-405df8d7a3f7', formalized).
narrative_ontology:cs_authority_grounding('83f41b96-385b-4521-b098-405df8d7a3f7', lineage).
narrative_ontology:cs_interpretation_layer_present('83f41b96-385b-4521-b098-405df8d7a3f7').
narrative_ontology:cs_reading_relation('83f41b96-385b-4521-b098-405df8d7a3f7', speech_protection_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('83f41b96-385b-4521-b098-405df8d7a3f7', speech_protection_boundary__balancing_reading, influences).
narrative_ontology:cs_axiom('83f41b96-385b-4521-b098-405df8d7a3f7', foundational, speech_is_not_absolute).
narrative_ontology:cs_axiom_status(speech_is_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('83f41b96-385b-4521-b098-405df8d7a3f7', speech_is_not_absolute, conventional).
narrative_ontology:cs_axiom('83f41b96-385b-4521-b098-405df8d7a3f7', foundational, dignity_and_equality_are_preconditions_for_speech).
narrative_ontology:cs_axiom_status(dignity_and_equality_are_preconditions_for_speech, holdable).
narrative_ontology:cs_axiom_grounding('83f41b96-385b-4521-b098-405df8d7a3f7', dignity_and_equality_are_preconditions_for_speech, deontological).
narrative_ontology:cs_reference_frame('83f41b96-385b-4521-b098-405df8d7a3f7', inclusive_public_sphere_framework).
narrative_ontology:cs_drift_state('83f41b96-385b-4521-b098-405df8d7a3f7', contemporary_social_justice_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('83f41b96-385b-4521-b098-405df8d7a3f7', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__harm_limited_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, state_regulators).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, marginalized_groups).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, advocates_for_equality).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, speakers_of_harmful_speech).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, free_speech_absolutists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the boundaries of 'significant harm' to dignity, equality, and freedom from harassment. They gain power and legitimacy by protecting vulnerable groups and maintaining social order, but also face accusations of censorship.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, state_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from protection against hate speech, harassment, and other forms of harmful expression that undermine their dignity and equality. This enables greater participation in public life, but they remain vulnerable to subtle or coded forms of harm.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, marginalized_groups, beneficiary,
    organized, generational, constrained, national).

% See their values of dignity and equality codified and enforced, gaining influence in shaping public discourse and legal interpretation. They actively lobby for broader definitions of harm and stronger enforcement.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, advocates_for_equality, beneficiary,
    organized, generational, mobile, national).

% Bear the direct costs of this constraint, as their speech is suppressed, potentially leading to legal penalties or social ostracization. Their options are to self-censor, face consequences, or attempt to reframe their speech as non-harmful.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, speakers_of_harmful_speech, payer,
    powerless, immediate, trapped, national).

% Experience this constraint as a curtailment of fundamental speech rights, arguing that it opens the door to censorship and thought control. They resist its expansion through legal challenges and public advocacy, but face an uphill battle against the prevailing interpretation.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, free_speech_absolutists, payer,
    organized, generational, constrained, national).

% Interprets and applies the harm standard in specific cases, setting precedents that shape the boundaries of protected speech. Its decisions are crucial in determining the practical scope and enforcement of the constraint.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, judicial_system, agenda_setter,
    institutional, generational, analytical, national).

% The arena where speech occurs, which is actively shaped by the constraint. It becomes more 'civil' for some, but potentially less 'free' for others, leading to ongoing debates about its quality and inclusivity.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, public_discourse, observer,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__harm_limited_reading, marginalized_groups).
narrative_ontology:fixing_cost_class(speech_protection_boundary__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate social interaction by establishing boundaries for speech that prevent significant harm to dignity, equality, and freedom from harassment, fostering a more inclusive and respectful public sphere.
% TRANSFER_FUNCTION: Transfers the right to speak certain categories of expression (deemed harmful) from individual speakers to the collective (or state), in exchange for enhanced protection of dignity and equality for vulnerable groups.
% ABSENT_VOICES: Those who believe any restriction on speech, beyond direct incitement to violence, is an unacceptable infringement on fundamental liberties. They are often dismissed as prioritizing abstract 'freedom' over concrete 'harm' or as implicitly endorsing harmful ideologies.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, public discourse would immediately become more hostile for marginalized groups, hate speech and harassment would proliferate unchecked, and the state would lose a primary tool for maintaining social order and protecting vulnerable populations, leading to significant social unrest and a breakdown of trust.
% FOUNDING_PROBLEM: Unfettered speech, particularly hate speech, harassment, and discriminatory rhetoric, caused significant and systemic harm to marginalized communities, undermining their dignity, equality, and ability to participate freely and safely in society.
% FOUNDING_PROBLEM_CORROBORATION: Marginalized communities, human rights organizations, and international legal bodies consistently attest to the ongoing harm caused by unrestricted speech, providing extensive evidence of its impact on dignity, mental health, and social participation. This corroboration comes from outside the direct beneficiaries of the constraint.
narrative_ontology:disappearance_verdict(speech_protection_boundary__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__harm_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__harm_limited_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(speech_protection_boundary__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__harm_limited_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because this reading actively curtails certain forms of expression, transferring the 'right' to speak them from individuals to the collective, often against the will of the speakers. Suppression is also high (0.75) as the constraint relies on active enforcement mechanisms (legal penalties, platform moderation) to prevent and punish speech deemed harmful. The theater ratio is moderate (0.25) because while there is genuine intent to prevent harm, the process of defining and adjudicating 'harm' can sometimes involve performative aspects or be influenced by political pressures. Resistance is high (0.80) due to ongoing challenges from free speech advocates and those whose speech is targeted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized groups, this constraint is a necessary protection that enables their participation and upholds their dignity. From the perspective of free speech absolutists, it is an extractive mechanism that suppresses legitimate expression and risks state overreach. The engine's per-seat classification will reflect these divergent experiences based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   State regulators, marginalized groups, and advocates for equality are beneficiaries, as the constraint aligns with their goals of social protection and equity. Speakers of harmful speech and free speech absolutists are victims, as their expressive freedoms are curtailed. The judicial system acts as an agenda-setter, interpreting and applying the evolving standards of harm. Public discourse is an observer, shaped by the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_harm_ambiguity,
    'Is ''significant harm to dignity, equality, and freedom from harassment'' objectively measurable and consistently applicable, or is it inherently subjective and prone to political interpretation?',
    'Longitudinal study of judicial precedents and regulatory enforcement actions across diverse contexts, assessing consistency and predictability of outcomes. Analysis of public perception surveys regarding what constitutes ''harm''.',
    'If subjective, the constraint''s effective suppression is higher and more arbitrary, increasing the risk of abuse and chilling effects on legitimate speech. If objective, the constraint functions more predictably as a coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_harm_ambiguity, conceptual, 'Ambiguity in the definition and application of ''harm'' in speech regulation.').

omega_variable(
    slippery_slope_risk,
    'Does the ''harm-limited'' reading inevitably lead to an expanding definition of ''harm'' and subsequent overreach, suppressing legitimate dissent or unpopular opinions?',
    'Comparative legal analysis of jurisdictions that have adopted similar harm-limited frameworks, tracking the evolution of speech restrictions over time and their impact on political discourse and minority viewpoints.',
    'If a ''slippery slope'' is empirically demonstrated, the constraint''s long-term extractiveness and suppression are higher than current measures suggest, potentially shifting its classification towards a Snare. If not, the constraint''s boundaries are more stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(slippery_slope_risk, empirical, 'Risk of expanding definitions of harm leading to over-suppression of speech.').

omega_variable(
    state_gatekeeper_abuse,
    'Does granting the state (or other powerful institutions) the authority to define and enforce ''harm'' create an unacceptable risk of abuse against political opponents or marginalized groups themselves?',
    'Case studies of historical and contemporary instances where ''harm'' provisions have been used to silence dissent or target specific communities. Analysis of institutional safeguards and accountability mechanisms.',
    'If abuse is prevalent and unchecked, the constraint''s effective extractiveness and suppression are significantly higher, and its coordination function is undermined, potentially reclassifying it as a Snare for targeted groups. If safeguards are effective, the risk is mitigated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_gatekeeper_abuse, empirical, 'Risk of state abuse of power in enforcing harm-limited speech.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__harm_limited_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t2000, speech_protection_boundary__harm_limited_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(spee_tr_t2005, speech_protection_boundary__harm_limited_reading, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(spee_tr_t2010, speech_protection_boundary__harm_limited_reading, theater_ratio, 2010, 0.23).
narrative_ontology:measurement(spee_tr_t2015, speech_protection_boundary__harm_limited_reading, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(spee_tr_t2020, speech_protection_boundary__harm_limited_reading, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(spee_tr_t2025, speech_protection_boundary__harm_limited_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(spee_be_t2000, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(spee_be_t2005, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(spee_be_t2010, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(spee_be_t2015, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement(spee_be_t2020, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(spee_be_t2025, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t2000, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(spee_su_t2005, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement(spee_su_t2010, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(spee_su_t2015, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2015, 0.73).
narrative_ontology:measurement(spee_su_t2020, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2020, 0.75).
narrative_ontology:measurement(spee_su_t2025, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__harm_limited_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
