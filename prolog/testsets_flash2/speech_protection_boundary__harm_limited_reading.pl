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
 *   constraint_id: speech_protection_boundary__harm_limited_reading
 *   human_readable: Speech Protection Boundary (Harm-Limited Reading)
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint represents the 'harm-limited' reading of speech
 *   protection, where the scope of protected speech is conditional on its
 *   absence of significant harm to dignity, equality, and freedom from
 *   harassment. This reading expands the categories of unprotected speech
 *   beyond traditional incitement to include hate speech, harassment, and
 *   potentially coded dog whistles. While intended to protect vulnerable
 *   groups, it grants significant gatekeeping power to the state, raising
 *   concerns about potential abuse and chilling effects on legitimate, albeit
 *   controversial, expression. This is one reading of the
 *   'speech_protection_boundary' kernel.
 *
 * KEY AGENTS:
 *   - vulnerable_groups: Primary beneficiary (organized/constrained) — protected from harmful speech.
 *   - state_regulators: Agenda-setter/beneficiary (institutional/analytical) — gains authority to define and enforce harm boundaries.
 *   - speakers_of_controversial_views: Payer (moderate/constrained) — bears costs of increased scrutiny and potential legal action.
 *   - political_dissidents: Payer (powerless/identity_locked) — particularly vulnerable to suppression of critical speech.
 *   - civil_liberties_advocates: Observer (organized/analytical) — monitors for overreach and defends expressive freedoms.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, 0.65).
domain_priors:suppression_score(speech_protection_boundary__harm_limited_reading, 0.7).
domain_priors:theater_ratio(speech_protection_boundary__harm_limited_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__harm_limited_reading, "Speech Protection Boundary (Harm-Limited Reading)").
narrative_ontology:topic_domain(speech_protection_boundary__harm_limited_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(speech_protection_boundary__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__harm_limited_reading, '97d0da71-a01f-4dec-abfd-78636b497556').
narrative_ontology:cs_kernel_codification('97d0da71-a01f-4dec-abfd-78636b497556', fixed_text).
narrative_ontology:cs_authority_grounding('97d0da71-a01f-4dec-abfd-78636b497556', lineage).
narrative_ontology:cs_interpretation_layer_present('97d0da71-a01f-4dec-abfd-78636b497556').
narrative_ontology:cs_reading_relation('97d0da71-a01f-4dec-abfd-78636b497556', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('97d0da71-a01f-4dec-abfd-78636b497556', speech_protection_boundary__balancing_reading, coexists_with).
narrative_ontology:cs_axiom('97d0da71-a01f-4dec-abfd-78636b497556', foundational, speech_must_not_cause_significant_dignity_harm).
narrative_ontology:cs_axiom_status(speech_must_not_cause_significant_dignity_harm, holdable).
narrative_ontology:cs_axiom_grounding('97d0da71-a01f-4dec-abfd-78636b497556', speech_must_not_cause_significant_dignity_harm, deontological).
narrative_ontology:cs_axiom('97d0da71-a01f-4dec-abfd-78636b497556', foundational, state_has_duty_to_protect_vulnerable_from_speech_harms).
narrative_ontology:cs_axiom_status(state_has_duty_to_protect_vulnerable_from_speech_harms, holdable).
narrative_ontology:cs_axiom_grounding('97d0da71-a01f-4dec-abfd-78636b497556', state_has_duty_to_protect_vulnerable_from_speech_harms, deontological).
narrative_ontology:cs_reference_frame('97d0da71-a01f-4dec-abfd-78636b497556', post_civil_rights_era_inclusive_discourse).
narrative_ontology:cs_drift_state('97d0da71-a01f-4dec-abfd-78636b497556', contemporary_social_media_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('97d0da71-a01f-4dec-abfd-78636b497556', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__harm_limited_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, vulnerable_groups).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, state_regulators).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, speakers_of_controversial_views).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, political_dissidents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from legal frameworks that protect their dignity, equality, and freedom from harassment, particularly from hate speech and discriminatory rhetoric. They advocate for robust enforcement of harm-limited speech regulations.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, vulnerable_groups, beneficiary,
    organized, generational, constrained, national).

% Are tasked with interpreting and enforcing speech regulations based on the harm-limited principle. They gain expanded authority to define and adjudicate 'harm' in speech, which can lead to mission creep and potential overreach.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, state_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Bear the cost of increased scrutiny and potential legal action for speech deemed harmful, even if not directly inciting violence. Their speech is chilled, and they face uncertainty regarding the boundaries of protected expression.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, speakers_of_controversial_views, payer,
    moderate, biographical, constrained, national).

% Are particularly vulnerable to harm-limited speech regulations, as their critiques of power structures can be reinterpreted as 'harmful' or 'harassing' by those in authority. Their ability to challenge the status quo is significantly curtailed.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, political_dissidents, payer,
    powerless, biographical, identity_locked, local).

% Monitor the application of harm-limited speech regulations, often challenging their scope and enforcement in court. They argue that such regulations, while well-intentioned, can be abused to suppress legitimate dissent and expression.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__harm_limited_reading, state_regulators).
narrative_ontology:fixing_cost_class(speech_protection_boundary__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate social interaction by establishing boundaries for speech that prevent significant harm to dignity, equality, and freedom from harassment, thereby fostering a more inclusive public discourse.
% TRANSFER_FUNCTION: Transfers a degree of expressive freedom from speakers (especially those with controversial or critical views) to vulnerable groups, who gain protection from harmful speech, and to state regulators, who gain authority to enforce these boundaries.
% ABSENT_VOICES: Those who believe in a near-absolute right to free speech, fearing that any harm-based limitation inevitably leads to censorship and state overreach, are often marginalized in the discourse that establishes these boundaries. Their arguments for robust protection of even offensive speech are often dismissed as insensitive.
% DISAPPEARANCE_RATIONALE: If this harm-limited reading of speech protection vanished, there would be a significant increase in hate speech, harassment, and discriminatory rhetoric, leading to a more hostile public sphere for vulnerable groups. Conversely, speakers of controversial views might feel less constrained, but the social costs would be substantial, forcing a re-evaluation of speech norms.
% FOUNDING_PROBLEM: The problem of speech causing demonstrable harm to individuals and groups, undermining their ability to participate equally in society, and creating environments of fear and intimidation.
% FOUNDING_PROBLEM_CORROBORATION: Vulnerable groups and human rights organizations consistently attest to the ongoing problem of harmful speech. While some speakers argue the problem is exaggerated to justify censorship, the lived experience of those targeted by hate speech provides strong corroboration from outside the benefiting parties.
narrative_ontology:disappearance_verdict(speech_protection_boundary__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__harm_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__harm_limited_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.65) is substantial because a broad range of speech, previously considered protected, now carries a cost or risk for speakers. Suppression (0.70) is high due to the active enforcement required to identify and penalize 'harmful' speech, and the chilling effect this creates. Theater ratio (0.20) is moderate; while there's genuine intent to protect, a portion of the enforcement activity becomes performative in demonstrating state commitment to these values, sometimes at the expense of clear, consistent application. Accessibility collapse (0.40) is moderate, as alternatives for expression exist but are significantly constrained by the risk of being deemed harmful. Resistance (0.75) is high, reflecting ongoing legal and political challenges from those who view these limitations as infringements on fundamental rights.
 *
 * PERSPECTIVAL GAP:
 *   Vulnerable groups and state regulators perceive this constraint as a necessary and beneficial coordination mechanism to ensure a safe and equitable public sphere. Speakers of controversial views and political dissidents, however, experience it as a highly extractive and suppressive mechanism that curtails their fundamental right to free expression. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable groups are beneficiaries (d near 0.0) as the constraint directly addresses harms they face. State regulators are also beneficiaries (d near 0.0) due to increased authority and the ability to shape public discourse. Speakers of controversial views and political dissidents are targets (d near 1.0) as their speech is directly curtailed and they bear the costs of compliance or challenge. Civil liberties advocates are observers (d near 0.5), analyzing the constraint's impact without direct benefit or cost.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling coordination as pure extraction by acknowledging the genuine problem of harmful speech and the coordination function of protecting vulnerable groups. However, the high extractiveness and suppression metrics, coupled with the risk of state overreach, indicate that the coordination function is intertwined with significant extraction from speakers, making it a Tangled Rope rather than a pure Rope. The mandate to protect dignity and equality is live, but the method of enforcement creates substantial costs for other parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_significant_harm,
    'How is ''significant harm to dignity, equality, and freedom from harassment'' precisely defined and consistently applied across different contexts and by different adjudicators?',
    'Development of clear, objective legal standards and precedents, coupled with empirical studies on the actual impact of various forms of speech on target groups.',
    'If definitions are vague or inconsistently applied, the constraint''s suppression and extractiveness will be higher due to chilling effects and arbitrary enforcement. Clearer definitions could reduce these, potentially shifting the classification towards a more functional Tangled Rope or even a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_significant_harm, empirical, 'Ambiguity in the definition and application of ''significant harm'' in speech regulation.').

omega_variable(
    state_gatekeeper_abuse_risk,
    'To what extent does the state''s expanded role as a gatekeeper of ''harmful'' speech lead to the suppression of legitimate political dissent or unpopular ideas, rather than solely protecting vulnerable groups?',
    'Longitudinal studies of enforcement patterns, analysis of speech cases involving political or social critique, and comparison with jurisdictions employing different speech protection models.',
    'If evidence shows significant suppression of dissent, the constraint''s classification would lean more strongly towards Snare, as the coordination function becomes a cover for extraction of expressive freedom. If abuse is minimal, it remains a Tangled Rope with a stronger coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_gatekeeper_abuse_risk, empirical, 'Risk of state overreach and suppression of dissent under harm-limited speech regulations.').

omega_variable(
    kernel_reading_divergence,
    'Is this harm-limited reading of speech protection structurally compatible with an absolutist or balancing reading within a single constitutional framework?',
    'Legal and philosophical analysis of the foundational axioms of each reading, and judicial decisions that explicitly reconcile or reject competing interpretations.',
    'If incompatible, the persistence of multiple readings indicates a fundamental conceptual tension in the constitutional order, leading to ongoing legal and political contestation. If compatible, it suggests a more unified, albeit complex, understanding of speech rights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'This constraint is one reading of the ''speech_protection_boundary'' kernel. Sibling readings (absolutist_reading, balancing_reading) offer alternative structural interpretations of speech rights, with different implications for the scope of protected and unprotected speech.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__harm_limited_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__harm_limited_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(spee_tr_t5, speech_protection_boundary__harm_limited_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(spee_tr_t10, speech_protection_boundary__harm_limited_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(spee_tr_t15, speech_protection_boundary__harm_limited_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(spee_tr_t20, speech_protection_boundary__harm_limited_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__harm_limited_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(spee_be_t5, speech_protection_boundary__harm_limited_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(spee_be_t10, speech_protection_boundary__harm_limited_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(spee_be_t15, speech_protection_boundary__harm_limited_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(spee_be_t20, speech_protection_boundary__harm_limited_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__harm_limited_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(spee_su_t5, speech_protection_boundary__harm_limited_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(spee_su_t10, speech_protection_boundary__harm_limited_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(spee_su_t15, speech_protection_boundary__harm_limited_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(spee_su_t20, speech_protection_boundary__harm_limited_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__harm_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'speech_protection_boundary' kernel. The 'harm_limited_reading' emphasizes protection from dignity and equality harms, leading to a narrower scope of protected speech compared to the 'absolutist_reading' and a more predefined set of unprotected speech than the 'balancing_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
