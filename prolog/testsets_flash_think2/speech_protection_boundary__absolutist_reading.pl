% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__absolutist_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: speech_protection_boundary__absolutist_reading
 *   human_readable: Near-Absolute Speech Protection (Absolutist Reading)
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint represents the absolutist reading of free speech
 *   protection in the United States, where the exception for harm is narrowly
 *   limited to direct incitement to imminent lawless action (the Brandenburg
 *   standard). Proponents view this as a fundamental, almost natural, limit
 *   on state power to regulate speech, ensuring maximum expressive freedom.
 *   However, this reading imposes significant externalities on minoritized
 *   communities who bear the brunt of speech-related harms that do not meet
 *   the high Brandenburg threshold. The claimed type is 'mountain' from the
 *   perspective of its proponents, but the metrics reflect its extractive
 *   impact on vulnerable groups.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, 0.65).
domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, 0.75).
domain_priors:theater_ratio(speech_protection_boundary__absolutist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__absolutist_reading, mountain).
narrative_ontology:human_readable(speech_protection_boundary__absolutist_reading, "Near-Absolute Speech Protection (Absolutist Reading)").
narrative_ontology:topic_domain(speech_protection_boundary__absolutist_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(speech_protection_boundary__absolutist_reading).
domain_priors:emerges_naturally(speech_protection_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__absolutist_reading, '3392418a-9035-4cad-b486-b89715100c02').
narrative_ontology:cs_kernel_codification('3392418a-9035-4cad-b486-b89715100c02', fixed_text).
narrative_ontology:cs_authority_grounding('3392418a-9035-4cad-b486-b89715100c02', lineage).
narrative_ontology:cs_interpretation_layer_present('3392418a-9035-4cad-b486-b89715100c02').
narrative_ontology:cs_reading_relation('3392418a-9035-4cad-b486-b89715100c02', speech_protection_boundary__harm_limited_reading, forecloses).
narrative_ontology:cs_reading_relation('3392418a-9035-4cad-b486-b89715100c02', speech_protection_boundary__balancing_reading, forecloses).
narrative_ontology:cs_axiom('3392418a-9035-4cad-b486-b89715100c02', foundational, speech_is_free_unless_incitement).
narrative_ontology:cs_axiom_status(speech_is_free_unless_incitement, holdable).
narrative_ontology:cs_axiom_grounding('3392418a-9035-4cad-b486-b89715100c02', speech_is_free_unless_incitement, deontological).
narrative_ontology:cs_axiom('3392418a-9035-4cad-b486-b89715100c02', foundational, state_cannot_regulate_content).
narrative_ontology:cs_axiom_status(state_cannot_regulate_content, holdable).
narrative_ontology:cs_axiom_grounding('3392418a-9035-4cad-b486-b89715100c02', state_cannot_regulate_content, deontological).
narrative_ontology:cs_reference_frame('3392418a-9035-4cad-b486-b89715100c02', first_amendment_original_intent).
narrative_ontology:cs_drift_state('3392418a-9035-4cad-b486-b89715100c02', contemporary_digital_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('3392418a-9035-4cad-b486-b89715100c02', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(speech_protection_boundary__absolutist_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, speakers).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, civil_liberties_advocates).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, minoritized_communities).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, victims_of_hate_speech).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, public_safety_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, legislature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enjoy broad protection for their expression, even if it is offensive or controversial, as long as it does not directly incite imminent lawless action. They benefit from minimal state interference.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, speakers, beneficiary,
    powerful, biographical, arbitrage, national).

% Champion the expansive interpretation of free speech, viewing it as a bulwark against tyranny and a necessary condition for a robust democracy. They actively defend speakers against attempts at regulation.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, civil_liberties_advocates, beneficiary,
    organized, generational, analytical, national).

% Bear the aggregate harm and dignitary costs of hate speech, harassment, and incitement that falls short of the Brandenburg standard. Their concerns are often framed as secondary to the protection of speech itself.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, minoritized_communities, payer,
    powerless, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__absolutist_reading, minoritized_communities, excluded).

% Directly experience the psychological, social, and sometimes physical harms resulting from speech that is protected under this standard. They have limited legal recourse for such harms.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, victims_of_hate_speech, payer,
    powerless, immediate, trapped, local).

% Seek to balance free speech with the need for public order and safety, arguing for broader exceptions for speech that poses a clear and present danger, even if not strictly 'imminent lawless action'.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, public_safety_advocates, payer,
    organized, biographical, constrained, national).

% Interpret and enforce the First Amendment, applying the Brandenburg standard to determine the constitutionality of speech regulations. They are the primary arbiters of the speech protection boundary.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Is constrained in its ability to pass laws regulating speech, as any such legislation must survive strict judicial scrutiny under the Brandenburg standard. Attempts to address harms through legislation are often struck down.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, legislature, payer,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a high, clear bar for government intervention in speech, aiming to prevent arbitrary censorship and foster a robust public discourse by maximizing the scope of protected expression.
% TRANSFER_FUNCTION: Transfers the burden of potential social and individual harm from speakers to those affected by speech, and the cost of managing societal friction from the state to individuals and communities.
% ABSENT_VOICES: Minoritized communities and victims of hate speech are often structurally excluded from the framing of free speech debates, as their experiences of harm are frequently dismissed as mere 'offense' or 'externalities' that do not justify speech regulation.
% DISAPPEARANCE_RATIONALE: If this standard vanished, the legal landscape for speech would become highly unstable. Either government censorship would expand dramatically, or a chaotic free-for-all with no clear limits would emerge, leading to a complete reorganization of public discourse and legal precedent.
% FOUNDING_PROBLEM: Preventing government overreach and censorship, ensuring a vibrant marketplace of ideas, and protecting political dissent from suppression.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations and legal scholars frequently cite historical examples of government attempts to suppress dissent and argue that the threat of censorship remains live, requiring robust speech protections. This is corroborated by ongoing debates about government surveillance and information control.
narrative_ontology:disappearance_verdict(speech_protection_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(speech_protection_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__absolutist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, ExtMetricName, E),
    domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(speech_protection_boundary__absolutist_reading),
    narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(speech_protection_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.65) is substantial because the broad protection for speakers comes at the cost of unaddressed harms to vulnerable groups. Suppression (0.75) is high for those seeking to regulate speech for broader social harms, as the legal standard severely limits such efforts. The theater ratio (0.15) is low, indicating that the standard is genuinely applied, not merely performative. Accessibility collapse (0.8) is high for alternative regulatory approaches. Resistance (0.55) is moderate, reflecting ongoing advocacy and legal challenges from groups seeking to expand harm exceptions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of speakers and civil liberties advocates, this constraint is a 'mountain' or 'rope' – a fundamental protection of liberty. From the perspective of minoritized communities and victims of hate speech, it operates as a 'snare' or 'tangled_rope', extracting significant costs in the form of unaddressed harms while providing little benefit. The engine's computation will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers and civil liberties advocates are clear beneficiaries, experiencing low directionality as the constraint subsidizes their expressive freedom. Minimized communities, victims of hate speech, and public safety advocates are targets, bearing the costs of unaddressed harms and regulatory limitations, leading to high directionality. Courts act as agenda-setters, enforcing the standard, while the legislature is a payer, constrained in its ability to regulate. The temporal measurements show a slight increase in extractiveness and suppression over time, reflecting growing awareness and contestation of the standard's impact.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''absolutist_reading'' of the speech protection boundary, or does its practical application implicitly incorporate elements of balancing or harm limitation?',
    'Detailed case law analysis of judicial decisions, particularly in novel contexts (e.g., online speech), to identify implicit balancing tests or unacknowledged harm considerations.',
    'If implicit balancing/harm limitation is found, the constraint''s true nature might be closer to a ''tangled_rope'' or ''balancing_reading'', indicating a divergence from its claimed absolutist stance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Clarifying the true interpretive framework of speech protection.').

omega_variable(
    aggregate_harm_quantification,
    'How can the aggregate, systemic harms to minoritized communities from protected speech be quantified and weighed against the benefits of broad speech protection?',
    'Interdisciplinary research combining social science, public health data, and legal analysis to develop robust metrics for dignitary, psychological, and social harms from hate speech and other forms of protected expression.',
    'If quantifiable, significant aggregate harms are demonstrated, the ''absolutist_reading''s'' extractiveness would be more clearly justified, potentially shifting its classification for affected groups further towards ''snare'' or ''tangled_rope'' and strengthening arguments for alternative readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_harm_quantification, empirical, 'Measuring the unacknowledged costs of broad speech protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__absolutist_reading, 1969, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1969, speech_protection_boundary__absolutist_reading, theater_ratio, 1969, 0.1).
narrative_ontology:measurement(spee_tr_t1980, speech_protection_boundary__absolutist_reading, theater_ratio, 1980, 0.11).
narrative_ontology:measurement(spee_tr_t1990, speech_protection_boundary__absolutist_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_boundary__absolutist_reading, theater_ratio, 2000, 0.13).
narrative_ontology:measurement(spee_tr_t2010, speech_protection_boundary__absolutist_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_boundary__absolutist_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(spee_be_t1969, speech_protection_boundary__absolutist_reading, base_extractiveness, 1969, 0.55).
narrative_ontology:measurement(spee_be_t1980, speech_protection_boundary__absolutist_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement(spee_be_t1990, speech_protection_boundary__absolutist_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(spee_be_t2000, speech_protection_boundary__absolutist_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(spee_be_t2010, speech_protection_boundary__absolutist_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(spee_be_t2024, speech_protection_boundary__absolutist_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1969, speech_protection_boundary__absolutist_reading, suppression_requirement, 1969, 0.65).
narrative_ontology:measurement(spee_su_t1980, speech_protection_boundary__absolutist_reading, suppression_requirement, 1980, 0.68).
narrative_ontology:measurement(spee_su_t1990, speech_protection_boundary__absolutist_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(spee_su_t2000, speech_protection_boundary__absolutist_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(spee_su_t2010, speech_protection_boundary__absolutist_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(spee_su_t2024, speech_protection_boundary__absolutist_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__absolutist_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
