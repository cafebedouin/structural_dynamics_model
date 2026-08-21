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
 *   constraint_id: speech_protection_boundary__harm_limited_reading
 *   human_readable: Speech Protection Boundary: Harm-Limited Reading
 *   domain: Constitutional Law / Political Philosophy / Speech Regulation
 *
 * SUMMARY:
 *   This constraint represents the 'harm-limited' reading of the fundamental
 *   kernel of speech protection, where the scope of protected speech is
 *   conditional on the absence of significant harm to dignity, equality, and
 *   freedom from harassment. This reading emerged and gained prominence in
 *   response to historical and contemporary abuses of speech, particularly
 *   hate speech and incitement to discrimination. It contrasts sharply with
 *   absolutist views and offers a specific framework for balancing speech
 *   with other fundamental rights. The state becomes an active gatekeeper,
 *   with attendant risks of abuse.
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
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__harm_limited_reading, "Speech Protection Boundary: Harm-Limited Reading").
narrative_ontology:topic_domain(speech_protection_boundary__harm_limited_reading, "Constitutional Law / Political Philosophy / Speech Regulation").

domain_priors:requires_active_enforcement(speech_protection_boundary__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__harm_limited_reading, '8d0eaaa0-ab12-43ef-a19c-e9d8680b7561').
narrative_ontology:cs_kernel_codification('8d0eaaa0-ab12-43ef-a19c-e9d8680b7561', formalized).
narrative_ontology:cs_authority_grounding('8d0eaaa0-ab12-43ef-a19c-e9d8680b7561', lineage).
narrative_ontology:cs_interpretation_layer_present('8d0eaaa0-ab12-43ef-a19c-e9d8680b7561').
narrative_ontology:cs_reading_relation('8d0eaaa0-ab12-43ef-a19c-e9d8680b7561', speech_protection_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('8d0eaaa0-ab12-43ef-a19c-e9d8680b7561', speech_protection_boundary__balancing_reading, coexists_with).
narrative_ontology:cs_axiom('8d0eaaa0-ab12-43ef-a19c-e9d8680b7561', foundational, speech_is_not_absolute).
narrative_ontology:cs_axiom_status(speech_is_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('8d0eaaa0-ab12-43ef-a19c-e9d8680b7561', speech_is_not_absolute, deontological).
narrative_ontology:cs_axiom('8d0eaaa0-ab12-43ef-a19c-e9d8680b7561', foundational, dignity_equality_are_preconditions_for_free_speech).
narrative_ontology:cs_axiom_status(dignity_equality_are_preconditions_for_free_speech, holdable).
narrative_ontology:cs_axiom_grounding('8d0eaaa0-ab12-43ef-a19c-e9d8680b7561', dignity_equality_are_preconditions_for_free_speech, deontological).
narrative_ontology:cs_reference_frame('8d0eaaa0-ab12-43ef-a19c-e9d8680b7561', post_wwii_human_rights_framework).
narrative_ontology:cs_drift_state('8d0eaaa0-ab12-43ef-a19c-e9d8680b7561', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8d0eaaa0-ab12-43ef-a19c-e9d8680b7561', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__harm_limited_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, vulnerable_groups).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, state_regulators).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, speakers_of_harmful_speech).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, free_speech_absolutists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, public_discourse).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, public_discourse).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for defining what constitutes 'significant harm' to dignity, equality, and freedom from harassment, and for enforcing these limits through legal and administrative means. They benefit from the expanded scope of their authority.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, state_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from legal protections against speech that causes significant harm to their dignity, equality, and freedom from harassment. They advocate for robust enforcement of these limits to create a more inclusive public sphere.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, vulnerable_groups, beneficiary,
    organized, generational, constrained, national).

% Bear the direct costs of this constraint, as their speech is restricted, censored, or subject to legal penalties if deemed to cause significant harm. Their ability to express certain views is curtailed.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, speakers_of_harmful_speech, payer,
    moderate, immediate, constrained, national).

% Oppose any content-based restrictions on speech, viewing them as a dangerous precedent that undermines fundamental liberties. They bear the cost of a narrowed scope of protected speech and actively resist its expansion.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, free_speech_absolutists, payer,
    organized, generational, constrained, national).

% Adjudicate challenges to speech regulations, interpreting the boundaries of 'significant harm' and balancing competing rights. Their rulings shape the practical application and evolution of this constraint.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Potentially benefits from a less hostile and more inclusive environment, fostering broader participation. However, it also bears the cost of potentially reduced expressive range and increased self-censorship.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, public_discourse, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__harm_limited_reading, public_discourse, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__harm_limited_reading, vulnerable_groups).
narrative_ontology:fixing_cost_class(speech_protection_boundary__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a social and legal environment where the exercise of speech does not systematically undermine the dignity, equality, and freedom from harassment of vulnerable groups, thereby fostering a more inclusive and equitable public sphere.
% TRANSFER_FUNCTION: Transfers the burden of potential harm from vulnerable groups to speakers whose expression is deemed to cause such harm, by restricting the latter's speech and empowering state actors to enforce these limits.
% ABSENT_VOICES: Those who adhere to a near-absolutist view of free speech, arguing that any content-based restriction, even for harm, inevitably leads to censorship and thought control, and that the best response to harmful speech is more speech. They are often marginalized in the discourse that champions this reading.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the legal landscape for speech would revert to a more permissive standard, likely leading to an increase in hate speech and harassment, and a significant shift in the balance of power between speakers and vulnerable groups, forcing society to reorganize its approach to managing speech harms.
% FOUNDING_PROBLEM: The historical and ongoing use of speech to systematically marginalize, dehumanize, and incite violence against vulnerable groups, undermining their dignity, equality, and ability to participate freely in society, particularly in the wake of atrocities like the Holocaust.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, civil rights advocates, and international legal bodies consistently document the harms caused by hate speech and harassment, corroborating the ongoing nature of the problem and the need for protective measures. This is attested by reports from the UN, Amnesty International, and national human rights commissions.
narrative_ontology:disappearance_verdict(speech_protection_boundary__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__harm_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__harm_limited_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.65) is substantial because this reading imposes significant costs on speakers whose expression is deemed harmful, effectively extracting their right to speak freely in certain contexts. Suppression (0.75) is high, as the state actively enforces these limits through legal mechanisms, requiring continuous monitoring and adjudication. Theater ratio (0.25) is moderate, reflecting that while there is genuine intent to prevent harm, the application of 'harm' can sometimes be performative or politically motivated. The increasing trend in extractiveness and suppression reflects the growing societal recognition of speech harms and the expanding legal efforts to address them, particularly in the digital age.
 *
 * PERSPECTIVAL GAP:
 *   The 'harm-limited' reading is experienced as a necessary protection by vulnerable groups and state regulators, who see it as a coordination mechanism to ensure a safe and equitable public sphere. Conversely, speakers whose speech is restricted and free speech absolutists experience it as a highly extractive and suppressive mechanism that curtails fundamental liberties. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable groups and state regulators are beneficiaries, as the constraint directly serves their interests in protection and order, respectively. Speakers of harmful speech and free speech absolutists are targets, as they bear the direct costs of restricted expression and the philosophical challenge to their principles. Courts act as agenda-setters, interpreting and applying the constraint's boundaries.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as the 'founding problem' of speech-related harm remains live and contested. The classification as a Tangled Rope acknowledges both its genuine coordination function (protecting vulnerable groups) and its asymmetric extraction (from speakers whose speech is limited), preventing it from being mislabeled as pure extraction or pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_significant_harm,
    'How is ''significant harm to dignity, equality, and freedom from harassment'' consistently and objectively defined and measured across diverse contexts and evolving social norms?',
    'Development of clear, judicially consistent, and empirically grounded criteria for harm, potentially through legislative guidance or a robust body of case law that minimizes subjective interpretation.',
    'If harm definitions remain ambiguous or inconsistently applied, the constraint''s suppression can become arbitrary and more extractive, increasing the risk of abuse of power. If definitions are clear, its coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_significant_harm, conceptual, 'Ambiguity in defining the threshold and nature of ''significant harm''.').

omega_variable(
    state_gatekeeper_abuse_risk,
    'Does empowering the state as a gatekeeper of speech, even for harm prevention, inevitably lead to politically motivated censorship or suppression of dissenting views?',
    'Longitudinal empirical studies of jurisdictions with harm-limited speech regimes, tracking instances of politically motivated suppression, chilling effects on legitimate speech, and judicial oversight effectiveness.',
    'If the risk of abuse is high and frequently realized, the constraint''s effective suppression and extractiveness are higher than measured, and its claimed coordination function is undermined by its use as a tool of power. If robust safeguards prevent abuse, the constraint functions closer to its stated purpose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_gatekeeper_abuse_risk, empirical, 'Risk of state power being abused to suppress legitimate, non-harmful speech.').

omega_variable(
    effectiveness_of_suppression,
    'Does suppressing harmful speech effectively reduce the underlying social harms (e.g., discrimination, violence), or does it merely drive such expression underground, potentially making it harder to counter?',
    'Sociological and psychological research on the long-term impacts of speech regulation on social attitudes, hate group formation, and the prevalence of discriminatory acts, comparing outcomes in different regulatory environments.',
    'If suppression is found to be ineffective or counterproductive in reducing actual harm, the constraint''s coordination function is weakened, and its extractiveness (from speakers) becomes less justifiable. If effective, its legitimacy is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(effectiveness_of_suppression, empirical, 'Whether speech suppression genuinely mitigates social harms or merely displaces them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__harm_limited_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1948, speech_protection_boundary__harm_limited_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(spee_tr_t1968, speech_protection_boundary__harm_limited_reading, theater_ratio, 1968, 0.18).
narrative_ontology:measurement(spee_tr_t1988, speech_protection_boundary__harm_limited_reading, theater_ratio, 1988, 0.2).
narrative_ontology:measurement(spee_tr_t2008, speech_protection_boundary__harm_limited_reading, theater_ratio, 2008, 0.23).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_boundary__harm_limited_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(spee_be_t1948, speech_protection_boundary__harm_limited_reading, base_extractiveness, 1948, 0.4).
narrative_ontology:measurement(spee_be_t1968, speech_protection_boundary__harm_limited_reading, base_extractiveness, 1968, 0.48).
narrative_ontology:measurement(spee_be_t1988, speech_protection_boundary__harm_limited_reading, base_extractiveness, 1988, 0.55).
narrative_ontology:measurement(spee_be_t2008, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2008, 0.62).
narrative_ontology:measurement(spee_be_t2024, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1948, speech_protection_boundary__harm_limited_reading, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(spee_su_t1968, speech_protection_boundary__harm_limited_reading, suppression_requirement, 1968, 0.58).
narrative_ontology:measurement(spee_su_t1988, speech_protection_boundary__harm_limited_reading, suppression_requirement, 1988, 0.65).
narrative_ontology:measurement(spee_su_t2008, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2008, 0.72).
narrative_ontology:measurement(spee_su_t2024, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__harm_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, hate_speech_legislation).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, online_content_moderation).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, freedom_of_assembly_limits).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
