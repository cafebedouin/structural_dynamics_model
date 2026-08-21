% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__democratic_participation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__democratic_participation_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: speech_protection_kernel__democratic_participation_reading
 *   human_readable: Speech Protection for Democratic Participation
 *   domain: Constitutional Law / Political Philosophy / Communication Rights
 *
 * SUMMARY:
 *   This constraint represents the 'democratic participation' reading of
 *   speech protection, which posits that the highest degree of constitutional
 *   protection should be afforded to political expression essential for
 *   self-governance. This reading establishes a hierarchy of speech, where
 *   political speech receives near-absolute protection, while other forms of
 *   speech (e.g., commercial, private) may be more readily regulated. The
 *   constraint is claimed as a 'rope' because it coordinates a vital function
 *   for democratic society, with net benefits for participants, despite the
 *   inherent limitations it places on government and the differential
 *   treatment of non-political speech.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, 0.22).
domain_priors:suppression_score(speech_protection_kernel__democratic_participation_reading, 0.45).
domain_priors:theater_ratio(speech_protection_kernel__democratic_participation_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__democratic_participation_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__democratic_participation_reading, "Speech Protection for Democratic Participation").
narrative_ontology:topic_domain(speech_protection_kernel__democratic_participation_reading, "Constitutional Law / Political Philosophy / Communication Rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__democratic_participation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__democratic_participation_reading, '1421aa75-7d36-40b0-8b27-e6fbcbe9e4d1').
narrative_ontology:cs_kernel_codification('1421aa75-7d36-40b0-8b27-e6fbcbe9e4d1', fixed_text).
narrative_ontology:cs_authority_grounding('1421aa75-7d36-40b0-8b27-e6fbcbe9e4d1', lineage).
narrative_ontology:cs_interpretation_layer_present('1421aa75-7d36-40b0-8b27-e6fbcbe9e4d1').
narrative_ontology:cs_reading_relation('1421aa75-7d36-40b0-8b27-e6fbcbe9e4d1', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1421aa75-7d36-40b0-8b27-e6fbcbe9e4d1', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('1421aa75-7d36-40b0-8b27-e6fbcbe9e4d1', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('1421aa75-7d36-40b0-8b27-e6fbcbe9e4d1', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('1421aa75-7d36-40b0-8b27-e6fbcbe9e4d1', foundational, political_speech_essential_for_democracy).
narrative_ontology:cs_axiom_status(political_speech_essential_for_democracy, holdable).
narrative_ontology:cs_axiom_grounding('1421aa75-7d36-40b0-8b27-e6fbcbe9e4d1', political_speech_essential_for_democracy, deontological).
narrative_ontology:cs_reference_frame('1421aa75-7d36-40b0-8b27-e6fbcbe9e4d1', founding_era_republicanism).
narrative_ontology:cs_drift_state('1421aa75-7d36-40b0-8b27-e6fbcbe9e4d1', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1421aa75-7d36-40b0-8b27-e6fbcbe9e4d1', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, citizens_engaging_in_political_discourse).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, political_candidates_and_parties).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, government_actors).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, those_harmed_by_political_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, non_political_speakers).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, self_governance_principle).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, informed_electorate_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from robust protection for their political speech, enabling them to participate in public debate without undue fear of government reprisal. Their ability to influence policy and hold leaders accountable is enhanced.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, citizens_engaging_in_political_discourse, beneficiary,
    moderate, biographical, mobile, national).

% Bear the cost of restricted ability to control or censor political discourse, even when they perceive it as harmful or disruptive. They must tolerate speech that challenges their authority or policies, adhering to judicial interpretations of protected expression.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, government_actors, payer,
    institutional, generational, constrained, national).

% Are tasked with interpreting and enforcing the boundaries of protected speech, particularly political expression. They adjudicate disputes, balancing speech rights against other state interests, and their rulings shape the constraint's practical application.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, courts_and_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Experience a relatively lower degree of protection for their speech compared to political expression. Their speech is more readily subject to restriction based on content or context, making them 'payers' in the sense of bearing a higher burden of potential censorship.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, non_political_speakers, payer,
    moderate, biographical, constrained, national).

% Benefit directly from the strong protection of political speech, allowing them to campaign, debate, and criticize without significant government interference. This enables their participation in the electoral process and public policy formation.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, political_candidates_and_parties, beneficiary,
    organized, immediate, mobile, national).

% May experience direct harm (e.g., defamation, incitement to violence, harassment) from political speech that is protected under this reading. Their ability to seek redress or prevent such speech is curtailed, as the constraint prioritizes the speaker's right over their protection from harm.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, those_harmed_by_political_speech, payer,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public discourse to ensure a robust and uninhibited exchange of ideas, particularly political ones, deemed essential for democratic decision-making, self-governance, and the accountability of public officials.
% TRANSFER_FUNCTION: Transfers the burden of tolerating potentially offensive, challenging, or even harmful political speech from the state and some individuals to the broader public, in exchange for an informed electorate and a vibrant democratic process. It also transfers power to shape public discourse to citizens and away from government censorship.
% ABSENT_VOICES: Those advocating for a more expansive view of protected speech beyond the political, or those prioritizing individual dignity and protection from harm over the unfettered expression of political views. Also, those who believe the definition of 'political' is too narrow or too broad.
% DISAPPEARANCE_RATIONALE: If this robust protection for political speech vanished, government censorship would likely increase, chilling public debate, undermining electoral integrity, and severely weakening the mechanisms of democratic accountability. The political landscape would fundamentally reorganize around state-controlled narratives.
% FOUNDING_PROBLEM: Preventing government tyranny and ensuring citizens possess the means to hold their leaders accountable, shape public policy, and participate meaningfully in a self-governing republic.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, civil liberties organizations, and historical analyses of democratic decline consistently attest to the ongoing necessity of robust political speech protection for a functioning democracy. Judicial opinions frequently reiterate this foundational rationale.
narrative_ontology:disappearance_verdict(speech_protection_kernel__democratic_participation_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__democratic_participation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__democratic_participation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(speech_protection_kernel__democratic_participation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__democratic_participation_reading, 0.22, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__democratic_participation_reading_tests).
:- end_tests(speech_protection_kernel__democratic_participation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.22) because the primary function is protection and enablement of political discourse, not extraction from its beneficiaries. However, it's not zero due to the differential treatment of non-political speech and the costs borne by those harmed by protected political speech. Suppression is moderate (0.45) as it requires active judicial enforcement to suppress government attempts at censorship and to manage the boundaries of protected speech. Theater ratio is low (0.15) because the protection afforded to political speech is generally real and functionally effective. Accessibility collapse is low (0.30) for political speakers, but higher for government actors seeking to regulate speech and for non-political speakers whose alternatives for unfettered expression are more constrained. Resistance is moderate (0.35) from government actors and those advocating for broader restrictions or greater protection from speech-related harms.
 *
 * PERSPECTIVAL GAP:
 *   Political speakers and democratic institutions perceive this constraint as a fundamental safeguard, enabling their participation and function. Government actors, however, experience it as a significant limitation on their regulatory power. Those harmed by political speech, or engaged in non-political expression, may perceive it as a source of unmitigated harm or unequal treatment, highlighting the inherent trade-offs and the constraint's differential impact.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizens engaging in political discourse and political candidates/parties are clear beneficiaries, as the constraint directly enables their core activities (low directionality). Government actors are payers, as their power to regulate speech is curtailed (high directionality). Courts and the judiciary act as agenda-setters, interpreting and enforcing the constraint. Non-political speakers and those harmed by political speech are also payers, as their interests are subordinated to the protection of political expression (high directionality).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_political_speech,
    'How is ''political speech'' precisely defined in practice, and does this definition adequately capture all expression necessary for self-governance?',
    'Analysis of judicial rulings and legislative definitions over time, particularly in novel contexts like digital communication and social media.',
    'A narrow or inconsistent definition could lead to under-protection of vital discourse, increasing effective extraction from citizens and shifting the constraint towards a Tangled Rope or Snare for certain forms of expression. A broad definition might dilute the special protection, increasing perceived harm.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_political_speech, conceptual, 'Ambiguity in the scope and boundaries of ''political speech'' protected by the constraint.').

omega_variable(
    balancing_with_other_rights,
    'How effectively does this reading balance the protection of political speech against other fundamental rights, such as privacy, reputation, and protection from harassment or incitement?',
    'Empirical study of legal outcomes in cases where political speech conflicts with other rights, and comparative analysis with jurisdictions employing different balancing tests.',
    'If the balance consistently favors political speech at the expense of severe harm to other rights, the constraint''s effective extraction from victims of speech-related harm would be higher, potentially pushing it towards a Snare for those specific seats. If the balance is perceived as arbitrary, it could erode legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_with_other_rights, empirical, 'The inherent tension and balancing act between political speech protection and other fundamental rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__democratic_participation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__democratic_participation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__democratic_participation_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__democratic_participation_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(spee_tr_t30, speech_protection_kernel__democratic_participation_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__democratic_participation_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(spee_tr_t50, speech_protection_kernel__democratic_participation_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 10, 0.19).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(spee_be_t30, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 30, 0.21).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 40, 0.22).
narrative_ontology:measurement(spee_be_t50, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 50, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 10, 0.41).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(spee_su_t30, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 30, 0.44).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(spee_su_t50, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 50, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__democratic_participation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, electoral_process_integrity).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, freedom_of_assembly_constraint).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, press_freedom_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'speech_protection_kernel', focusing on democratic participation. Other readings (absolutist, harm_threshold, marketplace, dignity) are distinct constraints with different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
