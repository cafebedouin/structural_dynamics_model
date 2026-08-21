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
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint represents the 'democratic participation' reading of free
 *   speech, where protection is strongest for political expression deemed
 *   necessary for self-governance. It establishes a hierarchy of speech, with
 *   political speech at the apex, and other forms of speech (e.g.,
 *   commercial, obscenity) receiving less stringent protection and being more
 *   susceptible to regulation. The constraint is claimed as a Rope because it
 *   genuinely coordinates democratic function, but its enforcement requires
 *   active judicial interpretation and occasional suppression of
 *   non-political speech.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, 0.15).
domain_priors:suppression_score(speech_protection_kernel__democratic_participation_reading, 0.2).
domain_priors:theater_ratio(speech_protection_kernel__democratic_participation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__democratic_participation_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__democratic_participation_reading, "Speech Protection for Democratic Participation").
narrative_ontology:topic_domain(speech_protection_kernel__democratic_participation_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__democratic_participation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__democratic_participation_reading, '199a0035-d329-457c-97d4-18ef1abc5624').
narrative_ontology:cs_kernel_codification('199a0035-d329-457c-97d4-18ef1abc5624', fixed_text).
narrative_ontology:cs_authority_grounding('199a0035-d329-457c-97d4-18ef1abc5624', lineage).
narrative_ontology:cs_interpretation_layer_present('199a0035-d329-457c-97d4-18ef1abc5624').
narrative_ontology:cs_reading_relation('199a0035-d329-457c-97d4-18ef1abc5624', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('199a0035-d329-457c-97d4-18ef1abc5624', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('199a0035-d329-457c-97d4-18ef1abc5624', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('199a0035-d329-457c-97d4-18ef1abc5624', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_axiom('199a0035-d329-457c-97d4-18ef1abc5624', foundational, speech_hierarchy_for_democracy).
narrative_ontology:cs_axiom_status(speech_hierarchy_for_democracy, holdable).
narrative_ontology:cs_axiom_grounding('199a0035-d329-457c-97d4-18ef1abc5624', speech_hierarchy_for_democracy, deontological).
narrative_ontology:cs_axiom('199a0035-d329-457c-97d4-18ef1abc5624', foundational, informed_electorate_is_paramount).
narrative_ontology:cs_axiom_status(informed_electorate_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('199a0035-d329-457c-97d4-18ef1abc5624', informed_electorate_is_paramount, instrumental).
narrative_ontology:cs_reference_frame('199a0035-d329-457c-97d4-18ef1abc5624', founding_era_republican_ideal).
narrative_ontology:cs_drift_state('199a0035-d329-457c-97d4-18ef1abc5624', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('199a0035-d329-457c-97d4-18ef1abc5624', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, citizens).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, political_candidates).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, advocacy_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, non_political_speakers).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, self_governance_principle).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, informed_electorate_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from robust protection for political speech, enabling them to participate in public discourse and hold elected officials accountable. Their ability to exit the political system is constrained, but their speech is highly protected.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, citizens, beneficiary,
    organized, generational, constrained, national).

% Benefit from the ability to freely express their views on public matters without undue government interference, crucial for campaigning and policy debate. Their mobility within the political system is high.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, political_candidates, beneficiary,
    powerful, biographical, mobile, national).

% Rely on strong speech protections to articulate their positions on policy issues, lobby government, and mobilize public support. Their ability to influence policy is directly tied to their freedom of expression.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, advocacy_groups, beneficiary,
    moderate, biographical, constrained, national).

% Responsible for interpreting and enforcing speech protections, particularly distinguishing political from non-political speech and determining permissible restrictions on the latter. They balance free speech with other societal interests.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, government_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Experience comparatively weaker protection for their speech (e.g., commercial speech, obscenity), which is more readily subject to regulation based on content or context. They bear the cost of this hierarchical distinction.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, non_political_speakers, payer,
    moderate, immediate, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public discourse by prioritizing speech essential for self-governance, ensuring a robust exchange of ideas necessary for an informed electorate and democratic decision-making.
% TRANSFER_FUNCTION: Transfers a higher degree of protection and immunity from regulation to political speech, while allowing greater regulatory burdens on other forms of expression, from the state to citizens and political actors.
% ABSENT_VOICES: Those advocating for an absolutist view of speech protection, or those who believe all speech, regardless of content, should receive equal protection, are implicitly excluded from this hierarchical framework.
% DISAPPEARANCE_RATIONALE: If this principle vanished, the foundation of democratic discourse would erode. Political speech could be regulated more easily, undermining elections, public debate, and the ability of citizens to hold power accountable. The entire political system would need to re-evaluate its operational principles.
% FOUNDING_PROBLEM: The problem of ensuring a free and robust exchange of ideas necessary for citizens to make informed decisions in a self-governing society, particularly protecting dissent and criticism of government.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and political scientists widely corroborate the ongoing necessity of this principle for democratic health, citing historical examples of its erosion leading to authoritarianism. Independent analyses of democratic backsliding often point to the suppression of political speech as a key indicator.
narrative_ontology:disappearance_verdict(speech_protection_kernel__democratic_participation_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__democratic_participation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__democratic_participation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_protection_kernel__democratic_participation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__democratic_participation_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.15) because the primary function is to enable, not to extract, though non-political speakers bear some cost of differential protection. Suppression is low (0.2) but present, as the state actively enforces distinctions between speech categories. Theater ratio is low (0.1) as the judicial system genuinely strives to uphold this principle, though some performative aspects exist in balancing competing interests. Accessibility collapse is moderate (0.7) because while political speech is highly protected, alternatives for unrestricted non-political speech are significantly collapsed. Resistance is low (0.1) as the principle is widely accepted as foundational to democracy, though specific applications may face contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of political actors, this is a pure Rope, enabling their function. From the perspective of non-political speakers, it introduces a subtle form of extraction by limiting their expressive freedom relative to political speech. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizens, political candidates, and advocacy groups are beneficiaries, as their core activities are enabled and protected by this reading. Government regulators act as agenda-setters, interpreting and applying the distinctions. Non-political speakers are payers, as their speech is more readily restricted. There are no direct 'victims' in the sense of active extraction, but rather differential treatment.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_political_speech,
    'What constitutes ''political expression necessary for self-governance'' in an increasingly complex and interconnected society?',
    'Ongoing judicial interpretation and legislative action, informed by evolving social norms and empirical studies of communication''s impact on democratic processes.',
    'A broader definition would extend high protection to more forms of speech, potentially reducing the ''payer'' burden on non-political speakers. A narrower definition would increase regulatory leeway for non-political speech.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_political_speech, conceptual, 'Ambiguity in defining the boundaries of ''political speech''.').

omega_variable(
    balancing_with_other_rights,
    'How should the strong protection for political speech be balanced against other fundamental rights, such as privacy, reputation, or the right to be free from harassment?',
    'Case-by-case adjudication by courts, developing a jurisprudence that articulates the precise contours of these competing rights.',
    'If other rights are given more weight, the effective protection for political speech might be subtly reduced, increasing the ''extractiveness'' for political speakers. If political speech remains paramount, other rights might be implicitly subordinated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_with_other_rights, preference, 'Tension between political speech protection and other rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__democratic_participation_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1900, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(spee_tr_t1950, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_kernel__democratic_participation_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_kernel__democratic_participation_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(spee_be_t1900, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1900, 0.1).
narrative_ontology:measurement(spee_be_t1950, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1950, 0.12).
narrative_ontology:measurement(spee_be_t2000, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(spee_be_t2024, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1900, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1900, 0.15).
narrative_ontology:measurement(spee_su_t1950, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1950, 0.18).
narrative_ontology:measurement(spee_su_t2000, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 2000, 0.19).
narrative_ontology:measurement(spee_su_t2024, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__democratic_participation_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
