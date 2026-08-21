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
 *   constraint_id: speech_protection_kernel__democratic_participation_reading
 *   human_readable: Speech Protection for Democratic Self-Governance
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint represents the 'democratic participation' reading of
 *   speech protection, which posits that the primary purpose of free speech
 *   is to facilitate self-governance. Consequently, political expression
 *   receives the highest level of constitutional protection, while other
 *   forms of speech (e.g., commercial, artistic) are more readily subject to
 *   regulation. The constraint is claimed as a 'rope' by its proponents,
 *   emphasizing its coordination function for democracy. However, the
 *   authored metrics reflect a substantial degree of extraction and
 *   suppression, particularly for non-political speech, indicating that its
 *   operation is more akin to a 'tangled_rope' or 'snare' from the
 *   perspective of those whose speech is less protected. The divergence
 *   between claimed type and operational metrics is intentional, allowing the
 *   engine to measure this gap.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, 0.65).
domain_priors:suppression_score(speech_protection_kernel__democratic_participation_reading, 0.55).
domain_priors:theater_ratio(speech_protection_kernel__democratic_participation_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__democratic_participation_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__democratic_participation_reading, "Speech Protection for Democratic Self-Governance").
narrative_ontology:topic_domain(speech_protection_kernel__democratic_participation_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__democratic_participation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__democratic_participation_reading, '7609a03f-8285-4230-a566-6f05ac262c9f').
narrative_ontology:cs_kernel_codification('7609a03f-8285-4230-a566-6f05ac262c9f', fixed_text).
narrative_ontology:cs_authority_grounding('7609a03f-8285-4230-a566-6f05ac262c9f', lineage).
narrative_ontology:cs_interpretation_layer_present('7609a03f-8285-4230-a566-6f05ac262c9f').
narrative_ontology:cs_reading_relation('7609a03f-8285-4230-a566-6f05ac262c9f', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('7609a03f-8285-4230-a566-6f05ac262c9f', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('7609a03f-8285-4230-a566-6f05ac262c9f', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('7609a03f-8285-4230-a566-6f05ac262c9f', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('7609a03f-8285-4230-a566-6f05ac262c9f', foundational, political_speech_is_paramount).
narrative_ontology:cs_axiom_status(political_speech_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('7609a03f-8285-4230-a566-6f05ac262c9f', political_speech_is_paramount, deontological).
narrative_ontology:cs_axiom('7609a03f-8285-4230-a566-6f05ac262c9f', foundational, self_governance_requires_informed_discourse).
narrative_ontology:cs_axiom_status(self_governance_requires_informed_discourse, holdable).
narrative_ontology:cs_axiom_grounding('7609a03f-8285-4230-a566-6f05ac262c9f', self_governance_requires_informed_discourse, instrumental).
narrative_ontology:cs_reference_frame('7609a03f-8285-4230-a566-6f05ac262c9f', meiklejohnian_democracy_theory).
narrative_ontology:cs_drift_state('7609a03f-8285-4230-a566-6f05ac262c9f', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7609a03f-8285-4230-a566-6f05ac262c9f', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, political_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, informed_citizenry).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, public_at_large).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, non_political_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, commercial_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, public_at_large).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and groups whose expression directly relates to public affairs, elections, or government policy. Their speech receives the highest level of constitutional protection, making it difficult for the state to restrict.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, political_speakers, beneficiary,
    powerful, biographical, mobile, national).

% The collective body of citizens who rely on a robust exchange of political ideas to make informed decisions and participate in self-governance. They benefit from the prioritization of political speech.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, informed_citizenry, beneficiary,
    organized, generational, mobile, national).

% The broader society that benefits from a stable democratic process and the free flow of political information. However, they may indirectly bear the cost of less protection for other forms of speech they value.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, public_at_large, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__democratic_participation_reading, public_at_large, payer).

% Individuals and groups whose expression is artistic, scientific, personal, or otherwise not directly political. Their speech receives a lower tier of protection and is more readily subject to government regulation or restriction.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, non_political_speakers, payer,
    moderate, biographical, constrained, national).

% Businesses and advertisers whose speech is primarily economic in nature. Commercial speech receives intermediate protection, meaning it can be regulated more easily than political speech, often leading to restrictions on content or advertising methods.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, commercial_speakers, payer,
    powerful, biographical, constrained, national).

% The primary interpreters and enforcers of constitutional speech protections. They establish the hierarchy of speech, define what constitutes 'political expression,' and adjudicate challenges to speech restrictions.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, courts_and_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Elected bodies that pass laws regulating speech. While constrained by judicial interpretation, they actively shape the boundaries of permissible expression, particularly for non-political and commercial speech.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, legislature, agenda_setter,
    institutional, generational, mobile, national).

% Organizations and legal professionals who monitor, litigate, and advocate for broader or more consistent speech protections. They analyze the impact of this reading on various forms of expression.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, free_speech_advocates, observer,
    organized, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate public discourse by prioritizing and protecting speech essential for democratic self-governance, thereby ensuring a robust exchange of political ideas necessary for an informed citizenry.
% TRANSFER_FUNCTION: This reading transfers greater legal protection and legitimacy to political speech, while implicitly allowing for more extensive regulation and less protection for non-political, commercial, or other forms of expression, effectively reallocating expressive capacity within society.
% ABSENT_VOICES: Artists, scientists, and individuals engaged in purely personal expression might argue for a more content-neutral approach to speech protection, asserting that their forms of expression are also vital for a flourishing society, even if not directly 'political.' They are often marginalized in debates focused solely on democratic participation.
% DISAPPEARANCE_RATIONALE: If this hierarchical framework for speech protection vanished, the legal landscape of public discourse would fundamentally reorganize. Either an absolutist standard would emerge, or a more restrictive regime based on other criteria (e.g., harm, dignity) would take precedence, altering how citizens engage with government and each other.
% FOUNDING_PROBLEM: The constraint was built to address the problem of ensuring a well-functioning democracy by safeguarding the free exchange of ideas necessary for citizens to make informed decisions, hold their government accountable, and participate effectively in self-governance, while allowing for reasonable regulation of other forms of expression.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, political scientists, and civil liberties organizations (acting as independent observers) corroborate the ongoing importance of robust political discourse for democratic health, supporting the claim that the founding problem remains live, even as its application evolves.
narrative_ontology:disappearance_verdict(speech_protection_kernel__democratic_participation_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__democratic_participation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__democratic_participation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(speech_protection_kernel__democratic_participation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__democratic_participation_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__democratic_participation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__democratic_participation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.65) is driven by the differential protection: while political speakers benefit, non-political and commercial speakers bear the cost of greater state control over their expression. Suppression (0.55) is moderate because the legal system actively enforces this hierarchy, restricting certain speech categories. The theater ratio is low (0.15) as the core function of protecting political discourse is genuinely pursued, though the scope of 'political' is often contested. The temporal measurements show a slight increase in extractiveness and suppression over time, reflecting the ongoing tension and judicial adjustments in balancing different speech interests.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of political speakers and the informed citizenry, this constraint functions as a vital 'rope' for democratic coordination. However, from the perspective of non-political or commercial speakers, the same structure operates as a 'tangled_rope' or 'snare,' extracting expressive freedom and suppressing alternatives for their forms of communication. The courts, as agenda-setters, navigate these competing interests, often reinforcing the hierarchy.
 *
 * DIRECTIONALITY LOGIC:
 *   Political speakers and the informed citizenry are clear beneficiaries, receiving amplified protection for their core interests. Non-political and commercial speakers are targets, experiencing greater state control and bearing the costs of differential protection. The courts and legislature, as agenda-setters, enforce this hierarchy. The public at large benefits from democratic stability but may indirectly pay through reduced expressive diversity.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_political_speech,
    'What constitutes ''political expression necessary for self-governance'' in an increasingly complex and interconnected society, especially in digital contexts?',
    'Judicial clarification through landmark cases, legislative definitions, or evolving societal consensus on the boundaries of political discourse.',
    'A narrower definition would increase extraction from speech deemed non-political; a broader definition would extend higher protection to more forms of expression, reducing extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_political_speech, conceptual, 'Ambiguity in defining the scope of highly protected political speech.').

omega_variable(
    balancing_non_political_speech,
    'At what point do restrictions on non-political or commercial speech become unduly extractive, undermining other societal values (e.g., artistic expression, economic innovation)?',
    'Empirical studies on the impact of speech regulations on specific sectors, comparative legal analysis across jurisdictions, and ongoing public and legal debate.',
    'If restrictions are found to be unduly burdensome, it could lead to calls for higher protection for these categories, shifting the balance of extraction. If deemed necessary, the current level of extraction would be justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_non_political_speech, empirical, 'The appropriate balance between protecting political speech and regulating other forms of expression.').

omega_variable(
    impact_on_marginalized_voices,
    'Does prioritizing political speech inadvertently marginalize or disproportionately impact the expressive capacity of certain minority or vulnerable groups whose primary forms of expression may not be overtly political?',
    'Sociological research on expressive practices of marginalized communities, legal challenges from these groups, and critical legal scholarship examining the practical effects of the hierarchy.',
    'If disproportionate impact is demonstrated, it could lead to pressure for re-evaluating the hierarchy or developing specific protections for marginalized voices, potentially reducing the effective extraction from these groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_marginalized_voices, empirical, 'Whether the speech hierarchy creates unintended marginalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__democratic_participation_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1950, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(spee_tr_t1970, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(spee_tr_t1990, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1990, 0.13).
narrative_ontology:measurement(spee_tr_t2010, speech_protection_kernel__democratic_participation_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_kernel__democratic_participation_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(spee_be_t1950, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(spee_be_t1970, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(spee_be_t1990, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1990, 0.63).
narrative_ontology:measurement(spee_be_t2010, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(spee_be_t2024, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1950, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1950, 0.45).
narrative_ontology:measurement(spee_su_t1970, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(spee_su_t1990, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1990, 0.53).
narrative_ontology:measurement(spee_su_t2010, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 2010, 0.54).
narrative_ontology:measurement(spee_su_t2024, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__democratic_participation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, hate_speech_regulation).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, commercial_speech_regulation).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, campaign_finance_regulation).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'speech_protection_kernel,' focusing on democratic participation. Its structural properties and metrics differ significantly from other readings (absolutist, harm-threshold, marketplace, dignity), which are modeled as separate constraints within the same family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
