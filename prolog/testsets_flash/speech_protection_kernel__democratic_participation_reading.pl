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
 *   This constraint represents a specific reading of the 'speech protection'
 *   kernel, where the highest degree of protection is afforded to political
 *   expression deemed necessary for a functioning democracy and
 *   self-governance. Other forms of speech (e.g., commercial, artistic,
 *   private) receive lesser protection and are more susceptible to
 *   regulation. This reading establishes a hierarchy within protected speech,
 *   with political discourse at the apex.
 *
 * KEY AGENTS:
 *   - citizens: Primary beneficiaries (moderate/constrained) – benefit from robust political discourse.
 *   - political_candidates: Beneficiaries (powerful/mobile) – rely on broad protection for campaign speech.
 *   - advocacy_groups: Beneficiaries (organized/mobile) – benefit from ability to engage in political advocacy.
 *   - judicial_system: Agenda-setter (institutional/analytical) – interprets and enforces the scope of speech protection.
 *   - legislature: Agenda-setter (institutional/mobile) – passes laws that may regulate speech, subject to judicial review.
 *   - general_public: Beneficiary (organized/constrained) – benefits from an informed public sphere, but may bear costs of harmful non-political speech.
 *   - victims_of_non_political_harmful_speech: Payer (powerless/trapped) – bear the costs of speech not deemed 'political' enough for high protection, but still causing harm.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, 0.25).
domain_priors:suppression_score(speech_protection_kernel__democratic_participation_reading, 0.15).
domain_priors:theater_ratio(speech_protection_kernel__democratic_participation_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__democratic_participation_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__democratic_participation_reading, "Speech Protection for Democratic Participation").
narrative_ontology:topic_domain(speech_protection_kernel__democratic_participation_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__democratic_participation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__democratic_participation_reading, '0ed65edf-5955-40fd-b00c-d8b6d4d47c17').
narrative_ontology:cs_kernel_codification('0ed65edf-5955-40fd-b00c-d8b6d4d47c17', fixed_text).
narrative_ontology:cs_authority_grounding('0ed65edf-5955-40fd-b00c-d8b6d4d47c17', lineage).
narrative_ontology:cs_interpretation_layer_present('0ed65edf-5955-40fd-b00c-d8b6d4d47c17').
narrative_ontology:cs_reading_relation('0ed65edf-5955-40fd-b00c-d8b6d4d47c17', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ed65edf-5955-40fd-b00c-d8b6d4d47c17', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('0ed65edf-5955-40fd-b00c-d8b6d4d47c17', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ed65edf-5955-40fd-b00c-d8b6d4d47c17', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_axiom('0ed65edf-5955-40fd-b00c-d8b6d4d47c17', foundational, political_speech_is_core_to_self_governance).
narrative_ontology:cs_axiom_status(political_speech_is_core_to_self_governance, holdable).
narrative_ontology:cs_axiom_grounding('0ed65edf-5955-40fd-b00c-d8b6d4d47c17', political_speech_is_core_to_self_governance, deontological).
narrative_ontology:cs_axiom('0ed65edf-5955-40fd-b00c-d8b6d4d47c17', secondary, hierarchy_of_speech_values_is_necessary).
narrative_ontology:cs_axiom_status(hierarchy_of_speech_values_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('0ed65edf-5955-40fd-b00c-d8b6d4d47c17', hierarchy_of_speech_values_is_necessary, conventional).
narrative_ontology:cs_reference_frame('0ed65edf-5955-40fd-b00c-d8b6d4d47c17', uninhibited_robust_open_debate).
narrative_ontology:cs_drift_state('0ed65edf-5955-40fd-b00c-d8b6d4d47c17', contemporary_disinformation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0ed65edf-5955-40fd-b00c-d8b6d4d47c17', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, citizens).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, political_candidates).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, advocacy_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, general_public).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, victims_of_non_political_harmful_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, general_public).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, self_governance_principle).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, informed_electorate_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a robust public sphere where political ideas can be freely debated, enabling informed participation in self-governance. Their ability to exit is constrained by the nature of citizenship itself.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, citizens, beneficiary,
    moderate, generational, constrained, national).

% Benefit from broad protection for their campaign speech and political advocacy, allowing them to reach voters and articulate platforms without undue restriction. They have mobility within the political system.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, political_candidates, beneficiary,
    powerful, biographical, mobile, national).

% Benefit from the ability to engage in political discourse, protest, and lobbying to influence public policy and elections. They can shift their advocacy strategies and targets.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, advocacy_groups, beneficiary,
    organized, biographical, mobile, national).

% Interprets and enforces the scope of speech protection, particularly in cases involving political expression. Its role is to balance free speech with other constitutional values. Its exit is analytical, through re-interpretation.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, judicial_system, agenda_setter,
    institutional, generational, analytical, national).

% Passes laws that may regulate speech, but these laws are subject to judicial review under the framework of speech protection. It can revise laws, offering a form of mobility.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, legislature, agenda_setter,
    institutional, generational, mobile, national).

% Bear the costs of speech that causes harm (e.g., defamation, harassment, incitement to non-political violence) but is not deemed 'political expression necessary for self-governance,' and thus receives less protection. Their ability to exit the harm is limited.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, victims_of_non_political_harmful_speech, payer,
    powerless, immediate, trapped, local).

% Benefits from an informed public sphere and the ability to participate in democratic processes. However, they may also bear the diffuse costs of speech that, while not 'political,' is harmful or offensive, and receives some protection due to the difficulty of drawing clear lines.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, general_public, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__democratic_participation_reading, general_public, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__democratic_participation_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__democratic_participation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the public sphere to prioritize and protect speech essential for democratic self-governance, ensuring citizens can make informed decisions and hold leaders accountable.
% TRANSFER_FUNCTION: Transfers a high degree of protection to political speech, effectively subordinating other interests (e.g., protection from non-political harm, commercial regulation) when they conflict with the needs of democratic discourse.
% ABSENT_VOICES: Those advocating for stronger protection against non-political harms (e.g., privacy advocates, victims' rights groups) often find their concerns secondary to the imperative of protecting political speech. Their voices are present but often outranked by the democratic participation rationale.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the hierarchy of speech protection would collapse. Political speech would lose its privileged status, leading to a chaotic and potentially less informed public discourse, and a significant reordering of how speech is regulated across all domains.
% FOUNDING_PROBLEM: The problem of ensuring a robust and uninhibited exchange of ideas necessary for a self-governing populace, particularly in the face of potential government overreach or suppression of dissent.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, political scientists, and civil liberties organizations (outside the direct beneficiaries of specific political campaigns) consistently attest to the ongoing necessity of protecting political speech for democratic health, citing historical and contemporary threats to free expression.
narrative_ontology:disappearance_verdict(speech_protection_kernel__democratic_participation_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__democratic_participation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__democratic_participation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_protection_kernel__democratic_participation_reading, 'none', 1).

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
 *   The constraint is classified as a Rope because it genuinely coordinates a vital public good (democratic discourse) with broad benefits. Extractiveness is low (0.25) as it primarily facilitates, rather than extracts from, political speech. Suppression is low (0.15) for political speech, but higher for other categories. Theater ratio is low (0.05) as the system genuinely aims to protect speech, even if its application is contested. The metrics reflect the *intended* and *largely achieved* function for political speech, while acknowledging the lower protection for other categories.
 *
 * PERSPECTIVAL GAP:
 *   Citizens and political actors experience this constraint as a strong protection for their core activities, enabling participation. However, individuals or groups whose speech is deemed 'non-political' or harmful, but not directly impacting self-governance, may experience it as a limitation, with their interests subordinated to the higher value placed on political discourse. The judicial system, as agenda-setter, balances these competing interests, often prioritizing the democratic function.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizens, political candidates, and advocacy groups are beneficiaries (low d) as the constraint directly enables their participation in self-governance. The judicial system and legislature are agenda-setters (d near symmetric) as they administer the constraint, balancing protection with other societal interests. Victims of non-political harmful speech are payers (high d) as their interests are subordinated when speech is protected on democratic grounds, even if it causes them harm.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a genuine coordination mechanism (facilitating democracy) as pure extraction. While there are costs to other forms of speech, the core mandate of protecting political expression for self-governance remains live and actively defended. The constraint's persistence is tied to the ongoing need for democratic discourse, not merely institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''democratic participation'' reading of the speech protection kernel, or is it a different reading in disguise?',
    'Analysis of judicial opinions and legislative intent: if restrictions on non-political speech are consistently upheld with lower scrutiny, it corroborates this reading. If political speech is restricted on non-democratic grounds (e.g., ''dignity''), it suggests a different reading is operative.',
    'If confirmed as the democratic participation reading, the classification holds. If found to be a different reading, the constraint''s classification (and its beneficiaries/victims) would shift to reflect that underlying structural reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this constraint as the ''democratic participation'' reading of the ''speech protection'' kernel, distinguishing it from ''absolutist'', ''harm threshold'', ''marketplace'', and ''dignity'' readings.').

omega_variable(
    scope_of_political_speech,
    'What constitutes ''political expression necessary for self-governance'' in practice, and how does this definition affect the scope of protected speech?',
    'Empirical analysis of court decisions: track the types of speech consistently granted highest protection and those deemed ''non-political'' and thus more readily restricted. Examine how new forms of communication (e.g., social media, AI-generated content) are categorized.',
    'A narrow definition of ''political speech'' would reduce the scope of highly protected expression, potentially increasing effective suppression for other forms of speech. A broad definition would expand protection, potentially increasing the ''cost'' of the constraint for those seeking to regulate non-political harms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_political_speech, empirical, 'Ambiguity in defining ''political expression'' and its impact on speech protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__democratic_participation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__democratic_participation_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__democratic_participation_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__democratic_participation_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 20, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__democratic_participation_reading, identity_coordination).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'speech protection' kernel, each with distinct structural properties and classifications. This reading prioritizes speech essential for democratic self-governance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
