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
 *   constraint_id: speech_protection_boundary__absolutist_reading
 *   human_readable: Speech Protection Boundary (Absolutist Reading)
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint represents the 'absolutist' reading of the First
 *   Amendment's speech protection, primarily defined by the Brandenburg v.
 *   Ohio (1969) standard. Under this reading, speech is protected unless it
 *   is directed to inciting or producing imminent lawless action and is
 *   likely to incite or produce such action. This interpretation maximizes
 *   the scope of protected speech, even offensive or hateful speech, at the
 *   cost of aggregate harm to minoritized communities. This is one reading of
 *   the 'speech_protection_boundary' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, 0.65).
domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, 0.1).
domain_priors:theater_ratio(speech_protection_boundary__absolutist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__absolutist_reading, "Speech Protection Boundary (Absolutist Reading)").
narrative_ontology:topic_domain(speech_protection_boundary__absolutist_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(speech_protection_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__absolutist_reading, '5d5016ff-65b3-4e6f-a612-88d75e220009').
narrative_ontology:cs_kernel_codification('5d5016ff-65b3-4e6f-a612-88d75e220009', fixed_text).
narrative_ontology:cs_authority_grounding('5d5016ff-65b3-4e6f-a612-88d75e220009', lineage).
narrative_ontology:cs_interpretation_layer_present('5d5016ff-65b3-4e6f-a612-88d75e220009').
narrative_ontology:cs_reading_relation('5d5016ff-65b3-4e6f-a612-88d75e220009', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_reading_relation('5d5016ff-65b3-4e6f-a612-88d75e220009', speech_protection_boundary__balancing_reading, coexists_with).
narrative_ontology:cs_axiom('5d5016ff-65b3-4e6f-a612-88d75e220009', foundational, maximal_speech_protection_is_foundational).
narrative_ontology:cs_axiom_status(maximal_speech_protection_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('5d5016ff-65b3-4e6f-a612-88d75e220009', maximal_speech_protection_is_foundational, deontological).
narrative_ontology:cs_axiom('5d5016ff-65b3-4e6f-a612-88d75e220009', secondary, harm_from_speech_is_self_correcting).
narrative_ontology:cs_axiom_status(harm_from_speech_is_self_correcting, holdable).
narrative_ontology:cs_axiom_grounding('5d5016ff-65b3-4e6f-a612-88d75e220009', harm_from_speech_is_self_correcting, empirically_contingent).
narrative_ontology:cs_reference_frame('5d5016ff-65b3-4e6f-a612-88d75e220009', post_brandenburg_maximal_protection).
narrative_ontology:cs_drift_state('5d5016ff-65b3-4e6f-a612-88d75e220009', contemporary_digital_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('5d5016ff-65b3-4e6f-a612-88d75e220009', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__absolutist_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, speakers_of_controversial_speech).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, free_speech_advocates).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, minoritized_communities).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, targets_of_hate_speech).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, legislators).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, marketplace_of_ideas_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, slippery_slope_argument_against_regulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate arbiter of speech rights, interpreting the First Amendment to establish the Brandenburg standard. Its rulings define the scope of protected speech and the narrow exceptions for harm, effectively setting the boundaries for all other actors.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from broad protection for their expression, even if it is offensive or hateful, as long as it does not directly incite imminent lawless action. They can express views that might be restricted under other standards without fear of legal repercussions.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, speakers_of_controversial_speech, beneficiary,
    moderate, biographical, mobile, national).

% Bear the brunt of aggregate harm from hate speech and other forms of offensive expression that fall within the protected category. They experience psychological distress, social marginalization, and sometimes physical threats, with limited legal recourse.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, minoritized_communities, payer,
    powerless, generational, trapped, local).

% Directly experience the harm of speech that targets them based on identity. Their ability to participate equally in public life is diminished, and they are often left without effective legal remedies due to the high bar for 'imminent lawless action'.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, targets_of_hate_speech, payer,
    powerless, immediate, identity_locked, local).

% Actively defend the absolutist interpretation, viewing any restriction on speech as a dangerous precedent. They benefit from the broad scope of protected speech, aligning with their ideological commitment to maximal expression.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, free_speech_advocates, beneficiary,
    organized, generational, analytical, national).

% Are constrained in their ability to pass laws regulating harmful speech, even when there is a clear public demand for such regulation. They must navigate the narrow exceptions defined by the Supreme Court, often leading to ineffective or overturned legislation.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, legislators, payer,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, high bar for speech regulation, providing predictability for speakers and limiting government overreach in a diverse society. It coordinates the expectation that nearly all speech is protected.
% TRANSFER_FUNCTION: Transfers the burden of harm from speakers of offensive or hateful speech to minoritized communities and targets of such speech, who must endure its effects without legal remedy. It also transfers power to speakers by maximizing their expressive freedom.
% ABSENT_VOICES: Minoritized communities and targets of hate speech are often marginalized in the legal and political discourse that shapes speech doctrine. Their lived experience of harm is frequently discounted or reframed as a necessary cost of free expression.
% DISAPPEARANCE_RATIONALE: If the Brandenburg standard vanished overnight, there would be immediate legislative efforts to regulate various forms of harmful speech. The legal landscape for expression would become highly contested and fragmented, and the balance of power between speakers and those harmed by speech would fundamentally shift.
% FOUNDING_PROBLEM: The founding problem was to prevent government censorship and protect political dissent, ensuring a robust public discourse free from state interference, particularly in the context of McCarthy-era anti-communist hysteria.
% FOUNDING_PROBLEM_CORROBORATION: Free speech advocates and some legal scholars argue the problem of government censorship remains live. However, minoritized communities and critical legal scholars argue the original problem has been largely solved, and the current standard now enables new forms of private and social harm, with independent academic research supporting this shifted-function reading.
narrative_ontology:disappearance_verdict(speech_protection_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_protection_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__absolutist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because the broad protection for speech, particularly hate speech, imposes significant social and psychological costs on vulnerable groups without offering them adequate legal recourse. Suppression (0.10) is low because the constraint's primary function is to prevent suppression of speech, not to enforce it. The claimed type is 'tangled_rope' because it genuinely coordinates expressive freedom for many (beneficiaries) while simultaneously extracting a heavy cost from specific groups (victims) through the same legal structure, requiring active judicial enforcement to maintain this balance.
 *
 * PERSPECTIVAL GAP:
 *   Speakers of controversial speech and free speech advocates experience this as a 'rope' or even a 'mountain' (natural law of expression), providing essential coordination for a free society. In contrast, minoritized communities and targets of hate speech experience it as a 'snare' or 'tangled_rope', where the coordination function for others directly enables their harm. The engine's per-seat classification should reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court (agenda_setter) and free speech advocates (beneficiary) have low directionality, as the constraint largely aligns with their interests. Speakers of controversial speech are direct beneficiaries. Minoritized communities and targets of hate speech are high-directionality targets (victims), bearing the costs. Legislators are also targets, as their ability to address social harms is constrained by the absolutist standard.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a 'tangled_rope' prevents mislabeling this as a pure 'rope' (which would ignore the significant extraction from victims) or a pure 'snare' (which would ignore the genuine coordination function for speakers). It highlights that the mandate to protect speech has, for some, outlived its original anti-censorship function and now serves to protect speech that causes harm, creating a new form of extraction. The 'contested' status of the founding problem further supports this analysis, indicating a potential drift from original intent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregate_harm_quantification,
    'How can the aggregate harm to minoritized communities from protected hate speech be reliably quantified to inform legal standards?',
    'Longitudinal sociological studies, public health data on stress and discrimination, and economic analyses of participation barriers for targeted groups.',
    'If aggregate harm is demonstrably severe and systemic, it could challenge the ''absolutist'' reading''s premise that such speech is harmless or self-correcting, potentially shifting the legal framework towards a ''balancing'' or ''harm-limited'' approach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_harm_quantification, empirical, 'The difficulty in measuring the cumulative, non-imminent harm of protected speech.').

omega_variable(
    absolutist_vs_balancing_framing,
    'Is the ''absolutist'' reading a genuine interpretation of constitutional text, or a policy choice that prioritizes one value (free expression) over others (equality, dignity)?',
    'Deep historical and philosophical analysis of First Amendment jurisprudence, examining the evolution of ''absolutism'' against alternative interpretive traditions. This is a conceptual, not empirical, question.',
    'If it''s primarily a policy choice, it opens the door for re-evaluation based on contemporary social values and empirical evidence of harm, potentially leading to a ''balancing'' or ''harm-limited'' reclassification. If it''s a genuine textual interpretation, the path to change is through constitutional amendment or a fundamental shift in interpretive methodology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolutist_vs_balancing_framing, conceptual, 'The conceptual framing of speech protection as absolute versus balanced.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (low, 0.10) purely structural (legal barriers to regulation), or is there an internalized component where targets of hate speech self-censor due to fear of reprisal or futility?',
    'Post-exit suppression trajectory: if targets of hate speech continue to self-censor even after legal protections are strengthened, it suggests an internalized component. Qualitative studies of self-censorship among minoritized groups.',
    'If internalized suppression is significant, the constraint''s effective suppression on targets is higher than the structural measure suggests, as they carry the suppression with them even in the absence of direct legal barriers. This would amplify the effective extraction from these groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for targets of hate speech.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__absolutist_reading, 1969, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(spee_be_t1969, speech_protection_boundary__absolutist_reading, base_extractiveness, 1969, 0.5).
narrative_ontology:measurement(spee_be_t1980, speech_protection_boundary__absolutist_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(spee_be_t1990, speech_protection_boundary__absolutist_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(spee_be_t2000, speech_protection_boundary__absolutist_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(spee_be_t2010, speech_protection_boundary__absolutist_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(spee_be_t2024, speech_protection_boundary__absolutist_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1969, speech_protection_boundary__absolutist_reading, suppression_requirement, 1969, 0.15).
narrative_ontology:measurement(spee_su_t1980, speech_protection_boundary__absolutist_reading, suppression_requirement, 1980, 0.12).
narrative_ontology:measurement(spee_su_t1990, speech_protection_boundary__absolutist_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(spee_su_t2000, speech_protection_boundary__absolutist_reading, suppression_requirement, 2000, 0.09).
narrative_ontology:measurement(spee_su_t2010, speech_protection_boundary__absolutist_reading, suppression_requirement, 2010, 0.09).
narrative_ontology:measurement(spee_su_t2024, speech_protection_boundary__absolutist_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, hate_speech_regulation_constraint).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, public_discourse_quality_constraint).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
