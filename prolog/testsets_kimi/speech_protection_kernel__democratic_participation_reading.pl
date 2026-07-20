% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__democratic_participation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Democratic Participation Reading of Speech Protection Kernel
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the democratic_participation_reading of the
 *   speech_protection_kernel: the constitutional doctrine that political
 *   expression necessary for self-governance receives the strongest judicial
 *   protection, while non-political speech is more readily restricted. It is
 *   a commitment-system constraint enforced by courts through tiered scrutiny
 *   frameworks. The claim/metric independence is maintained: the reading is
 *   claimed as Tangled Rope because it combines genuine democratic
 *   coordination with asymmetric extraction against non-political speakers,
 *   and the metrics are authored descriptively to match the doctrinal
 *   operation.
 *
 * KEY AGENTS:
 *   - political_speakers: Primary beneficiary (organized/mobile) â receive heightened strict scrutiny protection.
 *   - non_political_speakers: Primary target (moderate/constrained) â bear the cost of reduced judicial protection.
 *   - judiciary: Agenda-setter (institutional/constrained) â articulates and enforces the tiered framework.
 *   - electorate: Diffuse beneficiary (moderate/constrained) â gains from protected political discourse.
 *   - dignity_advocates: Excluded voice (moderate/constrained) â would argue for dignitary limits on political speech.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, 0.62).
domain_priors:suppression_score(speech_protection_kernel__democratic_participation_reading, 0.65).
domain_priors:theater_ratio(speech_protection_kernel__democratic_participation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__democratic_participation_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__democratic_participation_reading, "Democratic Participation Reading of Speech Protection Kernel").
narrative_ontology:topic_domain(speech_protection_kernel__democratic_participation_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__democratic_participation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__democratic_participation_reading, '50d1bd00-5aab-4c34-8176-ec8462fe96e5').
narrative_ontology:cs_kernel_codification('50d1bd00-5aab-4c34-8176-ec8462fe96e5', formalized).
narrative_ontology:cs_authority_grounding('50d1bd00-5aab-4c34-8176-ec8462fe96e5', lineage).
narrative_ontology:cs_interpretation_layer_present('50d1bd00-5aab-4c34-8176-ec8462fe96e5').
narrative_ontology:cs_reading_relation('50d1bd00-5aab-4c34-8176-ec8462fe96e5', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('50d1bd00-5aab-4c34-8176-ec8462fe96e5', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('50d1bd00-5aab-4c34-8176-ec8462fe96e5', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('50d1bd00-5aab-4c34-8176-ec8462fe96e5', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_axiom('50d1bd00-5aab-4c34-8176-ec8462fe96e5', foundational, democratic_governance_speech_nexus).
narrative_ontology:cs_axiom_status(democratic_governance_speech_nexus, holdable).
narrative_ontology:cs_axiom_grounding('50d1bd00-5aab-4c34-8176-ec8462fe96e5', democratic_governance_speech_nexus, conventional).
narrative_ontology:cs_axiom('50d1bd00-5aab-4c34-8176-ec8462fe96e5', foundational, content_based_scrutiny_hierarchy).
narrative_ontology:cs_axiom_status(content_based_scrutiny_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('50d1bd00-5aab-4c34-8176-ec8462fe96e5', content_based_scrutiny_hierarchy, conventional).
narrative_ontology:cs_reference_frame('50d1bd00-5aab-4c34-8176-ec8462fe96e5', democratic_deliberation_framework).
narrative_ontology:cs_drift_state('50d1bd00-5aab-4c34-8176-ec8462fe96e5', contemporary_polarized_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('50d1bd00-5aab-4c34-8176-ec8462fe96e5', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, political_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, electorate).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, non_political_speakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in expression concerning elections, governance, and public policy. Enjoy the highest level of judicial protection against government restriction; courts apply strict scrutiny to regulations targeting their speech. They may shift to non-political topics, but doing so removes the heightened shield.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, political_speakers, beneficiary,
    organized, generational, mobile, national).

% Benefits from the flow of political information and argumentation that the doctrine is meant to secure, enabling informed participation in self-governance. Bears the systemic cost of weakened protections for other speech categories that might also serve public understanding.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, electorate, beneficiary,
    moderate, generational, constrained, national).

% Produce commercial, artistic, or cultural expression that courts categorize as lying outside the core of political self-governance. Subject to intermediate or rational-basis review, making their speech more vulnerable to regulation, licensing, and restriction than political discourse.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, non_political_speakers, payer,
    moderate, biographical, constrained, national).

% Federal courts, especially the Supreme Court, articulate and enforce the tiered framework of speech protection. They draw the boundary between political and non-political expression, apply varying levels of scrutiny, and legitimize the hierarchy through constitutional interpretation. Cannot abandon the doctrine without undermining institutional authority and doctrinal stability.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Advance frameworks that would condition speech protection on non-subordination of marginalized groups. Their preferred approach is systematically marginalized within the democratic-participation framework, which prioritizes political expression over dignitary harm.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, dignity_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects the speech necessary for democratic self-governance, ensuring that citizens can receive the information and argumentation required to hold representatives accountable and participate in political decision-making.
% TRANSFER_FUNCTION: Moves judicial protection strength from non-political categories of speech toward political expression; moves regulatory discretion and vulnerability toward speakers of commercial, artistic, and other non-political expression.
% ABSENT_VOICES: Dignity-focused advocates and speakers of marginalized cultural expression who would argue that the hierarchy devalues their voices and enables structural subordination; they are excluded because the doctrinal framework centers electoral and policy debate over dignitary harm.
% DISAPPEARANCE_RATIONALE: If the tiered protection vanished overnight, campaign finance regimes, protest permit standards, commercial speech regulations, and artistic censorship rules would all shift to a single standard; the constitutional architecture of the First Amendment would reorganize around either categorical protection or a balancing test.
% FOUNDING_PROBLEM: How to prevent democratic self-governance from being captured by government censorship of political opposition and dissent while preserving space for necessary regulation of non-political harms.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians attest the founding concern was political censorship. Critical legal scholars and free-speech theorists outside the immediate beneficiary set attest the problem has morphed: the hierarchy now empowers wealthy political spenders and marginalizes non-political voices, suggesting the original problem is dead or transformed.
narrative_ontology:disappearance_verdict(speech_protection_kernel__democratic_participation_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__democratic_participation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__democratic_participation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__democratic_participation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__democratic_participation_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is substantial because the hierarchy systematically disadvantages non-political speakers by design, exposing them to regulation that political speakers avoid. Suppression (0.65) is high because the constraint's persistence depends on courts actively policing the political/non-political boundary and dismissing alternative frameworks (dignity, harm-threshold). Theater ratio (0.40) reflects that while the coordination function (protecting dissent) is real, an increasing share of doctrinal maintenance is formalistic boundary-drawing that performs democratic legitimacy without clear empirical grounding. Accessibility collapse (0.70) is high: once inside the doctrinal framework, alternative theories (equal protection for all speech) appear legally incorrect. Resistance (0.45) is moderate: commercial speakers and critical theorists resist, but the interpretive layer absorbs much of the challenge.
 *
 * PERSPECTIVAL GAP:
 *   Political speakers experience the constraint as protective scaffolding (Rope-like), while non-political speakers experience it as an exposed regulatory target (Snare-like). The judiciary experiences it as necessary constitutional architecture. The engine will compute divergent per-seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Political speakers and the electorate are declared beneficiaries: their relationship to the constraint is subsidized protection (low d, damped extraction). Non-political speakers are declared victims: their relationship is heightened regulatory exposure (high d, amplified extraction). The judiciary is agenda-setter with constrained exit, placing its d near symmetric but slightly toward administration.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing government censorship of political dissent â remains live in authoritarian contexts, but within mature constitutional democracies the doctrine has atrophied into a formalistic hierarchy that protects spending and devalues cultural speech. The founding_problem_status is 'contested' because beneficiaries claim the problem persists while external scholars argue it has transformed. This prevents mislabeling the constraint as pure extraction (Snare) because the coordination function is still operationally real, and prevents mislabeling it as pure coordination (Rope) because the asymmetric extraction is structurally embedded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_speech_boundary_contestability,
    'Who decides what speech is ''necessary for self-governance,'' and does that definitional power create extraction for the agenda-setter?',
    'Comparative doctrinal analysis across jurisdictions; empirical study of which speech gets categorized as political versus non-political in practice.',
    'If the boundary is manipulable by courts and legislatures, the constraint is more extractive than coordinating; if the boundary is natural and stable, the hierarchy is closer to genuine coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_speech_boundary_contestability, conceptual, 'Whether the political/non-political boundary is constructed or natural.').

omega_variable(
    democratic_coordination_extraction_separability,
    'Can the protection of politically necessary speech be maintained without the accompanying devaluation of non-political speech?',
    'Natural experiment from jurisdictions with proportionality review rather than tiered scrutiny.',
    'If separable, the hierarchy is extractive overlay on a simpler coordination mechanism; if inseparable, the extraction is inherent cost of the coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_coordination_extraction_separability, conceptual, 'Whether coordination and extraction are structurally separable in this doctrine.').

omega_variable(
    sibling_reading_dignity_pressure,
    'Does the democratic participation reading structurally foreclose the dignity reading, or merely influence it?',
    'Case law analysis of whether political speech doctrine blocks dignitary harm remedies.',
    'If foreclosing, the reading is more coercive than influential; classification shifts toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_dignity_pressure, conceptual, 'Structural relationship to the dignity sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__democratic_participation_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spk_dpr_tr_t0, speech_protection_kernel__democratic_participation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(spk_dpr_tr_t12, speech_protection_kernel__democratic_participation_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(spk_dpr_tr_t24, speech_protection_kernel__democratic_participation_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(spk_dpr_tr_t36, speech_protection_kernel__democratic_participation_reading, theater_ratio, 36, 0.42).
narrative_ontology:measurement(spk_dpr_tr_t48, speech_protection_kernel__democratic_participation_reading, theater_ratio, 48, 0.4).
narrative_ontology:measurement(spk_dpr_tr_t60, speech_protection_kernel__democratic_participation_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(spk_dpr_be_t0, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(spk_dpr_be_t12, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(spk_dpr_be_t24, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(spk_dpr_be_t36, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 36, 0.64).
narrative_ontology:measurement(spk_dpr_be_t48, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 48, 0.62).
narrative_ontology:measurement(spk_dpr_be_t60, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(spk_dpr_su_t0, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(spk_dpr_su_t12, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(spk_dpr_su_t24, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(spk_dpr_su_t36, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 36, 0.68).
narrative_ontology:measurement(spk_dpr_su_t48, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 48, 0.65).
narrative_ontology:measurement(spk_dpr_su_t60, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 60, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__democratic_participation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the speech_protection_kernel. The kernel decomposes into multiple structurally distinct constraints because each reading produces a different beneficiary/victim structure, different epsilon, and different classification. This file instantiates the democratic_participation_reading only; sibling readings are separate constraints in the same family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
