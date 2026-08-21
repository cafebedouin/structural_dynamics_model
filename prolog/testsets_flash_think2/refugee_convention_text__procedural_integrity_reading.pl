% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__procedural_integrity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__procedural_integrity_reading, []).

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
 *   constraint_id: refugee_convention_text__procedural_integrity_reading
 *   human_readable: Refugee Convention: Procedural Integrity Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This constraint is the 'procedural integrity' reading of the Refugee
 *   Convention text, which emphasizes fair individualized assessment and
 *   non-negotiable process integrity, distinct from more restrictive or
 *   expansive interpretations. It posits the Convention primarily as a
 *   framework for due process in asylum claims, where the integrity of the
 *   assessment procedure is paramount, even if the outcome for individual
 *   claimants may vary. The metrics reflect the ongoing tension between the
 *   ideal of fair process and the reality of state practices that can make
 *   the process burdensome or performative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, 0.45).
domain_priors:suppression_score(refugee_convention_text__procedural_integrity_reading, 0.6).
domain_priors:theater_ratio(refugee_convention_text__procedural_integrity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__procedural_integrity_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__procedural_integrity_reading, "Refugee Convention: Procedural Integrity Reading").
narrative_ontology:topic_domain(refugee_convention_text__procedural_integrity_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__procedural_integrity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__procedural_integrity_reading, '91010e9f-1a74-408f-aaa0-4521c434b5bd').
narrative_ontology:cs_kernel_codification('91010e9f-1a74-408f-aaa0-4521c434b5bd', fixed_text).
narrative_ontology:cs_authority_grounding('91010e9f-1a74-408f-aaa0-4521c434b5bd', lineage).
narrative_ontology:cs_interpretation_layer_present('91010e9f-1a74-408f-aaa0-4521c434b5bd').
narrative_ontology:cs_reading_relation('91010e9f-1a74-408f-aaa0-4521c434b5bd', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('91010e9f-1a74-408f-aaa0-4521c434b5bd', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_axiom('91010e9f-1a74-408f-aaa0-4521c434b5bd', foundational, individualized_assessment_is_non_negotiable).
narrative_ontology:cs_axiom_status(individualized_assessment_is_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('91010e9f-1a74-408f-aaa0-4521c434b5bd', individualized_assessment_is_non_negotiable, deontological).
narrative_ontology:cs_axiom('91010e9f-1a74-408f-aaa0-4521c434b5bd', foundational, procedural_fairness_is_paramount).
narrative_ontology:cs_axiom_status(procedural_fairness_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('91010e9f-1a74-408f-aaa0-4521c434b5bd', procedural_fairness_is_paramount, conventional).
narrative_ontology:cs_reference_frame('91010e9f-1a74-408f-aaa0-4521c434b5bd', due_process_as_foundational_safeguard).
narrative_ontology:cs_drift_state('91010e9f-1a74-408f-aaa0-4521c434b5bd', contemporary_migration_crises_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('91010e9f-1a74-408f-aaa0-4521c434b5bd', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__procedural_integrity_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, states_seeking_legitimacy).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, unhcr).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, human_rights_advocates).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, asylum_seekers).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, states_seeking_to_avoid_obligations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Subject to the procedural requirements, bearing the burden of proof and navigating complex legal systems. While the process is meant to safeguard them, it can also be a source of delay, uncertainty, and deterrence, especially when states implement burdensome interpretations.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, asylum_seekers, payer,
    powerless, biographical, trapped, global).

% Responsible for implementing and interpreting the Convention's procedural requirements. They benefit from the legitimacy conferred by adherence to international law but may seek to minimize the scope or burden of these procedures.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, states_of_asylum, agenda_setter,
    institutional, generational, constrained, national).

% Mandated to supervise the application of the Convention. Benefits when states uphold procedural integrity, as it aligns with its protection mandate. Acts as an observer and advocate, but lacks direct enforcement power.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, unhcr, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, unhcr, observer).

% Work to ensure states adhere to fair procedures, often through litigation and public advocacy. They benefit when procedural integrity is upheld, but bear the costs of challenging state non-compliance.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, human_rights_advocates, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, human_rights_advocates, payer).

% Interpret the Convention and its procedural requirements, setting precedents that guide state practice. Their rulings provide a check on state discretion and reinforce the non-negotiable aspect of process integrity.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, international_courts, agenda_setter,
    institutional, generational, analytical, global).

% These states actively seek to limit their obligations under the Convention, often by implementing restrictive interpretations of procedural requirements or by creating barriers to access. They bear the cost of international scrutiny and legal challenges when they deviate too far from procedural integrity.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, states_seeking_to_avoid_obligations, payer,
    powerful, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common, internationally recognized framework for assessing asylum claims, ensuring a minimum standard of due process and preventing arbitrary refoulement, thereby coordinating state action around a shared legal standard.
% TRANSFER_FUNCTION: Transfers the obligation to conduct fair individualized assessments onto states, while transferring the burden of proof and procedural compliance onto asylum seekers. It also transfers legitimacy to states that adhere to the process.
% ABSENT_VOICES: Asylum seekers who are denied access to any procedure or whose claims are summarily rejected without individualized assessment; they would argue for substantive protection over mere procedural form, emphasizing the outcome rather than just the process.
% DISAPPEARANCE_RATIONALE: If the procedural integrity requirement of the Refugee Convention vanished overnight, states would be free to implement arbitrary or non-existent assessment processes, leading to widespread refoulement, a collapse of the international protection regime, and a significant increase in human rights violations for asylum seekers.
% FOUNDING_PROBLEM: The post-WWII need for a standardized, fair process to determine who qualifies for international protection, preventing states from arbitrarily returning individuals to persecution without due consideration of their claims.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR reports, international human rights bodies, and legal scholars consistently highlight the ongoing need for robust procedural safeguards in asylum systems, corroborating the problem's live status from outside the immediate state beneficiaries. The persistence of arbitrary detention, summary rejections, and lack of access to legal aid in many jurisdictions underscores this ongoing need.
narrative_ontology:disappearance_verdict(refugee_convention_text__procedural_integrity_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__procedural_integrity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__procedural_integrity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(refugee_convention_text__procedural_integrity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__procedural_integrity_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__procedural_integrity_reading_tests).
:- end_tests(refugee_convention_text__procedural_integrity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting that while the process is a safeguard, it can also be a significant burden on asylum seekers, and states may use procedural complexity to deter claims. Suppression (0.60) is substantial because states often resist robust procedural requirements, necessitating active enforcement and advocacy to maintain standards. The theater ratio (0.40) indicates a risk of performative compliance, where the form of due process is maintained while its substantive protective function is diminished. The claimed type is Tangled Rope because it genuinely coordinates state action around a procedural standard, but also involves asymmetric extraction from asylum seekers and states seeking to avoid obligations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states seeking to avoid obligations, the procedural integrity requirements are an extractive burden on their sovereignty. From the perspective of human rights advocates, these procedures are a vital, though often imperfect, safeguard. The engine will compute these divergent classifications based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   States seeking legitimacy, UNHCR, and human rights advocates are beneficiaries, as the constraint upholds a framework they endorse or benefit from. Asylum seekers are payers, bearing the direct costs and burdens of the process. States seeking to avoid obligations are also payers, as they incur costs from being compelled to implement robust procedures. International courts act as agenda-setters, shaping the interpretation and enforcement of these procedures.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_vs_substantive_protection,
    'To what extent does upholding procedural integrity genuinely lead to substantive protection outcomes, versus merely legitimizing denials through a ''fair'' process?',
    'Empirical studies comparing asylum outcomes in jurisdictions with varying degrees of procedural robustness, controlling for other factors. Analysis of appeal success rates and subsequent refoulement incidents.',
    'If procedural integrity frequently leads to substantive protection, the constraint''s coordination function is stronger and extraction lower. If it primarily legitimizes denials, the extraction component is higher, and the constraint leans more towards a Snare or highly extractive Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_vs_substantive_protection, empirical, 'The functional relationship between procedural fairness and actual protection outcomes.').

omega_variable(
    offshore_processing_procedural_guarantees,
    'Can offshore processing arrangements genuinely provide full procedural guarantees as required by this reading, or do they inherently compromise integrity?',
    'Independent monitoring and legal review of specific offshore processing regimes, assessing access to legal counsel, appeal mechanisms, and non-refoulement safeguards in practice.',
    'If full procedural guarantees are demonstrably impossible in offshore contexts, this reading would foreclose such practices. If they are achievable, then offshore processing, under strict conditions, could be consistent with this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(offshore_processing_procedural_guarantees, conceptual, 'Compatibility of offshore processing with procedural integrity requirements.').

omega_variable(
    reading_naturalness_vs_advocacy,
    'Is this ''procedural integrity'' reading a natural interpretation of the Convention''s text and history, or is it primarily an advocacy position developed to counter more restrictive state practices?',
    'Historical legal scholarship tracing the evolution of Convention interpretation, and comparative analysis of state practice and jurisprudence over time.',
    'If it''s a natural interpretation, its legitimacy is inherent. If it''s primarily an advocacy position, its persistence depends more on the power of advocates and less on the inherent structure of the Convention, potentially shifting its classification towards a more actively defended (and thus more extractive) Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_naturalness_vs_advocacy, conceptual, 'The origin and grounding of the procedural integrity reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__procedural_integrity_reading, 1951, 2021).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1951, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1951, 0.25).
narrative_ontology:measurement(refu_tr_t1965, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1965, 0.28).
narrative_ontology:measurement(refu_tr_t1979, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1979, 0.32).
narrative_ontology:measurement(refu_tr_t1993, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1993, 0.35).
narrative_ontology:measurement(refu_tr_t2007, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2007, 0.38).
narrative_ontology:measurement(refu_tr_t2021, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2021, 0.4).

% Extraction over time
narrative_ontology:measurement(refu_be_t1951, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1951, 0.35).
narrative_ontology:measurement(refu_be_t1965, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1965, 0.38).
narrative_ontology:measurement(refu_be_t1979, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1979, 0.4).
narrative_ontology:measurement(refu_be_t1993, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1993, 0.42).
narrative_ontology:measurement(refu_be_t2007, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2007, 0.44).
narrative_ontology:measurement(refu_be_t2021, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2021, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1951, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1951, 0.5).
narrative_ontology:measurement(refu_su_t1965, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1965, 0.52).
narrative_ontology:measurement(refu_su_t1979, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1979, 0.55).
narrative_ontology:measurement(refu_su_t1993, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1993, 0.57).
narrative_ontology:measurement(refu_su_t2007, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2007, 0.59).
narrative_ontology:measurement(refu_su_t2021, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2021, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__procedural_integrity_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
