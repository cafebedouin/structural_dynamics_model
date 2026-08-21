% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__temporal_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__temporal_accommodation_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__temporal_accommodation_reading
 *   human_readable: Eternal Marriage Covenant: Temporal Accommodation Reading
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint is the 'temporal_accommodation_reading' of the
 *   'eternal_marriage_covenant' kernel. It describes the religious
 *   institution's response to federal anti-polygamy laws, where the practice
 *   of plural marriage was suspended (via the Manifesto) to ensure
 *   institutional survival and compliance with the law of the land, while
 *   explicitly maintaining the underlying doctrine's eternal validity and the
 *   implicit expectation of future restoration. It functions as a
 *   transitional support structure for the community during a period of
 *   external pressure.
 *
 * KEY AGENTS:
 *   - religious_institution_leadership: Agenda-setter (institutional/constrained)
 *   - devout_members_accommodating: Beneficiary/Payer (moderate/identity_locked)
 *   - devout_members_dissenting: Payer (powerless/identity_locked)
 *   - secular_government: Agenda-setter (institutional/mobile)
 *   - analytical_theologians: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__temporal_accommodation_reading, 0.35).
domain_priors:suppression_score(eternal_marriage_covenant__temporal_accommodation_reading, 0.75).
domain_priors:theater_ratio(eternal_marriage_covenant__temporal_accommodation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(eternal_marriage_covenant__temporal_accommodation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__temporal_accommodation_reading, scaffold).
narrative_ontology:human_readable(eternal_marriage_covenant__temporal_accommodation_reading, "Eternal Marriage Covenant: Temporal Accommodation Reading").
narrative_ontology:topic_domain(eternal_marriage_covenant__temporal_accommodation_reading, "religious_law/political_theology/commitment_system_dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__temporal_accommodation_reading).
narrative_ontology:has_sunset_clause(eternal_marriage_covenant__temporal_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__temporal_accommodation_reading, 'ef04227f-dd00-48fa-9b31-8a5aab9aa95e').
narrative_ontology:cs_kernel_codification('ef04227f-dd00-48fa-9b31-8a5aab9aa95e', fixed_text).
narrative_ontology:cs_authority_grounding('ef04227f-dd00-48fa-9b31-8a5aab9aa95e', lineage).
narrative_ontology:cs_interpretation_layer_present('ef04227f-dd00-48fa-9b31-8a5aab9aa95e').
narrative_ontology:cs_reading_relation('ef04227f-dd00-48fa-9b31-8a5aab9aa95e', eternal_marriage_covenant__immutable_commandment_reading, coexists_with).
narrative_ontology:cs_reading_relation('ef04227f-dd00-48fa-9b31-8a5aab9aa95e', eternal_marriage_covenant__prophetic_override_reading, coexists_with).
narrative_ontology:cs_axiom('ef04227f-dd00-48fa-9b31-8a5aab9aa95e', foundational, obedience_to_law_of_land_is_divine_mandate).
narrative_ontology:cs_axiom_status(obedience_to_law_of_land_is_divine_mandate, holdable).
narrative_ontology:cs_axiom_grounding('ef04227f-dd00-48fa-9b31-8a5aab9aa95e', obedience_to_law_of_land_is_divine_mandate, theological).
narrative_ontology:cs_axiom('ef04227f-dd00-48fa-9b31-8a5aab9aa95e', foundational, eternal_principles_can_be_temporarily_suspended).
narrative_ontology:cs_axiom_status(eternal_principles_can_be_temporarily_suspended, holdable).
narrative_ontology:cs_axiom_grounding('ef04227f-dd00-48fa-9b31-8a5aab9aa95e', eternal_principles_can_be_temporarily_suspended, theological).
narrative_ontology:cs_reference_frame('ef04227f-dd00-48fa-9b31-8a5aab9aa95e', eternal_principle_unimpeded_practice).
narrative_ontology:cs_drift_state('ef04227f-dd00-48fa-9b31-8a5aab9aa95e', post_manifesto_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('ef04227f-dd00-48fa-9b31-8a5aab9aa95e', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, religious_institution_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__temporal_accommodation_reading, devout_members_accommodating).
narrative_ontology:constraint_victim(eternal_marriage_covenant__temporal_accommodation_reading, devout_members_dissenting).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the Manifesto, suspending the practice of plural marriage while affirming the eternal validity of the doctrine. They enforce obedience to the law of the land among members, preserving the institution's legal standing and long-term doctrinal integrity. They bear the cost of internal dissent but gain institutional survival.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, religious_institution_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Adhere to the Manifesto, prioritizing obedience to the religious leadership and the law of the land. They benefit from maintaining community standing and avoiding legal persecution, but bear the cost of suspending a deeply held religious practice. Their identity is fused with the institution's teachings.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, devout_members_accommodating, beneficiary,
    moderate, biographical, identity_locked, local).

% Believe in the eternal principle of plural marriage and desire to practice it, but are compelled by institutional authority and secular law to suspend it. They bear the cost of unfulfilled religious conviction and potential social ostracization if they defy the accommodation. Their identity is also deeply tied to the doctrine, making exit difficult.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, devout_members_dissenting, payer,
    powerless, biographical, identity_locked, local).

% Imposed legal and political pressure that led to the Manifesto. It benefits from the religious institution's compliance with federal law, maintaining social order and its own authority. It does not directly enforce the Manifesto but benefits from its existence.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, secular_government, agenda_setter,
    institutional, generational, mobile, national).

% Study the theological and historical implications of the Manifesto, analyzing its impact on doctrine, practice, and institutional development. They are external to the direct operation of the constraint but provide critical commentary.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__temporal_accommodation_reading, analytical_theologians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__temporal_accommodation_reading, religious_institution_leadership).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__temporal_accommodation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the religious community's adherence to both its eternal doctrine and the prevailing secular law, preventing legal conflict and ensuring the institution's survival and continuity.
% TRANSFER_FUNCTION: Transfers the immediate right to practice a core religious principle from individual members to the authority of the law of the land, in exchange for the preservation of the religious institution and the future (dormant) validity of the doctrine.
% ABSENT_VOICES: Members who believe the eternal principle should be practiced immediately, regardless of secular law, or those who believe the doctrine should be renounced entirely. Their voices are suppressed by institutional pressure and the framing of the Manifesto as divine guidance.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its underlying accommodation vanished overnight, the religious institution would face immediate legal and existential threats, its relationship with the state would collapse, and its internal cohesion would be severely tested as members would either resume the practice or demand formal renunciation of the doctrine. The entire social and theological structure would reorganize.
% FOUNDING_PROBLEM: The religious institution faced existential legal persecution, confiscation of property, and imprisonment of its leaders due to the conflict between its practice of plural marriage and federal anti-polygamy laws.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, federal court decisions, and independent historians corroborate the severe legal and political pressure faced by the institution. While the specific legal threat of polygamy is largely resolved, the broader tension between religious freedom and secular law remains a live issue for many religious groups, making the 'status' of the founding problem contested in its broader implications.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__temporal_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__temporal_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__temporal_accommodation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(eternal_marriage_covenant__temporal_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__temporal_accommodation_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).
:- end_tests(eternal_marriage_covenant__temporal_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) because while the direct practice (and thus its immediate costs/benefits) is suspended, the underlying doctrine still extracts identity-level commitment and the potential for future obligations. Suppression is high (0.75) due to the combined force of secular law and institutional enforcement of the accommodation. Theater ratio starts moderate (0.45) as the initial suspension involves a degree of public performance of obedience while internal doctrinal validity is maintained, gradually decreasing as the accommodation becomes more integrated into institutional practice. The 'scaffold' classification reflects its transitional nature, designed to support the institution through a crisis while preserving its core identity, with an implicit 'sunset' tied to a future (though undefined) restoration of the eternal principle.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the religious institution's leadership, this is a necessary and divinely guided accommodation that preserves the institution and its doctrine. From the perspective of devout members dissenting, it is a painful suppression of a core religious practice, even if framed as obedience. The engine will compute these divergent experiences based on the declared structural roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious_institution_leadership benefits from the constraint by preserving the institution and its doctrine, placing them near the beneficiary end. Devout_members_accommodating also benefit from community standing but pay the cost of suspending practice, placing them closer to symmetric. Devout_members_dissenting are clear targets, bearing the cost of suppressed practice. The secular_government is an external agenda-setter whose laws drive the constraint, benefiting from compliance.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling a strategic, temporary accommodation as either pure extraction (Snare) or pure coordination (Rope). While it involves suppression and costs for some members, its primary function is to scaffold the institution through a period of external pressure, preserving its mandate for future fulfillment. The 'has_sunset_clause: true' reflects the implicit expectation of future restoration, preventing it from being a Piton (inertial decay) or a permanent Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    temporary_vs_permanent_suspension,
    'Is the ''temporal accommodation'' truly temporary, with an expectation of future restoration of the practice, or has it become a de facto permanent renunciation of the practice, despite doctrinal claims?',
    'Future institutional actions (e.g., formal re-endorsement of the practice, or further doctrinal clarification explicitly renouncing it) or long-term sociological analysis of member expectations and behaviors.',
    'If permanent, the constraint''s ''scaffold'' nature would degrade towards a ''piton'' (inertial maintenance of a dead mandate) or a ''snare'' (if the dormant doctrine continues to extract identity costs without any prospect of fulfillment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporary_vs_permanent_suspension, empirical, 'The true temporal nature of the accommodation.').

omega_variable(
    coordination_vs_strategic_retreat,
    'Is the ''obedience to the law of the land'' a genuine coordination with secular authority, or a strategic retreat to preserve institutional power and assets, with the intent to reassert the practice when political conditions allow?',
    'Analysis of internal institutional communications and historical records for evidence of strategic intent, and comparison with other religious groups'' responses to similar legal pressures.',
    'If a strategic retreat, the ''scaffold'' classification would lean more towards a ''tangled_rope'' or ''snare'', as the coordination narrative would be a cover for preserving extractive power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_strategic_retreat, conceptual, 'The underlying motivation for the accommodation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__temporal_accommodation_reading, 1890, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1890, 0.45).
narrative_ontology:measurement(eter_tr_t1900, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1900, 0.42).
narrative_ontology:measurement(eter_tr_t1910, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1910, 0.38).
narrative_ontology:measurement(eter_tr_t1920, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1920, 0.35).
narrative_ontology:measurement(eter_tr_t1930, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1930, 0.32).
narrative_ontology:measurement(eter_tr_t1940, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1940, 0.3).
narrative_ontology:measurement(eter_tr_t1950, eternal_marriage_covenant__temporal_accommodation_reading, theater_ratio, 1950, 0.28).

% Extraction over time
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1890, 0.35).
narrative_ontology:measurement(eter_be_t1900, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1900, 0.32).
narrative_ontology:measurement(eter_be_t1910, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1910, 0.3).
narrative_ontology:measurement(eter_be_t1920, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1920, 0.31).
narrative_ontology:measurement(eter_be_t1930, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1930, 0.33).
narrative_ontology:measurement(eter_be_t1940, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1940, 0.34).
narrative_ontology:measurement(eter_be_t1950, eternal_marriage_covenant__temporal_accommodation_reading, base_extractiveness, 1950, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1890, 0.75).
narrative_ontology:measurement(eter_su_t1900, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1900, 0.78).
narrative_ontology:measurement(eter_su_t1910, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1910, 0.8).
narrative_ontology:measurement(eter_su_t1920, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1920, 0.79).
narrative_ontology:measurement(eter_su_t1930, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1930, 0.77).
narrative_ontology:measurement(eter_su_t1940, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1940, 0.76).
narrative_ontology:measurement(eter_su_t1950, eternal_marriage_covenant__temporal_accommodation_reading, suppression_requirement, 1950, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__temporal_accommodation_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__temporal_accommodation_reading, eternal_marriage_covenant__prophetic_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'eternal_marriage_covenant' kernel. This 'temporal_accommodation_reading' focuses on the suspension of practice due to external pressure, while maintaining doctrinal validity. It differs from the 'immutable_commandment_reading' (emphasizing unchanging divine law) and the 'prophetic_override_reading' (emphasizing living prophetic authority to supersede revelation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
