% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__popular_constitutionalism_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism: Ongoing Democratic Interpretation
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint describes the 'popular constitutionalism' reading of
 *   basic law interpretive authority, where constitutional meaning is
 *   understood to emerge from ongoing democratic contestation rather than
 *   being fixed by judicial or legislative fiat. It is a 'rope' because it
 *   facilitates broad participation and prevents institutional capture,
 *   though it entails costs of instability and ongoing debate. The metrics
 *   reflect a system with moderate extraction (costs of contestation), low
 *   suppression (no single authority can suppress dissent), and low theater
 *   (the contestation is genuine, not performative).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.45).
domain_priors:suppression_score(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.3).
domain_priors:theater_ratio(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__popular_constitutionalism_reading, rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__popular_constitutionalism_reading, "Popular Constitutionalism: Ongoing Democratic Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_authority__popular_constitutionalism_reading, "constitutional_law/political_theory/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__popular_constitutionalism_reading, '18fe4307-6096-48d9-a352-1337c2524cda').
narrative_ontology:cs_kernel_codification('18fe4307-6096-48d9-a352-1337c2524cda', fixed_text).
narrative_ontology:cs_authority_grounding('18fe4307-6096-48d9-a352-1337c2524cda', distributed).
narrative_ontology:cs_reading_relation('18fe4307-6096-48d9-a352-1337c2524cda', basic_law_interpretive_authority__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('18fe4307-6096-48d9-a352-1337c2524cda', basic_law_interpretive_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('18fe4307-6096-48d9-a352-1337c2524cda', foundational, popular_sovereignty_is_supreme_interpretive_authority).
narrative_ontology:cs_axiom_status(popular_sovereignty_is_supreme_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('18fe4307-6096-48d9-a352-1337c2524cda', popular_sovereignty_is_supreme_interpretive_authority, deontological).
narrative_ontology:cs_axiom('18fe4307-6096-48d9-a352-1337c2524cda', foundational, no_single_institutional_arbiter_of_meaning).
narrative_ontology:cs_axiom_status(no_single_institutional_arbiter_of_meaning, holdable).
narrative_ontology:cs_axiom_grounding('18fe4307-6096-48d9-a352-1337c2524cda', no_single_institutional_arbiter_of_meaning, conventional).
narrative_ontology:cs_reference_frame('18fe4307-6096-48d9-a352-1337c2524cda', founding_moment_of_popular_sovereignty).
narrative_ontology:cs_drift_state('18fe4307-6096-48d9-a352-1337c2524cda', contemporary_era_of_institutional_claims, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('18fe4307-6096-48d9-a352-1337c2524cda', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, the_people).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, democratic_institutions).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, judicial_supremacists).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, parliamentary_supremacists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from direct participation in shaping constitutional meaning, preventing ossification and ensuring responsiveness to evolving societal values. Bears the costs of ongoing contestation and potential instability.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, the_people, beneficiary,
    organized, generational, constrained, national).

% Gain legitimacy and dynamism from their role in mediating and expressing popular constitutional judgments. Must constantly engage in debate and persuasion, avoiding claims of final, unchallengeable authority.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, democratic_institutions, beneficiary,
    institutional, generational, constrained, national).

% Their claim to final interpretive authority is challenged, leading to a loss of institutional prestige and a more contested role for courts. They bear the cost of having their judgments subject to popular override or reinterpretation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, judicial_supremacists, payer,
    institutional, biographical, identity_locked, national).

% Their claim to final interpretive authority is also challenged, as popular constitutionalism asserts a higher, more direct form of democratic will. They bear the cost of having legislative acts potentially invalidated or reinterpreted by popular movements.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, parliamentary_supremacists, payer,
    institutional, biographical, identity_locked, national).

% Analyze the mechanisms and outcomes of popular constitutional engagement, documenting its historical manifestations and theoretical implications. Their work informs, but does not directly adjudicate, the ongoing contestation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the ongoing, decentralized process of constitutional meaning-making, ensuring that the 'basic law' remains responsive to the evolving will of the sovereign people, preventing capture by any single institutional branch.
% TRANSFER_FUNCTION: Transfers interpretive authority from specialized institutional actors (courts, legislatures) to the broader democratic public, distributing the power to shape fundamental law.
% ABSENT_VOICES: Those who believe constitutional meaning should be fixed and insulated from democratic passions, or those who advocate for a purely technocratic or expert-driven interpretation, are structurally marginalized in this framework. Their arguments for stability or expertise are overridden by the emphasis on popular sovereignty.
% DISAPPEARANCE_RATIONALE: If the idea that constitutional meaning emerges from ongoing democratic contestation vanished, the vacuum would likely be filled by claims of judicial or parliamentary supremacy, leading to a re-centralization of interpretive authority and a different institutional balance of power. The nature of constitutional law itself would fundamentally shift.
% FOUNDING_PROBLEM: The problem of constitutional ossification and the potential for a 'dead hand' of the past to bind future generations, or for a single branch of government to usurp ultimate interpretive authority.
% FOUNDING_PROBLEM_CORROBORATION: Historians of constitutional development and political theorists outside the immediate beneficiaries corroborate the ongoing tension between stability and democratic responsiveness, and the recurring attempts by institutional actors to claim final interpretive authority. Public opinion polls often reflect a desire for greater popular input on fundamental issues.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__popular_constitutionalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(basic_law_interpretive_authority__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).
:- end_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the constant need for public engagement and contestation imposes real costs in terms of time, resources, and potential for gridlock. However, these costs are distributed and are seen as legitimate expenses of self-governance. Suppression is low (0.3) because no single institution holds the power to definitively suppress alternative interpretations; the system is designed to keep the interpretive field open. Theater ratio is low (0.1) as the democratic contestation is a genuine, functional aspect of this reading, not a performance masking an inert or extractive core.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of judicial or parliamentary supremacists, this reading would appear highly extractive, as it diminishes their institutional power and introduces instability. From the perspective of the people and democratic institutions, it is a necessary and beneficial coordination mechanism for self-governance. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'people' and 'democratic institutions' are beneficiaries, as this reading empowers them and makes the constitution responsive to their will. Judicial and parliamentary supremacists are 'victims' in this reading, as their claims to final authority are directly challenged and diminished. Constitutional scholars act as observers, analyzing the process without directly participating in the contestation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stability_vs_responsiveness_tradeoff,
    'At what point do the costs of ongoing contestation (instability, gridlock) outweigh the benefits of democratic responsiveness, leading to a collapse of effective constitutional governance?',
    'Empirical analysis of polities exhibiting high levels of popular constitutional engagement: identify thresholds where contestation becomes dysfunctional rather than dynamic.',
    'If the costs are found to frequently outweigh benefits, this reading''s classification might shift towards a ''tangled_rope'' or even ''snare'' for the ''people'' themselves, as the process becomes self-defeating. If the system proves robust, it reinforces the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_vs_responsiveness_tradeoff, empirical, 'The inherent trade-off between constitutional stability and democratic responsiveness.').

omega_variable(
    elite_capture_of_popular_contestation,
    'To what extent can ''popular'' constitutionalism be captured or manipulated by well-resourced elite factions, turning democratic contestation into a performative exercise that serves narrow interests?',
    'Sociological and political science studies analyzing the funding and organization of ''grassroots'' constitutional movements, identifying patterns of elite influence and agenda-setting.',
    'If capture is widespread and effective, the ''theater_ratio'' would be higher, and the ''extractiveness'' would be re-attributed to the capturing elites, potentially reclassifying the constraint as a ''tangled_rope'' or ''snare'' for the broader public.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_capture_of_popular_contestation, empirical, 'Risk of elite capture within a popular constitutional framework.').

omega_variable(
    interpretive_authority_location_ambiguity,
    'Is the ultimate interpretive authority truly distributed and perpetually contested, or does it de facto reside in a specific institutional site (e.g., a dominant political party, a media ecosystem) even under a popular constitutionalist framing?',
    'Detailed case studies of constitutional crises and their resolutions, identifying the actual institutional actors whose interpretations ultimately prevail in practice, regardless of formal claims.',
    'If de facto authority consistently centralizes, the ''popular_constitutionalism_reading'' might be reclassified as a ''piton'' (if the popular aspect is theatrical) or a ''snare'' (if a hidden institutional actor extracts from the ''popular'' process).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_authority_location_ambiguity, conceptual, 'Ambiguity of actual vs. claimed locus of interpretive authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__popular_constitutionalism_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(basi_tr_t50, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(basi_be_t50, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 30, 0.32).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 40, 0.3).
narrative_ontology:measurement(basi_su_t50, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__popular_constitutionalism_reading, identity_coordination).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'basic_law_interpretive_authority' kernel. This 'popular constitutionalism' reading emphasizes ongoing democratic contestation, contrasting with 'judicial_supremacy_reading' and 'parliamentary_sovereignty_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
