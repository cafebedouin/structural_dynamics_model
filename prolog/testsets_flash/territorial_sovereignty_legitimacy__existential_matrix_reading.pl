% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__existential_matrix_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__existential_matrix_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: territorial_sovereignty_legitimacy__existential_matrix_reading
 *   human_readable: Existential Matrix Reading of Territorial Sovereignty Legitimacy
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint models the 'existential matrix' reading of territorial
 *   sovereignty, where legitimacy is not derived from juridical arguments but
 *   from a perceived zero-sum struggle for collective survival and identity.
 *   This framing renders legal and historical claims secondary, making
 *   territorial compromise inherently unstable and conflict persistent. The
 *   constraint is claimed as a Snare because it actively extracts from the
 *   subjugated population and the international legal order, while its
 *   persistence relies on high suppression and the suppression of
 *   alternative, non-existential framings. The metrics reflect a high and
 *   increasing degree of extraction and suppression over time, with a
 *   significant 'theater ratio' indicating that legal and diplomatic efforts
 *   often serve as a performative cover for the underlying existential
 *   struggle.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.9).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.95).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__existential_matrix_reading, snare).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__existential_matrix_reading, "Existential Matrix Reading of Territorial Sovereignty Legitimacy").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__existential_matrix_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__existential_matrix_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__existential_matrix_reading, '27e5cc4d-0dd5-4497-a9ff-d20cb49a4c24').
narrative_ontology:cs_kernel_codification('27e5cc4d-0dd5-4497-a9ff-d20cb49a4c24', implicit).
narrative_ontology:cs_authority_grounding('27e5cc4d-0dd5-4497-a9ff-d20cb49a4c24', extraction).
narrative_ontology:cs_interpretation_layer_present('27e5cc4d-0dd5-4497-a9ff-d20cb49a4c24').
narrative_ontology:cs_reading_relation('27e5cc4d-0dd5-4497-a9ff-d20cb49a4c24', territorial_sovereignty_legitimacy__covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('27e5cc4d-0dd5-4497-a9ff-d20cb49a4c24', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_axiom('27e5cc4d-0dd5-4497-a9ff-d20cb49a4c24', foundational, territorial_control_is_existential_precondition).
narrative_ontology:cs_axiom_status(territorial_control_is_existential_precondition, holdable).
narrative_ontology:cs_axiom_grounding('27e5cc4d-0dd5-4497-a9ff-d20cb49a4c24', territorial_control_is_existential_precondition, deontological).
narrative_ontology:cs_axiom('27e5cc4d-0dd5-4497-a9ff-d20cb49a4c24', foundational, collective_survival_requires_zero_sum_territory).
narrative_ontology:cs_axiom_status(collective_survival_requires_zero_sum_territory, holdable).
narrative_ontology:cs_axiom_grounding('27e5cc4d-0dd5-4497-a9ff-d20cb49a4c24', collective_survival_requires_zero_sum_territory, empirically_contingent).
narrative_ontology:cs_reference_frame('27e5cc4d-0dd5-4497-a9ff-d20cb49a4c24', unconditional_territorial_security).
narrative_ontology:cs_drift_state('27e5cc4d-0dd5-4497-a9ff-d20cb49a4c24', contemporary_globalized_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('27e5cc4d-0dd5-4497-a9ff-d20cb49a4c24', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_power_bloc).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, subjugated_population).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, international_law_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% This bloc believes its collective survival and identity are inextricably linked to territorial control. It actively enforces its claims through military and political means, viewing any compromise as an existential threat. It benefits from maintaining a zero-sum conflict frame.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_power_bloc, agenda_setter,
    institutional, generational, identity_locked, regional).

% This population experiences direct extraction through loss of land, resources, and self-determination. Their collective survival and identity are also tied to the territory, but they lack the power to assert their claims effectively against the dominant bloc. They are trapped by the existential framing of the conflict.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, subjugated_population, payer,
    powerless, generational, trapped, local).

% The framework of international law, treaties, and human rights is undermined and rendered ineffective by the existential framing. Its principles are treated as secondary to perceived survival needs, leading to a collapse of its authority in this context. It 'pays' by losing legitimacy and enforcement capacity.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, international_law_framework, payer,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_non_agent(territorial_sovereignty_legitimacy__existential_matrix_reading, international_law_framework).

% These actors attempt to mediate and apply juridical solutions (e.g., two-state solutions) but find their efforts consistently frustrated by the underlying existential claims. They observe the conflict's persistence despite legal arguments, often misinterpreting the 'irrationality' of the parties.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, international_diplomats, observer,
    institutional, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading asserts that the constraint coordinates the actions of a 'people' towards securing its collective survival and identity expression through territorial control, by framing any alternative as an existential threat.
% TRANSFER_FUNCTION: It transfers territorial control, resources, and the right to self-determination from the subjugated population to the dominant power bloc. It also transfers the burden of conflict and instability to the international community.
% ABSENT_VOICES: Any voice advocating for purely juridical solutions, shared sovereignty, or non-territorial forms of collective identity would be dismissed as naive or dangerous by the dominant power bloc, as such arguments fail to address the perceived existential stakes.
% DISAPPEARANCE_RATIONALE: If this existential framing of sovereignty legitimacy vanished, the entire conflict dynamic would fundamentally shift. Territorial disputes would become negotiable legal or political issues, rather than zero-sum survival struggles. This would open pathways for compromise and drastically alter the behavior of all parties, leading to a complete rearrangement of regional power structures and international engagement.
% FOUNDING_PROBLEM: The problem this reading addresses is the perceived vulnerability and threat to collective survival and identity of a 'people' in a contested territory, leading to a need for absolute territorial control.
% FOUNDING_PROBLEM_CORROBORATION: The dominant power bloc consistently attests to the live status of this problem, citing historical grievances and ongoing threats. The subjugated population also experiences an existential threat, albeit from the dominant bloc's actions. International observers often acknowledge the deep-seated fears, even if they dispute the zero-sum conclusion.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__existential_matrix_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__existential_matrix_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__existential_matrix_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.9) and suppression (0.95) reflect the zero-sum nature of the conflict under this reading: one side's gain is the other's absolute loss, enforced through military and political dominance. The theater ratio (0.7) is high because diplomatic processes and legal arguments are often used performatively to justify or obscure the underlying existential struggle, rather than genuinely resolving it. Accessibility collapse is high (0.9) because the existential framing makes any alternative (e.g., shared sovereignty, non-territorial identity) seem impossible or suicidal. Resistance is also high (0.9) because the subjugated population, facing an existential threat, will resist fiercely, even if outmatched.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the dominant power bloc, this constraint is a necessary 'mountain' or 'rope' for survival, justifying all actions. From the subjugated population's perspective, it is a 'snare' of absolute extraction and oppression. International diplomats, attempting to apply 'rope' or 'scaffold' solutions, find themselves caught in a 'tangled rope' where their efforts are co-opted or rendered irrelevant by the underlying existential logic. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The dominant_power_bloc is a full beneficiary (d=0.0) as it achieves its perceived survival and identity goals through territorial control. The subjugated_population is a full target (d=1.0) as it bears the full cost of this zero-sum struggle. The international_law_framework is also a target (d=0.8) as its authority is undermined. International_diplomats are observers (d=0.5) but their efforts are constrained by the underlying dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as the 'founding problem' (existential threat) is perceived as perpetually 'live' by the dominant actors. The classification as a Snare prevents mislabeling this as a coordination problem or a degraded institution. The high theater ratio indicates that the 'mandate' is often a cover for ongoing extraction, rather than a genuine, atrophied function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_vs_juridical_primacy,
    'Is the primacy of existential claims over juridical claims an inherent feature of this conflict, or a strategic framing adopted by the dominant power bloc to justify extraction?',
    'Analysis of historical periods where juridical arguments held greater sway, or counterfactual scenarios where external guarantees of security altered the perceived existential stakes. Examination of internal discourse within the dominant bloc for evidence of strategic manipulation vs. genuine belief.',
    'If strategic, the constraint''s suppression and extractiveness are even more purely constructed, and the ''theater_ratio'' is higher, indicating a more cynical Snare. If inherent, the constraint is a more ''naturalized'' Snare, deeply embedded in the parties'' worldviews.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_vs_juridical_primacy, conceptual, 'Ambiguity regarding the origin and nature of the existential framing.').

omega_variable(
    identity_fusion_vs_instrumental_control,
    'To what extent is the ''identity_locked'' exit option for the dominant power bloc a genuine fusion of identity with territorial control, versus an instrumental claim used to maintain control and suppress dissent?',
    'Sociological and psychological studies of collective identity formation within the dominant bloc, compared with political science analysis of elite rhetoric and policy choices. Examination of internal dissent or alternative identity narratives.',
    'If primarily instrumental, the ''identity_locked'' status is a form of self-imposed constraint that could be more easily dislodged by shifts in political incentives, potentially lowering the effective suppression. If genuine, the constraint is more deeply entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_vs_instrumental_control, empirical, 'Distinguishing genuine identity fusion from instrumental identity claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__existential_matrix_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1948, 0.4).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1967, 0.5).
narrative_ontology:measurement(terr_tr_t1993, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1993, 0.6).
narrative_ontology:measurement(terr_tr_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2000, 0.65).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2024, 0.7).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1967, 0.8).
narrative_ontology:measurement(terr_be_t1993, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1993, 0.85).
narrative_ontology:measurement(terr_be_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2000, 0.88).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1967, 0.85).
narrative_ontology:measurement(terr_su_t1993, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1993, 0.9).
narrative_ontology:measurement(terr_su_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2000, 0.92).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__existential_matrix_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.08).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__self_determination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'territorial_sovereignty_legitimacy' kernel. This 'existential_matrix_reading' posits that sovereignty is fundamentally about collective survival and identity, making territorial control a zero-sum game, which directly influences and is influenced by the other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
