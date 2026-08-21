% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__absolute_non_intervention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__absolute_non_intervention, []).

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
 *   constraint_id: westphalia_sovereignty__absolute_non_intervention
 *   human_readable: Westphalian Sovereignty: Absolute Non-Intervention
 *   domain: international_relations/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'absolute non-intervention' reading of
 *   Westphalian sovereignty, where external interference in a state's
 *   domestic affairs is deemed illegitimate regardless of internal conduct.
 *   It functions as a high barrier to intervention, effectively shielding
 *   state elites and authoritarian regimes from external accountability for
 *   internal actions, including mass atrocities. The victim set explicitly
 *   excludes populations under authoritarian control, as their suffering is
 *   framed as an internal matter. The claimed type is 'tangled_rope' because
 *   it provides a coordination function (stable state system) but also
 *   enables significant extraction (from vulnerable populations) through its
 *   asymmetric application.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, 0.65).
domain_priors:suppression_score(westphalia_sovereignty__absolute_non_intervention, 0.8).
domain_priors:theater_ratio(westphalia_sovereignty__absolute_non_intervention, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, extractiveness, 0.65).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__absolute_non_intervention, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__absolute_non_intervention, "Westphalian Sovereignty: Absolute Non-Intervention").
narrative_ontology:topic_domain(westphalia_sovereignty__absolute_non_intervention, "international_relations/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__absolute_non_intervention).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__absolute_non_intervention, '1b39dae9-6177-4691-875e-54fdffdc6243').
narrative_ontology:cs_kernel_codification('1b39dae9-6177-4691-875e-54fdffdc6243', formalized).
narrative_ontology:cs_authority_grounding('1b39dae9-6177-4691-875e-54fdffdc6243', lineage).
narrative_ontology:cs_interpretation_layer_present('1b39dae9-6177-4691-875e-54fdffdc6243').
narrative_ontology:cs_reading_relation('1b39dae9-6177-4691-875e-54fdffdc6243', westphalia_sovereignty__conditional_responsibility, forecloses).
narrative_ontology:cs_reading_relation('1b39dae9-6177-4691-875e-54fdffdc6243', westphalia_sovereignty__graded_sovereignty, forecloses).
narrative_ontology:cs_axiom('1b39dae9-6177-4691-875e-54fdffdc6243', foundational, territorial_integrity_absolute).
narrative_ontology:cs_axiom_status(territorial_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('1b39dae9-6177-4691-875e-54fdffdc6243', territorial_integrity_absolute, conventional).
narrative_ontology:cs_axiom('1b39dae9-6177-4691-875e-54fdffdc6243', foundational, domestic_jurisdiction_exclusive).
narrative_ontology:cs_axiom_status(domestic_jurisdiction_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('1b39dae9-6177-4691-875e-54fdffdc6243', domestic_jurisdiction_exclusive, conventional).
narrative_ontology:cs_reference_frame('1b39dae9-6177-4691-875e-54fdffdc6243', post_westphalian_state_system).
narrative_ontology:cs_drift_state('1b39dae9-6177-4691-875e-54fdffdc6243', post_cold_war_humanitarian_intervention_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1b39dae9-6177-4691-875e-54fdffdc6243', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, state_elites_claiming_territorial_monopoly).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, authoritarian_regimes).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_control).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, international_human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These elites assert the absolute right to govern their territory without external interference, regardless of internal conduct. They benefit from the constraint by maintaining power and control, often suppressing internal dissent under the guise of sovereignty. They actively enforce this principle in international forums.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, state_elites_claiming_territorial_monopoly, agenda_setter,
    institutional, generational, arbitrage, global).

% These regimes rely on the absolute non-intervention principle to shield their internal policies, including human rights abuses, from international scrutiny and action. They are direct beneficiaries of the high barrier to intervention this reading creates.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, authoritarian_regimes, beneficiary,
    institutional, biographical, constrained, national).

% These populations bear the direct costs of the absolute non-intervention principle, as it legitimizes their governments' actions and prevents external intervention even in cases of mass atrocities. Their options for relief are severely limited by this constraint.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_control, payer,
    powerless, immediate, trapped, local).

% These advocates work to protect human rights globally but find their efforts constrained by the absolute non-intervention principle. They bear the cost of being unable to effectively intervene or compel action against states committing atrocities, often facing diplomatic and legal barriers.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, international_human_rights_advocates, payer,
    organized, generational, constrained, global).

% These states often publicly uphold human rights but are bound by the principle of non-intervention, leading to a tension between their values and international legal obligations. They observe and debate the application of the principle, sometimes seeking to find loopholes or alternative justifications for intervention.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, liberal_democracies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, universal rule for state interaction, preventing constant conflict over internal affairs and providing a stable framework for diplomatic relations and international law.
% TRANSFER_FUNCTION: Transfers the absolute right to internal governance from any potential external oversight to the state elites, effectively transferring the burden of internal conduct onto the populations governed by those elites.
% ABSENT_VOICES: Victims of state-sponsored violence and human rights abuses are structurally excluded from the international conversation about intervention, as their suffering is deemed an 'internal affair.'
% DISAPPEARANCE_RATIONALE: If the principle of absolute non-intervention vanished, the international system would undergo a profound rearrangement. States would face immediate pressure regarding their internal conduct, potentially leading to widespread interventions, new forms of international governance, or increased instability as the foundational rules of state interaction are rewritten.
% FOUNDING_PROBLEM: The problem of incessant warfare and interference in the internal affairs of states, particularly in post-Reformation Europe, leading to instability and conflict.
% FOUNDING_PROBLEM_CORROBORATION: Many states, particularly those with a strong emphasis on national sovereignty, attest that the problem of external interference remains live and that the principle is essential for global stability. Critics, however, argue that the problem has evolved to include state-sponsored violence against populations, which the principle now exacerbates rather than solves.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__absolute_non_intervention, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__absolute_non_intervention, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__absolute_non_intervention, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(westphalia_sovereignty__absolute_non_intervention, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__absolute_non_intervention, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__absolute_non_intervention_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__absolute_non_intervention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the principle allows state elites to maintain power and control without external checks, often at the expense of their own populations. Suppression is very high (0.80) as the constraint actively prevents external actors from challenging internal state power, effectively suppressing alternatives for oppressed populations. Theater ratio is moderate (0.20) as the principle is genuinely invoked for diplomatic stability, but also performatively to deflect criticism of internal abuses. Accessibility collapse is high (0.70) because the principle severely limits the options for external intervention, making alternatives for victims difficult to access. Resistance is moderate (0.40) from human rights advocates and some liberal states, but it is often ineffective against the entrenched norm.
 *
 * PERSPECTIVAL GAP:
 *   State elites perceive this as a legitimate and necessary coordination mechanism for global order, ensuring their autonomy. Populations under authoritarian control, however, experience it as a snare that traps them under oppressive regimes. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State elites and authoritarian regimes are clear beneficiaries (d near 0.0) as the constraint protects their power and autonomy. Populations under authoritarian control and international human rights advocates are targets (d near 1.0) as they bear the costs of non-intervention. Liberal democracies, while sometimes advocating for human rights, are also bound by the principle, placing them in a more complex, observer-like role.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to prevent interstate conflict arising from internal interference. While this function persists, the 'absolute non-intervention' reading has allowed it to be co-opted to protect regimes engaged in mass atrocities. The classification as a tangled_rope prevents mislabeling it as pure coordination by highlighting the asymmetric extraction from vulnerable populations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_norm,
    'Is the principle of absolute non-intervention a fundamental, natural law of international relations, or a constructed norm that benefits identifiable state actors?',
    'Historical analysis of the evolution of international law and political theory, examining periods where intervention was more common or justified, and identifying the interests served by the norm''s entrenchment.',
    'If a constructed norm, its extractiveness and suppression are more clearly attributable to human agency and power dynamics, supporting its classification as a tangled_rope. If a natural law, its classification would lean towards mountain, but the presence of beneficiaries would still trigger FSM.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_norm, conceptual, 'Ambiguity regarding the inherent ''naturalness'' of the non-intervention principle.').

omega_variable(
    coordination_vs_extraction_balance,
    'What is the optimal balance between state stability (coordination) and accountability for internal conduct (extraction prevention)?',
    'Empirical studies comparing long-term stability and human rights outcomes in regions with varying intervention norms, alongside normative debate on the ethical limits of sovereignty.',
    'A shift in the perceived optimal balance could lead to a re-evaluation of the constraint''s legitimacy, potentially reducing its extractiveness and suppression if accountability is prioritized, or increasing it if stability is paramount.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_balance, preference, 'The normative trade-off between state stability and human rights accountability.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternatives for oppressed populations structural (external barriers) or internalized (lack of belief in external help)?',
    'Post-intervention trajectory: if suppression persists after the extractive mechanism is removed (e.g., after a successful humanitarian intervention), reclassify as partially internalized. This would require observing the psychological and social effects on populations after external barriers are lifted.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more resilient to external pressure. This would deepen the ''snare'' aspect of the tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for populations under authoritarian control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__absolute_non_intervention, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0, 0.15).
narrative_ontology:measurement(west_tr_t10, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 10, 0.16).
narrative_ontology:measurement(west_tr_t20, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 20, 0.17).
narrative_ontology:measurement(west_tr_t30, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 30, 0.18).
narrative_ontology:measurement(west_tr_t40, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 40, 0.19).
narrative_ontology:measurement(west_tr_t50, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 50, 0.2).
narrative_ontology:measurement(west_tr_t60, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 60, 0.2).
narrative_ontology:measurement(west_tr_t70, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 70, 0.2).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(west_be_t10, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(west_be_t20, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(west_be_t30, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(west_be_t40, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(west_be_t50, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 50, 0.64).
narrative_ontology:measurement(west_be_t60, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(west_be_t70, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 70, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(west_su_t10, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(west_su_t20, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(west_su_t30, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(west_su_t40, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(west_su_t50, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 50, 0.79).
narrative_ontology:measurement(west_su_t60, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 60, 0.8).
narrative_ontology:measurement(west_su_t70, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 70, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__absolute_non_intervention, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, international_criminal_court_jurisdiction).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, un_security_council_veto_power).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'westphalia_sovereignty' kernel. It represents the absolute non-interventionist interpretation, distinct from conditional_responsibility and graded_sovereignty, which offer different justifications and thresholds for external intervention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
