% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contraction_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: total_war_reachability_boundary__contraction_reading
 *   human_readable: Total War Reachability Boundary (Contraction Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint represents the 'contraction reading' of the total war
 *   reachability boundary kernel. It posits that the advent of nuclear
 *   weapons fundamentally and irreversibly contracted the strategic space,
 *   making 'winnable total war' a physical impossibility due to the certainty
 *   of Mutual Assured Destruction (MAD). This reading classifies the boundary
 *   as a Mountain, an unchangeable feature of the strategic landscape, with
 *   universal victims (the human species and global ecosystems) and no
 *   beneficiaries, as no actor can 'win' under these conditions. The low
 *   extractiveness reflects that no party collects rents from this
 *   'constraint'; rather, it imposes a universal, unavoidable cost of
 *   existential risk. The high suppression reflects the physical reality of
 *   nuclear destruction, which suppresses the option of total war.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contraction_reading, 0.05).
domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, 0.98).
domain_priors:theater_ratio(total_war_reachability_boundary__contraction_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, 0.99).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contraction_reading, mountain).
narrative_ontology:human_readable(total_war_reachability_boundary__contraction_reading, "Total War Reachability Boundary (Contraction Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contraction_reading, 'adfd82b6-6669-45e2-b610-130cfd7392f2').
narrative_ontology:cs_kernel_codification('adfd82b6-6669-45e2-b610-130cfd7392f2', implicit).
narrative_ontology:cs_authority_grounding('adfd82b6-6669-45e2-b610-130cfd7392f2', self_enforcing).
narrative_ontology:cs_reading_relation('adfd82b6-6669-45e2-b610-130cfd7392f2', total_war_reachability_boundary__dropping_reading, coexists_with).
narrative_ontology:cs_reading_relation('adfd82b6-6669-45e2-b610-130cfd7392f2', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_axiom('adfd82b6-6669-45e2-b610-130cfd7392f2', foundational, mutual_assured_destruction_is_absolute).
narrative_ontology:cs_axiom_status(mutual_assured_destruction_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('adfd82b6-6669-45e2-b610-130cfd7392f2', mutual_assured_destruction_is_absolute, empirically_contingent).
narrative_ontology:cs_axiom('adfd82b6-6669-45e2-b610-130cfd7392f2', foundational, winnable_total_war_is_a_logical_impossibility).
narrative_ontology:cs_axiom_status(winnable_total_war_is_a_logical_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('adfd82b6-6669-45e2-b610-130cfd7392f2', winnable_total_war_is_a_logical_impossibility, deontological).
narrative_ontology:cs_reference_frame('adfd82b6-6669-45e2-b610-130cfd7392f2', post_nuclear_strategic_reality).
narrative_ontology:cs_drift_state('adfd82b6-6669-45e2-b610-130cfd7392f2', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('adfd82b6-6669-45e2-b610-130cfd7392f2', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, human_species).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, global_ecosystems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Faces existential threat from nuclear war, with no means to opt out of the consequences. Bears the ultimate cost of the contracted strategic space.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, human_species, payer,
    powerless, civilizational, trapped, universal).

% Would suffer irreversible damage and collapse in the event of nuclear war, with no agency to prevent or mitigate the outcome.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, global_ecosystems, payer,
    powerless, civilizational, trapped, universal).

% Possess the means to initiate nuclear war but are also constrained by the certainty of mutual destruction. Their strategic choices are fundamentally altered by the unreachability of total victory.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, nuclear_powers, agenda_setter,
    institutional, generational, constrained, global).

% Analyze the implications of nuclear weapons for international relations and the feasibility of various conflict scenarios. Their work is shaped by the premise that total war is no longer a rational option.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, strategic_theorists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a de facto coordination around mutual restraint, as no party can achieve a meaningful victory in total war. This 'coordination' is a consequence of the physical reality, not an active agreement.
% TRANSFER_FUNCTION: Transfers the possibility of 'winnable total war' out of the strategic feasible set for all actors, imposing a universal cost of existential risk.
% ABSENT_VOICES: Historical military strategists who planned for total victory in conventional terms; their strategic frameworks are rendered obsolete by the nuclear reality. Future generations, who inherit this contracted strategic space.
% DISAPPEARANCE_RATIONALE: If the nuclear-induced contraction of strategic space vanished (e.g., through a technological breakthrough rendering nuclear weapons obsolete and harmless), the nature of international conflict and grand strategy would fundamentally rearrange, potentially re-introducing the concept of winnable total war.
% FOUNDING_PROBLEM: The problem of preventing global catastrophic war in an era of unprecedented destructive capability.
% FOUNDING_PROBLEM_CORROBORATION: The continued existence of nuclear arsenals and the ongoing discourse around deterrence theory, supported by international treaties and non-proliferation efforts, corroborate that the problem remains live. This is attested by international organizations, non-governmental peace organizations, and scientific bodies, not just nuclear powers.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_reachability_boundary__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contraction_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_reachability_boundary__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is near zero because no actor benefits from the unreachability of total war; rather, all are subject to the existential risk. Suppression is near maximal because the physical reality of nuclear destruction makes total war an unfeasible option, regardless of political will. Theater ratio is minimal as there is little performative maintenance; the constraint is a brute fact. Accessibility collapse is near total, as the option of winnable total war is effectively removed. Resistance is minimal because the physical reality is not something that can be 'resisted' in a meaningful strategic sense, only acknowledged and managed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of nuclear powers, the constraint is a fundamental limit on their strategic options, forcing a shift towards deterrence. From the perspective of the human species, it is an existential threat that has fundamentally altered the trajectory of civilization. The classification as a Mountain holds across these perspectives because the physical reality of MAD is invariant.
 *
 * DIRECTIONALITY LOGIC:
 *   All actors, particularly the human species and global ecosystems, are universal targets (d=1.0) of the existential risk imposed by this boundary. Nuclear powers, while possessing the weapons, are also constrained by this reality, making them targets of the contracted strategic space, even if they are the 'agenda setters' of nuclear policy. There are no beneficiaries in this reading, as no one 'wins' from the unreachability of total war.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, as a Mountain, is not subject to mandatrophy in the traditional sense, as its function (preventing winnable total war) is a direct consequence of physical reality, not a human mandate. The 'mandate' is effectively 'do not commit species suicide,' which remains live as long as nuclear weapons exist. The classification prevents mislabeling this physical reality as a human-constructed constraint that could atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contingent_vs_permanent_contraction,
    'Is the contraction of strategic space a permanent feature (Mountain) or contingent on current technology (Piton, reversible)?',
    'Future technological developments: if anti-missile defense or other technologies render nuclear weapons obsolete without creating new existential threats, the constraint might reclassify as a Piton or even disappear.',
    'If contingent, the constraint''s classification would shift from Mountain to Piton, indicating an atrophied function that could be reversed, with significant implications for strategic planning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contingent_vs_permanent_contraction, empirical, 'Whether the nuclear-induced strategic contraction is permanent or technologically reversible.').

omega_variable(
    deterrence_as_rope_vs_mountain,
    'Is nuclear deterrence a coordination equilibrium (Rope) that could fail, or a physical impossibility of victory (Mountain)?',
    'Analysis of historical near-misses and theoretical models of escalation: if these demonstrate a genuine, non-trivial probability of total war despite MAD, it would support the ''dropping_reading'' (Rope).',
    'If deterrence is a Rope, it implies human agency and coordination are central, and the constraint is not a fixed physical limit, but a managed equilibrium, making it vulnerable to miscalculation or breakdown.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_as_rope_vs_mountain, conceptual, 'The fundamental nature of nuclear deterrence: coordination or physical limit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contraction_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__contraction_reading, theater_ratio, 1945, 0.01).
narrative_ontology:measurement(tota_tr_t1955, total_war_reachability_boundary__contraction_reading, theater_ratio, 1955, 0.01).
narrative_ontology:measurement(tota_tr_t1965, total_war_reachability_boundary__contraction_reading, theater_ratio, 1965, 0.01).
narrative_ontology:measurement(tota_tr_t1985, total_war_reachability_boundary__contraction_reading, theater_ratio, 1985, 0.01).
narrative_ontology:measurement(tota_tr_t2005, total_war_reachability_boundary__contraction_reading, theater_ratio, 2005, 0.01).
narrative_ontology:measurement(tota_tr_t2024, total_war_reachability_boundary__contraction_reading, theater_ratio, 2024, 0.01).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1945, 0.01).
narrative_ontology:measurement(tota_be_t1955, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1955, 0.03).
narrative_ontology:measurement(tota_be_t1965, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1965, 0.05).
narrative_ontology:measurement(tota_be_t1985, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1985, 0.05).
narrative_ontology:measurement(tota_be_t2005, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2005, 0.05).
narrative_ontology:measurement(tota_be_t2024, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1945, 0.1).
narrative_ontology:measurement(tota_su_t1955, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1955, 0.5).
narrative_ontology:measurement(tota_su_t1965, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1965, 0.9).
narrative_ontology:measurement(tota_su_t1985, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1985, 0.98).
narrative_ontology:measurement(tota_su_t2005, total_war_reachability_boundary__contraction_reading, suppression_requirement, 2005, 0.98).
narrative_ontology:measurement(tota_su_t2024, total_war_reachability_boundary__contraction_reading, suppression_requirement, 2024, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, nuclear_proliferation_regime).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, arms_control_treaties).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, conventional_warfare_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_reachability_boundary' kernel. This 'contraction_reading' asserts a permanent, Mountain-like contraction of strategic space. The 'dropping_reading' views total war as merely less probable (Rope), and the 'contingent_reachability_reading' sees the current state as a reversible Piton.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
