% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__sovereignty_maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__sovereignty_maximalist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: rbio_practice_norm_complex__sovereignty_maximalist_reading
 *   human_readable: Sovereignty Maximalist Reading of RBIO Norms
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   The sovereignty maximalist reading of the Rules-Based International Order
 *   (RBIO) holds that state sovereignty is absolute and non-derogable; RBIO
 *   norms are legitimate only insofar as they protect sovereignty against
 *   external interference; humanitarian exceptions (R2P, humanitarian
 *   intervention) are structurally pretexts for regime change. This reading
 *   instantiates a constraint: the non-intervention norm as operationalized
 *   through P5 veto power and customary law. The constraint coordinates by
 *   preventing great power conflict (genuine coordination function) but
 *   extracts by granting impunity to repressive regimes at the expense of
 *   trapped populations (asymmetric extraction). The reading claims this
 *   constraint is a Mountain (natural law of international order); the
 *   authored metrics describe a substantially extractive, actively enforced
 *   structure with identifiable beneficiaries and victims.
 *
 * KEY AGENTS:
 *   - authoritarian_regimes: Primary beneficiary (powerful/constrained) — gains non-interference guarantee for internal repression
 *   - trapped_populations_under_repression: Primary victim (powerless/trapped) — loses all external recourse, bears full cost of repression
 *   - p5_members_using_veto: Agenda setter and secondary beneficiary (institutional/arbitrage) — controls enforcement, deploys norm selectively
 *   - civil_society_actors_in_closed_states: Payer (moderate/identity_locked) — cannot exit without abandoning mission, denied international protection
 *   - liberal_institutional_actors: Excluded (organized/mobile) — operates R2P framework but categorized as illegitimate by maximalist reading
 *   - humanitarian_intervention_advocates: Excluded (moderate/constrained) — arguments treated as bad faith, face diplomatic retaliation
 *   - analytical_observers: Observer (analytical/analytical) — tracks full structure without stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.68).
domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.72).
domain_priors:theater_ratio(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__sovereignty_maximalist_reading, mountain).
narrative_ontology:human_readable(rbio_practice_norm_complex__sovereignty_maximalist_reading, "Sovereignty Maximalist Reading of RBIO Norms").
narrative_ontology:topic_domain(rbio_practice_norm_complex__sovereignty_maximalist_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__sovereignty_maximalist_reading).
domain_priors:emerges_naturally(rbio_practice_norm_complex__sovereignty_maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'dadf94d6-1ece-478a-8b90-9ec87f64ed1e').
narrative_ontology:cs_kernel_codification('dadf94d6-1ece-478a-8b90-9ec87f64ed1e', distributed).
narrative_ontology:cs_authority_grounding('dadf94d6-1ece-478a-8b90-9ec87f64ed1e', practice).
narrative_ontology:cs_interpretation_layer_present('dadf94d6-1ece-478a-8b90-9ec87f64ed1e').
narrative_ontology:cs_reading_relation('dadf94d6-1ece-478a-8b90-9ec87f64ed1e', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('dadf94d6-1ece-478a-8b90-9ec87f64ed1e', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('dadf94d6-1ece-478a-8b90-9ec87f64ed1e', foundational, absolute_state_sovereignty).
narrative_ontology:cs_axiom_status(absolute_state_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('dadf94d6-1ece-478a-8b90-9ec87f64ed1e', absolute_state_sovereignty, deontological).
narrative_ontology:cs_axiom('dadf94d6-1ece-478a-8b90-9ec87f64ed1e', foundational, non_intervention_as_peremptory_norm).
narrative_ontology:cs_axiom_status(non_intervention_as_peremptory_norm, holdable).
narrative_ontology:cs_axiom_grounding('dadf94d6-1ece-478a-8b90-9ec87f64ed1e', non_intervention_as_peremptory_norm, conventional).
narrative_ontology:cs_reference_frame('dadf94d6-1ece-478a-8b90-9ec87f64ed1e', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('dadf94d6-1ece-478a-8b90-9ec87f64ed1e', post_r2p_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dadf94d6-1ece-478a-8b90-9ec87f64ed1e', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, p5_members_using_veto).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, trapped_populations_under_repression).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, civil_society_actors_in_closed_states).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__sovereignty_maximalist_reading, westphalian_sovereignty_principle).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__sovereignty_maximalist_reading, non_intervention_as_customary_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain unambiguous legal cover against external pressure for human rights violations. The sovereignty maximalist norm lets them treat any external criticism or sanctioning as illegitimate interference in domestic affairs. They actively invoke this reading in UN forums and bilateral diplomacy to shield internal repression.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes, beneficiary,
    powerful, biographical, constrained, national).

% Bear the full cost of repression with no external recourse. When the sovereignty maximalist reading prevails, R2P and humanitarian intervention doctrines are neutralized, leaving populations at the mercy of their own governments. Exit is physically prevented (border controls) and politically blocked (no asylum pathways).
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, trapped_populations_under_repression, payer,
    powerless, biographical, trapped, local).

% Control the enforcement machinery of the RBIO through Security Council veto power. They deploy the sovereignty maximalist reading selectively — invoking it to block interventions against allies or themselves, while ignoring it when authorizing interventions against adversaries. The veto makes the norm operationally un-amendable.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, p5_members_using_veto, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__sovereignty_maximalist_reading, p5_members_using_veto, beneficiary).

% Human rights defenders, journalists, and opposition figures who cannot exit without abandoning their constituencies and life's work. The sovereignty maximalist norm denies them international protective mechanisms. Their identity is fused to the domestic struggle; exit means functional death of their mission.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, civil_society_actors_in_closed_states, payer,
    moderate, biographical, identity_locked, national).

% States, NGOs, and international officials who operate the R2P/humanitarian intervention framework. They are structurally excluded from the sovereignty maximalist reading's legitimate conversation — their vocabulary (R2P, protection mandates) is treated as category error rather than competing claim.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_institutional_actors, excluded,
    organized, generational, mobile, global).

% Activist networks, legal scholars, and mid-power states that argue for intervention authority. Their exclusion is active: the maximalist reading frames their position as inherently illegitimate (regime change pretext), not merely mistaken. They face diplomatic retaliation when pushing intervention agendas.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, humanitarian_intervention_advocates, excluded,
    moderate, biographical, constrained, global).

% Scholars of international law and IR theorists who track the normative contest. They see the full structure: how the maximalist reading functions as both a genuine coordination mechanism (preventing great power war) and an extraction shield for repressive regimes. They have no stake in the outcome.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents great power conflict by establishing a bright-line rule: no military intervention across borders except self-defense. This solved the founding problem of interstate war among major powers by making sovereignty the supreme organizing principle.
% TRANSFER_FUNCTION: Moves political survival and impunity for internal repression from vulnerable populations to incumbent regimes, using the veto-wielding great powers as the enforcement guarantors. The transfer runs: trapped populations lose external recourse → authoritarian regimes gain non-interference guarantee → P5 members retain veto control over any exception.
% ABSENT_VOICES: Trapped populations and civil society actors in closed states are the primary excluded voices — they would object to the non-intervention rule but have no access to the forums where the norm is adjudicated (UNSC, diplomatic channels). Humanitarian intervention advocates are formally present but substantively excluded: their arguments are categorized as bad faith rather than engaged.
% DISAPPEARANCE_RATIONALE: If the sovereignty maximalist norm vanished overnight, the R2P framework would become the default interpretive lens for Security Council action. Authoritarian regimes would lose their primary legal shield. Intervention coalitions would form more readily. The veto power would still exist but its normative justification would shift. The international order would reorganize around conditional sovereignty.
% FOUNDING_PROBLEM: The post-WWII order needed a principle to prevent the interstate wars that had devastated Europe twice in thirty years. Absolute sovereignty — the Westphalian bargain upgraded with UN Charter enforcement — was the solution: states agreed not to interfere in each other's internals, backed by great power veto to prevent the mechanism from being weaponized.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (preventing great power war) is attested as still live by realist IR scholars (Mearsheimer, Waltz tradition) and by P5 foreign ministries in official doctrine. It is attested as substantially solved by liberal institutionalists (Keohane, Ikenberry) and R2P advocates who argue great power war is obsolete and the norm now serves only to shield repression. No neutral arbiter exists; the corroboration split maps exactly to the reading contest.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__sovereignty_maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__sovereignty_maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, ExtMetricName, E),
    domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(rbio_practice_norm_complex__sovereignty_maximalist_reading),
    narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the norm transfers survival probability from populations to regimes without compensation. Suppression (0.72) is higher because the norm's persistence depends on active veto enforcement and diplomatic delegitimization of alternatives — not on participant consent. Theater ratio (0.42) reflects that the great power war prevention function is real but declining in relevance (nuclear deterrence does more work), while the repression-shielding function grows. Accessibility collapse (0.61) is moderate: alternative norms (R2P, conditional sovereignty) exist and are articulated but cannot overcome the veto barrier. Resistance (0.58) is substantial: the norm faces continuous contestation from liberal institutionalists, R2P advocates, and affected populations, but the veto structure contains it.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (P5 members) experiences the constraint as a Mountain: it is the bedrock of their institutional power, appears natural and unchangeable, and they administer it. The beneficiary seat (authoritarian regimes) experiences it as a Rope: a coordination mechanism that genuinely solves their security dilemma (external regime change) with minimal coercive overhead on them. The payer seats (trapped populations, civil society) experience it as a Snare: pure extraction with no coordination benefit for them, maintained by coercion (veto) and exit suppression. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regimes and P5 veto-users are structural beneficiaries (d near 0.0): they collect the rents of non-interference and control the enforcement machinery. Trapped populations and civil society actors are structural targets (d near 1.0): they bear the extraction with trapped/identity_locked exit. Liberal institutional actors and intervention advocates are excluded (d undefined): they are not coordinated by the constraint but actively suppressed by its categorization logic. The analytical observer sits at d=0.5 (symmetric). The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing great power war) is contested: realists say it's live; liberal institutionalists say it's solved by nuclear deterrence and economic interdependence. The constraint persists despite the contested founding problem because the veto-wielding agenda setters benefit from the arrangement's current form. This is not classic mandatrophy (where the original function atrophies but the form remains) — it is a contested mandate where the agenda setters actively maintain the form because it serves their current interests, not just inertia. The mandate has not atrophied; it has been captured.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'At which structural element do the three readings of the RBIO kernel genuinely disagree: the kernel''s codification, the authority grounding, the interpretation layer, or the axioms?',
    'Structural mapping of each reading''s cs_structure declarations. If all three declare different authority_grounding values, the disagreement is at the authority layer. If they share authority_grounding but differ on axioms, the disagreement is at the normative premise layer.',
    'If disagreement is at authority layer, the kernel has no stable adjudicator — drift is irresolvable. If at axiom layer, empirical or normative resolution may be possible. Determines whether the kernel contest is structural (forecloses) or discursive (coexists_with).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locating the structural disagreement within the kernel''s commitment-system anatomy.').

omega_variable(
    sovereignty_naturalness_vs_construction,
    'Is the absolute sovereignty norm a genuine natural law of international order (Mountain) or a constructed constraint that benefits identifiable agents (Tangled Rope/Snare)?',
    'Counterfactual: if P5 veto were reformed and R2P operationalized without great power war, the natural law claim fails. Historical: track whether great power war prevention correlates with sovereignty absolutism or with nuclear deterrence/alliances.',
    'If natural law, the constraint is a genuine Mountain (FSM does not fire). If constructed, FSM reclassifies to tangled_rope (coordination + extraction) or snare (pure extraction). The beneficiary declarations (authoritarian_regimes, p5_members) make this a live FSM candidate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_naturalness_vs_construction, empirical, 'The core naturalness ambiguity that drives false summit detection for this Mountain-claimed constraint.').

omega_variable(
    coordination_extraction_separability,
    'Can the great power war prevention function be separated from the repression-shielding function, or are they structurally inseparable in the veto mechanism?',
    'Natural experiment: observe Security Council behavior on non-repression threats (pandemics, climate, asteroids). If veto is used only on repression/intervention items, functions are separable. If veto extends to all collective action, they are coupled.',
    'If separable, the constraint is a Tangled Rope (genuine coordination + asymmetric extraction). If inseparable, it may be a Scaffold (transitional) or Piton (degraded). Determines whether reform can preserve coordination while removing extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether the constraint''s coordination and extraction components are structurally separable — the Tangled Rope gate.').

omega_variable(
    identity_lock_mechanism_civil_society,
    'What specific identity-fusion mechanism binds civil society actors in closed states to the domestic struggle, making exit functionally impossible?',
    'Qualitative study of human rights defenders who stayed vs. fled: professional identity (mandate requires presence), relational identity (constituency would collapse), ideological identity (cause constitutes self), or institutional identity (organization cannot operate in exile).',
    'If professional/relational, exit_options might shift with technology (remote advocacy). If ideological/institutional, identity_locked is structural. Affects directionality computation for this seat and thus effective extraction χ.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_civil_society, empirical, 'Identity-lock dynamics for civil society actors — determines whether their exit_options are structurally fixed or context-dependent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__sovereignty_maximalist_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_sovmax_tr_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(rbio_sovmax_tr_t1960, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(rbio_sovmax_tr_t1975, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement(rbio_sovmax_tr_t1990, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(rbio_sovmax_tr_t2000, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(rbio_sovmax_tr_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(rbio_sovmax_tr_t2011, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2011, 0.38).
narrative_ontology:measurement(rbio_sovmax_tr_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(rbio_sovmax_tr_t2020, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement(rbio_sovmax_tr_t2025, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(rbio_sovmax_be_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1945, 0.25).
narrative_ontology:measurement(rbio_sovmax_be_t1960, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1960, 0.3).
narrative_ontology:measurement(rbio_sovmax_be_t1975, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1975, 0.35).
narrative_ontology:measurement(rbio_sovmax_be_t1990, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(rbio_sovmax_be_t2000, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(rbio_sovmax_be_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(rbio_sovmax_be_t2011, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2011, 0.6).
narrative_ontology:measurement(rbio_sovmax_be_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement(rbio_sovmax_be_t2020, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(rbio_sovmax_be_t2025, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(rbio_sovmax_su_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1945, 0.3).
narrative_ontology:measurement(rbio_sovmax_su_t1960, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1960, 0.35).
narrative_ontology:measurement(rbio_sovmax_su_t1975, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1975, 0.4).
narrative_ontology:measurement(rbio_sovmax_su_t1990, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(rbio_sovmax_su_t2000, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(rbio_sovmax_su_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2005, 0.62).
narrative_ontology:measurement(rbio_sovmax_su_t2011, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2011, 0.66).
narrative_ontology:measurement(rbio_sovmax_su_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2015, 0.69).
narrative_ontology:measurement(rbio_sovmax_su_t2020, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement(rbio_sovmax_su_t2025, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__sovereignty_maximalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.1).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, r2p_operationalization).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, unsc_veto_reform_proposals).

% DUAL FORMULATION NOTE:
% This story is one member of the rbio_practice_norm_complex constraint family. The three readings (sovereignty_maximalist_reading, liberal_institutional_reading, hegemonic_extraction_reading) share the kernel but instantiate different constraints with different ε values, beneficiary/victim structures, and claimed types. The sovereignty maximalist reading claims Mountain with ε=0.68; the liberal institutional reading likely claims Rope/Scaffold with lower ε; the hegemonic extraction reading likely claims Snare/Tangled Rope with higher ε. The ε-invariance principle requires separate stories because the kernel label 'RBIO' conflates structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__sovereignty_maximalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(rbio_practice_norm_complex__sovereignty_maximalist_reading, powerless, 0.95).
constraint_indexing:directionality_override(rbio_practice_norm_complex__sovereignty_maximalist_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
