% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__structural_contraction_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: total_war_winnability_post1945__structural_contraction_reading
 *   human_readable: Structural Contraction of Total War Under Mutual Assured Destruction
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This story instantiates the structural_contraction_reading of the
 *   total_war_winnability_post1945 kernel: the claim that nuclear weapons did
 *   not persuade great powers to abandon total war as a norm, and did not
 *   merely shift elite strategic discourse away from it, but physically
 *   removed it from the reachable strategy space. A state contemplating total
 *   war against another nuclear-armed state cannot execute a winning total
 *   war — survivable second-strike capability guarantees mutual destruction
 *   regardless of intent, doctrine, or normative commitment. This is authored
 *   as a Mountain: the barrier holds independent of any party's belief in it,
 *   requires no enforcement apparatus, and would hold even if every relevant
 *   treaty and norm were repealed tomorrow, so long as the underlying physics
 *   (yield, delivery reliability, detection) remained intact. Sibling
 *   readings — normative_reading_drop (total war remains physically possible
 *   but is now illegitimate under international law) and
 *   strategic_culture_drift (total war remains reachable but fell out of
 *   elite discourse) — are NOT part of this file; they are separate
 *   constraints with their own epsilon values, evaluated elsewhere. The ε
 *   here is stable and low because the claim under evaluation is exclusively
 *   the physical-reachability claim, not the legal or discursive claims the
 *   other readings evaluate.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: primary beneficiary of the structural barrier (institutional/trapped) — cannot exit the deterrence condition even if they wished to
 *   - global_civilian_population: diffuse beneficiary (powerless/trapped) — benefits from the removed possibility without any agency in producing it
 *   - counterfactual_exchange_populations: hypothetical/non-agent victim class whose non-occurrence is the constraint's content
 *   - strategic_studies_analysts: analytical observers who study but do not administer the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__structural_contraction_reading, 0.04).
domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, 0.02).
domain_priors:theater_ratio(total_war_winnability_post1945__structural_contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, extractiveness, 0.04).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__structural_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_winnability_post1945__structural_contraction_reading, "Structural Contraction of Total War Under Mutual Assured Destruction").
narrative_ontology:topic_domain(total_war_winnability_post1945__structural_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__structural_contraction_reading, '955f831f-24b0-4a07-a679-69a5b27c7aff').
narrative_ontology:cs_kernel_codification('955f831f-24b0-4a07-a679-69a5b27c7aff', implicit).
narrative_ontology:cs_authority_grounding('955f831f-24b0-4a07-a679-69a5b27c7aff', none).
narrative_ontology:cs_reading_relation('955f831f-24b0-4a07-a679-69a5b27c7aff', total_war_winnability_post1945__normative_reading_drop, influences).
narrative_ontology:cs_reading_relation('955f831f-24b0-4a07-a679-69a5b27c7aff', total_war_winnability_post1945__strategic_culture_drift, influences).
narrative_ontology:cs_axiom('955f831f-24b0-4a07-a679-69a5b27c7aff', foundational, physical_second_strike_survivability_forecloses_victory).
narrative_ontology:cs_axiom_status(physical_second_strike_survivability_forecloses_victory, holdable).
narrative_ontology:cs_axiom_grounding('955f831f-24b0-4a07-a679-69a5b27c7aff', physical_second_strike_survivability_forecloses_victory, empirically_contingent).
narrative_ontology:cs_axiom('955f831f-24b0-4a07-a679-69a5b27c7aff', secondary, structural_foreclosure_requires_no_normative_consent).
narrative_ontology:cs_axiom_status(structural_foreclosure_requires_no_normative_consent, holdable).
narrative_ontology:cs_axiom_grounding('955f831f-24b0-4a07-a679-69a5b27c7aff', structural_foreclosure_requires_no_normative_consent, empirically_contingent).
narrative_ontology:cs_reference_frame('955f831f-24b0-4a07-a679-69a5b27c7aff', pre_nuclear_total_war_reachability).
narrative_ontology:cs_drift_state('955f831f-24b0-4a07-a679-69a5b27c7aff', post_1945_deterrence_equilibrium, gap(stable, severe, true)).
narrative_ontology:cs_created_at('955f831f-24b0-4a07-a679-69a5b27c7aff', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__structural_contraction_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__structural_contraction_reading, global_civilian_population).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__structural_contraction_reading, mutual_assured_destruction_stability_thesis).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__structural_contraction_reading, nuclear_revolution_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess arsenals whose second-strike survivability makes any attempt at total war against another nuclear power certain to end in the attacker's own annihilation. They did not choose this outcome as policy; it is a physical consequence of yield, delivery reliability, and detection latency. They cannot exit the condition by declaring war differently — the retaliatory physics does not consult intent.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, nuclear_weapon_states, beneficiary,
    institutional, civilizational, trapped, global).

% Benefit from the fact that great-power total war — the kind that produced tens of millions of deaths in 1939-45 — is no longer a live strategic option for any nuclear-armed state against another. This benefit accrues without their participation, negotiation, or consent; it is a side effect of the weapons' existence, not a policy extended to them.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, global_civilian_population, beneficiary,
    powerless, civilizational, trapped, global).

% A hypothetical victim class: the populations that would bear the cost if the structural barrier failed (accident, miscalculation, breakdown of second-strike survivability). They are not victims of this constraint's operation — they are the counterfactual class whose non-occurrence is the entire content of the constraint holding. Included for completeness of the causal structure, not because the constraint currently extracts from them.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, counterfactual_exchange_populations, excluded,
    powerless, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(total_war_winnability_post1945__structural_contraction_reading, counterfactual_exchange_populations).

% Study the deterrence architecture, model first-strike/second-strike stability, and debate whether the contraction is durable (technology-dependent, reversible with missile defense breakthroughs) or a permanent structural feature of high-yield thermonuclear physics.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, strategic_studies_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the ordinary sense — this is not a negotiated coordination mechanism but a physical foreclosure. No treaty, norm, or institution is required to sustain it; it holds because the destructive yield and retaliatory reliability of nuclear arsenals make total war unwinnable by construction, not because parties agreed to refrain.
% TRANSFER_FUNCTION: Nothing is transferred between parties by this constraint's operation. Unlike a coordination or extraction structure, there is no rent, tribute, or resource flow — the constraint is the absence of a previously reachable state (winnable total war), not a redistribution.
% ABSENT_VOICES: Populations in the counterfactual exchange scenario have no voice because they do not exist in the actual world — they are the class whose absence is precisely what the constraint guarantees. There is no exclusion mechanism to critique; there is nothing to exclude them from.
% DISAPPEARANCE_RATIONALE: If nuclear weapons' physical deterrent effect vanished overnight (arsenals disarmed or rendered non-functional simultaneously across all nuclear states), the strategic calculus that has held since 1945 would revert: total war between great powers would re-enter the reachable space as a live strategic option, and military planning, alliance structures, and force postures would reorganize around that reopened possibility within a single planning cycle.
% FOUNDING_PROBLEM: Prior to 1945, total war between great powers was a physically executable strategy with historical precedent (WWI, WWII) — the founding problem this constraint 'solves' is not one anyone designed to solve; it is the emergent physical consequence of thermonuclear yield and reliable delivery systems removing survivable victory from the total-war option.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by declassified nuclear targeting studies (the physical damage models underlying strategic planning), independent physics of blast/fallout/climate effects, and convergent analysis from strategic studies scholars across ideologically opposed traditions (realist deterrence theorists and disarmament advocates alike accept the physical premise, disputing only its normative implications) — none of whom benefit from the constraint in a rent-collecting sense, since no party collects rents from a physical impossibility.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__structural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__structural_contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_winnability_post1945__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__structural_contraction_reading, 0.04, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__structural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_winnability_post1945__structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near-zero (0.04) because no party collects rent, tribute, or advantage from the barrier's operation — it is not a toll booth, it is a wall that happens to have formed from weapons physics. Suppression is authored near-zero (0.02) because the constraint requires no active enforcement: no institution polices compliance, no punishment apparatus exists to compel adherence, because there is nothing to comply with — the option is simply absent from the space, not forbidden within it. Accessibility collapse is authored very high (0.92) because once a state's leadership understands second-strike survivability, the total-war option does not become merely unattractive — it becomes strategically nonsensical, which is the signature of a genuine mountain rather than a norm that could in principle be defied. Resistance is authored low (0.08): the small residual reflects ongoing strategic debate about missile defense, first-strike theories, and emerging technologies (hypersonics, cyber-attacks on command-and-control) that some analysts argue could reopen the reachable space — this is real intellectual resistance to the mountain's permanence, not resistance to an enforced rule.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states and civilian populations are both declared beneficiaries, but through structurally different mechanisms — the states benefit by having a costly, high-casualty strategic option foreclosed from their opponents as well as themselves (a mutual foreclosure that stabilizes deterrence), while civilian populations benefit as a pure externality of a barrier they had no role in constructing. Both groups sit near the low-directionality end because the constraint subsidizes their survival prospects rather than extracting from them. This triggers FSM evaluation (Mountain + declared beneficiaries) — the omega below documents why this is treated as a genuine natural-law reading rather than a constructed one masquerading as physics: the physical yield/delivery-reliability facts are independent of any party's policy preference, which distinguishes this from cases where 'natural law' framing is deployed to launder a constructed advantage.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (great-power total war being physically executable and periodically executed) remains live in the sense that the underlying capability for conventional total war persists among non-nuclear or asymmetric dyads — the constraint's coverage is specifically the nuclear-dyad case, where the physical foreclosure remains as robust as the weapons themselves. There is no mandatrophy risk in the classic sense (an institution outliving its function) because this is not an institution — it has no administrators to check for capture, no mandate that could be quietly redirected. The disappearance_verdict of world_rearranges reflects that the barrier's removal (via disarmament or a technological breakthrough undoing second-strike survivability) would reopen a strategic option, not that anyone currently profits from maintaining it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_vs_constructed_foreclosure,
    'Is the removal of total war from the reachable space a genuine physical/mathematical consequence of thermonuclear yield and delivery-system reliability (a true mountain), or does it depend on maintained institutional arrangements (arsenal maintenance budgets, early-warning systems, command-and-control doctrine) that could in principle be allowed to degrade, making the barrier partially constructed rather than purely physical?',
    'Technical assessment of whether second-strike survivability is a robust physical property of dispersed, hardened, or submarine-based arsenals independent of day-to-day institutional choices, versus a property that requires continuous costly maintenance decisions that could be reversed by policy choice (unilateral disarmament, deliberate arsenal degradation).',
    'If the barrier depends on continuously renewed institutional maintenance decisions rather than pure physics, this reading would need reclassification toward a Tangled Rope or Scaffold (coordination requiring active investment) rather than a pure Mountain — the FSM signature would need to be taken seriously rather than resolved in favor of naturalness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_vs_constructed_foreclosure, conceptual, 'Whether the physical-foreclosure reading survives scrutiny of the maintenance dependencies underlying second-strike survivability.').

omega_variable(
    kernel_reading_boundary_disagreement,
    'Where exactly does the physical-contraction claim (this reading) end and the normative-illegitimacy claim (normative_reading_drop) or discursive-shift claim (strategic_culture_drift) begin, given that all three phenomena emerged in the same historical period and are causally entangled — nuclear deterrence likely accelerated humanitarian law development and shifted strategic discourse, rather than existing in causal isolation?',
    'Historical and legal analysis tracing whether Article 2(4) and humanitarian law development would plausibly have proceeded on a similar timeline absent the nuclear condition, and whether strategic culture shifted independently of, or entirely downstream from, the physical deterrence reality.',
    'If the normative and discursive readings turn out to be substantially downstream effects of the physical reading rather than independent causal tracks, this reading''s network edges to the sibling constraints should be authored as influences rather than merely coexists_with, and the sibling readings'' extraction/legitimacy metrics may need revision to reflect the upstream physical dependency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_disagreement, conceptual, 'How cleanly the three kernel readings can be causally and structurally disentangled given their shared historical origin.').

omega_variable(
    emerging_technology_reversibility,
    'Do emerging technologies (missile defense at scale, hypersonic glide vehicles undermining second-strike warning, offensive cyber operations against nuclear command-and-control, AI-enabled counterforce targeting) threaten to reopen the reachable space for total war by undermining the survivability assumption this mountain rests on?',
    'Ongoing tracking of missile defense efficacy against modern countermeasures, and expert assessment of whether any credible near-term technology could restore first-strike advantage sufficient to make total war rationally winnable again.',
    'If a technology credibly restores first-strike advantage, the mountain classification would need revision — the constraint would reveal itself as time-bound and technology-contingent (a scaffold-like structure resting on a particular technological equilibrium) rather than a permanent physical fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emerging_technology_reversibility, empirical, 'Whether the physical barrier is permanent or contingent on the current technological equilibrium of offense-defense balance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__structural_contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1945, 0.03).
narrative_ontology:measurement(tota_tr_t1962, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1962, 0.08).
narrative_ontology:measurement(tota_tr_t1979, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1979, 0.06).
narrative_ontology:measurement(tota_tr_t1991, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1991, 0.04).
narrative_ontology:measurement(tota_tr_t2008, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2008, 0.05).
narrative_ontology:measurement(tota_tr_t2025, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2025, 0.05).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1945, 0.05).
narrative_ontology:measurement(tota_be_t1962, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1962, 0.06).
narrative_ontology:measurement(tota_be_t1979, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1979, 0.05).
narrative_ontology:measurement(tota_be_t1991, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1991, 0.04).
narrative_ontology:measurement(tota_be_t2008, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2008, 0.04).
narrative_ontology:measurement(tota_be_t2025, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2025, 0.04).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_winnability_post1945__structural_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__structural_contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_winnability_post1945__structural_contraction_reading, 0.02).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, normative_reading_drop).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, strategic_culture_drift).

% DUAL FORMULATION NOTE:
% This story is the structural_contraction_reading member of the total_war_winnability_post1945 kernel family (three readings). The physical-foreclosure claim (this file, Mountain, epsilon ~0.04) is authored as causally upstream of and structurally distinct from the normative-illegitimacy claim (normative_reading_drop, evaluates Article 2(4) and IHL development as the operative mechanism) and the discursive-shift claim (strategic_culture_drift, evaluates elite strategic-culture change as the operative mechanism). All three readings address the same colloquial claim ('total war became unthinkable after 1945') but instantiate structurally distinct constraints with different epsilon values, different beneficiary/victim structures, and different failure modes — per the epsilon-invariance principle, they are not merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
