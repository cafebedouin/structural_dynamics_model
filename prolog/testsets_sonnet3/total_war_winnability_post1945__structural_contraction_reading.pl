% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: total_war_winnability_post1945__structural_contraction_reading
 *   human_readable: Structural Contraction of Total War Under Mutual Assured Destruction
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   Under this reading, the post-1945 absence of total war between great
 *   powers is explained by an irreducible physical fact: once nuclear
 *   arsenals achieved second-strike survivability, no rational actor could
 *   expect to survive initiating total war against another such state. This
 *   is authored as a Mountain — zero degrees of freedom, not chosen or
 *   maintained by any party's preference, and not revocable by treaty or
 *   shift in norms. The 'beneficiaries' (nuclear weapon states, non-nuclear
 *   states operating under the resulting stability) are declared to trigger
 *   honest False-Summit-Mountain evaluation: physical laws do not have
 *   beneficiaries in the ordinary extractive sense, but the fact that some
 *   parties' security is enhanced by a structural fact is worth flagging as
 *   an omega rather than suppressing.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__structural_contraction_reading, 0.05).
domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, 0.03).
domain_priors:theater_ratio(total_war_winnability_post1945__structural_contraction_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__structural_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_winnability_post1945__structural_contraction_reading, "Structural Contraction of Total War Under Mutual Assured Destruction").
narrative_ontology:topic_domain(total_war_winnability_post1945__structural_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__structural_contraction_reading, 'ff9b240a-6e51-4c77-94fe-e8520c88a16c').
narrative_ontology:cs_kernel_codification('ff9b240a-6e51-4c77-94fe-e8520c88a16c', distributed).
narrative_ontology:cs_authority_grounding('ff9b240a-6e51-4c77-94fe-e8520c88a16c', distributed).
narrative_ontology:cs_reading_relation('ff9b240a-6e51-4c77-94fe-e8520c88a16c', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_reading_relation('ff9b240a-6e51-4c77-94fe-e8520c88a16c', total_war_winnability_post1945__strategic_culture_drift, coexists_with).
narrative_ontology:cs_axiom('ff9b240a-6e51-4c77-94fe-e8520c88a16c', foundational, physical_capability_determines_reachable_strategy_space).
narrative_ontology:cs_axiom_status(physical_capability_determines_reachable_strategy_space, holdable).
narrative_ontology:cs_axiom_grounding('ff9b240a-6e51-4c77-94fe-e8520c88a16c', physical_capability_determines_reachable_strategy_space, empirically_contingent).
narrative_ontology:cs_axiom('ff9b240a-6e51-4c77-94fe-e8520c88a16c', secondary, removal_of_physical_floor_immediately_restores_total_war_option).
narrative_ontology:cs_axiom_status(removal_of_physical_floor_immediately_restores_total_war_option, holdable).
narrative_ontology:cs_axiom_grounding('ff9b240a-6e51-4c77-94fe-e8520c88a16c', removal_of_physical_floor_immediately_restores_total_war_option, empirically_contingent).
narrative_ontology:cs_reference_frame('ff9b240a-6e51-4c77-94fe-e8520c88a16c', pre_nuclear_total_war_as_rational_instrument).
narrative_ontology:cs_drift_state('ff9b240a-6e51-4c77-94fe-e8520c88a16c', contemporary_missile_defense_and_multipolar_nuclear_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ff9b240a-6e51-4c77-94fe-e8520c88a16c', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__structural_contraction_reading, nuclear_weapon_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__structural_contraction_reading, non_nuclear_states).
narrative_ontology:constraint_victim(total_war_winnability_post1945__structural_contraction_reading, populations_in_counterfactual_exchange).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__structural_contraction_reading, mutual_assured_destruction_stability).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__structural_contraction_reading, nuclear_revolution_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess arsenals whose existence physically forecloses any adversary's rational path to victory through total war against them, since retaliation would guarantee unacceptable destruction regardless of who strikes first. They did not choose this outcome as policy in the way a treaty is chosen; the constraint emerged from the physics of fission/fusion yields and delivery reliability once arsenals reached second-strike survivability. They benefit from security against existential conquest but did not construct the underlying physical fact and cannot repeal it by choice.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, nuclear_weapon_states, beneficiary,
    institutional, civilizational, arbitrage, global).

% Operate in an international system where total war between great powers is no longer a live strategic option, which removes them from the shadow of great-power total war as a background condition of world politics — though they remain exposed to proxy conflict, coercion below the nuclear threshold, and conventional war. They have no lever over the physical constraint itself; it is not theirs to negotiate.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, non_nuclear_states, beneficiary,
    moderate, civilizational, constrained, global).

% A hypothetical victim class: the populations who would bear the cost of total war if the physical constraint did not hold. They are not actual claimants under this reading — no total war has occurred post-1945 — but the constraint's stakes are measured against what would happen to them absent the structural floor. Their situation exists only counterfactually, which is why this constraint is authored as Mountain rather than as an arrangement with an active victim set.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, populations_in_counterfactual_exchange, payer,
    powerless, civilizational, trapped, global).

% Study whether the post-1945 absence of great-power total war reflects a physical ceiling (this reading), a normative shift (sibling reading), or an ideational drift in elite discourse (sibling reading). Their disagreement is the kernel contest itself; they do not administer the constraint, they characterize it.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, strategic_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the ordinary sense — there is no negotiated agreement being coordinated. The 'coordination' is better described as a physical ceiling that removes an option from every actor's feasible set simultaneously, regardless of preference or agreement. States are not solving a collective-action problem; they are operating under a floor that is not theirs to raise or lower by policy choice.
% TRANSFER_FUNCTION: Nothing is transferred between parties under this reading — no rents, no payments, no coerced compliance. The constraint does not move value from one actor to another; it removes a strategic option (total war as a winnable, rational instrument) from the entire system's reachable space. Any 'benefit' nuclear weapon states receive (security from existential conquest) is a byproduct of the physical fact, not an extraction from another party.
% ABSENT_VOICES: Proponents of the sibling readings (normative_reading_drop, strategic_culture_drift) would object that this reading understates the causal contribution of legal and normative development (UN Charter Article 2(4), humanitarian law) and of ideational change in what elites consider thinkable. They are not excluded from the broader kernel debate — they are simply describing a different constraint, authored separately per the ε-invariance principle.
% DISAPPEARANCE_RATIONALE: If nuclear weapons and their second-strike survivability were subtracted from the world (not merely disarmed by treaty, but as if the physics did not permit them), total war between great powers would re-enter the reachable strategic space; deterrence architectures, alliance structures, and the entire post-1945 great-power peace would have to be re-derived from conventional balance-of-power calculations, which is a substantially different world.
% FOUNDING_PROBLEM: The problem this constraint 'solves' is not one anyone built to solve — it is the physical consequence of fission/fusion weapons reaching yields and delivery reliability sufficient to guarantee mutual destruction. Prior to that technical threshold, total war between great powers was a live, survivable, winnable strategic option (as in 1914 and 1939); after it, no rational actor could expect to survive initiating it against a state with second-strike capability.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside any single beneficiary state by decades of independent strategic-studies scholarship (Schelling, Waltz, Jervis on the nuclear revolution and the stability-instability paradox), by the empirical absence of direct great-power total war since 1945 despite numerous crises (Cuban Missile Crisis, Able Archer) that would plausibly have escalated to total war under pre-nuclear conditions, and by military planning documents from multiple nuclear states independently acknowledging no rational path to victory in a general nuclear exchange. This is not merely nuclear-weapon-state self-testimony.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__structural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__structural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_winnability_post1945__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__structural_contraction_reading, 0.05, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored near-zero (0.05) because no party transfers value to another through this constraint's operation — it removes an option from everyone's feasible set rather than redistributing anything. Suppression is likewise near-zero (0.03): the constraint is not maintained by coercion against resisters, because there is no viable path to total war for anyone to be coerced away from — the physics itself forecloses it. Accessibility collapse is very high (0.92): once the deterrence logic is understood, the alternative of a survivable total war between nuclear peers essentially disappears from consideration, which is the hallmark of a genuine natural-law-like ceiling rather than a socially negotiated norm. Resistance is low (0.1): unlike a treaty or law that requires ongoing defense against violation, no serious actor mounts a rational strategic case that total war against a second-strike-capable adversary is winnable; residual doctrinal disputes (e.g. limited nuclear war theories, damage-limitation strategies) are the modest theater/resistance signal captured in the small nonzero values, not evidence the ceiling itself is contested.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states and non-nuclear states are declared as beneficiaries because the constraint enhances their security relative to a counterfactual world without it, but this is NOT an extraction relationship — no other party pays for their benefit. The hypothetical victim class (populations_in_counterfactual_exchange) exists only to mark what the constraint's failure would cost; they are not an active payer group because no exchange has occurred under this reading's timeframe. This asymmetry — beneficiaries without correlative victims — is exactly what an FSM-candidate Mountain declaration is for: the schema requires an omega precisely because 'beneficiary of a physical law' is a structurally different claim from 'beneficiary of an extractive arrangement,' and the omega documents that distinction rather than letting it pass silently.
 *
 * MANDATROPHY ANALYSIS:
 *   There is no mandatrophy risk under this reading in the ordinary sense — a Mountain has no mandate to outlive, since it is not administered by anyone. The interesting genealogical question this reading raises is a different one: IF nuclear arsenals were someday neutralized (missile defense breakthrough, disarmament, or novel offense-dominant technology), would total war re-enter the reachable space immediately, or would decades of accumulated normative and cultural adaptation (the sibling readings' mechanisms) provide residual friction? This reading's own commitments imply the former — the structural floor, once removed, removes its effect entirely — which is precisely what distinguishes it from the sibling readings and is captured in the omega and axioms below.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_of_physical_law_ambiguity,
    'Can a genuine physical/structural constraint have ''beneficiaries'' in any sense that matters for classification, or does declaring beneficiaries here import an extraction framing that doesn''t belong to a Mountain?',
    'Compare against other physical-limit constraints with asymmetric benefit distribution (e.g., states with defensible geography benefiting from the physical difficulty of amphibious invasion) — if the corpus treats those consistently as Mountains despite asymmetric benefit, the same treatment applies here.',
    'If beneficiary declaration is judged inappropriate for physical constraints regardless of asymmetric benefit, this constraint should be re-authored with beneficiaries removed. If FSM correctly fires and reclassifies toward tangled_rope, that would indicate the underlying claim is better modeled as partly constructed (nuclear strategy, deterrence doctrine, alliance structures) rather than purely physical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_of_physical_law_ambiguity, conceptual, 'Whether asymmetric-benefit is compatible with genuine physical/Mountain status.').

omega_variable(
    kernel_causal_attribution_ambiguity,
    'Is the post-1945 absence of great-power total war better explained by physical impossibility (this reading), normative illegitimacy (normative_reading_drop), or strategic-cultural drift (strategic_culture_drift) — or is it genuinely overdetermined by all three operating jointly?',
    'Counterfactual and historical analysis: examine near-miss crises (Cuban Missile Crisis, 1983 Able Archer) for evidence of which mechanism was doing the restraining work in the moment — physical deterrence calculations, legal/normative constraint, or cultural unthinkability. Comparative analysis of nuclear vs. non-nuclear great-power dyads (e.g. India-Pakistan pre/post nuclearization) could isolate the physical variable.',
    'If the physical mechanism is shown to be doing most or all of the causal work, this reading''s Mountain classification is well-supported. If normative or cultural mechanisms are shown to be doing substantial independent work (e.g., total war would remain unthinkable even absent nuclear weapons), this reading overclaims and the true constraint is better modeled as overdetermined, requiring a fourth, joint-mechanism story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_causal_attribution_ambiguity, empirical, 'Which of the three kernel readings carries the true causal weight, or whether the mechanisms are jointly sufficient.').

omega_variable(
    reversibility_of_the_floor,
    'If second-strike nuclear capability were neutralized (missile defense, arms control, technological obsolescence), would total war immediately re-enter the reachable strategic space, as this reading''s structural-determinism implies?',
    'No direct empirical test is available (the counterfactual has not occurred); closest available evidence is historical analysis of strategic behavior during periods of reduced nuclear asymmetry or perceived first-strike vulnerability (e.g., early Cold War window of vulnerability debates) for signs of renewed total-war planning.',
    'If total war would NOT immediately re-enter the reachable space even after physical removal of nuclear deterrence, this indicates the sibling readings'' normative/cultural mechanisms have independent causal force layered on top of the physical floor — meaning the true post-1945 peace is jointly produced, not purely structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reversibility_of_the_floor, conceptual, 'Whether the physical constraint alone is sufficient, or whether normative/cultural residue would persist without it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__structural_contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(tota_tr_t1962, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement(tota_tr_t1979, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1979, 0.09).
narrative_ontology:measurement(tota_tr_t1991, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1991, 0.07).
narrative_ontology:measurement(tota_tr_t2008, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2008, 0.08).
narrative_ontology:measurement(tota_tr_t2025, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2025, 0.08).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1945, 0.04).
narrative_ontology:measurement(tota_be_t1962, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1962, 0.05).
narrative_ontology:measurement(tota_be_t1979, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1979, 0.05).
narrative_ontology:measurement(tota_be_t1991, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1991, 0.05).
narrative_ontology:measurement(tota_be_t2008, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2008, 0.05).
narrative_ontology:measurement(tota_be_t2025, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2025, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_winnability_post1945__structural_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, normative_reading_drop).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, strategic_culture_drift).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the total_war_winnability_post1945 kernel. The natural-language claim 'total war became unwinnable/unthinkable after 1945' conflates three structurally distinct causal claims: physical impossibility via mutual assured destruction (this story, Mountain), normative illegitimacy via international law development (normative_reading_drop), and ideational drift in strategic culture (strategic_culture_drift). Each carries its own epsilon, its own beneficiary/victim structure, and its own classification. This story's epsilon (0.05, near-zero, Mountain-consistent) should not be averaged or reconciled with the siblings' epsilon values — per the epsilon-invariance principle, these are three different constraints sharing a common label, not one constraint viewed three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
