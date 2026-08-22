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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Structural Removal of Total War from the Reachable Space by Nuclear Weapons
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This story instantiates the 'structural_contraction_reading' of the
 *   kernel total_war_winnability_post1945: the claim that total war between
 *   nuclear-armed great powers has not merely become rare, discouraged, or
 *   normatively disfavored, but has become physically unreachable as a
 *   rational strategy because mutual second-strike capability guarantees the
 *   initiator's own destruction. This is a Mountain-class reading — the
 *   constraint is authored as emerging from the physics of weapons yield and
 *   delivery survivability, not from treaty, norm, or elite belief. Two
 *   sibling readings of the same kernel (normative_reading_drop,
 *   strategic_culture_drift) are NOT this constraint; they claim the same
 *   absence of total war is explained by legal/normative development (Article
 *   2(4), IHL) or by ideational shift in strategic culture respectively, and
 *   each would carry a different ε, different ontology, and different
 *   classification (likely rope or tangled_rope, since norms and discourse
 *   conventions require maintenance and can be captured or eroded in ways
 *   physical constraints cannot). This reading's ε stays low and stable
 *   across the entire interval precisely because a genuine physical ceiling
 *   does not accumulate rent-seeking or require active defense the way a
 *   normative or cultural claim would.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__structural_contraction_reading, 0.03).
domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, 0.05).
domain_priors:theater_ratio(total_war_winnability_post1945__structural_contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__structural_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_winnability_post1945__structural_contraction_reading, "Structural Removal of Total War from the Reachable Space by Nuclear Weapons").
narrative_ontology:topic_domain(total_war_winnability_post1945__structural_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__structural_contraction_reading, 'ad4da439-8dc3-459f-af81-5b9c8b74e9ca').
narrative_ontology:cs_kernel_codification('ad4da439-8dc3-459f-af81-5b9c8b74e9ca', implicit).
narrative_ontology:cs_authority_grounding('ad4da439-8dc3-459f-af81-5b9c8b74e9ca', none).
narrative_ontology:cs_reading_relation('ad4da439-8dc3-459f-af81-5b9c8b74e9ca', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_reading_relation('ad4da439-8dc3-459f-af81-5b9c8b74e9ca', total_war_winnability_post1945__strategic_culture_drift, coexists_with).
narrative_ontology:cs_axiom('ad4da439-8dc3-459f-af81-5b9c8b74e9ca', foundational, physical_second_strike_certainty_forecloses_rational_initiation).
narrative_ontology:cs_axiom_status(physical_second_strike_certainty_forecloses_rational_initiation, holdable).
narrative_ontology:cs_axiom_grounding('ad4da439-8dc3-459f-af81-5b9c8b74e9ca', physical_second_strike_certainty_forecloses_rational_initiation, empirically_contingent).
narrative_ontology:cs_axiom('ad4da439-8dc3-459f-af81-5b9c8b74e9ca', secondary, absence_of_total_war_requires_no_social_maintenance).
narrative_ontology:cs_axiom_status(absence_of_total_war_requires_no_social_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('ad4da439-8dc3-459f-af81-5b9c8b74e9ca', absence_of_total_war_requires_no_social_maintenance, empirically_contingent).
narrative_ontology:cs_reference_frame('ad4da439-8dc3-459f-af81-5b9c8b74e9ca', pre_nuclear_great_power_war_regime).
narrative_ontology:cs_drift_state('ad4da439-8dc3-459f-af81-5b9c8b74e9ca', contemporary_multipolar_nuclear_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ad4da439-8dc3-459f-af81-5b9c8b74e9ca', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__structural_contraction_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__structural_contraction_reading, great_power_populations).
narrative_ontology:constraint_victim(total_war_winnability_post1945__structural_contraction_reading, hypothetical_exchange_populations).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__structural_contraction_reading, mutual_assured_destruction_stability_thesis).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__structural_contraction_reading, nuclear_revolution_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess arsenals whose second-strike capability makes any total-war strategy against a peer arithmetically self-destructive before it can be attempted. They did not choose this outcome socially; it is a consequence of the physics of fission/fusion yield scaling and delivery reliability meeting the logic of retaliation. They benefit from the removal of the total-war option in the sense that their populations are not subject to it, but they collect no rent from the arrangement and cannot dismantle it by policy choice while parity holds.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, nuclear_weapon_states, beneficiary,
    institutional, civilizational, analytical, global).

% Live under the umbrella of a structural fact they had no hand in creating and cannot vote to repeal: their governments cannot wage unrestrained war against another nuclear peer without incurring existential retaliation. They are structurally protected from a category of war, not by treaty or norm, but by the physical arithmetic of assured destruction.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, great_power_populations, beneficiary,
    powerless, civilizational, analytical, global).

% Exist only in the counterfactual space the constraint forecloses — the populations who would bear a total nuclear exchange if the structural barrier ever failed (accident, miscalculation, breakdown of second-strike survivability). They are victims of the residual tail risk the mountain does not fully eliminate, not of any extractive arrangement; their 'payment' is the small but nonzero probability mass the structural floor still permits.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, hypothetical_exchange_populations, payer,
    powerless, civilizational, trapped, global).

% Study whether the absence of total war since 1945 reflects a physical ceiling, a normative shift, or an ideational drift in strategic culture. Their disagreement is the kernel contest this story is one reading of; they do not benefit or pay directly but their framing choices affect which policy interventions (arms control vs. norm-building vs. culture-shaping) seem load-bearing.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, strategic_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the coordination sense — this is not a solved collective-action problem but a physical boundary condition. To the extent a 'function' exists, it is that mutual second-strike capability removes a strategic option (total war between nuclear peers) from the feasible set without requiring any party's continued consent, enforcement, or belief.
% TRANSFER_FUNCTION: No transfer of value occurs through this constraint; it forecloses an action, it does not move resources. The only 'transfer' is counterfactual: the risk that would have been borne by total-war-exchange populations under a pre-nuclear technological regime is instead borne, in residual and much smaller form, by the same populations as tail probability of nuclear accident or miscalculation.
% ABSENT_VOICES: Populations in the counterfactual exchange scenario have no voice because they are hypothetical — they exist only as the object the structural floor protects against realizing. Proliferation-risk analysts and accident-risk researchers are the closest real-world stand-ins for this absent constituency, and their warnings about the residual tail risk are the nearest thing to their advocacy.
% DISAPPEARANCE_RATIONALE: If nuclear weapons were removed from the world tomorrow while all other conditions held constant, total war between great powers would re-enter the reachable strategic space: the physical mechanism preventing rational actors from selecting total war as a strategy (assured mutual annihilation) would vanish, and the calculus of major-power conflict would revert to pre-1945 constraints (conventional force ratios, attritional capacity, alliance structures) under which total war was periodically chosen. This is the diagnostic that marks the constraint as structural rather than normative or cultural: removing the physical object removes the boundary, whereas removing a norm or a discourse convention would not, by itself, restore total-war-as-strategy if the physical deterrent remained.
% FOUNDING_PROBLEM: Nothing was 'built' to solve a problem — this is not an institutional response but a physical consequence of weapons yield and delivery technology reaching a threshold where retaliatory destruction cannot be prevented or survived by the initiator. The closest analogue to a 'founding problem' is the pre-1945 absence of any physical ceiling on great-power war, which total war (1914-18, 1939-45) repeatedly filled.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the nuclear weapon states themselves by decades of independent deterrence scholarship (Schelling, Jervis, Waltz-Sagan debate), non-nuclear-weapon-state security analysts who have no stake in nuclear possession but observe the same absence of great-power total war, and accident/near-miss historical research (Petrov incident, 1983 Able Archer, Cuban Missile Crisis) documenting how close the structural floor has come to failing without the underlying physical logic being socially negotiable by any party.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__structural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__structural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_winnability_post1945__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__structural_contraction_reading, 0.03, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored near zero throughout because no party collects rent from the arrangement — nuclear weapon states do not charge anyone for the deterrent effect, and non-nuclear populations are structurally protected without payment. Accessibility collapse is authored very high (0.92): once mutual assured destruction obtains between peers, there is no accessible alternative strategy of 'total war and win' available to a rational actor — the option is not suppressed by choice, it collapses because any attempt terminates in mutual annihilation before achieving the strategic aims total war traditionally sought. Resistance is authored low (0.08) because there is essentially no actor pushing to restore total war as a live strategic option against a nuclear peer; the few voices that do (some deterrence-skeptic theorists, occasional rogue-state rhetoric) do not constitute meaningful structural resistance to the physical fact itself, only debate about its interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states and their populations are coded as beneficiaries not because they extract anything, but because the directionality derivation needs a beneficiary declaration to satisfy the FSM check the schema requires when a Mountain names beneficiaries. Genuinely, no one profits from this constraint the way a rent-collector profits from a snare — the 'benefit' is simply not being subject to a physically superseded category of war. The hypothetical exchange populations are victims only in a counterfactual sense: they are who *would* pay if the structural floor failed, which is why their exit_options is coded trapped and time_horizon civilizational — the tail risk, though small, is borne by everyone equally and cannot be exited by any individual choice.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a Mountain that legitimately declares beneficiaries without being a false summit: the beneficiary declaration exists here to trigger the FSM check honestly, and the omega below documents the natural-law-vs-constructed ambiguity the schema requires. The correct verdict is that this constraint IS a genuine mountain despite naming beneficiaries, because the beneficiaries collect nothing and could not, even if they wished, dismantle the physical mechanism that protects them. Mandatrophy would apply if the deterrent relationship persisted as policy theater after the physical mechanism no longer held (e.g., if delivery systems became reliably interceptable, restoring first-strike viability, while doctrine continued to assert assured destruction) — that drift is exactly what the accumulating-extraction omega and T17 trigger are positioned to catch if the temporal record ever showed extractiveness climbing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_versus_normative_ceiling,
    'Is the absence of great-power total war since 1945 attributable to the physical impossibility of survivable initiation (this reading), or does the physical mechanism merely provide cover for a normative/cultural shift that would persist even if the physical ceiling were technologically circumvented (e.g., via reliable missile defense or novel decapitation strategies)?',
    'Track counterfactual and quasi-experimental evidence: does strategic behavior among nuclear peers change measurably when second-strike survivability is credibly threatened (e.g., missile defense breakthroughs, hypersonic glide vehicles undermining retaliatory certainty)? If behavior reverts toward pre-nuclear brinkmanship as physical certainty erodes, this favors the structural reading; if normative constraints hold even as physical certainty erodes, this favors the sibling normative reading.',
    'If the sibling readings are correct and this reading is wrong, then the entire Mountain classification collapses — the true constraint would be a maintained normative or cultural artifact (likely tangled_rope, since maintaining the total-war taboo may involve extraction — e.g., nonproliferation regimes disadvantaging non-nuclear states) rather than a physical law with zero degrees of freedom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_versus_normative_ceiling, conceptual, 'Whether the removal of total war from the reachable space is genuinely physical or is a normative/cultural fact wearing physical cover.').

omega_variable(
    residual_tail_risk_magnitude,
    'How large is the residual probability that the structural floor fails (accident, miscalculation, novel decapitation capability, or a non-peer nuclear actor without assured second-strike logic), and does that residual risk mean the ''impossibility'' claim is better characterized as ''extremely high but nonzero cost'' rather than true zero-degrees-of-freedom impossibility?',
    'Historical near-miss frequency analysis (Petrov 1983, Cuban Missile Crisis, false-alarm incidents) combined with expert elicitation on current arsenal control reliability and emerging technology (AI-enabled early warning, cyber vulnerabilities in command-and-control) to estimate an annualized failure probability.',
    'If the residual risk is non-negligible and trending upward (e.g., due to cyber vulnerabilities or multipolar nuclear dynamics with unstable second-strike guarantees among newer nuclear states), the mountain classification should be qualified as a high but eroding structural floor rather than treated as permanent — this would also inform whether hypothetical_exchange_populations should be modeled with rising rather than flat exposure over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_tail_risk_magnitude, empirical, 'How solid the physical floor actually is, given known near-miss history and emerging technological threats to second-strike assurance.').

omega_variable(
    kernel_framing_choice_rationale,
    'The three sibling readings of this kernel (structural, normative, cultural) are not mutually exclusive causal claims about the same historical fact — they could all be partially true simultaneously. Why does this story treat them as three distinct constraints rather than components of one multi-causal constraint?',
    'This follows the ε-invariance principle: each candidate causal mechanism (physical impossibility, legal illegitimacy, discourse shift) has a different persistence profile, different failure mode, and would be measured with a different ε if evaluated on its own terms — a physical mechanism cannot be captured or eroded by institutional politics the way a norm or discourse convention can. Treating them as one constraint would force an artificial averaging of ε across mechanisms with genuinely different structural properties.',
    'If future analysis determines all three mechanisms are so causally entangled that they cannot be evaluated independently (e.g., the physical deterrent only ''works'' because it is embedded in a normative framework of legitimate retaliation), the decomposition into three stories may need to be revisited in favor of a single constraint with multiple contributing factors modeled as sub-properties rather than sibling constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice_rationale, conceptual, 'Whether decomposing the total-war-absence kernel into three sibling constraints versus one multi-causal constraint is the right analytical move.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__structural_contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1945, 0.03).
narrative_ontology:measurement(tota_tr_t1962, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1962, 0.06).
narrative_ontology:measurement(tota_tr_t1983, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1983, 0.07).
narrative_ontology:measurement(tota_tr_t1991, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1991, 0.05).
narrative_ontology:measurement(tota_tr_t2010, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(tota_tr_t2025, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2025, 0.05).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1945, 0.02).
narrative_ontology:measurement(tota_be_t1962, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1962, 0.04).
narrative_ontology:measurement(tota_be_t1983, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1983, 0.05).
narrative_ontology:measurement(tota_be_t1991, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1991, 0.03).
narrative_ontology:measurement(tota_be_t2010, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2010, 0.03).
narrative_ontology:measurement(tota_be_t2025, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2025, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_winnability_post1945__structural_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__structural_contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_winnability_post1945__structural_contraction_reading, 0.05).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, normative_reading_drop).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, strategic_culture_drift).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the kernel total_war_winnability_post1945, each authored as a separate story per the epsilon-invariance principle. structural_contraction_reading (this story) claims Mountain status with near-zero epsilon on the basis of physical impossibility via assured mutual destruction. normative_reading_drop claims the option remains physically live but has become illegitimate through international humanitarian law and the UN Charter Article 2(4) prohibition on aggressive war, and would be authored with a materially higher epsilon reflecting the maintenance costs and selective enforcement of that legal order. strategic_culture_drift claims the option remains reachable but has fallen out of elite discourse through ideational change in strategic culture, and would be authored with a moderate epsilon reflecting the contestability and reversibility of discourse norms. All three stories link to each other via affects_constraints because a change in one mechanism's credibility (e.g., erosion of the physical deterrent through missile defense breakthroughs) would place structural pressure on whether the normative and cultural mechanisms could independently sustain the total-war absence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
