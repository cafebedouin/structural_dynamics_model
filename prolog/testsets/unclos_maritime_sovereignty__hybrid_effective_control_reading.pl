% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__hybrid_effective_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__hybrid_effective_control_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unclos_maritime_sovereignty__hybrid_effective_control_reading
 *   human_readable: UNCLOS Maritime Sovereignty via Hybrid Effective Control (Natural/Artificial Feature Graduated Rights)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This story models ONE READING of the contested UNCLOS maritime
 *   sovereignty kernel. The reading instantiated here is the
 *   hybrid_effective_control_reading, which splits the difference between
 *   strict geographic rules (natural features only) and expansive
 *   construction rights (artificial features = immediate territorial claims).
 *   Under this reading, natural features above high-tide water generate full
 *   territorial sea and EEZ; artificial features initially qualify only for
 *   500m safety zones but can mature into full territorial claims if occupied
 *   and controlled continuously for sufficient duration without effective
 *   challenge from other claimants. This reading is operationally embedded in
 *   state practice (especially in Southeast Asia and the South China Sea) but
 *   remains contested by legal scholars and weaker claimant states. The
 *   constraint is CLAIMED as tangled_rope because it simultaneously solves a
 *   genuine coordination problem (how to adjudicate boundary claims when
 *   states engineer features) and extracts from weaker claimants (by
 *   permitting gradual claim expansion through construction and occupation
 *   strategies that only the wealthy and militarily strong can execute). The
 *   claim/metric gap is deliberate and intentional—the metrics reflect the
 *   actual operation of the constraint as a vehicle for power asymmetry
 *   extraction, even though the doctrine itself is framed as neutral law.
 *
 * KEY AGENTS:
 *   - states_with_construction_capacity: Benefit from the hybrid reading by being able to construct artificial features and claim gradual expansion of zones as effective control solidifies. Set the standard of what constitutes 'prolonged control' through their practice. Institutional power, arbitrage exit options.
 *   - regional_power_projectors: Benefit from the doctrine by projecting geopolitical influence through island construction. Powerful, mobile exit, regional scope.
 *   - militarily_weaker_claimants: Pay by facing one-way ratchet: stronger neighbors build and expand claims; weaker states must either acquiesce or mount expensive military contestation with no legal support. Moderate power, constrained exit.
 *   - developing_nations_without_capital: Trapped by the constraint because they lack the engineering and military resources to build features or mount sustained counter-presence. Powerless, no arbitrage exit, forced to accept expanded claims by stronger neighbors.
 *   - international_maritime_law_community: Analytical seat—interprets whether specific constructed features meet the 'effective control' threshold and adjudicates disputes. Their readings constitute the legitimacy standard.
 *   - strict_geographic_reading_advocates: Excluded from agenda-setting because the hybrid reading has been operationalized by powerful states. They argue artificial construction should confer zero rights, but their position is sidelined by practice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.62).
domain_priors:suppression_score(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.71).
domain_priors:theater_ratio(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__hybrid_effective_control_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__hybrid_effective_control_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__hybrid_effective_control_reading, "UNCLOS Maritime Sovereignty via Hybrid Effective Control (Natural/Artificial Feature Graduated Rights)").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__hybrid_effective_control_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__hybrid_effective_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__hybrid_effective_control_reading, '24454809-757f-4d8b-b6d7-ba6049fa8ed4').
narrative_ontology:cs_kernel_codification('24454809-757f-4d8b-b6d7-ba6049fa8ed4', fixed_text).
narrative_ontology:cs_authority_grounding('24454809-757f-4d8b-b6d7-ba6049fa8ed4', extraction).
narrative_ontology:cs_interpretation_layer_present('24454809-757f-4d8b-b6d7-ba6049fa8ed4').
narrative_ontology:cs_reading_relation('24454809-757f-4d8b-b6d7-ba6049fa8ed4', unclos_maritime_sovereignty__strict_geographic_reading, coexists_with).
narrative_ontology:cs_reading_relation('24454809-757f-4d8b-b6d7-ba6049fa8ed4', unclos_maritime_sovereignty__expansive_construction_reading, coexists_with).
narrative_ontology:cs_axiom('24454809-757f-4d8b-b6d7-ba6049fa8ed4', foundational, graduated_sovereignty_based_on_feature_type_and_control_duration).
narrative_ontology:cs_axiom_status(graduated_sovereignty_based_on_feature_type_and_control_duration, holdable).
narrative_ontology:cs_axiom_grounding('24454809-757f-4d8b-b6d7-ba6049fa8ed4', graduated_sovereignty_based_on_feature_type_and_control_duration, instrumental).
narrative_ontology:cs_axiom('24454809-757f-4d8b-b6d7-ba6049fa8ed4', foundational, effective_control_as_sovereignty_maturation_pathway).
narrative_ontology:cs_axiom_status(effective_control_as_sovereignty_maturation_pathway, holdable).
narrative_ontology:cs_axiom_grounding('24454809-757f-4d8b-b6d7-ba6049fa8ed4', effective_control_as_sovereignty_maturation_pathway, deontological).
narrative_ontology:cs_reference_frame('24454809-757f-4d8b-b6d7-ba6049fa8ed4', natural_features_full_rights_artificial_features_contested).
narrative_ontology:cs_drift_state('24454809-757f-4d8b-b6d7-ba6049fa8ed4', contemporary_acceleration_of_construction_and_claims, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('24454809-757f-4d8b-b6d7-ba6049fa8ed4', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, states_with_construction_capacity).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__hybrid_effective_control_reading, regional_power_projectors).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, militarily_weaker_claimants).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__hybrid_effective_control_reading, developing_nations_without_capital).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__hybrid_effective_control_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__hybrid_effective_control_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__hybrid_effective_control_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__hybrid_effective_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.62, rising to 0.62 by interval end) reflects intermediate but substantial extraction. The constraint does solve a real coordination problem (how to handle artificial islands without either permitting unlimited claims or denying them all relevance), so it is not pure extraction; but it systematically advantages states with capital and military power over weaker claimants. Suppression is high (0.71) because the constraint's persistence depends on actively excluding alternative readings (the strict interpretation) and on the practical enforcement of claim boundaries through military presence and administrative occupation—weaker states are suppressed from mounting effective counter-claims not just by economic costs but by the asymmetry in what constitutes 'effective control' (your garrison counts; their protest does not). Theater is moderate (0.48), rising slightly to 0.48 by the end of the interval: the coordination function is real, but as construction accelerates, the performative element of claiming 'prolonged control' grows—states construct, occupy briefly or establish symbolic presence, and claim maturation has occurred, regardless of actual control duration. The measurement series run on one shared time grid: base_extractiveness and theater both show gradual rise over the 40-year interval as more states engage in construction and the doctrine becomes embedded in practice; suppression_requirement also rises as more constructed features create more contested zones requiring active defense. Accessibility_collapse (how completely alternatives close off) is moderate (0.58 at t0, rising to 0.68 at tn) because alternative interpretations still exist (strict geographic reading, expansive construction reading) but the hybrid reading's operational embedding makes those alternatives increasingly costly to assert. Resistance is high (0.72) because weaker claimants actively resist the reading (mounting diplomatic protests, legal challenges, counter-construction projects) but their resistance is structurally suppressed by the power asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (states_with_construction_capacity, regional_power_projectors) experience this constraint as liberating—it permits them to expand maritime jurisdiction through legitimate engineering and control demonstration, which they frame as winning claims through capability and persistence rather than coercion. The victim seats (militarily_weaker_claimants, developing_nations_without_capital) experience it as extractive—they watch stronger neighbors expand unchallenged and have no legal or practical recourse. The agenda-setter seat (the same as the primary beneficiary: states_with_construction_capacity) sets the standard of what constitutes effective control by their own practice, creating a tautology: if I occupy and build, I demonstrate effective control; if you protest, you lack effective control. The law-community observer seat (international_maritime_law_community) sees a defensible doctrinal compromise between extreme positions but is pressured by the operational embedding—the more the hybrid reading is used to expand claims, the more courts face choices between law-as-written and law-as-practiced. The engine should compute different classification outcomes for each seat: beneficiaries near the beneficiary end of directionality (d ~0.1–0.2, receiving subsidy or coordination benefit), victims near the target end (d ~0.8–0.9, bearing asymmetric extraction cost), observers analytical. The claim (tangled_rope) is correct for the seats that benefit from the coordination function; the metrics (high suppression, moderate-high extraction) are correct for the seats that bear the costs. This divergence is diagnostic—it shows that the constraint distributes coordination benefits asymmetrically and masks extraction under coordination language.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality: States with construction capacity and regional power are the beneficiaries—they collect expanded maritime zones and geopolitical influence. Their directionality is low (d ~ 0.15–0.25) because they benefit directly and their exit options are mobile (if the hybrid reading were overturned, they would compete under a different regime but remain powerful). Victim directionality: Weaker claimants and developing nations are the victims—they pay by losing maritime zones and resource rights to stronger neighbors. Their directionality is high (d ~ 0.75–0.85) because they bear costs they cannot escape (trapped or identity_locked exit: maritime sovereignty is part of state identity and cannot be abandoned). The power asymmetry (institutional vs. powerless/moderate) amplifies this: weaker states are more vulnerable to extraction because they have no alternative. The duration mechanism ('prolonged control') adds to victim directionality—the longer the constraint operates undisputed, the more the victim's options narrow (they can no longer contest a 20-year-established claim without appearing aggressive and destabilizing). Observer directionality is analytical (d ~ 0.5 by default, no significant extraction or benefit from the constraint's operation).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy detection hinges on the (founding_problem_status x disappearance_verdict) mismatch. The founding problem (how to integrate artificial island construction into maritime law) is contested—some argue it is still live (new construction projects continue, new claims arise) and others argue the problem was solved by the hybrid reading (it provides a stable rule set, even if imperfectly enforced). Disappearance verdict is world_rearranges—if the hybrid reading vanished, maritime claims, military positions, and geopolitical alignments would shift dramatically. The mismatch (contested status + rearrangement verdict) suggests the mandate is challenged but not obsolete. However, the theater_ratio rising to 0.48 and the measurement series showing construction and claim acceleration hint at mandate drift: is the constraint still solving the original problem (integrating artificial features into a coherent law-of-the-sea framework) or is it now serving as a justification for unilateral claim expansion by those with engineering capacity? A deeper reading: the founding problem was 'how to handle artificial construction without either permitting unlimited claims or denying them relevance.' The hybrid reading solved this by introducing 'duration and effective control' as a maturation criterion. But as construction technology democratizes and states accelerate building (in real-world data, island construction in the South China Sea accelerated 2013–2018), the maturation threshold is under pressure—is five years enough? ten years? The constraint's coordination function (providing a rule set) persists, but its fairness/legitimacy function (ensuring that the rule set does not just permit the strongest to expand unchecked) is degrading. This is not obsolete mandate but eroding mandate: the constraint still solves its original problem but is increasingly perceived as solving it unfairly. Reclassification would not be justified until the coordination function fails entirely (e.g., the hybrid reading is rejected by a major arbitration tribunal), but the theater and resistance measurements should trigger mandatrophy_analysis as a flag that the constraint is drifting from fair coordination toward power-laundering.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_sibling_foreclosure,
    'Does the hybrid effective-control reading logically foreclose the strict geographic reading, or can both coexist as live positions?',
    'Examination of the kernel text (UNCLOS Articles 60, 121, 135) to determine whether they contain explicit language supporting or prohibiting artificial feature rights. If the text is silent on artificial construction maturation, both readings coexist; if the text explicitly prohibits artificial islands from generating territorial sea, the hybrid reading forecloses the geographic reading within that framework.',
    'If the strict reading is foreclosed, the hybrid reading is the only coherent interpretation and courts must accept it. If both coexist, the hybrid reading is a negotiated compromise—more vulnerable to revision as political power shifts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Whether this reading''s core premise logically rules out the strict geographic alternative.').

omega_variable(
    duration_threshold_indeterminacy,
    'What constitutes ''prolonged effective control'' sufficient to mature an artificial feature from safety-zone to territorial claim? Is it years, decades, generations, or is the threshold deliberately left vague?',
    'Review arbitration awards (South China Sea Arbitration, ICJ cases), state practice data on construction dates vs. claim recognition, and international law commission reports. If state behavior shows a consistent threshold (e.g., 10+ years of occupation + administrative presence + no successful challenge = maturation), the threshold is empirically determinable. If no such pattern emerges, the vagueness is structural.',
    'If the threshold is determinable, weaker claimants could calculate the cost-benefit of mounting a challenge within the maturation window. If vague, the constraint operates as pure power assertion—the state with stronger military presence can claim maturation has occurred whenever it serves political interest, and weaker states cannot contest the timeline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duration_threshold_indeterminacy, empirical, 'Whether ''prolonged control'' has a stable, verifiable definition or remains strategically ambiguous.').

omega_variable(
    effective_control_suppression_mechanism,
    'Is the suppression in this constraint primarily structural (military capability asymmetry makes mounting sustained counter-presence prohibitively expensive for weaker states) or internalized (weaker states accept the dominance narrative and believe they have no legitimate standing to challenge)?',
    'Analysis of diplomatic protests, legal challenges, and counter-construction attempts by weaker claimants. High frequency of explicit legal/diplomatic resistance with minimal counter-presence = structural suppression. Low frequency of protest combined with internal narratives of acceptance = internalized suppression. Mixed pattern = both mechanisms operative.',
    'If structural, the constraint could be undermined by external intervention (e.g., great-power enforcement of counter-claims). If internalized, weaker states would carry the suppression even if structural barriers were removed—they would need delegitimization of the dominance narrative before challenging.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effective_control_suppression_mechanism, empirical, 'Whether suppression of weaker claimants operates through external barriers or internalized acceptance.').

omega_variable(
    hybrid_reading_mandate_obsolescence,
    'As engineering technology advances and artificial island construction becomes cheaper and faster, does the hybrid reading''s original coordination function (reconciling natural and artificial features under a coherent framework) persist, or does it become a rationalization for unilateral claim expansion by any state with capital?',
    'Comparison of construction project frequency, state claims, and dispute escalation over successive decades. If construction accelerates but dispute frequency does not, the constraint is shifting toward pure extraction. If dispute frequency accelerates in tandem, the coordination function is being actively contested and the mandate is still contested.',
    'If mandate obsolescence occurs, the constraint should reclassify from tangled_rope (coordination + extraction) toward snare (pure extraction with coordination cover story).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_reading_mandate_obsolescence, empirical, 'Whether the hybrid reading''s coordination function survives as engineering capabilities democratize.').

omega_variable(
    kernel_sibling_reading_definitions,
    'This story is one reading of the contested kernel unclos_maritime_sovereignty. Which sibling readings coexist, which does this reading foreclose, and where is the disagreement located structurally?',
    'Compare the three readings'' core premises: (a) strict_geographic_reading: natural features only; (b) hybrid_effective_control_reading (this reading): natural = full rights, artificial = maturation through control; (c) expansive_construction_reading: artificial = immediate full rights. The disagreement is located in the interpretation of UNCLOS Article 60 (artificial islands) and the customary international law doctrine of effective occupation.',
    'The hybrid reading coexists with both siblings (all three are live positions in contemporary state practice) but influences them: it constrains the expansive reading by requiring ''maturation'' rather than immediacy, and it pressures the strict reading by acknowledging effective control as a maturation pathway. Neither is foreclosed by the hybrid reading''s internal logic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_reading_definitions, conceptual, 'Relationship of this reading to the other two readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(uncl_tr_t5, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 5, 0.39).
narrative_ontology:measurement(uncl_tr_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(uncl_tr_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement(uncl_tr_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(uncl_tr_t40, unclos_maritime_sovereignty__hybrid_effective_control_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(uncl_be_t5, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(uncl_be_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(uncl_be_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(uncl_be_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(uncl_be_t40, unclos_maritime_sovereignty__hybrid_effective_control_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(uncl_su_t5, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(uncl_su_t10, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(uncl_su_t20, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(uncl_su_t30, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(uncl_su_t40, unclos_maritime_sovereignty__hybrid_effective_control_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__hybrid_effective_control_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__hybrid_effective_control_reading, 0.12).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__hybrid_effective_control_reading, unclos_maritime_sovereignty__expansive_construction_reading).

% DUAL FORMULATION NOTE:
% This story is the hybrid_effective_control_reading of the contested UNCLOS maritime sovereignty kernel. Two sibling constraint stories model the strict_geographic_reading and expansive_construction_reading. The three readings differ on the ε-invariance axis: they instantiate different structural claims about what features generate what maritime rights, and therefore have different beneficiary/victim structures. Decomposition is required (OQ-89 ε-invariance principle): measuring the same constraint-kernel under different readings produces different ε values because the structural claim being made (who benefits, who pays, how much extraction) differs. Each reading is a separate, ε-invariant constraint story. Sibling stories are linked via network.affects_constraints so contamination analysis can route influence through the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_maritime_sovereignty__hybrid_effective_control_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
