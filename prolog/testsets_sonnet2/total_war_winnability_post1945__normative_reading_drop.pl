% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__normative_reading_drop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__normative_reading_drop, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: total_war_winnability_post1945__normative_reading_drop
 *   human_readable: UN Charter Article 2(4) and Humanitarian Law Prohibition on Total War
 *   domain: international_relations/law/security
 *
 * SUMMARY:
 *   This story reads the post-1945 disappearance of total war from legitimate
 *   state practice as a normative achievement: Article 2(4)'s prohibition on
 *   the threat or use of force, layered with Geneva Convention and Additional
 *   Protocol restrictions on targeting civilians and civilian infrastructure,
 *   converted total war from an available strategic option into a legally and
 *   reputationally sanctioned one. Total war remains physically possible —
 *   the productive and military capacity to wage it has not vanished — but
 *   the constraint operates at the level of legitimate practice, not physical
 *   reachability. This is the normative_reading_drop reading of the
 *   total_war_winnability_post1945 kernel. It is distinct from the
 *   structural_contraction_reading (which locates the change in nuclear
 *   deterrence physically removing total war from the reachable space) and
 *   the strategic_culture_drift reading (which locates it in an ideational
 *   shift within elite strategic culture, independent of formal treaty law).
 *   This reading's ε is authored for the treaty-and-IHL architecture
 *   specifically, assessed on its own terms: a genuine coordination
 *   achievement with real but moderate extraction from revisionist powers who
 *   lose a coercive option, not from the fact of physical total-war capacity
 *   persisting.
 *
 * KEY AGENTS:
 *   - global_civilian_populations: primary beneficiary (powerless/trapped) — protected class the norm exists to shield
 *   - un_charter_signatory_states: agenda_setter (institutional/constrained) — administers and is bound by the treaty architecture
 *   - revisionist_powers: primary target (powerful/constrained) — loses access to total-war coercive tools, pays in isolation when it breaches the norm
 *   - irregular_belligerents_denied_total_mobilization: secondary victim (powerless/trapped) — a population the framework was not built around and does not reliably protect
 *   - international_humanitarian_law_bodies: analytical/agenda-setting observer — interprets and extends the norm without enforcement power of its own
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__normative_reading_drop, 0.28).
domain_priors:suppression_score(total_war_winnability_post1945__normative_reading_drop, 0.42).
domain_priors:theater_ratio(total_war_winnability_post1945__normative_reading_drop, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, extractiveness, 0.28).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__normative_reading_drop, rope).
narrative_ontology:human_readable(total_war_winnability_post1945__normative_reading_drop, "UN Charter Article 2(4) and Humanitarian Law Prohibition on Total War").
narrative_ontology:topic_domain(total_war_winnability_post1945__normative_reading_drop, "international_relations/law/security").

domain_priors:requires_active_enforcement(total_war_winnability_post1945__normative_reading_drop).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__normative_reading_drop, '7c9a3aa3-aaea-46d1-8cc2-655be76dff65').
narrative_ontology:cs_kernel_codification('7c9a3aa3-aaea-46d1-8cc2-655be76dff65', formalized).
narrative_ontology:cs_authority_grounding('7c9a3aa3-aaea-46d1-8cc2-655be76dff65', lineage).
narrative_ontology:cs_interpretation_layer_present('7c9a3aa3-aaea-46d1-8cc2-655be76dff65').
narrative_ontology:cs_reading_relation('7c9a3aa3-aaea-46d1-8cc2-655be76dff65', total_war_winnability_post1945__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c9a3aa3-aaea-46d1-8cc2-655be76dff65', total_war_winnability_post1945__strategic_culture_drift, influences).
narrative_ontology:cs_axiom('7c9a3aa3-aaea-46d1-8cc2-655be76dff65', foundational, legal_prohibition_has_independent_causal_force).
narrative_ontology:cs_axiom_status(legal_prohibition_has_independent_causal_force, holdable).
narrative_ontology:cs_axiom_grounding('7c9a3aa3-aaea-46d1-8cc2-655be76dff65', legal_prohibition_has_independent_causal_force, empirically_contingent).
narrative_ontology:cs_axiom('7c9a3aa3-aaea-46d1-8cc2-655be76dff65', secondary, civilian_protection_is_the_normative_core_of_legitimacy).
narrative_ontology:cs_axiom_status(civilian_protection_is_the_normative_core_of_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('7c9a3aa3-aaea-46d1-8cc2-655be76dff65', civilian_protection_is_the_normative_core_of_legitimacy, deontological).
narrative_ontology:cs_reference_frame('7c9a3aa3-aaea-46d1-8cc2-655be76dff65', un_charter_prohibition_of_force).
narrative_ontology:cs_drift_state('7c9a3aa3-aaea-46d1-8cc2-655be76dff65', post_cold_war_intervention_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7c9a3aa3-aaea-46d1-8cc2-655be76dff65', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, small_and_middle_powers).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, un_charter_signatory_states).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, revisionist_powers).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, irregular_belligerents_denied_total_mobilization).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, military_planning_establishments).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, military_planning_establishments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are the class the prohibition on total war (unrestricted targeting of population centers, economies, and civil infrastructure as legitimate military objects) exists to protect. They do not administer the norm and cannot exit the international system they live inside, but the normative delegitimation of total war measurably reduces the probability their cities become deliberate military targets.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations, beneficiary,
    powerless, generational, trapped, global).

% Ratified Article 2(4)'s prohibition on the threat or use of force against territorial integrity and political independence, and layered on Geneva/Additional Protocol obligations restricting targeting of civilians and civilian objects. They administer the treaty architecture, invoke it in Security Council debate and international courts, and bear the coordination cost of maintaining a norm that constrains their own future options as well as others'.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, un_charter_signatory_states, agenda_setter,
    institutional, generational, constrained, global).

% Lack the capacity to wage or deter total war themselves. The normative illegitimacy of total war disproportionately protects them from being crushed by great-power total mobilization campaigns; the norm substitutes for military capacity they do not have. Their exit from the system is nominal — withdrawal from the UN framework leaves them more exposed, not less.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, small_and_middle_powers, beneficiary,
    moderate, generational, constrained, global).

% States seeking to alter territorial or political arrangements by force find the norm operates specifically against them: it delegitimizes the total-war strategies (siege economies, deliberate starvation, indiscriminate area bombing, unrestricted submarine warfare against civilian shipping) that would otherwise be available tools of coercion. They pay in diplomatic isolation, sanctions, and loss of legal cover when they breach the norm, though physical capability to conduct total war is undiminished. Exit means becoming an international pariah, which is costly but not impossible — this is a constrained rather than trapped position.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, revisionist_powers, payer,
    powerful, biographical, constrained, global).

% Non-state and insurgent actors fighting asymmetric conflicts sometimes find that the humanitarian law framework, built around interstate total-war prohibition, does not straightforwardly apply to or protect them, and international responses to their tactics can invoke violation-of-the-norm framing against them while state adversaries retain the norm's protections. They have no institutional voice in shaping the framework and cannot exit a system they were never party to negotiating.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, irregular_belligerents_denied_total_mobilization, payer,
    powerless, immediate, trapped, regional).

% The ICRC, international tribunals, and treaty-monitoring bodies interpret, extend, and adjudicate violations of the prohibition on total war conduct. They document violations, shape customary law through interpretation, and have no enforcement capacity of their own — they depend on state cooperation to give the norm teeth.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, international_humanitarian_law_bodies, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__normative_reading_drop, international_humanitarian_law_bodies, observer).

% National militaries must plan and train within legal constraints that foreclose total-war doctrines (unrestricted targeting, collective punishment, siege starvation) that could otherwise be operationally attractive. They benefit reciprocally — their own civilian populations and captured personnel gain the same protections when facing an adversary bound by the same norm — but bear real doctrinal and operational cost when the norm restricts otherwise available options against a norm-violating adversary.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, military_planning_establishments, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__normative_reading_drop, military_planning_establishments, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_winnability_post1945__normative_reading_drop, diffuse).
narrative_ontology:fixing_cost_class(total_war_winnability_post1945__normative_reading_drop, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: without a shared prohibition, each state's rational incentive under conditions of total war (once technology made societal-scale destruction reciprocal) is to strike civilian and economic targets first, producing a mutually worse equilibrium than restraint. Article 2(4) plus IHL development lets states commit credibly to mutual restraint on population-targeting and unrestricted warfare, lowering the expected cost of conflict for everyone including non-combatants.
% TRANSFER_FUNCTION: Moves the option to wage total war (unrestricted targeting of civilians, deliberate starvation, area destruction of population centers) away from all states as legally available strategy, transferring expected security from those without capacity to independently deter total war (small/middle powers, civilians everywhere) onto revisionist powers who lose access to a coercive tool set they would otherwise retain.
% ABSENT_VOICES: Non-state armed groups and populations under occupation or in failed-state conflicts had no seat at San Francisco in 1945 or at the Geneva Conventions' revision tables; the framework was negotiated by and for states, and its protections and obligations map imperfectly onto the conflicts where total-war-adjacent tactics (siege, starvation, indiscriminate bombardment) are most often actually used today.
% DISAPPEARANCE_RATIONALE: If Article 2(4) and the accumulated humanitarian law prohibition on total war conduct vanished overnight, states would not instantly regain the physical capacity for total war they never lost, but the legal and reputational cost of exercising it would disappear — sanctions regimes, war crimes tribunals, and diplomatic isolation mechanisms keyed to violations of these norms would lose their normative predicate. Great powers would face materially lower cost for adopting total-war doctrines against weaker states, and civilian protections currently backed by treaty obligation would revert to unilateral restraint alone.
% FOUNDING_PROBLEM: The unrestricted warfare of 1914-1945 — total economic blockade, strategic bombing campaigns deliberately targeting civilian morale and industry, unrestricted submarine warfare, and the demonstrated willingness of industrial states to mobilize entire societies as targets — showed that without an explicit prohibition, total war was not merely possible but had become the default expectation of great-power conflict, at a cost the postwar order deemed civilizationally intolerable.
% FOUNDING_PROBLEM_CORROBORATION: UN Charter drafters and international law scholars attest the founding problem (unconstrained total war as normalized great-power practice) was real and that the norm has substantially suppressed its recurrence between major powers. Independent conflict historians and IHL monitoring bodies outside the treaty-administering states corroborate a measurable decline in explicit total-war doctrine adoption since 1945, while also documenting that siege, starvation, and indiscriminate bombardment tactics persist in asymmetric and civil conflicts the framework was not built to fully reach — supporting a contested rather than settled status.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__normative_reading_drop, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__normative_reading_drop, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__normative_reading_drop, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_winnability_post1945__normative_reading_drop, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__normative_reading_drop, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__normative_reading_drop_tests).
:- end_tests(total_war_winnability_post1945__normative_reading_drop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28 at 2025) because the coordination function is genuine and dominant: the norm removes a mutually destructive equilibrium option and the extraction from revisionist powers is real but narrow — loss of a specific coercive tool set, not general subjugation. Suppression sits moderate (0.42) reflecting the active diplomatic, legal, and sanctions machinery required to hold the norm against powerful states that would otherwise find total-war doctrines operationally attractive; this is a treaty norm that requires continuous defense, not a self-enforcing equilibrium. Theater ratio is modest but rising (0.10 to 0.22) reflecting a genuine, if imperfect, concern: enforcement is more consistent against weaker violators than against permanent Security Council members, and some invocations of the norm function more as diplomatic signaling than as operative constraint. Accessibility collapse (0.58) is moderate rather than mountain-grade, because total war remains a genuinely available physical option for any state with sufficient capacity — the norm collapses its legitimacy, not its reachability. Resistance (0.35) reflects real but contained pushback: revisionist powers periodically test the norm's edges (sieges, indiscriminate bombardment in civil wars) without abandoning the framework outright, since even violators generally prefer to contest the norm's application rather than repudiate it.
 *
 * PERSPECTIVAL GAP:
 *   From the un_charter_signatory_states' and global_civilian_populations' seats, this is close to a rope: a coordination problem genuinely solved, with the constraint constraining the constrainer as much as anyone (great powers gave up options too). From the revisionist_powers' seat, the same structure looks more like a tangled rope or even snare-adjacent: a rule set drafted overwhelmingly by the 1945 victors that locks in a distribution of coercive capability favoring states that already possessed conventional and (later) nuclear dominance, dressed as universal humanitarian coordination. The engine computing divergent per-seat types from this same structural data is exactly the intended signal, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Global civilian populations and small/middle powers are declared beneficiaries because the norm's function is protective and they lack independent capacity to secure the same protection; their derived directionality sits near the full-beneficiary end. Revisionist powers are declared victims because the norm specifically forecloses a strategy category (total war) they would otherwise retain as an option, and they bear the reputational and legal cost of testing or breaching it; their derived directionality sits toward the target end, though 'constrained' rather than 'trapped' exit options moderate the effective extraction somewhat since exit (accepting pariah status) exists even if costly. Irregular belligerents are also victims but for a structurally different reason — not because the norm targets them, but because the framework's state-centric design leaves them underprotected, which the engine should register as a distinct extraction mechanism (exclusion-based, not targeting-based) from the revisionist-power case.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unconstrained industrial-scale total war as normalized great-power practice) is genuinely contested as live vs. dead: among great powers, direct total war has not recurred, suggesting the mandate succeeded and could be read as obsolete-by-success; but siege, starvation, and indiscriminate bombardment tactics persist in the asymmetric and civil conflicts the framework reaches imperfectly, suggesting the underlying problem has migrated rather than resolved. Classifying this as rope rather than snare or tangled_rope depends on taking seriously that the coordination benefit (reduced likelihood of civilizational-scale industrial war) is real and widely shared, not merely a cover story for great-power advantage — while still registering, via the revisionist_powers victim declaration, that the norm is not costless to enforce and falls asymmetrically on states seeking to revise the post-1945 settlement by force.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_norm_vs_physical_deterrent_causal_priority,
    'Did Article 2(4) and IHL development actually cause the disappearance of total war from legitimate practice, or did they codify a change already driven by nuclear deterrence (structural_contraction_reading) or shifting strategic culture (strategic_culture_drift), making the legal architecture epiphenomenal rather than causal?',
    'Comparative historical analysis of state behavior in the pre-nuclear interwar period under early legal restraint attempts (e.g., interwar disarmament treaties) versus post-1945 behavior; and examination of whether non-nuclear middle powers show the same total-war abstention pattern as nuclear powers, which would support the normative rather than structural reading.',
    'If the legal architecture is shown to be primarily epiphenomenal to nuclear deterrence, this reading''s claimed coordination function collapses and the constraint would be better understood as a Rope-flavored gloss on a structural_contraction reality, sharply reducing its independent explanatory and causal weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_norm_vs_physical_deterrent_causal_priority, conceptual, 'Whether the normative reading has independent causal force or merely narrates a structural/ideational change.').

omega_variable(
    victors_settlement_vs_universal_coordination,
    'Is Article 2(4) and the associated IHL framework better understood as genuine universal coordination against a mutually destructive equilibrium, or as a settlement imposed by 1945''s victors that happens to also suppress a coercive tool category — i.e., is the coordination story cover for a distributional lock-in favoring incumbent great powers?',
    'Examine whether the framework''s obligations fall symmetrically on drafting powers and later entrants; track enforcement asymmetry between Security Council permanent members and other states when total-war-adjacent violations occur.',
    'If enforcement is shown to be substantially asymmetric and drafting-power-favoring, the classification would trend from rope toward tangled_rope, since a genuine coordination function could coexist with entrenched asymmetric benefit for the founding parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victors_settlement_vs_universal_coordination, empirical, 'Whether the norm is neutral coordination or victors''-settlement coordination with a distributional tilt.').

omega_variable(
    framing_choice_kernel_vs_authority,
    'Should this constraint be framed around the treaty text (Article 2(4), Geneva Conventions) as the kernel, or around the broader legitimacy narrative of a ''rules-based international order'' that the treaty text is invoked to support — since the latter framing would surface a different beneficiary structure (institutions whose authority depends on the order''s perceived legitimacy) than the former (states and civilians directly covered by the treaty obligations)?',
    'Compare classification outcomes under a kernel defined strictly as the codified treaty text (formalized, fixed_text-adjacent) versus a kernel defined as the diffuse legitimacy narrative of the postwar order (distributed, harder to pin down); check whether beneficiary sets and ε diverge materially between the two framings.',
    'The treaty-text framing (adopted here) supports a cleaner rope classification with concentrated, traceable beneficiaries; the legitimacy-narrative framing would diffuse the beneficiary set toward the entire postwar institutional order and likely raise theater_ratio, since legitimacy-narrative invocation is more susceptible to selective/performative use than the treaty text itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_choice_kernel_vs_authority, conceptual, 'Alternative framing of the kernel (codified text vs. diffuse legitimacy narrative) and its effect on classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__normative_reading_drop, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1960, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(tota_tr_t1975, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(tota_tr_t1990, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1990, 0.17).
narrative_ontology:measurement(tota_tr_t2005, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2005, 0.19).
narrative_ontology:measurement(tota_tr_t2025, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(tota_be_t1960, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1960, 0.18).
narrative_ontology:measurement(tota_be_t1975, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1975, 0.2).
narrative_ontology:measurement(tota_be_t1990, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement(tota_be_t2005, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2005, 0.25).
narrative_ontology:measurement(tota_be_t2025, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1945, 0.3).
narrative_ontology:measurement(tota_su_t1960, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1960, 0.33).
narrative_ontology:measurement(tota_su_t1975, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1975, 0.36).
narrative_ontology:measurement(tota_su_t1990, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement(tota_su_t2005, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(tota_su_t2025, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__normative_reading_drop, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, structural_contraction_reading).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, strategic_culture_drift).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the total_war_winnability_post1945 kernel, each authored as a separate ε-invariant story per the ε-invariance principle: normative_reading_drop (this story, Rope-class, legal/normative causal claim), structural_contraction_reading (nuclear deterrence physically removed total war from reachable space), and strategic_culture_drift (ideational shift in elite strategic culture). The three do not average into one constraint; they are linked here so contamination and coupling analysis can trace how a purity change in one reading's evidentiary support propagates to the others' plausibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
