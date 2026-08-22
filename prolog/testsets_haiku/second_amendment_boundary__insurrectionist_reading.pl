% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__insurrectionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__insurrectionist_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: second_amendment_boundary__insurrectionist_reading
 *   human_readable: Constitutional Right to Armed Resistance Against Tyranny (Insurrectionist Reading)
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   The Second Amendment's operative text ('the right of the people to keep
 *   and bear Arms, shall not be infringed') is read under the insurrectionist
 *   interpretation as granting individuals a constitutional right to possess
 *   military-grade weapons, with the purpose of maintaining capacity for
 *   armed resistance against a tyrannical government. This reading treats the
 *   'well regulated Militia' prefatory clause as stating a purpose
 *   (preserving the ability to resist tyranny) rather than as a limiting
 *   condition on the right. The constraint under this reading is TANGLED
 *   ROPE: it coordinates a political philosophy (citizens can check tyranny
 *   through armed readiness) while extracting costs from the state security
 *   apparatus (which must operate under threat of armed challenge and
 *   constrained authority to disarm) and from civilians exposed to armed
 *   conflict risk. The insurrectionist reading is one of three readings of
 *   the Second Amendment kernel; it coexists with individual-rights and
 *   militia-conditioned readings held by different constitutional
 *   communities.
 *
 * KEY AGENTS:
 *   - armed_citizens_militia_claimants: beneficiaries of the right claim; organized but identity-locked into the worldview that makes the claim coherent
 *   - state_security_apparatus: target bearing enforcement costs and constrained disarmament authority
 *   - civilians_in_conflict_zones: exposed to the contingent costs of armed resistance if operationalized
 *   - judicial_authority: agenda-setter determining which reading becomes operative law
 *   - competing_constitutional_readers: excluded by the insurrectionist framing but holding alternative readings of the same kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, 0.68).
domain_priors:suppression_score(second_amendment_boundary__insurrectionist_reading, 0.72).
domain_priors:theater_ratio(second_amendment_boundary__insurrectionist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(second_amendment_boundary__insurrectionist_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__insurrectionist_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__insurrectionist_reading, "Constitutional Right to Armed Resistance Against Tyranny (Insurrectionist Reading)").
narrative_ontology:topic_domain(second_amendment_boundary__insurrectionist_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(second_amendment_boundary__insurrectionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__insurrectionist_reading, '9a9db7de-c293-4501-b099-b5a007126edc').
narrative_ontology:cs_kernel_codification('9a9db7de-c293-4501-b099-b5a007126edc', fixed_text).
narrative_ontology:cs_authority_grounding('9a9db7de-c293-4501-b099-b5a007126edc', lineage).
narrative_ontology:cs_interpretation_layer_present('9a9db7de-c293-4501-b099-b5a007126edc').
narrative_ontology:cs_reading_relation('9a9db7de-c293-4501-b099-b5a007126edc', second_amendment_boundary__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a9db7de-c293-4501-b099-b5a007126edc', second_amendment_boundary__militia_conditioned_reading, coexists_with).
narrative_ontology:cs_axiom('9a9db7de-c293-4501-b099-b5a007126edc', foundational, armed_resistance_final_check_on_tyranny).
narrative_ontology:cs_axiom_status(armed_resistance_final_check_on_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('9a9db7de-c293-4501-b099-b5a007126edc', armed_resistance_final_check_on_tyranny, deontological).
narrative_ontology:cs_axiom('9a9db7de-c293-4501-b099-b5a007126edc', foundational, military_grade_arms_protected_for_resistance_capacity).
narrative_ontology:cs_axiom_status(military_grade_arms_protected_for_resistance_capacity, holdable).
narrative_ontology:cs_axiom_grounding('9a9db7de-c293-4501-b099-b5a007126edc', military_grade_arms_protected_for_resistance_capacity, deontological).
narrative_ontology:cs_reference_frame('9a9db7de-c293-4501-b099-b5a007126edc', founding_era_tyranny_deterrence_framework).
narrative_ontology:cs_drift_state('9a9db7de-c293-4501-b099-b5a007126edc', contemporary_institutional_robustness_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9a9db7de-c293-4501-b099-b5a007126edc', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__insurrectionist_reading, armed_citizens_militia_claimants).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, state_security_apparatus).
narrative_ontology:constraint_victim(second_amendment_boundary__insurrectionist_reading, civilians_in_conflict_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims a constitutional right to possess military-grade arms as a structural safeguard against government tyranny. Reads the Second Amendment as granting individuals the instrumentality to mount armed resistance if the state becomes oppressive. Frames disarmament efforts as tyranny precursors and views armed readiness as the ultimate check on state power. Their identity is constituted through the role of 'armed defender of liberty'; exit would mean abandoning the worldview that makes their claim intelligible.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, armed_citizens_militia_claimants, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__insurrectionist_reading, armed_citizens_militia_claimants, agenda_setter).

% Bears the structural cost of operating under the threat of armed resistance and faces legal/political constraints on disarmament or enforcement. Must maintain security operations while the insurrectionist reading treats many of those operations (licensing, restrictions, seizures) as illegitimate tyranny precursors. The reading's operationalization imposes costs: enforcement becomes contested, legitimacy of security measures is perpetually undermined, and the state cannot unilaterally disarm.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, state_security_apparatus, payer,
    institutional, generational, constrained, national).

% Bear the risk of armed conflict if the insurrectionist premise is operationalized—i.e., if organized armed resistance activity occurs against perceived tyranny. They are not participants in the armed readiness framework but are exposed to its contingent costs (crossfire, collateral damage, destabilization of civil order in the event of armed confrontation).
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, civilians_in_conflict_zones, payer,
    powerless, biographical, trapped, local).

% Hold alternative readings of the Second Amendment (militia-conditioning, individual-rights-but-regulated, historical-militia-only) but are excluded from the insurrectionist framing by the logical structure of that reading. They would argue for narrower scope or collective context but are positioned outside the discourse arena where insurrectionist claims dominate certain interpretive communities.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, competing_constitutional_readers, excluded,
    institutional, generational, constrained, national).

% Adjudicates which reading of the amendment is operative law. Courts have historically rejected pure insurrectionist readings in favor of individual-rights readings (with varying regulation scope); the insurrectionist reading persists as a live political-constitutional claim even when judicially subordinated. The authority structure is contested: insurrectionists argue courts are captured by tyranny-enabling interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__insurrectionist_reading, judicial_authority, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a structural premise that armed citizens constitute a check on government power, creating a deterrent against tyranny through the credible threat of armed resistance. The coordination problem solved is: how can a polity prevent state consolidation of power sufficient to enable systematic oppression, without relying solely on electoral/institutional checks?
% TRANSFER_FUNCTION: Transfers to organized armed citizens the authority to define when government has become sufficiently tyrannical to justify armed resistance, and the legal protection to maintain the weaponry necessary for that resistance. Transfers to the state the cost of operating under perpetual threat of armed challenge and constrained ability to disarm.
% ABSENT_VOICES: Civilians in conflict zones would object strongly to the operationalization of insurrectionist doctrine but are structurally excluded from the armed readiness framework. Victims of armed conflict in failed-state or civil-war contexts would attest to the severe costs of armed resistance doctrine; their testimony is present in comparative political science but absent from constitutional discourse in stable democracies. Alternative constitutional readings (militia-conditioned, heavily-regulated individual-rights) are excluded by the insurrectionist reading's framing.
% DISAPPEARANCE_RATIONALE: If the insurrectionist reading were definitively foreclosed (replaced by a militia-conditional or heavily-regulated interpretation), the armed readiness infrastructure would dissolve, political claims about tyranny-deterrence would lose constitutional grounding, and the balance of power between state security apparatus and organized armed groups would shift sharply toward the state. The constitutional legitimacy of the armed resistance posture would vanish overnight.
% FOUNDING_PROBLEM: The Framers faced a problem: how to constrain government power and prevent tyrannical consolidation without relying on the government's own forbearance. The insurrectionist reading holds that an armed citizenry capable of resistance is the ultimate structural check—more fundamental than electoral processes or constitutional limits, because it provides a final recourse when other checks fail.
% FOUNDING_PROBLEM_CORROBORATION: Some Founders (e.g. Jefferson) made insurrectionist statements; others did not. Historical scholarship is divided: some historians support insurrectionist grounding (Rakove, Bellesiles debate), others find it anachronistic or secondary to militia framing. The reading is corroborated by armed citizens' movements and some constitutional scholars but contested by alternative readings and contemporary security specialists who attest that armed resistance doctrine creates instability, not prevents tyranny.
narrative_ontology:disappearance_verdict(second_amendment_boundary__insurrectionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__insurrectionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__insurrectionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_boundary__insurrectionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__insurrectionist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__insurrectionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__insurrectionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__insurrectionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68 at interval end) reflects the constraint's operation under contested authority: the state cannot fully disarm despite security concerns because the insurrectionist reading claims constitutional protection for armed readiness. Suppression (0.72) is high because maintaining the constraint requires the state to tolerate organized armed groups and suppress its own disarmament authority—a high structural cost. Theater ratio (0.41) is moderate-to-high because the constraint's deterrent function is largely counterfactual: the insurrectionist reading has never been operationalized (no successful armed resistance against a US government) so much of the armed readiness framework operates as performance and symbolic affirmation rather than as functional deterrence. The measurement series tracks rising extractiveness from t=0 (lower articulation of armed readiness doctrine) through t=50 (higher organizational and political salience of insurrectionist claims), with theater ratio rising as the articulation becomes more theatrical and less functionally grounded. Suppression requirement rises as state security apparatus must invest more effort suppressing its own disarmament authority to maintain constitutional deference.
 *
 * PERSPECTIVAL GAP:
 *   The insurrectionist reading looks like ROPE from the beneficiary seat: citizens are coordinated around a shared constitutional authority and derive deterrent benefit from coordinated readiness without bearing direct cost. From the state security apparatus seat, the same structure looks like SNARE: the armed readiness constraint extracts obedience and suppresses disarmament authority, while the deterrent benefit (preventing tyranny) is hypothetical and never tested. From the judicial seat, the structure looks TANGLED: courts must balance coordination (citizens' rights) against extraction (state's costs and security risks), which is why judicial authority has generally rejected pure insurrectionist readings in favor of individual-rights readings with scope limitations. The engine computes these divergences from the structural data; the authored claim (tangled_rope) reflects the overall structure where coordination and extraction are both substantial and actively enforced.
 *
 * DIRECTIONALITY LOGIC:
 *   Armed citizens claiming the insurrectionist right are the beneficiary seat (d near 0.0): they gain the constitutional protection, the deterrent authority, and the political legitimacy of the reading. The state security apparatus is the target seat (d near 1.0): it bears the cost of constrained disarmament and perpetual threat. The judicial authority sits at moderate directionality (d ~0.5): courts are not beneficiaries or targets but are positioned to adjudicate between readings; they experience cost (managing contested authority) and benefit (preserving constitutional legitimacy through reasoned interpretation) in proportion. Identity-lock for armed citizens reflects that the insurrectionist reading constitutes their self-concept as 'armed defenders of liberty'; exit would mean abandoning the identity framework that makes the claim intelligible. This high identity-lock differentiates them from other powerful actors with mobile exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows moderate mandatrophy signals: the founding problem (preventing tyranny through structural deterrence) is contested in status (some argue it is live and essential, others argue modern institutional checks render it obsolete). The disappearance verdict (world_rearranges) reflects that the constraint's removal would shift power toward the state security apparatus. The contradiction between a potentially-dead founding problem and a world-rearranging disappearance verdict is the mark of a zombie constraint: the reason it was built for (deterring tyranny in the pre-constitutional-court era) may be obsolete, but the structural effect (armed readiness) persists because beneficiaries maintain it and the state cannot unilaterally remove it. Theater ratio rising over the interval (0.22 to 0.41) supports the mandatrophy reading: the constraint's deterrent function becomes increasingly performative as the insurrectionist reading is articulated in political rhetoric rather than operationalized in actual resistance. The constraint is not yet in full mandatrophy (it still has functional effects—state behavior is constrained, armed readiness is maintained, civilians are exposed to risk) but is migrating toward piton classification if theater ratio continues rising and the founding problem remains definitively dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_doctrine_boundary,
    'Is the insurrectionist reading a legitimate constitutional interpretation, or a doctrine imposed by political commitment on the text?',
    'Textual analysis comparing the reading''s claim (individual right to military-grade arms for resistance) to the text''s language (prefatory militia clause, operative keep-and-bear-arms clause) and historical context. Competing scholarly methods will produce conflicting resolutions.',
    'If the reading is deemed doctrinally imposed rather than textually grounded, its constitutional status weakens; if deemed a defensible interpretation, alternative readings (militia-conditioned) lose foreclosing power. This determines whether sibling readings coexist_with or foreclose this one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_doctrine_boundary, conceptual, 'Whether insurrectionist premise is texturally defensible or politically imposed.').

omega_variable(
    tyranny_definition_operationalizability,
    'What constitutes ''tyranny'' sufficient to justify armed resistance under this reading? Is the criterion shareable across political communities, or does each faction define tyranny to fit its interests?',
    'Compare insurrectionist claims about what government actions constitute tyranny across different political periods and movements. If the definition shifts with partisan interest, the criterion is not operationalizable as constitutional law; if it stabilizes around a core (e.g., systematic denial of electoral participation, enslavement, genocide), it may be.',
    'High operationalizability supports the reading as a coherent constitutional constraint; low operationalizability reveals it as a cover for political resistance claims that lack constitutional grounding. Low operationalizability also increases extraction (the armed readiness framework extracts without coherent activation criteria) and theater ratio (the deterrent function becomes performative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tyranny_definition_operationalizability, empirical, 'Whether tyranny criterion is stable or shifts with partisan interest.').

omega_variable(
    armed_resistance_efficacy_empirical,
    'Does an armed citizenry actually deter tyranny, or does armed resistance capacity increase state repression, civil violence, and regime instability without improving outcomes for civilians?',
    'Comparative political science: does armed resistance capacity in democracies correlate with lower tyranny risk, or with higher civil violence and failed-state outcomes? Case studies from Switzerland (armed citizen militias, low tyranny), US (armed citizens, no tyranny so far), failed states (high arms prevalence, high tyranny), and democratic collapses with armed resistance (Weimar, Spain, failed to prevent fascism).',
    'If empirical evidence shows armed resistance deters tyranny without increasing violence, the reading''s beneficiary framing (armed citizens) and victim set (state apparatus) stabilizes. If evidence shows armed resistance increases violence without preventing tyranny, the victim set expands (civilians in conflict zones become primary victims, not externalities), and the classification shifts from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(armed_resistance_efficacy_empirical, empirical, 'Empirical efficacy of armed populace in preventing tyranny.').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Is this reading a genuine alternative interpretation of the Second Amendment kernel, or does the insurrectionist reading redefine the kernel itself (shifting from ''right to arms'' to ''right to armed resistance'')? Are the three sibling readings addressing the same kernel or three different kernels?',
    'Textual comparison: does each reading trace its authority to the same text (the Second Amendment) and disagree only on scope/meaning, or do the readings invoke different textual fragments as the kernel (prefatory clause vs. operative clause as primary)? If different kernels, this story and its siblings decompose into separate constraint families per ε-invariance principle.',
    'If the readings share a single kernel with different interpretations, the family structure holds and reading_relations (coexists_with, forecloses, influences) apply. If the readings invoke different kernels, the family dissolves and each becomes an independent constraint with separate ε and separate cs_structure blocks—a different set of constraint_ids altogether.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Whether the three readings share one kernel or three kernels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__insurrectionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__insurrectionist_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(seco_tr_t10, second_amendment_boundary__insurrectionist_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(seco_tr_t20, second_amendment_boundary__insurrectionist_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement(seco_tr_t30, second_amendment_boundary__insurrectionist_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(seco_tr_t40, second_amendment_boundary__insurrectionist_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(seco_tr_t50, second_amendment_boundary__insurrectionist_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(seco_be_t10, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(seco_be_t20, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(seco_be_t30, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(seco_be_t40, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(seco_be_t50, second_amendment_boundary__insurrectionist_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(seco_su_t10, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(seco_su_t20, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(seco_su_t30, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(seco_su_t40, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(seco_su_t50, second_amendment_boundary__insurrectionist_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__insurrectionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_boundary__insurrectionist_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__insurrectionist_reading, second_amendment_boundary__militia_conditioned_reading).

% DUAL FORMULATION NOTE:
% The three constraints in the second_amendment_boundary kernel family (individual_right_reading, insurrectionist_reading, militia_conditioned_reading) are readings of the same disputed text with fundamentally different ε values and victim/beneficiary structures. They are not the same constraint viewed from different seats; they are different constraints instantiated by different interpretations of the kernel. Each story independently asserts what the constraint is and who bears its costs. The insurrectionist reading is substantially more extractive (ε=0.68) and has a clearer victim set (state apparatus, civilians in conflict) than the individual-rights reading (which is less extractive because it permits regulation) or the militia-conditioned reading (which has minimal extraction because the scope is bounded to collective defense). The network links are not causal influence in the usual sense; they are structural contestation: the readings contest each other for authority over how the kernel is read, and the reading that prevails as operative law constrains the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
