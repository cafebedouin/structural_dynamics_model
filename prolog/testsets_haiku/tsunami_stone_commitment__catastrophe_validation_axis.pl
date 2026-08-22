% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__catastrophe_validation_axis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__catastrophe_validation_axis, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: tsunami_stone_commitment__catastrophe_validation_axis
 *   human_readable: Tsunami Stone Commitment: Catastrophe Validation Axis
 *   domain: disaster anthropology / commitment systems
 *
 * SUMMARY:
 *   Japanese coastal communities have preserved stone monuments inscribed
 *   with warnings about historical tsunamis, some dating back centuries
 *   (Anping Stone, Miyako stones, others). These stones mark safe settlement
 *   elevations and encode ancestral knowledge of tsunami periodicity and
 *   reach. The 2011 Tōhoku tsunami provided an empirical test: did
 *   communities with these stones evacuate faster and survive at higher rates
 *   than comparable communities without them? This reading treats the 2011
 *   catastrophe as a decisive binary validation of whether the
 *   stone-inscription commitment system retains binding behavioral force
 *   across generations of peace. The 2011 event is the empirical referent;
 *   the constraint under this reading is the physical and cultural encoding
 *   device (stones + oral transmission + settlement patterns) and its
 *   measurable effect on behavior under catastrophic stress.
 *
 * KEY AGENTS:
 *   - Coastal communities in Anping, Sendai, Miyako, and other sites with historical tsunami stones: observed the constraint's predictive power in real time
 *   - Stone inscriptions and oral traditions: the physical commitment device under evaluation
 *   - Historical tsunami record (prior 1700s, 1800s, 1900s events): the memory encoded in stone and settlement
 *   - 2011 Tōhoku tsunami: the exogenous shock providing the empirical test
 *   - Disaster researchers (anthropologists, historians, disaster scientists): the empirical adjudicators who measured evacuation correlations and mortality rates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__catastrophe_validation_axis, 0.0).
domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, 0.0).
domain_priors:theater_ratio(tsunami_stone_commitment__catastrophe_validation_axis, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, extractiveness, 0.0).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__catastrophe_validation_axis, mountain).
narrative_ontology:human_readable(tsunami_stone_commitment__catastrophe_validation_axis, "Tsunami Stone Commitment: Catastrophe Validation Axis").
narrative_ontology:topic_domain(tsunami_stone_commitment__catastrophe_validation_axis, "disaster anthropology / commitment systems").

domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__catastrophe_validation_axis, 'e79534d0-722c-4580-9081-9572b21ce950').
narrative_ontology:cs_kernel_codification('e79534d0-722c-4580-9081-9572b21ce950', fixed_text).
narrative_ontology:cs_authority_grounding('e79534d0-722c-4580-9081-9572b21ce950', practice).
narrative_ontology:cs_interpretation_layer_present('e79534d0-722c-4580-9081-9572b21ce950').
narrative_ontology:cs_reading_relation('e79534d0-722c-4580-9081-9572b21ce950', tsunami_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('e79534d0-722c-4580-9081-9572b21ce950', tsunami_stone_commitment__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('e79534d0-722c-4580-9081-9572b21ce950', foundational, catastrophic_events_empirically_validate_ancestral_memory).
narrative_ontology:cs_axiom_status(catastrophic_events_empirically_validate_ancestral_memory, holdable).
narrative_ontology:cs_axiom_grounding('e79534d0-722c-4580-9081-9572b21ce950', catastrophic_events_empirically_validate_ancestral_memory, empirically_contingent).
narrative_ontology:cs_axiom('e79534d0-722c-4580-9081-9572b21ce950', foundational, id_2011_tsunami_is_decisive_test_of_commitment_binding).
narrative_ontology:cs_axiom_status(id_2011_tsunami_is_decisive_test_of_commitment_binding, holdable).
narrative_ontology:cs_axiom_grounding('e79534d0-722c-4580-9081-9572b21ce950', id_2011_tsunami_is_decisive_test_of_commitment_binding, empirically_contingent).
narrative_ontology:cs_reference_frame('e79534d0-722c-4580-9081-9572b21ce950', ancestral_catastrophe_memory_binding_behavior).
narrative_ontology:cs_drift_state('e79534d0-722c-4580-9081-9572b21ce950', post_2011_tsunami_empirical_validation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e79534d0-722c-4580-9081-9572b21ce950', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__catastrophe_validation_axis, intergenerational_behavioral_transmission_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities in Anping, Sendai, and other sites with historical tsunami stones faced the 2011 event. Their behavior — whether they evacuated or sheltered in place — served as the empirical test of whether stone inscriptions and ancestral memory encoding provided binding behavioral force or remained symbolic decoration.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, coastal_communities_2011, observer,
    powerless, immediate, trapped, local).

% Physical monuments carved with warnings ('Do not build below this line,' 'High tides reach here,' 'Remember the calamity of the great tsunami'). Their existence as a constraint on human settlement and evacuation behavior is the subject of the catastrophe-validation reading.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, stone_inscriptions, beneficiary,
    analytical, civilizational, analytical, local).
narrative_ontology:stakeholder_non_agent(tsunami_stone_commitment__catastrophe_validation_axis, stone_inscriptions).

% The physical and cultural memory of prior tsunamis, encoded in stones and oral tradition. Its predictive power — whether prior catastrophes constrained 2011 behavior — is what the 2011 event empirically validated.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, historical_tsunami_record, agenda_setter,
    analytical, civilizational, analytical, local).
narrative_ontology:stakeholder_non_agent(tsunami_stone_commitment__catastrophe_validation_axis, historical_tsunami_record).

% Anthropologists, historians, and disaster scientists who studied the correlation between stone-inscription locations, community evacuation patterns, and survival outcomes in the 2011 tsunami. They served as the empirical adjudicators of competing readings.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, disaster_researchers, observer,
    analytical, biographical, analytical, global).

% Alternative explanations for evacuation behavior (modern warning systems, government mandates, random chance, social coordination independent of ancestral memory). These are excluded from the catastrophe-validation axis because they do not address whether the stone constraint itself had binding force.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, competing_institutional_narratives, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(tsunami_stone_commitment__catastrophe_validation_axis, competing_institutional_narratives).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stone inscriptions and oral traditions encode ancestral knowledge of tsunami periodicity and damage extent, creating a physical and cultural coordinating device that (if binding) aligns community settlement patterns and evacuation decisions to hazard-history rather than requiring real-time central authority.
% TRANSFER_FUNCTION: No transfer occurs in this reading. The constraint is a measurement device, not an extraction or coordination mechanism in the transactional sense. The 2011 tsunami provided the empirical datum for evaluating whether ancestral memory and stone monuments carry behavioral force across generations.
% ABSENT_VOICES: Communities that evacuated cite modern warning systems and government communication, not stone inscriptions; Japanese disaster-management authorities privilege technological infrastructure over folk memory. A reading that centralizes behavioral motivation in modern systems would argue for the exclusion of stone-memory as a causal factor. They are not present in the catastrophe-validation frame because that frame takes the stone inscription's potential binding force as the specific empirical question under test.
% DISAPPEARANCE_RATIONALE: The constraint is a natural physical and historical fact — the prior tsunamis happened, the stones were carved, the 2011 event occurred. Disappearing the constraint as an object of empirical analysis would not change the world; it would only change what we know about the world. The empirical test already occurred; its outcomes are recorded.
% FOUNDING_PROBLEM: Do long-duration intergenerational commitments — encoded in stone, oral tradition, and settlement patterns — persist as behavioral constraints across centuries of peace, such that communities with ancestral memory of disasters evacuate faster and suffer lower mortality than communities without such memory?
% FOUNDING_PROBLEM_CORROBORATION: The 2011 Tōhoku tsunami provided direct empirical evidence. Researchers (Ishikawa et al., Japanese disaster science, coastal anthropology teams) documented higher evacuation rates and lower mortality in communities with historical tsunami stones and oral traditions relative to otherwise similar communities. This corroboration comes from outside the 'behavioral competence' thesis beneficiaries — it is an observed empirical outcome, not a self-interested claim. The evidence shows the founding problem is measurable and not vacuous.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__catastrophe_validation_axis, world_unchanged).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__catastrophe_validation_axis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__catastrophe_validation_axis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tsunami_stone_commitment__catastrophe_validation_axis, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__catastrophe_validation_axis, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, ExtMetricName, E),
    domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading claims the constraint is a Mountain: a natural physical and historical fact (prior catastrophes occurred, stones were carved, memory was encoded, the 2011 event tested the system) with negligible extractiveness or suppression. Accessibility collapse is very high (0.92) because once a tsunami occurs, the question of whether intergenerational memory was binding is observable — alternatives (that modern systems alone drove behavior, that stones were symbolic only) become empirically testable. Resistance is very low (0.05) because the constraint is not enforced against anyone; it is an adjudication mechanism. The empirical outcome is measured, not contested in the catastrophe moment itself. The 2011 tsunami showed that communities with ancestral tsunami memory and stone-marked safe zones evacuated at higher rates and suffered lower mortality than baseline expectations, which supports the catastrophe-validation reading of the kernel. The constraint's claim as a Mountain is that this pattern is a durable feature of the system, not contingent on 2011 alone.
 *
 * PERSPECTIVAL GAP:
 *   From the catastrophe-validation reading's seat, the 2011 event is a definitive empirical test that the commitment persists. From the commemorative-husk reading's seat, 2011 proves only coincidental correlation — modern systems did the work, stones were incidental. From the behavioral-competence reading's seat, 2011 validates that intergenerational enforcement is real and measurable. Each reading computes a different classification of what the stones ARE from the same empirical event. The engine does not compute per-seat classifications of a Mountain (all seats see a Mountain), but the three readings represent three different commitments to what the constraint MEANS.
 *
 * DIRECTIONALITY LOGIC:
 *   This is a Mountain reading, so directionality is not modulated by beneficiary/victim extraction. The 'beneficiary' listed (intergenerational_behavioral_transmission_thesis) is a non-agent proposition, not a real actor collecting rents. The constraint serves as a measurement device for competing readings of the kernel. Its binding force is not directional in the sense of extracting from targets or subsidizing beneficiaries — it constrains all coastal communities equally by providing a physical and cultural test of behavioral formation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (do long-duration intergenerational commitments persist in behavior?) is live and directly addressed by the 2011 empirical test. The 2011 tsunami did not become mandatrophic; it was the scheduled adjudication event the system was designed for. The constraint shows no signs of divorced founding and actual operation — the founding problem and the 2011 outcome are tightly coupled. This reading shows zero mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selection_bias_in_survival_correlation,
    'Did communities with tsunami stones survive better in 2011 because the stones carried binding behavioral force, or because communities that survived prior tsunamis AND valued that survival were more likely to settle again in stone-inscribed locations?',
    'Counterfactual settlement analysis: examining whether communities WITHOUT stones in high-risk zones (if they exist or can be identified) showed different evacuation behavior, and whether modern communities relocating to stone-marked sites adopt the ancestral behavior or treat stones as arbitrary geography.',
    'If selection bias explains the correlation, the constraint is not binding force but filtering device — prior-catastrophe survivors self-selected into stone-marked zones, not vice versa. The reading would shift from ''stones constrain behavior'' to ''stones mark places where past behavior was effective.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selection_bias_in_survival_correlation, empirical, 'Whether tsunami-stone correlation reflects behavioral constraint or survivor selection.').

omega_variable(
    convergent_validity_of_catastrophe_test,
    'Is the 2011 tsunami a valid empirical test of commitment-system binding force, or does a single catastrophe conflate too many co-varying factors (modern warning systems, government messaging, psychological priming, demographic change) to isolate the stone-inscription effect?',
    'Multivariate analysis isolating evacuation behavior by prior-exposure cohorts; comparison with other recent tsunamis (2004 Indian Ocean, 2010 Chile) where stone-inscription presence varies but modern warning systems are constant; long-duration follow-up on 2011 reconstruction to measure whether stone-marked zones rebuild faster or slower than unmarked zones.',
    'If the stone effect cannot be isolated from modern systems, the catastrophe-validation axis becomes inconclusive — multiple constraint-types might generate the observed behavior. The reading would shift to ''evidence is suggestive, not determinative.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(convergent_validity_of_catastrophe_test, empirical, 'Whether 2011 tsunami is a sufficient empirical test of intergenerational commitment binding.').

omega_variable(
    intergenerational_memory_mechanism_underspecified,
    'What is the mechanism by which stone inscriptions constrain behavior? Does the constraint operate through conscious recall of the inscription''s warning, unconscious transmission of settlement-pattern habits, status-signaling alignment with ancestral settlement, or something else?',
    'Post-evacuation interviews with survivors and non-evacuees, examining reported knowledge of stone locations and inscriptions, memory of ancestors'' instructions, and decision-factors in evacuation choices. Ethnographic work on how stone-knowledge is transmitted within families.',
    'Clarifying the mechanism would distinguish between ''stones are binding commitments'' (conscious constraint) and ''stones are markers of safe settlement that persist through social learning'' (behavioral filter). Different mechanisms have different implications for the commitment-system reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_memory_mechanism_underspecified, conceptual, 'Causal pathway from stone inscription to behavior.').

omega_variable(
    alternative_reading_kernel_contest,
    'This constraint instantiates ONE reading of the tsunami-stone-commitment kernel. What are the conditions under which the competing readings (behavioral_competence_reading focusing on intergenerational enforcement, commemorative_husk_reading focusing on symbolic decay) would be vindicated instead?',
    'Empirical outcomes differing from the 2011 pattern: if a future tsunami finds communities with stones evacuating LESS readily (or not at all), or if detailed interviews show stones are no longer transmitted in family knowledge, the readings would shift. If stones show exponential decay in cultural salience post-2011, the husk reading gains ground.',
    'The catastrophe-validation axis treats 2011 as a decisive binary test. But future events could falsify or refine the reading. This omega documents that the reading''s validity is time-indexed and contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_kernel_contest, preference, 'Contest between sibling readings over kernel interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__catastrophe_validation_axis, 0, 11).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t0, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 0, 0.0).
narrative_ontology:measurement(tsun_tr_t11, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 11, 0.0).

% Extraction over time
narrative_ontology:measurement(tsun_be_t0, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(tsun_be_t11, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 11, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(tsun_su_t0, tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(tsun_su_t11, tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 11, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__catastrophe_validation_axis, attachment_coordination).
narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment__behavioral_competence_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The tsunami-stone-commitment kernel decomposes into three constraint stories, each instantiating a different reading of what the stones ARE and what the 2011 event proves about them. The catastrophe-validation-axis reading treats 2011 as a decisive empirical test. The behavioral-competence reading emphasizes intergenerational norm transmission. The commemorative-husk reading emphasizes symbolic decay and coincidental survival. All three share the same referent (the stones and their measurable effect on 2011 behavior) but author different ε values and different structural relationships to the kernel, because they read the evidence differently. No single ε correctly captures all three readings' positions — each reading instantiates its own constraint with its own ε. This reading (catastrophe-validation) claims ε ≈ 0.0 because the constraint is a natural physical fact (prior catastrophes, stones, 2011 event) with no extractive component — it is a measurement device. The behavioral-competence reading will claim higher ε if it emphasizes the constraint's role in enforcing behavioral alignment. The husk reading will claim lower ε if it emphasizes coincidence. The divergence is data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
