% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-22
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__performance_only, []).

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
 *   constraint_id: kodashim_commandment_status__performance_only
 *   human_readable: Sacrifice Laws Suspended Without Temple (Performance-Only Reading)
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   This constraint story represents the 'performance-only' reading of the
 *   kodashim commandment status kernel: sacrifice laws are contingent on
 *   Temple existence; without a functioning altar, the commandment to offer
 *   sacrifices is suspended (husk). The arrangement persists as intensive
 *   study of inoperative laws (Tractates Zevachim, Menachot, Tamid, Middot,
 *   Keritot, Me'ilah) within the yeshiva curriculum and halakhic discourse.
 *   The claimed type is piton — a former coordination mechanism (Temple
 *   service) whose function has atrophied completely, leaving theatrical
 *   maintenance (study-as-performance) sustained by institutional inertia and
 *   identity-locked scholarly investment. Extraction accumulates over
 *   centuries as scholarly labor is diverted from live halakhic questions
 *   (agunah, conversion, medical ethics, economic justice) to the mastery of
 *   laws that cannot be performed. Suppression is low because no active
 *   coercion prevents exit — the constraint persists through professional
 *   identity fusion, career path dependence, and the epistemic authority
 *   structure that treats kodashim mastery as a prerequisite for halakhic
 *   legitimacy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, 0.68).
domain_priors:suppression_score(kodashim_commandment_status__performance_only, 0.22).
domain_priors:theater_ratio(kodashim_commandment_status__performance_only, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, extractiveness, 0.68).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__performance_only, piton).
narrative_ontology:human_readable(kodashim_commandment_status__performance_only, "Sacrifice Laws Suspended Without Temple (Performance-Only Reading)").
narrative_ontology:topic_domain(kodashim_commandment_status__performance_only, "religious/halakhic/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__performance_only, 'acd9e9c7-07fe-4da0-ba69-8605b94f9107').
narrative_ontology:cs_kernel_codification('acd9e9c7-07fe-4da0-ba69-8605b94f9107', fixed_text).
narrative_ontology:cs_authority_grounding('acd9e9c7-07fe-4da0-ba69-8605b94f9107', lineage).
narrative_ontology:cs_interpretation_layer_present('acd9e9c7-07fe-4da0-ba69-8605b94f9107').
narrative_ontology:cs_reading_relation('acd9e9c7-07fe-4da0-ba69-8605b94f9107', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('acd9e9c7-07fe-4da0-ba69-8605b94f9107', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_axiom('acd9e9c7-07fe-4da0-ba69-8605b94f9107', foundational, commandment_suspended_without_temple).
narrative_ontology:cs_axiom_status(commandment_suspended_without_temple, holdable).
narrative_ontology:cs_axiom_grounding('acd9e9c7-07fe-4da0-ba69-8605b94f9107', commandment_suspended_without_temple, conventional).
narrative_ontology:cs_axiom('acd9e9c7-07fe-4da0-ba69-8605b94f9107', foundational, study_of_husk_is_not_fulfillment).
narrative_ontology:cs_axiom_status(study_of_husk_is_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('acd9e9c7-07fe-4da0-ba69-8605b94f9107', study_of_husk_is_not_fulfillment, deontological).
narrative_ontology:cs_reference_frame('acd9e9c7-07fe-4da0-ba69-8605b94f9107', temple_service_operative).
narrative_ontology:cs_drift_state('acd9e9c7-07fe-4da0-ba69-8605b94f9107', contemporary_post_temple, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('acd9e9c7-07fe-4da0-ba69-8605b94f9107', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__performance_only, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, halakhic_scholars_preserving_kodashim).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, yeshiva_institutions_kodashim_curriculum).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, rabbinic_authority_structure).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, scholarly_labor_diverted_to_kodashim).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, contemporary_halakhic_questions_neglected).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, resource_claimants_on_scholarly_attention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars whose professional legitimacy, communal recognition, and epistemic authority are gated by mastery of kodashim tractates. They invest decades in mastering laws that cannot be performed. Exit means forfeiting the credential that defines their professional identity and authority within the halakhic system. The study is intellectually rigorous and personally meaningful, but its referent (Temple service) has been absent for 1950 years.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, halakhic_scholars_preserving_kodashim, beneficiary,
    organized, biographical, identity_locked, global).

% Yeshivot that structure their advanced curriculum around kodashim tractates as a prerequisite for ordination and advanced standing. The curriculum is self-reproducing: faculty are drawn from kodashim masters, ordination requires kodashim proficiency, and the institutional identity is bound to maintaining the full traditional curriculum. Redirecting curricular resources would require restructuring the entire prestige economy of the yeshiva system.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, yeshiva_institutions_kodashim_curriculum, agenda_setter,
    institutional, generational, constrained, global).

% The rabbinic authority structure (chief rabbinates, batei din, poskim networks) that treats kodashim mastery as a gatekeeping criterion for halakhic legitimacy. This structure benefits from a stable, difficult, tradition-anchored barrier to entry that regulates who may issue binding rulings. The gate is self-justifying: it selects for those who have invested in the tradition's own self-reproduction.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, rabbinic_authority_structure, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__performance_only, rabbinic_authority_structure, agenda_setter).

% The collective scholarly capacity — the most talented, dedicated, and rigorously trained minds in the halakhic world — that is absorbed by the kodashim apparatus. Each scholar who masters Zevachim and Menachot is a scholar not addressing agunah crises, conversion standards, medical ethics, economic justice, or technology halakha. The cost is opportunity cost: live halakhic questions go undertheorized because the prestige economy rewards kodashim mastery. Exit is constrained because the prestige economy itself is the gate.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, scholarly_labor_diverted_to_kodashim, payer,
    powerful, biographical, constrained, global).

% Live halakhic questions (agunah, conversion, end-of-life, reproductive technology, digital commerce, environmental ethics) that receive insufficient scholarly attention because the field's prestige and curricular resources are locked into kodashim. These questions affect millions of observant Jews daily. The victims are not a unified group but a dispersed set of questioners and communities whose needs are met with recycled precedents rather than fresh analysis.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, contemporary_halakhic_questions_neglected, payer,
    moderate, biographical, constrained, global).

% Communities and individuals who need halakhic guidance on pressing contemporary issues but find the scholarly apparatus oriented toward inoperative laws. They have no voice in curricular decisions, no leverage over yeshiva priorities, and no alternative halakhic infrastructure. Their exclusion is structural: the halakhic system's authority derives from the very tradition that marginalizes their needs.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, resource_claimants_on_scholarly_attention, excluded,
    powerless, immediate, trapped, local).

% External observer tracing the 1950-year trajectory of a constraint whose coordination referent vanished at T=0 (70 CE) but whose maintenance apparatus expanded in scope, intensity, and resource capture. Sees the full structural arc: from genuine coordination (Temple service) through degraded coordination (Yavneh-era preservation) to theatrical maintenance (medieval-to-modern yeshiva curriculum) to piton (contemporary resource diversion with no functional referent).
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: centralized sacrificial worship coordinating the Israelite polity around a single ritual center (Temple), maintaining tribal cohesion, and mediating divine-human relationship through prescribed performance. Post-70 CE: preservation of textual memory and analytic tradition against loss during exile and persecution.
% TRANSFER_FUNCTION: Moves scholarly labor, curricular time, institutional prestige, and communal resources from live halakhic questions (agunah, conversion, medical ethics, economic justice, technology) to the mastery of inoperative sacrificial laws. The transfer is not monetary but capacitative: the field's best minds spend their prime years on laws with no performative referent.
% ABSENT_VOICES: The communities and individuals needing halakhic guidance on contemporary issues (agunot, converts, patients, workers, families) who are not represented in yeshiva curriculum committees or rabbinic ordination boards. Their needs are structurally invisible to the apparatus that allocates scholarly attention. Also absent: scholars who would pursue live halakhic innovation but are filtered out by the kodashim gate.
% DISAPPEARANCE_RATIONALE: If the kodashim curriculum requirement vanished overnight, yeshivot would restructure advanced learning around live halakhic questions within a generation. Scholarly labor would redirect to agunah, conversion, medical ethics, technology halakha. The halakhic system's responsiveness to contemporary life would increase. The prestige economy of rabbinic authority would lose its traditional gate and need a new one. The world of halakhic discourse would rearrange substantially.
% FOUNDING_PROBLEM: Centralized sacrificial worship at the Jerusalem Temple coordinating the Israelite polity around a single ritual center, maintaining tribal cohesion, and mediating the divine-human relationship through prescribed performance.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's death is attested by the historical record: the Temple was destroyed in 70 CE and has not been rebuilt for 1950 years. No living halakhic authority claims the Temple exists or that sacrificial service is currently performable. Even the messianic_deferral reading acknowledges the problem is currently dead — it argues for readiness, not current performance. The performance_only and study_as_performance readings agree the founding problem is dead; they differ on whether the kernel remains occupied.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__performance_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(kodashim_commandment_status__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__performance_only, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from near-zero (Temple standing) to 0.68 as the scholarly apparatus maintains itself on an obsolete function. Theater ratio reaches 0.78 because the performance of studying inoperative laws substitutes for the coordination function it once served — the study is real, but its referent is absent. Suppression remains low (0.22) because exit is structurally possible (scholars could redirect effort) but identity-locked: professional legitimacy, communal recognition, and epistemic authority are gated by kodashim mastery. Accessibility collapse is moderate (0.35) because alternative scholarly agendas exist but are marginalized by the curriculum structure. Resistance is moderate (0.42) from reform-minded scholars and those pressing live halakhic needs, but dispersed and non-coordinated.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars and yeshiva institutions are structural beneficiaries (d ~ 0.15): they collect professional legitimacy, curriculum stability, and authority from maintaining the kodashim apparatus. The scholarly labor diverted to kodashim is the payer seat (d ~ 0.85): their career capital is invested in mastering inoperative laws instead of addressing live questions. Rabbinic authority structure benefits indirectly (d ~ 0.25) by preserving the epistemic gate that kodashim mastery represents. Contemporary halakhic questioners and resource claimants on scholarly attention are victims (d ~ 0.8) — they bear the opportunity cost of scholarly capacity locked into husk laws. The analytical observer sees the full structure: a constraint whose coordination referent vanished in 70 CE but whose maintenance apparatus expanded for 19 centuries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (centralized sacrificial worship) is dead — the Temple was destroyed and not restored. The arrangement persists because the scholarly class that maintains it has fused its professional identity with the performance. This is not coordination (no collective action problem solved), not extraction in the active sense (no one is coerced), but theatrical maintenance of an atrophied function. The mandate has resolved into inertia; the constraint is a piton. The high theater ratio and rising extractiveness over time confirm the Goodhart drift: the proxy goal (mastery of kodashim texts) has replaced the real function (actual sacrificial service), and the system now optimizes for the proxy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_ambiguity,
    'Does the performance-only reading describe a genuine structural constraint on scholarly labor, or is it a meta-reading that contests the kernel''s own self-understanding?',
    'Trace whether the performance-only position is held by any living halakhic authority as their operative framework, or whether it exists only as an external analytical characterization of the other two readings.',
    'If no authority holds this reading operatively, the constraint story models an analytical projection rather than a live commitment-system seat — the engine''s per-seat computation would have no occupant for this reading''s computed type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the performance-only reading is a live halakhic position or an external critique').

omega_variable(
    study_as_coordination_vs_extraction,
    'Does the intensive study of inoperative kodashim laws serve a genuine coordination function (maintaining textual continuity, training analytic rigor, preserving communal boundaries) that justifies its resource consumption?',
    'Measure scholarly output and communal cohesion in communities that maintain full kodashim curriculum versus those that have abbreviated or redirected it, controlling for other variables.',
    'If study provides genuine coordination benefits, extractiveness is overstated — the constraint is a degraded rope, not a piton. If no measurable coordination benefit exists beyond identity maintenance, the piton classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_as_coordination_vs_extraction, empirical, 'Whether kodashim study has residual coordination value beyond theatrical maintenance').

omega_variable(
    identity_lock_mechanism,
    'What specific identity-fusion mechanism binds scholars to kodashim mastery — professional credentialing, communal recognition, epistemic authority, or theological self-concept?',
    'Survey career trajectories: do scholars who specialize in live halakhic areas (medical ethics, agunah) without kodashim mastery achieve equivalent professional recognition and authority?',
    'If identity lock is primarily professional credentialing, exit becomes possible if credentialing structures change. If theological self-concept, exit requires identity rupture — a deeper lock. This modulates the effective directionality for the payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'The specific mechanism of identity lock for scholars in the kodashim apparatus').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__performance_only, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_commandment_status__performance_only_tr_t0, kodashim_commandment_status__performance_only, theater_ratio, 0, 0.1).
narrative_ontology:measurement(kodashim_commandment_status__performance_only_tr_t650, kodashim_commandment_status__performance_only, theater_ratio, 650, 0.25).
narrative_ontology:measurement(kodashim_commandment_status__performance_only_tr_t1100, kodashim_commandment_status__performance_only, theater_ratio, 1100, 0.48).
narrative_ontology:measurement(kodashim_commandment_status__performance_only_tr_t1500, kodashim_commandment_status__performance_only, theater_ratio, 1500, 0.62).
narrative_ontology:measurement(kodashim_commandment_status__performance_only_tr_t1800, kodashim_commandment_status__performance_only, theater_ratio, 1800, 0.73).
narrative_ontology:measurement(kodashim_commandment_status__performance_only_tr_t1950, kodashim_commandment_status__performance_only, theater_ratio, 1950, 0.78).

% Extraction over time
narrative_ontology:measurement(kodashim_commandment_status__performance_only_be_t0, kodashim_commandment_status__performance_only, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(kodashim_commandment_status__performance_only_be_t650, kodashim_commandment_status__performance_only, base_extractiveness, 650, 0.35).
narrative_ontology:measurement(kodashim_commandment_status__performance_only_be_t1100, kodashim_commandment_status__performance_only, base_extractiveness, 1100, 0.52).
narrative_ontology:measurement(kodashim_commandment_status__performance_only_be_t1500, kodashim_commandment_status__performance_only, base_extractiveness, 1500, 0.61).
narrative_ontology:measurement(kodashim_commandment_status__performance_only_be_t1800, kodashim_commandment_status__performance_only, base_extractiveness, 1800, 0.65).
narrative_ontology:measurement(kodashim_commandment_status__performance_only_be_t1950, kodashim_commandment_status__performance_only, base_extractiveness, 1950, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_commandment_status__performance_only_su_t0, kodashim_commandment_status__performance_only, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(kodashim_commandment_status__performance_only_su_t650, kodashim_commandment_status__performance_only, suppression_requirement, 650, 0.1).
narrative_ontology:measurement(kodashim_commandment_status__performance_only_su_t1100, kodashim_commandment_status__performance_only, suppression_requirement, 1100, 0.15).
narrative_ontology:measurement(kodashim_commandment_status__performance_only_su_t1500, kodashim_commandment_status__performance_only, suppression_requirement, 1500, 0.18).
narrative_ontology:measurement(kodashim_commandment_status__performance_only_su_t1800, kodashim_commandment_status__performance_only, suppression_requirement, 1800, 0.2).
narrative_ontology:measurement(kodashim_commandment_status__performance_only_su_t1950, kodashim_commandment_status__performance_only, suppression_requirement, 1950, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__performance_only, 0.1).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__study_as_performance).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__messianic_deferral).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, halakhic_curriculum_structure).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, rabbinic_ordination_requirements).

% DUAL FORMULATION NOTE:
% This story decomposes the 'kodashim commandment status' kernel into three readings. The performance_only reading treats the kernel as a husk — the commandment is suspended without the Temple, and continued study is theatrical maintenance of an atrophied function. The study_as_performance reading treats study as fulfillment. The messianic_deferral reading treats suspension as temporary readiness. Each has distinct ε, victim/beneficiary structures, and computed types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_commandment_status__performance_only, organized, 0.15).
constraint_indexing:directionality_override(kodashim_commandment_status__performance_only, institutional, 0.25).
constraint_indexing:directionality_override(kodashim_commandment_status__performance_only, powerful, 0.85).
constraint_indexing:directionality_override(kodashim_commandment_status__performance_only, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
