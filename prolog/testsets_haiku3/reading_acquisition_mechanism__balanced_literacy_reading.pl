% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__balanced_literacy_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: reading_acquisition_mechanism__balanced_literacy_reading
 *   human_readable: Balanced Literacy Reading Acquisition Model
 *   domain: educational/cognitive/literacy
 *
 * SUMMARY:
 *   This constraint describes the institutional adoption of 'balanced
 *   literacy'—a compromise framework requiring reading instruction to
 *   integrate explicit phonics and authentic literature exposure. The
 *   balanced literacy reading is one reading of the contested kernel
 *   'reading_acquisition_mechanism,' sibling to phonics-first and
 *   whole-language approaches. The institutional constraint differs from the
 *   cognitive scientific claim: research establishes that phonics MUST be
 *   foundational and systematic, with literature as supplementary; balanced
 *   literacy treats them as interchangeable components, opening the
 *   possibility of collapse toward whole-language practice. Teachers,
 *   especially in under-resourced schools, often implement this by
 *   de-emphasizing systematic phonics in favor of more familiar literature
 *   engagement—a form of implementation drift that harms struggling and
 *   dyslexic readers. The constraint is claimed as tangled_rope because it
 *   coordinates two legitimate pedagogical insights while extracting
 *   administrative authority from both cognitive science and student voice.
 *
 * KEY AGENTS:
 *   - institutional_literacy_administrators: set the mandate, control professional development, benefit from institutional coherence without defending either pure approach
 *   - teacher_training_programs: design curricula around the compromise, benefit from enrollment and alignment, constrained from choosing pure approaches
 *   - teachers: implement in classrooms, bear cognitive load of holding both approaches without clear integration, identity-locked to one pedagogical style or the other
 *   - struggling_readers / dyslexic_learners: depend entirely on teacher consistency in phonics instruction, bear cost when phonics is de-emphasized, trapped with no voice
 *   - cognitive_science_researchers: observe the constraint, produce evidence for phonics necessity, have limited enforcement power
 *   - literature_centered and phonics_specialist educators: excluded from institutional standard-setting but maintain organized advocacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, 0.58).
domain_priors:suppression_score(reading_acquisition_mechanism__balanced_literacy_reading, 0.62).
domain_priors:theater_ratio(reading_acquisition_mechanism__balanced_literacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__balanced_literacy_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__balanced_literacy_reading, "Balanced Literacy Reading Acquisition Model").
narrative_ontology:topic_domain(reading_acquisition_mechanism__balanced_literacy_reading, "educational/cognitive/literacy").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__balanced_literacy_reading, '3081fa31-3951-498b-84c9-608d71e5ae92').
narrative_ontology:cs_kernel_codification('3081fa31-3951-498b-84c9-608d71e5ae92', distributed).
narrative_ontology:cs_authority_grounding('3081fa31-3951-498b-84c9-608d71e5ae92', extraction).
narrative_ontology:cs_interpretation_layer_present('3081fa31-3951-498b-84c9-608d71e5ae92').
narrative_ontology:cs_reading_relation('3081fa31-3951-498b-84c9-608d71e5ae92', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('3081fa31-3951-498b-84c9-608d71e5ae92', reading_acquisition_mechanism__whole_language_reading, coexists_with).
narrative_ontology:cs_axiom('3081fa31-3951-498b-84c9-608d71e5ae92', foundational, phonics_and_literature_equally_necessary).
narrative_ontology:cs_axiom_status(phonics_and_literature_equally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('3081fa31-3951-498b-84c9-608d71e5ae92', phonics_and_literature_equally_necessary, empirically_contingent).
narrative_ontology:cs_axiom('3081fa31-3951-498b-84c9-608d71e5ae92', foundational, integration_method_orthogonal_to_outcomes).
narrative_ontology:cs_axiom_status(integration_method_orthogonal_to_outcomes, overridden).
narrative_ontology:cs_axiom_grounding('3081fa31-3951-498b-84c9-608d71e5ae92', integration_method_orthogonal_to_outcomes, empirically_contingent).
narrative_ontology:cs_reference_frame('3081fa31-3951-498b-84c9-608d71e5ae92', balanced_integration_paradigm).
narrative_ontology:cs_drift_state('3081fa31-3951-498b-84c9-608d71e5ae92', contemporary_cognitive_science_consensus_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3081fa31-3951-498b-84c9-608d71e5ae92', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, institutional_literacy_administrators).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__balanced_literacy_reading, teacher_training_programs).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, dyslexic_learners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__balanced_literacy_reading, teachers).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__balanced_literacy_reading, cognitive_science_phonemic_awareness_necessity).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__balanced_literacy_reading, literature_engagement_motivation_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets curriculum standards requiring balanced literacy (phonics + literature) and allocates professional development funding. Benefits from the compromise framing because it avoids defending either pure phonics (which faces resistance from literature-centered educators) or pure whole-language (which faces criticism from cognitive science). Controls what teachers are trained to do and how reading instruction time is distributed.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, institutional_literacy_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Design teacher certification curricula and coursework around balanced literacy. Benefit from the institutional mandate: it legitimizes their programs, generates enrollment, and allows them to avoid the contested theoretical ground (they teach 'both' approaches without fully integrating them). Their exit option—recommitting to either pure phonics or whole-language—would require wholesale curriculum redesign and loss of institutional alignment.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, teacher_training_programs, beneficiary,
    organized, generational, constrained, national).

% Implement balanced literacy in classrooms but often receive fragmented professional development and conflicting guidance on how to weight phonics vs. literature, leading to implementation fidelity problems. Bear the cognitive load of holding both approaches in tension without clear decision rules. Many teachers' professional identity is fused to either literature-centered or skills-centered pedagogy, making genuine integration difficult. Exit (committing to a pure approach) risks certification or evaluation problems.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, teachers, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__balanced_literacy_reading, teachers, observer).

% When phonics instruction is insufficient or poorly sequenced (collapse to whole-language in practice), they do not acquire foundational decoding skills and fall further behind. Trapped: cannot choose their school, teacher, or reading curriculum. No voice in instructional design decisions. Depend entirely on the consistency of teacher implementation.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Require explicit, intensive, multi-sensory phonics instruction structured around their specific phonological deficits. When balanced literacy collapses toward whole-language because systematic phonics is treated as optional or de-emphasized in practice, dyslexic learners are especially harmed—they cannot extract phonics principles implicitly from text exposure and fall into failure spirals. No voice in their own instructional pathway.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, dyslexic_learners, payer,
    powerless, biographical, trapped, local).

% Conduct empirical research on phonemic awareness, orthographic mapping, and reading development. Produce evidence supporting the necessity of explicit phonics for most learners and especially for dyslexic and struggling readers. Observe the constraint but have limited enforcement power; their findings are often cited but institutional adoption lags behind evidence.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, cognitive_science_researchers, observer,
    analytical, generational, analytical, global).

% Advocate for authentic literature engagement and whole-language approaches grounded in meaning-making. Excluded from the balanced literacy framing because their core premise (that phonics instruction can be implicit/emergent) is subordinated in the official mandate. Would argue for centering literature and learner agency; their voice is present in some professional communities but not in institutional curriculum standards.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, literature_centered_educators, excluded,
    organized, generational, constrained, national).

% Advocate for explicit, systematic, structured phonics instruction. Excluded from institutional-level constraint authorship; they argue the balanced literacy compromise under-specifies phonics systematicity and allows the constraint to collapse to whole-language in practice. Would mandate the phonics component as primary; instead, the compromise treats it as one of two equally-weighted dimensions.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__balanced_literacy_reading, phonics_specialist_educators, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__balanced_literacy_reading, institutional_literacy_administrators).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__balanced_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bridges two legitimate pedagogical insights—that reading requires decoding skill development AND meaning-making motivation—into a single instructional framework that serves both learners and institutional governance.
% TRANSFER_FUNCTION: Moves authority over reading-instruction design from cognitive science (which emphasizes phonics as foundational) and from student experience (which emphasizes engagement) to institutional curriculum committees that can claim to honor both without fully integrating them, retaining administrative control over what counts as 'balanced' implementation.
% ABSENT_VOICES: Struggling readers and dyslexic learners themselves—the populations most dependent on consistent phonics instruction—have no voice in curriculum design. Phonics specialists and whole-language advocates remain in organized communities but are excluded from institutional-level standard-setting; their disagreement is suppressed by the 'both/and' framing.
% DISAPPEARANCE_RATIONALE: If the balanced literacy mandate vanished, schools would split: some would adopt explicit phonics-first frameworks (improving outcomes for struggling readers and dyslexic learners but narrowing literature engagement); others would return to whole-language or reader-response models (maintaining literature engagement but potentially harming phonetically-vulnerable learners). Reading instruction would become a contested landscape instead of an administratively unified one. Institutional coherence would dissolve.
% FOUNDING_PROBLEM: Early reading instruction was split between two incompatible paradigms—phonics-focused (emphasizing decoding) and whole-language (emphasizing meaning and engagement)—with research suggesting both were partially correct and both were partially incomplete. Balanced literacy emerged as a compromise to honor both insights and provide coherent institutional guidance.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive science researchers, literacy practitioners, and teacher-training programs all attest the original problem is still live: reading development does involve both decoding skill and meaning-making engagement. However, independent cognitive-science meta-analyses and intervention studies (outside the institutional benefiting parties) show that the founding problem was solved by 2010s research clarifying the INTEGRATION method—that phonics must be foundational and literature must be supplementary, not equal—and the institutional constraint no longer tracks that scientific consensus. The constraint persists despite the founding problem being technically resolved.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__balanced_literacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__balanced_literacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__balanced_literacy_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__balanced_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects moderate asymmetry: institutional administrators and teacher-training programs benefit from the coherence and avoid contested theoretical ground; teachers carry implementation burden and identity conflict; struggling readers carry the cost of implementation fidelity collapse. Suppression (0.62) is moderate-high because the constraint's persistence depends on suppressing the cognitive-science consensus (phonics must be foundational) and on suppressing student/specialist voice in instructional design. Theater (0.48) reflects the gap between the official 'integration' narrative and actual practice: many classrooms deliver literature-centered instruction with supplementary phonics rather than the reverse. The measurement trajectory shows extractiveness and theater rising over 25 years as the constraint has become more institutionally entrenched while implementation fidelity has declined (collapse toward whole-language). Suppression requirement plateaus after year 15—the cognitive science evidence became overwhelming by then, requiring more active suppression to maintain the compromise frame.
 *
 * PERSPECTIVAL GAP:
 *   The institutional agenda-setter's seat perceives balanced literacy as genuine coordination—honoring both insights and providing coherent guidance. The struggling-reader and dyslexic-learner seats perceive it as a structure that permits collapse to whole-language, leaving them without foundational phonics instruction. The teacher seat carries both perceptions simultaneously: trained to implement balance but caught between two incompatible pedagogies and professional identities. The engine should compute tangled_rope from the institutional/beneficiary side (coordination + enforcement) and snare from the payer side (collapsed phonics + no exit). The divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional administrators and teacher-training programs are beneficiaries (d near 0.0): they gain institutional authority, coherence, and avoid theoretical contests. Teachers sit near symmetric in aggregate (d ≈ 0.5) but polarized individually: teachers with whole-language identity see the phonics mandate as constraint; teachers with phonics identity see the literature requirement as constraint. Struggling readers and dyslexic learners are clear targets (d near 1.0): they depend on phonics instruction but the constraint permits collapse, and they are trapped with no exit or voice. Excluded parties (phonics specialists, whole-language advocates) are analytically off the directionality axis—they remain organized but are kept from institutional design.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy with clear resolution: the founding problem (reconcile phonics and whole-language insights) was SOLVED by cognitive science research showing phonics must be foundational—the integration method is known. The institutional constraint persists despite the problem being solved because it serves the agenda-setter's authority interests (institutional coherence, avoiding theoretical contests). The persistence through solved-problem marks mandatrophy. The remedy would be to subordinate literature engagement to systematic phonics instruction and place cognitive science evidence at the center—but that remedy is actively suppressed because it would dissolve the institutional compromise position and require either pure-phonics or restructured integration. The constraint has become a vehicle for administrative authority over pedagogical design, not a mechanism for solving the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_fidelity_collapse_mechanism,
    'Does the institutional constraint''s equal-weighting of phonics and literature actually permit systematic collapse toward whole-language in practice, or do implementation safeguards prevent this collapse?',
    'Observational studies of classroom practice coding phonics instruction time, systematicity, and sequencing; comparison with schools that mandate phonics-first with literature supplement. Analysis of reading outcomes correlating with implementation fidelity.',
    'If collapse is systematic and preventable, the constraint is a snare masquerading as tangled_rope; if collapse is random or adequately prevented, the constraint is genuine tangled_rope. This determines whether the constraint targets struggling readers or coordinates their development.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(integration_fidelity_collapse_mechanism, empirical, 'Whether equal-weighting permits institutional collapse to whole-language practice.').

omega_variable(
    phonics_sufficiency_threshold,
    'What minimum level of phonics systematicity is required for struggling and dyslexic readers to acquire foundational decoding, and does the balanced literacy framework specify or enforce this threshold?',
    'Meta-analysis of intervention studies showing phonics intensity thresholds for different learner populations; audit of balanced-literacy curriculum materials and state standards to determine whether a phonics-intensity minimum is specified and measurable.',
    'If no minimum is specified, the constraint permits collapse by design—it is extractive. If a minimum is specified but not enforced, the constraint is snare-adjacent with weak enforcement. If both specified and enforced, the constraint is genuinely coordinative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(phonics_sufficiency_threshold, empirical, 'Whether the balanced literacy framework specifies minimum phonics systematicity.').

omega_variable(
    teacher_identity_lock_suppression_mechanism,
    'Is teacher resistance to phonics-first framing driven by genuine pedagogical disagreement (holdable alternative) or by professional identity fusion (suppressed alternative)?',
    'Post-training identity-friction analysis: teachers implementing phonics-first report whether the shift requires identity change or skill change. Comparison of adoption rates for teachers trained after identity commitment vs. before. Analysis of teacher-community discourse distinguishing pedagogical argument from identity-defense language.',
    'If identity-locked, the suppression is internalized and survives institutional mandate removal—victims carry suppression post-exit. If genuine disagreement, the suppression is structural and lift is more achievable. This affects whether exit from the constraint actually liberates struggling readers or merely shifts suppression to a different seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_identity_lock_suppression_mechanism, conceptual, 'Whether teacher resistance to phonics-first is ideological or identity-fused.').

omega_variable(
    founding_problem_mandatrophy_verification,
    'Has the founding problem of reconciling phonics and whole-language been actually solved by cognitive science research into the integration method, or does the apparent solution mask remaining uncertainty?',
    'Meta-analysis of reading science and longitudinal studies showing outcomes for phonics-first + literature supplementation vs. balanced + implementation variation. Consensus check: do cognitive science professional bodies (e.g., Society for the Scientific Study of Reading) now specify an integration method that resolves the original dilemma?',
    'If the problem is solved and the constraint persists despite, mandatrophy_resolved = true and the constraint is rent-collection for institutional authority. If uncertainty remains, the constraint reflects genuine unresolved scientific question and mandatrophy_resolved = false.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_mandatrophy_verification, empirical, 'Whether cognitive science has solved the phonics/whole-language integration problem.').

omega_variable(
    sibling_reading_foreclosure_mutual_exclusivity,
    'Are the phonics_reading and whole_language_reading mutually exclusive (one rules out the other), or can both coexist as different points in the same integration framework?',
    'Examine the core premises: phonics_reading asserts ''decoding skill must be explicitly taught before literature engagement''; whole_language_reading asserts ''decoding skills emerge implicitly from text engagement.'' These premises directly contradict at the causal-priority level. If either can be true within a single coherent learning theory, they coexist; if one must be false for the other to be true, they foreclose.',
    'If they foreclose, the balanced_literacy_reading''s attempt to honor both is incoherent—it is not a third position but an unstable compromise. If they coexist (as different learning pathways for different learner types), the balanced reading is coherent and the constraint is genuinely coordinative. This determines whether the constraint''s internal instability is a flaw or a feature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_mutual_exclusivity, conceptual, 'Whether phonics-first and whole-language readings mutually foreclose or can coexist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__balanced_literacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(read_tr_t0, observed).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement_basis(read_tr_t5, observed).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(read_tr_t10, observed).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement_basis(read_tr_t15, observed).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement_basis(read_tr_t20, observed).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_mechanism__balanced_literacy_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(read_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(read_be_t0, observed).
narrative_ontology:measurement(read_be_t5, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(read_be_t5, observed).
narrative_ontology:measurement(read_be_t10, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(read_be_t10, observed).
narrative_ontology:measurement(read_be_t15, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement_basis(read_be_t15, observed).
narrative_ontology:measurement(read_be_t20, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(read_be_t20, observed).
narrative_ontology:measurement(read_be_t25, reading_acquisition_mechanism__balanced_literacy_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(read_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(read_su_t0, observed).
narrative_ontology:measurement(read_su_t5, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement_basis(read_su_t5, observed).
narrative_ontology:measurement(read_su_t10, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(read_su_t10, observed).
narrative_ontology:measurement(read_su_t15, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(read_su_t15, observed).
narrative_ontology:measurement(read_su_t20, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(read_su_t20, observed).
narrative_ontology:measurement(read_su_t25, reading_acquisition_mechanism__balanced_literacy_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(read_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__balanced_literacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__balanced_literacy_reading, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, reading_acquisition_mechanism__whole_language_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, dyslexia_identification_gatekeeping).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__balanced_literacy_reading, teacher_professional_identity_formation).

% DUAL FORMULATION NOTE:
% This story is the balanced_literacy_reading of the contested kernel reading_acquisition_mechanism. Two sibling readings exist as separate constraint stories: phonics_reading (reading acquisition requires systematic grapheme-phoneme instruction as foundational) and whole_language_reading (reading emerges from meaningful text engagement). The three readings represent different institutional and pedagogical positions on the same contested kernel. Each reading has its own ε, its own beneficiary/victim structure, and its own type. They are linked via network.affects_constraints because the institutional adoption of one reading (balanced) structurally shapes the resources and legitimacy available to the others (phonics and whole-language remain organized at the practitioner level but are excluded from institutional standard-setting). The ε values differ substantially: phonics_reading has low extraction (addressing a genuine cognitive-science fact); whole_language_reading has moderate extraction (addressing engagement and meaning-making but possibly at cost to decoding); balanced_literacy_reading has moderate-to-high extraction (administrative authority, mandate control, suppressed expertise).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_mechanism__balanced_literacy_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
