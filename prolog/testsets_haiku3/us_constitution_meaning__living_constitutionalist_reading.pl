% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__living_constitutionalist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: us_constitution_meaning__living_constitutionalist_reading
 *   human_readable: Living Constitutionalism: Adaptive Constitutional Interpretation
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   The living constitutionalist reading claims that constitutional
 *   principles endure but their application evolves with social attitudes and
 *   circumstances. Judges interpreting the Constitution are constrained by
 *   textual principles and historical trajectory but empowered to adapt
 *   application to contemporary contexts—new technologies, evolved moral
 *   understanding, demographic realities the framers did not anticipate. This
 *   reading vindicates rights claims in evolving social contexts (privacy,
 *   equal dignity, reproductive autonomy) while imposing costs on
 *   majoritarian democracy and settled legal expectations. The constraint is
 *   NOT the Constitution itself but ONE READING of what the Constitution
 *   means and demands. Sibling readings—originalism and legal
 *   positivism—dispute whether the Constitution's meaning is fixed at
 *   ratification, fixed by formal enactment, or evolves with judicial
 *   interpretation.
 *
 * KEY AGENTS:
 *   - Rights claimants in evolving social contexts (beneficiary): persons asserting rights derived from enduring constitutional principles applied to new circumstances
 *   - Judicial interpreters (agenda-setter): courts that adopt living constitutionalism and gain authority to declare what the Constitution demands in changed times
 *   - Majoritarian democracy (payer): elected branches and democratic majorities whose policy choices are constrained by judicial findings of evolved constitutional principles
 *   - Settled legal expectations (payer): persons and institutions who relied on prior constitutional understandings and face disruption from reinterpretation
 *   - Originalist critics (excluded): jurists and philosophers who reject living constitutionalism as judicial legislation rather than interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__living_constitutionalist_reading, 0.43).
domain_priors:suppression_score(us_constitution_meaning__living_constitutionalist_reading, 0.28).
domain_priors:theater_ratio(us_constitution_meaning__living_constitutionalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, extractiveness, 0.43).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__living_constitutionalist_reading, rope).
narrative_ontology:human_readable(us_constitution_meaning__living_constitutionalist_reading, "Living Constitutionalism: Adaptive Constitutional Interpretation").
narrative_ontology:topic_domain(us_constitution_meaning__living_constitutionalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__living_constitutionalist_reading, '7a8c5bd3-0522-4744-93a7-f258b2936df5').
narrative_ontology:cs_kernel_codification('7a8c5bd3-0522-4744-93a7-f258b2936df5', fixed_text).
narrative_ontology:cs_authority_grounding('7a8c5bd3-0522-4744-93a7-f258b2936df5', lineage).
narrative_ontology:cs_interpretation_layer_present('7a8c5bd3-0522-4744-93a7-f258b2936df5').
narrative_ontology:cs_reading_relation('7a8c5bd3-0522-4744-93a7-f258b2936df5', us_constitution_meaning__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a8c5bd3-0522-4744-93a7-f258b2936df5', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('7a8c5bd3-0522-4744-93a7-f258b2936df5', foundational, enduring_principles_transcend_historical_moment).
narrative_ontology:cs_axiom_status(enduring_principles_transcend_historical_moment, holdable).
narrative_ontology:cs_axiom_grounding('7a8c5bd3-0522-4744-93a7-f258b2936df5', enduring_principles_transcend_historical_moment, deontological).
narrative_ontology:cs_axiom('7a8c5bd3-0522-4744-93a7-f258b2936df5', foundational, contemporary_moral_consensus_legitimate_interpretive_input).
narrative_ontology:cs_axiom_status(contemporary_moral_consensus_legitimate_interpretive_input, holdable).
narrative_ontology:cs_axiom_grounding('7a8c5bd3-0522-4744-93a7-f258b2936df5', contemporary_moral_consensus_legitimate_interpretive_input, empirically_contingent).
narrative_ontology:cs_reference_frame('7a8c5bd3-0522-4744-93a7-f258b2936df5', textual_principles_binding_application_contextual).
narrative_ontology:cs_drift_state('7a8c5bd3-0522-4744-93a7-f258b2936df5', contemporary_expansion_of_rights, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7a8c5bd3-0522-4744-93a7-f258b2936df5', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, rights_claimants_evolving_contexts).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, judicial_interpreters).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, majoritarian_accountability).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, settled_expectations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, majoritarian_democracy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons asserting rights not textually explicit in the Constitution but derived from enduring principles applied to contemporary circumstances. Living constitutionalism permits judges to recognize rights (privacy, equal dignity regardless of sexual orientation, reproductive autonomy) by reinterpreting principles in light of evolved social understanding. Their exit from the constraint would mean losing judicial recognition of these derived rights.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, rights_claimants_evolving_contexts, beneficiary,
    organized, generational, constrained, national).

% Courts that adopt living constitutionalism gain interpretive authority to adapt constitutional meaning to changing circumstances, consulted on questions of fundamental rights and social policy. They set the interpretive frame by declaring which principles endure and how contemporary moral consensus shapes their application. Their authority depends on public acceptance that the Constitution's meaning evolves legitimately.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, judicial_interpreters, agenda_setter,
    institutional, generational, constrained, national).

% Democratic majorities and elected branches face judicial constraint on policy they enact, justified by the judge's power to find unenumerated rights in the evolving Constitution. They cannot simply change a rights regime through ordinary legislation if judges find a constitutional principle against it. This represents a structural cost: majoritarian processes are superseded by judicial moral reasoning about what contemporary values demand.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, majoritarian_democracy, payer,
    powerful, generational, constrained, national).

% Legal regimes and reliance interests built on prior constitutional understandings become vulnerable to reinterpretation. Persons who relied on a settled reading of constitutional law (e.g., that certain regulations were permissible) find their positions overturned when the judicial interpretation evolves. The cost is legal unpredictability and the erosion of settled law.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, settled_expectations, payer,
    moderate, biographical, constrained, national).

% Judicial theorists and practitioners who reject living constitutionalism in favor of original public meaning are excluded from the interpretive framework this constraint instantiates. They would argue the constraint is lawlessness disguised as evolution, that it permits judges to project contemporary politics onto the Constitution. Their voices are present in dissent but do not set the framework.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, originalist_critics, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__living_constitutionalist_reading, judicial_interpreters).
narrative_ontology:fixing_cost_class(us_constitution_meaning__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable interpretive framework for the Constitution that permits judges to apply enduring principles to novel circumstances without requiring constitutional amendment for every social change. Solves the coordination problem: how do we keep a written text valid across centuries of changing technology, demography, and moral understanding without constant formal amendment?
% TRANSFER_FUNCTION: Transfers interpretive authority from the text's fixed historical moment to judges' contemporary reasoning about evolved moral and social context. Rights claimants gain judicial vindication of unenumerated rights; judges gain authority to shape constitutional meaning; majoritarian institutions lose direct control over what counts as a constitutional constraint on their power.
% ABSENT_VOICES: Originalists and textualists who would reject the premise that constitutional meaning evolves; citizens who rely on settled legal expectations and fear unpredictability from reinterpretation; originalist or positivist judges whose competing reading is subordinated within this constraint's frame. They would argue for a fixed Constitution, not an evolving one, or for constitutional validity grounded in enactment rather than moral consensus.
% DISAPPEARANCE_RATIONALE: If living constitutionalism disappeared and were replaced by originalism or textual positivism, the landscape of recognized rights would contract: privacy rights, equal protection for sexual orientation and gender identity, reproductive autonomy derived from penumbral reasoning would all become vulnerable or disappear. Conversely, majoritarian institutions would regain authority to regulate conduct presently protected by evolved constitutional doctrine. The legal and political world would reorganize around a frozen-in-time Constitution or a positivist reading focused on formal enactment rather than principle.
% FOUNDING_PROBLEM: The Constitution was drafted for an agrarian, slower-changing society; its text could not anticipate modern contexts (internet surveillance, reproductive technology, social media, global commerce). A rigid, unchanging Constitution would become increasingly irrelevant or would accumulate incoherence as society changes faster than the amendment process can accommodate. How can a 1787 document govern a 21st-century society fairly?
% FOUNDING_PROBLEM_CORROBORATION: Living constitutionalists attest the problem is live and unsolvable by amendment alone—the amendment process is too slow and too difficult, and society changes faster than formal procedures can accommodate. Originalists and positivists dispute the problem's premise: originalists argue the Constitution's principles are timeless enough to apply to novel facts without reinterpretation; positivists argue new problems require legislation or formal amendment, not judicial evolution. Legal historians and empirical studies of amendment difficulty (showing only 27 amendments in 235 years, most minor) support the living constitutionalist diagnosis; conservative constitutionalists reject the diagnosis as overstated and argue that the Framers' principles are sufficiently general to address novel situations.
narrative_ontology:disappearance_verdict(us_constitution_meaning__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__living_constitutionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__living_constitutionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_meaning__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__living_constitutionalist_reading, 0.43, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__living_constitutionalist_reading_tests).
:- end_tests(us_constitution_meaning__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.43) because the constraint transfers meaningful authority—constitutional meaning-making power—from formal amendment (the majoritarian check) to judicial interpretation. This is extraction from majoritarian accountability, but it is justified (within this reading) as the price of adaptive constitutional governance. The justification is coordination: living constitutionalism solves the problem of keeping a fixed text valid across centuries without requiring amendment for every social change. Suppression is relatively low (0.28) because the constraint does not require majorities to abandon rights expansion; it permits judges to recognize rights through evolved interpretation, which rights claimants want. Theater is low (0.22): the constraint's functional core is adaptation to novel circumstances, and doctrine does evolve responsively. The trajectory shows extractiveness plateauing after ~25 time points: the constraint reaches a stable equilibrium where judges have recognized major unenumerated rights (privacy, equal protection for sexual orientation), and the rate of new expansion slows. Suppression and theater also plateau, suggesting the constraint stabilizes in institutional practice once the major battles are decided.
 *
 * PERSPECTIVAL GAP:
 *   From the judicial interpreter's seat, living constitutionalism is principled stewardship: keeping the Constitution alive and relevant by adapting timeless principles to new contexts. From the majoritarian accountability seat, it is power usurpation: judges imposing their view of contemporary values under the guise of interpreting an old document. From the rights-claimant seat, it is liberation: enabling recognition of rights the framers did not foresee but that enduring principles (dignity, liberty, equal protection) support. From the settled-expectations seat, it is disruption: legal doctrines built on prior understandings become vulnerable to reinterpretation. The engine computes these divergent types from the structural data—the same constraint, different directionalities for different seats. The authored claim (rope: genuine coordination + active enforcement) and the metrics (moderate extractiveness, relatively low suppression) describe how the constraint operates across seats, not whether all seats experience it the same way.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights claimants occupy the beneficiary end of d: they benefit from the constraint without bearing its costs (the costs fall on majoritarian institutions). Judicial interpreters are the agenda-setter, claiming to steward the constraint in light of principles—high institutional power, stable exit options (tenure), and authority to shape the frame. Majoritarian institutions and settled expectations occupy the payer end: they bear the constraint's cost (loss of direct control over constitutional meaning, legal unpredictability). The constraint is enforced through constitutional review: judges can strike down legislation they find unconstitutional under the evolved reading, and this enforcement is active and requires sustained institutional capacity (courts, legal theory, academic legitimation). The directionality is high for majority institutions (they cannot simply overturn judicial constitutional findings through legislation) and low for rights claimants (they collect from the constraint's operation). No override is needed; the structural derivation is sound.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to keep a fixed text valid across time) remains contested in status. Living constitutionalism is one answer; originalism and positivism are others. If we asked whether the founding problem is dead, the reading would have to argue that the problem persists—that amendment is indeed too slow and difficult, so living interpretation remains necessary. If the founding problem were dead (e.g., if amendment procedures became efficient or if society agreed that the Constitution should be frequently replaced), living constitutionalism would become a vestigial mechanism defending its own authority against replacement. The measurement trajectory shows extractiveness plateauing, which suggests the constraint is not degrading into theater but reaching a stable equilibrium. Mandatrophy is not resolved, but the constraint shows resilience rather than decay. If judicial authority were eroding (extractiveness and suppression declining toward zero while theater rose) that would signal mandatrophy onset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_grounding_vs_contemporary_consensus,
    'How much do constitutional principles remain anchored in textual meaning versus how much are they rewritten by contemporary moral consensus? Is living constitutionalism constrained interpretation of enduring principles, or is it a cover for judicial legislation of contemporary values?',
    'Comparative analysis of judicial opinions: do judges ground evolved interpretations in textual principle and historical trajectory, or do they invent principles de novo from contemporary values? Empirical study of whether lived doctrine remains recognizable across generations or undergoes periodic reinvention.',
    'If principles remain textually grounded and evolve by principled extension, living constitutionalism is a genuine coordination mechanism for textual stability across time. If principles are repeatedly reinvented by judges'' contemporary values, the constraint is more extractive and extractive of majoritarian accountability than coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_grounding_vs_contemporary_consensus, empirical, 'Degree to which evolved interpretation remains tethered to the Constitution''s text versus becomes projection of contemporary political values.').

omega_variable(
    moral_progress_assumption,
    'Is contemporary moral consensus in fact a more enlightened understanding of constitutional principles, or is it one contingent perspective among many valid readings? Does the constraint assume moral progress is real and unidirectional?',
    'Historical analysis: do periods treated as moral progress by one era become viewed as moral error by later eras? Philosophical examination of whether the constraint can accommodate moral reversals (e.g., a future generation rejecting today''s consensus on a rights issue) or whether it commits to an assumption of directional moral evolution.',
    'If moral progress is assumed unidirectional, the constraint builds in a progressive bias and is extractive of alternative moral frameworks. If the constraint can accommodate moral reversals, it is more robustly a coordination mechanism that happens to track contemporary understanding without claiming universal moral direction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_progress_assumption, conceptual, 'Whether living constitutionalism assumes directional moral progress or can accommodate moral reversals and contingency.').

omega_variable(
    judicial_expertise_vs_democratic_deficit,
    'Does transferring constitutional meaning-making authority to judges reflect legitimate expertise in interpreting enduring principles, or does it impose a counter-majoritarian check that lacks democratic justification?',
    'Empirical: study predictability and consistency of judicial evolution across judges and circuits. Normative: philosophical argument for whether constitutional interpretation is the sort of task where judicial expertise is defensible or whether all such authority is inherently anti-democratic. Cross-jurisdictional study of alternative institutional arrangements (citizen assemblies, super-majoritarian legislatures, amendment procedures).',
    'If judicial interpretation evolves predictably and remains grounded in principles, the counter-majoritarian cost might be justified as the price of stable constitutional coordination. If judicial evolution is erratic and appears driven by judges'' policy preferences, the institutional legitimacy of the constraint is compromised and the extractive cost (loss of majoritarian accountability) exceeds coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_expertise_vs_democratic_deficit, empirical, 'Whether the judicial expertise justifies the counter-majoritarian extraction, or whether the constraint amounts to unjustified usurpation of democratic authority.').

omega_variable(
    living_vs_originalist_kernel_reading,
    'This constraint instantiates living constitutionalism as ONE reading of the contested kernel US_CONSTITUTION_MEANING. What makes this reading a legitimate instantiation of the enduring Constitution versus a competing interpretation that treats the Constitution as a fixed historical artifact? Can both readings coexist within a single constitutional framework, or do they foreclose each other?',
    'Structural: if living and originalist readings are adopted by different judges in a unified court system, what happens at their intersection? Empirical: do jurisdictions that adopt living constitutionalism versus originalism converge on similar constitutional doctrines, or do they diverge? Normative: is there a constitutional metatheory that can accommodate both readings as legitimate, or does one necessarily displace the other?',
    'If the readings coexist peacefully, both constraints remain live and the constitutional system exhibits pluralism. If they foreclose each other, the constraint selection is zero-sum and the defeated reading becomes a residual claim on legitimacy (feeding originalist dissent). If they influence each other (each reading shapes which moves are available to the other), the constraint structure is more complex than either reading claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(living_vs_originalist_kernel_reading, conceptual, 'The kernel-level indeterminacy of whether living constitutionalism is a valid reading of an enduring Constitution or a displacement of constitutional meaning by contemporary politics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__living_constitutionalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(us_c_tr_t0, projected).
narrative_ontology:measurement(us_c_tr_t5, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement_basis(us_c_tr_t5, projected).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(us_c_tr_t10, observed).
narrative_ontology:measurement(us_c_tr_t15, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(us_c_tr_t15, observed).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(us_c_tr_t20, observed).
narrative_ontology:measurement(us_c_tr_t25, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 25, 0.21).
narrative_ontology:measurement_basis(us_c_tr_t25, observed).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(us_c_tr_t30, observed).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(us_c_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(us_c_be_t0, projected).
narrative_ontology:measurement(us_c_be_t5, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement_basis(us_c_be_t5, projected).
narrative_ontology:measurement(us_c_be_t10, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement_basis(us_c_be_t10, observed).
narrative_ontology:measurement(us_c_be_t15, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 15, 0.39).
narrative_ontology:measurement_basis(us_c_be_t15, observed).
narrative_ontology:measurement(us_c_be_t20, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement_basis(us_c_be_t20, observed).
narrative_ontology:measurement(us_c_be_t25, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement_basis(us_c_be_t25, observed).
narrative_ontology:measurement(us_c_be_t30, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement_basis(us_c_be_t30, observed).
narrative_ontology:measurement(us_c_be_t40, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 40, 0.43).
narrative_ontology:measurement_basis(us_c_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(us_c_su_t0, projected).
narrative_ontology:measurement(us_c_su_t5, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 5, 0.18).
narrative_ontology:measurement_basis(us_c_su_t5, projected).
narrative_ontology:measurement(us_c_su_t10, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement_basis(us_c_su_t10, observed).
narrative_ontology:measurement(us_c_su_t15, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 15, 0.25).
narrative_ontology:measurement_basis(us_c_su_t15, observed).
narrative_ontology:measurement(us_c_su_t20, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 20, 0.27).
narrative_ontology:measurement_basis(us_c_su_t20, observed).
narrative_ontology:measurement(us_c_su_t25, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 25, 0.28).
narrative_ontology:measurement_basis(us_c_su_t25, observed).
narrative_ontology:measurement(us_c_su_t30, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement_basis(us_c_su_t30, observed).
narrative_ontology:measurement(us_c_su_t40, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 40, 0.28).
narrative_ontology:measurement_basis(us_c_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_meaning__living_constitutionalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning__positivist_reading).

% DUAL FORMULATION NOTE:
% US_CONSTITUTION_MEANING is a contested kernel with three structurally distinct readings. LIVING_CONSTITUTIONALIST_READING (this story) treats constitutional meaning as adaptive to social change. ORIGINALIST_READING treats meaning as fixed at ratification. POSITIVIST_READING treats validity as derived from enactment authority, not moral content. All three are constraint stories of the same kernel; each has its own ε, beneficiary/victim structure, and type. The readings influence each other: originalism constrains how aggressively living interpretation can evolve; living constitutionalism creates pressure on originalists to show practical applicability; positivism offers a neutral-ground alternative that brackets moral debates. Each story links to the others via affects_constraints and documents its axioms and reading relations in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
