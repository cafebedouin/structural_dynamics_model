% ============================================================================
% CONSTRAINT STORY: state_execution_authority__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__retributive_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: state_execution_authority__retributive_reading
 *   human_readable: State Execution Authority (Retributive Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the retributive reading of state execution
 *   authority: the claim that execution restores moral balance by imposing
 *   proportionate punishment for heinous crimes. The reading constitutes one
 *   pole of a persistent kernel contest around capital punishment. The
 *   constraint is CLAIMED as tangled_rope (it coordinates public responses to
 *   heinous crime AND asymmetrically extracts from executed offenders and
 *   marginalized defendants). The authored metrics describe substantial
 *   extraction (0.82 base extractiveness) driven by the irreplaceability of
 *   death as the punishment mechanism under retributive logic—imprisonment
 *   cannot substitute because the doctrine requires proportionality between
 *   crime severity and penalty. Suppression is high (0.71) because the
 *   constraint persists through active exclusion of abolition voices from
 *   capital sentencing and through procedural foreclosure of defendants' own
 *   agency in the process. Theater ratio is moderate (0.28): the retributive
 *   justification is genuinely operative in judicial reasoning, but an
 *   increasing share of enforcement machinery (appellate review, jury
 *   management, jury questioning about retributive beliefs) defends the
 *   constraint's legitimacy against empirical and ethical challenge rather
 *   than implementing core retributive functions.
 *
 * KEY AGENTS:
 *   - Victims' families: beneficiaries of symbolic moral restoration; powerless agents amplified by prosecutors and victim advocates; their need for closure is central to retributive legitimacy.
 *   - State execution apparatus: agenda-setter; institutional power; controls capital charging, trial procedures, appellate review, and execution; grounds authority in constitutional authorization and retributive desert principle.
 *   - Executed offenders: powerless payers; the literal cost of the constraint; their death is framed as deserved rather than coerced, but they have no voice in the process.
 *   - Marginalized defendants: powerless payers; face elevated capital prosecution risk due to poverty, race, and mental illness; bear extraction through asymmetric execution likelihood relative to privileged defendants.
 *   - Retributive justice doctrine: non-agent beneficiary; the intellectual tradition that grounds and legitimates the constraint; its operation vindicates the doctrine; vulnerable to competing readings (deterrence, abolition).
 *   - Abolition advocates: excluded; would argue categorical impermissibility regardless of crime severity; their exclusion is structural, not accidental.
 *   - Constitutional courts: observer seat; review for constitutional compliance; can potentially reclassify the constraint but remain analytically removed from beneficiary/payer positions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__retributive_reading, 0.82).
domain_priors:suppression_score(state_execution_authority__retributive_reading, 0.71).
domain_priors:theater_ratio(state_execution_authority__retributive_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__retributive_reading, "State Execution Authority (Retributive Reading)").
narrative_ontology:topic_domain(state_execution_authority__retributive_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__retributive_reading, 'cf8d10ca-6e9f-4da7-937b-498030df4c88').
narrative_ontology:cs_kernel_codification('cf8d10ca-6e9f-4da7-937b-498030df4c88', fixed_text).
narrative_ontology:cs_authority_grounding('cf8d10ca-6e9f-4da7-937b-498030df4c88', lineage).
narrative_ontology:cs_interpretation_layer_present('cf8d10ca-6e9f-4da7-937b-498030df4c88').
narrative_ontology:cs_reading_relation('cf8d10ca-6e9f-4da7-937b-498030df4c88', state_execution_authority__abolition_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf8d10ca-6e9f-4da7-937b-498030df4c88', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('cf8d10ca-6e9f-4da7-937b-498030df4c88', foundational, proportional_desert_restores_moral_balance).
narrative_ontology:cs_axiom_status(proportional_desert_restores_moral_balance, holdable).
narrative_ontology:cs_axiom_grounding('cf8d10ca-6e9f-4da7-937b-498030df4c88', proportional_desert_restores_moral_balance, deontological).
narrative_ontology:cs_axiom('cf8d10ca-6e9f-4da7-937b-498030df4c88', foundational, execution_is_proportionate_penalty_for_heinous_crime).
narrative_ontology:cs_axiom_status(execution_is_proportionate_penalty_for_heinous_crime, holdable).
narrative_ontology:cs_axiom_grounding('cf8d10ca-6e9f-4da7-937b-498030df4c88', execution_is_proportionate_penalty_for_heinous_crime, deontological).
narrative_ontology:cs_reference_frame('cf8d10ca-6e9f-4da7-937b-498030df4c88', constitutional_capital_punishment_authority).
narrative_ontology:cs_drift_state('cf8d10ca-6e9f-4da7-937b-498030df4c88', contemporary_rights_expansion_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cf8d10ca-6e9f-4da7-937b-498030df4c88', '').
narrative_ontology:cs_kernel_id(state_execution_authority__retributive_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, victims_families).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, retributive_justice_doctrine).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, marginalized_defendants).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, proportional_desert_principle).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, moral_balance_restoration_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Families of murder victims participate in victim-impact testimony and witness executions as a form of closure and moral vindication. The retributive reading frames their suffering as rectified through proportionate state punishment. They lack institutional power but carry emotional standing in the sentencing and execution process; their voices are amplified by victim advocacy groups and prosecutors.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, victims_families, beneficiary,
    powerless, biographical, constrained, national).

% The prosecutorial, judicial, and correctional institutions that administer capital punishment. They set the criteria for death-eligible crimes, conduct capital trials, appellate review, and carry out executions. They frame the process as restoring moral balance and protecting societal order. Their authority derives from constitutional text and legislative authorization; the retributive reading grounds their legitimacy in proportionality and desert.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, state_execution_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Individuals sentenced to death and executed. In the retributive reading, they are the legitimate cost of moral restoration—their death is framed as deserved punishment proportionate to their heinous crimes. They have no voice in the process after conviction; appellate rights offer limited exit (reviewed claims, clemency petitions) but execution remains the only terminal state once all legal remedies are exhausted or denied.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, executed_offenders, payer,
    powerless, immediate, trapped, national).

% Poor, racial-minority, and mentally ill defendants who face capital prosecution at rates far exceeding their proportional representation in homicide arrests. They bear extraction through elevated risk of capital conviction relative to privileged defendants in equivalent factual scenarios. Their constrained exit includes limited legal resources, geographic disadvantage in obtaining expert representation, and historical discrimination in jury selection and sentencing.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, marginalized_defendants, payer,
    powerless, biographical, constrained, national).

% The philosophical and jurisprudential tradition that grounds state punishment in proportional desert and moral restoration rather than deterrence or rehabilitation. The retributive reading instantiates and vindicates this doctrine; its operation depends on the doctrine's intellectual legitimacy within judicial reasoning and legislative framing.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, retributive_justice_doctrine, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(state_execution_authority__retributive_reading, retributive_justice_doctrine).

% Advocacy organizations and scholars who argue execution is categorically impermissible. They are structurally excluded from the retributive framework—they would argue that the moral balance restoration doctrine is a cover story for state violence and that executed offenders retain inalienable dignity that death forfeits. They challenge the constraint through litigation and legislative reform but remain outside the operational consensus of capital sentencing.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, abolition_advocates, excluded,
    organized, generational, constrained, national).

% Scholars and prosecutors who ground execution in preventing future murders by raising the cost of capital crime. They are structurally adjacent to the retributive reading but offer a different justification; their exclusion is partial—they coexist within capital sentencing discourse but the retributive reading does not depend on deterrent effects being real, only on desert being proportionate.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, deterrence_theorists, excluded,
    analytical, civilizational, analytical, universal).

% Courts that review capital convictions for constitutional compliance with cruel-and-unusual-punishment prohibitions. They adjudicate whether particular execution methods, procedural inadequacies, or substantive guilt findings violate constitutional text. Their analytical seat enables them to potentially reclassify the constraint (e.g., by finding execution unconstitutional) but does not position them as direct beneficiaries or payers within the retributive framework.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__retributive_reading, state_execution_apparatus).
narrative_ontology:fixing_cost_class(state_execution_authority__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective problem of responding to heinous crime in a way that is publicly visible, ritually finalized, and grounded in a principle (proportionate desert) that offers symbolic closure to affected communities and maintains public confidence in the justice system's seriousness about severe crimes.
% TRANSFER_FUNCTION: Transfers the executed offender's life to the state as payment for proportionate moral restoration, with symbolic benefits accruing to victims' families (closure, vindication) and the retributive doctrine (authority and legitimacy). Victims' families and state authority are the named recipients; executed offenders bear the cost; marginalized defendants bear elevated execution risk as an asymmetric extraction.
% ABSENT_VOICES: Abolition advocates, living crime victims who oppose execution, executed offenders themselves (their voices are formally structured out of the process after conviction), and defendants from privileged socioeconomic backgrounds in jurisdictions where capital prosecution is discretionary. Their absence reflects both procedural exclusion (death row inmates cannot participate in their own sentencing or appeal) and structural exclusion (abolitionists are not seated in capital sentencing decisions).
% DISAPPEARANCE_RATIONALE: If the constraint vanished—if execution authority were abolished while retaining capital-crime categories and life imprisonment as maximum punishment—victims' families would lose the symbolic closure the retributive framework promises; the state would lose a declarative tool for expressing that certain crimes are unforgivable; and the retributive doctrine would lose its primary institutional instantiation. Societies would reorganize punishment around alternative justifications (rehabilitation, incapacitation, deterrence via life imprisonment) and victims' advocacy would reshape around different symbolic practices.
% FOUNDING_PROBLEM: Heinous crimes (premeditated murder, especially of children, police, or multiple victims) create a moral injury to the community that ordinary punishment (imprisonment) is perceived as inadequate to address. The founding problem is the gap between the felt severity of the worst crimes and the ordinary carceral penalty. Retribution asserts that only a proportionate response—one that matches the crime's severity—can restore moral balance and public confidence that justice has been done.
% FOUNDING_PROBLEM_CORROBORATION: Retributive theorists and victim advocacy groups attest the founding problem is live: grieving families report that life imprisonment does not provide moral closure, and prosecutors invoke desert-based justifications in charging decisions. Abolitionists and empirical criminologists outside the retributive consensus attest the problem is illusory or misframed: they argue that moral balance cannot be restored through execution, that victims' families' expressed need for closure is psychologically exploited by the state, and that the 'adequacy gap' is a constructed narrative rather than an authentic unmet need. Comparative evidence from abolition-practicing democracies (most of Europe, Canada, Australia) suggests the founding problem can be addressed through alternative symbolic and procedural means without execution.
narrative_ontology:disappearance_verdict(state_execution_authority__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__retributive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_execution_authority__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__retributive_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__retributive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.82 by interval end) because the retributive reading treats the executed offender's life as a necessary, non-substitutable cost of moral restoration—imprisonment cannot fulfill the doctrine's requirement for proportionate desert matching the crime's heinousness. This is the reading's core structural claim: there is no lower penalty that satisfies retributive justice for the worst crimes. The extractiveness measures the degree to which this reading asserts an irreducible, non-negotiable extraction requirement. Suppression (0.71) reflects the constraint's dependence on active procedures that foreclose alternative voices: executed offenders are excluded from the process after conviction; abolition advocates are structurally kept out of capital sentencing; defendants cannot refuse the retributive framework. Theater ratio (0.28, moderate) indicates that retributive moral reasoning is genuinely operative in judicial opinion-writing and legislative justification, but an increasing fraction of procedural and appellate activity (jury management, proportionality review, comparative sentence-culpability analysis) operates to defend retributive legitimacy against empirical scrutiny rather than to implement the core moral-balance function. The measurement series track rising extractiveness (as capital prosecution becomes more discretionary and concentrated on marginalized defendants) and rising suppression requirements (as abolition advocacy grows and procedural challenges mount). Theater ratio rises modestly because procedural complexity around retributive justification increases even as the underlying doctrine remains constant.
 *
 * PERSPECTIVAL GAP:
 *   The seated agents should compute very differently per seat. State apparatus, victims' families, and retributive theorists will compute the constraint as genuine coordination (solving the moral-adequacy problem) with justified asymmetry (the executed offender deserves the cost). Marginalized defendants, abolition advocates, and defendants' counsel will compute the constraint as pure extraction disguised as desert—the retributive framing masks discretionary prosecution, racial disparities, and the state's symbolic interest in maintaining execution authority. Constitutional courts occupy an observer seat that can adjudicate between readings but are not themselves beneficiaries or payers. The engine's per-seat computation will reflect this structural divergence: the beneficiary seats (victims' families, state apparatus) will see relatively low extractiveness and high legitimacy; the payer seats (executed offenders, marginalized defendants) will see high extractiveness and low legitimacy; the observer seat will see the structural asymmetry without resolving it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality differs sharply by power and exit options. Victims' families are powerless but beneficiary-positioned (d near beneficiary end, ~0.15): they receive symbolic restoration without bearing operational cost. State apparatus is institutional and beneficiary-positioned (d near beneficiary, ~0.20): it controls the rules and collects legitimacy. Executed offenders are powerless and victim-positioned (d near target, ~0.95): they are trapped (no exit except exhausted appeals) and the constraint extracts their life. Marginalized defendants are powerless and asymmetrically targeted (d near target, ~0.85): they face elevated prosecution risk relative to privileged defendants in equivalent cases, making the constraint's extraction fall disproportionately on them. No override is needed; the derivation from beneficiary/victim + power + exit options produces accurate directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (moral inadequacy of imprisonment for heinous crimes) is CONTESTED in status. Retributive theorists, victims' families, and prosecutors attest it is live and requires execution as the solution. Abolitionists, empirical criminologists, and comparative evidence from abolition democracies attest it is either dead (life imprisonment with ritual closure procedures serves all legitimate needs) or illusory (the 'adequacy gap' is a constructed narrative that exploits grieving families' psychology). The constraint is CLASSIFIED as tangled_rope because it solves a genuine coordination problem (public response to heinous crime with a principle grounding legitimacy) AND enforces asymmetric extraction (executed offenders and marginalized defendants bear costs). This classification prevents misreading it as pure snare (the coordination function is real) or as pure rope (the asymmetry and extraction are real). The mandatrophy threshold is crossed if the founding problem resolves to 'dead' + disappearance verdict = 'world_rearranges': that would signal the constraint persists despite its original justification ceasing to apply. Current classification holds the ambiguity open via omegas; the engine's per-seat computation will flag seats that experience high extraction without coordination benefit (marginalized defendants).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_measurement_ambiguity,
    'How is proportionality between crime severity and punishment measured in the retributive framework, and is that measurement stable across cases, judges, and jurisdictions?',
    'Empirical analysis of sentencing outcomes: if death sentences are imposed proportionately (equivalent heinousness yields equivalent sentences across jurisdictions and judges), proportionality is measured consistently; if sentencing varies widely for factually equivalent crimes, proportionality is measured unstably or inconsistently, suggesting the framework''s moral content is less determinate than claimed.',
    'If proportionality is measured unstably, the retributive doctrine''s claim to objective moral grounding is undermined, and the constraint shifts toward snare (procedurally arbitrary extraction defended by a cover story). If proportionality is measured consistently, the doctrine''s structural claim is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_measurement_ambiguity, empirical, 'Whether retributive proportionality is a measurable, stable principle or a post-hoc rationalization of discretionary prosecution.').

omega_variable(
    closure_effectiveness_and_exploitation,
    'Does participation in execution actually provide the psychological closure to victims'' families that the retributive framework promises, or does it exploit grieving families'' trauma to legitimize state violence?',
    'Longitudinal psychological studies of victims'' families pre- and post-execution witnessing; comparison of closure trajectories between families in execution and abolition jurisdictions; qualitative testimony from families about whether promised closure materialized.',
    'If closure is genuine and durable, victims'' families are legitimate beneficiaries and the coordination function is real. If closure is illusory or temporary, or if the invitation to witness exploits vulnerability, the constraint''s framing as coordination breaks down and the extraction from executed offenders is less justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(closure_effectiveness_and_exploitation, empirical, 'Whether victims'' families actually benefit from execution or are instrumentalized by it.').

omega_variable(
    moral_desert_universality_vs_doctrine_contingency,
    'Is proportionate desert a universal principle of justice independent of the retributive doctrine, or is retributive desert a contingent doctrinal commitment that competing readings (abolition, deterrence) reject on valid grounds?',
    'Philosophical analysis of desert foundations: if proportionality is grounded in intrinsic moral principles that all ethical frameworks must honor, desert is universal; if proportionality is a doctrine-specific claim that can be coherently rejected (by deontological rights-based frameworks, utilitarian frameworks, or other normative systems), desert is contingent on retributive commitments.',
    'If desert is universal, the retributive reading''s core axiom is foundational to justice itself, and the constraint is justified beyond retributive doctrine alone. If desert is contingent, the retributive reading''s legitimacy depends on accepting the retributive doctrine, and competing readings are not ruled out by the constraint''s structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_desert_universality_vs_doctrine_contingency, conceptual, 'Whether moral desert is a universal principle or a retributive doctrine''s specific claim.').

omega_variable(
    racial_and_socioeconomic_disparity_mechanism,
    'Are racial and socioeconomic disparities in capital sentencing a consequence of the retributive principle applied in a society with pre-existing discrimination, or a structural feature of how retributive authority concentrates on marginalized defendants?',
    'Comparative analysis of capital prosecution patterns in racially segregated vs. integrated judicial systems, and of discretionary vs. mandatory capital systems. If disparities persist even when retributive criteria are applied consistently, the mechanism is structural; if disparities correlate with discrimination in the broader society, the mechanism is upstream discrimination.',
    'If disparities are structural to retributive application, the constraint extracts asymmetrically from marginalized defendants by design, making the tangles-rope classification stronger. If disparities are consequences of upstream discrimination, the constraint could theoretically be applied without asymmetry, weakening the extraction claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(racial_and_socioeconomic_disparity_mechanism, empirical, 'Whether capital punishment''s racial and socioeconomic disparities are structural or upstream.').

omega_variable(
    kernel_reading_relationship_to_sibling_readings,
    'Does the retributive reading''s core premise (execution restores moral balance) logically foreclose the abolition reading''s core premise (execution is categorically impermissible), or do both readings remain logically available in a single normative framework?',
    'Philosophical analysis of axiom contradiction: if retributive moral restoration requires execution and abolition denies all capital punishment, they contradict at the level of core claims; if both can be held conditionally (retributive IF capital punishment is legitimate, abolition denies that legitimacy), they coexist in different frameworks.',
    'If the readings foreclose each other, one reading is logically impossible given the other''s acceptance (a rare situation, typically reserved for direct contradictions). If they coexist, both remain live positions held by different parties, and neither rules out the other at the level of individual framework coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relationship_to_sibling_readings, conceptual, 'The logical relationship between the retributive and abolition readings of capital punishment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__retributive_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__retributive_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t10, state_execution_authority__retributive_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(stat_tr_t10, observed).
narrative_ontology:measurement(stat_tr_t20, state_execution_authority__retributive_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(stat_tr_t20, observed).
narrative_ontology:measurement(stat_tr_t30, state_execution_authority__retributive_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(stat_tr_t30, observed).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__retributive_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement_basis(stat_tr_t40, observed).
narrative_ontology:measurement(stat_tr_t50, state_execution_authority__retributive_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(stat_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__retributive_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t10, state_execution_authority__retributive_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement_basis(stat_be_t10, observed).
narrative_ontology:measurement(stat_be_t20, state_execution_authority__retributive_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement_basis(stat_be_t20, observed).
narrative_ontology:measurement(stat_be_t30, state_execution_authority__retributive_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement_basis(stat_be_t30, observed).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__retributive_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement_basis(stat_be_t40, observed).
narrative_ontology:measurement(stat_be_t50, state_execution_authority__retributive_reading, base_extractiveness, 50, 0.82).
narrative_ontology:measurement_basis(stat_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__retributive_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t10, state_execution_authority__retributive_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(stat_su_t10, observed).
narrative_ontology:measurement(stat_su_t20, state_execution_authority__retributive_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(stat_su_t20, observed).
narrative_ontology:measurement(stat_su_t30, state_execution_authority__retributive_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement_basis(stat_su_t30, observed).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__retributive_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(stat_su_t40, observed).
narrative_ontology:measurement(stat_su_t50, state_execution_authority__retributive_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(stat_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__retributive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_execution_authority__retributive_reading, 0.12).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__deterrence_reading).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% State execution authority is a contested kernel with three distinct constraint stories: retributive_reading (this file), deterrence_reading, and abolition_reading. Each reading instantiates a different constraint with different beneficiaries, extractiveness profiles, and axioms. The retributive reading treats execution as moral restoration; deterrence reading treats it as crime prevention; abolition reading treats it as categorically impermissible violence. The three stories are linked via network.affects_constraints because they are competing readings of the same constitutional/doctrinal commitment (state capital authority). Each story's epsilon is stable within its reading (not observer-dependent); the three readings produce three ε values because they frame the standing arrangement differently. This is NOT a single constraint viewed from three angles; it is three structurally distinct constraints that happen to operate on the same institutional mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
