% ============================================================================
% CONSTRAINT STORY: autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_autonomy_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: autonomy_reading
 *   human_readable: Individual Autonomy and End-of-Life Choice (Autonomy Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint instantiates the AUTONOMY READING of the contested kernel
 *   'end_of_life_authority.' The autonomy reading grounds the right to
 *   control death's timing in individual self-determination: when facing
 *   unbearable suffering with no prospect of relief, respecting the person as
 *   an autonomous agent means respecting their decision about when to end
 *   their life. This reading is one of three competing interpretations of the
 *   same kernel (end-of-life authority). The SANCTITY READING grounds
 *   authority in the transcendent value of life independent of the person's
 *   will — death timing is controlled by natural process or divine authority,
 *   not by individual choice. The SLIPPERY SLOPE READING grants that some
 *   autonomy may apply to death timing but claims that permissive frameworks
 *   inevitably expand to include non-terminal suffering, psychological
 *   distress, and eventually non-voluntary deaths. This story generates ONLY
 *   the autonomy reading as a clean ε-invariant constraint, with its own
 *   beneficiary/victim structure, suppression mechanisms, and temporal
 *   trajectory. The sibling readings are separate constraint stories (not
 *   included here). The autonomy reading produces a TANGLED ROPE
 *   classification: it coordinates genuine interests (respecting persons as
 *   self-determining agents, preventing suffering) while simultaneously
 *   extracting through suppression (legal prohibition, physician gatekeeping,
 *   institutional resistance to patient choice). The constraint exhibits
 *   perspectival range from SNARE (powerless patient trapped by prohibition)
 *   through ROPE (permissive jurisdictions coordinating choice) to PITON
 *   (religious institutions maintaining opposition through inertia). The
 *   theater_ratio (0.35) is relatively low because the autonomy reading
 *   relies on functional verification mechanisms (patient capacity
 *   assessment, consultation, waiting periods) rather than on performative
 *   ritual — permissive jurisdictions test the reading against real outcomes
 *   rather than maintaining symbolic authority.
 *
 * KEY AGENTS:
 *   - Patients facing unbearable suffering: Primary victim (powerless/trapped) — denied choice by legal prohibition and medical gatekeeping; bear full cost of paternalistic restriction
 *   - Patients with jurisdictional mobility: Secondary agent (moderate/constrained) — can exit to permissive jurisdictions at high cost (travel, time, resources); constrained by remaining lifespan and mobility limitations
 *   - Permissive jurisdictions (Netherlands, Belgium, Switzerland, Oregon, Canada): Primary beneficiary (institutional/arbitrage) — benefit through alignment with liberal autonomy norms, demonstrated safe implementation, institutional legitimacy; arbitrage exit through policy choice
 *   - Restrictive jurisdictions: Institutional actor (institutional/constrained) — maintain prohibition through legal and medical gatekeeping; constrained exit by political, religious, and professional commitments
 *   - Right-to-die advocacy movement: Organized agent (organized/constrained) — build pressure for policy change through evidence accumulation and generational succession; constrained by need for sustained political coalition
 *   - Medical profession: Powerful divided actor (powerful/mobile) — coordinate patient autonomy in permissive frameworks or maintain prohibition in restrictive ones; mobile exit through jurisdictional choice
 *   - Religious institutions: Institutional actor (institutional/constrained) — oppose on grounds of sanctity of life; constrained exit by foundational commitments; decreasing institutional force through secularization
 *   - Analytical observer: Sees potential false summit in 'natural law' view of death inevitability (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(autonomy_reading, 0.38).
domain_priors:suppression_score(autonomy_reading, 0.68).
domain_priors:theater_ratio(autonomy_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(autonomy_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(autonomy_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(autonomy_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(autonomy_reading, tangled_rope).
narrative_ontology:human_readable(autonomy_reading, "Individual Autonomy and End-of-Life Choice (Autonomy Reading)").
narrative_ontology:topic_domain(autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(autonomy_reading, 'fff4e6a7-c07a-4da6-8de1-983f8b6fc199').
narrative_ontology:cs_created_at('fff4e6a7-c07a-4da6-8de1-983f8b6fc199', '').
narrative_ontology:cs_kernel_codification('fff4e6a7-c07a-4da6-8de1-983f8b6fc199', distributed).
narrative_ontology:cs_authority_grounding('fff4e6a7-c07a-4da6-8de1-983f8b6fc199', extraction).
narrative_ontology:cs_kernel_id(autonomy_reading, end_of_life_authority).
narrative_ontology:cs_reading_relation('fff4e6a7-c07a-4da6-8de1-983f8b6fc199', sanctity_reading, forecloses).
narrative_ontology:cs_reading_relation('fff4e6a7-c07a-4da6-8de1-983f8b6fc199', slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('fff4e6a7-c07a-4da6-8de1-983f8b6fc199', foundational, respect_for_autonomous_choice_over_unbearable_suffering).
narrative_ontology:cs_axiom_status(respect_for_autonomous_choice_over_unbearable_suffering, holdable).
narrative_ontology:cs_axiom('fff4e6a7-c07a-4da6-8de1-983f8b6fc199', foundational, permissive_frameworks_prevent_coercion_through_safeguards).
narrative_ontology:cs_axiom_status(permissive_frameworks_prevent_coercion_through_safeguards, holdable).
narrative_ontology:cs_reference_frame('fff4e6a7-c07a-4da6-8de1-983f8b6fc199', individual_self_determination_authority).
narrative_ontology:cs_drift_state('fff4e6a7-c07a-4da6-8de1-983f8b6fc199', contemporary_evidence_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(autonomy_reading, patients_with_agency).
narrative_ontology:constraint_beneficiary(autonomy_reading, medical_practitioners_autonomy_respecting).
narrative_ontology:constraint_victim(autonomy_reading, patients_denied_choice).
narrative_ontology:constraint_victim(autonomy_reading, suffering_individuals_constrained).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUFFERING PATIENT (SNARE) — Trapped by legal prohibition and medical gatekeeping. Faces maximum suppression (0.68): legal barriers, physician discretion, institutional resistance, social stigma. No exit option from either the suffering or the constraint that prevents relief. Experiences pure extraction: the constraint denies autonomy without coordination benefit. The powerless agent bears the full cost of paternalistic restriction.
constraint_indexing:constraint_classification(autonomy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PATIENT WITH JURISDICTIONAL OPTIONS (TANGLED ROPE) — Can potentially travel to jurisdictions permitting assistance in dying (Oregon, Netherlands, Switzerland). Exit option exists but is constrained by cost, mobility limitations, distance, and remaining life expectancy. Experiences mixed coordination and extraction: some jurisdictions coordinate patient choice and medical assistance; others extract through travel burden and fragmented access. Moderate power through exit option; biographical time horizon reflects feasibility within remaining lifespan.
constraint_indexing:constraint_classification(autonomy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PERMISSIVE JURISDICTION (ROPE) — Jurisdictions that permit medical assistance in dying (Netherlands, Belgium, Switzerland) experience this constraint as pure coordination: balancing patient autonomy with safeguarding against coercion. The framework coordinates genuine competing interests (respecting choice vs preventing abuse) with minimal extraction. Beneficiary through institutional legitimacy and alignment with liberal autonomy norms. Arbitrage exit: these jurisdictions can move toward or away from permissive policies depending on political will and evidence.
constraint_indexing:constraint_classification(autonomy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RESTRICTIVE JURISDICTION (TANGLED ROPE) — Jurisdictions maintaining blanket prohibition experience genuine coordination function (preventing coercion of vulnerable patients, maintaining trust in medicine) alongside asymmetric extraction (denying autonomous choice). Constrained exit: legal, religious, and political commitments limit policy flexibility. Generational time horizon reflects slow cultural change in end-of-life norms. Requires active enforcement (criminal penalties, professional discipline) to maintain prohibition.
constraint_indexing:constraint_classification(autonomy_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RIGHT-TO-DIE MOVEMENT (SCAFFOLD) — Organized advocates (patient rights organizations, civil liberties groups, medical ethicists) see the autonomy constraint as a temporary policy problem with a structural sunset. Empirical evidence of safe implementation (Netherlands 25+ years, Oregon 27+ years, Canada expanding) demonstrates that permissive frameworks do not produce predicted harms (mass coercion, pressure on vulnerable patients). As evidence accumulates and demographic pressures mount (aging populations, chronic suffering), restrictive policies face generational pressure toward liberalization. Constrained exit reflects that policy change requires sustained advocacy; scaffold classification reflects that the sunset is real and predictable — eligibility expansion is the observed trajectory.
constraint_indexing:constraint_classification(autonomy_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: MEDICAL PROFESSION (TANGLED ROPE) — The medical community globally is divided on end-of-life assistance. Some physicians and specialties (palliative care, oncology) coordinate patient autonomy within permissive frameworks; others maintain prohibition on grounds of medical ethics (duty to preserve life, risk of coercion). Mobile exit option: individual physicians can select jurisdictions aligning with their values; powerful institutional position shapes policy debate. Immediate time horizon reflects career-length decisions about where to practice. Tangled rope because the profession simultaneously coordinates patient care (genuine function) and extracts through gatekeeping (denying some patients' autonomous choices).
constraint_indexing:constraint_classification(autonomy_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: RELIGIOUS INSTITUTIONAL OPPOSITION (PITON) — Religious institutions (Vatican, traditional theological authorities) maintain opposition to medical assistance in dying on grounds of sanctity of life and divine authority over death. Theater ratio (0.35 for this reading overall, but localized high theater in ritual authority claims) reflects that the institutional position increasingly relies on performative authority rather than functional control. Civilizational horizon captures that religious opposition frames itself as transcendent and unchanging. Constrained exit: religious institutions cannot easily abandon foundational commitments without institutional crisis. Piton because institutional position persists through inertia and legitimacy claims rather than through active enforcement of restrictive policies — most restrictive jurisdictions maintain prohibition through secular law and paternalistic medical ethics, not direct religious governance.
constraint_indexing:constraint_classification(autonomy_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 8: ANALYTICAL / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some might argue that the constraint is inherent to the human condition: death is inevitable, suffering is part of existence, and medicine has limits. This perspective risks naturalizing what is actually a contestable normative commitment about who controls the timing of death. The analytical observer sees the mountain as a false summit — the claim that autonomy cannot govern death timing appears immutable only from within a framework that treats life-preservation as supreme value. The autonomy reading reverses this: death inevitability is the mountain; WHO CONTROLS THE TIMING is the contested policy constraint.
constraint_indexing:constraint_classification(autonomy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(autonomy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(autonomy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(autonomy_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(autonomy_reading, TR),
    TR >= 0.70.

:- end_tests(autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.38): Moderate. The autonomy reading's base extraction reflects suppression of patient choice without strong coordination benefits in restrictive jurisdictions (0.68 suppression). However, the extraction is not maximal because: (1) some jurisdictions implement permissive frameworks successfully, demonstrating that alternative institutional arrangements can respect autonomy while preventing abuse; (2) the constraint is contestable — no structural law requires prohibition; (3) empirical evidence shows that patient outcomes and safety in permissive jurisdictions are comparable to or better than restrictive jurisdictions, suggesting the restriction serves political/institutional interests rather than genuine patient protection. The temporal increase from 0.22 to 0.38 reflects that restrictive jurisdictions' prohibition becomes increasingly costly (measured in accumulated suffering, jurisdictional inequality, policy lag behind evidence) as evidence of safe implementation accumulates. SUPPRESSION (0.68): High. Significant barriers to autonomous choice include: legal prohibition with criminal penalties; physician gatekeeping (doctors can refuse to assess or assist despite patient request); institutional resistance (hospital policies against discussion or referral); social stigma; information barriers (restrictive jurisdictions prohibit public education about options); unequal access (only wealthy patients can travel to permissive jurisdictions). Suppression is not maximal (0.68 < 1.0) because some patients do access assistance (through jurisdictional travel, physician cooperation in gray zones, or through informal networks). THEATER_RATIO (0.35): Low-moderate. The autonomy reading relies relatively less on performative ritual and more on functional verification. Permissive jurisdictions test the framework against real outcomes: patient capacity assessments, physician consultation, waiting periods, and longitudinal follow-up reveal whether safeguards prevent coercion or merely create appearance of caution. The functional verification mechanism makes the constraint less dependent on theater than prohibitionist framing (which relies on symbolic protection and abstract harm prevention). The theater_ratio increases slightly over the interval (0.28 to 0.35) as restrictive jurisdictions' prohibition becomes harder to justify empirically — they increasingly rely on performative protection claims rather than demonstrated safety benefits.
 *
 * PERSPECTIVAL GAP:
 *   The autonomy reading produces maximal perspectival range across power levels. The powerless patient sees SNARE: pure extraction without coordination benefit, trapped exit, biographical horizon (choice must be made before death). The moderate patient with jurisdictional mobility sees TANGLED ROPE: some coordination (respecting autonomy) alongside extraction (cost and burden of travel). The permissive jurisdiction sees ROPE: coordination of competing interests (autonomy vs preventing coercion) with institutional benefit (legitimacy alignment). The restrictive jurisdiction sees TANGLED ROPE: coordination function (protecting vulnerable from coercion) alongside asymmetric extraction (denying autonomous choice). The medical profession (powerful) sees divided classification: ROPE in permissive contexts (coordinating patient autonomy and medical ethics), TANGLED ROPE in restrictive contexts (coordinating professional ethics with institutional gatekeeping). The right-to-die movement sees SCAFFOLD: a temporary policy problem with visible sunset (evidence of safe implementation, demographic pressure, generational change). Religious institutions see PITON: institutional opposition persisting through inertia as secular legal barriers remain primary enforcement mechanism. The analytical observer risks false-summit MOUNTAIN: naturalizing contestable policy choice (autonomy denial) as inevitable fact of human condition. The perspectival gap reveals that the constraint's classification depends entirely on power position and exit options — the same structural mechanism (legal prohibition + physician gatekeeping + suppression) appears as different constraint types from different perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is computed from: agent power level + exit options + beneficiary/victim relationship to the constraint. The autonomy reading's directionality pipeline derives d as follows: (1) Powerless patient trapped by prohibition: d→1.0 (full target of extraction); (2) Moderate patient with constrained exit: d→0.75 (target, but with exit option reducing effective extraction); (3) Beneficiary patient with agency in permissive jurisdiction: d→0.15 (beneficiary, since constraint respects autonomy); (4) Institutional beneficiary (permissive jurisdiction): d→0.10 (institutional arbitrage exit + beneficiary status); (5) Institutional actor in restrictive jurisdiction: d→0.55 (constrained exit + mixed beneficiary/victim — coordinates public safety but extracts through prohibition); (6) Organized coalition (right-to-die advocates): d→0.60 (victim of restrictive policy but organized exit through advocacy pressure); (7) Medical profession: d→0.50 (symmetric position — some physicians benefit from autonomy respect, others benefit from gatekeeping authority); (8) Analytical observer: d→0.72 (analytical position observing the constraint structure). The directionality spread reflects that the autonomy reading genuinely asymmetrically affects different agents: those denied choice (high d) versus those empowered by autonomy respect (low d).
 *
 * MANDATROPHY ANALYSIS:
 *   The autonomy reading resolves mandatrophy by showing that the constraint is genuinely TANGLED ROPE (not pure extraction, not pure coordination): it coordinates real interests (preventing coercion of vulnerable patients, maintaining trust in medicine) while extracting from those denied autonomous choice. The mandatrophy question ('Is this extraction justified?') depends on two empirical omega variables: (1) Do permissive frameworks with safeguards actually prevent coercion? (2) Can 'unbearable suffering' be defined with sufficient clarity to prevent scope creep? If both questions resolve positively, the extraction is partly unjustified — the restriction prevents less harm than it causes. If both resolve negatively, the extraction is justified as protection of the vulnerable. The autonomy reading bets on empirical support for (1) and (2) — the evidence from permissive jurisdictions 25+ years demonstrates that safeguards work and that definitional drift is manageable. This empirical trajectory suggests the constraint is moving toward SCAFFOLD (temporary protection with sunset) as evidence accumulates and policies liberalize.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_mechanism_empirical,
    'In permissive jurisdictions, what is the actual rate of coercive or non-autonomous end-of-life decisions compared to restrictive jurisdictions?',
    'Longitudinal comparative analysis of Netherlands, Belgium, Switzerland (permissive) vs. restrictive jurisdictions; interview studies of patients and families; mortality statistics and autopsy comparisons; detection of non-voluntary deaths through epidemiological methods',
    'If coercion rates are lower in permissive jurisdictions: the restriction is purely extractive (snare classification strengthened). If coercion rates are comparable: the paternalistic restriction has some empirical justification (tangled rope classification strengthened). If rates are higher in permissive jurisdictions: the restriction is partially justified protection (scaffold classification becomes operative).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_mechanism_empirical, empirical, 'Actual coercion rates in permissive vs restrictive jurisdictions').

omega_variable(
    unbearable_suffering_definition,
    'Can ''unbearable suffering'' be defined with sufficient clarity to prevent expansion of eligibility to those not facing terminal illness or intractable physical pain?',
    'Analysis of eligibility criteria drift in permissive jurisdictions over time (Netherlands: 1984 terminal → 2002 untreatable suffering → 2020 psychological suffering); comparative legal analysis of statutory definitions; case law review showing grounds for approval/denial',
    'If definition remains stable or narrows: autonomy reading holds strong (beneficiaries remain those genuinely facing unbearable suffering). If definition consistently expands: slippery slope mechanism reading gains empirical support (scope creep from physical to psychological to social suffering). The trajectory determines whether the reading''s own foundational axiom (respect_for_autonomous_choice_over_unbearable_suffering) is foreclosed or overridden by expansion beyond intended scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unbearable_suffering_definition, empirical, 'Whether ''unbearable suffering'' definition drifts over time in permissive jurisdictions').

omega_variable(
    vulnerable_population_safeguarding,
    'Do permissive frameworks with safeguards (multiple physician consultation, waiting periods, capacity assessment) actually prevent exploitation of cognitively impaired or depressed patients, or does the safeguard theater create illusion of protection while vulnerable populations remain at risk?',
    'Epidemiological analysis of decision-making capacity assessments in approved cases; comparison of psychiatric diagnoses and depression screening rates in those approved vs denied; qualitative analysis of safeguard protocol adherence; follow-up studies on factors influencing approval/denial decisions',
    'If safeguards are genuinely effective: tangled rope with legitimate enforcement reduces to rope (coordination without extraction for vulnerable populations). If safeguards are performative: theater ratio rises, piton classification becomes operative, and the autonomy reading''s axiom (respect_for_autonomous_choice) is foreclosed for cognitively impaired patients. The reading must then specify: does autonomy respect apply only to unimpaired agents, or do vulnerable populations require additional protection?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_population_safeguarding, empirical, 'Effectiveness of safeguards in protecting vulnerable populations from non-autonomous decisions').

omega_variable(
    reading_kernel_distinction,
    'Does the autonomy reading instantiate a distinct normative kernel from the sanctity reading, or are both readings interpretations of a single underlying principle about life''s value?',
    'Philosophical analysis of whether autonomy and sanctity of life can coexist within a single normative framework. If autonomy means ''agent controls own fate'' and sanctity means ''life has transcendent value independent of agent will,'' can these be jointly held? (Answer: only if authority is distributed between agent and transcendent source — requires framework revision or hierarchy specification.)',
    'If readings share a kernel: classification revises to ''coexists_with'' (different parties hold different interpretations). If readings have distinct kernels: autonomy reading forecloses sanctity reading (in any single framework, either agent autonomy or divine/transcendent authority controls death timing, not both). The distinction determines whether the constraint''s authority_grounding is ''extraction'' (paternalistic restriction) or ''expertise'' (medical judgment about unbearable suffering).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_distinction, conceptual, 'Whether autonomy and sanctity readings share a single kernel or have distinct normative foundations').

omega_variable(
    physician_participation_framing,
    'Does framing medical assistance in dying as ''physician-assisted'' vs ''patient self-determined with physician support'' change the constraint classification, or are these framings semantically equivalent?',
    'Comparative analysis of how Netherlands (''assistance in dying'' by physician) vs Oregon (''physician-assisted suicide'' — patient self-administers) produces different legal, ethical, and empirical outcomes; interview studies of physician and patient decision-making under each framing',
    'If framings are empirically equivalent: constraint classification is stable across both. If framings produce different patient/physician behavior: the autonomy reading may depend on the ''physician support'' framing to maintain safeguards and ethical alignment, and ''physician-assisted suicide'' framing shifts toward snare (extraction of decision authority from patient to physician).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physician_participation_framing, empirical, 'Whether framing (assistance in dying vs physician-assisted suicide) affects decision outcomes and constraint classification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(autonomy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(auto_tr_t0, autonomy_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(auto_tr_t10, autonomy_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(auto_tr_t20, autonomy_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(auto_be_t0, autonomy_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(auto_be_t10, autonomy_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(auto_be_t20, autonomy_reading, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(autonomy_reading, resource_allocation).
narrative_ontology:affects_constraint(autonomy_reading, sanctity_reading).
narrative_ontology:affects_constraint(autonomy_reading, slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% The end-of-life authority kernel is decomposed into three separate constraint stories corresponding to three competing readings. Each reading instantiates a different ε value reflecting different empirical claims about safe implementation, scope drift, and institutional stability. AUTONOMY_READING (this story): ε=0.38, grounds authority in individual self-determination, predicts stable safe implementation. SANCTITY_READING: ε differs because it grounds authority in life's transcendent value and predicts maximum harm from any permissive framework. SLIPPERY_SLOPE_READING: ε differs because it predicts inevitable scope expansion and institutional drift. These are not measurements of the same constraint under different observables — they are claims about different institutional arrangements (permissive vs restrictive) with different empirical predictions. The network links enable the engine to track how evidence about permissive jurisdiction outcomes directly tests the three readings' competing empirical claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
