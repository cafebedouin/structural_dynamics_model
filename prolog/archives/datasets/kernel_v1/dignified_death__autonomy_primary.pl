% ============================================================================
% CONSTRAINT STORY: dignified_death__autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__autonomy_primary, []).

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
 *   constraint_id: dignified_death__autonomy_primary
 *   human_readable: Dignified Death: Autonomy-Primary Reading
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   The autonomy-primary reading of the dignified_death kernel grounds the
 *   right to die in individual self-determination: dignity resides in the
 *   agent's authority over their own death. A suffering individual has final
 *   authority over timing and method — this reading lodges decision power in
 *   the autonomous agent, not in medical gatekeepers, family, or state. The
 *   constraint arises from the tension between this autonomy principle and
 *   institutional prohibition mechanisms (legal bans on assisted death,
 *   professional ethics codes against physician aid, medical gatekeeping
 *   through institutional policy). The kernel conflict is deep:
 *   autonomy_primary and sanctity_primary readings rest on incompatible
 *   foundational premises about what dignity means. Relational_autonomy
 *   attempts to bridge by distributing decision authority across a relational
 *   triad (patient-family-clinician) with procedural safeguards, but this
 *   distribution directly contradicts the autonomy-primary claim that the
 *   suffering individual has FINAL authority. This constraint story
 *   instantiates only the autonomy-primary reading — the sibling readings are
 *   separate constraint stories with their own ε values, their own
 *   victim/beneficiary structures, and their own perspectives.
 *
 * KEY AGENTS:
 *   - Suffering Individual (Powerless/Trapped) — Primary victim. Denied exit by legal and medical prohibition; bears full cost of forced continuation of suffering against will.
 *   - Autonomous Agent as Rights Bearer (Institutional/Arbitrage) — Primary beneficiary. The autonomy principle explicitly grants decision authority and dignity through self-determination. Benefits from the norm even if prohibition prevents exercising it — the norm grounds their moral claim.
 *   - Clinician (Moderate/Constrained) — Secondary actor. Caught between autonomy norm (clear decision rule: patient decides) and legal prohibition (criminal liability, professional sanctions). Mixed extraction and coordination.
 *   - State Regulatory Authority (Powerful/Mobile) — Institutional actor. Enforces prohibition but also benefits from autonomy norm's coordination function (clarity, reduced arbitrariness vs discretionary case-by-case judgment).
 *   - Right-to-Die Advocacy Coalition (Organized/Constrained) — Organized victim-advocates. See autonomy-primary framework as scaffolding toward legal assisted death; building alternative pathways (ballot initiatives, litigation, policy windows).
 *   - Medical Establishment (Institutional/Constrained) — Maintains performative prohibition (Hippocratic oath, 'do no harm') while managing end-of-life practices that functionally accomplish hastened death (DNR, palliative sedation, treatment withdrawal). Theater ratio reflects this gap.
 *   - Analytical Observer (Analytical/Analytical) — Risks naturalizing autonomy principle as transcendent law; false summit detector reveals the autonomy-primary reading as contested, not universal.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__autonomy_primary, 0.52).
domain_priors:suppression_score(dignified_death__autonomy_primary, 0.68).
domain_priors:theater_ratio(dignified_death__autonomy_primary, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, extractiveness, 0.52).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__autonomy_primary, tangled_rope).
narrative_ontology:human_readable(dignified_death__autonomy_primary, "Dignified Death: Autonomy-Primary Reading").
narrative_ontology:topic_domain(dignified_death__autonomy_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__autonomy_primary, '0e0817f1-073e-4411-be5f-9d35e3c391f5').
narrative_ontology:cs_kernel_codification('0e0817f1-073e-4411-be5f-9d35e3c391f5', fixed_text).
narrative_ontology:cs_authority_grounding('0e0817f1-073e-4411-be5f-9d35e3c391f5', lineage).
narrative_ontology:cs_interpretation_layer_present('0e0817f1-073e-4411-be5f-9d35e3c391f5').
narrative_ontology:cs_reading_relation('0e0817f1-073e-4411-be5f-9d35e3c391f5', dignified_death__sanctity_primary, forecloses).
narrative_ontology:cs_reading_relation('0e0817f1-073e-4411-be5f-9d35e3c391f5', dignified_death__relational_autonomy, influences).
narrative_ontology:cs_axiom('0e0817f1-073e-4411-be5f-9d35e3c391f5', foundational, dignity_constituted_by_self_determination).
narrative_ontology:cs_axiom_status(dignity_constituted_by_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('0e0817f1-073e-4411-be5f-9d35e3c391f5', dignity_constituted_by_self_determination, deontological).
narrative_ontology:cs_axiom('0e0817f1-073e-4411-be5f-9d35e3c391f5', foundational, autonomous_agent_has_final_authority_over_death).
narrative_ontology:cs_axiom_status(autonomous_agent_has_final_authority_over_death, holdable).
narrative_ontology:cs_axiom_grounding('0e0817f1-073e-4411-be5f-9d35e3c391f5', autonomous_agent_has_final_authority_over_death, deontological).
narrative_ontology:cs_reference_frame('0e0817f1-073e-4411-be5f-9d35e3c391f5', autonomous_agent_dignity_framework).
narrative_ontology:cs_drift_state('0e0817f1-073e-4411-be5f-9d35e3c391f5', contemporary_bioethics_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0e0817f1-073e-4411-be5f-9d35e3c391f5', '').
narrative_ontology:cs_kernel_id(dignified_death__autonomy_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, autonomous_agent_with_terminal_illness).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, suffering_individuals_denied_exit).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, relational_autonomy_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUFFERING INDIVIDUAL (SNARE) — Trapped by state prohibition on assisted death despite autonomous will to end suffering. Cannot exit the constraint through legal channels; bears full extraction cost of forced continuation of suffering. Medical gatekeeping (legal requirement, physician refusal, institutional policy) removes all alternatives. Maximal experienced extraction — no negotiation space, no exit option.
constraint_indexing:constraint_classification(dignified_death__autonomy_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CLINICIAN (TANGLED ROPE) — Constrained by legal liability, professional ethics codes, institutional policy, and conflicting duties (to relieve suffering AND to preserve life). But also benefits from the autonomy norm's clarity: clear decision rule (patient decides) reduces moral ambiguity and defensive medicine overhead compared to discretionary relational gatekeeping. Mixed extraction and coordination — genuine tension between autonomy principle and enforcement barriers.
constraint_indexing:constraint_classification(dignified_death__autonomy_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AUTONOMOUS AGENT AS RIGHTS BENEFICIARY (ROPE) — The autonomy-primary reading explicitly benefits rights-bearing agents through the norm that 'dignity resides in self-determination.' For this agent, the constraint is coordination (establishing their decision authority) with minimal extraction. Exit via arbitrage: can move to jurisdictions with legal assisted death, can exit through non-legal channels at lower cost under this norm than under prohibition. Net beneficiary — extraction runs toward vulnerability, not toward this agent.
constraint_indexing:constraint_classification(dignified_death__autonomy_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE REGULATORY AUTHORITY (TANGLED ROPE) — Powerful actor with mobile exit options (can change law, can enforce selectively). But also benefits from the autonomy norm's coordination function: clear decision rule reduces institutional liability and public controversy compared to case-by-case discretion. Coordination benefit (clarity, reduced arbitrariness) entangled with extraction mechanism (prohibition enforces state authority over life-and-death decisions). Active enforcement required — prohibition is not self-executing; requires legal prohibition, criminal penalties, medical board sanctions.
constraint_indexing:constraint_classification(dignified_death__autonomy_primary, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: RIGHT-TO-DIE ADVOCACY COALITION (SCAFFOLD) — Organized agents (patient advocates, civil rights organizations, some medical ethicists) see the autonomy-primary framework as a temporary scaffolding toward legal assisted death. The constraint is experienced as a coordination problem with a sunset: expanding access (Oregon model, Belgium/Netherlands model, Canadian MAID evolution) is building alternative institutional pathways that bypass prohibition. Low effective extraction because the coalition has agency and sees an exit path with near-term feasibility (policy windows, ballot initiatives, litigation).
constraint_indexing:constraint_classification(dignified_death__autonomy_primary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: MEDICAL ESTABLISHMENT (PITON) — The Hippocratic prohibition on intentionally causing death persists through institutional inertia and oath-taking ritual despite the autonomy norm's logical contradiction with it. Medical institutions maintain the 'do no harm' framing as a cover story for prohibition while acknowledging (in advance care planning, DNR policies, palliative sedation) that hastening death under certain conditions is ethically legitimate. The constraint is substantially performative — the medical system enacts prohibition while simultaneously managing end-of-life practices that functionally accomplish what is nominally forbidden. Theater ratio reflects the gap between stated prohibition and actual practice.
constraint_indexing:constraint_classification(dignified_death__autonomy_primary, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the autonomy-primary reading risks naturalizing the autonomy principle as a transcendent moral law (dignity IS self-determination, universally and necessarily). This perspective frames the constraint as immutable: individual autonomy is an inherent feature of human dignity that cannot be overridden without logical contradiction. However, the structural data contradicts the mountain classification — the engine's false summit detector identifies this as naturalization of a philosophical reading that is actually contested by sanctity_primary and relational_autonomy frameworks. The mountain appearance masks the kernel conflict underneath.
constraint_indexing:constraint_classification(dignified_death__autonomy_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__autonomy_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dignified_death__autonomy_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dignified_death__autonomy_primary, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dignified_death__autonomy_primary, TR),
    TR >= 0.70.

:- end_tests(dignified_death__autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from suffering individuals who wish to die but are denied exit by prohibition. The extraction is real — forced continuation of unwanted suffering is a tangible harm. However, the extraction is not maximal (not 0.70+) because the autonomy-primary reading itself grants moral authority to the suffering agent, which partially mitigates the extraction. The victim experiences the constraint as violation of their recognized right (autonomy), not as mere suffering. The beneficiary (the autonomous agent in principle, the state in practice) experiences net benefit, but the coordination function (clarity of decision rule) reduces the purely extractive aspect. Suppression (0.68): Moderate-high. Significant barriers include legal prohibition (criminal liability for physicians and family), professional ethics codes (medical board sanctions), institutional gatekeeping (hospital policies), and enforcement mechanisms (prosecution, license revocation). These are structural external barriers, not merely costly — they remove legal pathways entirely. However, some non-legal exit remains possible (jurisdictional arbitrage for those with resources, informal practices, underground networks), so suppression is not maximal (not 0.85+). Theater ratio (0.55): Moderate. The constraint has both genuine coordination function (the autonomy norm provides clarity — patient decides, removing discretionary gatekeeping) and performative elements (medical institutions maintain prohibition while engaging in functionally equivalent practices like palliative sedation and treatment withdrawal). The theater reflects the gap between stated prohibition and actual end-of-life practice. Over the interval, suppression has increased (from 0.55 to 0.68) as legal regimes have tightened enforcement in response to advocacy pressure; extractiveness has increased correspondingly (from 0.38 to 0.52). Theater ratio has increased slightly (from 0.40 to 0.55) as the gap between nominally-prohibited practices and actual practice has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates substantial perspectival divergence. The suffering individual sees pure snare (trapped, no exit, maximum extraction). The right-to-die coalition sees scaffold (temporary problem with near-term sunset, legal pathway visible). The medical establishment sees piton (degraded ritual, performative prohibition masking functional practices). The state sees tangled rope (coordination benefit entangled with extraction mechanism). The clinician sees tangled rope (genuine tension between autonomy clarity and enforcement barriers). The autonomous agent as rights bearer sees rope (their moral authority is clearly established). The civilizational analytical observer risks seeing mountain (naturalizing autonomy as transcendent principle) — false summit. The perspectival gap is diagnostic: it reveals that the autonomy-primary reading is not universal law but a contested institutional choice, instantiated differently depending on the observer's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation for each perspective follows the structural relationship to extraction flow. Suffering individuals denied exit are victims with trapped exit options — structural derivation produces high d (~0.92) → high f(d) (~1.35) → high experienced χ. The autonomous agent as rights bearer is a nominal beneficiary of the autonomy norm, but extraction runs toward the victim (the suffering person denied exit), not toward this agent — structural derivation produces low d (~0.15) → low f(d) (~-0.01) → negative or minimal χ. Clinicians are constrained victims-by-entanglement: the autonomy norm benefits them through clarity (reduces moral ambiguity), but prohibition constrains them through liability exposure — structural derivation produces moderate d (~0.60) → moderate f(d) (~0.78) → moderate χ. The state is powerful with mobile exit (can change law) but benefits from the autonomy norm's coordination function (clearer rule than discretionary relational gatekeeping), so experiences mixed extraction-coordination — structural derivation produces moderate d (~0.55) → moderate f(d) (~0.72) → moderate χ. No directionality overrides needed; the derivation chain captures the structural relationships accurately.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_definition_boundary,
    'Does autonomy require decisional capacity, or does it encompass the right to refuse treatment even when capacity is questionable (dementia, depression-driven request, coercion)?',
    'Legal analysis of capacity standards across jurisdictions; empirical data on decision reversal rates post-legalization; philosophical analysis of autonomy under cognitive impairment',
    'If narrow autonomy (high capacity requirement): fewer agents qualify for exit, lower ε for constraint. If wide autonomy (lower capacity threshold): more agents qualify, higher ε for constraint, more conflict with safeguarding norms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_definition_boundary, conceptual, 'Boundary definition of decisional capacity within autonomy principle').

omega_variable(
    autonomy_vs_relational_foreclosure,
    'Does the autonomy-primary reading logically foreclose the relational_autonomy reading, or can both coexist as frameworks in different institutional contexts?',
    'Jurisprudential analysis: can a single legal system hold both ''individual autonomy is final authority'' AND ''decision authority is distributed across relational triad''? Empirical: do jurisdictions that adopt autonomy-primary eventually move toward relational safeguards, or do they maintain pure autonomy doctrine?',
    'If foreclosure: reading_relations should declare ''forecloses''. If coexistence: declare ''coexists_with''. If influence: declare ''influences''. This determines the kernel contest structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_vs_relational_foreclosure, conceptual, 'Whether autonomy-primary reading logically forecloses relational_autonomy reading').

omega_variable(
    suffering_measurement_validity,
    'Is subjective suffering (as reported by the patient) a valid measure of harm requiring exit authorization, or does it require external validation (medical diagnosis, clinical assessment, functional impairment threshold)?',
    'Empirical analysis of subjective vs objective harm measures; case law on suffering as legal criterion; neuroscience of pain perception and its relationship to harm',
    'If subjective sufficiency: lower barriers to exit authorization, higher ε for the prohibition constraint. If objective validation required: higher barriers to exit, lower ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suffering_measurement_validity, empirical, 'Validity of subjective suffering as measure of exit-authorizing harm').

omega_variable(
    reading_kernel_contest,
    'This constraint instantiates the autonomy_primary reading of the dignified_death kernel. How does this reading relate to the sanctity_primary and relational_autonomy siblings?',
    'Philosophical analysis of the three readings'' foundational axioms; examination of whether they can coexist in a single legal framework or whether institutional choice between them is zero-sum',
    'Determines cs_structure.reading_relations values (forecloses/coexists_with/influences). Determines whether the kernel itself represents a genuine logical antinomy or a preference-dependent institutional choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Structural relationship between autonomy_primary and sibling readings of dignified_death kernel').

omega_variable(
    prohibition_enforcement_efficacy,
    'Does legal prohibition on assisted death (1) actually prevent acts of mercy killing, or (2) merely drive them underground into non-transparent practices (covert physician-assisted death, family-administered overdose) with worse safeguards?',
    'Empirical: comparative analysis of jurisdictions with and without legal assisted death; data on prevalence of covert practices; analysis of palliative sedation and treatment withdrawal as functional equivalents under prohibition',
    'If prevention efficacy high: prohibition extraction mechanism is strong, ε stays high. If prevention efficacy low: prohibition is performative theater without functional effect, ε should drop, classification shifts toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prohibition_enforcement_efficacy, empirical, 'Whether legal prohibition actually prevents assisted death or drives it underground').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__autonomy_primary, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digdeath_auto_tr_t0, dignified_death__autonomy_primary, theater_ratio, 0, 0.4).
narrative_ontology:measurement(digdeath_auto_tr_t5, dignified_death__autonomy_primary, theater_ratio, 5, 0.48).
narrative_ontology:measurement(digdeath_auto_tr_t10, dignified_death__autonomy_primary, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(digdeath_auto_be_t0, dignified_death__autonomy_primary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(digdeath_auto_be_t5, dignified_death__autonomy_primary, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(digdeath_auto_be_t10, dignified_death__autonomy_primary, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(digdeath_auto_su_t0, dignified_death__autonomy_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(digdeath_auto_su_t5, dignified_death__autonomy_primary, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(digdeath_auto_su_t10, dignified_death__autonomy_primary, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__autonomy_primary, identity_coordination).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__sanctity_primary).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__relational_autonomy).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, palliative_sedation_ambiguity).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, advance_care_planning_efficacy).

% DUAL FORMULATION NOTE:
% The dignified_death kernel decomposes into three constraint stories: autonomy_primary (this file), sanctity_primary, and relational_autonomy. Each story has its own ε value, its own beneficiary/victim structure, and its own perspectives. They are linked via network.affects_constraints because they represent alternative institutional readings of the same foundational commitment. The ε-invariance principle applies: if changing the foundational axiom (from autonomy to sanctity to relational) changes the structural classification, then the constraints are genuinely distinct. They are not multiple measurements of one constraint; they are three different constraints grounded in three different ethical kernels. Upstream constraints (palliative_sedation_ambiguity, advance_care_planning_efficacy) instantiate functionally-equivalent practices that exist under prohibition; they influence the autonomy_primary constraint by providing covert pathways that reduce suppression in practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
