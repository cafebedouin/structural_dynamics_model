% ============================================================================
% CONSTRAINT STORY: sixth_amendment__gideon_counsel_revolution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sixth_amendment__gideon_counsel_revolution, []).

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
 *   constraint_id: sixth_amendment__gideon_counsel_revolution
 *   human_readable: Gideon v. Wainwright: The Right to Counsel as Gateway Constitutional Guarantee
 *   domain: legal/constitutional/criminal_procedure
 *
 * SUMMARY:
 *   Gideon v. Wainwright (1963) established that the Sixth Amendment
 *   guarantees the right to counsel in felony cases, with appointed counsel
 *   for defendants who cannot afford private representation. This constraint
 *   represents ONE reading of the contested Sixth Amendment kernel — a
 *   reading that makes appointed counsel the gateway right that
 *   operationalizes all other protections. Without counsel, a defendant
 *   cannot effectively invoke confrontation, challenge evidence, or navigate
 *   jury selection. The Gideon reading posits that counsel access is the
 *   foundational prerequisite that makes the other Sixth Amendment rights
 *   cognizable. This reading coexists with Crawford's confrontation reading
 *   (which privileges the right to cross-examine witnesses) and the jury
 *   cross-section reading (which grounds legitimacy in jury composition). The
 *   Gideon constraint shows how a single constitutional commitment can
 *   produce multiple structurally distinct readings, each with different
 *   beneficiary/victim structures and different extractive characteristics.
 *   Post-Gideon, the constraint has evolved: the formal right to appointed
 *   counsel now exists nationwide, but the structural capacity to deliver
 *   effective counsel remains underfunded. Theater has increased as the gap
 *   widens between the right as stated and as practiced. Suppression has
 *   decreased (fewer defendants face truly unrepresented trials) but
 *   extraction has increased (underfunded counsel creates structural pressure
 *   toward plea bargains and conviction).
 *
 * KEY AGENTS:
 *   - Indigent Felony Defendants: Primary beneficiary (powerless/trapped pre-Gideon, constrained post-Gideon) — Gideon suppresses lawyerless prosecution and provides access to trained counsel; but underfunding constrains counsel effectiveness
 *   - Public Defender Organizations: Institutional actor (organized/constrained) — implement the right to counsel; coordinate delivery across thousands of cases; face structural underfunding that limits their function
 *   - State Criminal Justice System: Institutional beneficiary (institutional/arbitrage) — gains coordination benefits from predictable procedural regime and appointed-counsel system; avoids reversals and liability
 *   - Conviction-by-Default Economics: Victim (powerless/trapped) — the structural mechanism that Gideon suppresses — unrepresented defendants defaulting to conviction; this is not an agent but a mechanism that Gideon's suppression targets
 *   - Public Defender Resource Scarcity: Victim (organized/constrained) — the extraction mechanism that post-Gideon underfunding perpetuates — excessive caseloads and limited expert support systematically constrain counsel effectiveness
 *   - Judicial System: Institutional mediator (institutional/arbitrage) — administers the Gideon right; experiences reduced appellate reversals and increased procedural efficiency; has incentive to formalize the right while resisting resource demands
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sixth_amendment__gideon_counsel_revolution, 0.38).
domain_priors:suppression_score(sixth_amendment__gideon_counsel_revolution, 0.62).
domain_priors:theater_ratio(sixth_amendment__gideon_counsel_revolution, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sixth_amendment__gideon_counsel_revolution, extractiveness, 0.38).
narrative_ontology:constraint_metric(sixth_amendment__gideon_counsel_revolution, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sixth_amendment__gideon_counsel_revolution, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sixth_amendment__gideon_counsel_revolution, tangled_rope).
narrative_ontology:human_readable(sixth_amendment__gideon_counsel_revolution, "Gideon v. Wainwright: The Right to Counsel as Gateway Constitutional Guarantee").
narrative_ontology:topic_domain(sixth_amendment__gideon_counsel_revolution, "legal/constitutional/criminal_procedure").

domain_priors:requires_active_enforcement(sixth_amendment__gideon_counsel_revolution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sixth_amendment__gideon_counsel_revolution, '6d2b50ed-874d-4495-9d0c-5cfd5eb33b78').
narrative_ontology:cs_kernel_codification('6d2b50ed-874d-4495-9d0c-5cfd5eb33b78', formalized).
narrative_ontology:cs_authority_grounding('6d2b50ed-874d-4495-9d0c-5cfd5eb33b78', lineage).
narrative_ontology:cs_interpretation_layer_present('6d2b50ed-874d-4495-9d0c-5cfd5eb33b78').
narrative_ontology:cs_reading_relation('6d2b50ed-874d-4495-9d0c-5cfd5eb33b78', sixth_amendment__confrontation_crawford_reading, influences).
narrative_ontology:cs_reading_relation('6d2b50ed-874d-4495-9d0c-5cfd5eb33b78', sixth_amendment__jury_cross_section_reading, influences).
narrative_ontology:cs_axiom('6d2b50ed-874d-4495-9d0c-5cfd5eb33b78', foundational, counsel_access_prerequisite_for_fair_trial).
narrative_ontology:cs_axiom_status(counsel_access_prerequisite_for_fair_trial, holdable).
narrative_ontology:cs_axiom_grounding('6d2b50ed-874d-4495-9d0c-5cfd5eb33b78', counsel_access_prerequisite_for_fair_trial, deontological).
narrative_ontology:cs_axiom('6d2b50ed-874d-4495-9d0c-5cfd5eb33b78', secondary, state_obligation_to_finance_counsel).
narrative_ontology:cs_axiom_status(state_obligation_to_finance_counsel, holdable).
narrative_ontology:cs_axiom_grounding('6d2b50ed-874d-4495-9d0c-5cfd5eb33b78', state_obligation_to_finance_counsel, deontological).
narrative_ontology:cs_reference_frame('6d2b50ed-874d-4495-9d0c-5cfd5eb33b78', lawyerless_felony_trial_regime).
narrative_ontology:cs_drift_state('6d2b50ed-874d-4495-9d0c-5cfd5eb33b78', contemporary_underfunded_appointment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6d2b50ed-874d-4495-9d0c-5cfd5eb33b78', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(sixth_amendment__gideon_counsel_revolution, sixth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sixth_amendment__gideon_counsel_revolution, indigent_felony_defendants).
narrative_ontology:constraint_victim(sixth_amendment__gideon_counsel_revolution, conviction_by_default_economics).
narrative_ontology:constraint_victim(sixth_amendment__gideon_counsel_revolution, public_defender_resource_scarcity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNREPRESENTED DEFENDANT (SNARE) — Before Gideon, a felony defendant without resources faced the court unrepresented, navigating statutory procedure, evidentiary rules, and cross-examination without training. The suppression is absolute: no exit exists within the legal process. The extraction is maximal — conviction without counsel functions as a default outcome. This perspective shows what the constraint suppresses.
constraint_indexing:constraint_classification(sixth_amendment__gideon_counsel_revolution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIGENT DEFENDANT POST-GIDEON (TANGLED ROPE) — With appointed counsel, the defendant has genuine coordination benefit: a trained advocate navigates procedure, cross-examines witnesses, and marshals defense strategy. But the constraint also extracts: the public defender system is systematically underfunded, caseloads are excessive, and the quality of representation varies dramatically by jurisdiction. The defendant is no longer trapped, but constrained — they receive counsel, yet that counsel's capacity to mount a vigorous defense is structurally limited. Both coordination and extraction operate simultaneously.
constraint_indexing:constraint_classification(sixth_amendment__gideon_counsel_revolution, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE CRIMINAL JUSTICE SYSTEM (ROPE) — From the state's perspective, Gideon is pure coordination: appointed counsel speeds trials, reduces appellate reversals on ineffective-assistance grounds, and creates a predictable procedural regime. The state benefits from faster case disposition and reduced institutional liability for convictions obtained without counsel. The coordination function is primary; extraction is not the point.
constraint_indexing:constraint_classification(sixth_amendment__gideon_counsel_revolution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC DEFENDER ORGANIZATION (TANGLED ROPE) — Gideon created the public defender system (or expanded existing systems) and thus the mechanism through which counsel is appointed. The PD system has genuine coordination function: it organizes the delivery of legal defense across thousands of cases and jurisdictions. But it also exhibits extraction: inadequate funding relative to caseload creates systematic constraints on vigorous defense. The PD system coordinates access to counsel while simultaneously suppressing the quality of that counsel through resource scarcity.
constraint_indexing:constraint_classification(sixth_amendment__gideon_counsel_revolution, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SIXTH AMENDMENT RITUAL (PITON) — At the civilizational timescale, the Gideon right has become largely performative. The formal ritual of appointment occurs — counsel is assigned at arraignment — but the structural conditions for effective counsel (adequate time, resources, expert support) are often absent. The ritual persists because the constitutional mandate cannot be repealed; the actual function has atrophied. Theater ratio is moderate but rising as underfunding widens the gap between the right as stated and the right as practiced.
constraint_indexing:constraint_classification(sixth_amendment__gideon_counsel_revolution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the right to counsel might appear immutable: the logical prerequisite for a fair trial is that the defendant can mount a defense, and mounting a defense requires legal knowledge that the layperson lacks. This perspective naturalizes the constraint as a consequence of the unavoidable asymmetry between trained counsel and untrained defendants. However, the structural data reveals this as a false summit: Gideon is a specific historical choice (1963) about how to organize counsel access, not an inevitable feature of adjudication. The 'natural law' reading conceals the contingent institutional arrangements (underfunding, caseload, resource allocation) that define the constraint's extractive character.
constraint_indexing:constraint_classification(sixth_amendment__gideon_counsel_revolution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sixth_amendment__gideon_counsel_revolution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sixth_amendment__gideon_counsel_revolution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sixth_amendment__gideon_counsel_revolution, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sixth_amendment__gideon_counsel_revolution, TR),
    TR >= 0.70.

:- end_tests(sixth_amendment__gideon_counsel_revolution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, rising. At the point of Gideon (1963), extractiveness was low (0.15) — the constraint was primarily suppression of lawyerless prosecution without significant extraction. As decades progressed and public defender funding failed to keep pace with caseload growth, extractiveness increased. The current value (0.38) reflects that the state gains coordination benefit (faster case disposition) while defendants experience extraction (underfunded counsel, excessive caseloads, systematic pressure toward plea). The trajectory shows extraction accumulating as the state captures the efficiency gains while defendants bear the under-resourcing cost. Suppression (0.62): Moderate-high and declining. At Gideon, suppression was maximal (0.85) — lawyerless prosecution was pervasive and absolute. Post-Gideon, the formal suppression of lawyerless trials has declined substantially; appointed counsel now exists nationwide. But suppression remains high because the structural capacity of appointed counsel to mount vigorous defense is systematically constrained by funding. Underfunding is itself a suppression mechanism: it suppresses the defendant's ability to exercise the right they nominally possess. Theater ratio (0.48): Moderate and rising. The formal ritual of appointing counsel at arraignment occurs with fidelity — this is not theater. But the gap between the formal right and the actual capacity to exercise it has widened, creating performative elements: counsel is appointed but often lacks time, resources, and expert support to provide the defense the right purports to guarantee. The measurement trajectory shows theater accumulating as institutions formalize the right while constraining its substance.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is dramatic. The powerless defendant sees Snare pre-Gideon (trapped, maximal extraction via conviction default) and Tangled Rope post-Gideon (rescued from the snare but constrained by underfunding). The state sees pure Rope (coordination benefit, faster disposition, reduced liability). The public defender system sees Tangled Rope (genuine coordination function mixed with underfunding extraction). The ritual-level view sees Piton (formal appointment persisting through inertia, substance atrophied). The analytical observer faces pressure toward Mountain (naturalizing counsel as logical prerequisite) but the structural data reveals this as false summit: Gideon was a historical choice to remedy a recognized problem, not a discovery of natural law. The constraint's power to coordinate depends entirely on adequate resourcing; the natural-law reading conceals this contingency.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural relationship to the constraint. Indigent defendants are primary victims receiving beneficiary treatment (low d toward beneficiary, deriving from victim status + constrained exit + underfunded appointment = d~0.72). Public defenders are organized actors with constrained exit facing underfunding (d~0.60). The state system is institutional beneficiary with arbitrage exit (d~0.20, net positive gain from coordination). The piton perspective derives from theater_ratio rising above 0.48, indicating performative elements exceeding functional ones. The mountain perspective is a false-summit candidate: it naturalizes the counsel requirement as logically necessary, but the structural data reveals it as a specific institutional choice (Gideon as 1963 innovation, not discovery of inherent law).
 *
 * MANDATROPHY ANALYSIS:
 *   Gideon exemplifies mandatrophy resolution through kernel-reading analysis. The same Sixth Amendment produces three distinct readings: Gideon's counsel-as-gateway, Crawford's confrontation-as-test, and jury cross-section's legitimacy-through-composition. Each reading instantiates different constraints with different extractive characteristics. Gideon is Tangled Rope post-1963, but only because the state captured the coordination benefits while constraining the counsel capacity. If Gideon had been fully resourced (counsel caseloads at 50 cases/year instead of 300), it would classify as pure Rope — the constraint would coordinate defender access without extraction. The mandatrophy is resolved not by choosing one reading as correct, but by recognizing that the reading's structural outcome depends on implementation (adequate resourcing). The reading itself is stable; the constraint's extractive character changed historically as underfunding accumulated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adequacy_of_counsel_threshold,
    'What level of legal resources constitutes effective counsel sufficient to remedy the structural suppression of lawyerless prosecution?',
    'Empirical correlation between caseload, hours per case, expert access, and appellate reversal rates on Strickland ineffective-assistance claims; comparison across jurisdictions with different funding models',
    'If threshold is achievable with current funding: Gideon is primarily Rope (coordination). If threshold requires 3x current spending: Gideon is primarily Tangled Rope (genuine extraction via resource constraint). If threshold is unachievable due to volume: Gideon is performative (Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adequacy_of_counsel_threshold, empirical, 'Whether current public defender resources meet threshold for effective counsel').

omega_variable(
    natural_law_vs_constructed_choice,
    'Is the right to counsel a logical prerequisite for fair trial (natural law) or a specific historical institutional choice about how to remedy conviction-by-default economics?',
    'Comparative constitutional study: do alternative jurisdictions guarantee counsel through different mechanisms (duty to provide witnesses, transcripts, appellate review)? Does the historical record show Gideon as innovation (solving a recognized problem) or discovery (revealing an inherent requirement)?',
    'If natural law: Mountain classification holds; the constraint is unchangeable. If constructed choice: Gideon is a Tangled Rope with specifc extractive elements (underfunding, caseload) that could be redesigned. This omega determines whether the analytical perspective should classify as Mountain or Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_choice, conceptual, 'Whether counsel right is logical prerequisite or historical institutional choice').

omega_variable(
    kernel_reading_contest,
    'How does Gideon''s counsel-as-gateway reading relate to Crawford''s confrontation reading and the jury cross-section reading within the Sixth Amendment''s coherence?',
    'Doctrinal analysis of how courts have integrated these three readings; identification of cases where counsel and confrontation readings conflict (e.g., can counsel waive confrontation?); analysis of jury composition''s role in counsel effectiveness',
    'If readings coexist without logical conflict: Gideon is one of three stable doctrinal positions. If Crawford''s confrontation reading forecloses Gideon''s gate-keeper model: the readings are in latent contradiction. If jury cross-section reading pressures both: Gideon''s effectiveness depends on jury legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Doctrinal relationships among three Sixth Amendment readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sixth_amendment__gideon_counsel_revolution, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gideon_theater_1963, sixth_amendment__gideon_counsel_revolution, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gideon_theater_1983, sixth_amendment__gideon_counsel_revolution, theater_ratio, 20, 0.38).
narrative_ontology:measurement(gideon_theater_2003, sixth_amendment__gideon_counsel_revolution, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(gideon_extract_1963, sixth_amendment__gideon_counsel_revolution, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(gideon_extract_1983, sixth_amendment__gideon_counsel_revolution, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(gideon_extract_2003, sixth_amendment__gideon_counsel_revolution, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(gideon_suppress_1963, sixth_amendment__gideon_counsel_revolution, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(gideon_suppress_1983, sixth_amendment__gideon_counsel_revolution, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(gideon_suppress_2003, sixth_amendment__gideon_counsel_revolution, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sixth_amendment__gideon_counsel_revolution, enforcement_mechanism).
narrative_ontology:affects_constraint(sixth_amendment__gideon_counsel_revolution, confrontation_crawford_reading).
narrative_ontology:affects_constraint(sixth_amendment__gideon_counsel_revolution, jury_cross_section_reading).
narrative_ontology:affects_constraint(sixth_amendment__gideon_counsel_revolution, strickland_ineffective_assistance_standard).
narrative_ontology:affects_constraint(sixth_amendment__gideon_counsel_revolution, public_defender_underfunding_trap).

% DUAL FORMULATION NOTE:
% Gideon is the gateway reading that creates structural pressure on both Crawford's confrontation reading and the jury cross-section reading. Changes to how counsel is appointed, resourced, or trained cascade downstream to both. Additionally, Gideon's extractiveness accumulation has created a downstreamconstraint: the Strickland ineffective-assistance standard, which now functions as a gating mechanism that filters out systemic underfunding from constitutional cognizance. The public defender underfunding trap is structurally downstream of Gideon's implementation failure — it would not exist in the same form if Gideon had been fully resourced post-1963.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sixth_amendment__gideon_counsel_revolution, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
