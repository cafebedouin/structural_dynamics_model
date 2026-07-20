% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
 *   human_readable: Dual Practice Equilibrium: Domain-Partitioned Legitimacy
 *   domain: political_history/modernization/institutional_change
 *
 * SUMMARY:
 *   The domain-partitioned dual practice equilibrium is a recurring
 *   institutional solution in modernization contexts where state-building
 *   projects encounter entrenched traditional authority. Rather than claiming
 *   total supremacy or surrendering to customary law, the modernizing state
 *   accepts a permanent bifurcation: Gregorian calendars, Western dress, and
 *   bureaucratic procedure govern the public/administrative sphere, while
 *   lunar calendars, traditional dress, and ritual norms govern
 *   private/festival/agricultural life. This constraint is one reading of the
 *   contested kernel 'legitimacy_of_practice_standardization' â
 *   specifically the dual_practice_equilibrium_reading, which posits that
 *   legitimacy is structurally domain-specific and that no convergence is
 *   expected. The structural delta from sibling readings is clear: unlike
 *   endogenous displacement (change through voluntary adoption) or exogenous
 *   override (state decree across all domains), this reading encodes a
 *   stable, strategic compliance pattern where practitioners context-switch
 *   but do not internalize both systems as naturally unified.
 *
 * KEY AGENTS:
 *   - state_bureaucracy: Primary beneficiary/agenda-setter (institutional/constrained) â secures public domain legitimacy without fighting total wars over ritual practice
 *   - traditional_authority: Secondary beneficiary/agenda-setter (organized/constrained) â retains private domain jurisdiction in exchange for conceding public supremacy
 *   - ordinary_practitioners: Primary payer (powerless/constrained) â bears material and cognitive costs of bifurcated compliance
 *   - totalizing_modernizers: Excluded voice (organized/constrained) â demands uniform state standardization and is treated as destabilizing
 *   - comparative_historian: Analytical observer (analytical/analytical) â documents the pattern across societies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.48).
domain_priors:suppression_score(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.42).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "Dual Practice Equilibrium: Domain-Partitioned Legitimacy").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "political_history/modernization/institutional_change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '868b279e-69f0-4319-bb6f-3d29e6ec4066').
narrative_ontology:cs_kernel_codification('868b279e-69f0-4319-bb6f-3d29e6ec4066', formalized).
narrative_ontology:cs_authority_grounding('868b279e-69f0-4319-bb6f-3d29e6ec4066', distributed).
narrative_ontology:cs_reading_relation('868b279e-69f0-4319-bb6f-3d29e6ec4066', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('868b279e-69f0-4319-bb6f-3d29e6ec4066', legitimacy_of_practice_standardization__exogenous_override_reading, influences).
narrative_ontology:cs_axiom('868b279e-69f0-4319-bb6f-3d29e6ec4066', foundational, practice_legitimacy_domain_specific).
narrative_ontology:cs_axiom_status(practice_legitimacy_domain_specific, holdable).
narrative_ontology:cs_axiom_grounding('868b279e-69f0-4319-bb6f-3d29e6ec4066', practice_legitimacy_domain_specific, conventional).
narrative_ontology:cs_axiom('868b279e-69f0-4319-bb6f-3d29e6ec4066', foundational, dual_authority_non_convergence).
narrative_ontology:cs_axiom_status(dual_authority_non_convergence, holdable).
narrative_ontology:cs_axiom_grounding('868b279e-69f0-4319-bb6f-3d29e6ec4066', dual_authority_non_convergence, conventional).
narrative_ontology:cs_reference_frame('868b279e-69f0-4319-bb6f-3d29e6ec4066', bifurcated_authority_framework).
narrative_ontology:cs_drift_state('868b279e-69f0-4319-bb6f-3d29e6ec4066', modernizing_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('868b279e-69f0-4319-bb6f-3d29e6ec4066', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_bureaucracy).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authority).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, ordinary_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces Gregorian calendars, Western dress codes, and administrative procedure in public domains including taxation, courts, and education. Gains legitimate jurisdiction over the public sphere without expending resources to suppress traditional practice in private domains. Cannot easily abandon the partition without risking either rebellion from traditional authorities or loss of legitimacy among modernizing elites who demand total reform.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_bureaucracy, agenda_setter,
    institutional, generational, constrained, national).

% Sets and enforces lunar calendars, customary dress, and ritual norms in private, festival, and agricultural domains. Retains protected jurisdiction over these spheres by conceding public/administrative supremacy to the state. Its exit from the equilibrium is constrained because rejecting state public authority entirely would trigger state suppression, while accepting full state override would dissolve its institutional role.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authority, agenda_setter,
    organized, generational, constrained, regional).

% Wear Western suits and follow Gregorian deadlines for state business; wear kimono or traditional dress and observe lunar festivals for family and village life. Bear the material cost of maintaining dual wardrobes, the cognitive load of dual calendar systems, and the social risk of context-switching errors. Compliance is strategic and performative rather than internalized â they do not experience the two systems as naturally unified.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, ordinary_practitioners, payer,
    powerless, biographical, constrained, local).

% Advocate for abolishing traditional authority in all domains and imposing uniform state standards for calendars, dress, and language. Their position is structurally excluded from the dual-practice equilibrium, which treats their demands as illegitimate destabilization of a necessary compromise. They are not invited to legitimacy negotiations between state and traditional authorities.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, totalizing_modernizers, excluded,
    organized, generational, constrained, national).

% Documents the recurrent pattern of domain-partitioned dual practice across modernization contexts such as Meiji Japan, Ottoman Tanzimat, and colonial India. Observes that the equilibrium is an institutional solution to authority competition rather than a natural or inevitable arrangement, and notes its persistent extraction from practitioner populations.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, comparative_historian, observer,
    analytical, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the zero-sum conflict between modernizing state authority and entrenched traditional authority by permanently partitioning governance domains, allowing both to operate without claiming total supremacy over all practice.
% TRANSFER_FUNCTION: Moves compliance burden and cognitive overhead from state and traditional authorities to ordinary practitioners, who must context-switch between public/administrative and private/ritual norms. Moves legitimacy rents to both authorities by securing their respective domains of control.
% ABSENT_VOICES: Totalizing modernizers who demand uniform state standardization across all domains, and unified traditionalists who reject any state encroachment, are excluded from the legitimacy framework. Their objections are treated as destabilizing rather than as valid alternative readings of legitimacy.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished, state and traditional authorities would compete directly for every practice domain. The likely outcomes are either total state absorption of ritual life, traditionalist rebellion in administrative spheres, or a destructive legitimacy crisis. Ordinary practitioners would lose the strategic clarity of context-dependent compliance and face winner-take-all imposition.
% FOUNDING_PROBLEM: Competing claims to total authority by modernizing states and traditional institutions produce persistent legitimacy crises, rebellions, or governance failures when each tries to impose uniform practice standards across all domains simultaneously.
% FOUNDING_PROBLEM_CORROBORATION: Late-imperial reformers and colonial administrators attest to governance crises of competing authorities from the state side. Ethnographers and traditional leaders attest from the non-state side. Independent comparative political historians and historical sociologists corroborate the recurrent crisis pattern in societies that attempted unified standardization before arriving at partition.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) reflects the substantial but bounded cost of maintaining dual calendars, dual wardrobes, and context-dependent norm systems. It is not extreme because practitioners develop coping mechanisms, but it is significant because neither domain can be neglected without penalty. Suppression (0.42) is moderate: each authority suppresses attempts to import the other's norms into its domain (e.g., wearing traditional dress to a state office, or using the lunar calendar for tax filing), but enforcement is routinized rather than violent. Theater ratio (0.35) captures the strategic, non-internalized compliance â practitioners perform the correct norms for each domain without necessarily believing in their inherent legitimacy, producing a performative layer. Accessibility collapse (0.50) is moderate because alternatives (unified practice) exist as ideas but collapse under cross-domain social and bureaucratic sanctions. Resistance (0.38) is moderate: totalizing modernizers and traditionalists resist the partition, but the equilibrium dampens open conflict by giving each authority a protected sphere.
 *
 * PERSPECTIVAL GAP:
 *   The state bureaucracy seat computes the constraint as a Rope or low-extraction Tangled Rope: it solved a legitimacy crisis, prevented rebellion, and gained public compliance at the cost of tolerating private tradition. The ordinary practitioner seat computes it as a Tangled Rope with higher extraction: they pay the daily cost of the compromise. The comparative observer seat sees the structural asymmetry â the authorities trade domain recognition, but the practitioners pay the transaction costs. The engine derives this divergence from the same structural data: beneficiaries with constrained but institutionally secure exits versus payers with constrained exits and no compensating domain control.
 *
 * DIRECTIONALITY LOGIC:
 *   State bureaucracy and traditional authority are declared beneficiaries; their structural relationship to the constraint is subsidizing (low d) because the constraint secures their respective domains of control without requiring total victory. Ordinary_practitioners are declared victims; their structural relationship is targeting (high d) because the constraint extracts compliance costs and cognitive load from them. Totalizing modernizers are excluded â their exclusion is the enforcement boundary that stabilizes the equilibrium. No directionality override is needed because the beneficiary/victim declarations map cleanly to the power and exit structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents the mislabeling that would occur if one focused only on the coordination function (Rope) or only on the extraction (Snare). The genuine coordination is real: without domain partition, state and traditional authority would fight destructive zero-sum battles over every practice. But the asymmetric extraction is equally real: the 'peace' is paid for by practitioners who must live in two normative worlds. A Snare classification would miss the coordination and overstate the coercion; a Rope classification would naturalize a costly compromise. The Tangled Rope gate requires active enforcement, which is satisfied by both authorities' ongoing suppression of cross-domain norm violation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_permanence,
    'Is the domain-partitioned dual practice a permanent structural equilibrium, or a transient phase that inevitably collapses toward either full state standardization or traditional restoration?',
    'Longitudinal comparative analysis across three or more societies over 100-plus years; if all cases eventually converge to unified practice, the permanence claim is falsified.',
    'If transient, the constraint is a Scaffold rather than a Tangled Rope, and its classification should shift accordingly; if permanent, the extraction is a stable feature of modernity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_permanence, empirical, 'Whether dual-practice equilibrium is permanent or transitional').

omega_variable(
    compliance_internality_ambiguity,
    'Does strategic compliance with dual practice remain purely instrumental over generations, or does prolonged bifurcation produce internalized bicultural identity?',
    'Ethnographic and psychological measurement of identity fusion in populations under dual-practice regimes across multiple generations; detection of cognitive dissonance reduction or identity integration versus persistent strategic framing.',
    'If internalized, the extraction metric overstates the burden and the constraint moves toward Rope; if purely strategic, extraction remains accurately measured as active performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_internality_ambiguity, empirical, 'Whether dual-practice compliance remains strategic or becomes internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(legi_tr_t60, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 60, 0.35).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 40, 0.49).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(legi_be_t60, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 60, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 50, 0.38).
narrative_ontology:measurement(legi_su_t60, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 60, 0.36).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The kernel 'legitimacy_of_practice_standardization' decomposes into three structurally distinct readings: dual_practice_equilibrium (domain partition), endogenous_displacement (voluntary adoption), and exogenous_override (state decree). Each reading instantiates a different constraint with different beneficiary structures and epsilon values. The dual practice reading structurally influences the exogenous override reading by providing an alternative legitimacy framework that resists blanket state override, while coexisting with the endogenous displacement reading as an orthogonal theory of change mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
