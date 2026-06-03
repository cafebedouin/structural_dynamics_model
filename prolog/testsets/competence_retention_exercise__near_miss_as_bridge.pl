% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__near_miss_as_bridge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__near_miss_as_bridge, []).

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
 *   constraint_id: competence_retention_exercise__near_miss_as_bridge
 *   human_readable: Near-Miss as Competence Validation Bridge
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   The competence retention problem in high-reliability organizations
 *   (aviation, nuclear operations, emergency response) creates a structural
 *   tension between the need to maintain catastrophe-avoidance skills and the
 *   impossibility of practicing on actual catastrophes. Three readings of the
 *   competence-maintenance kernel contest how this tension should be
 *   resolved. This story instantiates the 'near-miss as bridge' reading:
 *   near-miss incidents and minor failures provide sufficient real-world
 *   feedback to validate and update simulator training without requiring full
 *   catastrophes. The near-miss reading claims that a hybrid system
 *   (high-fidelity simulation for routine skill preservation plus active
 *   near-miss investigation and integration into training systems) is both
 *   necessary and sufficient for maintaining competence. Catastrophes are
 *   neither necessary (because near-miss feedback is richer than catastrophe
 *   data) nor sufficient (because learning from catastrophe destroys the
 *   system that was learning). The reading coexists with
 *   catastrophe_as_necessary (which claims only visceral stakes enable real
 *   learning) and simulation_as_sufficient (which claims cognitive and
 *   procedural demands of high-fidelity simulation are structurally
 *   equivalent to real events). The constraint exhibits tangled rope
 *   structure: simulator program authorities genuinely benefit from near-miss
 *   integration (validation for training investment), while front-line
 *   operators bear extraction (involuntary participation in incident
 *   investigation with career consequences). The system coordinates improved
 *   training design while extracting from operators. Theater ratio rising
 *   from 0.35 to 0.55 reflects regulatory documentation requirements
 *   (incident reports, compliance checklists) increasingly decoupling from
 *   actual simulator updates.
 *
 * KEY AGENTS:
 *   - Front-Line Operators: Primary victim (powerless/trapped) — involuntary participation in both simulator validation and near-miss incident investigation; no exit from dual-validation requirement
 *   - Simulator Program Authority: Primary beneficiary (institutional/arbitrage) — uses near-miss data to justify simulator investment ROI and expansion; can exit to alternative training methodologies if simulator effectiveness questioned
 *   - Safety Culture Advocates: Secondary actor (moderate/constrained) — benefit from near-miss investigation feedback but constrained by organizational resistance to incident disclosure and liability exposure
 *   - Regulatory Compliance Framework: Institutional actor (institutional/arbitrage) — maintains documentary requirement for near-miss integration but decoupled from operational improvement (piton perspective)
 *   - Independent Safety Standards Body: Organized actor (organized/constrained) — constrained by liability cascade; dual-validation approach distributes accountability across stakeholders
 *   - Predictive Analytics Coalition: Organized agent (organized/mobile) — sees near-miss integration as temporary bridge toward predictive systems that eliminate need for incident investigation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contested epistemic claim (tacit knowledge requires real stakes) as immutable natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__near_miss_as_bridge, 0.38).
domain_priors:suppression_score(competence_retention_exercise__near_miss_as_bridge, 0.48).
domain_priors:theater_ratio(competence_retention_exercise__near_miss_as_bridge, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, extractiveness, 0.38).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__near_miss_as_bridge, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__near_miss_as_bridge, "Near-Miss as Competence Validation Bridge").
narrative_ontology:topic_domain(competence_retention_exercise__near_miss_as_bridge, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_retention_exercise__near_miss_as_bridge).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__near_miss_as_bridge, 'cd006456-53ae-4886-abf8-053d7e84f5ff').
narrative_ontology:cs_kernel_codification('cd006456-53ae-4886-abf8-053d7e84f5ff', distributed).
narrative_ontology:cs_authority_grounding('cd006456-53ae-4886-abf8-053d7e84f5ff', extraction).
narrative_ontology:cs_interpretation_layer_present('cd006456-53ae-4886-abf8-053d7e84f5ff').
narrative_ontology:cs_reading_relation('cd006456-53ae-4886-abf8-053d7e84f5ff', competence_retention_exercise__catastrophe_as_necessary, coexists_with).
narrative_ontology:cs_reading_relation('cd006456-53ae-4886-abf8-053d7e84f5ff', competence_retention_exercise__simulation_as_sufficient, influences).
narrative_ontology:cs_axiom('cd006456-53ae-4886-abf8-053d7e84f5ff', foundational, hybrid_system_necessity).
narrative_ontology:cs_axiom_status(hybrid_system_necessity, holdable).
narrative_ontology:cs_axiom_grounding('cd006456-53ae-4886-abf8-053d7e84f5ff', hybrid_system_necessity, empirically_contingent).
narrative_ontology:cs_axiom('cd006456-53ae-4886-abf8-053d7e84f5ff', foundational, catastrophe_neither_necessary_nor_sufficient).
narrative_ontology:cs_axiom_status(catastrophe_neither_necessary_nor_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('cd006456-53ae-4886-abf8-053d7e84f5ff', catastrophe_neither_necessary_nor_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('cd006456-53ae-4886-abf8-053d7e84f5ff', hybrid_validation_framework).
narrative_ontology:cs_drift_state('cd006456-53ae-4886-abf8-053d7e84f5ff', contemporary_regulatory_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cd006456-53ae-4886-abf8-053d7e84f5ff', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, simulator_fidelity_programs).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, incident_investigation_infrastructure).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, front_line_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, resource_allocation_in_prevention).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONT-LINE OPERATOR (SNARE) — Trapped in dual-validation requirement: must demonstrate competence in both simulator and real-world near-miss response, with career consequences for failures in either context. Simulator training is mandatory; near-miss participation is involuntary incident investigation. No exit option from the integrated system; trapped within both modalities. Experiences extraction without coordination benefit — the system exists to validate institutional competence claims, not to serve the operator's learning.
constraint_indexing:constraint_classification(competence_retention_exercise__near_miss_as_bridge, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SAFETY CULTURE ADVOCATE (TANGLED ROPE) — Constrained by institutional resistance to near-miss disclosure (organizational shame, legal liability), but also genuinely benefits from the hybrid system: near-miss investigations improve simulator design and reveal real-world gaps. Mixed experience: coordination benefit (improved training) alongside extraction (career risk when investigations expose institutional failures). Constrained exit — can leave the organization but cannot fully exit the constraint while remaining in safety-critical industry.
constraint_indexing:constraint_classification(competence_retention_exercise__near_miss_as_bridge, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SIMULATOR PROGRAM AUTHORITY (ROPE) — Primary beneficiary with arbitrage exit. Benefits from near-miss integration: each incident becomes validation data for simulator investment ROI, justifying budget and expansion. Experiences the constraint as pure coordination: using real-world failures to improve training systems. Exit at arbitrage level — can shift to different training methodologies or redirect funding if simulator effectiveness is questioned. Net beneficiary.
constraint_indexing:constraint_classification(competence_retention_exercise__near_miss_as_bridge, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PREDICTIVE ANALYTICS COALITION (SCAFFOLD) — Organized agents (data scientists, ML practitioners, regulatory bodies) see near-miss classification and pattern extraction as temporary problem: advanced predictive systems will identify failure modes before near-misses occur, enabling transition to purely preventive training focused on rare-event signatures. Low extraction because the coalition has agency and sees a clear sunset: as predictive tools mature, near-miss incident investigation shifts from reactive to proactive, and the dual-validation constraint dissolves into pure simulation-plus-monitoring.
constraint_indexing:constraint_classification(competence_retention_exercise__near_miss_as_bridge, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY COMPLIANCE FRAMEWORK (PITON) — The documented requirement for 'real-world validation of training efficacy' persists largely as institutional theater: regulators require certification that near-miss data is incorporated into training, but the actual feedback loop is often decoupled from operational improvements. Theater ratio high because compliance documentation (checklist completion, incident report filing) does not necessarily translate to simulator update or procedural change. The requirement maintains itself through regulatory inertia — removing it would invite liability questions, so the ritual persists.
constraint_indexing:constraint_classification(competence_retention_exercise__near_miss_as_bridge, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: INDEPENDENT SAFETY STANDARDS BODY (TANGLED ROPE) — Organized but structurally constrained by liability cascade (if they endorse simulation-only training and a near-miss escalates, they face institutional liability). The hybrid approach (simulation + near-miss validation) distributes liability across multiple stakeholders. Benefits from the constraint: dual validation provides justification for rigorous standards. Constrained by institutional liability exposure — cannot fully exit to pure simulation without absorbing risk that member organizations will shed. Mixed coordination (rigorous standards setting) and extraction (liability protection via distributed accountability).
constraint_indexing:constraint_classification(competence_retention_exercise__near_miss_as_bridge, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, human competence maintenance in catastrophe-avoidance domains has an irreducible requirement: simulation cannot fully substitute for real-world feedback because tacit knowledge and embodied responses only update when predictions meet reality with stakes. The constraint appears as an immutable natural law: any system that removes catastrophic feedback entirely risks atrophy. However, the structural data contradicts the mountain classification — this is a false summit, naturalizing what is actually a contestable institutional and epistemic arrangement around how competence is validated and maintained.
constraint_indexing:constraint_classification(competence_retention_exercise__near_miss_as_bridge, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__near_miss_as_bridge_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(competence_retention_exercise__near_miss_as_bridge, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(competence_retention_exercise__near_miss_as_bridge, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(competence_retention_exercise__near_miss_as_bridge, TR),
    TR >= 0.70.

:- end_tests(competence_retention_exercise__near_miss_as_bridge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The near-miss bridge mechanism extracts from operators through mandatory incident investigation participation and career risk exposure, but the extraction is not total because the system does generate genuine training improvements. The measurement trajectory (0.22 → 0.38) reflects increasing extraction as regulatory documentation requirements expand without corresponding operational improvement — rising theater ratio indicates that compliance activities are decoupling from actual simulator updates. Suppression (0.48): Moderate-high. Operators face organizational barriers to incident disclosure (shame, liability concerns, career risk), tacit knowledge requirements that resist translation into explicit training modules, and structural barriers to exiting the validation requirement. However, suppression is not total — some organizations have strong near-miss reporting cultures, and operators can exit through career change. Theater ratio (0.55): High-moderate. The regulatory requirement to 'incorporate near-miss data into training' has increasingly become documentary theater: incident report filing, compliance checklists, and formalized review processes that do not necessarily translate to simulator updates or procedural changes. The rise from 0.35 to 0.55 reflects this decoupling.
 *
 * PERSPECTIVAL GAP:
 *   The near-miss reading exhibits strong perspectival gaps across power axes. The simulator authority sees pure coordination (rope) — using real-world incidents to improve training. The predictive analytics coalition sees a temporary problem with a sunset (scaffold) — predictive systems will eliminate need for incident-based feedback. The regulatory framework sees its own performative ritual (piton) — compliance documentation without operational consequence. Front-line operators see pure extraction (snare) — mandatory participation without agency or benefit. Safety advocates see mixed coordination and extraction (tangled rope, this reading) — genuine training improvement coupled with extraction through liability exposure. The civilizational analytical observer risks seeing immutable natural law (mountain) — tacit knowledge in catastrophe-avoidance requires real stakes — but the structural data reveals this as naturalization of a contested epistemic claim. The perspectival gaps are not measurement artifacts but genuine differences in how the constraint functions across different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from structural position: beneficiary status, power level, and exit options. Simulator program authorities (institutional/arbitrage) experience low effective extraction because they benefit from the system and can exit to alternative training methods — their d value is low (approximately 0.15), producing negative χ (they are subsidized). Front-line operators (powerless/trapped) experience maximum effective extraction because they bear costs and have no exit — their d value is high (approximately 0.92), producing high χ (approximately 1.28). Safety culture advocates (moderate/constrained) occupy mixed position: benefit from training improvements but constrained by liability and organizational resistance — their d value is moderate (approximately 0.55), producing moderate χ (approximately 0.75). The organized agents (safety standards body, analytics coalition) have intermediate d values reflecting their agency within constraints. The mountain perspective's d value (analytical/0.72) is canonical for that power atom.
 *
 * MANDATROPHY ANALYSIS:
 *   COMMITTER FRAME DIAGNOSTIC: This constraint resolves mandatrophy by instantiating one specific reading of a contested kernel. The mandatrophy is not 'which reading is correct?' but 'which epistemic commitments and empirical assumptions define each reading?' The near-miss reading assumes: (1) tacit knowledge can transfer from real-world incidents to simulator training at sufficient fidelity; (2) near-miss incidents provide learning signal without system destruction; (3) hybrid approach is superior to pure simulation because of fidelity gains and to catastrophe-dependence because of sustainability. The catastrophe_as_necessary reading assumes: (1) only visceral stakes (threat of catastrophic failure) enable genuine competence maintenance; (2) simulation is rehearsal but not real exercise; (3) catastrophes are necessary for knowledge update. The simulation_as_sufficient reading assumes: (1) cognitive and procedural demands are structurally equivalent; (2) fidelity is achievable; (3) real-world incidents add no information gain beyond simulation design feedback. These are not empirically decidable from a single observation point — different studies with different outcome measures will support different readings. The engine's omega variables document the empirical and conceptual ambiguities that prevent universal adjudication.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    near_miss_fidelity_threshold,
    'What magnitude and type of near-miss incident provides sufficient feedback for simulator validation, and what incidents are too minor to update training?',
    'Comparative analysis of simulator updates triggered by different incident classes; longitudinal tracking of operator performance on subsequent near-misses after simulator updates; identification of incident severity thresholds where feedback loops activate vs. deactivate',
    'If threshold is low: routine incidents drive constant simulator changes, creating theater (documentation churn without performance gain). If threshold is high: many preventable failure modes are not captured until they escalate to catastrophe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_miss_fidelity_threshold, empirical, 'Minimum incident severity required for simulator training feedback').

omega_variable(
    simulation_fidelity_saturation,
    'Can simulator fidelity ever reach a point where real-world near-miss data provides zero additional improvement signal?',
    'Measurement of marginal simulator updates per incident as function of simulator age and sophistication; correlation between incident frequency and update frequency; identification of constraints where learning curve plateaus',
    'If saturation is reachable: the constraint can transition to pure simulation (simulation_as_sufficient reading becomes valid). If saturation is theoretically infinite: near-miss validation will always be required (this reading''s hybrid approach is permanent, not transitional).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_saturation, empirical, 'Whether simulator fidelity can reach asymptotic sufficiency').

omega_variable(
    tacit_knowledge_capture_mechanism,
    'Are the critical competencies in catastrophe-avoidance domains (emergency response, crisis decision-making, system failure diagnosis) primarily epistemic (learnable via simulation) or ontologically dependent on real-world stakes and embodied response histories?',
    'Comparison of operator performance on simulator scenarios vs. real-world near-misses; analysis of reported decision-making process for identical failure signatures in simulation vs. reality; identification of classes of competence that transfer vs. degrade without real-world validation',
    'If primarily epistemic: simulation_as_sufficient reading becomes structurally viable (near-miss data is coordination layer, not core competence requirement). If ontologically dependent: this reading is correct — hybrid approach is not temporary bridge but permanent necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tacit_knowledge_capture_mechanism, conceptual, 'Whether critical competence is purely epistemic or stakes-dependent').

omega_variable(
    catastrophe_necessity_vs_sufficiency,
    'Does the catastrophe_as_necessary reading conflate necessity with sufficiency? Can catastrophes be necessary for certain competence validation yet insufficient for competence maintenance (since repeated catastrophe-scale events destroy systems)?',
    'Theoretical analysis of high-reliability organization literature; examination of whether organizations cite catastrophes as training events vs. system failures; comparison of competence decay rates in systems that have experienced catastrophe vs. systems maintained by near-miss feedback',
    'If sufficiency is not claimed: catastrophe_as_necessary is about information gain, not training methodology; this reading forecloses a different claim. If sufficiency is claimed: the readings coexist in conceptual space with different implications for training resource allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_necessity_vs_sufficiency, conceptual, 'Logical distinction between catastrophe necessity and sufficiency in competence validation').

omega_variable(
    reading_kernel_identity,
    'What is the kernel that these three readings contest? Is it ''what constitutes valid competence maintenance'' or ''what event types provide learning signal'' or ''what is the proper role of catastrophic vs routine experience''?',
    'Mapping of each reading''s core commitment to common referent. Identification of whether readings differ on empirical facts (simulation fidelity can reach threshold X) or normative claims (competence requires Y type of experience) or both.',
    'If empirical disagreement: measurement and observation can resolve. If normative disagreement: readings may be permanently coexistent (committer frame valid). If kernel identity is disputed: the three readings may not be readings of the same constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_identity, conceptual, 'Identity of the contested kernel across the three readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__near_miss_as_bridge, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cre_nmb_tr_t0, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cre_nmb_tr_t5, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 5, 0.48).
narrative_ontology:measurement(cre_nmb_tr_t10, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(cre_nmb_be_t0, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cre_nmb_be_t5, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(cre_nmb_be_t10, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(cre_nmb_su_t0, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cre_nmb_su_t5, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 5, 0.46).
narrative_ontology:measurement(cre_nmb_su_t10, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__near_miss_as_bridge, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise__catastrophe_as_necessary).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise__simulation_as_sufficient).

% DUAL FORMULATION NOTE:
% The competence_retention_exercise kernel has three structurally distinct readings with different ε values, different beneficiary/victim structures, and different terminal states. Each reading is instantiated in a separate constraint story: catastrophe_as_necessary (ε≈0.65, snare-dominant), near_miss_as_bridge (ε=0.38, tangled_rope), simulation_as_sufficient (ε≈0.15, rope). The reading-specific stories are linked via network.affects_constraints to enable contamination analysis and committer frame reasoning. Each story instantiates one consistent reading; no single story attempts to adjudicate between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_retention_exercise__near_miss_as_bridge, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
