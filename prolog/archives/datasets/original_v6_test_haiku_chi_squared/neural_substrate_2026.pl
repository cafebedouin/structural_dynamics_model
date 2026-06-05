% ============================================================================
% CONSTRAINT STORY: neural_substrate_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_neural_substrate_2026, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: neural_substrate_2026
 *   human_readable: Active Manipulation of Consciousness via tFUS
 *   domain: technological/biological/neuroethics
 *
 * SUMMARY:
 *   Transcranial focused ultrasound (tFUS) represents a qualitative shift in
 *   neuroscience: the field has transitioned from correlation (measuring
 *   brain activity via fMRI/EEG) to active causation (directly modifying
 *   neural substrate and observing behavioral/cognitive consequences). This
 *   capability creates a structural tension between the scientific value of
 *   causal manipulation and the ethical requirement to protect cognitive
 *   autonomy. The constraint exhibits classic Tangled Rope structure: genuine
 *   coordination function (standardized methods, shared data, scientific
 *   advance) combined with asymmetric extraction (beneficiaries capture IP
 *   and funding while subjects bear unknown risks and cognitive autonomy
 *   degrades as a collective good). Theater ratio (0.55) reflects moderate
 *   performativity in ethics review: IRBs assess informed consent and risk
 *   disclosure but lack technical capacity to verify what causal effects
 *   actually occur in subjects' cognition, monitor long-term changes, or
 *   audit industry applications. The constraint's extractiveness has
 *   increased over the 6-year interval (0.35 → 0.58) as the capability
 *   shifted from experimental proof-of-concept to deployable technology,
 *   expanding the population exposed to manipulation and creating commercial
 *   incentives to minimize consent friction.
 *
 * KEY AGENTS:
 *   - Research Subjects: Primary victims (powerless/trapped) — cannot verify what tFUS does to their cognition; face information asymmetry and exit barriers
 *   - Neurotechnology Researchers: Secondary beneficiaries (moderate/constrained) — gain career and funding advantage from tFUS adoption; constrained by competitive pressure to use the technology
 *   - Neurotechnology Companies: Primary beneficiaries (institutional/arbitrage) — capture IP, licensing revenue, and market position through first-mover advantage
 *   - Military/Defense Agencies: Structural beneficiaries (institutional/arbitrage) — seek causal access to adversary cognition for non-consensual modification capabilities
 *   - Cognitive Autonomy Commons: Primary victim (organized/constrained) — collective good with no enforcement mechanism; degrades as tFUS capability spreads
 *   - IRB/Ethics Review Institutions: Gatekeeper (institutional/constrained) — maintain performative review ritual; lack technical capacity to verify actual causal effects
 *   - Analytical Observer: Civilization-level perspective (analytical/analytical) — sees the structural gap between technological capability and ethical governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(neural_substrate_2026, 0.58).
domain_priors:suppression_score(neural_substrate_2026, 0.68).
domain_priors:theater_ratio(neural_substrate_2026, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(neural_substrate_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(neural_substrate_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(neural_substrate_2026, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(neural_substrate_2026, tangled_rope).
narrative_ontology:human_readable(neural_substrate_2026, "Active Manipulation of Consciousness via tFUS").
narrative_ontology:topic_domain(neural_substrate_2026, "technological/biological/neuroethics").

domain_priors:requires_active_enforcement(neural_substrate_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(neural_substrate_2026, neurotechnology_researchers).
narrative_ontology:constraint_beneficiary(neural_substrate_2026, commercial_neurotech_firms).
narrative_ontology:constraint_beneficiary(neural_substrate_2026, military_defense_agencies).
narrative_ontology:constraint_victim(neural_substrate_2026, research_subjects).
narrative_ontology:constraint_victim(neural_substrate_2026, cognitive_autonomy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESEARCH SUBJECT (SNARE) — Consents to participation under information asymmetry: subjects cannot verify what tFUS actually does to their cognition, cannot opt out mid-session without penalty, face career/educational consequences for withdrawal. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.74. High extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(neural_substrate_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NEUROSCIENCE RESEARCH COMMUNITY (TANGLED ROPE) — Constrained by funding competition and the need to adopt tFUS to remain competitive in the field. Also benefits from shared methods, data, and the genuine scientific advance that causal manipulation enables. d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.55. Hybrid: coordination through method standardization + extraction through funding concentration.
constraint_indexing:constraint_classification(neural_substrate_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NEUROTECHNOLOGY COMPANIES (ROPE) — Capture IP, funding, and market position through first-mover advantage in causal consciousness manipulation. Experience constraint as pure coordination: ability to modify cognition is a commodity they can monetize and license. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary via arbitrage.
constraint_indexing:constraint_classification(neural_substrate_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COGNITIVE AUTONOMY COMMONS (SNARE) — The ability to think without external causal manipulation is a collective good with no enforcement mechanism. As tFUS capability spreads, the commons degrades: no actor can exit or organize to prevent use without consensual framing becoming fiction. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.72. The suppression mechanism is the inability to detect or prevent unauthorized tFUS use.
constraint_indexing:constraint_classification(neural_substrate_2026, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL ETHICS REVIEW (PITON) — IRB oversight of tFUS is largely performative: committees assess informed consent and risk disclosure but cannot verify what causal effects occur, cannot monitor long-term cognitive changes, cannot audit industry applications. The review ritual persists through institutional inertia. theater_ratio=0.55 reflects moderate performativity — some genuine gate-keeping exists but effectiveness is degraded by technical limitations. d≈0.12, f(d)≈0.08, σ=1.0 → χ≈0.04.
constraint_indexing:constraint_classification(neural_substrate_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational view, tFUS represents a structural transition in human technological power: first time humanity has causal access to the substrate of conscious experience itself. This is neither pure law (not inherent to biology) nor pure coordination (enabling genuine scientific knowledge) nor pure extraction (subjects do consent). The constraint is the gap between causal capability and the ethical/legal framework to govern it. d≈0.70, f(d)≈1.12, σ=1.2 → χ≈0.59. The civilization temporarily lacks the institutions to manage this technology responsibly.
constraint_indexing:constraint_classification(neural_substrate_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(neural_substrate_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(neural_substrate_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(neural_substrate_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(neural_substrate_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(neural_substrate_2026, TR),
    TR >= 0.70.

:- end_tests(neural_substrate_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. tFUS enables direct causal modification of conscious experience — the highest-level extraction possible from a biological substrate. However, extraction is not at Snare-level (≥0.66) because: (1) subjects do nominally consent, (2) some genuine scientific coordination exists, (3) the technology is still in research phase with limited deployment. Extractiveness increases over the interval as capability matures and commercialization pressure grows. Suppression (0.68): High. Multiple suppression mechanisms: (a) information asymmetry — subjects cannot verify what tFUS does; (b) exit barriers — no meaningful way to refuse ongoing manipulation once enrolled; (c) institutional blindness — ethics review cannot audit actual causal effects; (d) unknown long-term consequences; (e) potential military non-consensual application. Theater ratio (0.55): Moderate. Ethics review and informed consent rituals are performed, but their effectiveness is degraded by technical limitations (reviewers cannot verify what actually happens in subjects' cognition). The performance is not theatrical enough for Piton (which requires ≥0.70) but substantial enough to flag the performativity issue.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiaries (neurotechnology companies, defense agencies) experience tFUS as pure coordination and arbitrage opportunity: they are solving a technical problem and monetizing the solution. The research community experiences it as mixed coordination-extraction: they benefit from standardized methods but are constrained by funding competition. Research subjects and cognitive autonomy as a collective good experience it as extraction with suppression: they cannot verify what is happening, cannot exit, and bear unknown risks. The ethics review system sees it as a manageable oversight problem (Piton) — performing institutional gatekeeping rituals that they believe are functional but which are actually degraded by technical incapacity. The civilizational observer sees the constraint as a real structural gap between capability and governance — neither pure law nor pure coordination nor pure extraction, but a hybrid that is evolving toward greater extraction as the technology matures. The perspectival gap is maximal: beneficiaries see Rope, research community sees Tangled Rope, victims see Snare, ethics institutions see Piton, civilization sees Tangled Rope with uncertain trajectory.
 *
 * DIRECTIONALITY LOGIC:
 *   Research subjects: victims + trapped → d≈0.93, f(d)≈1.40. Maximum extraction. Cannot exit, cannot verify harm, bear full risk of unknown cognitive modification. Neurotechnology researchers: moderate beneficiaries + constrained → d≈0.65, f(d)≈0.95. Moderate extraction. Benefit from career/funding advantage but constrained by competitive adoption pressure and institutional risk. Neurotechnology companies: primary beneficiaries + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiaries. Can exit by licensing/selling capability; capture monopoly rents. Defense agencies: institutional beneficiaries + arbitrage → d≈0.05, f(d)≈-0.12. Institutional beneficiaries seeking weapon capability. Cognitive autonomy commons: organized victim + constrained → d≈0.92, f(d)≈1.38. Cannot defend itself; extracted by default as capability spreads. Ethics institutions: institutional gatekeeper + constrained → d≈0.12, f(d)≈0.08. Low effective extraction because they perceive gatekeeping function (Piton classification comes from theater gate, not high chi). Analytical observer: d≈0.70, f(d)≈1.12. Moderate-high extraction from civilization's perspective; the technology is advancing without adequate ethical governance.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint RESOLVES mandatrophy via three mechanisms: (1) STRUCTURAL BENEFICIARY/VICTIM IDENTIFICATION: Clearly identifies beneficiaries (neurotechnology companies, researchers, military) and victims (subjects, cognitive autonomy commons). The beneficiary group has high power and exit options; the victim group has low power and no exit. This differentiation prevents misclassifying pure extraction as coordination. (2) SUPPRESSION ASYMMETRY: High suppression (0.68) combined with clear victim identification prevents Rope classification — subjects cannot simply coordinate out of the constraint because they lack information and exit options. (3) TEMPORAL EVOLUTION: Extractiveness increasing over the interval (0.35 → 0.58) as technology matures shows the constraint evolving toward greater Snare characteristics. This prevents static misclassification: the system was lower-extraction during proof-of-concept phase but is trending toward Snare as deployment accelerates. The constraint is genuinely Tangled Rope NOW (because some coordination exists, some consent occurs) but has clear trajectory toward Snare if military/commercial non-consensual applications materialize. The mandatrophy is resolved by tracking structural evolution and distinguishing between current classification (Tangled Rope) and trajectory (toward Snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_of_detectable_modification,
    'At what magnitude of tFUS stimulation does a subject reliably detect that their cognition has been modified, vs. when modification occurs below conscious awareness?',
    'Blinded subject self-reports at varying stimulation intensities; correlation between reported perception and objective neural markers; comparison with sham controls',
    'If detection threshold is low: subjects can refuse ongoing manipulation (exit improves). If high: subjects cannot know they are being modified (suppression is severe; classification moves toward pure Snare). This directly determines whether informed consent is achievable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_of_detectable_modification, empirical, 'Detection threshold for tFUS-induced cognitive modification').

omega_variable(
    persistence_of_tfus_effects,
    'Do tFUS-induced cognitive changes persist after stimulation ceases? Over what timescale do they decay or consolidate into long-term modification?',
    'Longitudinal cognitive testing; neuroimaging at 1 hour, 1 day, 1 week, 1 month post-stimulation; correlation with reported subjective changes',
    'If effects are transient (< 1 hour): constraint is lower-extraction, subjects recover autonomy. If persistent or consolidating: extraction is severe, the manipulation may be permanent, and suppression classification rises. This determines whether the constraint is a temporary intervention or an irreversible modification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_of_tfus_effects, empirical, 'Persistence timeline of tFUS-induced cognitive changes').

omega_variable(
    off_target_effects_and_detectability,
    'How frequently does tFUS applied to one neural region affect unintended nearby structures? Can subjects or experimenters detect these off-target effects?',
    'High-resolution fMRI/DTI during stimulation; post-hoc lesion analysis if available; audits of manufacturer calibration specs vs. actual focal patterns',
    'If off-target effects are frequent and undetectable: subjects are bearing unknown risks (suppression is severe, classification is Snare). If rare or detectable: informed consent becomes more meaningful. This determines whether the constraint includes hidden harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(off_target_effects_and_detectability, empirical, 'Frequency and detectability of off-target tFUS effects').

omega_variable(
    military_non_consensual_application,
    'Are military/defense agencies exploring tFUS capability for non-consensual modification of adversary cognition (e.g., remote impairment of decision-making, induction of distress)?',
    'FOIA requests to US DoD and equivalent foreign agencies; analysis of classified literature and defense-industry contracts; investigation of dual-use research pathways',
    'If non-consensual military application is being pursued: the constraint transitions from extractive research (Snare/Tangled Rope) to weaponization (Snare with total suppression). The cognitive autonomy commons would face direct threat. Classification becomes pure Snare from all non-military perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(military_non_consensual_application, empirical, 'Whether non-consensual military applications of tFUS are being developed').

omega_variable(
    reversibility_and_consent_withdrawal,
    'If a subject withdraws consent mid-study, can experimenters reverse any tFUS-induced cognitive changes? What are the actual procedures and success rates?',
    'Review of research protocols for reversal procedures; interview with tFUS researchers about feasibility; test cases where subjects requested reversal',
    'If reversal is reliable: subjects have meaningful exit (exit_options improve from ''trapped'' to ''constrained''). If reversal is impossible or unknown: suppression is near-total and the classification is pure Snare. This determines whether the constraint allows genuine autonomy recovery.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reversibility_and_consent_withdrawal, empirical, 'Reversibility of tFUS-induced cognitive changes and consent-withdrawal procedures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(neural_substrate_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(neus_tr_t0, neural_substrate_2026, theater_ratio, 0, 0.3).
narrative_ontology:measurement(neus_tr_t3, neural_substrate_2026, theater_ratio, 3, 0.43).
narrative_ontology:measurement(neus_tr_t6, neural_substrate_2026, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(neus_be_t0, neural_substrate_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(neus_be_t3, neural_substrate_2026, base_extractiveness, 3, 0.47).
narrative_ontology:measurement(neus_be_t6, neural_substrate_2026, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(neural_substrate_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(neural_substrate_2026, brain_computer_interface_extraction).
narrative_ontology:affects_constraint(neural_substrate_2026, neurocognitive_surveillance_commons).
narrative_ontology:affects_constraint(neural_substrate_2026, informed_consent_fiction).

% DUAL FORMULATION NOTE:
% tFUS active manipulation is downstream of neuroscience's capability transition from correlation to causation. The upstream constraint (correlation_causation_boundary, ε≈0.12, Mountain) represents the immutable fact that causal knowledge requires intervention. This story (neural_substrate_2026, ε=0.58, Tangled Rope) focuses on the social-technical constraint created by deploying that causal capability without adequate governance. A third downstream story (military_cognitive_weaponization, ε≈0.75, Snare) would model non-consensual application. All three are linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(neural_substrate_2026, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
