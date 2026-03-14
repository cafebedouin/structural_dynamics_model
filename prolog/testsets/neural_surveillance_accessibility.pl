% ============================================================================
% CONSTRAINT STORY: neural_surveillance_accessibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_neural_surveillance_accessibility, []).

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
 *   constraint_id: neural_surveillance_accessibility
 *   human_readable: Neural Surveillance Accessibility: The Asymmetric Transparency Trap
 *   domain: neurotechnology/surveillance/privacy
 *
 * SUMMARY:
 *   Neural surveillance accessibility refers to the infrastructure and norms
 *   that enable comprehensive monitoring of human neural activity through
 *   ubiquitous neurotechnologies — workplace brain-computer interfaces,
 *   wearable neural sensors, ambient IoT devices that infer cognitive and
 *   emotional states, and consumer neurotechnology platforms marketed for
 *   'wellness' and 'productivity.' The constraint is not the technology
 *   itself, but the asymmetric transparency structure it creates: operators
 *   (employers, platforms, state agencies) gain detailed access to
 *   individuals' neural data, while individuals have limited knowledge of
 *   what is being measured, how it is used, and what inferences are made.
 *   This constraint exhibits snare characteristics across multiple
 *   perspectives: high extraction (neural data is harvested for behavioral
 *   prediction, cognitive profiling, compensation adjustment, and influence
 *   targeting), high suppression (exit is materially difficult due to
 *   employment dependency, and epistemically difficult due to the opacity of
 *   neural data use), and growing theater (regulatory frameworks create
 *   appearance of governance while extraction mechanisms proceed). The
 *   extractiveness has increased over a ten-year interval as neural
 *   measurement has become cheaper, more ubiquitous, and integrated into
 *   mandatory workplace and consumer systems.
 *
 * KEY AGENTS:
 *   - Neural Data Subjects: Primary victims (powerless/trapped) — individuals whose neural activity is captured without meaningful knowledge or consent; bears maximum extraction through employment dependency or technological ubiquity
 *   - Employed Knowledge Workers: Secondary victims (moderate/constrained) — face high but surmountable costs to exit; must choose between employment income and neural autonomy
 *   - Surveillance Operators (Employers, Platforms, State Agencies): Primary beneficiaries (institutional/arbitrage) — capture neural data for behavioral prediction, cognitive optimization, and population management; operate the extraction infrastructure
 *   - Data Aggregators and Brokers: Secondary beneficiaries (powerful/arbitrage) — purchase, aggregate, and resell neural profiles; abstract the data from original context to increase monetization
 *   - Privileged Opt-Out Sector: Tertiary actors (powerful/mobile) — can negotiate exemption from neural surveillance through professional standing; experience constraint as coordination mechanism with asymmetric benefit
 *   - Regulatory Bodies: Institutional theater (institutional/arbitrage) — maintain appearance of neural privacy governance through consent requirements and data minimization rules; in practice, enforcement is weak and capture by operators is deep
 *   - Advocacy Coalition: Organized resistance (organized/constrained) — civil liberties organizations and workers unions organizing against neural surveillance; face suppression through data-driven preemption and platform control
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing neural surveillance as inevitable feature of advanced technology rather than contingent policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(neural_surveillance_accessibility, 0.68).
domain_priors:suppression_score(neural_surveillance_accessibility, 0.72).
domain_priors:theater_ratio(neural_surveillance_accessibility, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(neural_surveillance_accessibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(neural_surveillance_accessibility, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(neural_surveillance_accessibility, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(neural_surveillance_accessibility, snare).
narrative_ontology:human_readable(neural_surveillance_accessibility, "Neural Surveillance Accessibility: The Asymmetric Transparency Trap").
narrative_ontology:topic_domain(neural_surveillance_accessibility, "neurotechnology/surveillance/privacy").

domain_priors:requires_active_enforcement(neural_surveillance_accessibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(neural_surveillance_accessibility, surveillance_operators).
narrative_ontology:constraint_beneficiary(neural_surveillance_accessibility, data_aggregators).
narrative_ontology:constraint_victim(neural_surveillance_accessibility, neural_data_subjects).
narrative_ontology:constraint_victim(neural_surveillance_accessibility, cognitive_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNAWARE NEURAL SUBJECT (SNARE) — Individuals whose neural activity is captured through ubiquitous sensors (wearables, ambient IoT, workplace brain-computer interfaces) without meaningful consent or knowledge. Exit is materially impossible: the surveillance infrastructure is embedded in mandatory technologies (employment devices, medical systems, consumer platforms). The subject bears maximum extraction — their neural data is harvested for behavioral prediction, cognitive profiling, and influence targeting with no compensatory benefit or recourse.
constraint_indexing:constraint_classification(neural_surveillance_accessibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMPLOYED KNOWLEDGE WORKER (SNARE) — Workers whose employers mandate neural monitoring (focus tracking, cognitive load sensors, emotional state analysis) as a condition of employment. Exit costs are severe but not total: finding employment in sectors without neural surveillance requires retraining or relocation. The suppression is structural and economic — most stable, well-paying positions in knowledge sectors now include neural monitoring clauses. Extraction is extreme: the employer captures detailed cognitive and emotional data to optimize work output, adjust compensation, and predict quit risk.
constraint_indexing:constraint_classification(neural_surveillance_accessibility, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PRIVILEGED OPT-OUT SECTOR (TANGLED ROPE) — High-income professionals in sectors with strong privacy norms (law, medicine, academia at elite institutions) can often negotiate exemption from neural surveillance. These actors experience the constraint as a coordination mechanism: the monitoring infrastructure enables trust and security for the institutions that opt-in. They also benefit from the extraction directed at others — knowing that competitors' cognitive patterns are monitored reduces uncertainty about rival capabilities. This creates genuine mixed extraction: coordination function (maintaining institutional trust through selective monitoring) + asymmetric benefit (extraction falls on those without opt-out power).
constraint_indexing:constraint_classification(neural_surveillance_accessibility, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: SURVEILLANCE CAPITALIST (SNARE FROM OBSERVER VIEW, BUT ROPE FROM BENEFICIARY VIEW) — Corporations and state agencies operating the neural surveillance infrastructure experience it as pure coordination: the system solves the problem of predicting and managing populations. No extraction is experienced from their position — they are net beneficiaries. However, the engine will classify this as rope-like from the beneficiary perspective, revealing a significant perspectival gap. The actual classification from this agent's view would be rope (pure coordination with negative extraction flowing away from them), but the snare structure is what emerges when we measure from the target's structural position.
constraint_indexing:constraint_classification(neural_surveillance_accessibility, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY THEATER (PITON) — Data protection regulations (GDPR, CCPA, neural privacy laws) appear to govern neural surveillance through consent requirements and data minimization principles. In practice, these rules are largely performative: consent is bundled with service access (unreadable terms of service), data minimization is bypassed through derivative inference (inferring neural state from behavioral proxies), and enforcement is underfunded relative to the surveillance apparatus. The regulatory system maintains theater while the underlying extraction mechanisms proceed. Theater ratio is high because compliance checking creates the appearance of governance without restricting surveillance capability.
constraint_indexing:constraint_classification(neural_surveillance_accessibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, one might argue that neural surveillance is inevitable: as neurotechnology advances and becomes ubiquitous, some level of neural data leakage is inherent to the technology itself. Proponents might frame this as an immutable property of the neurotechnological substrate — no way to have the benefits without the surveillance risk. However, this is a false summit. The inevitability framing naturalizes what is actually a policy choice: the degree of surveillance, the consent mechanisms, the data retention, the adversarial use — these are all contingent on institutional arrangements, not on the physics of neural measurement itself.
constraint_indexing:constraint_classification(neural_surveillance_accessibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: ADVOCACY COALITION / ORGANIZED RESISTANCE (SNARE) — Civil liberties organizations, workers' unions, and neurorights advocates recognize the snare structure and organize collective resistance. From their position (organized power, constrained exit — they cannot opt out of the society they're trying to change, but can pool resources), the constraint appears as a snare with potential for collective action. However, their organizing capacity is severely suppressed through information control (neural data surveillance is used to identify and profile organizers), regulatory capture (surveillance companies fund and staff regulatory bodies), and platform control (organizing occurs on platforms that themselves engage in neural surveillance, creating dual surveillance: of the organizing activity and of the organizers' neural responses).
constraint_indexing:constraint_classification(neural_surveillance_accessibility, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(neural_surveillance_accessibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(neural_surveillance_accessibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(neural_surveillance_accessibility, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(neural_surveillance_accessibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(neural_surveillance_accessibility, TR),
    TR >= 0.70.

:- end_tests(neural_surveillance_accessibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Neural surveillance captures detailed, continuous data about cognitive and emotional states — potentially more revealing than any prior surveillance technology. This data is harvested for behavioral prediction, work optimization, compensation adjustment, quit-risk profiling, and targeted influence. The extraction is not total because some agents (privileged opt-out sector) can negotiate exemptions, and regulatory frameworks create minimum standards. However, the trend is toward higher extractiveness as measurement becomes cheaper and integration into mandatory systems deepens. Suppression (0.72): High. Suppression operates through multiple mechanisms: material economic coercion (employment dependency, cost of opt-out), epistemic opacity (individuals don't understand what neural data reveals or how it is used), platform control (organizing infrastructure is itself surveilled), and regulatory capture (operators write their own oversight). The suppression is robust because it combines structural barriers (material employment costs, technological integration) with cognitive barriers (internalized acceptance of surveillance as inevitable cost of modern work). Theater ratio (0.58): Moderate-high. Regulatory compliance creates theater: consent mechanisms are bundled and unreadable; data minimization rules are bypassed through derivative inference; enforcement budgets are trivial relative to surveillance apparatus. However, theater is not yet dominant because some actual enforcement exists (fines for CCPA violations, GDPR cases) and some genuine technical barriers to data use remain. As regulations are further captured, theater ratio will increase.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how structural position transforms classification. The surveillance operator's rope (pure coordination solving population prediction) and the data subject's snare (pure extraction with no escape) are measuring the same infrastructure from opposite ends. No single classification is 'correct' — the presheaf of perspectives reveals the asymmetry. The privileged opt-out sector's tangled_rope (genuine coordination plus asymmetric benefit) shows that the constraint could be transformed into Rope through universal opt-out rights or universal exemption. The analytical observer's false mountain (naturalizing neural surveillance as inevitable technology) reveals the risk of the civilizational perspective — technology determinism risks legitimizing what are actually policy choices. The regulatory theater (piton) shows that formal governance can be purely performative when enforcement is weak and regulator capture is deep.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's structural position: who benefits from neural data, who bears the cost of collection, and what exit options exist. Surveillance operators and data aggregators are primary beneficiaries (d ≈ 0.05-0.15): they have arbitrage exit options (can migrate neural data infrastructure to new applications), they benefit directly from data monetization, and they face no extraction themselves. Neural data subjects are primary victims (d ≈ 0.90-0.95): they have trapped or severely constrained exit (material employment dependency or technological ubiquity), they bear the cost of having their neural activity exposed, and they receive no direct compensatory benefit. Employed knowledge workers occupy an intermediate position (d ≈ 0.75-0.85): they have constrained but not trapped exit (can theoretically find employment elsewhere or change sectors at significant cost), they bear extraction (neural monitoring), but also receive some benefit (income, institutional affiliation). The privileged opt-out sector has lower d (≈ 0.35-0.45) because they have mobile exit (can actually negotiate exemption) and gain asymmetric benefit from knowing others are surveilled. The regulatory theater derives d indirectly from its capture by operators: from the beneficiary's perspective (d ≈ 0.05), from the target's perspective (d ≈ 0.90), the system appears to mediate extraction. The organized advocacy coalition has constrained exit (can't opt out of the society they're trying to change) and faces suppression (d ≈ 0.65-0.75), but organized power moderates the classification slightly from snare toward tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY ANALYSIS (EXTRACTIVENESS 0.68 > 0.46): This constraint requires resolution of the mandatrophy — the tension between seeing it as a coordination problem (neural surveillance enables trust and security for institutions) and seeing it as pure extraction (operators harvest neural data with minimal compensatory benefit to subjects). The resolution lies in recognizing that the mandatrophy is legitimate: the constraint IS both coordination and extraction, depending on perspective. From the operator's view, the infrastructure solves a genuine coordination problem (predicting and managing populations requires neural data). From the subject's view, that same coordination is experienced as extraction. The snare classification is correct for the primary target (powerless trapped subject), but the constraint exhibits genuine tangled_rope characteristics from the privileged opt-out perspective: coordination function (institutional security through neural monitoring) + asymmetric extraction (monitoring falls on those without opt-out power). The mandatrophy is resolved by recognizing that neural surveillance is intrinsically a Tangled Rope constraint that appears as a Snare when exit options are constrained and as a Rope when exit options are mobile. The snare classification in base_properties reflects the most common structural position (employed knowledge worker with constrained exit), not the intrinsic classification across all contexts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_fiction_vs_structural_coercion,
    'Is neural data collection coercive because exit is materially impossible, or is it coercive because individuals lack epistemic access to what neural data reveals about them?',
    'Comparison of exit behavior when: (a) neural monitoring is mandatory but transparent (subjects know exactly what is measured), vs (b) neural monitoring is optional but hidden (subjects don''t know they''re monitored). If exit increases dramatically in (a) despite mandatory framing, the coercion is epistemic-structural (cognitive autonomy violation) rather than material. If exit remains low in both, coercion is material (employment dependency).',
    'If epistemic: reclassify suppression from structural economic barriers to cognitive capture; may require identity_locked exit option for some agents. If material: suppression is correctly characterized; snare classification is robust. If both: suppression is higher than the raw metric suggests; the constraint exhibits both material traps and internalized acceptance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_fiction_vs_structural_coercion, empirical, 'Whether neural surveillance coercion is material or epistemic').

omega_variable(
    neural_data_derivative_inference,
    'Can neural state be accurately inferred from behavioral and physiological proxies without direct neural measurement, and if so, does this make neural surveillance technically redundant or does it reveal that the constraint''s function is fundamentally about behavioral control rather than neural prediction?',
    'Comparative accuracy studies: predictions from direct neural data vs predictions from behavioral proxy data (keystroke dynamics, eye tracking, voice stress, micro-expressions, heart rate variability) across cognitive and emotional inference tasks. If proxies match neural data accuracy, determine whether surveillance operators use both (suggesting over-collection for control purposes) or migrate away from neural data (suggesting neural surveillance is redundant and extinction-vulnerable).',
    'If proxies are sufficient: neural surveillance is Theater — the snare is maintained through regulatory capture and norm lock-in rather than technical necessity. Reclassify toward piton. If neural data adds significant capability: snare classification is robust. If operators collect both despite proxy sufficiency: suppression mechanism includes cognitive lock-in (subjects believe neural data is necessary for their own benefit), suggesting identity_locked considerations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neural_data_derivative_inference, empirical, 'Whether neural data is technically necessary or functionally redundant').

omega_variable(
    cognitive_autonomy_measurement,
    'What metrics operationalize ''cognitive autonomy'' and how do we measure its degradation from neural surveillance?',
    'Longitudinal cognitive assessments in populations with varying neural surveillance exposure: creative task performance, novel problem-solving, voluntary attention control, sense of agency, decision diversity. If cognitive autonomy metrics decline with surveillance exposure after controlling for other factors (stress, sleep, education), the victim designation is empirically grounded. If no decline appears, either (a) the threat is prospective (future degradation) or (b) the victim framing is normative (autonomy violation regardless of measured harm).',
    'If autonomy degrades measurably: snare classification is confirmed; victims are correctly identified. If no degradation: classification must rest on violation of autonomy principle (normative) rather than demonstrated harm; reclassify as tangled_rope (coordination + normative extraction). If prospective only: scaffold perspective gains force — current surveillance has sunset clause if cognitive autonomy protection mechanisms mature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cognitive_autonomy_measurement, empirical, 'Measurement of cognitive autonomy degradation from surveillance').

omega_variable(
    regulatory_capture_depth,
    'To what extent do surveillance technology operators fund, staff, and write the technical standards for neural privacy regulations, creating a circularity where the regulated entities design their own oversight?',
    'Network analysis of funding flows: which neural surveillance companies fund which neurorights NGOs, privacy policy organizations, and regulatory advisory bodies? Personnel flow analysis: do surveillance company employees rotate into regulatory positions? Standards committee membership: what proportion of neural measurement standard committees are staffed by surveillance operators? If capture is deep (>60% funding, >40% personnel overlap), the regulatory theater classification is confirmed. If capture is minimal (<20%), the regulatory system may have genuine independence and snare may be transitioning toward tangled_rope.',
    'If deeply captured: regulatory theater (piton) is sustained through institutional capture; paradoxically, the depth of capture means the constraint may be vulnerable to a single regulatory break (once captured regulator is discovered and replaced, the system collapses). If minimally captured: regulations may actually constrain extraction; snare classification remains robust but exit for organized agents may be less constrained than modeled.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Regulatory capture by neural surveillance operators').

omega_variable(
    collective_action_suppression_mechanism,
    'Are workers'' unions and civil liberties organizations unable to organize effective resistance against neural surveillance because: (a) they lack resources and messaging reach relative to surveillance operators, (b) surveillance operators use neural data to identify and preempt organizing, or (c) the cognitive capture runs so deep that potential organizers don''t perceive surveillance as a problem worth organizing against?',
    'Historical case studies of organizing attempts: timeline from organizing initiation to preemption/failure; analysis of whether preemption correlates with neural data access by operators; surveys of potential allies asking why they don''t organize (resource explanation vs awareness explanation vs belief explanation). If preemption timing correlates with surveillance operator data access, mechanism (b) is confirmed. If surveys show low perceived threat, mechanism (c) is dominant. If organizing fails due to resource asymmetry alone, mechanism (a) is primary.',
    'If (a): suppression is material-structural; snare classification is robust; collective action by organized agents remains theoretically possible. If (b): suppression is enhanced through data-driven preemption; snare classification is stronger; organized agents face a surveillance panopticon. If (c): suppression is cognitive-internal (agents don''t see the threat even when it exists); reclassify as involving identity_locked dynamics; cognitive lock-in is the binding mechanism, not just material suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_suppression_mechanism, empirical, 'Suppression mechanism targeting collective action').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(neural_surveillance_accessibility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(neuro_surv_tr_t0, neural_surveillance_accessibility, theater_ratio, 0, 0.35).
narrative_ontology:measurement(neuro_surv_tr_t3, neural_surveillance_accessibility, theater_ratio, 3, 0.45).
narrative_ontology:measurement(neuro_surv_tr_t6, neural_surveillance_accessibility, theater_ratio, 6, 0.54).
narrative_ontology:measurement(neuro_surv_tr_t10, neural_surveillance_accessibility, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(neuro_surv_be_t0, neural_surveillance_accessibility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(neuro_surv_be_t3, neural_surveillance_accessibility, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(neuro_surv_be_t6, neural_surveillance_accessibility, base_extractiveness, 6, 0.61).
narrative_ontology:measurement(neuro_surv_be_t10, neural_surveillance_accessibility, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(neural_surveillance_accessibility, enforcement_mechanism).
narrative_ontology:affects_constraint(neural_surveillance_accessibility, cognitive_liberty_erosion).
narrative_ontology:affects_constraint(neural_surveillance_accessibility, employment_coercion).
narrative_ontology:affects_constraint(neural_surveillance_accessibility, regulatory_capture).

% DUAL FORMULATION NOTE:
% Neural surveillance accessibility decomposes into three structurally distinct constraints: (1) the technology itself (neural measurement accuracy and ubiquity), (2) the asymmetric transparency structure (operators see neural data, subjects don't), and (3) the enforcement mechanism (institutions use neural data for behavioral control). This story addresses the accessibility structure (asymmetric transparency) and its institutional enforcement. Upstream constraints include cognitive_liberty_erosion (the philosophical claim that neural autonomy is a fundamental right being eroded) and employment_coercion (the economic dependency that makes neural surveillance mandatory). Downstream effects include regulatory_capture (the fact that surveillance operators write their own oversight rules) and cognitive_lock_in (the internalized acceptance of surveillance as inevitable).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(neural_surveillance_accessibility, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
