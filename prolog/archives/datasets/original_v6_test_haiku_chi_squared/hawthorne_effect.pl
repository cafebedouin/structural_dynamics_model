% ============================================================================
% CONSTRAINT STORY: hawthorne_effect
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hawthorne_effect, []).

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
 *   constraint_id: hawthorne_effect
 *   human_readable: The Hawthorne Effect (Observer Effect)
 *   domain: social/economic/behavioral_science
 *
 * SUMMARY:
 *   The Hawthorne Effect—the phenomenon in which individuals modify their
 *   behavior in response to awareness of being observed—represents a
 *   structural tension in social science between the desire to measure
 *   authentic behavior and the impossibility of measurement that does not
 *   disturb the measured system. This constraint exhibits characteristic
 *   features of a tangled rope: it enables genuine coordination (observation
 *   can reveal problems, improve safety, surface legitimate concerns) while
 *   simultaneously extracting behavioral modification and epistemic
 *   contamination (the measured behavior is not authentic to the system in
 *   its unobserved state). The effect has become a foundational assumption in
 *   management science, behavioral economics, and experimental design, often
 *   invoked to dismiss surprising research findings as 'just the Hawthorne
 *   effect.' Yet the original Hawthorne studies (1924-1932) at the Western
 *   Electric factory in Illinois are now recognized as methodologically
 *   compromised by confounding variables: simultaneous changes to working
 *   conditions, attention from management, social cohesion in study groups,
 *   and wage increases alongside observation. The constraint's extractiveness
 *   has increased over time (0.25 → 0.52) as institutional actors learned to
 *   leverage observation as a control mechanism independent of material
 *   improvements. The theater ratio (0.68) reflects that much contemporary
 *   discussion of the Hawthorne effect serves a performative function:
 *   invoking it as an explanation for unexpected findings, without engaging
 *   the deeper question of how observation apparatus can be designed to
 *   minimize reactivity.
 *
 * KEY AGENTS:
 *   - Observed Worker: Primary victim (powerless/trapped) — must modify behavior in response to observation; cannot exit without leaving employment
 *   - Research Participant: Secondary victim/beneficiary (moderate/constrained) — experiences both extraction and coordination benefits; constrained by study enrollment
 *   - Management/Research Institution: Primary beneficiary (institutional/arbitrage) — extracts productivity and data; can exit constraint by ceasing observation
 *   - Labor Protection Coalition: Organized agent (organized/mobile) — advocating for worker consent frameworks, privacy protection, alternative measurement methodologies
 *   - Scientific Establishment: Institutional actor (institutional/constrained) — maintains observation-centric paradigm despite recognition of measurement reactivity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional practice by analogy to quantum measurement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hawthorne_effect, 0.52).
domain_priors:suppression_score(hawthorne_effect, 0.58).
domain_priors:theater_ratio(hawthorne_effect, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hawthorne_effect, extractiveness, 0.52).
narrative_ontology:constraint_metric(hawthorne_effect, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hawthorne_effect, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hawthorne_effect, tangled_rope).
narrative_ontology:human_readable(hawthorne_effect, "The Hawthorne Effect (Observer Effect)").
narrative_ontology:topic_domain(hawthorne_effect, "social/economic/behavioral_science").

domain_priors:requires_active_enforcement(hawthorne_effect).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hawthorne_effect, institutional_observers).
narrative_ontology:constraint_beneficiary(hawthorne_effect, research_institutions).
narrative_ontology:constraint_beneficiary(hawthorne_effect, management_hierarchy).
narrative_ontology:constraint_victim(hawthorne_effect, authentic_behavior_agents).
narrative_ontology:constraint_victim(hawthorne_effect, epistemic_commons).
narrative_ontology:constraint_victim(hawthorne_effect, worker_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OBSERVED WORKER (SNARE) — Cannot cease being observed without exit from employment or social context. Must modify behavior in response to awareness of observation. Extraction: productivity is extracted and attributed to external causes (the observer effect itself) rather than to structural incentive changes. d≈0.93, f(d)≈1.40, σ=0.8 → χ≈0.58.
constraint_indexing:constraint_classification(hawthorne_effect, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: RESEARCH PARTICIPANT (TANGLED ROPE) — Constrained by employment relationship or study enrollment, but also benefits from the attention regime: improved work conditions, safety focus, feedback loops. Experiences both extraction (behavioral modification is involuntary) and coordination (observation can reveal genuine problems). d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.50.
constraint_indexing:constraint_classification(hawthorne_effect, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MANAGEMENT/RESEARCH INSTITUTION (ROPE) — Benefits from enhanced productivity and data collection. Experiences observation as pure coordination: attention improves outcomes, systematic measurement enables optimization. Can exit the constraint by ceasing observation (arbitrage). d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(hawthorne_effect, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR PROTECTION COALITION (SCAFFOLD) — Organized agents (labor unions, worker advocacy groups, privacy advocates) see observation as a temporary coordination problem with sunset logic. Alternative mechanisms emerging: blind performance metrics, algorithmic fairness standards, worker consent frameworks, anonymized data collection. d≈0.42, f(d)≈0.42, σ=1.0 → χ≈0.22. Low effective extraction because coalition has agency and sees exit pathways.
constraint_indexing:constraint_classification(hawthorne_effect, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: SCIENTIFIC METHOD ESTABLISHMENT (PITON) — Institutional commitment to controlled observation and measurement persists despite recognition that observation changes outcomes. The Hawthorne effect itself is now taught as a methodological artifact to control for, yet the control mechanisms (blinding, randomization) are increasingly difficult to implement in real-world social science. Theater ratio ≈0.68: much effort goes into discussing the constraint rather than dissolving it. The scientific establishment maintains the observation paradigm through inertia despite degraded functional capacity.
constraint_indexing:constraint_classification(hawthorne_effect, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a fundamental physics perspective, any measurement of a complex system changes the system. The observer effect in quantum mechanics (measurement collapse) is presented as a natural law. Some analysts argue the Hawthorne effect is an irreducible feature of social observation: you cannot measure behavior without changing it. However, structural data (ε=0.52, suppression=0.58) contradicts mountain classification. The false summit exposes the analogy as rhetorical naturalization of a contingent institutional practice.
constraint_indexing:constraint_classification(hawthorne_effect, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hawthorne_effect_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hawthorne_effect, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hawthorne_effect, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hawthorne_effect, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hawthorne_effect, TR),
    TR >= 0.70.

:- end_tests(hawthorne_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Institutional actors extract behavioral modification and epistemic advantage (they know the baseline is distorted; the observed do not). The original Hawthorne studies showed productivity increases during observation that reverted when observation ended, revealing extraction: the gains belonged to the observer's analytical framework, not to authentic worker improvement. However, extractiveness is not maximal because some genuine benefits do flow to workers (attention to safety, management responsiveness when conditions improve). The temporal trajectory (0.25 → 0.52) reflects institutional learning: early use of observation was entangled with material improvements; modern surveillance capitalism uses observation decoupled from worker benefits, driving extraction higher. Suppression (0.58): Moderate-high. Significant barriers to authentic behavior include employment dependence, power asymmetry, career risk of nonconformity, and the epistemic impossibility of measuring behavior without affecting it. However, suppression is not total because workers can sometimes anticipate what behavior is expected and consciously choose conformity (constrained agency rather than total trapped-ness). Theater ratio (0.68): High. Extensive discussion of the Hawthorne effect in academic and management contexts serves a performative function: it signals methodological sophistication ('we acknowledge measurement reactivity') without requiring actual redesign of observation apparatus. The effect is invoked as explanation rather than provocation to alternative methodologies.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates significant perspectival divergence. Management sees pure coordination (Rope): observation improves outcomes, reveals problems, enables optimization. They experience it as voluntary information-gathering with benefits flowing back to the system. The observed worker sees extraction (Snare): behavior modification is involuntary, information asymmetry is permanent, and the extracted behavioral change is attributed to external causes rather than authentic preference shifts. The research participant sees mixed coordination and extraction (Tangled Rope): observation creates conditions for attention and feedback (coordination benefit) while simultaneously constraining authentic choice (extraction cost). The labor protection coalition sees a temporary institutional arrangement with a sunset clause (Scaffold): alternative mechanisms (consent frameworks, blind metrics, anonymized data) are emerging and will eventually displace observation-based control. The scientific establishment sees its own degraded ritual (Piton): the Hawthorne effect is now recognized as a methodological artifact, yet the solution (truly blind observation, which is conceptually contradictory) cannot be fully implemented, leaving the institution committed to a mechanism it knows is compromised. The analytical observer risks seeing a natural law (Mountain): the quantum mechanical analogy (you cannot measure without disturbing) is presented as proving the Hawthorne effect is inevitable. However, the moderate structural scores (ε=0.52, not 0.85+) reveal this as a false summit—the constraint is contingent on institutional designs, not immutable.
 *
 * DIRECTIONALITY LOGIC:
 *   Observed worker: Victim + trapped → d≈0.93, f(d)≈1.40. Near-maximum extraction. Employment dependence + awareness of observation change + inability to avoid observation + information asymmetry = high d. Research participant: Victim + constrained → d≈0.68, f(d)≈1.05. High extraction but moderated by some coordination benefits (attention, feedback, safety improvements). Management/research institution: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Negative effective extraction (net benefit) because institutional actors control observation apparatus and can exit the constraint by ceasing observation. Labor protection coalition: Organized + mobile → d≈0.42, f(d)≈0.42. Low effective extraction; coalition has agency and real alternative pathways (consent frameworks, blind metrics) that provide genuine exit. Scientific establishment: Institutional + constrained → d≈0.25, f(d)≈0.15. Piton classification comes from theater ratio gate (0.68 ≥ 0.70 threshold approached), not from high chi. The institution is partially captured by its own commitment to observation paradigm but has agency to redesign. Analytical observer: Analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival false summit; the observer naturalizes contingent institutional practice.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the Hawthorne effect itself is a methodological response to the problem of measurement reactivity, not a natural law. The constraint operates through institutional architecture: (1) observation apparatus is observable to the subject, (2) institutional actors control observation apparatus, (3) institutional actors extract both productivity and epistemic advantage, (4) subjects lack exit options or consent frameworks. This architecture is recent and contingent—it became dominant in the 20th century with the rise of scientific management and industrial psychology. The false summit detection reveals that invoking quantum mechanics as justification ('measurement always disturbs the system') naturalizes what is actually a choice about institutional design. Alternative architectures exist: (a) subjects informed of observation design and given consent (reduces extraction, increases coordination), (b) blind metrics where observation apparatus is hidden from subject (reduces theater, maintains extraction), (c) algorithmic fairness standards replacing human observation (shifts constraint upstream to algorithm design), (d) distributed observation where subjects also observe observers (reduces information asymmetry, increases coordination). The scaffold perspective captures real emerging alternatives: labor regulations increasingly require consent, workplace privacy laws are tightening, algorithmic accountability frameworks are developing. The sunset is not inevitable but is structurally supported by organized agents with agency. The constraint is Tangled Rope (mixed coordination and extraction) rather than Mountain (immutable law) precisely because the institutional architecture that enables it is within human control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    baseline_behavior_definition,
    'What is ''authentic'' or ''baseline'' behavior independent of observation, and how would we measure it?',
    'Comparison of behavior in contexts with and without awareness of observation; longitudinal tracking of behavior after observation apparatus is removed; neuroimaging or physiological markers of authentic vs performed behavior',
    'If baseline is stable and knowable: Hawthorne effect is contingent and remediable (Tangled Rope/Scaffold). If baseline is undefined or observation-dependent at all levels: constraint is close to a natural law (Mountain).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(baseline_behavior_definition, conceptual, 'Whether authentic baseline behavior independent of observation can be defined or measured').

omega_variable(
    observation_apparatus_opacity,
    'Can observation ever be sufficiently opaque or delayed that subjects cease modifying behavior?',
    'Hidden camera experiments with delayed feedback; comparison of behavior when told observation is active vs inactive (cross-verified with actual observation data); analysis of behavior ''drift'' over extended unobserved periods',
    'If subjects can habituate to observation or forget about it: effect is contingent on salience and can be engineered away (Rope-like mitigation). If effect persists even with opacity: constraint is structural (closer to Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observation_apparatus_opacity, empirical, 'Whether observation apparatus opacity or latency can reduce behavioral modification').

omega_variable(
    institutional_incentive_alignment,
    'How much of the productivity gain attributed to ''Hawthorne effect'' is actually due to improved working conditions, attention to worker concerns, or alignment of observer interests with worker interests?',
    'Controlled comparison: observation WITH management responsiveness vs observation WITHOUT responsiveness; measurement of productivity changes when observation occurs but conditions/feedback do not change; historical analysis of original Hawthorne studies'' confounding variables',
    'If gains are primarily from attention and responsiveness: constraint is coordination (Rope from all perspectives, minor Snare from worker). If gains persist even without responsiveness: constraint is pure behavioral modification (Snare/Tangled Rope mix).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_incentive_alignment, empirical, 'How much productivity gain is from observer attention vs improved conditions or feedback').

omega_variable(
    emergent_authenticity_threshold,
    'Is there a time horizon beyond which authentic behavior re-emerges despite ongoing observation?',
    'Longitudinal studies spanning months to years; behavioral analysis of long-term monitored populations (prisoners, institutionalized patients, remote workers under continuous surveillance); identification of habituation patterns and return to ''baseline'' behavior',
    'If authenticity emerges after N months: scaffold sunset is real and measurable. If behavior modification persists indefinitely: constraint approaches structural inevitability (mountain-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergent_authenticity_threshold, empirical, 'Whether authentic behavior re-emerges after extended exposure to observation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hawthorne_effect, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(haw_tr_t0, hawthorne_effect, theater_ratio, 0, 0.42).
narrative_ontology:measurement(haw_tr_t5, hawthorne_effect, theater_ratio, 5, 0.55).
narrative_ontology:measurement(haw_tr_t10, hawthorne_effect, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(haw_be_t0, hawthorne_effect, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(haw_be_t5, hawthorne_effect, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(haw_be_t10, hawthorne_effect, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hawthorne_effect, information_standard).
narrative_ontology:affects_constraint(hawthorne_effect, measurement_reactivity_general).
narrative_ontology:affects_constraint(hawthorne_effect, workplace_surveillance_capitalism).
narrative_ontology:affects_constraint(hawthorne_effect, algorithmic_fairness_proxy_problems).

% DUAL FORMULATION NOTE:
% The Hawthorne Effect is upstream of broader constraints around measurement reactivity in social science and downstream of institutional choices about observation apparatus design and worker consent frameworks. The effect itself (behavioral modification in response to observation awareness) is a coordination problem with entangled extraction; alternative observational methodologies could reduce extractiveness while maintaining coordination benefits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hawthorne_effect, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
