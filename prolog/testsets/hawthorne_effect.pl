% ============================================================================
% CONSTRAINT STORY: hawthorne_effect
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   domain: social/economic/behavioral
 *
 * SUMMARY:
 *   The Hawthorne Effect describes how individuals modify their behavior in
 *   response to awareness of being observed. The original Hawthorne studies
 *   (1924-1933) investigated worker productivity at the Western Electric
 *   Company and found that both improved working conditions AND deteriorated
 *   conditions led to increased productivity — a result attributed to the
 *   fact that workers were aware they were being studied. This constraint
 *   creates a structural tension between the epistemic goal of accurate
 *   behavioral measurement and the institutional reality that measurement
 *   itself corrupts the observed behavior. The same structural phenomenon —
 *   the reactive nature of observation — appears as an immutable natural law
 *   (mountain: observer effect is inherent to consciousness), a coordination
 *   problem solved through better experimental design (rope: observation
 *   enables communication), a mixed coordination-extraction hybrid (tangled
 *   rope: measurement enables knowledge production but suppresses
 *   authenticity), a temporary problem being solved through transparency and
 *   consent (scaffold: informed observation regimes), and a degraded
 *   methodological ritual (piton: statistical controls persist despite
 *   widespread acknowledgment of their insufficiency). The constraint
 *   operates through awareness and behavioral modification pressure, making
 *   it fundamentally about the gap between authentic behavior and performed
 *   behavior.
 *
 * KEY AGENTS:
 *   - Observed Subjects: Primary victim (powerless/trapped) — workers, research participants, monitored citizens who cannot exit the observation context without career/participation consequences
 *   - Observer Institution: Primary beneficiary (institutional/arbitrage) — research organizations, management consultancies, government agencies, and surveillance systems that extract behavioral and productivity data
 *   - Middle Management: Secondary actor (moderate/constrained) — supervisors and implementers who benefit from measured productivity but also face pressure from knowing that metrics are reactive
 *   - Transparency Coalition: Organized agents (organized/constrained) — labor unions, worker advocates, research ethicists building informed consent and disclosure protocols as sunset mechanisms
 *   - Scientific Method Apparatus: Institutional actor (institutional/arbitrage) — the standardized protocols, statistical controls, and methodological rituals that maintain the observation regime through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent institutional practice as an inherent property of measurement itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hawthorne_effect, 0.38).
domain_priors:suppression_score(hawthorne_effect, 0.42).
domain_priors:theater_ratio(hawthorne_effect, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hawthorne_effect, extractiveness, 0.38).
narrative_ontology:constraint_metric(hawthorne_effect, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(hawthorne_effect, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hawthorne_effect, tangled_rope).
narrative_ontology:human_readable(hawthorne_effect, "The Hawthorne Effect (Observer Effect)").
narrative_ontology:topic_domain(hawthorne_effect, "social/economic/behavioral").

domain_priors:requires_active_enforcement(hawthorne_effect).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hawthorne_effect, observer_institution).
narrative_ontology:constraint_beneficiary(hawthorne_effect, knowledge_extractors).
narrative_ontology:constraint_victim(hawthorne_effect, observed_subjects).
narrative_ontology:constraint_victim(hawthorne_effect, behavioral_authenticity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OBSERVED SUBJECT (SNARE) — Worker or research participant cannot exit the observation context without career/participation consequences. Awareness of observation creates behavioral modification pressure with no genuine choice. The subject experiences maximum extraction: authentic behavior is suppressed, replaced by performative compliance. No exit options available within the constraint.
constraint_indexing:constraint_classification(hawthorne_effect, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MIDDLE MANAGER (TANGLED ROPE) — Benefits from improved measured productivity during observation periods (gets credit for 'managing the effect'), but also constrained by knowledge that measurements are reactive and behavioral data is compromised. Experiences both coordination function (supervision does enable communication) and extraction (pressure to maintain performative metrics). Constrained exit: cannot fully leave the observation regime without losing supervisory authority.
constraint_indexing:constraint_classification(hawthorne_effect, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: OBSERVER INSTITUTION (ROPE) — Research institutions, management consultancies, and survey organizations benefit from extracting productivity data and behavioral information. Experience the constraint as pure coordination: observation enables measurement, measurement enables knowledge production. Can arbitrage the behavioral modification effect (designing experiments to measure it) or bypass it (switching to unobserved contexts). Net beneficiary with genuine exit options.
constraint_indexing:constraint_classification(hawthorne_effect, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRANSPARENCY COALITION (SCAFFOLD) — Organized labor, worker advocacy groups, and scientific integrity organizations advocate for observer disclosure and informed consent regimes. See the raw Hawthorne effect as a temporary problem with a sunset: transparent observation protocols, automated measurement, and participatory research design are reducing the reactivity gap. Theater ratio declining as observation becomes normalized and depersonalized. Extraction mechanism weakens as agency and consent replace covert surveillance.
constraint_indexing:constraint_classification(hawthorne_effect, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SCIENTIFIC METHOD APPARATUS (PITON) — The Hawthorne effect is now largely recognized as a confound in experimental design rather than a novel phenomenon. Observation protocols persist through institutional ritual (double-blind procedures, control groups, statistical adjustment) despite widespread acknowledgment that they don't solve reactivity — they just redistribute it. The apparatus maintains theater (observance of methodological procedure) while the original functional claim (that we can separate observer from observed) has degraded. Sustained through methodological inertia.
constraint_indexing:constraint_classification(hawthorne_effect, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, observation-induced behavior modification is an inherent feature of conscious agents: awareness of observation necessarily alters the observed system (related to thermodynamic measurement uncertainty, quantum mechanics' observer effect, and the reflexivity principle in social science). The constraint appears as an immutable law of measurement itself. However, the structural data (moderate extractiveness, significant suppression, high theater) contradicts the mountain classification — the engine will compute this as a false summit, revealing naturalization of what is actually a contingent institutional practice.
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

test(piton_threshold) :-
    domain_priors:theater_ratio(hawthorne_effect, TR),
    TR >= 0.70.

:- end_tests(hawthorne_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts behavioral authenticity and replaces it with performative compliance. The original 1924-1933 Hawthorne studies showed productivity increases under both improvement and deterioration conditions, indicating the effect size is real but not maximal — workers were modifying behavior but not completely suppressing authentic responses. Modern assessment suggests the effect is conditional on context and subject population; extractiveness is not as high as pure coercion (which would be >0.60). Suppression (0.42): Moderate-high. Significant barriers prevent authentic behavior: awareness of observation creates psychological pressure, career/institutional consequences for non-compliance, lack of genuine alternatives, and no meaningful exit. But suppression is not total — some subjects can and do resist performative pressure, and transparency protocols partially reduce the barrier. Theater ratio (0.68): High. Contemporary observation regimes are substantially performative: double-blind procedures, statistical controls, and methodological rituals persist despite widespread acknowledgment that they don't solve reactivity. The rituals maintain the appearance of controlled measurement while the core problem (observer-induced modification) remains unresolved. Theater has increased over the interval as the scientific community has institutionalized observation protocols and defended them through methodological elaboration rather than addressing the underlying reactivity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival divergence between those who experience measurement and those who conduct it. The observed subject sees pure extraction (Snare) — awareness of observation is mandatory, behavioral modification is coercive, and there is no exit. The observer institution sees pure coordination (Rope) — observation enables knowledge production, and they can exit by choosing alternative measurement contexts. The middle manager sees hybrid extraction-coordination (Tangled Rope) — they benefit from measured improvements but understand the measurements are reactive. The transparency coalition sees a temporary problem with a sunset (Scaffold) — informed consent and automated monitoring are reducing the extraction mechanism. The scientific apparatus sees its own degraded ritual (Piton) — methodological controls persist through professional convention despite widespread acknowledgment of their insufficiency. The civilizational analytical observer risks seeing an immutable natural law (Mountain) — observation necessarily modifies behavior — but the structural data reveals this as naturalization: the effect size is contingent on institutional context, power asymmetries, and disclosure regimes.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's structural position relative to the observation-extraction flow. Observed subjects (powerless/trapped) have zero exit options and bear full cost of behavioral suppression — they experience high d (~0.95) leading to high f(d) and high chi. Observer institutions (institutional/arbitrage) benefit from behavioral data and can exit by switching to alternative measurement contexts or by arbitraging the effect itself — they experience low d (~0.15) leading to negative f(d) and negative chi. Middle managers (moderate/constrained) occupy an intermediate position: they benefit from measured productivity improvements but face pressure from knowing metrics are reactive — they experience moderate d (~0.55) leading to moderate chi. The transparency coalition (organized/constrained) experiences constrained exit through advocacy and protocol reform rather than individual escape — they experience moderate-high d (~0.65) but with agency that produces organized action rather than trapped suffering. The scientific apparatus (institutional/arbitrage) is a beneficiary through its role in maintaining the observation regime — low d (~0.15). The analytical observer's mountain perspective risks zero d (natural law, no directionality) but is exposed as false because the engine computes the perspective's directionality from the constraint's actual structure, not from its natural-law framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The Hawthorne Effect resolves the mandatrophy by demonstrating that all six types are legitimate perspectival readings reflecting genuinely different structural positions. The constraint is not 'which type is correct?' but 'which observer position are you reading from?' The beneficiary's rope and the victim's snare are both accurate descriptions of their structural experiences. The scaffold is a real institutional structure (consent protocols, transparency norms) being built to provide sunset mechanisms. The piton is a real observation about scientific practice (ritualized controls with degraded function). The mountain is a false summit — the analytical observer risks naturalizing what is actually a contingent, modifiable institutional arrangement (observation regimes, power asymmetries, disclosure requirements). The presheaf over the observation site reveals that mandatrophy is not a flaw but a feature: the constraint genuinely looks different from different structural positions, and naming those positions precisely is the work of indexed classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    automation_vs_awareness_threshold,
    'Does automated observation (sensors, algorithmic monitoring) reduce reactivity compared to human observation, or does awareness of automation produce equivalent behavioral modification?',
    'Controlled experiments comparing behavior under human observation vs algorithmic observation vs no observation; analysis of stress biomarkers and behavioral deviation rates',
    'If automation reduces reactivity: extractiveness drops to ~0.15, constraint reclassifies as Rope from most perspectives. If awareness of algorithms produces equivalent modification: extractiveness remains ~0.38, and automation is merely a theater substitution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automation_vs_awareness_threshold, empirical, 'Whether automation reduces observation-induced reactivity').

omega_variable(
    informed_consent_suppression_reduction,
    'Does explicit informed consent about observation reduce the extraction mechanism, or does the knowledge that one has consented create a different form of behavioral pressure?',
    'Meta-analysis of consent protocols and behavioral modification rates; longitudinal studies tracking behavior before, during, and after consent disclosure',
    'If consent genuinely reduces extraction: suppression drops to ~0.20, constraint reclassifies as Rope or Scaffold from most perspectives. If consent creates performative compliance of a different type: suppression remains ~0.42, and transparency is theater substitution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_consent_suppression_reduction, empirical, 'Whether informed consent reduces observation-induced suppression').

omega_variable(
    observer_effect_universality,
    'Is observation-induced behavior modification a universal feature of conscious measurement systems, or is it a contingent property of specific social/institutional contexts?',
    'Cross-cultural and cross-species studies of reactivity; analysis of whether measurement protocols in non-human systems exhibit equivalent modification patterns',
    'If universal: mountain classification is justified, extractiveness is ~0.10, constraint reflects a natural law. If contingent: constraint is fundamentally social/institutional, extractiveness remains ~0.38, and the mountain perspective is a false summit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(observer_effect_universality, conceptual, 'Whether observation effects are universal laws or contingent institutional properties').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hawthorne_effect, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(haw_tr_t0, hawthorne_effect, theater_ratio, 0, 0.45).
narrative_ontology:measurement(haw_tr_t25, hawthorne_effect, theater_ratio, 25, 0.62).
narrative_ontology:measurement(haw_tr_t50, hawthorne_effect, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(haw_be_t0, hawthorne_effect, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(haw_be_t25, hawthorne_effect, base_extractiveness, 25, 0.34).
narrative_ontology:measurement(haw_be_t50, hawthorne_effect, base_extractiveness, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hawthorne_effect, information_standard).
narrative_ontology:affects_constraint(hawthorne_effect, measurement_validity_degradation).
narrative_ontology:affects_constraint(hawthorne_effect, workplace_surveillance_extraction).

% DUAL FORMULATION NOTE:
% The Hawthorne Effect is a general phenomenon but operates through distinct mechanisms in different institutional contexts. Workplace observation (productivity measurement, surveillance) and research observation (experimental design, informed consent) share the same base structure (awareness → behavior modification) but differ in exit options and consent regimes. This story captures the general constraint; downstream stories handle domain-specific implementations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
