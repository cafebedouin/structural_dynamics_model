% ============================================================================
% CONSTRAINT STORY: sleep_deprivation_cycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sleep_deprivation_cycle, []).

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
 *   constraint_id: sleep_deprivation_cycle
 *   human_readable: Sleep Deprivation Cycle
 *   domain: physiology/behavioral_psychology
 *
 * SUMMARY:
 *   The sleep deprivation cycle is a self-reinforcing constraint where
 *   initial sleep loss triggers physiological and behavioral adaptations that
 *   prevent recovery, creating escalating cognitive and health damage. The
 *   constraint exhibits multiple structural classifications depending on
 *   observer position: the individual trapped in the cycle experiences pure
 *   extraction (snare); the workplace coordination system depends on it for
 *   temporal synchronization (tangled rope); institutional extractors benefit
 *   from productivity gains (rope); occupational health frameworks address it
 *   performatively without enforcement (piton); a natural law view risks
 *   treating biological sleep need as if it justifies the social arrangement
 *   (false mountain); and organized movements are building alternative
 *   coordination without deprivation (scaffold). The extractiveness
 *   trajectory shows escalation over 12 months as acute sleep loss becomes
 *   chronic, suppression deepens through physiological compensation
 *   mechanisms and behavioral habituation, and theater increases as formal
 *   sleep hygiene interventions persist without functional integration.
 *
 * KEY AGENTS:
 *   - Sleep-Deprived Individual: Primary victim (powerless/trapped) — bears cumulative cognitive and health damage; no structural exit available without intervention
 *   - Workplace/Productivity System: Primary beneficiary (institutional/arbitrage) — captures productivity surplus from extended hours; experiences constraint as coordination mechanism
 *   - Workplace Managers: Secondary extractors (moderate/mobile) — face cultural pressure to demand extended hours but often unaware of suppression mechanisms
 *   - Sleep Science and Occupational Health Community: Institutional observer (organized/constrained) — maintains performative safety frameworks with limited enforcement
 *   - Work-Life Integration Movement: Organized resistance (organized/mobile) — building alternative coordination pathways with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing social pressure as biological constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sleep_deprivation_cycle, 0.68).
domain_priors:suppression_score(sleep_deprivation_cycle, 0.72).
domain_priors:theater_ratio(sleep_deprivation_cycle, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sleep_deprivation_cycle, extractiveness, 0.68).
narrative_ontology:constraint_metric(sleep_deprivation_cycle, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sleep_deprivation_cycle, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sleep_deprivation_cycle, snare).
narrative_ontology:human_readable(sleep_deprivation_cycle, "Sleep Deprivation Cycle").
narrative_ontology:topic_domain(sleep_deprivation_cycle, "physiology/behavioral_psychology").

% --- Structural relationships ---
narrative_ontology:constraint_victim(sleep_deprivation_cycle, sleep_deprived_individual).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SLEEP-DEPRIVED INDIVIDUAL (SNARE) — Caught in escalating physiological and behavioral entrapment. Initial sleep loss triggers compensatory arousal systems (elevated cortisol, adrenaline) that prevent recovery sleep even when opportunity arises. Attempts to exit through medication or behavioral intervention often fail due to learned insomnia patterns. The individual bears extraction through cognitive degradation, health damage, and inability to organize collective response. Maximum experienced extraction — no structural exit option remains available without external intervention.
constraint_indexing:constraint_classification(sleep_deprivation_cycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: WORKPLACE COORDINATION SYSTEM (TANGLED ROPE) — The constraint serves a genuine coordination function: time pressure and extended hours coordinate multiple project components and create shared deadline commitment. But this coordination function is paired with asymmetric extraction. Workers bear sleep deprivation while employers capture productivity gains. Exit from the cycle requires losing employment or explicit contract renegotiation, creating persistent constraint even when exit is technically possible. Moderate power agents face constrained exit options.
constraint_indexing:constraint_classification(sleep_deprivation_cycle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PRODUCTIVITY EXTRACTION INSTITUTION (ROPE) — From the perspective of institutional extractors (demanding employers, high-pressure organizational cultures), the sleep deprivation cycle appears as a coordination mechanism: extended hours create temporal synchronization and concentrated cognitive effort during perceived critical periods. The institution experiences the constraint as solving a real scheduling problem while capturing productivity surplus. Net beneficiary position — extraction runs toward this institutional actor with minimal visible coercion cost to them.
constraint_indexing:constraint_classification(sleep_deprivation_cycle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SLEEP SCIENCE AND OCCUPATIONAL HEALTH COMMUNITY (PITON) — The research and regulatory frameworks addressing sleep deprivation harms are substantially performative. Sleep hygiene guidelines, fatigue-risk management systems, and occupational sleep standards are maintained through institutional ritual despite limited enforcement and widespread non-compliance. The community sees its own interventions as degraded — evidence-based recommendations persist through inertia while behavioral reality diverges sharply. Theater ratio is high because formal systems (workplace sleep policies, fatigue breaks) exist but are neither functionally integrated nor genuinely enforced.
constraint_indexing:constraint_classification(sleep_deprivation_cycle, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some sleep requirement is an immutable biological constraint: human neurobiology requires consolidated sleep; chronic deprivation causes cumulative cognitive and physical damage; no technology has eliminated this biological floor. This perspective risks naturalizing the social arrangement (workplace time pressure, productivity expectations) as if it were a law of physiology. However, the structural data reveals this as a false summit: the biological sleep requirement is real, but the deprivation cycle that extracts from individuals is a contingent institutional arrangement, not a law of nature.
constraint_indexing:constraint_classification(sleep_deprivation_cycle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: WORK-LIFE INTEGRATION MOVEMENT (SCAFFOLD) — Organized advocacy (labor movements, worker cooperatives, remote-work normalization, flexible scheduling legislation) is building alternative coordination pathways that decouple temporal synchronization from extended hours. Sleep-protecting work norms represent a temporary support structure with sunset logic: as remote work, asynchronous communication, and results-based evaluation normalize, the pressure for synchronous presence and extended hours declines. Organized agents with mobile exit options (ability to switch employers, form alternatives) see a sunset pathway rather than permanent entrapment. Theater is moderate because scaffolding mechanisms (4-day work weeks, sleep-protected leave) show genuine functional alternatives rather than purely performative activity.
constraint_indexing:constraint_classification(sleep_deprivation_cycle, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sleep_deprivation_cycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sleep_deprivation_cycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sleep_deprivation_cycle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sleep_deprivation_cycle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sleep_deprivation_cycle, TR),
    TR >= 0.70.

:- end_tests(sleep_deprivation_cycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and escalating. The sleep deprivation cycle generates sustained extraction through cognitive degradation (impaired decision-making, reduced learning capacity, attention deficits), health damage (increased infection risk, cardiovascular stress, metabolic disruption), and productivity paradox (apparent short-term output gains from extended hours mask longer-term capability loss). The trajectory from 0.35 to 0.68 reflects that acute sleep loss is reversible and partially legitimized by project urgency, but chronic deprivation accumulates irreversible harms. Suppression (0.72): High. Multiple suppression mechanisms operate: (1) Physiological: elevated cortisol and adrenaline prevent recovery sleep even when opportunity exists, creating biological lock-in. (2) Behavioral: sleep deprivation impairs executive function and cognitive capacity for organizing resistance. (3) Institutional: career risk, income dependency, and professional identity fusion trap individuals in deprivation cycles even when exit is nominally available. (4) Epistemic: deprivation itself impairs recognition of the constraint's severity — sleep-deprived individuals misestimate their own cognitive degradation. Theater ratio (0.58): Moderate. Sleep hygiene recommendations, fatigue-risk management systems, and workplace sleep policies exist but are substantially decoupled from actual enforcement or behavior change. Formal interventions (sleep education, nap facilities) operate alongside continued time pressure and extended-hours expectations, creating performative commitment without functional integration.
 *
 * PERSPECTIVAL GAP:
 *   The sleep deprivation cycle demonstrates maximal perspectival divergence. The trapped individual sees a snare: they cannot recover, cannot organize, and cannot exit despite clear awareness of harm. The workplace system sees tangled rope: genuine coordination of project timelines mixed with asymmetric extraction of worker capacity. The institutional extractor sees rope: solving scheduling problems through temporal synchronization without perceiving coercion. The occupational health community sees piton: performative frameworks without enforcement, institutional maintenance through ritual. The civilizational analytical observer risks seeing mountain: sleep need is real, so deprivation seems inevitable. The movement sees scaffold: deprivation is a contingent institutional choice, and alternative coordination (asynchronous work, flexible schedules, remote options) decouples temporal pressure from extended hours. No single perspective is wrong — they are measuring from genuinely different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position within the extraction flow. The powerless individual with trapped exit (d ≈ 0.95) bears maximum extraction through physiological lock-in and impaired capacity for organizing. The institutional extractor with arbitrage exit (d ≈ 0.10) experiences the constraint as beneficial coordination. The moderate workplace system with constrained exit (d ≈ 0.60) experiences mixed coordination benefit and extraction cost. The organized movement with mobile exit (d ≈ 0.40) experiences the constraint as solvable through alternative coordination. The analytical observer sees the entire structure and risks naturalizing social pressure as biological necessity (d ≈ 0.72 for mountain perspective, which the engine flags as false summit). The derivation prioritizes beneficiary/victim declarations (victims: sleep_deprived_individual; beneficiaries: none declared, reflecting that extraction is not coordinating toward a beneficiary but dissipating through institutional overhead and productivity paradox) over power atoms, correctly identifying the constraint as pure extraction rather than hybrid coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH STRUCTURAL DECOMPOSITION: The mandatrophy risk is that sleep deprivation appears simultaneously as (a) a biological necessity (mountain) and (b) an institutional extraction mechanism (snare). The resolution is structural: the biological constraint (humans need sleep) is real and immutable. The deprivation cycle is not. Biological sleep need does not require synchronous workplace presence, extended hours, or suppression of recovery opportunities. The snare is not 'sleep loss' but 'the institutional arrangement that prevents recovery despite available time and resources.' These are different constraints: (1) Biological sleep requirement (mountain, ε ≤ 0.25), and (2) Sleep deprivation cycle enforced through workplace time pressure (snare, ε = 0.68). The false summit detector identifies the mountain perspective as naturalization of (2) onto (1). Mandatrophy is resolved by decomposing the natural law claim from the institutional extraction claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    acute_vs_chronic_extraction_boundary,
    'What timeline distinguishes legitimate acute sleep loss (short-term project urgency) from exploitative chronic deprivation?',
    'Longitudinal tracking of individual health outcomes; correlation between cumulative sleep debt duration and irreversible cognitive/physical damage; measurement of individual recovery capacity across deprivation durations',
    'If boundary < 2 weeks: many legitimate high-effort projects misclassified as extraction. If boundary > 12 weeks: chronic health damage is permitted before classification as exploitative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(acute_vs_chronic_extraction_boundary, empirical, 'Timeline boundary between acute and chronic sleep deprivation').

omega_variable(
    compensatory_recovery_availability,
    'Can individuals actually recover from chronic sleep deprivation through catch-up sleep, or is cumulative sleep debt irreversible?',
    'Controlled studies of recovery protocols post-deprivation; measurement of cognitive and physical performance restoration; assessment of whether weekend/vacation catch-up sleep reverses weekday deprivation effects',
    'If recovery is possible: suppression value decreases (exit through recovery becomes available). If recovery is limited/impossible: suppression increases (trapped exit confirmed at physiological level).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compensatory_recovery_availability, empirical, 'Whether chronic sleep debt permits recovery through catch-up sleep').

omega_variable(
    identity_locked_vs_constrained_boundary,
    'Is the sleep deprivation cycle maintained by identity fusion (professionals internalize hustle culture identity, measure self-worth through overwork) or by material economic constraints (job loss risk, income dependence)?',
    'Exit narrative analysis; measurement of whether individuals exit when economic constraints are removed; assessment of whether identity frame changes without economic incentive shift',
    'If identity-locked dominant: constraint operates as internalized norm even when exit barriers are removed. If constrained dominant: exit happens when economic dependency is severed. Classification shifts toward rope/scaffold if identity lock is primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_vs_constrained_boundary, conceptual, 'Whether sleep deprivation maintenance is identity-locked or materially constrained').

omega_variable(
    circadian_disruption_irreversibility,
    'Are circadian rhythm disruptions from sustained sleep deprivation permanent, or can adaptive schedules restore synchronization?',
    'Longitudinal measurement of melatonin timing, core body temperature rhythm, and cortisol curve across recovery period; assessment of whether 28-day adjustment period fully restores circadian phase',
    'If permanent: deprivation cycle creates irreversible biological damage, elevating snare classification confidence. If reversible: recovery pathway exists, potentially supporting scaffold perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(circadian_disruption_irreversibility, empirical, 'Permanence of circadian disruption from chronic sleep deprivation').

omega_variable(
    collective_action_paradox,
    'Why do sleep-deprived populations fail to organize collective refusal despite shared interest in exiting the deprivation cycle?',
    'Analysis of collective action barriers; measurement of cognitive capacity reduction sufficient to prevent organizing; identification of threshold where sleep loss impairs coordination ability below organizing capacity',
    'If cognitive impairment blocks organizing: snare classification confirmed (deprivation prevents exit even when exit would benefit all). If organizing fails for other reasons: classification may be tangled_rope with coordination dysfunction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_paradox, empirical, 'Why collective exit from sleep deprivation cycles fails despite aligned interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sleep_deprivation_cycle, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sleep_tr_t0, sleep_deprivation_cycle, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sleep_tr_t4, sleep_deprivation_cycle, theater_ratio, 4, 0.5).
narrative_ontology:measurement(sleep_tr_t8, sleep_deprivation_cycle, theater_ratio, 8, 0.58).
narrative_ontology:measurement(sleep_tr_t12, sleep_deprivation_cycle, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(sleep_be_t0, sleep_deprivation_cycle, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sleep_be_t4, sleep_deprivation_cycle, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(sleep_be_t8, sleep_deprivation_cycle, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(sleep_be_t12, sleep_deprivation_cycle, base_extractiveness, 12, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sleep_deprivation_cycle, resource_allocation).
narrative_ontology:affects_constraint(sleep_deprivation_cycle, cognitive_capacity_degradation).
narrative_ontology:affects_constraint(sleep_deprivation_cycle, workplace_time_discipline).
narrative_ontology:affects_constraint(sleep_deprivation_cycle, health_burden_accumulation).

% DUAL FORMULATION NOTE:
% The sleep deprivation cycle is downstream of workplace productivity demands and upstream of cumulative health damage. The three linked constraints share a common mechanism: temporal pressure creates sleep loss, which impairs capacity for resistance or alternative organizing, which deepens entrenchment. Each constraint has its own extractiveness value reflecting domain-specific measurement (cognitive tests for capacity, time logs for discipline, medical outcomes for health burden).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sleep_deprivation_cycle, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
