% ============================================================================
% CONSTRAINT STORY: emotional_cycles_of_change
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emotional_cycles_of_change, []).

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
 *   constraint_id: emotional_cycles_of_change
 *   human_readable: The Kelley-Connor Cycle of Change
 *   domain: psychology/behavioral_science
 *
 * SUMMARY:
 *   The Kelley-Connor Cycle of Change describes the emotional trajectory of
 *   individuals and organizations adopting new systems, practices, or
 *   technologies. The model identifies five stages: Uninformed Optimism
 *   (enthusiasm before encountering real constraints), Informed Pessimism
 *   (realization of difficulty), Valley of Despair (emotional nadir where old
 *   competence is lost and new competence not yet gained), Informed Optimism
 *   (breakthrough as new skills accumulate), and Success/Completion
 *   (integration and mastery). This constraint exhibits a fundamental
 *   structural tension: change adoption requires coordination (new systems do
 *   enable new capabilities, new processes do improve outcomes) but imposes
 *   an asymmetric emotional and productivity cost primarily borne by adopters
 *   during the valley phase. The constraint's extractiveness rises from 0.15
 *   to 0.38 over the implementation interval, reflecting increasing
 *   realization that the valley phase is neither temporary nor avoidable, but
 *   rather an institutionalized cost transfer. The theater ratio (0.48 at
 *   endpoint) indicates that a moderate portion of adoption activity is
 *   performative—compliance reporting, checkbox training, surface-level
 *   system use alongside continued legacy workflows—rather than genuine
 *   capability change. This is not cynicism but structural: organizations
 *   often treat adoption as a project to complete rather than a capability to
 *   build, enabling theater to persist.
 *
 * KEY AGENTS:
 *   - Individual adopters (staff, employees, learners): Primary victims (powerless/trapped) — bear full emotional cost of valley phase without exit option; competence loss precedes competence gain
 *   - Organizational change advocates (leadership, change management): Primary beneficiaries (institutional/arbitrage) — capture efficiency gains and capability uplift after valley; can exit constraint (move to next change initiative) while staff remain trapped
 *   - Middle managers: Secondary actors (moderate/constrained) — caught between mandate to implement change and staff emotional distress; manage both coordination and suppression
 *   - Training and support programs: Organized actors (organized/constrained) — provide coordination infrastructure; structured with sunset (training cohort completes) and measurable outcomes
 *   - Legacy system infrastructure: Institutional inertia (institutional/mobile) — persists in parallel due to technical debt; represents performative compliance with new system while real work continues in old tools
 *   - Change management frameworks (like Kelley-Connor itself): Analytical tool (analytical/analytical) — linguistic framing that normalizes valley phase and may provide genuine coordination or may be primarily reassuring theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emotional_cycles_of_change, 0.38).
domain_priors:suppression_score(emotional_cycles_of_change, 0.52).
domain_priors:theater_ratio(emotional_cycles_of_change, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emotional_cycles_of_change, extractiveness, 0.38).
narrative_ontology:constraint_metric(emotional_cycles_of_change, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(emotional_cycles_of_change, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emotional_cycles_of_change, tangled_rope).
narrative_ontology:human_readable(emotional_cycles_of_change, "The Kelley-Connor Cycle of Change").
narrative_ontology:topic_domain(emotional_cycles_of_change, "psychology/behavioral_science").

domain_priors:requires_active_enforcement(emotional_cycles_of_change).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emotional_cycles_of_change, change_advocates).
narrative_ontology:constraint_beneficiary(emotional_cycles_of_change, system_designers).
narrative_ontology:constraint_victim(emotional_cycles_of_change, individual_adopters).
narrative_ontology:constraint_victim(emotional_cycles_of_change, organizational_staff).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINFORMED ADOPTER IN VALLEY OF DESPAIR (SNARE) — Individual or team member forced into new system adoption without adequate preparation. Experiences full emotional cost of the valley phase (stage 3) without exit option. Trapped between old competence (now deprecated) and new incompetence (not yet mastered). d≈0.92, f(d)≈1.40, σ=0.8 → χ≈0.41.
constraint_indexing:constraint_classification(emotional_cycles_of_change, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ORGANIZATIONAL MIDDLE MANAGER (TANGLED ROPE) — Constrained by both top-down mandate to implement change and bottom-up staff resistance. Experiences genuine coordination benefit (new system enables new capabilities) but also bears suppressive burden of managing emotional distress and productivity dips during valley phase. d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.38.
constraint_indexing:constraint_classification(emotional_cycles_of_change, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CHANGE MANAGEMENT CONSULTANT (ROPE) — Institutional actor with arbitrage exit (can move to next client). Experiences the cycle as a coordination problem to solve through framework communication and emotional coaching. The consultant's model (Kelley-Connor itself) is a coordination mechanism: naming the emotional stages gives adopters linguistic tools to normalize despair and persist through it. d≈0.10, f(d)≈0.02, σ=1.2 → χ≈0.01. Near-zero effective extraction.
constraint_indexing:constraint_classification(emotional_cycles_of_change, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRAINING PROGRAM WITH SUNSET (SCAFFOLD) — Organized support structures (training cohorts, peer groups, temporary skill-building resources) frame the change as temporary coordination with a clear endpoint. Suppression (mandatory training attendance) is tolerated because the program has a defined completion date and demonstrable competence milestone. Theater is low (0.35 at program level) because outcomes are measured. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.13. Low effective extraction because the coordination has agency and sunset logic.
constraint_indexing:constraint_classification(emotional_cycles_of_change, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY SYSTEM HOLDOUT (PITON) — Institutional adoption of a new system where the old system persists in parallel due to technical debt or staff inertia. Performs compliance with new system while maintaining old workflows. Theater ratio (0.61) reflects that much adoption activity is performative (generating required reports in new system while real work continues in legacy tools). The constraint persists through institutional inertia despite degraded function.
constraint_indexing:constraint_classification(emotional_cycles_of_change, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Civilizational view reveals that the Kelley-Connor cycle describes a genuine coordination function (systems do enable new capabilities) AND an asymmetric extraction (emotional labor is externalized to adopters as an unpaid cost). The cycle is not a natural law of psychology but a structural feature of how change adoption is institutionally managed. d≈0.65, f(d)≈1.00, σ=1.2 → χ≈0.46.
constraint_indexing:constraint_classification(emotional_cycles_of_change, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emotional_cycles_of_change_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(emotional_cycles_of_change, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emotional_cycles_of_change, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(emotional_cycles_of_change, TR),
    TR >= 0.70.

:- end_tests(emotional_cycles_of_change_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. The constraint extracts emotional labor from adopters during the valley phase (stage 3). This is not a temporary discomfort but a structured cost transfer: adopters lose old competence (productivity collapse) before gaining new competence (productivity recovery delayed 6-12 months depending on system complexity). Organizations benefit from the new system's capabilities without proportionally bearing the emotional/productivity cost. The extractiveness value reflects that this transfer is somewhat normalized and expected, but it remains a genuine asymmetric cost. Suppression (0.52): Moderate-high. Multiple barriers prevent adopters from exiting the valley: (a) mandatory adoption policies, (b) lack of legitimate alternative workflows (old system often deprecated), (c) career/employment consequences for lagging adoption, (d) organizational narrative framing adoption as non-negotiable change. The suppression rises as the cycle progresses. Theater ratio (0.48 at endpoint, 0.35 initially): Moderate. Some adoption activity is performative: checkbox completion of training, compliance reports showing system use while real work continues in legacy tools, performative enthusiasm during organizational change communication. However, theater is not dominant because the new system does deliver real capability gains eventually, and the valley phase involves genuine psychological distress (not merely theater distress). Theater rises over the interval as organizations move from genuine learning (phase 0-2) to surface-level compliance (phase 3-4).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The powerless adopter sees a snare: mandatory participation in an emotionally destructive process with no exit, no choice, and no compensation. The beneficiary sees a rope: the new system solves genuine coordination problems and enables better outcomes; the emotional cost is framed as a necessary price for progress. The consultant sees a rope: their model (Kelley-Connor) is a linguistic coordination tool—naming the valley normalizes it and helps adopters persist. The middle manager sees tangled rope: they genuinely need the new system's coordination benefits but also bear the burden of managing staff distress. The legacy system sees a piton: the new system is officially adopted but old workflows continue due to inertia and technical debt. The analytical observer sees tangled rope with institutional extraction: the cycle legitimately enables coordination but institutions systematize the emotional cost asymmetry, making what could be a shared coordination burden into an externalized cost transfer. These gaps are not observational ambiguity—they reflect real differences in structural position and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual adopters: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. No exit option, forced participation, bears full emotional cost. Organizational change advocates: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Can exit (move to next initiative) while staff remain trapped in valley. Middle managers: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction. Caught between mandate and staff suffering; cannot refuse implementation but cannot escape responsibility for team emotional management. Training programs: Organized + constrained → d≈0.35, f(d)≈0.35. Low-moderate extraction. Constrained by organizational mandate but possess agency and structure (sunset, measurable outcomes). Change management consultant: Beneficiary + arbitrage → d≈0.10, f(d)≈0.02. Near-zero extraction. Institutional position with easy exit. Legacy system: Institutional + mobile → d≈0.15, f(d)≈0.05. Low extraction; persists through inertia rather than active extraction. Analytical observer: d≈0.65, f(d)≈1.00. Moderate extraction; reveals that the cycle is a structural feature of institutional change management, not a natural law of psychology.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by disambiguating between two structurally distinct claims: (1) Change adoption has an inherent emotional cost (psychological law, mountain-like), and (2) organizations systematically externalize this cost to adopters (institutional extraction, snare-like). Claim 1 is partially true and mountain-like: complex learning does involve phases of incompetence and emotional difficulty. Claim 2 is empirically controversial but structurally true: organizations often treat valley-phase costs as non-negotiable rather than minimizable through institutional investment. The tangled_rope classification reflects that the constraint serves genuine coordination (new systems do enable capabilities) AND extracts emotional labor asymmetrically. The false summit would be calling this a mountain—'the valley is just how human psychology works'—because that naturalizes what is a contingent institutional choice (depth of valley, duration, support level, compensation). The constraint becomes a rope (or scaffold) to the extent that organizations intentionally minimize valley depth through robust training, peer support, temporary workload reduction, or compensation for emotional labor. It becomes a snare to the extent that these supports are absent and valley-phase productivity loss is absorbed without mitigation. The Kelley-Connor model itself is instrumentally interesting: it can either increase awareness and empathy (rope outcome) or provide performative reassurance while perpetuating extraction (piton outcome). This depends on whether naming the cycle changes institutional behavior toward support or merely normalizes the cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    valley_depth_variability,
    'What determines the depth and duration of the Valley of Despair for different systems and populations?',
    'Longitudinal studies of adoption cohorts; comparison of valley metrics across system types (software, manufacturing process, educational reform); analysis of pre-training, coaching intensity, and peer support effects',
    'If depth is primarily system-determined: the cycle is largely fixed and unavoidable (mountain-like). If depth is adoption-management-determined: the valley can be engineered down, and the suppression score is contingent on institutional investment choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(valley_depth_variability, empirical, 'Determinants of Valley of Despair severity and duration').

omega_variable(
    emotional_externality_quantification,
    'Is the emotional cost borne by adopters a hidden transfer payment from staff to organization, or is it a genuine shared cost of any complex coordination?',
    'Measurement of productivity loss, burnout rates, health impacts during valley phase; comparison with adoption approaches that distribute emotional labor (peer-led vs top-down); policy analysis of whether organizations compensate for valley-phase costs',
    'If externalized: snare classification is correct for adopters; organization benefits from unpaid emotional labor. If shared: tangled_rope is correct; all parties bear some cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emotional_externality_quantification, empirical, 'Whether emotional costs are externalized or shared').

omega_variable(
    linguistic_framing_efficacy,
    'Does naming the Kelley-Connor cycle (linguistic framing) materially reduce the severity of the valley phase, or is it primarily theatrical reassurance?',
    'RCT or quasi-experiment: adoption cohorts with cycle training vs. cohorts without; measurement of psychological distress, persistence rates, productivity recovery timeline; analysis of whether named vs. unnamed emotional stages produce different outcomes',
    'If naming is efficacious: the framework is a genuine coordination tool (rope). If theater is substantial: the framework is partly performative (piton component), and institutional suppression persists regardless of framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(linguistic_framing_efficacy, empirical, 'Whether naming the cycle reduces valley severity or is performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emotional_cycles_of_change, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emot_tr_t0, emotional_cycles_of_change, theater_ratio, 0, 0.35).
narrative_ontology:measurement(emot_tr_t2, emotional_cycles_of_change, theater_ratio, 2, 0.45).
narrative_ontology:measurement(emot_tr_t4, emotional_cycles_of_change, theater_ratio, 4, 0.48).

% Extraction over time
narrative_ontology:measurement(emot_be_t0, emotional_cycles_of_change, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(emot_be_t2, emotional_cycles_of_change, base_extractiveness, 2, 0.32).
narrative_ontology:measurement(emot_be_t4, emotional_cycles_of_change, base_extractiveness, 4, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emotional_cycles_of_change, resource_allocation).
narrative_ontology:affects_constraint(emotional_cycles_of_change, organizational_capability_debt).
narrative_ontology:affects_constraint(emotional_cycles_of_change, staff_burnout_externality).

% DUAL FORMULATION NOTE:
% The Kelley-Connor cycle decomposes into two structurally distinct claims: (1) psychological inevitability of emotional phases during learning (mountain-like, ε≈0.08), and (2) organizational cost externalization during adoption (institutional extraction, ε≈0.38). This story addresses claim 2—the institutional structure and suppression dynamics. The upstream natural learning phases (claim 1) would constitute a separate mountain constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(emotional_cycles_of_change, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
