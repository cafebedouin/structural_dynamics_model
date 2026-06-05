% ============================================================================
% CONSTRAINT STORY: consensus_without_truth
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_consensus_without_truth, []).

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
 *   constraint_id: consensus_without_truth
 *   human_readable: The Social Cohesion Mirage
 *   domain: social/political/informational
 *
 * SUMMARY:
 *   The Social Cohesion Mirage describes a structural constraint that emerges
 *   when a community achieves total agreement on a shared narrative that is
 *   factually false or decoupled from physical reality. This constraint is a
 *   diagnostic exemplar of how institutional coordination mechanisms can
 *   invert: what appears to be a coordination solution (achieving consensus)
 *   becomes an extraction mechanism (suppressing reality-aligned dissent)
 *   because the consensus is maintained not by making the narrative true but
 *   by making dissent costly. The constraint exhibits a perverse stability:
 *   the beneficiaries of the false narrative (institutional gatekeepers,
 *   narrative sponsors) have strong incentives to enforce belief, while the
 *   victims (reality-aligned agents, the epistemic commons, future planners)
 *   bear the cost of misalignment between collective action and physical
 *   reality. Theater ratio (0.81) indicates that consensus enforcement is
 *   substantially performative — repetition of the narrative, ritual
 *   affirmation, authority invocation — rather than grounded in evidence or
 *   calibration. The constraint's evolution over the interval shows both
 *   metrics rising, suggesting a pattern of deepening narrative entrenchment
 *   combined with increasing performative enforcement overhead (Goodhart
 *   drift). The analytical risk is that observers may naturalize this pattern
 *   as inevitable ('humans are consensus-seeking') when the constraint is
 *   actually a contingent institutional arrangement that could be unwound.
 *
 * KEY AGENTS:
 *   - Reality-Aligned Observer: Primary victim (powerless/trapped) — cannot exit without severe social cost; bears full cost of narrative-reality misalignment
 *   - Marginalized Dissident: Secondary victim (moderate/constrained) — knows narrative is false; faces institutional barriers to truth-telling but retains some exit capacity
 *   - Local Authority/Opinion Leader: Secondary actor (institutional/constrained) — enforces consensus through social norm management; benefits from harmony but constrained by need for legitimacy
 *   - Institutional Narrative Sponsor: Primary beneficiary (institutional/arbitrage) — captures legitimacy and resource flows from consensus; experiences constraint as low-cost coordination
 *   - Reality-Based Coalition: Organized agent (organized/mobile) — building epistemic counter-institutions (fact-checking, alternative media, verification networks); sees sunset pathway
 *   - Institutional Maintenance Class: Tertiary actor (institutional/arbitrage) — perpetuates narrative through bureaucratic routine; perceives own mechanism as degraded (piton perspective)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent constraint as immutable law of human nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(consensus_without_truth, 0.58).
domain_priors:suppression_score(consensus_without_truth, 0.68).
domain_priors:theater_ratio(consensus_without_truth, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(consensus_without_truth, extractiveness, 0.58).
narrative_ontology:constraint_metric(consensus_without_truth, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(consensus_without_truth, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(consensus_without_truth, snare).
narrative_ontology:human_readable(consensus_without_truth, "The Social Cohesion Mirage").
narrative_ontology:topic_domain(consensus_without_truth, "social/political/informational").

domain_priors:requires_active_enforcement(consensus_without_truth).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(consensus_without_truth, narrative_beneficiaries).
narrative_ontology:constraint_beneficiary(consensus_without_truth, institutional_gatekeepers).
narrative_ontology:constraint_victim(consensus_without_truth, epistemic_commons).
narrative_ontology:constraint_victim(consensus_without_truth, reality_alignment_agents).
narrative_ontology:constraint_victim(consensus_without_truth, future_planners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REALITY-ALIGNED OBSERVER (SNARE) — Cannot exit the consensus without severe social cost (ostracism, career damage, isolation). Bears full cost of misalignment between narrative and reality. Suppression is maximal: dissent is defined as heresy/dangerousness. d≈0.93, f(d)≈1.40, σ=0.9 → χ≈0.72.
constraint_indexing:constraint_classification(consensus_without_truth, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MARGINALIZED DISSIDENT (SNARE) — Knows the narrative is false but faces institutional barriers (loss of employment, community standing, family relationships) to speaking truth. Exit is costly but not impossible; some dissidents do leave or build counter-communities. d≈0.82, f(d)≈1.25, σ=0.9 → χ≈0.62.
constraint_indexing:constraint_classification(consensus_without_truth, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LOCAL AUTHORITY (TANGLED ROPE) — Benefits from enforcing consensus (social stability, reputation, control); constrained because deviation risks losing institutional legitimacy. Experiences mixed extraction (benefits from harmony) and coercion (must enforce narrative). d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.39.
constraint_indexing:constraint_classification(consensus_without_truth, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: INSTITUTIONAL NARRATIVE SPONSOR (ROPE) — Benefits enormously from consensus (legitimacy, control, resource flows). Experiences constraint as pure coordination: maintaining the narrative requires minimal coercive overhead because believers do the enforcement work themselves. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.08. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(consensus_without_truth, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REALITY-BASED COALITION (SCAFFOLD) — Organized epistemic agents (researchers, fact-checkers, alternative media) building verification pathways and exit routes. See the consensus constraint as temporary — as information access improves and people discover contradictions with lived experience, the false narrative loses force. d≈0.45, f(d)≈0.45, σ=1.2 → χ≈0.25. Low effective extraction due to coalition agency.
constraint_indexing:constraint_classification(consensus_without_truth, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INSTITUTIONAL MAINTENANCE CLASS (PITON) — Bureaucrats, enforcers, educators who perpetuate the consensus narrative through institutional routine (curriculum, policy, appointment criteria). The mechanism is largely theatrical: repetition, ritual, authority invocation. Theater ratio = 0.81 (high — the enforcement is performative, not calibrated to reality). These actors see the constraint as degraded: they know the narrative is false but maintain it because alternatives haven't replaced it. d≈0.50, f(d)≈0.65, σ=0.9 → χ≈0.38.
constraint_indexing:constraint_classification(consensus_without_truth, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION RISK (MOUNTAIN) — Risk of falsely classifying the consensus as immutable natural law ('humans are tribal/consensus-seeking by nature'). Emergentist argument: shared meaning requires agreement, therefore some consensus-building is inevitable. CRITICAL: This perspective is a false summit. Base properties (ε=0.58, suppression=0.68, theater=0.81) contradict mountain thresholds (ε≤0.25, suppression≤0.05). The engine's false natural law detector will flag this.
constraint_indexing:constraint_classification(consensus_without_truth, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(consensus_without_truth_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(consensus_without_truth, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consensus_without_truth, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(consensus_without_truth, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(consensus_without_truth, TR),
    TR >= 0.70.

:- end_tests(consensus_without_truth_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The false narrative generates significant extraction from reality-aligned agents (suppression of truth, enforced conformity, behavioral alignment with false worldview). But extraction is not as severe as pure predatory snares (0.70+) because some exit routes exist (intellectual dissident communities, emigration, counter-institutions). The measurement trend (0.38→0.58) shows increasing extraction as enforcement deepens. Suppression (0.68): High. Dissent is actively discouraged through social ostracism, institutional penalties, and rhetorical delegitimization. Alternative information sources are degraded or blocked. Exit costs are substantial. Theater ratio (0.81): Very high. Consensus enforcement is substantially theatrical — repetition, ritual, authority appeals rather than empirical calibration. The trend (0.52→0.81) shows rising theater, indicating that maintaining the false narrative requires increasing performative overhead (Goodhart drift). The constraint is claimed as Snare because: (1) effective extraction χ = 0.58 × f(d) × σ for powerless victim yields χ≈0.72 (high); (2) suppression is severe (0.68); (3) at least one victim group (epistemic commons) has no exit option.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is acute and diagnostic. The narrative sponsor sees Rope (pure coordination with minimal enforcement cost) while the powerless observer sees Snare (suppression with no exit). The institutional maintenance class sees Piton (degraded ritual) while the reality-aligned coalition sees Scaffold (temporary constraint with escape velocity). The local authority sees Tangled Rope (mixed benefits and constraints) because they experience both the stability benefits and the moral cost of enforcing falsehood. The analytical observer risks Mountain (naturalizing as inevitable) but the structural data contradicts this — consensus without truth requires active suppression and theater, not passive emergence. The seven perspectives together show that this is a genuine Snare from the primary target's view, but appears as coordination (Rope) from the primary beneficiary's view. This gap is the constraint's diagnostic signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Reality-aligned observer: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction burden. Cannot exit consensus without catastrophic social cost. Marginalized dissident: Victim + constrained → d≈0.82, f(d)≈1.25. High extraction; some exit possible through dissent communities. Local authority: Neither beneficiary nor pure victim (mixed role) + constrained → d≈0.55, f(d)≈0.75. Moderate extraction because authority benefits from consensus but constrained by legitimacy requirements. Institutional narrative sponsor: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Minimal extraction cost; net beneficiary. Reality-based coalition: Organized + mobile → d≈0.45, f(d)≈0.45. Low effective extraction due to coalition agency and exit options. Institutional maintenance class: Beneficiary (through institutional position) + arbitrage but aware of degradation → d≈0.50, f(d)≈0.65. Moderate extraction with performative character. Analytical observer: Analytical perspective → d≈0.72, f(d)≈1.15 for naturalization risk. Engine's false natural law detector applies.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint is claimed as Snare (not Rope or Tangled Rope) because the false narrative provides minimal coordination benefit and requires active suppression. A true Rope or Tangled Rope would solve a genuine collective action problem (e.g., achieving agreement on safety standards, information protocols). This constraint does NOT solve a collective action problem — it CREATES extraction by decoupling agreement from reality. The false narrative may feel coordinating to its believers, but this is theatrical coordination: it aligns action in the short term while misaligning action from physical reality in the long term, creating deferred extraction costs. The mandatrophy is resolved by distinguishing: (1) Rope: consensus on factually accurate coordination mechanism (e.g., agreed safety protocols). (2) Consensus Without Truth (Snare): consensus on false narrative maintained by suppression. The difference is empirical testability and extraction structure. The false narrative extracts from reality-aligned agents and the epistemic commons because it requires them to either conform (suppressing true knowledge) or exit (incurring social cost). A genuine coordination mechanism would not require this choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feedback_loop_escape_velocity,
    'At what point does a false consensus become self-perpetuating regardless of external reality?',
    'Historical case analysis: societies where false narratives collapsed (USSR economic statistics, Nazi racial science, tobacco denial). Identify the tipping point where reality alignment became irrecoverable vs. where institutional collapse preceded epistemic recovery.',
    'If escape velocity is low (< 2 generations): consensus can flip quickly when reality contradicts it (Scaffold perspective valid). If escape velocity is high (> 3 generations): false consensus can persist indefinitely (Snare is permanent, not temporary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feedback_loop_escape_velocity, empirical, 'Self-perpetuation threshold for false consensus').

omega_variable(
    enforcement_vs_belief_asymmetry,
    'Does consensus enforcement require active coercion or do believers enforce the narrative themselves, reducing the energy cost to sponsors?',
    'Measurement of enforcement resource allocation: proportion of institutional budget devoted to actively punishing dissent vs. passive norm enforcement through socialization. Survey data on whether dissidents cite direct punishment or social isolation as primary barrier.',
    'If self-enforcing: extractiveness is lower (beneficiary still benefits, but without paying coercion costs) — Rope classification more defensible. If externally enforced: extractiveness is higher and suppression is more severe — Snare classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_vs_belief_asymmetry, empirical, 'Degree of self-enforcement vs active coercion in consensus').

omega_variable(
    reality_contact_degradation,
    'Does maintaining a false consensus require progressive degradation of reality contact (isolation, information control, institutional restructuring to prevent empirical contradiction)?',
    'Historical pattern analysis: does false consensus require increasing institutional opacity, information silos, or re-definition of reality metrics (moving from objective measures to subjective satisfaction/belief measures)?',
    'If yes: false consensus becomes increasingly expensive to maintain, suggesting temporary nature (Scaffold). If no: false consensus can coexist indefinitely with selective reality contact (Snare permanent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reality_contact_degradation, empirical, 'Whether false consensus requires progressive reality isolation').

omega_variable(
    alternative_narrative_viability,
    'Are there organizationally viable alternative narratives ready to replace the false consensus, or must reality alignment build institutions from scratch?',
    'Institutional readiness assessment: do counter-communities, fact-based media, or reality-aligned institutions have sufficient resources and reach to offer exit routes? Capacity analysis of alternative institutions.',
    'If viable alternatives exist: scaffold sunset is realistic, reality-based coalition can succeed. If alternatives must be built: exit is harder, snare may be more durable than scaffold suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_narrative_viability, empirical, 'Availability of viable alternative narratives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(consensus_without_truth, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(consensus_tr_t0, consensus_without_truth, theater_ratio, 0, 0.52).
narrative_ontology:measurement(consensus_tr_t2, consensus_without_truth, theater_ratio, 2, 0.67).
narrative_ontology:measurement(consensus_tr_t5, consensus_without_truth, theater_ratio, 5, 0.81).

% Extraction over time
narrative_ontology:measurement(consensus_be_t0, consensus_without_truth, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(consensus_be_t2, consensus_without_truth, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(consensus_be_t5, consensus_without_truth, base_extractiveness, 5, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(consensus_without_truth, information_standard).
narrative_ontology:affects_constraint(consensus_without_truth, institutional_narrative_capture).
narrative_ontology:affects_constraint(consensus_without_truth, epistemic_commons_degradation).
narrative_ontology:affects_constraint(consensus_without_truth, reality_contact_breakdown).

% DUAL FORMULATION NOTE:
% The Social Cohesion Mirage decomposes into three distinct structural constraints: (1) Institutional Narrative Capture (how narratives become entrenched in institutions); (2) Epistemic Commons Degradation (how shared knowledge infrastructure fails when consensus replaces truth); (3) Reality Contact Breakdown (progressive isolation from contradictory evidence). All three are downstream of the consensus constraint and share its suppression mechanism, but each has distinct ε values reflecting different extraction pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(consensus_without_truth, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
