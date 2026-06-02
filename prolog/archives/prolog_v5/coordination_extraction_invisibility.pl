% ============================================================================
% CONSTRAINT STORY: coordination_extraction_invisibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coordination_extraction_invisibility, []).

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
 *   constraint_id: coordination_extraction_invisibility
 *   human_readable: Coordination-Extraction Invisibility in Hybrid Constraints
 *   domain: social_epistemology/structural_misclassification
 *
 * SUMMARY:
 *   The coordination-extraction invisibility constraint describes a
 *   meta-level structural dynamic in how hybrid constraints (tangled ropes)
 *   are experienced and classified by agents at different power positions.
 *   When a constraint provides genuine coordination benefits to agents with
 *   mobile exit options while simultaneously extracting from agents with
 *   trapped exit options, the agents experiencing coordination have no direct
 *   evidence of the extraction — their classification (rope) is positionally
 *   accurate but systemically incomplete. This creates a stable
 *   misclassification equilibrium: moderate-power agents advocate for the
 *   constraint based on their genuine coordination experience, while
 *   powerless agents bear extraction that remains invisible to those with
 *   power to change the system. The constraint is not a cognitive bias or
 *   perceptual failure — it is a structural feature of how hybrid constraints
 *   partition their functions across power gradients. The theater_ratio
 *   (0.58) reflects that much of the discourse about these constraints
 *   focuses on coordination benefits (visible, legible, easily defended)
 *   while extraction mechanisms remain under-discussed or attributed to
 *   individual failure rather than structural design. The extractiveness has
 *   increased over the interval (0.48 → 0.68) as the coordination narrative
 *   has become more entrenched, making it harder for powerless agents to make
 *   extraction claims legible to moderate-power observers.
 *
 * KEY AGENTS:
 *   - Powerless Agents: Primary victims (powerless/trapped) — bear concentrated extraction while their experience is invisible to agents with power to change the constraint; cannot exit, cannot organize effectively, cannot make extraction legible
 *   - Moderate Power Agents: Primary beneficiaries (moderate/mobile) — experience genuine coordination benefits with minimal extraction; their positionally accurate rope classification obscures extraction borne by others; not malicious but structurally privileged
 *   - Institutional Observers: Secondary beneficiaries (institutional/arbitrage) — benefit from both coordination function and extraction mechanism; shape constraint design; experience as pure coordination because extraction flows toward them
 *   - Organized Advocates: Secondary victims (organized/constrained) — can see hybrid structure but face credibility problem when moderate agents' genuine coordination experience contradicts extraction claims; constrained exit means cannot simply leave
 *   - Epistemic Commons: Abstract victim (powerless/trapped) — collective knowledge about constraint structure is contaminated by power-indexed classification variance; policy decisions based on moderate agents' rope experience perpetuate extraction on powerless agents
 *   - Analytical Observer: Meta-level position (analytical/analytical) — sees the structural partitioning mechanism but must explain why moderate agents' rope classification is both accurate and misleading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coordination_extraction_invisibility, 0.68).
domain_priors:suppression_score(coordination_extraction_invisibility, 0.72).
domain_priors:theater_ratio(coordination_extraction_invisibility, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coordination_extraction_invisibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(coordination_extraction_invisibility, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(coordination_extraction_invisibility, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coordination_extraction_invisibility, snare).
narrative_ontology:human_readable(coordination_extraction_invisibility, "Coordination-Extraction Invisibility in Hybrid Constraints").
narrative_ontology:topic_domain(coordination_extraction_invisibility, "social_epistemology/structural_misclassification").

domain_priors:requires_active_enforcement(coordination_extraction_invisibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coordination_extraction_invisibility, moderate_power_agents).
narrative_ontology:constraint_beneficiary(coordination_extraction_invisibility, institutional_observers).
narrative_ontology:constraint_victim(coordination_extraction_invisibility, powerless_agents).
narrative_ontology:constraint_victim(coordination_extraction_invisibility, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POWERLESS TARGET (SNARE) — Trapped within the constraint, bears full extraction while moderate-power agents experience the same structure as coordination. Cannot exit, cannot organize, and critically, cannot make their extraction visible to those with power to change the system. The invisibility mechanism is itself extractive: the constraint's hybrid structure allows beneficiaries to genuinely experience coordination benefits while extraction concentrates on agents with no voice in the classification process.
constraint_indexing:constraint_classification(coordination_extraction_invisibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MOBILE MODERATE (ROPE) — Experiences genuine coordination benefits with minimal extraction. Can exit if costs rise. This perspective is structurally accurate for THIS agent but epistemically incomplete: the moderate agent's genuine coordination experience obscures extraction borne by powerless agents at the same constraint. The rope classification is not false — it is positionally true but systemically misleading. This is the core mechanism: hybrid constraints partition their extraction and coordination functions across power gradients, making the extraction invisible from positions of relative privilege.
constraint_indexing:constraint_classification(coordination_extraction_invisibility, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL BENEFICIARY (ROPE) — Primary beneficiary of both the coordination function and the extraction mechanism. Experiences the constraint as pure coordination because extraction flows toward this agent, not away from them. Has arbitrage exit options and shapes the constraint's design. The institutional perspective's rope classification is genuine but structurally privileged: this agent benefits from the coordination AND from the extraction, but the extraction component is invisible because it manifests as 'efficiency gains' or 'natural sorting' rather than as coercion.
constraint_indexing:constraint_classification(coordination_extraction_invisibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIZED ADVOCATE (TANGLED ROPE) — Organized agents (advocacy groups, unions, reform coalitions) can see the hybrid structure that moderate and institutional agents miss. They experience both the coordination function (which they may support) and the extraction mechanism (which they oppose). Constrained exit options mean they cannot simply leave, but organization gives them voice. This perspective correctly classifies the constraint as tangled_rope, but faces the structural problem that moderate and institutional agents' genuine rope experience makes the extraction claim seem like motivated reasoning rather than structural observation.
constraint_indexing:constraint_classification(coordination_extraction_invisibility, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — The analytical perspective reveals the constraint's true hybrid structure: genuine coordination function coexisting with asymmetric extraction. Base extraction 0.68, suppression 0.72, but experienced as rope by agents with mobile exit options. The invisibility is not perceptual failure by moderate agents — it is structural partitioning. The constraint distributes its coordination benefits to agents with exit options and its extraction costs to agents without exit options, creating a power-indexed classification variance where both the rope and snare perspectives are positionally accurate. This is the diagnostic signature of a tangled_rope that appears as rope from positions of privilege.
constraint_indexing:constraint_classification(coordination_extraction_invisibility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURALIZATION (MOUNTAIN — FALSE SUMMIT) — Some institutional observers at civilizational time horizons naturalize the invisibility mechanism itself, treating power-indexed classification variance as an inherent feature of complex social systems rather than as a contingent structural arrangement. This perspective sees the coordination-extraction partition as inevitable: 'some agents will always bear more costs in any coordination system.' The mountain classification is a false summit — it naturalizes what is actually a designed feature of hybrid constraints that could be structured differently.
constraint_indexing:constraint_classification(coordination_extraction_invisibility, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coordination_extraction_invisibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coordination_extraction_invisibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coordination_extraction_invisibility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(coordination_extraction_invisibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coordination_extraction_invisibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts significantly from powerless agents (career costs, economic costs, opportunity costs, dignity costs) while providing coordination benefits to moderate and institutional agents. The high extractiveness reflects that the coordination function could theoretically be provided with much lower extraction — the current level represents structural design choices that concentrate costs on agents with no exit options. The value is below the 0.70 mandatrophy threshold because some of the measured extraction is genuinely necessary coordination cost (the constraint does solve a real coordination problem), but the majority is asymmetric extraction enabled by power gradients. Suppression (0.72): High. Powerless agents face severe barriers to exit: economic dependency, geographic constraints, skill mismatches, legal barriers, and critically, the invisibility mechanism itself (moderate agents' genuine coordination experience makes extraction claims seem implausible, blocking collective action). The suppression is not total (some agents do exit, some do organize) but is severe enough to maintain the extraction equilibrium. Theater ratio (0.58): Moderate-high. Much of the public discourse about hybrid constraints focuses on coordination benefits (which are real and visible) while extraction mechanisms are under-discussed, attributed to individual failure, or dismissed as necessary costs. The theater has increased over time as the coordination narrative has become institutionally entrenched, making it harder to discuss extraction without appearing to oppose coordination itself.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the most diagnostically significant perspectival gap in the DR system: the gap between powerless/trapped (snare) and moderate/mobile (rope) perspectives on the same hybrid constraint. The moderate agent's rope classification is not false — they genuinely experience coordination with minimal extraction. The powerless agent's snare classification is not false — they genuinely experience extraction with minimal coordination benefit. Both are positionally accurate. The gap exists because the constraint partitions its functions across power gradients: coordination benefits flow to agents with exit options (who can leave if extraction rises), extraction costs concentrate on agents without exit options (who cannot leave and thus can be safely extracted from). The institutional perspective (rope) is even more privileged — these agents benefit from both functions. The organized perspective (tangled_rope) correctly identifies the hybrid structure but faces a credibility problem: when moderate agents report genuine coordination experience, extraction claims seem like motivated reasoning rather than structural observation. The analytical perspective (tangled_rope) reveals that the invisibility is not a bug but a feature: hybrid constraints that successfully partition their functions across power gradients are stable precisely because beneficiaries have no direct evidence of extraction and thus no reason to support reform. The naturalization perspective (mountain — false summit) treats this partitioning as inevitable rather than as a contingent design choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values reveal the constraint's core mechanism: power-indexed partitioning of coordination and extraction functions. Powerless agents are victims with trapped exit options, yielding high d (approaching 1.0) and maximum experienced extraction. Moderate agents are beneficiaries with mobile exit options, yielding low d (approaching 0.15) and minimal or negative experienced extraction — they genuinely experience coordination. Institutional agents are primary beneficiaries with arbitrage exit options, yielding very low d (approaching 0.05) and strongly negative experienced extraction — they benefit from both the coordination function and the extraction mechanism. Organized agents are victims (they oppose the extraction) but with constrained rather than trapped exit options, yielding moderate-high d (around 0.55-0.65) — they see the hybrid structure but cannot easily exit. The analytical observer has canonical d for the analytical power atom (around 0.72), experiencing the constraint as an object of study rather than as a direct cost or benefit. The perspectival gap between powerless (snare) and moderate (rope) agents is not a disagreement about facts — both perspectives are structurally accurate for their respective positions. The gap IS the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY UNRESOLVED: This constraint sits at the boundary of the mandatrophy (extractiveness 0.68, just below the 0.70 threshold) and demonstrates why the mandatrophy exists. Is this a tangled_rope (genuine coordination function coexisting with asymmetric extraction, both structurally necessary) or a snare (extraction mechanism with a coordination cover story, functions are separable)? The answer depends on omega variable 4: whether the coordination function requires the extraction mechanism or whether the extraction is parasitic. If the coordination could be preserved with much lower extraction (e.g., by redistributing costs across all beneficiaries rather than concentrating them on powerless agents), then the constraint is a snare from the analytical perspective despite moderate agents' genuine rope experience. If the coordination function structurally requires that costs concentrate on agents with trapped exit options (e.g., because the coordination depends on having a stable, immobile population to bear adjustment costs), then the constraint is a tangled_rope from the analytical perspective. The current classification (snare) reflects the hypothesis that most hybrid constraints showing this invisibility pattern could be redesigned to distribute costs more evenly, but this hypothesis is empirically unresolved. The mandatrophy is not an error in the classification system — it is a structural feature of constraints where coordination and extraction are deeply entangled and the boundary between 'necessary coordination cost' and 'extractive overhead' is empirically ambiguous.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    visibility_threshold,
    'At what power level does extraction become visible to the agent experiencing the constraint?',
    'Empirical measurement of Type III error rates (tangled_rope misclassified as rope) across power positions; identification of the power threshold where agents begin to report extraction alongside coordination benefits',
    'If threshold is at ''organized'' level: only collective action makes extraction visible. If threshold is at ''moderate'' level: individual mobility is sufficient. If threshold is above ''institutional'': even powerful agents cannot see extraction they benefit from.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(visibility_threshold, empirical, 'Power threshold for extraction visibility in hybrid constraints').

omega_variable(
    exit_option_causality,
    'Does mobile exit cause coordination experience, or does coordination experience cause exit options to appear mobile?',
    'Longitudinal tracking of agents whose exit options change (job loss, geographic mobility, skill acquisition) while the constraint remains constant; measurement of whether classification changes with exit options or remains stable',
    'If exit causes coordination experience: the invisibility is structural (agents with exit genuinely experience less extraction). If coordination experience causes perceived exit: the invisibility is cognitive (agents experiencing coordination believe they have exit options they may not actually have).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_causality, empirical, 'Causal direction between exit options and coordination experience').

omega_variable(
    extraction_concentration_mechanism,
    'Is extraction concentrated on powerless agents through active targeting or through passive sorting (agents with exit options leave, agents without exit remain)?',
    'Comparison of extraction rates in constraints with high vs low exit barriers; analysis of whether extraction increases when exit is blocked or whether it remains constant and only the agent population changes',
    'If active targeting: the constraint is designed to extract from powerless agents (snare from analytical perspective). If passive sorting: the constraint is neutral but interacts with power gradients to produce extractive outcomes (tangled_rope from analytical perspective, but with different intervention points).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_concentration_mechanism, empirical, 'Mechanism of extraction concentration on powerless agents').

omega_variable(
    coordination_function_necessity,
    'Could the coordination function be preserved while eliminating the extraction mechanism, or is the extraction structurally necessary for the coordination to work?',
    'Design and testing of alternative constraint structures that provide the same coordination benefits with lower extraction; measurement of whether coordination quality degrades when extraction is reduced',
    'If coordination requires extraction: the constraint is genuinely tangled (inseparable functions). If coordination can be preserved: the constraint is a snare with a coordination cover story (separable functions, extraction is parasitic rather than structural).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_necessity, conceptual, 'Structural necessity of extraction for coordination function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coordination_extraction_invisibility, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coord_extract_invis_tr_t0, coordination_extraction_invisibility, theater_ratio, 0, 0.35).
narrative_ontology:measurement(coord_extract_invis_tr_t3, coordination_extraction_invisibility, theater_ratio, 3, 0.42).
narrative_ontology:measurement(coord_extract_invis_tr_t6, coordination_extraction_invisibility, theater_ratio, 6, 0.5).
narrative_ontology:measurement(coord_extract_invis_tr_t9, coordination_extraction_invisibility, theater_ratio, 9, 0.55).
narrative_ontology:measurement(coord_extract_invis_tr_t12, coordination_extraction_invisibility, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(coord_extract_invis_be_t0, coordination_extraction_invisibility, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(coord_extract_invis_be_t3, coordination_extraction_invisibility, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(coord_extract_invis_be_t6, coordination_extraction_invisibility, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(coord_extract_invis_be_t9, coordination_extraction_invisibility, base_extractiveness, 9, 0.65).
narrative_ontology:measurement(coord_extract_invis_be_t12, coordination_extraction_invisibility, base_extractiveness, 12, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coordination_extraction_invisibility, identity_coordination).
narrative_ontology:affects_constraint(coordination_extraction_invisibility, power_indexed_classification_variance).

% DUAL FORMULATION NOTE:
% This constraint is downstream of power_indexed_classification_variance (the mountain-level observation that classification varies with observer power position) and represents the specific mechanism by which that variance produces stable misclassification equilibria. The upstream constraint establishes that perspectival gaps exist; this constraint explains why those gaps persist and why they are invisible from positions of relative privilege. Both constraints are part of the social epistemology constraint family, but they operate at different levels of abstraction: the upstream constraint is a meta-level observation about the classification system itself, while this constraint is an object-level mechanism within specific hybrid constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
