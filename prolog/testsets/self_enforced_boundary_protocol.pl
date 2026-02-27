% ============================================================================
% CONSTRAINT STORY: self_enforced_boundary_protocol
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_self_enforced_boundary_protocol, []).

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
 *   constraint_id: self_enforced_boundary_protocol
 *   human_readable: The Self-Enforced Boundary Protocol
 *   domain: psychological/social
 *
 * SUMMARY:
 *   The self-enforced boundary protocol is a psychological constraint that
 *   shifts all enforcement labor to the protected party rather than the
 *   harming party. A boundary is not a request for the other to change
 *   ('Please stop calling me drunk') but a unilateral protocol ('If you call
 *   me drunk, I will hang up and not answer for 48 hours'). The other person
 *   is required to do nothing—the entire constraint operates on the boundary
 *   setter's choice to execute Y when X occurs. This constraint exemplifies
 *   how a protective mechanism can simultaneously coordinate relationships
 *   and extract emotional labor from the protected party. The boundary setter
 *   experiences it as empowering (agency to protect oneself) and oppressive
 *   (perpetual vigilance and enforcement). The other person experiences it as
 *   a clarity mechanism (they know the consequences) and potentially as a
 *   control device (their behavior is monitored and judged). The therapeutic
 *   industry promotes boundaries as healing but may reinforce the extraction
 *   by institutionalizing the norm that victims are responsible for managing
 *   perpetrators' behavior. Theater ratio increases over time as repeated
 *   enforcement with no mutual change transforms the boundary from a
 *   functional protection into a performative ritual.
 *
 * KEY AGENTS:
 *   - Boundary Setter: Primary agent (powerless/trapped at onset, moderate/constrained at maturity) — carries unilateral enforcement burden; experiences extraction of emotional labor; also experiences protection benefit
 *   - Other Person: Secondary agent (moderate/constrained) — required to do nothing; experiences clarity but also constraint on behavior; may resent unilateral nature of protocol
 *   - Therapeutic Industry: Organized beneficiary (organized/arbitrage) — promotes boundaries as healing practice; benefits from client engagement; may reinforce extraction by normalizing victim responsibility
 *   - Boundary-Respecting Community: Organized potential beneficiary (organized/mobile) — can establish collective norms that reduce unilateral burden; enables sunset of explicit protocols as norms internalize
 *   - Dysfunctional Family System: Institutional inertia (institutional/arbitrage) — sustains boundary as performative ritual; provides institutional justification for perpetual enforcement
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional asymmetry as inherent to human interaction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(self_enforced_boundary_protocol, 0.32).
domain_priors:suppression_score(self_enforced_boundary_protocol, 0.48).
domain_priors:theater_ratio(self_enforced_boundary_protocol, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(self_enforced_boundary_protocol, extractiveness, 0.32).
narrative_ontology:constraint_metric(self_enforced_boundary_protocol, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(self_enforced_boundary_protocol, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(self_enforced_boundary_protocol, tangled_rope).
narrative_ontology:human_readable(self_enforced_boundary_protocol, "The Self-Enforced Boundary Protocol").
narrative_ontology:topic_domain(self_enforced_boundary_protocol, "psychological/social").

domain_priors:requires_active_enforcement(self_enforced_boundary_protocol).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(self_enforced_boundary_protocol, boundary_setter).
narrative_ontology:constraint_beneficiary(self_enforced_boundary_protocol, relationship_stability).
narrative_ontology:constraint_victim(self_enforced_boundary_protocol, boundary_setter_emotional_labor).
narrative_ontology:constraint_victim(self_enforced_boundary_protocol, other_person_freedom).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BOUNDARY SETTER AS POWERLESS AGENT (SNARE) — The boundary setter is trapped in perpetual enforcement. The boundary is a self-imposed conditional: 'If X, then I do Y.' The setter cannot exit without abandoning the boundary entirely, losing protection. The emotional labor of enforcement is unilateral. The other person need do nothing; all burden falls on the setter. Maximum experienced extraction because the setter must continuously monitor, decide, and execute—with no exit except dissolution of the protocol itself.
constraint_indexing:constraint_classification(self_enforced_boundary_protocol, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: OTHER PERSON (ROPE) — Experiences the boundary as a coordination mechanism. The other person is not asked to change; they are simply notified of the setter's autonomous response protocol. If they respect the boundary, relationship stability increases (coordination benefit). If they test or violate it, consequences follow predictably (setter executes Y). The other person's exit options are constrained but real: accept the boundary and maintain the relationship, or continue and face the setter's response. This is coordination without coercion—the other person retains agency.
constraint_indexing:constraint_classification(self_enforced_boundary_protocol, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: BOUNDARY AS INSTITUTIONAL SOCIAL PRACTICE (TANGLED ROPE) — From the perspective of therapeutic frameworks and relationship counseling, the boundary protocol coordinates healthy relationships (coordination function: protects both parties from harm, establishes mutual respect) while simultaneously extracting emotional labor from the boundary setter (asymmetric cost: setter must enforce unilaterally). The setter has more agency than a powerless agent but less than a powerful one—they must actively maintain the protocol. The other person benefits from clarity but may resent the unilateral nature. Active enforcement is required; both parties must participate for the boundary to function as designed, yet only the setter performs the enforcement work.
constraint_indexing:constraint_classification(self_enforced_boundary_protocol, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 4: THERAPEUTIC INDUSTRY (TANGLED ROPE) — Organized practitioners promote boundary-setting as a healing practice, benefiting from client engagement and book/course sales (coordination function: genuine assistance in trauma recovery and relationship repair; extraction: institutionalizes emotional labor burden on the traumatized, shifting responsibility for change from the harming party to the harmed party). The industry coordinates the social norm that boundaries are the victim's responsibility to maintain, while the perpetrator can continue unchanged. Suppression is moderate: victims are encouraged to believe that this unilateral emotional work is empowering, when it may conceal a structural extraction.
constraint_indexing:constraint_classification(self_enforced_boundary_protocol, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: BOUNDARY-RESPECTING COMMUNITY (SCAFFOLD) — Organized communities (support groups, trauma-informed spaces, healthy peer groups) can establish shared norms that boundaries are collective responsibility, not unilateral burden. The community sunset: as social maturity increases and boundary-respecting norms become internalized, the need for explicit enforcement protocols decreases. The setter's theater gradually declines as the boundary becomes background assumption rather than active performance. Theater is moderate (0.55 at interval midpoint) because some communities still require repeated boundary-restating (performative aspect), while others enforce through peer accountability (functional aspect).
constraint_indexing:constraint_classification(self_enforced_boundary_protocol, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: DYSFUNCTIONAL FAMILY AS INSTITUTIONAL INERTIA (PITON) — In rigid family systems, boundary protocols persist as performative rituals despite degraded function. Adult children maintain 'If you call me drunk, I hang up' boundaries indefinitely, executing enforcement repeatedly despite no change in the other's behavior. The protocol continues not because it works but because abandoning it feels like surrender. Theater ratio is high (0.70+) in this context: the boundary statement is repeated, the consequence is enacted, nothing changes, the statement is made again. The pattern persists through institutional habit, not effectiveness.
constraint_indexing:constraint_classification(self_enforced_boundary_protocol, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL ASYMMETRY (MOUNTAIN?) — From a civilizational view, the self-enforced boundary protocol reflects a fundamental asymmetry in human interaction: one party can unilaterally commit to a conditional response, but genuine mutual change requires bilateral action. The boundary is a solution to powerlessness that accepts structural asymmetry as natural law. However, this risks naturalizing what is a contingent institutional choice: societies where the harming party bears responsibility for change (rather than the harmed party) are structurally possible and observed. The mountain classification is a false summit if the asymmetry is institutional rather than natural.
constraint_indexing:constraint_classification(self_enforced_boundary_protocol, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(self_enforced_boundary_protocol_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(self_enforced_boundary_protocol, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(self_enforced_boundary_protocol, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(self_enforced_boundary_protocol, TR),
    TR >= 0.70.

:- end_tests(self_enforced_boundary_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The boundary protocol extracts emotional labor from the setter, but only if the other person continues the triggering behavior. The extraction is conditional: if the boundary achieves its coordinating function (other person respects and modifies behavior), extraction declines sharply. The moderate value reflects the typical case where some behavior modification occurs, but not complete, leaving the setter in perpetual partial enforcement. Suppression (0.48): Moderate-high. Significant barriers to exiting the protocol without relationship rupture; emotional attachment to the other person creates suppression of exit options. However, physical exit (leaving, ending relationship) remains available at high cost—not trapped in the sense of slavery or captivity. Theater ratio (0.55): Moderate-high and increasing. Early boundaries are functional (boundary stated once, behavior change follows). Over time, as repetition increases without matching behavior change, theater rises—the boundary becomes a repeated statement, a ritual performance, rather than a protective mechanism. By year 10, theater (0.65) reflects degradation to piton-like status in dysfunctional systems.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the boundary setter's experience (powerless/trapped burden) and the therapeutic framing (powerful/mobile agency). The setter sees unilateral labor (Snare perspective); the therapeutic industry frames it as empowerment (Rope perspective). The other person sees a coordination mechanism that clarifies their options (Rope perspective). The organized community sees an opportunity for collective norms that reduce individual burden (Scaffold perspective). The dysfunctional family sees a ritual that persists despite non-function (Piton perspective). The analytical observer risks seeing an inherent asymmetry in human nature (Mountain perspective), when the asymmetry is actually institutional—a choice to assign responsibility to the victim rather than the perpetrator. The tangled rope classification from the institutional and powerful perspectives captures the hybrid: genuine coordination benefit (both parties know the rules, stability increases) combined with asymmetric cost (setter enforces unilaterally) and active enforcement requirement (setter must continuously monitor and execute).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is derived from the setter's structural position: beneficiary/victim status plus exit options. The boundary setter is simultaneously beneficiary (protected from harm) and victim (bears enforcement labor). With trapped exit options, the setter's d approaches 1.0 (full target of extraction). With mobile or constrained exit options (option to leave, to reduce contact, to abandon the boundary), the setter's d decreases toward 0.5-0.7 range (mixed victim/agent position). The other person's d is derived from their relationship to the extraction flow: they benefit from clarity (d lowered toward beneficiary) but may resent constraint on their behavior (d raised toward target). The therapeutic industry's d is beneficiary-like (arbitrage exit, institutional power) despite claiming to serve the setter. The community's d is beneficiary-like (can reduce burden for others) if functioning; victim-like (constrained by ingrained dysfunctional norms) if the larger culture still assigns victim-responsibility.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through perspectival differentiation: the same boundary protocol is simultaneously (a) a coordination mechanism (both parties know the rules), (b) an extraction mechanism (setter bears unilateral labor), and (c) a ritual container for dysfunctional patterns (theater rising over time). The mandatrophy is resolved by recognizing that different structural positions see different types because the boundary's function depends on mutual change. When the other person respects the boundary and modifies behavior, the setter's perspective approaches Rope (coordination). When the other person ignores the boundary, the setter's perspective becomes Snare (extraction). The therapeutic industry frames all boundaries as Rope (empowering coordination), which masks extraction in cases where the other person does not actually change. The organized community's Scaffold perspective is real: as cultural norms shift to require mutual responsibility (harming party changes, not just harmed party enforces), the boundary transitions from unilateral protocol to mutual agreement—theater declines, extraction declines, classification moves toward pure Rope. The mountain perspective is a false summit: the 'natural' asymmetry of human interaction is not inherent but institutional. Societies that assign harming-party responsibility (versus victim-responsibility) exhibit lower theater and lower extraction at scale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_effectiveness_threshold,
    'At what point does boundary enforcement indicate relationship viability vs. perpetual unilateral labor with no mutual change?',
    'Longitudinal tracking: Does the other person''s behavior change after repeated boundary enforcement? What ratio of enforcement cycles to actual behavior modification indicates a functional vs. extracted relationship?',
    'If threshold is low (few cycles needed for change): boundary is coordination mechanism (Rope). If threshold is high or infinite (repeated enforcement with no change): boundary is extraction mechanism (Snare/Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_effectiveness_threshold, empirical, 'Threshold for distinguishing functional boundaries from perpetual unilateral labor').

omega_variable(
    other_person_intentionality,
    'Is the other person''s boundary-testing driven by unconscious pattern, deliberate disrespect, or capacity limitation (addiction, trauma, neurodivergence)?',
    'Clinical assessment; examination of whether other person can perceive and respond to the boundary communication; capacity for mutual negotiation',
    'If capacity is absent: boundary becomes a containment ritual (Piton). If capacity exists but is ignored: boundary reveals extraction (Snare). If capacity exists and is respected: boundary is coordination (Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(other_person_intentionality, empirical, 'Whether other person has capacity to perceive and respect the boundary').

omega_variable(
    institutional_boundary_responsibility,
    'Should healthy societies assign boundary maintenance responsibility to the harmed party or the harming party?',
    'Cross-cultural analysis of relationship structures; comparison of outcomes in societies with different responsibility assignments; examination of whether victim-responsibility norms correlate with harm perpetuation',
    'If victim-responsibility is natural law: mountain classification holds. If victim-responsibility is institutional choice: constraint is Tangled Rope (masquerades as natural while extracting from victims).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_boundary_responsibility, conceptual, 'Institutional assignment of boundary maintenance responsibility').

omega_variable(
    therapeutic_industry_incentive,
    'Does the therapeutic promotion of self-enforced boundaries benefit from keeping clients in extractive situations that require ongoing therapy?',
    'Analysis of therapeutic frameworks promoting individual responsibility for boundaries; comparison with frameworks promoting mutual change; examination of whether therapy outcomes improve when harming parties are required to change vs. when victims are required to enforce boundaries',
    'If industry benefits from prolonged extraction: organized beneficiaries include therapists and authors (Tangled Rope with captured beneficiaries). If industry genuinely promotes healing: extraction is minimized (approaches Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(therapeutic_industry_incentive, empirical, 'Whether therapeutic industry incentives align with client healing or client dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(self_enforced_boundary_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(boundary_tr_t0, self_enforced_boundary_protocol, theater_ratio, 0, 0.35).
narrative_ontology:measurement(boundary_tr_t5, self_enforced_boundary_protocol, theater_ratio, 5, 0.55).
narrative_ontology:measurement(boundary_tr_t10, self_enforced_boundary_protocol, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(boundary_be_t0, self_enforced_boundary_protocol, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(boundary_be_t5, self_enforced_boundary_protocol, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(boundary_be_t10, self_enforced_boundary_protocol, base_extractiveness, 10, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(self_enforced_boundary_protocol, enforcement_mechanism).
narrative_ontology:affects_constraint(self_enforced_boundary_protocol, trauma_reenactment_cycle).
narrative_ontology:affects_constraint(self_enforced_boundary_protocol, emotional_labor_extraction).
narrative_ontology:affects_constraint(self_enforced_boundary_protocol, relational_accountability_deficit).

% DUAL FORMULATION NOTE:
% The boundary protocol decomposes into two structurally distinct constraints: (1) boundary_as_protective_mechanism (ε ≈ 0.15, Rope dominant) — the coordination function that clarifies consequences and protects the setter; and (2) boundary_as_enforcement_burden (ε ≈ 0.45, Snare/Tangled Rope dominant) — the unilateral labor extraction when the other person does not change. The current story captures the union of these constraints. Societies that shift responsibility to the harming party collapse constraint #2 while preserving #1, moving the overall classification toward pure Rope. The therapeutic industry's promotion of victim-responsibility boundaries stabilizes constraint #2 and raises its extractiveness by institutionalizing the norm that the victim is responsible for managing the perpetrator's behavior.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(self_enforced_boundary_protocol, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
