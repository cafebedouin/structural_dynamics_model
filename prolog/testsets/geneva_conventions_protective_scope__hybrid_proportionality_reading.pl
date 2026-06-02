% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__hybrid_proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__hybrid_proportionality_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__hybrid_proportionality_reading
 *   human_readable: Geneva Conventions Protective Scope (Hybrid Proportionality Reading)
 *   domain: international_humanitarian_law/armed_conflict/legal_theory
 *
 * SUMMARY:
 *   The Geneva Conventions' protective scope scales by conflict
 *   classification — Additional Protocol I applies to international armed
 *   conflicts between states; Additional Protocol II and Common Article 3
 *   apply to non-international armed conflicts involving non-state actors or
 *   internal violence. This hybrid proportionality reading instantiates one
 *   specific interpretation: protective obligations are determined by the
 *   type of conflict and calibrated through a proportionality analysis that
 *   compares anticipated civilian harm to direct military advantage. The
 *   reading generates a tangled-rope structure because it combines genuine
 *   coordination (establishing baseline protections that reduce arbitrary
 *   violence) with systematic extraction (the proportionality calculus grants
 *   discretionary authority to the stronger party to determine protective
 *   scope). The constraint's extractiveness has risen from 0.35 to 0.58 over
 *   two decades as hybrid conflicts (state/non-state,
 *   conventional/unconventional) have become dominant, exposing gaps in
 *   protective coverage. Theater ratio has increased as courts perform
 *   proportionality analysis with increasingly complex fact-finding that
 *   occupying forces effectively control. Suppression has increased as the
 *   proportionality framework allows actors to classify conflicts
 *   strategically and thereby reduce the protective scope applied to weaker
 *   parties and civilian populations.
 *
 * KEY AGENTS:
 *   - Stronger Military Party: Primary beneficiary (institutional/arbitrage) — proportionality framework grants discretionary authority to classify conflict and determine protective scope; net benefit from legal clarity that favors military advantage
 *   - Weaker Armed Group or State: Mixed victim/constrained actor (powerful/constrained) — benefits from baseline protections but suppressed by proportionality discretion controlled by stronger party
 *   - Civilian Populations (especially in hybrid conflicts): Primary victim (powerless/trapped) — cannot establish protected status when proportionality analysis treats entire populations as ambiguous
 *   - International Humanitarian Law Field: Constrained coordinator (moderate/constrained) — benefits from stable framework but bears extraction costs from ambiguity that permits strategic interpretation
 *   - International Court System (ICC, ICJ): Institutional enforcer (institutional/arbitrage) — maintains enforcement apparatus but constrained by reliance on state cooperation and factual uncertainty about military necessity claims
 *   - Analytical Observer: Sees potential false summit (analytical/analytical) — risks naturalizing the proportionality principle as an immutable law of warfare rather than revealing it as a contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.58).
domain_priors:suppression_score(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.67).
domain_priors:theater_ratio(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__hybrid_proportionality_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__hybrid_proportionality_reading, "Geneva Conventions Protective Scope (Hybrid Proportionality Reading)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__hybrid_proportionality_reading, "international_humanitarian_law/armed_conflict/legal_theory").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__hybrid_proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__hybrid_proportionality_reading, '8a37f0b7-59c1-4629-8e68-8c2410d7cfdf').
narrative_ontology:cs_kernel_codification('8a37f0b7-59c1-4629-8e68-8c2410d7cfdf', fixed_text).
narrative_ontology:cs_authority_grounding('8a37f0b7-59c1-4629-8e68-8c2410d7cfdf', lineage).
narrative_ontology:cs_interpretation_layer_present('8a37f0b7-59c1-4629-8e68-8c2410d7cfdf').
narrative_ontology:cs_reading_relation('8a37f0b7-59c1-4629-8e68-8c2410d7cfdf', geneva_conventions_protective_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a37f0b7-59c1-4629-8e68-8c2410d7cfdf', geneva_conventions_protective_scope__universal_rights_reading, influences).
narrative_ontology:cs_axiom('8a37f0b7-59c1-4629-8e68-8c2410d7cfdf', foundational, proportionality_as_calibration_mechanism).
narrative_ontology:cs_axiom_status(proportionality_as_calibration_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('8a37f0b7-59c1-4629-8e68-8c2410d7cfdf', proportionality_as_calibration_mechanism, conventional).
narrative_ontology:cs_axiom('8a37f0b7-59c1-4629-8e68-8c2410d7cfdf', foundational, conflict_classification_determines_baseline_protection).
narrative_ontology:cs_axiom_status(conflict_classification_determines_baseline_protection, holdable).
narrative_ontology:cs_axiom_grounding('8a37f0b7-59c1-4629-8e68-8c2410d7cfdf', conflict_classification_determines_baseline_protection, conventional).
narrative_ontology:cs_reference_frame('8a37f0b7-59c1-4629-8e68-8c2410d7cfdf', conflict_classification_determines_protective_scope).
narrative_ontology:cs_drift_state('8a37f0b7-59c1-4629-8e68-8c2410d7cfdf', contemporary_hybrid_conflict_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8a37f0b7-59c1-4629-8e68-8c2410d7cfdf', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, stronger_military_party).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, states_with_legal_sophistication).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, weaker_armed_groups).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilian_populations_in_hybrid_conflicts).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_actors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POWERLESS / TRAPPED (SNARE) — Non-state armed groups cannot exit the conflict nor clearly establish their protected status under AP II/Common Article 3. Proportionality calculus conducted by the stronger party determines whether protective rules apply. No agency in classification; maximum exposure to extraction via ambiguous status.
constraint_indexing:constraint_classification(geneva_conventions_protective_scope__hybrid_proportionality_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: POWERLESS / TRAPPED (SNARE) — Civilians in hybrid conflicts (blurred state/non-state warfare) cannot reliably establish their non-combatant status when proportionality analysis by occupying forces treats entire populations as ambiguous. Suppression is structural — asymmetric information about protected status, no recourse mechanism.
constraint_indexing:constraint_classification(geneva_conventions_protective_scope__hybrid_proportionality_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MODERATE / CONSTRAINED (TANGLED ROPE) — The IHL field benefits from a stable normative framework (coordination function) while bearing extraction costs from legal ambiguity that permits strategic interpretation. Field actors constrained by need for state compliance and funding; proportionality rules require active enforcement by states with differential capacity.
constraint_indexing:constraint_classification(geneva_conventions_protective_scope__hybrid_proportionality_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL / ARBITRAGE (ROPE) — Stronger state parties benefit from proportionality framework that grants discretionary authority to classify conflict type and determine protective scope. Experiences the constraint as coordination: proportionality rules enable operation while preserving strategic flexibility. Net beneficiary.
constraint_indexing:constraint_classification(geneva_conventions_protective_scope__hybrid_proportionality_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: POWERFUL / CONSTRAINED (TANGLED ROPE) — Weaker state parties are constrained by the proportionality calculus but also benefit from AP II/Common Article 3 baseline protections. Mixed position: some genuine coordination (baseline rights protection) but also significant extraction via ambiguous application when stronger party controls proportionality interpretation.
constraint_indexing:constraint_classification(geneva_conventions_protective_scope__hybrid_proportionality_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INSTITUTIONAL / ARBITRAGE (PITON) — The ICC and other international courts enforce Geneva protections through proportionality analysis that has become largely performative. Theater ratio high: proportionality determinations require complex fact-finding about conflict classification that occupying forces control; courts rarely overturn military necessity claims. The enforcement mechanism persists through institutional inertia rather than functional verification capacity.
constraint_indexing:constraint_classification(geneva_conventions_protective_scope__hybrid_proportionality_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL (MOUNTAIN) — From a civilizational perspective, proportionality in warfare may appear as an immutable principle: the rules of war must scale to the nature of the conflict. Some view this proportionality principle as a natural law of ethical warfare. However, structural data reveals this as a false summit — the 'proportionality' framing allows strategic interpretation and conceals the beneficiary structure favoring stronger parties.
constraint_indexing:constraint_classification(geneva_conventions_protective_scope__hybrid_proportionality_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(geneva_conventions_protective_scope__hybrid_proportionality_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geneva_conventions_protective_scope__hybrid_proportionality_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(geneva_conventions_protective_scope__hybrid_proportionality_reading, TR),
    TR >= 0.70.

:- end_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The hybrid proportionality reading extracts through discretionary application rather than explicit coercion. The stronger party gains strategic advantage from a framework that requires proportionality in principle but grants them authority to determine both the proportionality standard and the conflict classification that determines which protections apply. The extractiveness reflects the degree to which protective scope is contingent on the stronger party's interpretive choices. Suppression (0.67): High. Suppression operates through structural ambiguity: weaker parties and civilian populations cannot reliably establish their protected status because the proportionality calculus is conducted by the stronger party using facts that the stronger party controls. Suppression is not violent — it is procedural and epistemic. Theater ratio (0.64): Moderate-high. Court proceedings around proportionality claims involve extensive fact-finding about anticipated civilian harm, direct military advantage, and military necessity — all determinations that the occupying force effectively controls through its monopoly on evidence and forward planning. The court's review is performative in the sense that it validates military judgments more often than it overturns them based on proportionality grounds, particularly when the stronger party has technical expertise in military planning.
 *
 * PERSPECTIVAL GAP:
 *   The principal gap is between the stronger party's experience (Rope: coordination with strategic flexibility) and the weaker party's experience (Snare: ambiguous protected status, no agency in classification). Intermediate perspectives show the mechanism: moderate field actors see genuine coordination benefits but also extraction costs from ambiguity; institutional courts see their enforcement function degraded to performance validation rather than substantive review.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's structural position in the proportionality calculus. The stronger military party has d ≈ 0.15 (beneficiary with arbitrage exit — they can declare conflicts and proportionality standards with high confidence their position will be upheld). The weaker armed group has d ≈ 0.88 (victim with trapped exit — no ability to exit the conflict, no control over classification, no recourse if proportionality is invoked). Civilians in hybrid conflicts have d ≈ 0.92 (even more trapped — not combatants but unable to establish non-combatant status when proportionality analysis is discretionary). The IHL field has d ≈ 0.60 (moderate position: they benefit from coordination functions but are constrained by state power and funding). The engine derives these d values from the beneficiary/victim declarations and exit options, producing the perspective-dependent χ values that feed classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification resolves the mandatrophy by revealing how the hybrid proportionality reading combines genuine coordination (establishing baseline protections that reduce arbitrary violence) with systematic extraction (the discretionary proportionality calculus that favors the stronger party). The constraint is not a pure coordination mechanism (Rope) because the proportionality standard is indeterminate and controlled by the stronger party. It is not a pure extraction mechanism (Snare) because it does establish meaningful baseline protections that weaker parties can invoke. It is not a scaffold (temporary) because the proportionality framework is structurally stable — there is no sunset clause or alternative mechanism displacing it. The tangled-rope type accurately captures the hybrid nature: coordination function + asymmetric extraction + active enforcement required.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conflict_classification_ambiguity,
    'Who determines whether a conflict is international (AP I) or non-international (AP II/Common Article 3), and what standard governs this determination?',
    'Analysis of control—determination by ICRC vs. national governments vs. occupying forces; review of cases where classification changed retroactively; comparative study of conflicts initially classified as one type then reclassified',
    'If determination is objective (controlled by ICRC): protective scope is stable and universally applicable. If determination is controlled by stronger party: protective scope becomes a tool of strategic advantage. Current practice: ambiguous, allowing discretionary interpretation by occupying forces.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conflict_classification_ambiguity, empirical, 'Who controls conflict classification and proportionality determination').

omega_variable(
    proportionality_standard_indeterminacy,
    'Does the proportionality calculus in AP I (anticipated civilian harm vs. direct military advantage) have an identifiable, reviewable standard, or is it inherently discretionary?',
    'Textual analysis of AP I proportionality language; comparison of military claims of proportionality vs. post-conflict evidence of civilian harm; meta-analysis of cases where proportionality claims were upheld vs. rejected by international courts',
    'If proportionality standard is determinate: protective scope is stable and verifiable. If inherently discretionary: proportionality becomes the mechanism through which stronger parties extract — framework grants protection in principle but denies it in application.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_standard_indeterminacy, conceptual, 'Whether proportionality calculus is determinate or discretionary').

omega_variable(
    hybrid_conflict_protective_coverage_gap,
    'In hybrid conflicts (state/non-state, conventional/unconventional), do AP II/Common Article 3 protections adequately cover civilians whose combatant status is ambiguous?',
    'Case study analysis: conflicts with significant civilian-combatant indistinguishability (Gaza, Syria, Ukraine); documentation of harm categories not clearly protected by AP II baseline; comparison of death/injury rates in hybrid conflicts vs. conventional conflicts under AP I',
    'If coverage is adequate: AP II baseline provides reliable protection even in hybrid contexts. If gap exists: weaker parties and civilians in hybrid conflicts experience higher extraction (reduced protective scope) due to structural ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_conflict_protective_coverage_gap, empirical, 'Coverage gap in hybrid conflict protection under AP II').

omega_variable(
    this_reading_vs_state_centric_reading,
    'How does the hybrid proportionality reading (this constraint) differ from the state-centric reading, and what makes them coexistent rather than foreclosing?',
    'Textual study of state vs. hybrid reading positions in legal literature; identification of which axioms each reading holds foundational; examination of whether a single legal framework could endorse both readings or whether they require different authority structures',
    'If the readings are truly coexistent: both remain live positions in international law and practice. If one logically forecloses the other: the doctrine is internally contradictory and one reading will eventually dominate. Current state: coexistent but in unstable equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(this_reading_vs_state_centric_reading, conceptual, 'Structural relationship between hybrid proportionality and state-centric readings of Geneva protections').

omega_variable(
    universal_rights_reading_incompatibility,
    'Does the universal-rights reading (all persons protected equally regardless of conflict classification) logically foreclose the hybrid proportionality reading''s conflict-type-dependent protections?',
    'Comparative legal analysis: whether international law could simultaneously hold that protective scope depends on conflict classification AND that fundamental protections are universal; examination of whether these are genuinely incompatible or merely different priority orderings',
    'If truly incompatible: one reading must eventually override. If coexistent: the hybrid proportionality reading stands in a presheaf of multiple simultaneous interpretations. Current state: nominally coexistent but with practical pressure toward universal-rights framing in human-rights discourse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_rights_reading_incompatibility, conceptual, 'Logical compatibility of universal-rights and proportionality-based protective scope').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geneva_hybrid_tr_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(geneva_hybrid_tr_t10, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(geneva_hybrid_tr_t20, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(geneva_hybrid_be_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(geneva_hybrid_be_t10, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(geneva_hybrid_be_t20, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(geneva_hybrid_su_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(geneva_hybrid_su_t10, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(geneva_hybrid_su_t20, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 20, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__hybrid_proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__universal_rights_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, proportionality_as_discretionary_authority).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, hybrid_conflict_civilian_targeting_rules).

% DUAL FORMULATION NOTE:
% The geneva_conventions_protective_scope constraint decomposes into three separate constraint stories, each instantiating a different reading of the same kernel text. The hybrid proportionality reading (this story) represents one interpretation. The state_centric_reading and universal_rights_reading are distinct constraints with potentially different ε values and beneficiary/victim structures. They are linked through the network as coexistent or foreclosing alternatives, not as different measurements of the same constraint. The ε value (0.58) is specific to this reading's interpretation — a different reading would have a different base_extractiveness reflecting a different structural analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_protective_scope__hybrid_proportionality_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
