% ============================================================================
% CONSTRAINT STORY: credibility_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_credibility_paradox_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: credibility_paradox_reading
 *   human_readable: Nuclear Deterrence Credibility Paradox (Instability Reading)
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   This constraint embodies one reading of the nuclear impossibility kernel:
 *   the credibility paradox reading asserts that nuclear deterrence is
 *   fundamentally unstable because the threat that sustains it is logically
 *   incredible. For a deterrent to work, an adversary must believe the threat
 *   of nuclear use is credible. But if the threat is carried out, mutual
 *   destruction follows, making the threat irrational to execute — hence
 *   incredible. This reading interprets the paradox as revealing that
 *   deterrence can never be stable; great powers will continuously seek to
 *   escape the paradox by developing 'usable' nuclear options (counterforce,
 *   limited war, escalation dominance) that restore credibility at the cost
 *   of making nuclear war thinkable. The paradox is not solvable by doctrine;
 *   it can only be managed by hedging toward warfighting capability. This
 *   reading contrasts sharply with the structural_contraction_reading, which
 *   sees the very insolubility of the paradox as proof that war has become
 *   structurally impossible — the more incredible the threat becomes, the
 *   more deterrence actually works because everyone knows use is unthinkable.
 *   These readings coexist as live strategic positions held by different
 *   institutions and strategic communities.
 *
 * KEY AGENTS:
 *   - Great Power Strategic Establishments (Defense ministries, strategic commands, weapons labs): Institutional beneficiaries — the paradox justifies continuous investment in nuclear modernization and counterforce capabilities
 *   - Nuclear-Armed Adversary States (Russia, China, US, UK, France, etc.): Both beneficiary and victim — constrained by the paradox but benefit from its role in preventing war
 *   - Non-Nuclear States and Global Population: Primary victims — trapped in deterrence system, suppressed by credibility requirements, bearing risk of deterrence failure
 *   - Nuclear Weapons Laboratories (Los Alamos, Livermore, etc.): Secondary beneficiaries — extractive demand for technical solutions to the paradox (advanced warheads, counterforce platforms)
 *   - Cold War Deterrence Doctrine (MAD, extended deterrence, nuclear strategy): Degraded institutional structure — persists through inertia despite internal inconsistencies
 *   - Analytical Observer: Sees logical contradiction; risks naturalizing strategic choice as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credibility_paradox_reading, 0.58).
domain_priors:suppression_score(credibility_paradox_reading, 0.72).
domain_priors:theater_ratio(credibility_paradox_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credibility_paradox_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(credibility_paradox_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(credibility_paradox_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(credibility_paradox_reading, tangled_rope).
narrative_ontology:human_readable(credibility_paradox_reading, "Nuclear Deterrence Credibility Paradox (Instability Reading)").
narrative_ontology:topic_domain(credibility_paradox_reading, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:requires_active_enforcement(credibility_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(credibility_paradox_reading, 'a0425342-0e20-4328-968d-cdcfc515bdbc').
narrative_ontology:cs_created_at('a0425342-0e20-4328-968d-cdcfc515bdbc', '').
narrative_ontology:cs_kernel_codification('a0425342-0e20-4328-968d-cdcfc515bdbc', fixed_text).
narrative_ontology:cs_authority_grounding('a0425342-0e20-4328-968d-cdcfc515bdbc', lineage).
narrative_ontology:cs_interpretation_layer_present('a0425342-0e20-4328-968d-cdcfc515bdbc').
narrative_ontology:cs_kernel_id(credibility_paradox_reading, nuclear_impossibility_kernel).
narrative_ontology:cs_reading_relation('a0425342-0e20-4328-968d-cdcfc515bdbc', structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('a0425342-0e20-4328-968d-cdcfc515bdbc', rational_dropout_reading, influences).
narrative_ontology:cs_axiom('a0425342-0e20-4328-968d-cdcfc515bdbc', foundational, credibility_paradox_is_insoluble).
narrative_ontology:cs_axiom_status(credibility_paradox_is_insoluble, holdable).
narrative_ontology:cs_axiom_grounding('a0425342-0e20-4328-968d-cdcfc515bdbc', credibility_paradox_is_insoluble, empirically_contingent).
narrative_ontology:cs_axiom('a0425342-0e20-4328-968d-cdcfc515bdbc', foundational, usable_nuclear_options_are_reachable).
narrative_ontology:cs_axiom_status(usable_nuclear_options_are_reachable, holdable).
narrative_ontology:cs_axiom_grounding('a0425342-0e20-4328-968d-cdcfc515bdbc', usable_nuclear_options_are_reachable, empirically_contingent).
narrative_ontology:cs_reference_frame('a0425342-0e20-4328-968d-cdcfc515bdbc', nuclear_deterrence_credibility_foundation).
narrative_ontology:cs_drift_state('a0425342-0e20-4328-968d-cdcfc515bdbc', contemporary_counterforce_era, gap(codification_collapse, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(credibility_paradox_reading, great_power_strategic_establishments).
narrative_ontology:constraint_beneficiary(credibility_paradox_reading, nuclear_weapons_laboratories).
narrative_ontology:constraint_victim(credibility_paradox_reading, non_nuclear_states).
narrative_ontology:constraint_victim(credibility_paradox_reading, global_civilian_population).
narrative_ontology:constraint_victim(credibility_paradox_reading, rational_escalation_avoidance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-NUCLEAR STATES / CIVILIAN POPULATIONS (SNARE) — Trapped in a deterrence system they did not construct and cannot exit. Suppressed by the credibility requirement itself: any state demonstrating weakness invites coercive pressure, yet any state attempting nuclear acquisition triggers intervention. Maximum extraction — no exit mechanism, no alternative security framework available, bearing full risk of deterrence failure.
constraint_indexing:constraint_classification(credibility_paradox_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL POWERS / THRESHOLD STATES (TANGLED ROPE) — Constrained by non-proliferation enforcement and great power pressure, yet also benefit from the deterrence system's logic: the credibility paradox justifies their own pursuit of 'escalation dominance' narratives and limited nuclear options. Mixed extraction — face severe barriers to nuclear development but can extract legitimacy for conventional superiority claims from great power deterrence rhetoric.
constraint_indexing:constraint_classification(credibility_paradox_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: GREAT POWER STRATEGIC ESTABLISHMENTS (ROPE) — Institutional actors (defense ministries, strategic commands, nuclear laboratories) experience the credibility paradox as a coordination mechanism: the paradox itself justifies continuous investment in counterforce capabilities, flexible response doctrines, and 'usable nuclear options.' Net beneficiaries — the paradox creates demand for the expertise, weapons systems, and strategic concepts these institutions provide. Low experienced extraction because the paradox serves their institutional interests.
constraint_indexing:constraint_classification(credibility_paradox_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COLD WAR DETERRENCE DOCTRINE / DEGRADED INSTITUTION (PITON) — Mutually Assured Destruction (MAD) was the peak institutional formalization of deterrence stability (1960s-1980s). The doctrine is now substantially degraded: no great power actively affirms MAD as operative, all major strategic commands maintain counterforce capabilities inconsistent with MAD, and the doctrinal coherence has been replaced by performative reaffirmation of 'extended deterrence' and 'escalation control.' Theater ratio (0.65) reflects the gap between what is said (deterrence stability through mutual vulnerability) and what is practiced (continuous hedging toward warfighting options). The old doctrine persists through institutional inertia, not because it functions.
constraint_indexing:constraint_classification(credibility_paradox_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NUCLEAR-ARMED ADVERSARY STATES (TANGLED ROPE) — Structurally constrained by the same credibility paradox that binds them: each must demonstrate willingness to use weapons to maintain deterrent threat, yet use guarantees annihilation. The constraint coordinates their mutual vulnerability (coordination function: mutual assured vulnerability prevents war) while extracting continuous military expenditure and operational risk. Both beneficiary and victim: the constraint prevents war but creates permanent threat environment and requires permanent strategic mobilization.
constraint_indexing:constraint_classification(credibility_paradox_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LOGICAL CONTRADICTION VIEW (MOUNTAIN) — From a purely logical standpoint, the paradox appears as an immutable structural feature: any threat to use weapons that trigger mutual destruction is logically incredible, and no rhetorical strategy or doctrinal innovation can resolve this logical impossibility. Credibility cannot be restored through institutional means because the underlying contradiction is not institutional but logical. However, the engine's false-summit detector will identify this as naturalization of a strategic problem as a logical law — the paradox is not immutable but rather reflects specific strategic choices about how nuclear weapons are integrated into deterrence systems.
constraint_indexing:constraint_classification(credibility_paradox_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credibility_paradox_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(credibility_paradox_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(credibility_paradox_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(credibility_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(credibility_paradox_reading, TR),
    TR >= 0.70.

:- end_tests(credibility_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts continuous military expenditure, technological development, and strategic mobilization from great powers and forces non-nuclear states into subordinate security positions. The extraction is not maximal (not a pure snare) because it serves a coordination function — it prevents conventional war among great powers. But the extraction is substantial because the only exit route (nuclear armament) is blocked by enforcement and counterproliferation. Suppression (0.72): High. Multiple barriers prevent exit from the deterrence system: (1) Non-nuclear states cannot escape through nuclear armament without triggering intervention. (2) Nuclear-armed states cannot escape by disarming without losing security credibility. (3) The credibility paradox itself creates a suppression feedback: states that appear to question the paradox are seen as unstable or untrustworthy, so the paradox must be accepted even by those who doubt it. Theater ratio (0.65): Moderate-high. The discrepancy between stated doctrine (deterrence stability through mutual vulnerability, MAD, extended deterrence) and actual practice (all great powers maintain counterforce capabilities, pursue escalation dominance, develop 'usable' nuclear options) reflects substantial theatrical performance. States claim to accept the paradox while continuously developing doctrines that deny it. The gap has widened over time as the Cold War (when some believed in MAD) gave way to post-Cold War doctrines (where all major powers hedge toward warfighting).
 *
 * PERSPECTIVAL GAP:
 *   The gap between perspectives reveals how the same structural fact (the credibility paradox) appears differently from different positions. The great power strategic establishment sees the constraint as a coordination mechanism that enables deterrence and prevents war — they benefit from the institutional arrangements that maintain the paradox. Non-nuclear states see pure extraction — they are trapped in a security hierarchy they cannot escape. The degraded Cold War doctrine (piton perspective) reflects the growing incoherence of strategic theory as practice has diverged from stated principles. The analytical observer risks seeing the paradox as a natural law of logic rather than as a contingent feature of how deterrence has been institutionalized. The core disagreement between this reading and the structural_contraction_reading appears in the piton and institutional tangled_rope perspectives: are great powers genuinely hedging toward usable nuclear options (credibility paradox reading), or are the elaborate doctrines masking universal acknowledgment that use is impossible (structural contraction reading)?
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies substantially by agent. Great power strategic establishments (beneficiaries, institutional power, arbitrage exit) derive d ≈ 0.15 from beneficiary status and escape options — they experience negative or near-zero effective extraction because the constraint serves their interests. Non-nuclear states (victims, powerless, trapped) derive d ≈ 0.95 from victim status and no exit — they experience maximum extraction despite moderate base extractiveness. Nuclear-armed adversaries (victims + beneficiaries, powerful, constrained) derive d ≈ 0.65 from mixed status and constrained exit — they experience moderate-high extraction despite being powerful. The analytical observer derives d ≈ 0.72 from the observation position itself: seeing the paradox clearly requires standing outside the strategic establishments' institutional logic. The perspectival gap is large: beneficiaries see coordination (Rope), victims see extraction (Snare), and the observer risks seeing a logical law (Mountain) rather than a strategic system.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING EXEMPLAR: This constraint resolves mandatrophy by being explicit about which reading it instantiates. The false-summit mountain perspective (logical paradox as natural law) is paired with omega variable 4 documenting that the paradox may be contingent to deterrence-based organization rather than natural to nuclear weapons. The tangled_rope classification reflects the core claim of this reading: deterrence is unstable (not rope's pure coordination) because the paradox cannot be resolved (not mountain's immutability) — instead, great powers continuously develop workarounds that maintain the extraction while claiming to solve the paradox. The piton perspective documents the degradation of Cold War deterrence doctrine, which claimed to have solved the paradox through MAD but has been replaced by doctrines that deny the solution ever existed. The mandatrophy is resolved by routing the alternative reading possibility (structural contraction) to omega variable 3, where it serves as a diagnostic check on whether doctrine is genuinely hedging toward war or performing stability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credibility_restoration_mechanisms,
    'Can the credibility paradox be resolved through doctrinal innovation (graduated response, counterforce targeting, limited nuclear war scenarios) or is it a fundamental logical problem that no institutional arrangement can overcome?',
    'Historical analysis of strategic doctrine effectiveness; examination of whether states can actually implement ''graduated'' nuclear responses without triggering escalation; empirical test of whether adversaries believe in limited nuclear war scenarios',
    'If credibility can be restored through doctrine: the constraint becomes a coordination/enforcement problem (Rope) and the paradox is solvable. If the paradox is truly fundamental: all doctrinal claims are theater, extractiveness remains high, and the tangled_rope classification is stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credibility_restoration_mechanisms, conceptual, 'Whether doctrinal innovation can resolve the credibility paradox or if it is logically irreducible').

omega_variable(
    rationality_assumption_dependence,
    'Does the credibility paradox depend on the assumption that states are rational actors maximizing expected utility, or does it persist under alternative behavioral and organizational models of how nuclear decisions are actually made?',
    'Examination of actual nuclear command authority structures, psychological research on decision-making under existential uncertainty, game theory with bounded rationality models, organizational sociology of nuclear weapons management',
    'If paradox depends on rationality assumption: may be dissolved by shifting to bounded rationality, organizational drift, or psychological factors (perception of credibility differs from logical credibility). If paradox persists under alternative models: it is a deeper structural feature of nuclear deterrence systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationality_assumption_dependence, empirical, 'Whether the paradox depends on rational-actor assumptions or persists under behavioral models').

omega_variable(
    this_reading_vs_structural_contraction,
    'Does the credibility paradox reveal that nuclear deterrence is inherently unstable (this reading: great powers will continuously seek usable nuclear options and escalation dominance), or does it reveal that deterrence is so constraining that it eliminates the structural possibility of rational great-power war altogether (structural contraction reading)?',
    'Longitudinal analysis of strategic doctrine over 75 years: does policy move toward acknowledging the paradox as insoluble (supporting this reading) or toward increasingly elaborate doctrinal frameworks claiming to resolve it (supporting structural contraction reading)? Examination of actual nuclear command protocols and counterforce capabilities: are they window-dressing on the knowledge that war is impossible, or genuine warfighting preparations?',
    'If this reading is correct (instability/credibility paradox is insoluble): great powers will continuously develop new nuclear doctrines and technologies hedging toward usable options, escalation remains reachable, and the threat of nuclear war persists. If structural contraction reading is correct (deterrence is structurally impossible to cross): the apparent variety of doctrines masks universal acknowledgment that war cannot happen, and the paradox has been structurally resolved despite persisting as logical impossibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(this_reading_vs_structural_contraction, conceptual, 'Core disagreement between credibility_paradox_reading and structural_contraction_reading: does the paradox reveal instability or structural impossibility?').

omega_variable(
    false_summit_naturalness,
    'Is the credibility paradox a natural/immutable feature of nuclear weapons physics (logical feature that cannot be overcome), or is it a contingent feature of how great powers have chosen to organize deterrence (policy choice that could be replaced)?',
    'Conceptual analysis: does the paradox flow from nuclear weapons physics itself or from the strategic choice to organize deterrence around credible use-threats rather than capability-based deterrence? Comparative examination of non-deterrence-based nuclear strategies (if any exist or could exist).',
    'If natural: mountain classification from analytical perspective is appropriate, and the constraint is genuinely immutable. If contingent: false summit detection is appropriate, the constraint could be replaced through different organizing principles, and the appearance of immutability is rhetorical/strategic rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalness, conceptual, 'Whether the paradox is natural to nuclear weapons or contingent to deterrence-based organization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(credibility_paradox_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1945_functional_monopoly, credibility_paradox_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(theater_1965_mad_stability_claims, credibility_paradox_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(theater_1985_flexible_response_narrative, credibility_paradox_reading, theater_ratio, 40, 0.62).
narrative_ontology:measurement(theater_2005_extended_deterrence_rhetoric, credibility_paradox_reading, theater_ratio, 60, 0.65).

% Extraction over time
narrative_ontology:measurement(extractiveness_1945_nuclear_monopoly, credibility_paradox_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(extractiveness_1965_mad_formalization, credibility_paradox_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(extractiveness_1985_reagan_counterforce, credibility_paradox_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(extractiveness_2005_proliferation_acceleration, credibility_paradox_reading, base_extractiveness, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(credibility_paradox_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(credibility_paradox_reading, structural_contraction_reading).
narrative_ontology:affects_constraint(credibility_paradox_reading, rational_dropout_reading).
narrative_ontology:affects_constraint(credibility_paradox_reading, nuclear_proliferation_suppression).
narrative_ontology:affects_constraint(credibility_paradox_reading, great_power_war_prevention).

% DUAL FORMULATION NOTE:
% The credibility_paradox_reading is one decomposition of the nuclear_impossibility_kernel. Sibling readings (structural_contraction_reading, rational_dropout_reading) are separate constraint stories with different ε values and different structural implications, despite sharing the same historical kernel. The network links show how this reading affects (and is affected by) related constraints in the nuclear deterrence domain. Each reading instantiates a different strategic interpretation with different policy consequences — this one emphasizing instability and hedging, the others emphasizing stability through structural impossibility or rational dropout.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
