% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__credibility_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__credibility_paradox_reading, []).

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
 *   constraint_id: nuclear_impossibility_kernel__credibility_paradox_reading
 *   human_readable: Nuclear Credibility Paradox: The Incredible Deterrent
 *   domain: strategic_studies/nuclear_deterrence_theory/international_security
 *
 * SUMMARY:
 *   Nuclear weapons created a paradox that this reading characterizes as
 *   fundamentally unstable: deterrence requires a credible threat of use, but
 *   actual use guarantees mutual destruction, which makes the threat
 *   incredible. From the credibility_paradox perspective, great powers cannot
 *   accept this instability and therefore seek to restore credibility by
 *   developing usable nuclear options—counterforce strategies, limited war
 *   doctrines, escalation ladders that disaggregate nuclear war from mutual
 *   annihilation. The constraint is a snare because it traps all actors in a
 *   logic that is simultaneously binding (the threat shapes behavior) and
 *   unstable (the threat is not rational). Status quo powers maintain the
 *   constraint through institutional theater: elaborate doctrine, taboos on
 *   first use, strategic ambiguity, and rhetorical unthinkability. But
 *   revisionist powers and smaller nuclear states experience the constraint
 *   as pure extraction—they are locked into strategic inferiority by a threat
 *   they know is incredible but cannot escape. The extractiveness has risen
 *   over time (0.45 to 0.68) as the paradox has become institutionalized into
 *   alliance structure, NATO extended deterrence, and strategic doctrine.
 *   Theater has also risen (0.42 to 0.68) as the deterrent establishment has
 *   invested more heavily in maintaining the 'unthinkability' narrative
 *   against its own logical fragility.
 *
 * KEY AGENTS:
 *   - Status Quo Nuclear Superpowers (institutional/arbitrage): Maintain the constraint through institutional commitment to unthinkability; benefit from strategic stability without requiring credible use-threat
 *   - Revisionist Nuclear Powers (institutional/constrained): Seek to break the paradox by developing counterforce and limited-war options; experience the constraint as suppression of credible escalation pathways
 *   - Non-Nuclear States (powerless/trapped): Locked into strategic inferiority; the paradox's incredibility is irrelevant because the power asymmetry binds them regardless
 *   - Regional Nuclear Powers (institutional/constrained): Coordinate deterrence through institutional taboos while also seeking doctrinal sophistication to signal limited options
 *   - International Deterrence Establishment (institutional/arbitrage): Academic and institutional machinery sustaining the unthinkability narrative; theater performers maintaining Cold War settlement
 *   - Analytical Observer (analytical/analytical): Risks seeing the paradox as a logical immutability rather than a contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, 0.68).
domain_priors:suppression_score(nuclear_impossibility_kernel__credibility_paradox_reading, 0.72).
domain_priors:theater_ratio(nuclear_impossibility_kernel__credibility_paradox_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__credibility_paradox_reading, snare).
narrative_ontology:human_readable(nuclear_impossibility_kernel__credibility_paradox_reading, "Nuclear Credibility Paradox: The Incredible Deterrent").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__credibility_paradox_reading, "strategic_studies/nuclear_deterrence_theory/international_security").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__credibility_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__credibility_paradox_reading, '857f5e40-1312-4037-bae2-898918833849').
narrative_ontology:cs_kernel_codification('857f5e40-1312-4037-bae2-898918833849', formalized).
narrative_ontology:cs_authority_grounding('857f5e40-1312-4037-bae2-898918833849', extraction).
narrative_ontology:cs_interpretation_layer_present('857f5e40-1312-4037-bae2-898918833849').
narrative_ontology:cs_reading_relation('857f5e40-1312-4037-bae2-898918833849', nuclear_impossibility_kernel__structural_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('857f5e40-1312-4037-bae2-898918833849', nuclear_impossibility_kernel__rational_dropout_reading, coexists_with).
narrative_ontology:cs_axiom('857f5e40-1312-4037-bae2-898918833849', foundational, credibility_requires_rationality).
narrative_ontology:cs_axiom_status(credibility_requires_rationality, holdable).
narrative_ontology:cs_axiom_grounding('857f5e40-1312-4037-bae2-898918833849', credibility_requires_rationality, empirically_contingent).
narrative_ontology:cs_axiom('857f5e40-1312-4037-bae2-898918833849', foundational, institutional_unthinkability_is_contingent).
narrative_ontology:cs_axiom_status(institutional_unthinkability_is_contingent, holdable).
narrative_ontology:cs_axiom_grounding('857f5e40-1312-4037-bae2-898918833849', institutional_unthinkability_is_contingent, conventional).
narrative_ontology:cs_reference_frame('857f5e40-1312-4037-bae2-898918833849', credible_escalation_deterrence).
narrative_ontology:cs_drift_state('857f5e40-1312-4037-bae2-898918833849', contemporary_counterforce_acceleration, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('857f5e40-1312-4037-bae2-898918833849', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, status_quo_nuclear_powers).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, escalation_pathway_logic).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, global_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-NUCLEAR STATE (SNARE) — Locked into strategic inferiority. Cannot credibly threaten escalation; faces persistent existential risk from a threat that is technically incredible but operationally binding. The paradox traps the non-nuclear state: the threat's incredibility is irrelevant because the power asymmetry is real. No exit; maximum extraction.
constraint_indexing:constraint_classification(nuclear_impossibility_kernel__credibility_paradox_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL NUCLEAR POWER (TANGLED ROPE) — Possesses nuclear capability but faces strategic constraints: first-use norms, escalation taboos, alliance structure limits. Genuinely coordinates deterrent communication (rope function) while also extracting coercive advantage (snare function). The paradox is weaponized: rhetorical unthinkability ('we will never use these') paradoxically strengthens deterrent credibility by making accidental or limited use unthinkable.
constraint_indexing:constraint_classification(nuclear_impossibility_kernel__credibility_paradox_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: STATUS QUO NUCLEAR SUPERPOWER (ROPE) — Maintains strategic stability through institutional commitment to 'unthinkability.' The no-first-use doctrine, extended deterrence, and alliance structure enable coordination without requiring credible use-threat. The paradox is structurally dissolved: credibility comes from institutional reputation, not rational calculation. Net beneficiary — deterrence works as coordination mechanism from this perspective.
constraint_indexing:constraint_classification(nuclear_impossibility_kernel__credibility_paradox_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REVISIONIST NUCLEAR POWER (SNARE) — Seeks to break the paradox by developing usable nuclear options (counterforce, limited war, escalation ladders). Invests in doctrinal sophistication to make the incredible credible. Experiences the constraint as pure extraction — the status quo's rhetorical unthinkability prevents this power from credibly signaling limited use, forcing binary choice (no use or total war). Suppression via alliance pressure and strategic ambiguity.
constraint_indexing:constraint_classification(nuclear_impossibility_kernel__credibility_paradox_reading, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL DETERRENCE ESTABLISHMENT (PITON) — Academic strategic studies, NATO doctrine, deterrence theorists, extended deterrence guarantees. Maintains elaborate institutional machinery to sustain the 'unthinkability' narrative despite its logical fragility. The establishment sees its own function as performative — theater prevents active strategic competition for credible nuclear options, preserving Cold War settlement. Theater ratio: 0.68 (doctrine, taboos, strategic ambiguity rhetoric are primarily performative; actual deterrent function decouples from believability).
constraint_indexing:constraint_classification(nuclear_impossibility_kernel__credibility_paradox_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (LOGICAL MOUNTAIN) — From a civilizational view, the paradox is logically immutable: any rational actor cannot credibly threaten an action that guarantees its own destruction. Nuclear deterrence is therefore a mathematical fact ('unthinkable' via rational choice theory), not a contingent institutional arrangement. However, this perspective instantiates a false summit — the structural data shows the paradox is institutionally contingent, not logically necessary. Different great powers resolve it differently (Rope, Snare, or Piton depending on their institutional commitment to 'unthinkability'). The mountain view naturalizes what is actually a political choice.
constraint_indexing:constraint_classification(nuclear_impossibility_kernel__credibility_paradox_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nuclear_impossibility_kernel__credibility_paradox_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nuclear_impossibility_kernel__credibility_paradox_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nuclear_impossibility_kernel__credibility_paradox_reading, TR),
    TR >= 0.70.

:- end_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The paradox extracts value from non-nuclear states and from revisionist powers by locking them into strategic inferiority. Status quo powers benefit from stability without requiring rational credibility. Over time (0.45→0.68), extractiveness has risen as the constraint has become embedded in alliance structure and force posture. Suppression (0.72): High and rising. The constraint requires active suppression of revisionist doctrines (counterforce, limited war, escalation ladders) via alliance pressure, strategic ambiguity, and institutional control of nuclear doctrine. As revisionist powers have sought to restore credibility, suppression has intensified. Theater ratio (0.68): High and rising. Deterrent credibility increasingly relies on performative elements—rhetoric of unthinkability, elaborate doctrine that masks underlying instability, strategic ambiguity as theater. The deterrence establishment maintains theater to prevent active strategic competition over credible nuclear options. Rising theater reflects the increasing gap between what the constraint claims (mutual destruction is unthinkable) and what force posture reveals (nuclear options remain available, limited war doctrines persist, escalation ladders are being developed).
 *
 * PERSPECTIVAL GAP:
 *   The non-nuclear state and the revisionist power both see a snare, but for different reasons: the non-nuclear state is trapped by power asymmetry and the credibility of the threat despite its logical incredibility; the revisionist power is trapped by the taboo that prevents it from making the threat credible. The status quo power sees a rope—they experience deterrence as coordination and benefit from stability without requiring rational credibility. The piton perspective identifies the institutional theater sustaining the constraint against its own logical fragility. The mountain perspective risks naturalizing the paradox as a logical necessity rather than a political settlement. The perspectival gap reveals that the 'paradox' is not universally experienced—it is contingent on one's structural position relative to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Status quo powers (beneficiaries with arbitrage exit) experience low effective extraction—they benefit from the current arrangement and have strategic options if deterrence fails (flexible response, extended deterrence, alliance restructuring). Revisionist powers (constrained) and non-nuclear states (trapped) experience high extraction because they lack credible alternatives to the status quo threat. The institutional taboo on usable nuclear options functions as a suppression mechanism preventing exit. The analytical observer derives d from the civilizational analytical context (d≈0.73, canonical to analytical power atom) and risks high chi, which would support the false summit classification (mountain with identifiable beneficiaries).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credibility_mechanism_locus,
    'Does nuclear deterrent credibility derive from rational calculation of mutual destruction (logical impossibility) or from institutional-political commitment to the unthinkability narrative?',
    'Historical analysis of close-call escalation (Cuban Missile Crisis, Kargil, etc.): do decision-makers calculate expected utility and rationally avoid escalation, or do they defer to institutional scripts and taboos? Behavioral economics / prospect theory examination of actual nuclear command decisions.',
    'If credibility is rational: the constraint is a mountain (logically immutable). If credibility is institutional: the constraint is a snare (contingent, sustained by power asymmetry and suppression of alternative doctrines). This omega locates the difference between the credibility_paradox_reading (institutional) and the structural_contraction_reading (logical).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_mechanism_locus, empirical, 'Whether credibility derives from logical impossibility or institutional commitment').

omega_variable(
    revisionist_option_credibility,
    'Can counterforce strategies, limited war doctrines, or escalation ladders make the use-threat credible by disaggregating it from mutual destruction?',
    'Game-theoretic analysis of limited nuclear war scenarios; empirical study of strategic ambiguity effectiveness in signaling constrained options; war-gaming outcomes when revisionist powers pursue usable nuclear strategies.',
    'If disaggregation succeeds: the paradox dissolves and deterrence becomes a tangled_rope with genuine escalation pathways. Snare perspective is mistaken. If disaggregation fails: use-threat remains incredible regardless of doctrinal sophistication, confirming snare structure and validating this reading''s core claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revisionist_option_credibility, empirical, 'Whether limited nuclear war doctrines can restore credibility to the use-threat').

omega_variable(
    institutional_unthinkability_stability,
    'How durable is the institutional taboo on nuclear use as strategic deterrent credibility mechanism? Does rhetoric sustain credibility or merely masks fragility?',
    'Longitudinal tracking of taboo erosion (emerging counterforce doctrines, tactical nuclear doctrine evolution, first-use policy shifts across nuclear powers); analysis of near-misses where taboo nearly broke; comparison of declared policies with implicit force posture (do capabilities match no-first-use commitments?).',
    'If taboo is durable: Rope and Piton perspectives are structurally sound; the constraint coordinates deterrence without credible threat. If taboo is fragile: Snare and revised-Tangled-Rope perspectives dominate; the constraint is unstable and war-prone. This omega separates the credibility_paradox_reading (taboo fragility makes paradox live) from the rational_dropout_reading (cost exceeds benefit regardless of credibility).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_unthinkability_stability, empirical, 'Durability and stability of institutional taboo on nuclear use').

omega_variable(
    committer_frame_kernel_contest,
    'Which sibling reading of the nuclear_impossibility_kernel is structurally correct: this credibility_paradox reading, the structural_contraction reading (mutual annihilation guaranteed), or the rational_dropout reading (costs exceed benefits)?',
    'The three readings instantiate different causal framings of the same constraint. This omega documents the contest itself. Credibility_paradox (this reading): deterrence is unstable because the use-threat is incredible; great powers seek usable options. Structural_contraction: deterrence is stable because mutual annihilation is logically impossible (mountain). Rational_dropout: deterrence is stable because costs exceed benefits (constraint is binding even if use-threat is credible). Resolution requires meta-analysis: which framework correctly models how nuclear powers actually make escalation decisions?',
    'Each reading produces different policy implications: credibility_paradox → great powers will seek usable options and war is reachable via escalation ladders. Structural_contraction → deterrence is immutable (mountain-level). Rational_dropout → deterrence is stable but contingent on costs remaining high.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_kernel_contest, conceptual, 'Kernel contest: which reading of nuclear_impossibility_kernel is correct').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__credibility_paradox_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_cred_tr_t0, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(nucl_cred_tr_t1975, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1975, 0.55).
narrative_ontology:measurement(nucl_cred_tr_t2000, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2000, 0.68).

% Extraction over time
narrative_ontology:measurement(nucl_cred_be_t0, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(nucl_cred_be_t1975, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1975, 0.58).
narrative_ontology:measurement(nucl_cred_be_t2000, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2000, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(nucl_cred_su_t0, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(nucl_cred_su_t1975, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1975, 0.65).
narrative_ontology:measurement(nucl_cred_su_t2000, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2000, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__credibility_paradox_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__rational_dropout_reading).

% DUAL FORMULATION NOTE:
% The nuclear_impossibility_kernel instantiates as three distinct constraint stories depending on the reading. The credibility_paradox_reading (this file) treats the paradox as institutionally contingent and unstable (snare with rising extractiveness). The structural_contraction_reading treats mutual annihilation as logically immutable (mountain). The rational_dropout_reading treats the constraint as stable via cost-benefit analysis (rope or tangled_rope). Each reading has its own epsilon and classification; together they map the space of possible framings of nuclear deterrence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nuclear_impossibility_kernel__credibility_paradox_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
