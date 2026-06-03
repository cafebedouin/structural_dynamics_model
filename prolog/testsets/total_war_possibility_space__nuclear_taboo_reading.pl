% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_nuclear_taboo, []).

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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Total War Norm Prohibition (Nuclear Taboo Reading)
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   This constraint story instantiates the 'nuclear taboo' reading of the
 *   contested kernel 'total_war_possibility_space'. The reading claims that
 *   total war became normatively prohibited through constructed taboo,
 *   independent of material capability. War remains materially possible —
 *   states possess the weapons and strategic doctrines to wage unlimited
 *   conflict — but has become normatively foreclosed through great-power norm
 *   entrepreneurship, non-proliferation regimes, no-first-use pledges, and
 *   the internalization of the taboo into strategic cultures. The constraint
 *   is tangled rope: it coordinates conflict limitation (genuine coordination
 *   benefit) while also extracting from states denied strategic options and
 *   enforcing asymmetric norm-definition (benefiting the norm entrepreneurs).
 *   The theater_ratio (0.65) reflects that much enforcement is rhetorical and
 *   norm-based rather than material: the NPT's verification capacity is
 *   limited, treaty commitments are routinely violated, and the mechanism
 *   persists through institutional inertia and great-power preference
 *   coordination rather than ironclad enforcement. The measurement trajectory
 *   shows rising extractiveness (0.18 → 0.39) and rising theater (0.35 →
 *   0.65) over 75 years, indicating that as the norm has matured and
 *   enforcement institutionalized, the performative aspect has grown while
 *   the genuine coordination benefit has been somewhat eclipsed by the
 *   norm-enforcement apparatus itself.
 *
 * KEY AGENTS:
 *   - Norm-Entrepreneur Great Powers (institutional/arbitrage): US, Soviet Union/Russia, France, UK — defined and maintained the taboo; beneficiaries of the norm system; possess arbitrage options (can violate or reshape norms)
 *   - Non-Proliferation Regime (institutional/arbitrage): IAEA, NPT signatories, security-guarantee providers — institutional actors that enforce the taboo; benefit from norm maintenance; constrained by limited verification capacity
 *   - Norm-Denied States (powerless/trapped): States that would benefit from total-war option (numerically superior but technologically inferior) but face normative prohibition and diplomatic isolation; cannot exit the norm system
 *   - Regional Powers Coalition (organized/constrained): States that benefit from reduced major-power intervention risk but are constrained by inability to threaten existential escalation as deterrent
 *   - Identity-Locked Proliferator States (powerless/identity_locked): States whose national identity fused with taboo-breaking project; structurally mobile but identity-constituted through norm resistance
 *   - Analytical Observer: Sees the constraint as emergent from physics (mountain perspective) or as coordination mechanism (rope perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.38).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.62).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Total War Norm Prohibition (Nuclear Taboo Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, '84794388-52f1-4f6d-8fe2-f90966b9725d').
narrative_ontology:cs_kernel_codification('84794388-52f1-4f6d-8fe2-f90966b9725d', distributed).
narrative_ontology:cs_authority_grounding('84794388-52f1-4f6d-8fe2-f90966b9725d', extraction).
narrative_ontology:cs_interpretation_layer_present('84794388-52f1-4f6d-8fe2-f90966b9725d').
narrative_ontology:cs_reading_relation('84794388-52f1-4f6d-8fe2-f90966b9725d', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('84794388-52f1-4f6d-8fe2-f90966b9725d', total_war_possibility_space__space_contraction_reading, influences).
narrative_ontology:cs_axiom('84794388-52f1-4f6d-8fe2-f90966b9725d', foundational, total_war_normatively_constructed_not_inevitable).
narrative_ontology:cs_axiom_status(total_war_normatively_constructed_not_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('84794388-52f1-4f6d-8fe2-f90966b9725d', total_war_normatively_constructed_not_inevitable, conventional).
narrative_ontology:cs_axiom('84794388-52f1-4f6d-8fe2-f90966b9725d', secondary, taboo_dependent_on_norm_entrepreneur_maintenance).
narrative_ontology:cs_axiom_status(taboo_dependent_on_norm_entrepreneur_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('84794388-52f1-4f6d-8fe2-f90966b9725d', taboo_dependent_on_norm_entrepreneur_maintenance, empirically_contingent).
narrative_ontology:cs_reference_frame('84794388-52f1-4f6d-8fe2-f90966b9725d', taboo_constituted_normative_prohibition).
narrative_ontology:cs_drift_state('84794388-52f1-4f6d-8fe2-f90966b9725d', contemporary_great_power_divergence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('84794388-52f1-4f6d-8fe2-f90966b9725d', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurs).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_great_powers).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, states_denied_strategic_options).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, non_state_actors_excluded_from_norm_system).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NORM-DENIED STATE (SNARE) — A state that would benefit from total war option (numerically superior but technologically inferior) but faces normative prohibition enforced by the non-proliferation regime, security guarantees, and diplomatic isolation. Cannot exit the norm system; bears full cost of strategic constraint. Experiences the constraint as coercive, not consensual. No self-exit from taboo without state-level regime collapse.
constraint_indexing:constraint_classification(total_war_possibility_space__nuclear_taboo_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL POWERS COALITION (TANGLED ROPE) — Organized states that benefit from the taboo (reduced major-power intervention risk) while also constrained by it (unable to threaten existential escalation as deterrent). Experiences both coordination function (the taboo prevents devastating regional wars) and asymmetric extraction (norm entrepreneurs define what options remain available). Can organize against the norm through proliferation or norm-violation rhetoric, but faces severe diplomatic and military costs. Mixed experience — genuine coordination benefit alongside real constraint.
constraint_indexing:constraint_classification(total_war_possibility_space__nuclear_taboo_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NORM-ENTREPRENEUR GREAT POWERS (ROPE) — Nuclear-armed or nuclearized states that benefit from defining and maintaining the taboo. Experience the constraint as coordination: the taboo ensures that conflicts remain limited and prevents the catastrophic outcome (peer nuclear war) that would harm them most severely. Possess arbitrage options (can violate norms, change proliferation policies, declare no-first-use or withdraw from it). Net beneficiaries — the norm system coordinates their security preferences.
constraint_indexing:constraint_classification(total_war_possibility_space__nuclear_taboo_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NPT INSTITUTIONAL APPARATUS (PITON) — The treaty structure, inspection regimes, and diplomatic machinery that sustains the taboo. Theater_ratio is high (0.65) because much of the enforcement is rhetorical and norm-based rather than material. The NPT's verification capacity is limited; the mechanism persists through institutional inertia and status-quo preference by great powers. The apparatus sees its own role as degraded — inspection cannot prevent determined proliferation, treaty commitments are routinely violated (Iran, North Korea, Pakistan), and the system survives because no superior alternative has emerged, not because it functions effectively.
constraint_indexing:constraint_classification(total_war_possibility_space__nuclear_taboo_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: IDENTITY-LOCKED PROLIFERATOR STATE (SNARE) — A state whose national identity and security doctrine have fused with the taboo-breaking project (e.g., 'we are the Islamic Republic pursuing the peaceful nuclear program despite imperialist prohibition'). Structurally mobile (could cease pursuit, could accept international inspections), but identity-constituted through resistance to the norm. Exit would require abandoning the identity frame that makes the state itself comprehensible to its own decision-makers. Experiences the taboo as the primary target of state action, not a background constraint. High extraction — the state is defined against the norm it resists.
constraint_indexing:constraint_classification(total_war_possibility_space__nuclear_taboo_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the taboo against total war emerges as an irreducible property of the post-nuclear strategic environment: the material capability to end civilization creates a natural prohibition that no agent can override without self-annihilation. This reading sees the norm as emergent from physics, not constructed. However, this perspective risks false-summit classification — the taboo is maintained through active norm entrepreneurship, great-power preference coordination, and institutional enforcement, not merely physics.
constraint_indexing:constraint_classification(total_war_possibility_space__nuclear_taboo_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / COORDINATION VIEW (ROPE) — From a generational analytical perspective, the total-war taboo is fundamentally a coordination mechanism: great powers have collectively learned that peer conflict escalation triggers mutual destruction, and the norm system coordinates their shared preference for conflict limitation. The taboo works because it solves a coordination problem (how to fight while preserving civilization), not because it is immutable. This reading sees the constraint as maintained through active preference alignment, not inertia — it could persist indefinitely if the coordination problem remains unsolved.
constraint_indexing:constraint_classification(total_war_possibility_space__nuclear_taboo_reading, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(total_war_possibility_space__nuclear_taboo_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(total_war_possibility_space__nuclear_taboo_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, TR),
    TR >= 0.70.

:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The reading assesses that the taboo extracts from norm-denied states (unable to exercise strategic options) while coordinating conflict limitation for others. The moderate value reflects that the extraction is real but not maximal — states can organize around the taboo (proliferation, norm-violation rhetoric, regional powers coalitions), and the taboo has internalized enough into strategic cultures that much of its force is self-enforcing rather than externally imposed. The rising trajectory (0.18 → 0.39) indicates that as norm-enforcement institutionalized (NPT, IAEA, security guarantees), the extractive machinery became more sophisticated and pervasive. Suppression (0.62): Moderate-high. Significant barriers to exercising the total-war option include non-proliferation regimes, security guarantees (NATO, extended deterrence), diplomatic isolation, and the genuine strategic risk that escalation triggers mutual destruction. These barriers are structural and enforced through institutional mechanisms. However, suppression is not total — some states can and do pursue proliferation, and all states retain the formal option to wage unlimited conflict (the taboo is normative, not legal). Theater ratio (0.65): Moderate-high. Enforcement is substantially performative: NPT verification cannot prevent determined proliferation (Iran, Pakistan, North Korea), treaty commitments are routinely violated, and the system survives because great powers prefer the status quo and no superior alternative has emerged, not because inspection regimes work reliably. The rising trajectory reflects that as nuclear capabilities proliferated and the number of actors increased, enforcement became increasingly rhetorical and increasingly dependent on great-power coordination rather than on technical verification.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. The norm-entrepreneur great powers see coordination (Rope) — the taboo solves their collective action problem and ensures conflicts remain limited. The norm-denied states see pure extraction (Snare) — they are denied strategic options without compensation or voice in norm-definition. The regional powers coalition sees mixed coordination and extraction (Tangled Rope) — they benefit from reduced intervention risk but are constrained by inability to threaten existential escalation. The identity-locked proliferator sees snare with identity fusion (Snare with identity_locked exit) — the state is defined through resistance to the taboo. The NPT institutional apparatus sees its own degradation (Piton) — verification fails routinely, treaties are violated, the mechanism persists through inertia. The analytical observer risks seeing an immutable natural law (Mountain) — the taboo emerges from physics and is unbreakable — but the structural data reveals this as a false summit: the taboo is actively maintained through norm entrepreneurship, great-power preference coordination, and institutional enforcement, contingent on ongoing actor commitment.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) reflects each agent's structural relationship to the total-war option and the taboo system. Norm-entrepreneur great powers (d ≈ 0.15, institutional/arbitrage) are beneficiaries of the taboo and possess arbitrage options — they can maintain, reshape, or violate the norm at a cost they can bear (sanctions are limited because they are too powerful). Norm-denied states (d ≈ 0.92, powerless/trapped) face high barriers to exit and are denied their preferred strategic option — they experience maximum extraction. Regional powers (d ≈ 0.65, organized/constrained) have moderate barriers to exit (organization, regional arms races) and experience both benefit (reduced intervention) and extraction (limited options). Identity-locked proliferators (d ≈ 0.88, powerless/identity_locked) are structurally mobile but identity-constituted through resistance — their d is derived from victim status + identity lock, producing high effective extraction even though some structural mobility exists. The readings' ability to produce different perspectival classifications from the same base metrics confirms that directionality is operative and produces meaningful differentiation.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: The reading resolves the mandatrophy by showing that the total-war taboo is fundamentally a mix of coordination (genuine conflict limitation) and extraction (norm-denial to states that would benefit from strategic options). The constraint cannot be classified as pure rope (only coordination) because the norm-definition asymmetry and enforcement mechanisms create real extraction. It cannot be classified as pure snare (only extraction) because the taboo solves a genuine coordination problem — the great powers have collectively benefited from conflict limitation and the reduction in mutual destruction risk. The tangled_rope classification accurately captures both functions and reflects that the reading sees the taboo as a functional coordination mechanism that has become layered with extraction as the enforcement apparatus has institutionalized. The piton perspective (institutional/arbitrage) offers diagnostic value: the NPT apparatus sees its own theatrical nature because verification fails routinely, treaty commitments are violated, and the mechanism persists through inertia. This signals that the taboo's enforcement is increasingly rhetorical rather than material, which supports the reading's prediction that the taboo is fragile if norm entrepreneurs exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    norm_construction_vs_material_causation,
    'Is the taboo against total war constructed through norm entrepreneurship and social transmission, or does it emerge inevitably from material nuclear capability?',
    'Historical counterfactual: Do non-nuclear states exhibit equivalent taboos against total war? Do states with nuclear capability but weak norm-internalization (e.g., early cold-war strategic doctrine) exhibit taboo behavior? Comparative analysis of norm-following states with varying material incentives.',
    'If constructed: the taboo is contingent on ongoing norm maintenance and will weaken if norm entrepreneurs exit (supports reading). If material: the taboo is robust even if norms degrade (supports false-summit mountain perspective, threatens reading validity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_construction_vs_material_causation, empirical, 'Whether taboo is constructed or materially emergent').

omega_variable(
    norm_entrepreneur_dependence,
    'How dependent is the taboo''s enforcement on the continued commitment of specific great powers (norm entrepreneurs)? Would the taboo persist if the US or Russia withdrew from non-proliferation commitments?',
    'Institutional analysis of norm-enforcement capacity without great-power backing. Historical precedent: how quickly did taboos collapse when enforcement actors withdrew (e.g., Kellogg-Briand Pact, 1939). Measurement of norm-enforcement mechanisms by actor type.',
    'If highly dependent: the taboo is fragile and specific to this power configuration (reading accurate). If robust to great-power exit: the taboo has achieved sufficient norm-internalization to persist without institutional enforcement (reading partially undermined).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_entrepreneur_dependence, empirical, 'Institutional dependence of taboo on norm-entrepreneur commitment').

omega_variable(
    sibling_reading_empirical_resolution,
    'Can the three readings (deterrence equilibrium, nuclear taboo, space contraction) be empirically distinguished, or do they make identical predictions?',
    'Prediction divergence under counterfactual: (1) If deterrence equilibrium is true, disarmament while maintaining capability comparison should preserve deterrence. (2) If nuclear taboo is true, disarmament should weaken the taboo and increase war risk even if capability ratios remain constant. (3) If space contraction is true, increasing capability asymmetry (e.g., missile defense breakthrough) should restore total war as thinkable even if taboo rhetoric persists.',
    'If empirically distinguishable: each reading is a falsifiable claim; this reading''s epistemic status is measurable. If predictions converge: the readings are equivalent ways of describing the same mechanism, and the choice between them is conventional rather than factual.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_empirical_resolution, empirical, 'Empirical distinguishability of kernel readings').

omega_variable(
    norm_internalization_depth,
    'To what degree has the taboo against total war been internalized by strategic elites, vs. externally imposed through norm-enforcement mechanisms (treaties, sanctions, isolation)?',
    'Analysis of strategic doctrine evolution: do military strategists treat total war as genuinely unthinkable, or merely as a high-cost option they choose not to exercise? Measurement of norm-violation rhetoric vs. norm-violation behavior. Examination of secret strategic plans that reveal true preferences unconstrained by public norms.',
    'If deeply internalized: the taboo is robust even if enforcement mechanisms degrade. If externally imposed: the taboo requires continuous institutional support and will collapse if enforcement capacity declines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_internalization_depth, empirical, 'Depth of internalization of total-war taboo among strategic elites').

omega_variable(
    non_nuclear_state_constraint_structure,
    'Do non-nuclear states experience the total-war taboo as a constraint, or only as a description of great-power behavior? Is the taboo genuinely global, or is it primarily a constraint on nuclear-armed states?',
    'Comparative analysis: do non-nuclear regional powers (India/Pakistan non-nuclear period, conventional-only Middle Eastern conflicts) exhibit equivalent taboos against unlimited war? Has the norm prevented total-war strategies among states lacking nuclear deterrent?',
    'If genuinely global: the reading correctly identifies a universal norm. If nuclear-specific: the reading should be reformulated as ''taboo on nuclear war'' (distinct from ''taboo on total war''), and the space_contraction reading becomes more accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_nuclear_state_constraint_structure, empirical, 'Whether taboo on total war extends to non-nuclear states').

omega_variable(
    norm_entrepreneur_identity_vs_institutional_role,
    'Are norm entrepreneurs enforcing the taboo because they believe in the norm''s intrinsic value, or because the taboo serves their material interests? Can we distinguish genuinely held norms from strategic norm-deployment?',
    'Analysis of norm-entrepreneur behavior under counterfactual interest shifts: if material conditions changed such that total-war option became advantageous to a norm entrepreneur, would they abandon the norm? Historical cases of norm abandonment by their original champions (Munich appeasement, NATO expansion rhetoric).',
    'If genuinely held: norms are robust to interest shifts. If strategic: the taboo is contingent on alignment between norm-entrepreneur interests and taboo maintenance, and will collapse if interests diverge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_entrepreneur_identity_vs_institutional_role, empirical, 'Distinction between genuine norm-belief and strategic norm-deployment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twnt_theater_1945, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(twnt_theater_1970, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement(twnt_theater_1995, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 50, 0.62).
narrative_ontology:measurement(twnt_theater_2020, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 75, 0.65).

% Extraction over time
narrative_ontology:measurement(twnt_extractiveness_1945, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(twnt_extractiveness_1970, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement(twnt_extractiveness_1995, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(twnt_extractiveness_2020, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 75, 0.39).

% Suppression requirement over time
narrative_ontology:measurement(twnt_suppression_1945, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(twnt_suppression_1970, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement(twnt_suppression_1995, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement(twnt_suppression_2020, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 75, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space__space_contraction_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, nuclear_proliferation_extraction).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, great_power_concert_coordination).

% DUAL FORMULATION NOTE:
% The total_war_possibility_space kernel has three structurally distinct readings, each producing different classification and different empirical predictions. This reading (nuclear_taboo_reading) claims the constraint is tangled_rope maintained through normative construction. The sibling readings claim deterrence_equilibrium (rope) and space_contraction (mountain or snare depending on perspective). Each reading should be authored as a separate constraint story with its own epsilon and perspectives, linked via network.affects_constraints to show they are competing interpretations of the same underlying phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
