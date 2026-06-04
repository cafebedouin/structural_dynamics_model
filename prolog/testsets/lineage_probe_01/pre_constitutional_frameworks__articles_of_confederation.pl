% ============================================================================
% CONSTRAINT STORY: pre_constitutional_frameworks__articles_of_confederation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_articles_of_confederation, []).

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
 *   constraint_id: pre_constitutional_frameworks__articles_of_confederation
 *   human_readable: Articles of Confederation: League of Friendship Among Sovereign States
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The Articles of Confederation instantiate a pure coordination mechanism
 *   among sovereign states designed to achieve unified defense and foreign
 *   diplomacy without compromising state autonomy. The central body —
 *   Congress — is explicitly a congress of ambassadors with no taxing power,
 *   no enforcement power, and no ability to compel state compliance.
 *   Amendments require unanimity. The constraint solves the immediate
 *   post-revolutionary problem: thirteen states must act as one unit in
 *   international relations without surrendering sovereignty to a central
 *   government. However, the same structure that protects sovereignty creates
 *   structural victimization of those who depend on central action: creditors
 *   owed war debts Congress cannot pay, merchants facing state trade wars
 *   Congress cannot regulate, western settlers needing territorial governance
 *   Congress cannot enforce without state cooperation. The extractiveness is
 *   low by design but fatal in consequence — the constraint produces its own
 *   failure.
 *
 * KEY AGENTS:
 *   - State Governments: Primary beneficiary (powerful/mobile) — retain full sovereignty and voluntarily coordinate on defense and diplomacy without surrender of autonomy
 *   - Continental Creditors: Primary victim (moderate/trapped) — war debts remain unpaid because Congress has no taxing authority; no alternative collection mechanism exists
 *   - Confederation Congress: Institutional actor (institutional/constrained) — performs ceremonial/administrative functions but lacks enforcement power; institution degrades into theater
 *   - Large States (Virginia, Pennsylvania, New York): Secondary beneficiary/victim (powerful/mobile) — benefit from coordination, harmed by inability to expand commercial influence
 *   - Small States: Secondary beneficiary (powerful/mobile) — benefit from equal vote in Congress despite smaller population; capture disproportionate blocking power through unanimity rule
 *   - Interstate Commerce Parties: Victim group (moderate/trapped) — face state-level trade barriers Congress cannot regulate; each state pursues narrow advantage at collective cost
 *   - Analytical Observer: Sees potential natural law (analytical/analytical) — risks treating league-vs-nation boundary as logical necessity rather than design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pre_constitutional_frameworks__articles_of_confederation, 0.08).
domain_priors:suppression_score(pre_constitutional_frameworks__articles_of_confederation, 0.02).
domain_priors:theater_ratio(pre_constitutional_frameworks__articles_of_confederation, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pre_constitutional_frameworks__articles_of_confederation, extractiveness, 0.08).
narrative_ontology:constraint_metric(pre_constitutional_frameworks__articles_of_confederation, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(pre_constitutional_frameworks__articles_of_confederation, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pre_constitutional_frameworks__articles_of_confederation, rope).
narrative_ontology:human_readable(pre_constitutional_frameworks__articles_of_confederation, "Articles of Confederation: League of Friendship Among Sovereign States").
narrative_ontology:topic_domain(pre_constitutional_frameworks__articles_of_confederation, "political/constitutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(pre_constitutional_frameworks__articles_of_confederation, '100fa7ff-d63b-4cb1-bf90-687a25a80c87').
narrative_ontology:cs_kernel_codification('100fa7ff-d63b-4cb1-bf90-687a25a80c87', formalized).
narrative_ontology:cs_authority_grounding('100fa7ff-d63b-4cb1-bf90-687a25a80c87', distributed).
narrative_ontology:cs_reading_relation('100fa7ff-d63b-4cb1-bf90-687a25a80c87', pre_constitutional_frameworks__northwest_ordinance, coexists_with).
narrative_ontology:cs_axiom('100fa7ff-d63b-4cb1-bf90-687a25a80c87', foundational, state_sovereignty_is_indivisible).
narrative_ontology:cs_axiom_status(state_sovereignty_is_indivisible, holdable).
narrative_ontology:cs_axiom_grounding('100fa7ff-d63b-4cb1-bf90-687a25a80c87', state_sovereignty_is_indivisible, deontological).
narrative_ontology:cs_axiom('100fa7ff-d63b-4cb1-bf90-687a25a80c87', foundational, unanimity_preserves_consent).
narrative_ontology:cs_axiom_status(unanimity_preserves_consent, holdable).
narrative_ontology:cs_axiom_grounding('100fa7ff-d63b-4cb1-bf90-687a25a80c87', unanimity_preserves_consent, deontological).
narrative_ontology:cs_reference_frame('100fa7ff-d63b-4cb1-bf90-687a25a80c87', confederal_sovereignty_preservation).
narrative_ontology:cs_drift_state('100fa7ff-d63b-4cb1-bf90-687a25a80c87', post_revolutionary_constitutional_moment, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('100fa7ff-d63b-4cb1-bf90-687a25a80c87', '').
narrative_ontology:cs_kernel_id(pre_constitutional_frameworks__articles_of_confederation, pre_constitutional_frameworks).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pre_constitutional_frameworks__articles_of_confederation, state_governments).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATE GOVERNMENTS (ROPE) — States experience the Articles as a pure coordination mechanism solving the immediate problem of unified defense and foreign diplomacy without surrendering sovereignty. The constraint imposes minimal suppression (unanimity required for amendment, no coercive enforcement) and minimal extractiveness (no central taxation or wealth transfer). This is the constraint's native beneficiary perspective: states retain all power and voluntarily participate in coordination.
constraint_indexing:constraint_classification(pre_constitutional_frameworks__articles_of_confederation, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: CONTINENTAL CREDITORS AND INTERSTATE COMMERCE (SNARE) — War debts from the Revolution remain unpaid because Congress has no taxing power. Interstate trade warfare escalates because Congress cannot regulate commerce. Creditors and merchants are trapped: the constraint's design forbids the central authority from solving the problems that created the debt and trade chaos. High experienced extractiveness despite low base extractiveness, because the constraint's *structure* prevents remedy. The victim has no exit except leaving the continent or accepting perpetual disadvantage.
constraint_indexing:constraint_classification(pre_constitutional_frameworks__articles_of_confederation, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CONFEDERATION CONGRESS (PITON) — Congress sees itself as an administrative body carrying out coordination decisions made unanimously by sovereign states. The actual function (processing appointments, issuing certificates, managing correspondence) is largely performative — Congress cannot execute policy, raise revenue, or compel compliance. The institution persists through inertia and habit despite low functional capacity. Theater ratio high because the ritual of congressional deliberation continues despite the powerlessness. This is a degraded institution that has lost the power it needs to perform its assigned role.
constraint_indexing:constraint_classification(pre_constitutional_frameworks__articles_of_confederation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE STATES / GENERATIONAL VIEW (TANGLED ROPE) — At the generational horizon, large states (Virginia, Pennsylvania, New York) recognize that the Articles provide genuine coordination for defense and foreign policy while simultaneously constraining their own growth and commercial expansion. The constraint benefits them (military coordination) and extracts from them (inability to expand influence through commercial regulation). At longer timescales, the gap between their power and their authority becomes structurally intolerable. This reading shows coordination and asymmetric extraction coexisting.
constraint_indexing:constraint_classification(pre_constitutional_frameworks__articles_of_confederation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: FOREIGN POWERS / CIVILIZATIONAL VIEW (ROPE) — From the international perspective, the Articles of Confederation represent a pure coordination solution to a collective action problem: thirteen newly independent states must present a unified front to European powers while respecting mutual sovereignty. The constraint solves the immediate problem (coordinated defense, unified diplomatic voice) with minimal coercive overhead. This is a rope at the civilizational timescale because it enables the continental polity to exist as a negotiating unit.
constraint_indexing:constraint_classification(pre_constitutional_frameworks__articles_of_confederation, rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LIMITS VIEW (MOUNTAIN) — From an analytical/universal perspective, the Articles might appear to instantiate a natural law: any voluntary coalition of sovereign states faces inherent limits on central authority — you cannot have both true sovereignty and central command. The constraint would be immutable by the logic of political philosophy itself. However, the kernel context and false summit detection will reveal this as naturalization of a contingent institutional choice. The analytical position risks hiding the political contest beneath an apparent logical necessity.
constraint_indexing:constraint_classification(pre_constitutional_frameworks__articles_of_confederation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pre_constitutional_frameworks__articles_of_confederation_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(pre_constitutional_frameworks__articles_of_confederation, TR),
    TR >= 0.70.

:- end_tests(pre_constitutional_frameworks__articles_of_confederation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint is designed to impose minimal extraction — no central taxation, no wealth transfer, no coercive power. States retain all sovereignty and benefit from voluntary coordination. However, extractiveness rises slightly over the interval (0.03 → 0.10) as the inability to solve coordination failures becomes visible: unpaid war debts accumulate, state trade wars intensify, and western settlers face governance vacuum. The rising trajectory reflects that the constraint's minimal extractiveness is purchased at the cost of functional capacity — extraction is low because the center is powerless, not because extraction is genuinely absent. Suppression (0.02): Negligible. The constraint explicitly forbids central suppression — unanimity for amendment means no state can be overruled, and Congress has no enforcement machinery. This is the opposite of a snare: it is structurally anti-suppressive. Theater ratio (0.15): Very low, rising slightly. The constraint's actual functional requirement is minimal: Congress must coordinate diplomatic representation and process some administrative decisions. The theater is low because the roles are genuinely lightweight. However, as Congress's inability to enforce grows more visible, the performative element rises (0.08 → 0.15) — Congress continues deliberating and passing resolutions that states ignore, maintaining the ritual of authority despite powerlessness.
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer perspective risks naturalizing a contingent institutional choice as a logical necessity ('sovereignty is binary; central authority is impossible'). This false summit hides the political contest — the Articles represent a design choice made by state governments who benefited from preserved sovereignty. The constraint is not a natural law but a negotiated outcome that served some actors' interests while harming others. The false summit detection mechanism should flag this perspective as revealing the naturalization pattern.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: State governments are beneficiaries with mobile exit (derived d ≈ 0.20, f(d) ≈ -0.01, negative/minimal χ). Creditors and merchants are victims with trapped exit (derived d ≈ 0.95, f(d) ≈ 1.42, maximum χ). Congress as an institutional actor with constrained exit sits in between, experiencing the constraint as performative degradation rather than extraction. The perspectival gap between state governments (rope) and creditors (snare) is driven entirely by exit options and victim status, not by power level.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not trigger mandatrophy: extractiveness is well below the 0.46 threshold. The constraint is genuinely a rope for its beneficiaries and genuinely a snare for its victims, with the gap explained entirely by structural position, not by ambiguity in classification. The instructive pattern here is that pure coordination can produce severe victimization of parties excluded from the coordination mechanism — the constraint solves the right problem for the right people and creates catastrophic externalities for everyone else. The mandatrophy would activate if extractiveness rose above 0.46 (which it does not) or if the analytical observer's mountain classification were challenged by empirical evidence that the league-vs-nation boundary is not logically immutable but rather a design choice. The kernel context provides exactly that challenge: the sibling reading (Northwest Ordinance) shows the Articles succeeding at something the league/nation binary would say is impossible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    league_vs_nation_boundary,
    'Is the Articles of Confederation a league of sovereign states fundamentally incompatible with national governance, or a temporary coordination mechanism that can be reformed toward nationalism?',
    'Clarification of what would count as ''national'' authority versus ''confederal'' authority. If the boundary is logical/immutable (sovereignty is binary), then nation-building requires the Articles to fail. If the boundary is conventional (states can delegate specific powers), then reform without collapse is possible.',
    'If logical boundary: the constraint is immutable and catastrophic failure is predetermined. If conventional boundary: the constraint is a design choice that can be revised by consensus or superseded by new institution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(league_vs_nation_boundary, conceptual, 'Whether league/nation boundary is logical or conventional').

omega_variable(
    unanimity_requirement_vs_coordination_failure,
    'Does the unanimity requirement for amendment represent genuine consensus-based coordination, or does it function as a tyranny of the minority that prevents collective action?',
    'Historical analysis of blocked amendments and their content. Count proposals that benefited the coalition as a whole but were blocked by one state acting in narrow self-interest. Distinguish between principled disagreement (state protecting legitimate sovereignty) and extractive blocking (state vetoing for leverage).',
    'If genuine consensus coordination: the constraint is a rope that honors all voices. If tyranny of the minority: the constraint is a snare that grants veto power to extractive actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unanimity_requirement_vs_coordination_failure, empirical, 'Whether unanimity requirement blocks legitimate collective action').

omega_variable(
    reading_kernel_contest,
    'Is the defining feature of the pre-constitutional framework the Articles'' weak central authority (this reading), or the Northwest Ordinance''s territorial governance achievement (sibling reading)?',
    'Historical narrative: if the Articles are remembered as failed coordination that necessitated constitutional rewrite, this reading is accurate. If the Articles are remembered as successfully creating the territorial infrastructure (Northwest Ordinance) that became the constitutional nation-state, the sibling reading is dominant.',
    'If this reading: Articles are a failed constraint that produced no stable institutional form. If sibling: Articles are a temporary but functionally significant framework that solved specific problems (territorial governance) despite failing at financial coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Kernel contest: Articles as failed coordination vs. Northwest Ordinance as constitutional achievement').

omega_variable(
    extractiveness_asymmetry_across_states,
    'Does the constraint extract equally from all states, or do small states benefit from blocking power while large states bear costs of coordination failure?',
    'Directional analysis: which states blocked amendments? Which states benefited from trade barriers? Which states paid debts? Compute per-state extraction asymmetry — if small states captured disproportionate blocking power, extractiveness is state-indexed, not uniform.',
    'If symmetric: constraint is a rope for all states equally. If asymmetric: constraint is a snare for large states / rope for small states — perspectival gap is pronounced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_asymmetry_across_states, empirical, 'Whether constraint extracts equally across states or produces asymmetric burdens').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pre_constitutional_frameworks__articles_of_confederation, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aoc_tr_t0, pre_constitutional_frameworks__articles_of_confederation, theater_ratio, 0, 0.08).
narrative_ontology:measurement(aoc_tr_t2, pre_constitutional_frameworks__articles_of_confederation, theater_ratio, 2, 0.12).
narrative_ontology:measurement(aoc_tr_t4, pre_constitutional_frameworks__articles_of_confederation, theater_ratio, 4, 0.15).

% Extraction over time
narrative_ontology:measurement(aoc_be_t0, pre_constitutional_frameworks__articles_of_confederation, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(aoc_be_t2, pre_constitutional_frameworks__articles_of_confederation, base_extractiveness, 2, 0.06).
narrative_ontology:measurement(aoc_be_t4, pre_constitutional_frameworks__articles_of_confederation, base_extractiveness, 4, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pre_constitutional_frameworks__articles_of_confederation, enforcement_mechanism).
narrative_ontology:affects_constraint(pre_constitutional_frameworks__articles_of_confederation, northwest_ordinance).

% DUAL FORMULATION NOTE:
% The pre_constitutional_frameworks kernel contains two readings. articles_of_confederation emphasizes weak central authority and design intent (forbidding taxation and coercion). northwest_ordinance emphasizes territorial governance achievement and the enduring institution that survived the Articles' financial collapse. The readings are not contradictory but emphasis-split: one foregrounds the design constraint, the other foregrounds the consequential success. Both operate within the same formal structure (Congress of ambassadors). Network linking: the Articles reading affects the Northwest reading because the territorial mechanism was achieved despite the Articles' financial powerlessness — the constraint's effects on territorial expansion are downstream of the sovereignty-preservation design.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
