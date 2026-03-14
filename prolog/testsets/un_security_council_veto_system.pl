% ============================================================================
% CONSTRAINT STORY: un_security_council_veto_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_un_security_council_veto_system, []).

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
 *   constraint_id: un_security_council_veto_system
 *   human_readable: UN Security Council Veto System
 *   domain: international_governance/geopolitics
 *
 * SUMMARY:
 *   The UN Security Council veto system represents a foundational constraint
 *   in international governance — a hybrid coordination-extraction mechanism
 *   that has evolved from a post-WWII great-power consensus-building device
 *   into an increasingly asymmetric blocking tool. Established by the UN
 *   Charter (1945) to ensure great-power participation in a universal
 *   security system, the veto grants five permanent members (US, Russia,
 *   China, France, UK) the unilateral power to block any substantive Council
 *   resolution. This structure exhibits all six constraint types from
 *   different structural positions, revealing how institutional design
 *   naturalizes power asymmetry as coordination necessity. The constraint's
 *   extractiveness has risen from 0.35 (1945, when the P5 were more aligned)
 *   to 0.62 (2020, amid great-power fragmentation), while theater ratio has
 *   nearly doubled as Council deliberations have become increasingly
 *   performative — votes are cast knowing they will fail, diplomatic language
 *   becomes coded, and the institution persists through ritual rather than
 *   function. The constraint suppresses alternative governance pathways:
 *   states cannot bypass the Council through General Assembly authorization
 *   without confronting the precedent that a universal system might not
 *   require great-power consensus; they cannot form competing security
 *   arrangements without fragmenting the post-1945 international order; they
 *   cannot exit through unilateral action without legitimacy costs. Yet the
 *   veto also coordinates: it has prevented great-power wars, protected
 *   smaller states from great-power domination (through P5 agreement to
 *   protect them), and forced multilateral deliberation on major conflicts.
 *   The constraint is structurally a tangled rope — genuine coordination
 *   function (great-power buy-in, multilateral deliberation,
 *   consensus-forcing) plus asymmetric extraction (P5 blocking power, ability
 *   to protect allies, shield from judgment).
 *
 * KEY AGENTS:
 *   - Permanent Members (US, Russia, China, France, UK): Institutional/arbitrage beneficiaries — retain veto power, extract blocking privilege and protection for allies
 *   - Non-Permanent Members: Powerless/trapped victims — participate in deliberations but lack decision-making power
 *   - Vetoed States: Powerless/trapped victims — cannot access Council authorization for legitimate intervention or peacekeeping
 *   - Global Conflict Resolution: Abstract victim (powerless/trapped) — humanitarian crises, genocides, and interstate conflicts blocked by vetoes; no formal representation but systemically harmed
 *   - Reform Coalitions: Organized/constrained actors — civil society, humanitarian orgs, mid-size powers pushing for veto limits; see sunset mechanisms (Uniting for Peace precedent, norm shifts, abstention practices)
 *   - UN Institution: Institutional/constrained — maintains veto through institutional inertia; sees own deliberative function degrading
 *   - Analytical Observer: Civilizational perspective — risks naturalizing P5 privilege as inherent necessity in universal systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(un_security_council_veto_system, 0.58).
domain_priors:suppression_score(un_security_council_veto_system, 0.65).
domain_priors:theater_ratio(un_security_council_veto_system, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(un_security_council_veto_system, extractiveness, 0.58).
narrative_ontology:constraint_metric(un_security_council_veto_system, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(un_security_council_veto_system, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(un_security_council_veto_system, tangled_rope).
narrative_ontology:human_readable(un_security_council_veto_system, "UN Security Council Veto System").
narrative_ontology:topic_domain(un_security_council_veto_system, "international_governance/geopolitics").

domain_priors:requires_active_enforcement(un_security_council_veto_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(un_security_council_veto_system, permanent_members).
narrative_ontology:constraint_beneficiary(un_security_council_veto_system, great_powers).
narrative_ontology:constraint_victim(un_security_council_veto_system, non_permanent_members).
narrative_ontology:constraint_victim(un_security_council_veto_system, small_states).
narrative_ontology:constraint_victim(un_security_council_veto_system, intervention_targets).
narrative_ontology:constraint_victim(un_security_council_veto_system, global_conflict_resolution).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VETOED STATE (SNARE) — A state facing military aggression or humanitarian crisis cannot obtain Security Council authorization for intervention or peacekeeping. The veto traps them without alternatives. Exit is impossible: they cannot withdraw from the UN, cannot bypass UNSC approval for legitimate multilateral action, and cannot access the coordination mechanism they were promised upon joining. Maximum suppression (0.65) applies — no legal alternatives exist; unilateral action is deemed illegitimate without UNSC cover.
constraint_indexing:constraint_classification(un_security_council_veto_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-PERMANENT MEMBERS (SNARE) — Fifteen non-permanent members serve two-year terms on the Council but have no veto power. They participate in deliberations but cannot prevent or authorize action without alignment with permanent members. They are trapped in a pseudo-decision-making body where their formal vote is meaningless if a P5 member disagrees. The suppression is structural: they cannot exit the arrangement without abandoning UN membership.
constraint_indexing:constraint_classification(un_security_council_veto_system, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: PERMANENT MEMBER — BENEFICIARY (ROPE) — A permanent member experiences the veto as coordination: the system protects their interests while enabling multilateral action. Exit is available (de facto: a P5 member can ignore UNSC decisions; legally: withdrawal is possible under Article 60). The veto serves as a coordination device ensuring great-power consensus on major interventions. Effective extraction is low because the beneficiary can arbitrage — they have the power to shape outcomes through the veto itself.
constraint_indexing:constraint_classification(un_security_council_veto_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PERMANENT MEMBER — CONSTRAINED (TANGLED ROPE) — A permanent member benefits from the veto but faces escalating reputational costs (legitimacy erosion, soft power loss) when blocking humanitarian action or conflict resolution. The veto is both a coordination tool (enables great-power consensus) and an extraction mechanism (concentrates decision-making power asymmetrically). The member experiences constraint because abandoning the veto would signal weakness, but using it too often degrades international legitimacy. This perspective shows the hybrid nature: genuine coordination function plus asymmetric extraction benefit.
constraint_indexing:constraint_classification(un_security_council_veto_system, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REFORM ADVOCATES (SCAFFOLD) — Civil society, humanitarian organizations, and mid-size powers see the veto as a temporary coordination failure with potential sunset pathways. Proposals for limiting veto use (Uniting for Peace, abstention votes, humanitarian carve-outs) represent organizational strategies to dissolve the constraint. High suppression currently (0.65) but declining as reform coalitions organize. Theater ratio is moderate (0.48) because reform efforts themselves are partly performative (they face permanent member opposition) but partly functional (they shift norms and accountability). Sunset mechanism: if P5 consensus shifts on veto scope, the constraint weakens rapidly.
constraint_indexing:constraint_classification(un_security_council_veto_system, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: UN INSTITUTION (PITON) — From the civilizational perspective, the UN system maintains the veto through institutional inertia despite degraded legitimacy and coordination function. The veto was a post-WWII coordination mechanism ensuring great-power buy-in to a universal security system. Today it primarily serves to block action (extractive function) rather than enable consensus (coordination function). Theater ratio is moderate-low (0.48) because the veto still shapes real decisions, but the institutional system's primary activity is now theater: debates, condemnations, and symbolic votes that go nowhere. The institution persists because replacing it requires P5 consensus — which the veto mechanism itself prevents.
constraint_indexing:constraint_classification(un_security_council_veto_system, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — MOUNTAIN (FALSE SUMMIT) — From the analytical/universal perspective, one might claim the veto is an immutable natural law of multilateral governance: great powers will never accept security decisions that override their interests, therefore some veto mechanism is inevitable in any universal system. However, this perspective naturalize a contingent institutional design. The structural data shows this is not a mountain — the veto's power derives from specific institutional rules, not from laws of nature. The false summit detection fires: this is a snare or tangled rope masquerading as a law.
constraint_indexing:constraint_classification(un_security_council_veto_system, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(un_security_council_veto_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(un_security_council_veto_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(un_security_council_veto_system, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(un_security_council_veto_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(un_security_council_veto_system, TR),
    TR >= 0.70.

:- end_tests(un_security_council_veto_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, and rising. The base value reflects the P5's asymmetric decision-making power — they can block any action, veto is costless in formal terms, and they use it to shield allies or block actions against themselves. However, extractiveness is not maximal (0.70+) because the veto also imposes legitimacy costs, and some P5 members show constraint from reputational damage. The measurement trajectory (0.35 → 0.62 over 75 years) reflects the shift from post-WWII alignment (P5 rarely vetoed) to contemporary fragmentation (vetoes are frequent and contentious). Suppression (0.65): High. Trapped states have no legal alternatives to UNSC action; unilateral intervention is delegitimized without Council cover; alternative security arrangements (NATO, regional bodies) exist but do not replace the universal legitimacy UNSC provides. Theater ratio (0.48): Moderate and rising. Council deliberations remain substantive (voting outcomes affect real policy) but increasingly performative (speeches coded with diplomatic language, votes cast knowing they will fail, ritual maintained despite degraded function). The theater ratio rise (0.25 → 0.48) reflects this degradation. Claimed type (Tangled Rope): The constraint coordinates great-power consensus (genuine function) while enabling asymmetric extraction (P5 blocking privilege). Both features are essential: remove coordination function and it becomes pure snare; remove extraction and it becomes pure coordination rope. The active enforcement requirement is met: the veto must be formally maintained and invoked to function — it does not emerge naturally.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same institutional mechanism appears as coordination to beneficiaries and extraction to victims. The P5's Rope perspective is genuine from their position: the veto ensures their security, enables multilateral deliberation, and prevents great-power wars. The vetoed state's Snare perspective is equally genuine: they have no legal alternative, cannot access intervention authorization, and face suppression from the P5 blocking power. The gap is not resolvable by better information or clearer framing — it reflects genuine structural asymmetry. The Scaffold perspective (reform advocates) sees a sunset: Uniting for Peace (1950) showed the veto could be worked around; rising norm shifts on humanitarian protection could limit veto scope. The Piton perspective (UN institution) sees the system's function degrading: the veto was a consensus-building tool; it now primarily blocks action. The false summit (Mountain/analytical) naturalizes the veto as inherent to universal systems, but this elides the contingency of the UN Charter's design — alternative institutional designs (supermajority voting, rotating permanent membership, mandatory humanitarian carve-outs) are structurally possible.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent class: P5 beneficiaries with institutional power and arbitrage exit derive d from their position as beneficiaries who can ignore consequences (d ≈ 0.08 → f(d) ≈ -0.12). Non-permanent members who are victims but have constrained (not trapped) exit because they can leave the Council at term end derive d ≈ 0.55 (victim + constrained → f(d) ≈ 0.75). Vetoed states facing genuine humanitarian crises with no legal alternative to UNSC action derive d ≈ 0.92 (victim + trapped → f(d) ≈ 1.40). The abstract collective 'global conflict resolution capacity' has no power, no exit, and no organizational form, so it derives d ≈ 1.00 (powerless/trapped/analytical → f(d) ≈ 1.42). These directionality variations explain the perspectival classifications: beneficiaries with low d experience χ < 0 and see Rope; victims with high d experience χ > 0.66 and see Snare. No override needed — the structural data produces the right directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The veto constraint resolves mandatrophy through perspectival pluralism: all six types are legitimate readings from their structural positions. The resolution is not 'which type is the veto?' but 'which perspective are you in?' P5 beneficiaries see Rope and this is not a mislabeling (genuine coordination function exists, consensus-forcing does occur, great-power wars have been prevented). Vetoed states see Snare and this is not a mislabeling (pure extraction, no exit, high suppression, no beneficiary status). The Tangled Rope classification for a constrained P5 member is not a collapse of categories but a reflection of hybrid experience: the member benefits from the veto but faces legitimacy costs, so they experience both coordination (system protects their interests) and extraction (but at reputational cost). The mandatrophy resolves because the Tangled Rope type correctly captures agents in genuinely mixed positions — agents who are both beneficiaries (in institutional terms) and victims (in legitimacy terms) simultaneously. The false summit (Mountain) is detected and rejected: the claim that the veto is a natural law fails because the constraint's power derives from institutional rules, not physical necessity. Alternative institutional designs are possible; the veto's necessity is contingent on Charter design, not inherent to all possible universal systems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_reform_ceiling,
    'Can the veto system be reformed (scope-limited, vote-qualified, humanitarian carve-outs) without P5 consensus, which the veto itself requires?',
    'Historical precedent analysis: Uniting for Peace resolution (1950) bypassed Security Council deadlock through General Assembly supermajority; assessment of whether similar workarounds could institutionalize veto restrictions',
    'If yes: scaffold sunset is real — reform pathways exist. If no: veto is locked by its own mechanism (a snare property). Determines whether the constraint can degrade to rope or must remain snare/tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_reform_ceiling, conceptual, 'Whether veto can be reformed without veto consensus').

omega_variable(
    coordination_vs_blocking_function,
    'Does the veto primarily serve as a coordination mechanism (ensuring consensus) or as a blocking mechanism (enabling extraction/obstruction)?',
    'Content analysis: proportion of vetoes cast for self-interest vs. P5 consensus-protecting vetoes; temporal trend of veto usage relative to major power agreement; counterfactual assessment of whether outcomes differ without veto option',
    'If coordination: classification upgrades toward rope from all perspectives. If blocking: snare and tangled rope classifications confirmed. High/rising ratio of self-interest vetoes suggests transition from rope to snare over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_blocking_function, empirical, 'Primary function of veto in contemporary context').

omega_variable(
    legitimacy_cost_asymmetry,
    'Do all P5 members experience equal legitimacy costs for veto use, or do some members (e.g., those with higher soft power dependence) experience higher reputational damage?',
    'Analysis of voting patterns and subsequent diplomatic outcomes; correlation between veto use and UN voting support loss, ally coordination changes, and international standing measurements',
    'If asymmetric: some P5 members are actually constrained (powerful/constrained → tangled rope or even rope); others are fully beneficiaries (institutional/arbitrage → rope). This differentiates P5 member perspectives. If symmetric: all P5 see rope or tangled rope identically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_cost_asymmetry, empirical, 'Asymmetry in legitimacy costs across P5 members').

omega_variable(
    unilateral_action_alternative,
    'When the veto blocks UNSC action, is unilateral or coalition-based action actually available as an alternative, or is the veto functionally irreplaceable?',
    'Case analysis: situations where veto blocked UNSC action followed by actual unilateral/coalition response vs. situations where no action occurred; assessment of whether absence of UNSC cover materially changes outcomes or merely affects legitimacy',
    'If alternatives are available: exit option for trapped agents upgrades to constrained or mobile (classification softens). If veto is functionally irreplaceable: trapped exit is confirmed, snare classification strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unilateral_action_alternative, empirical, 'Availability of unilateral/coalition alternatives to UNSC action').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(un_security_council_veto_system, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_veto_theater_1945, un_security_council_veto_system, theater_ratio, 0, 0.25).
narrative_ontology:measurement(unsc_veto_theater_1970, un_security_council_veto_system, theater_ratio, 25, 0.35).
narrative_ontology:measurement(unsc_veto_theater_1995, un_security_council_veto_system, theater_ratio, 50, 0.42).
narrative_ontology:measurement(unsc_veto_theater_2020, un_security_council_veto_system, theater_ratio, 75, 0.48).

% Extraction over time
narrative_ontology:measurement(unsc_veto_extractiveness_1945, un_security_council_veto_system, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(unsc_veto_extractiveness_1970, un_security_council_veto_system, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(unsc_veto_extractiveness_1995, un_security_council_veto_system, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(unsc_veto_extractiveness_2020, un_security_council_veto_system, base_extractiveness, 75, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(un_security_council_veto_system, enforcement_mechanism).
narrative_ontology:affects_constraint(un_security_council_veto_system, great_power_war_prevention).
narrative_ontology:affects_constraint(un_security_council_veto_system, humanitarian_intervention_authorization).
narrative_ontology:affects_constraint(un_security_council_veto_system, weapons_proliferation_coordination).

% DUAL FORMULATION NOTE:
% The veto system operates as a single constraint but could be decomposed into separate constraints for different decision categories: vetoes on military action, vetoes on admission/membership, vetoes on procedural matters show different extractiveness profiles (military vetoes are more extracted, procedural vetoes are more coordination-heavy). However, they share a common enforcement mechanism (the Charter veto power), so they are kept as one story rather than three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
