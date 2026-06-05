% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__deterrence_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__deterrence_equilibrium_reading, []).

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
 *   constraint_id: total_war_possibility_space__deterrence_equilibrium_reading
 *   human_readable: Total War Deterrence via Mutual Vulnerability (Equilibrium Reading)
 *   domain: international_relations/strategic_studies/nuclear_strategy
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of a contested kernel: the
 *   total-war possibility space. The kernel is the stabilized commitment that
 *   governs strategic reasoning about the ultimate form of interstate
 *   conflict — total war. Three incommensurable readings compete to explain
 *   why total war has not occurred between nuclear-armed states since 1945:
 *   (1) DETERRENCE EQUILIBRIUM READING (this story): total war remains
 *   strategically reachable but is deterred by mutual vulnerability;
 *   cost-benefit calculation with extremely high costs generates continuous
 *   investment in war-fighting capability and doctrine. (2) NUCLEAR TABOO
 *   READING: total war became normatively prohibited through constructed
 *   taboo, independent of material capability; the constraint is
 *   cultural/institutional norm-maintenance. (3) SPACE CONTRACTION READING:
 *   nuclear weapons removed total war from the strategically thinkable
 *   itself, not merely from the preferable; the constraint is cognitive
 *   impossibility, not cost-benefit trade-off. This story generates the
 *   deterrence equilibrium reading as a clean, ε-invariant constraint. The
 *   constraint's extractiveness (0.68) reflects that mutual vulnerability
 *   creates genuine coordination benefit (prevents mutually catastrophic
 *   escalation) alongside severe extraction: civilian populations remain
 *   permanent hostages to the deterrent logic; non-nuclear states are forced
 *   to accept conventional stalemate; doctrinal institutions extract
 *   resources and legitimacy from the requirement to maintain perpetual
 *   war-fighting readiness. The theater ratio (0.58) reflects that strategic
 *   doctrine is substantially performative—the actual constraint (mutual
 *   vulnerability creates unacceptable costs) does not require the elaborate
 *   doctrinal apparatus, yet institutional inertia maintains the frameworks.
 *   Suppression ranges from 0.65 (early deterrence, 1962) to 0.75 (doctrine
 *   hardening, 1980) and stabilizes at 0.72 (2010), reflecting that the
 *   constraint's suppressive force—the removal of total war from strategic
 *   choice—is sustained through doctrinal legitimacy, intelligence
 *   capability, and the sheer computational complexity of strategic
 *   signaling.
 *
 * KEY AGENTS:
 *   - Civilian populations within nuclear-armed states or allies (powerless/trapped) — permanent hostages to deterrent logic; cannot exit; bear full cost of annihilation risk in exchange for coordination benefit of no total war
 *   - Non-nuclear state actors (organized/constrained) — constrained by nuclear umbrella effect; cannot escalate to total war; coordinated benefit of nuclear protection paired with extraction of conventional stalemate acceptance
 *   - Nuclear-armed state hierarchies (institutional/arbitrage) — primary beneficiaries; mutual vulnerability stabilizes power hierarchy and removes total-military-victory option; maintain arbitrage by adjusting doctrine and signaling posture
 *   - Strategic doctrine communities (institutional/arbitrage) — maintain elaborate doctrinal frameworks (escalation ladders, counterforce targeting, extended deterrence) that are substantially performative; institutional inertia sustains doctrinal apparatus
 *   - Non-proliferation regime actors (organized/mobile) — see deterrence equilibrium as temporary institutional arrangement with sunset; advocate disarmament as alternative exit pathway
 *   - Analytical observer (analytical/analytical) — risks naturalizing the contingent deterrence equilibrium as a physical law inherent to nuclear capability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, 0.68).
domain_priors:suppression_score(total_war_possibility_space__deterrence_equilibrium_reading, 0.72).
domain_priors:theater_ratio(total_war_possibility_space__deterrence_equilibrium_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__deterrence_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__deterrence_equilibrium_reading, "Total War Deterrence via Mutual Vulnerability (Equilibrium Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__deterrence_equilibrium_reading, "international_relations/strategic_studies/nuclear_strategy").

domain_priors:requires_active_enforcement(total_war_possibility_space__deterrence_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__deterrence_equilibrium_reading, 'cec37379-b218-436e-8041-39a0dac061cd').
narrative_ontology:cs_kernel_codification('cec37379-b218-436e-8041-39a0dac061cd', distributed).
narrative_ontology:cs_authority_grounding('cec37379-b218-436e-8041-39a0dac061cd', extraction).
narrative_ontology:cs_reading_relation('cec37379-b218-436e-8041-39a0dac061cd', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_reading_relation('cec37379-b218-436e-8041-39a0dac061cd', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('cec37379-b218-436e-8041-39a0dac061cd', foundational, total_war_remains_rationally_calculable).
narrative_ontology:cs_axiom_status(total_war_remains_rationally_calculable, holdable).
narrative_ontology:cs_axiom_grounding('cec37379-b218-436e-8041-39a0dac061cd', total_war_remains_rationally_calculable, empirically_contingent).
narrative_ontology:cs_axiom('cec37379-b218-436e-8041-39a0dac061cd', secondary, continuous_doctrinal_signaling_enables_deterrence).
narrative_ontology:cs_axiom_status(continuous_doctrinal_signaling_enables_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('cec37379-b218-436e-8041-39a0dac061cd', continuous_doctrinal_signaling_enables_deterrence, instrumental).
narrative_ontology:cs_reference_frame('cec37379-b218-436e-8041-39a0dac061cd', strategic_rational_calculation_framework).
narrative_ontology:cs_drift_state('cec37379-b218-436e-8041-39a0dac061cd', contemporary_post_cold_war_deterrence_maintenance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cec37379-b218-436e-8041-39a0dac061cd', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_armed_state_hierarchies).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, strategic_doctrinal_institutions).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, civilian_populations_within_conflict_zones).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_state_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATION (SNARE) — Trapped within geography of nuclear-armed states or allies. Mutual vulnerability deters large-scale war but creates permanent hostage condition. No exit option; continuous extraction of security trust in exchange for acceptance of annihilation risk. Full target of the constraint's suppressive logic.
constraint_indexing:constraint_classification(total_war_possibility_space__deterrence_equilibrium_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-NUCLEAR STATE ACTORS (TANGLED ROPE) — Constrained by nuclear umbrella effect; cannot escalate to total war even if conventional capacity would enable it. Deterrence provides security coordination benefit (protected from nuclear-armed adversaries) alongside extraction: constraint removes total-war option, forcing acceptance of conventional stalemate or negotiated settlement. Organized enough to recognize the pattern but exit costs are prohibitive (would require nuclear weapons or alliance restructuring).
constraint_indexing:constraint_classification(total_war_possibility_space__deterrence_equilibrium_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: NUCLEAR-ARMED STATE HIERARCHIES (ROPE) — Primary beneficiaries. Mutual vulnerability stabilizes power hierarchy: nuclear capacity ensures no peer state can achieve total military victory. Deterrence provides genuine coordination benefit (prevents mutually catastrophic escalation) alongside asymmetric benefit (maintains state as peer-competitor in power calculus). Institutional agents with arbitrage options; can adjust doctrine, signaling, and targeting posture to extract maximum deterrent value while maintaining plausible coordination frame.
constraint_indexing:constraint_classification(total_war_possibility_space__deterrence_equilibrium_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NON-PROLIFERATION REGIME (SCAFFOLD) — Organized actors (NPT signatories, IAEA, disarmament coalitions) see deterrence equilibrium as temporary coordination mechanism with sunset clause: nuclear disarmament and treaties (CTBT, START, verification protocols) aim to contract the possibility space for total war by eliminating the weapons that enable it. Low effective extraction because this perspective has exit path (disarmament) and frames the deterrence equilibrium as transient institutional arrangement, not permanent feature.
constraint_indexing:constraint_classification(total_war_possibility_space__deterrence_equilibrium_reading, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: STRATEGIC DOCTRINE COMMUNITIES (PITON) — Military strategists, deterrence theorists, war-college faculty maintain elaborate doctrinal frameworks (escalation ladders, counterforce targeting, extended deterrence, second-strike doctrine) that are substantially performative. The frameworks appear to operationalize deterrence equilibrium but are largely theater — the actual constraint (mutual vulnerability creates unacceptable costs) does not require the doctrinal complexity. Institutional inertia maintains the doctrinal apparatus despite low functional necessity. Theater derives from the need to maintain the appearance of rational strategic planning in a domain where the core logic is irrational (mutual assured destruction is the actual stabilizer, not doctrine).
constraint_indexing:constraint_classification(total_war_possibility_space__deterrence_equilibrium_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICAL LAW READING (MOUNTAIN) — From a civilizational frame, the deterrence equilibrium appears to be a natural law: any two actors with second-strike nuclear capability are logically bound by mutual vulnerability calculus. The constraint seems to emerge automatically from the physics and mathematics of strategic capability, independent of institution, norm, or choice. However, this reading naturalizes what this constraint story explicitly frames as contingent institutional arrangement. The engine's false-summit detection will flag this perspective as category confusion.
constraint_indexing:constraint_classification(total_war_possibility_space__deterrence_equilibrium_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(total_war_possibility_space__deterrence_equilibrium_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(total_war_possibility_space__deterrence_equilibrium_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(total_war_possibility_space__deterrence_equilibrium_reading, TR),
    TR >= 0.70.

:- end_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): Moderate-high, reflecting genuine coordination function paired with severe extraction. The deterrence equilibrium does prevent total war (coordination benefit) but sustains civilian hostage condition, removes options from non-nuclear actors, and generates institutional extraction through doctrinal maintenance. The value increased from 0.08 (1945, atomic monopoly) to 0.52 (1962, mutual vulnerability achieved) to 0.68 (1980-2010, doctrine maturation and institutional solidification). Suppression (0.72): High, reflecting strong removal of total-war option from strategic choice space. The suppression is not coercive in the sense of external force (no enforcement agent preventing total war) but operates through structural logic: attempting total war produces mutual annihilation. Suppression increased from ~0.65 (early deterrence, when total-war plans still existed in doctrine) to 0.75 (doctrine hardening, when counterforce targeting and escalation ladders became institutionalized) and stabilized at 0.72 (institutional equilibrium reached). Theater ratio (0.58): Moderate-high, reflecting that strategic doctrine is substantially performative. The elaborate apparatus of escalation ladders, counterforce targeting, extended deterrence, damage limitation strategies, and strategic force modernization appear to operationalize rational deterrence management but function largely as theater—the actual constraint (mutual vulnerability creates unacceptable costs) does not require doctrinal complexity. Theater increased from 0.15 (1945, functional civil defense) to 0.42 (1962, doctrine emergence) to 0.60 (1980, doctrine professionalization) and stabilized at 0.58 (sustained doctrinal theater despite limited functional necessity). Claimed type (tangled_rope): The constraint exhibits both genuine coordination function (prevents mutually catastrophic escalation) and asymmetric extraction (removes options from non-nuclear actors, maintains civilian hostage condition, generates doctrinal institutional overhead). This satisfies tangled-rope gates: beneficiaries (nuclear-armed states, doctrinal institutions) + victims (civilian populations, non-nuclear states) + active enforcement (continuous doctrinal signaling and capability maintenance) = tangled rope.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates incommensurable perspectival gaps arising from the kernel contest. The nuclear-armed state hierarchy sees coordination (Rope)—mutual vulnerability solves the commitment problem in power competition. The non-proliferation regime sees a sunset mechanism (Scaffold)—disarmament and verification treaties aim to contract the possibility space. Strategic doctrine communities see their own degraded ritual (Piton)—doctrinal frameworks persist through inertia, not functional necessity. Non-nuclear state actors see mixed coordination and extraction (Tangled Rope)—deterrence provides security coordination benefit alongside removal of their strategic options. Civilian populations see pure extraction (Snare)—permanent hostage condition with no exit option. The analytical observer risks seeing a natural law (Mountain)—mutual vulnerability appears to be an automatic physical/mathematical law. These gaps do not resolve to a single classification but instead reveal the kernel contest: the constraint is perspectival manifestation of three competing institutional readings of the same event (no total war since 1945). The deterrence equilibrium reading produces tangled rope at the analytical level because the coordination benefit is real (mutual vulnerability prevents mutual annihilation) but the extraction is also real (doctrinal institutions extract resources, non-nuclear states lose options, civilians remain hostages). The perspectival gaps open onto the kernel ambiguity: is the constraint enforced by cost-benefit calculation (this reading), by cultural taboo (sibling reading), or by cognitive impossibility (third sibling)? Each reading predicts different institutional behaviors and different failure modes.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is derived from the agent's structural position relative to the deterrence equilibrium. Nuclear-armed state hierarchies (beneficiaries with arbitrage options) derive d ≈ 0.15-0.20: they capture deterrent value from the constraint while maintaining options to adjust doctrine and posture. Strategic doctrine communities (beneficiary-adjacent with arbitrage) derive d ≈ 0.25: they benefit from doctrinal legitimacy and institutional funding but face limited extraction themselves. Non-proliferation regime actors (mobile) derive d ≈ 0.40: they seek to constrain the deterrence equilibrium but operate within it. Non-nuclear state actors (constrained/organized) derive d ≈ 0.60: they face high costs from removal of total-war option but maintain some diplomatic and conventional capacity. Civilian populations (trapped/powerless) derive d ≈ 0.90: they bear maximum cost (annihilation risk) with zero exit and zero compensation other than the coordination benefit (no total war). The analytical observer (analytical/analytical) derives d ≈ 0.72: the observer position captures full structure but cannot leverage it—pure analysis produces no exit or agency. The chi formula χ = ε × f(d) × σ(S) produces highest effective extraction for the trapped civilian population and lowest for the beneficiary nuclear powers, despite the same base extractiveness. This differential is the perspectival structure that the framework captures.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rational_actor_assumption_validity,
    'Does deterrence equilibrium depend on continuous rational cost-benefit calculation by all nuclear-armed actors, or can irrational/accident-prone actors sustain the constraint?',
    'Historical case analysis of near-miss incidents (Cuban Missile Crisis, false alert incidents, miscalculation scenarios); structural comparison with deterrence systems that required genuine rationality vs those that survived under uncertainty',
    'If rationality is required: constraint is fragile and depends on institutional maintenance of doctrinal coherence. If constraint survives irrational actors: deterrence equilibrium is more robust than expected, suggesting the constraint is less about institutional signaling and more about raw mutual vulnerability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_actor_assumption_validity, empirical, 'Whether deterrence requires continuous rational calculation').

omega_variable(
    readings_of_the_total_war_kernel,
    'Is total war constrained by deterrence equilibrium (this reading), nuclear taboo norm (sibling reading), or removal from strategic possibility space entirely (third sibling)?',
    'Documentary evidence from strategic doctrine, military exercises, contingency planning, and policy reviews. Three competing readings of why total war has not occurred: (a) deterrence equilibrium—cost-benefit calculation with extremely high costs, (b) cultural/normative taboo—internalized prohibition independent of material capability, (c) contraction of the possibility space—total war became literally unthinkable strategically, not merely disfavored. Each reading predicts different failure modes and different institutional behaviors.',
    'Deterrence equilibrium reading predicts: continuous doctrinal development, war-fighting capability investment, escalation ladder theorization, intelligence focus on adversary capability. Taboo reading predicts: norm-maintenance focus, taboo-violation punishment, identity-based resistance to total war framing. Space-contraction reading predicts: historical institutionalization of limits, loss of institutional memory for total war planning, cognitive impossibility of strategic reasoning about total war.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(readings_of_the_total_war_kernel, conceptual, 'Kernel contest: three incommensurable readings of total war constraint').

omega_variable(
    doctrinal_theater_vs_functional_deterrence,
    'Does the elaborate doctrinal apparatus (escalation ladders, counterforce targeting, extended deterrence) functionally contribute to deterrence, or does mutual vulnerability alone sustain the constraint?',
    'Counterfactual analysis: modeling deterrence outcomes under simplified doctrine vs complex doctrine. Historical analysis of doctrine changes and their impact on conflict behavior. Game-theoretic analysis of whether doctrine-independent mutual vulnerability produces same equilibrium as doctrine-dependent signaling.',
    'If doctrine is functional: theater_ratio should be lower (~0.4), and strategic doctrine communities see themselves as coordinating genuine deterrent function. If doctrine is theater: theater_ratio confirmed at ~0.58, and the constraint is sustained by raw mutual vulnerability, not by institutional signaling logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_theater_vs_functional_deterrence, empirical, 'Whether strategic doctrine functionally enables deterrence or is performative apparatus').

omega_variable(
    continuous_investment_requirement_for_deterrence,
    'Does deterrence equilibrium require continuous investment in war-fighting capability and doctrine development, or would capability alone (frozen in place) sustain deterrence?',
    'Historical analysis of deterrence stability during periods of doctrine revision, capability development pauses, and force-posture changes. Strategic stability assessment under different investment regimes. Comparison with deterrence regimes that explicitly rejected continuous modernization.',
    'If continuous investment is required: constraint generates perpetual military-industrial extraction of resources and shapes strategic institutions around the requirement to maintain doctrinal legitimacy. If static capability sustains deterrence: deterrence equilibrium could be achieved with lower institutional overhead, and the continuous investment represents extractive institutional behavior rather than functional deterrent requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuous_investment_requirement_for_deterrence, empirical, 'Whether deterrence requires continuous capability and doctrine investment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__deterrence_equilibrium_reading, 1945, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twpe_theater_1945_functional_civil_defense, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(twpe_theater_1962_doctrine_emergence, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1962, 0.42).
narrative_ontology:measurement(twpe_theater_1980_doctrine_professionalization, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1980, 0.6).
narrative_ontology:measurement(twpe_theater_2010_sustained_doctrinal_theater, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2010, 0.58).

% Extraction over time
narrative_ontology:measurement(twpe_extractiveness_1945_atomic_monopoly, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1945, 0.08).
narrative_ontology:measurement(twpe_extractiveness_1962_mutual_vulnerability, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1962, 0.52).
narrative_ontology:measurement(twpe_extractiveness_1980_doctrine_maturation, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1980, 0.68).
narrative_ontology:measurement(twpe_extractiveness_2010_nuclear_deterrence_institutional, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2010, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(twpe_suppression_1962_early_deterrence, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1962, 0.65).
narrative_ontology:measurement(twpe_suppression_1980_doctrine_hardening, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(twpe_suppression_2010_stable_suppression, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2010, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__deterrence_equilibrium_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space__nuclear_taboo_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space__space_contraction_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_strategic_stability).
narrative_ontology:affects_constraint(total_war_possibility_space__deterrence_equilibrium_reading, extended_deterrence_alliance_commitment).

% DUAL FORMULATION NOTE:
% The total_war_possibility_space kernel admits three structurally distinct constraint stories. This story (deterrence_equilibrium_reading) models the constraint as cost-benefit calculation with institutional enforcement through doctrine. The sibling stories model the same event (no total war since 1945) as norm-based taboo and as cognitive impossibility. Each story has its own ε, its own beneficiary/victim structure, and its own predictions. They are not different measurements of one constraint but different constraints arising from different framings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
