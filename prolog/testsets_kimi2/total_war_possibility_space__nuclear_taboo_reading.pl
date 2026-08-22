% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Nuclear Taboo Reading of Total War Possibility Space
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint instantiates the nuclear_taboo_reading of the contested
 *   kernel total_war_possibility_space: the claim that total war became
 *   normatively prohibited through a constructed taboo operating
 *   independently of material military capability. The taboo is enforced
 *   through the NPT regime, no-first-use pledges, and normative
 *   stigmatization of nuclear weapons. It generates genuine coordination
 *   (reduced probability of nuclear war) alongside asymmetric extraction
 *   (sovereignty costs for non-nuclear states, maintenance of a nuclear
 *   oligopoly, and strategic dependence for umbrella allies). The claim and
 *   metrics are independently authored: claimed_type is tangled_rope because
 *   the structure contains both real coordination and asymmetric extraction,
 *   while the metrics describe a constraint that is moderately extractive,
 *   heavily suppressed, and increasingly theatrical as arsenal modernization
 *   and brinkmanship erode the normative core without abolishing it.
 *
 * KEY AGENTS:
 *   - nuclear_oligopoly_states: Primary agenda-setter and beneficiary (institutional/constrained) â administers the non-proliferation regime, retains nuclear privilege, enforces the taboo on others
 *   - nuclear_aspirant_states: Primary target (moderate/constrained) â bears extraction through sovereignty denial, sanctions, and technology denial
 *   - non_nuclear_weapon_states: Mixed beneficiary and payer (organized/constrained) â gains coordination benefit of reduced nuclear risk, pays through sovereignty constraints and asymmetric inspection burdens
 *   - norm_entrepreneurs: Secondary beneficiary (organized/mobile) â derives institutional relevance from taboo maintenance and mobilizes normative pressure
 *   - extended_deterrence_dependents: Secondary target (moderate/constrained) â pays strategic autonomy for ambiguous protection under extended nuclear umbrellas
 *   - strategic_studies_observers: Analytical observer (analytical/analytical) â maps taboo strength, erosion, and competing explanatory frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.62).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.75).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Nuclear Taboo Reading of Total War Possibility Space").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, '3d5fbbaf-e5b7-4afe-8267-d3b8bcb61ce9').
narrative_ontology:cs_kernel_codification('3d5fbbaf-e5b7-4afe-8267-d3b8bcb61ce9', distributed).
narrative_ontology:cs_authority_grounding('3d5fbbaf-e5b7-4afe-8267-d3b8bcb61ce9', distributed).
narrative_ontology:cs_reading_relation('3d5fbbaf-e5b7-4afe-8267-d3b8bcb61ce9', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d5fbbaf-e5b7-4afe-8267-d3b8bcb61ce9', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('3d5fbbaf-e5b7-4afe-8267-d3b8bcb61ce9', foundational, normative_prohibition_independent_of_capability).
narrative_ontology:cs_axiom_status(normative_prohibition_independent_of_capability, holdable).
narrative_ontology:cs_axiom_grounding('3d5fbbaf-e5b7-4afe-8267-d3b8bcb61ce9', normative_prohibition_independent_of_capability, empirically_contingent).
narrative_ontology:cs_reference_frame('3d5fbbaf-e5b7-4afe-8267-d3b8bcb61ce9', normative_prohibition_state).
narrative_ontology:cs_drift_state('3d5fbbaf-e5b7-4afe-8267-d3b8bcb61ce9', contemporary_multipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3d5fbbaf-e5b7-4afe-8267-d3b8bcb61ce9', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, nuclear_oligopoly_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurs).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, nuclear_aspirant_states).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, extended_deterrence_dependents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_weapon_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the non-proliferation regime through the NPT, Security Council structures, and nuclear-weapons-state diplomacy. Retain arsenals and veto-wielding status while enforcing abstinence on others. Bear the cost of maintaining arsenals and taboo-enforcement infrastructure, but retain ultimate deterrent privilege and hierarchical international status.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nuclear_oligopoly_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, nuclear_oligopoly_states, beneficiary).

% Comprise the majority of states that have renounced nuclear weapons under the NPT. Benefit from the reduced risk of nuclear war and the normative stigma on use. Pay through sovereignty constraints including IAEA inspections, dual-use technology denial, and strategic dependence on nuclear patrons.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_weapon_states, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_weapon_states, payer).

% International civil society organizations, legal scholars, and advocacy networks who construct, document, and mobilize support for the nuclear taboo. Derive institutional relevance, funding, and normative authority from the taboo's continued maintenance. Their engagement is structurally necessary for the taboo's vitality.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurs, beneficiary,
    organized, biographical, mobile, global).

% Mid-level or isolated states seeking nuclear capability for security or prestige. Face sanctions, sabotage, diplomatic isolation, and potential military pre-emption enforced by the taboo's guardians. Their strategic autonomy is curtailed by a normative order they did not design and cannot easily exit without severe penalties.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nuclear_aspirant_states, payer,
    moderate, biographical, constrained, regional).

% Allied states sheltering under extended nuclear guarantees such as NATO, Japan, and South Korea. Forgo independent nuclear programs and accept potential entrapment in patron-led nuclear crises. Their security depends on the taboo holding for adversaries while their patron retains nuclear privilege.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, extended_deterrence_dependents, payer,
    moderate, biographical, constrained, regional).

% Scholars and analysts who map the taboo's strength, document erosion or reinforcement, and debate whether the prohibition is normative, deterrent, or strategic in origin. Their assessments feed back into policy discourse but they neither pay nor benefit directly from the constraint's operation.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, strategic_studies_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__nuclear_taboo_reading, nuclear_oligopoly_states).
narrative_ontology:fixing_cost_class(total_war_possibility_space__nuclear_taboo_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents nuclear total war by constructing a shared normative expectation that nuclear weapons use is unacceptable regardless of immediate strategic advantage, thereby solving a civilization-scale coordination problem of unrestrained destruction.
% TRANSFER_FUNCTION: Moves sovereignty and strategic autonomy from non-nuclear and aspirant states to the nuclear oligopoly and norm-enforcement institutions; transfers legitimacy, funding, and institutional relevance to norm entrepreneurs who maintain the prohibition.
% ABSENT_VOICES: States that have actually used nuclear weapons are structurally absent from victim discourse; future generations who would suffer from a broken taboo are unrepresented; non-state actors who might acquire nuclear materials are excluded from the regime's legitimizing discourse.
% DISAPPEARANCE_RATIONALE: If the taboo disappeared overnight, the non-proliferation architecture would collapse, extended deterrence alliances would fracture, aspirant states would proliferate openly, and total war would re-enter the strategically thinkable. The current international security order is organized around this normative foreclosure.
% FOUNDING_PROBLEM: The problem of unrestrained nuclear total war threatening civilization-scale destruction, emerging from the atomic bombings of 1945 and the subsequent nuclear arms race.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by Hiroshima and Nagasaki survivor testimony, ICRC documentation of humanitarian consequences, and independent strategic studies from outside the nuclear oligarchy. However, the specific taboo-as-independent-cause framing is primarily asserted by norm entrepreneurs and non-nuclear states, with nuclear states offering ambivalent corroboration that often collapses into deterrence logic.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the constraint transfers sovereignty and strategic autonomy to a nuclear oligopoly while providing real coordination benefit. Suppression is high (0.75) because the taboo persists through active enforcement: sanctions, IAEA inspections, stigmatization, and potential military pre-emption. Theater ratio is moderate (0.45) because performative maintenance is significant â nuclear states modernize arsenals while professing the taboo, and disarmament diplomacy often outruns disarmament action. Accessibility collapse is substantial (0.70) because, once the regime is understood, alternatives (proliferation or nuclear use) carry overwhelming costs, though not absolute (witness North Korea). Resistance is moderate (0.55) because aspirant states, some non-aligned movements, and occasional nuclear rhetoric from established powers actively contest the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The nuclear oligopoly experiences the taboo as essential coordination that prevents civilization-scale destruction and stabilizes their privileged position. Nuclear aspirant states experience the same structure as a cartel enforcing technological apartheid. Non-nuclear weapon states occupy a genuinely mixed seat: they benefit from reduced nuclear risk but increasingly contest the asymmetry of the bargain when nuclear states fail to disarm per Article VI. Extended deterrence dependents experience protection and vulnerability simultaneously. The engine computes these divergences from the structural data; no single seat's perception adjudicates the classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear oligopoly states and norm entrepreneurs are structural beneficiaries (low d) because the constraint subsidizes their status and institutional relevance. Nuclear aspirant states and extended deterrence dependents are structural targets (high d) because the constraint extracts sovereignty and autonomy from them. Non-nuclear weapon states sit near symmetric (d approximating 0.5) due to their mixed role: they receive genuine coordination benefit while paying real sovereignty costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â unrestrained nuclear total war â remains live, so mandatrophy is not resolved. Classifying this constraint as a pure snare would miss the real coordination benefit of reduced nuclear war probability. Classifying it as a pure rope would miss the hierarchical enforcement, the nuclear oligopoly's rent-like privilege, and the sovereignty costs imposed on aspirants and non-nuclear states. Tangled rope is the only category that captures both the genuine coordination function and the asymmetric extraction embedded in the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_separability,
    'Does the nuclear taboo operate independently of deterrence equilibrium and strategic space contraction, or are these sibling readings describing inseparable aspects of a single material-normative complex?',
    'Comparative case analysis of near-nuclear-use crises (Cuban Missile Crisis, 1973 Middle East alert, 1983 Able Archer) to determine whether normative prohibition or material deterrence was the operative constraint in each instance.',
    'If inseparable, this reading''s epsilon is overstated as independent normative force and should be partially reallocated to sibling readings; if separable, the taboo is a distinct constraint with its own epsilon and classification profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_separability, conceptual, 'Whether the taboo reading describes a separable mechanism from deterrence and strategic contraction').

omega_variable(
    norm_entrepreneur_necessity,
    'Is the taboo''s persistence causally dependent on active norm entrepreneurship, or has it become self-sustaining through institutional inertia?',
    'Track taboo strength indicators during periods of norm-entrepreneur institutional decline or withdrawal versus active mobilization, and observe regime behavior in issue areas with and without ongoing norm-entrepreneur engagement.',
    'If dependent on active entrepreneurship, the constraint is more fragile than metrics suggest and drift toward practice_drift may accelerate; if self-sustaining, the constraint moves toward rope-like characteristics and away from tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_entrepreneur_necessity, empirical, 'Whether taboo persistence requires active norm entrepreneurs or is institutionalized').

omega_variable(
    non_nuclear_asymmetry,
    'Do non-nuclear weapon states experience the constraint as net coordination benefit or net extraction?',
    'Sovereignty-cost accounting: compare the value of forgoing nuclear weapons (avoided costs plus security benefits) against the costs of inspections, technology denial, strategic dependence, and the disarmament obligation asymmetry.',
    'If net extraction dominates for NNWS, their directionality shifts toward payer, strengthening the tangled_rope classification; if net benefit dominates, they move toward pure beneficiary, shifting the overall constraint toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(non_nuclear_asymmetry, empirical, 'Net directional position of non-nuclear weapon states within the constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tota_tr_t10, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(tota_tr_t20, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(tota_tr_t30, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(tota_tr_t40, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(tota_tr_t50, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(tota_tr_t60, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(tota_tr_t70, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 70, 0.42).
narrative_ontology:measurement(tota_tr_t80, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 80, 0.45).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(tota_be_t10, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(tota_be_t20, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(tota_be_t30, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(tota_be_t40, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(tota_be_t50, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(tota_be_t60, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(tota_be_t70, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 70, 0.61).
narrative_ontology:measurement(tota_be_t80, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 80, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(tota_su_t10, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(tota_su_t20, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(tota_su_t30, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(tota_su_t40, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(tota_su_t50, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(tota_su_t60, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 60, 0.73).
narrative_ontology:measurement(tota_su_t70, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 70, 0.74).
narrative_ontology:measurement(tota_su_t80, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 80, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, space_contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel total_war_possibility_space. The epsilon reflects normative construction and enforcement mechanisms rather than material deterrence or strategic unthinkability. Linked to sibling readings that share the referent but author different epsilons due to distinct observables: deterrence capacity versus normative stigma versus strategic possibility space contraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
