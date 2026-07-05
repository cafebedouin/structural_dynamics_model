% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__categorical_prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__categorical_prohibition_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__categorical_prohibition_reading
 *   human_readable: Categorical Prohibition Reading of the Martens Clause on Autonomous Weapons
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the categorical prohibition reading of the
 *   ihl_distinction_proportionality kernel: the claim that the Martens
 *   Clause's principles of humanity and public conscience prohibit lethal
 *   autonomous weapons systems (LAWS) as a category, independent of any
 *   technical performance data, because the act of delegating a lethal
 *   targeting decision to a machine itself violates human dignity. This is a
 *   distinct constraint from the human_agency_reading (which grounds the
 *   prohibition in the necessity of human moral judgment at the moment of
 *   force, a narrower and more defensible claim) and the
 *   outcomes_based_reading (which explicitly rejects categorical prohibition
 *   in favor of a performance benchmark against human operators). The three
 *   readings produce different beneficiary/victim structures and different
 *   epsilon values from the same underlying kernel text; they are not
 *   measurement variants of one constraint but three constraints sharing a
 *   contested textual root.
 *
 * KEY AGENTS:
 *   - anti_militarist_civil_society: primary agenda-setter and beneficiary, drives the normative campaign
 *   - states_lacking_laws_capability: secondary beneficiary, gains from a frozen technological hierarchy
 *   - states_with_advanced_autonomous_systems: primary target, loses developed military-technological advantage
 *   - defense_technology_sector: secondary target, bears reputational and market risk
 *   - military_commanders_seeking_precision_tools: trapped payer, loses access to a category of tools regardless of demonstrated performance
 *   - civilians_in_conflict_zones: excluded voice, actual stakes-bearer with no forum seat
 *   - international_humanitarian_law_scholars: analytical observer, contests whether the Clause bears this categorical weight
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, 0.71).
domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, 0.62).
domain_priors:theater_ratio(ihl_distinction_proportionality__categorical_prohibition_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__categorical_prohibition_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__categorical_prohibition_reading, "Categorical Prohibition Reading of the Martens Clause on Autonomous Weapons").
narrative_ontology:topic_domain(ihl_distinction_proportionality__categorical_prohibition_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__categorical_prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__categorical_prohibition_reading, '1899db15-433c-40cc-ad12-36a4855da107').
narrative_ontology:cs_kernel_codification('1899db15-433c-40cc-ad12-36a4855da107', fixed_text).
narrative_ontology:cs_authority_grounding('1899db15-433c-40cc-ad12-36a4855da107', distributed).
narrative_ontology:cs_reading_relation('1899db15-433c-40cc-ad12-36a4855da107', ihl_distinction_proportionality__human_agency_reading, influences).
narrative_ontology:cs_reading_relation('1899db15-433c-40cc-ad12-36a4855da107', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('1899db15-433c-40cc-ad12-36a4855da107', foundational, delegation_of_lethal_decision_violates_dignity_per_se).
narrative_ontology:cs_axiom_status(delegation_of_lethal_decision_violates_dignity_per_se, holdable).
narrative_ontology:cs_axiom_grounding('1899db15-433c-40cc-ad12-36a4855da107', delegation_of_lethal_decision_violates_dignity_per_se, deontological).
narrative_ontology:cs_axiom('1899db15-433c-40cc-ad12-36a4855da107', foundational, technical_performance_is_categorically_irrelevant_to_permissibility).
narrative_ontology:cs_axiom_status(technical_performance_is_categorically_irrelevant_to_permissibility, holdable).
narrative_ontology:cs_axiom_grounding('1899db15-433c-40cc-ad12-36a4855da107', technical_performance_is_categorically_irrelevant_to_permissibility, deontological).
narrative_ontology:cs_reference_frame('1899db15-433c-40cc-ad12-36a4855da107', martens_clause_residual_gap_filler_1899).
narrative_ontology:cs_drift_state('1899db15-433c-40cc-ad12-36a4855da107', post_ccw_laws_debate_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('1899db15-433c-40cc-ad12-36a4855da107', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, arms_control_ngos).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, defense_technology_sector).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, military_commanders_seeking_precision_tools).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Campaigns at UN CCW meetings and in domestic legislatures for a categorical ban on lethal autonomous weapons systems (LAWS), invoking the Martens Clause's principles of humanity and public conscience. Drafts model treaty language, mobilizes public opinion, and frames the debate in dignity terms that make any performance-based defense of LAWS look like a category error. Bears no direct cost from the ban; gains moral standing, funding, and institutional relevance from sustaining the campaign.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society, agenda_setter,
    organized, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society, beneficiary).

% Support the categorical ban diplomatically because they cannot compete in autonomous weapons development. A universal prohibition freezes the military-technological hierarchy in a configuration where their disadvantage cannot widen further. They incur no compliance cost since they possess no relevant systems to give up.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability, beneficiary,
    moderate, generational, constrained, national).

% Have invested heavily in autonomous targeting and weapons research and argue the categorical reading forecloses a technology that could reduce civilian casualties relative to human-operated systems. Their exit option is refusing ratification or treating any resulting instrument as non-binding customary law — a real but reputationally costly exit that isolates them diplomatically while not eliminating the pressure of an emerging normative consensus.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems, payer,
    institutional, generational, mobile, global).

% Firms developing autonomous targeting systems face reputational and eventually regulatory risk if the categorical reading hardens into binding law. They can relocate research to jurisdictions with looser normative commitments, but the trend in multilateral fora threatens market access and procurement contracts regardless of where R&D occurs.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, defense_technology_sector, payer,
    powerful, biographical, constrained, global).

% Operate under rules of engagement shaped by whatever normative consensus prevails. If the categorical reading is adopted into binding doctrine, commanders lose access to tools they believe could reduce both civilian and friendly casualties in specific engagement types, regardless of demonstrated system performance. They have no individual exit — compliance is mandated by chain of command.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, military_commanders_seeking_precision_tools, payer,
    moderate, immediate, trapped, national).

% Bear the actual consequences of targeting decisions, whether made by humans or machines, but have no seat at the CCW table or in national defense procurement debates. Their interest — minimizing harm regardless of decision-maker — is invoked by every party in the kernel dispute but tested by none of the readings against ground-level data before adoption.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, civilians_in_conflict_zones, excluded,
    powerless, immediate, trapped, local).

% Analyze whether the Martens Clause, drafted in 1899 for a different technological context, can bear the categorical weight this reading places on it. Some treat the dignity argument as a genuine extension of customary law; others view it as advocacy dressed in legal form, noting the Clause was historically read as a residual gap-filler, not a freestanding categorical bar.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, international_humanitarian_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__categorical_prohibition_reading, diffuse).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__categorical_prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a normative consensus among states and civil society to prevent an arms race in autonomous lethal targeting before verification and accountability mechanisms exist — a genuine collective-action problem given the difficulty of verifying compliance or attributing autonomous-system failures after the fact.
% TRANSFER_FUNCTION: Moves military-technological advantage away from states that have invested in autonomous targeting research and toward states that have not, by imposing a uniform prohibition regardless of the technology's demonstrated or achievable performance; also moves reputational and moral capital toward advocacy organizations that lead the campaign.
% ABSENT_VOICES: Civilians in active conflict zones, whose actual casualty outcomes under human versus machine targeting are the empirical question the categorical reading declines to test, are invoked rhetorically by all sides but represented by none in the treaty-drafting process. Field commanders who would operate under the resulting rules are similarly absent from the diplomatic forum shaping them.
% DISAPPEARANCE_RATIONALE: Advocacy organizations and technologically disadvantaged states would say the world rearranges catastrophically — an arms race in machine-decided killing would proceed unchecked. States with advanced systems and IHL scholars skeptical of the categorical extension would say the underlying legal obligations (distinction, proportionality) remain fully intact under existing IHL without this reading, and only the campaign's institutional infrastructure and moral framing would disappear, not any operative legal protection.
% FOUNDING_PROBLEM: The Martens Clause was drafted in 1899 to prevent a normative vacuum whenever technology outpaced explicit treaty text — asserting that unlisted means of warfare remain subject to the principles of humanity and the dictates of public conscience even absent a specific rule.
% FOUNDING_PROBLEM_CORROBORATION: Anti-militarist civil society and several UN special rapporteurs attest the founding problem is live and squarely matches autonomous weapons as the paradigm gap case the Clause exists to fill. IHL scholars outside the advocacy coalition, including some who support autonomous-weapons regulation on other grounds, attest that the Clause's traditional interpretive role is residual and evidentiary rather than an independent categorical prohibition, and that this reading extends the Clause well past its historically corroborated function.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__categorical_prohibition_reading, contested).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__categorical_prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__categorical_prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71) because this reading imposes the most sweeping cost of the three siblings: it bans an entire technology class outright rather than conditioning permissibility on human oversight or demonstrated performance, transferring military-technological advantage away from states that invested in the capability regardless of what that investment could show empirically. Suppression is moderate-high (0.62) because enforcement depends on treaty ratification, export control regimes, and reputational sanction rather than any court with compulsory jurisdiction — the categorical claim persists through diplomatic and normative pressure, not adjudicated law. Theater ratio is moderate (0.28) reflecting that much CCW proceeding activity is genuinely deliberative but an increasing share is positioning and signaling as the political sides harden. Accessibility collapse (0.58) is moderate: the categorical framing forecloses the outcomes-based alternative rhetorically but has not yet foreclosed it in binding law. Resistance is high (0.74): states with advanced systems and much of the IHL scholarly community actively contest the categorical extension of a 1899 clause.
 *
 * DIRECTIONALITY LOGIC:
 *   Anti-militarist civil society and technologically disadvantaged states sit near the beneficiary end: the former gains moral and institutional capital from leading the campaign, the latter gains from a frozen hierarchy it did not have to invest to achieve. States with advanced autonomous systems and the defense sector sit near the target end: their sunk technological investment is directly devalued by a categorical rule that does not admit performance evidence as a defense. Military commanders are structurally trapped: they bear the operational cost of a policy set at a diplomatic level far above their command, with no individual exit option. Civilians in conflict zones are the deepest stakeholders but structurally excluded from the forum that adjudicates the rule that governs their exposure to lethal force.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a normative gap-filler for un-anticipated means of warfare) is not dead — autonomous weapons are precisely the kind of case the Martens Clause was designed to reach. The contested question is whether THIS reading (categorical prohibition regardless of performance) is the correct extension of that founding function, or whether it has drifted into an instrument serving the anti-militarist coalition's institutional interests and the interests of technologically disadvantaged states independent of the founding humanitarian concern. Because corroboration is split between advocacy-aligned and non-aligned IHL scholars, the founding_problem_status is authored as contested rather than resolved in either direction — this is exactly the kind of case where declaring resolution prematurely would mislabel either genuine coordination as extraction or genuine extraction as coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    martens_clause_categorical_scope_ambiguity,
    'Does the Martens Clause''s ''principles of humanity and dictates of public conscience'' support a freestanding categorical prohibition on an entire weapons category, or was it historically understood only as a residual interpretive gap-filler applicable case-by-case within existing distinction/proportionality analysis?',
    'State practice and opinio juris tracking: whether states invoking the Clause in CCW proceedings treat it as generating an independent, self-executing prohibition versus using it as interpretive support for case-specific proportionality findings. A binding treaty text adopting the categorical reading, or an ICJ advisory opinion, would resolve this authoritatively.',
    'If the categorical reading is not supported by the Clause''s historical function, this constraint''s claimed legal grounding is substantially weaker than presented, and the coordination function shifts from ''law enforcing itself'' to ''advocacy coalition building new law under cover of existing text'' — raising the effective extraction further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martens_clause_categorical_scope_ambiguity, conceptual, 'Whether the Martens Clause historically supports categorical (versus case-specific) prohibition.').

omega_variable(
    dignity_harm_independent_of_outcomes,
    'Is the dignity harm this reading identifies (machine-decided killing) a real, independently measurable harm distinct from casualty outcomes, or is it a moral intuition that cannot be operationalized apart from performance comparisons?',
    'Philosophical and empirical work distinguishing process-based dignity harms from outcome-based harms; survey research on affected populations'' actual moral judgments about human versus machine targeting decisions, if such research becomes methodologically feasible in conflict settings.',
    'If the dignity harm is not separable from outcomes, the categorical reading collapses toward the outcomes_based_reading''s framework; if it is genuinely independent, the categorical reading has a defensible non-consequentialist foundation the other readings lack.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dignity_harm_independent_of_outcomes, conceptual, 'Whether machine-decided killing constitutes a harm independent of demonstrated casualty outcomes.').

omega_variable(
    beneficiary_capture_of_humanitarian_framing,
    'To what extent has the categorical prohibition campaign been captured by the strategic interests of technologically disadvantaged states seeking to freeze a military hierarchy, versus genuinely reflecting humanitarian concern independent of relative capability?',
    'Compare voting and advocacy patterns of technologically disadvantaged states across other arms-control contexts where they possess relevant capability (do they favor categorical bans symmetrically, or only when the ban targets a technology they lack?).',
    'If voting patterns show asymmetric support only where the state lacks capability, this substantially supports classifying the reading as tangled_rope (real humanitarian coordination function coexisting with strategic extraction) rather than a pure rope grounded solely in moral principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_humanitarian_framing, empirical, 'Whether support for the categorical ban correlates with lacking the banned capability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__categorical_prohibition_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(ihl__tr_t4, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(ihl__tr_t8, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 8, 0.23).
narrative_ontology:measurement(ihl__tr_t12, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(ihl__tr_t16, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(ihl__tr_t20, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ihl__be_t4, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(ihl__be_t8, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(ihl__be_t12, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(ihl__be_t16, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement(ihl__be_t20, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 20, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ihl__su_t4, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(ihl__su_t8, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(ihl__su_t12, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(ihl__su_t16, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(ihl__su_t20, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__categorical_prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, outcomes_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposed from the natural-language label 'the Martens Clause bars autonomous weapons,' per the epsilon-invariance principle. categorical_prohibition_reading (this file) authors the highest extractiveness (0.71) because it bans the entire technology class without a performance-based exception. human_agency_reading authors a narrower prohibition grounded in irreducible human judgment at the moment of force, compatible with automation short of full delegation, and should show materially lower extractiveness. outcomes_based_reading authors the lowest extractiveness and likely a rope or tangled_rope classification, since it conditions permissibility on demonstrated performance parity rather than banning the means outright. All three share the same kernel (ihl_distinction_proportionality) and the same underlying Martens Clause text but instantiate structurally distinct legal claims with different beneficiary/victim sets; they must not be treated as one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
