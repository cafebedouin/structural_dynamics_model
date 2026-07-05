% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__principle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__principle_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gelassenheit_separation__principle_reading
 *   human_readable: Gelassenheit Separation — Structural-Isolation (Principle) Reading
 *   domain: religious/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the 'principle reading' of the
 *   gelassenheit_separation kernel: separation from the world is defined
 *   structurally, by absence of ongoing entanglement with worldly
 *   infrastructure (the electrical grid, the internet, commercial insurance),
 *   rather than by visible resemblance to worldly artifacts (the sibling
 *   artifact_reading) or by measured effects on community practice (the
 *   sibling consequence_reading). Under this reading, a solar-powered
 *   irrigation pump or a pneumatic woodshop tool is acceptable because it can
 *   be engineered to have no ongoing tie back to a worldly system, even
 *   though it looks and functions like modern technology — while a landline
 *   telephone or basic internet connection is forbidden even when used
 *   minimally, because the connection itself is the entanglement, independent
 *   of appearance or measurable community harm. This produces a
 *   permissive-on-form, strict-on-connectivity profile distinct from both
 *   siblings: more permissive than the artifact reading toward off-grid
 *   modern equipment, more restrictive than the consequence reading toward
 *   low-impact connectivity that would pass a harm-based test.
 *
 * KEY AGENTS:
 *   - ministers_and_bishops: agenda_setter (institutional/arbitrage) — administer the isolation test case by case
 *   - off_grid_technology_adopters: beneficiary (moderate/constrained) — gain functional capability via engineered isolation
 *   - members_seeking_grid_dependent_tools: payer (moderate/constrained) — barred from grid/internet regardless of use
 *   - members_with_disabilities_needing_networked_devices: payer (powerless/trapped) — highest unmet need, least exit capacity
 *   - regional_bishops_conference: observer (institutional/analytical) — tracks cross-congregation consistency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__principle_reading, 0.28).
domain_priors:suppression_score(gelassenheit_separation__principle_reading, 0.42).
domain_priors:theater_ratio(gelassenheit_separation__principle_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__principle_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__principle_reading, "Gelassenheit Separation — Structural-Isolation (Principle) Reading").
narrative_ontology:topic_domain(gelassenheit_separation__principle_reading, "religious/technology_governance").

domain_priors:requires_active_enforcement(gelassenheit_separation__principle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__principle_reading, '6200d6eb-cc05-4312-8f08-65292b0c85e1').
narrative_ontology:cs_kernel_codification('6200d6eb-cc05-4312-8f08-65292b0c85e1', distributed).
narrative_ontology:cs_authority_grounding('6200d6eb-cc05-4312-8f08-65292b0c85e1', practice).
narrative_ontology:cs_interpretation_layer_present('6200d6eb-cc05-4312-8f08-65292b0c85e1').
narrative_ontology:cs_reading_relation('6200d6eb-cc05-4312-8f08-65292b0c85e1', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_reading_relation('6200d6eb-cc05-4312-8f08-65292b0c85e1', gelassenheit_separation__consequence_reading, influences).
narrative_ontology:cs_axiom('6200d6eb-cc05-4312-8f08-65292b0c85e1', foundational, entanglement_is_structural_not_visual).
narrative_ontology:cs_axiom_status(entanglement_is_structural_not_visual, holdable).
narrative_ontology:cs_axiom_grounding('6200d6eb-cc05-4312-8f08-65292b0c85e1', entanglement_is_structural_not_visual, conventional).
narrative_ontology:cs_axiom('6200d6eb-cc05-4312-8f08-65292b0c85e1', secondary, isolated_function_neutralizes_worldliness).
narrative_ontology:cs_axiom_status(isolated_function_neutralizes_worldliness, holdable).
narrative_ontology:cs_axiom_grounding('6200d6eb-cc05-4312-8f08-65292b0c85e1', isolated_function_neutralizes_worldliness, instrumental).
narrative_ontology:cs_reference_frame('6200d6eb-cc05-4312-8f08-65292b0c85e1', pre_electrification_agrarian_autonomy).
narrative_ontology:cs_drift_state('6200d6eb-cc05-4312-8f08-65292b0c85e1', contemporary_networked_economy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6200d6eb-cc05-4312-8f08-65292b0c85e1', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__principle_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, ministers_and_bishops).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, off_grid_technology_adopters).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, community_cohesion_interests).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, members_seeking_grid_dependent_tools).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, small_business_operators_needing_connectivity).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, members_with_disabilities_needing_networked_devices).
narrative_ontology:constraint_vindicates(gelassenheit_separation__principle_reading, functional_separation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate, congregation by congregation, whether a given technology counts as 'functionally isolated' — a solar panel powering a shop tool is approved, a grid tie-in or internet-connected device is not, regardless of how the device looks. They hold discretion over the isolation test itself, deciding case by case which entanglements count as structural, and their rulings bind the district. They bear none of the day-to-day cost of the tools they approve or forbid.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, ministers_and_bishops, agenda_setter,
    institutional, generational, arbitrage, regional).

% Farmers and tradesmen who can afford solar arrays, battery banks, or pneumatic tool systems get functional modern capability — irrigation pumps, power tools, milking equipment — without violating the separation principle, because these systems can be engineered to have no wire back to the grid or line back to a network. They benefit from a permissive reading that other readings (artifact-based) would have blocked on appearance alone.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, off_grid_technology_adopters, beneficiary,
    moderate, biographical, constrained, local).

% Members whose livelihoods would benefit from grid electricity or telephone/internet service — larger-scale dairy operations, home-based contractors bidding jobs online, families with members needing telehealth — are barred regardless of whether the specific use is otherwise low-risk, because grid and internet connections are treated as structural entanglement per se. They pay in foregone income and convenience that the isolation principle does not weigh against its own coherence.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, members_seeking_grid_dependent_tools, payer,
    moderate, biographical, constrained, local).

% Furniture makers, produce sellers, and craft businesses that could double revenue with e-commerce or networked payment processing cannot adopt them under this reading, since internet access constitutes structural entanglement with worldly commercial systems independent of how the connection is used. They compete against non-Anabaptist businesses without this tool and absorb the cost as a condition of membership.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, small_business_operators_needing_connectivity, payer,
    moderate, biographical, constrained, regional).

% Members needing networked medical monitoring, insurance-linked devices, or telecommunication aids for disability or chronic illness are denied under a blanket rule against internet and insurance participation, even though their specific use case has no bearing on community cohesion or visible worldliness. They have the least capacity to relocate to a more permissive congregation and the most acute unmet need.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, members_with_disabilities_needing_networked_devices, payer,
    powerless, biographical, trapped, local).

% The abstract interest in keeping the community's economic and social life outside the reach of grid utilities, insurance markets, and the internet — named for completeness as the non-actor good the doctrine claims to serve, distinct from any individual who administers or profits from the rule.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, community_cohesion_interests, beneficiary,
    institutional, generational, analytical, regional).
narrative_ontology:stakeholder_non_agent(gelassenheit_separation__principle_reading, community_cohesion_interests).

% Congregations holding the sibling artifact-based reading would object that a solar-powered internet router is still forbidden looking like a worldly artifact even if functionally isolated, and that the principle reading's permissiveness toward isolated grid-adjacent technology erodes visible distinctiveness. They are not seated in this reading's adjudication process, which is internal to congregations that have already adopted the principle test.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, artifact_reading_adherents, excluded,
    organized, generational, constrained, regional).

% Congregations holding the sibling consequence-based reading would object that a functionally isolated technology (e.g., a solar-powered personal entertainment device) could still erode visiting and mutual aid even without any structural entanglement, and that the principle reading ignores exactly the harm they measure. They have no voice in this reading's technology rulings.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, consequence_reading_adherents, excluded,
    organized, generational, constrained, regional).

% Tracks how individual congregations apply the isolation test, notes divergence and consistency across districts, and mediates disputes when members petition across congregation lines. Does not administer any single congregation's rulings but observes the pattern across many.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, regional_bishops_conference, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable, case-by-case test — functional isolation from worldly infrastructure — that lets congregations distinguish acceptable modern tools (solar power, pneumatic systems) from entangling ones (grid electricity, internet, insurance) without requiring a fixed list of forbidden objects that would need constant revision as technology changes.
% TRANSFER_FUNCTION: Moves discretionary authority to ministers and bishops who apply the isolation test, and moves real economic opportunity away from members whose livelihoods would benefit from grid-tied or networked technology, redirecting that opportunity toward members with the capital to build off-grid alternatives (solar arrays, batteries) that achieve similar function without triggering the entanglement bar.
% ABSENT_VOICES: Members needing networked medical or disability aids have no forum to argue their case is different from a business seeking internet commerce — the isolation test is applied categorically to the connection type, not to the use. Adherents of the sibling readings are also absent: they would push back on both the permissiveness (artifact reading) and the narrowness (consequence reading) of this test, but sit in different congregations entirely.
% DISAPPEARANCE_RATIONALE: Ministers and off-grid adopters would say the world rearranges — congregations would lose their principled basis for distinguishing acceptable technology from entangling technology and would either drift toward wholesale adoption or toward the stricter artifact reading by default. Payers denied grid and internet access would say little rearranges for them specifically unless the disappearance is replaced by outright permission; the dispute is over what replaces the test, not whether the test currently does real work.
% FOUNDING_PROBLEM: As diesel generators, solar technology, and telecommunications became cheap and widespread in the surrounding society, congregations needed a principled way to evaluate genuinely novel technologies (not covered by inherited artifact lists) without either freezing at a fixed 1900s technology set or dissolving separation into an unprincipled case-by-case negotiation with no doctrine behind it.
% FOUNDING_PROBLEM_CORROBORATION: Sociologists of Anabaptist communities (e.g. studies of Old Order technology adoption patterns) corroborate that congregations using functional/structural tests face genuinely novel technology questions every decade and that the test does real adjudicative work, not merely post-hoc rationalization — this attestation comes from academic observers outside the beneficiary set of ministers and off-grid adopters. Some affected payers dispute only the boundary-drawing (why internet counts as entanglement per se), not that a live adjudicative problem exists.
narrative_ontology:disappearance_verdict(gelassenheit_separation__principle_reading, contested).
narrative_ontology:founding_problem_status(gelassenheit_separation__principle_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__principle_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gelassenheit_separation__principle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__principle_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__principle_reading_tests).
:- end_tests(gelassenheit_separation__principle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored lower (0.28) than a plausible artifact-reading or consequence-reading sibling because the principle test is more permissive toward capital-intensive off-grid substitutes, reducing the number of members who are flatly denied functional equivalents to worldly tools. Suppression is moderate (0.42) because the categorical bar on internet and insurance connectivity is enforced without individualized exception even where the specific use (disability aid, telehealth) would not implicate structural entanglement in any practical sense — the rule bars the connection type, not the harm. Theater ratio is low (0.22): the isolation test does real discriminating work (solar yes, grid tie-in no) rather than merely performing separation.
 *
 * DIRECTIONALITY LOGIC:
 *   Ministers and bishops sit at the beneficiary/agenda-setter end: they hold the discretion to apply the isolation test and bear none of its costs. Off-grid adopters benefit from the reading's permissiveness toward engineered isolation. Members needing grid-dependent tools, connectivity for business, or networked disability aids are targets: the categorical bar on connection-type extracts real economic and welfare cost from them regardless of the actual entanglement risk their specific use presents, and their exit options are constrained (leaving the congregation) or, for disabled members with fewer resources, trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (needing a principled test for genuinely novel technology, rather than a frozen artifact list or an unprincipled negotiation) remains live per corroboration from outside observers — this is not classified as resolved mandatrophy. But the categorical connection-type bar, applied without regard to the specific entanglement risk of a given use (a telehealth monitor is treated identically to a commercial ISP contract), shows a rule that has hardened past the granularity its own founding rationale would justify — a milder, use-sensitive version of the drift the consequence_reading omega below interrogates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    principle_reading_kernel_disambiguation,
    'Is ''functional isolation'' a coherent, consistently-applicable test, or does its application collapse into artifact-resemblance reasoning or consequence-weighing in practice once ministers face hard cases (e.g. a solar-powered smartphone with no cellular plan)?',
    'Track adjudicated cases across congregations applying the principle test and code whether the stated rationale (structural entanglement) predicts the ruling better than artifact-resemblance or measured-community-effect would predict the same ruling.',
    'If the principle test frequently collapses into one of the sibling tests in hard cases, the three readings are less structurally distinct in practice than in doctrine, and the kernel''s coexists_with topology should be revisited toward more influence/convergence between readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(principle_reading_kernel_disambiguation, conceptual, 'Whether the principle reading is operationally distinct from its siblings or converges with them at the margins.').

omega_variable(
    categorical_vs_use_sensitive_entanglement,
    'Should the structural-entanglement bar on internet and insurance be categorical (connection type alone) or use-sensitive (does this specific use create the entanglement the doctrine is meant to prevent)?',
    'Compare congregations that grant individualized exceptions (e.g. for medical necessity) against those that apply the bar categorically; measure whether exception-granting congregations show measurably different rates of the entanglement the doctrine aims to prevent (debt dependency, worldly business assimilation).',
    'If use-sensitive exception-granting shows no increase in entanglement outcomes, the categorical form of this reading is over-extractive relative to its own stated rationale, and the current 0.28 extractiveness score may be an underestimate of avoidable cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_use_sensitive_entanglement, empirical, 'Whether the connection-type bar should be use-sensitive rather than categorical.').

omega_variable(
    off_grid_capital_stratification,
    'Does the principle reading''s permissiveness toward engineered off-grid technology (solar, batteries, pneumatics) create a wealth-stratified access pattern within the community, favoring members who can afford capital-intensive isolation infrastructure?',
    'Survey adoption rates of off-grid solar/pneumatic systems by household income within congregations applying the principle test; compare to adoption rates of the same functional capability among households that cannot afford the off-grid engineering premium.',
    'If adoption stratifies strongly by wealth, the reading''s apparent permissiveness masks a new extraction axis (capital access) layered on top of the doctrinal one, which would raise the effective extractiveness for lower-capital members beyond the story-level scalar.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(off_grid_capital_stratification, empirical, 'Whether off-grid technology access under this reading stratifies by household wealth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__principle_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__principle_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gela_tr_t8, gelassenheit_separation__principle_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(gela_tr_t16, gelassenheit_separation__principle_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(gela_tr_t24, gelassenheit_separation__principle_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement(gela_tr_t32, gelassenheit_separation__principle_reading, theater_ratio, 32, 0.21).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__principle_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__principle_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gela_be_t8, gelassenheit_separation__principle_reading, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(gela_be_t16, gelassenheit_separation__principle_reading, base_extractiveness, 16, 0.23).
narrative_ontology:measurement(gela_be_t24, gelassenheit_separation__principle_reading, base_extractiveness, 24, 0.25).
narrative_ontology:measurement(gela_be_t32, gelassenheit_separation__principle_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__principle_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__principle_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(gela_su_t8, gelassenheit_separation__principle_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(gela_su_t16, gelassenheit_separation__principle_reading, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(gela_su_t24, gelassenheit_separation__principle_reading, suppression_requirement, 24, 0.39).
narrative_ontology:measurement(gela_su_t32, gelassenheit_separation__principle_reading, suppression_requirement, 32, 0.41).
narrative_ontology:measurement(gela_su_t40, gelassenheit_separation__principle_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__principle_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__principle_reading, 0.1).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% Three sibling readings of the gelassenheit_separation kernel decompose a single colloquial concept ('Amish/Anabaptist separation from the world') into three structurally distinct constraints per the ε-invariance principle: artifact_reading (visible-distinction test, likely higher suppression and lower functional permissiveness), consequence_reading (community-effect test, likely most permissive toward isolated-but-connected technology that preserves visiting/mutual aid), and this principle_reading (structural-entanglement test, permissive toward off-grid engineered substitutes, strict on any live connection regardless of use). Each carries its own ε, beneficiary/victim structure, and classification; they are linked via affects_constraints for contamination/family analysis, never merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
