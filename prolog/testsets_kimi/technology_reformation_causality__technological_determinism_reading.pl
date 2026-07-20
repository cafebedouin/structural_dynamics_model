% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__technological_determinism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__technological_determinism_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: technology_reformation_causality__technological_determinism_reading
 *   human_readable: Technological Determinism Reading: Printing Press as Inevitable Cause of Reformation
 *   domain: history/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint story captures the technological determinism reading of
 *   the printing press's role in the Reformation: the press is treated as a
 *   physical-economic mountain that, by reducing text production costs, made
 *   mass vernacular scripture distribution inevitable and thereby caused the
 *   Reformation independent of reformer agency. The story is authored as a
 *   false-summit mountain candidate because identifiable beneficiaries
 *   (reformers, printers) and victims (Church hierarchy, copyists) are
 *   declared, triggering the FSM evaluation chain despite the mountain claim.
 *
 * KEY AGENTS:
 *   - reform_movements: Primary beneficiary (moderate/constrained) â gain scalable distribution from the technology
 *   - vernacular_printers: Secondary beneficiary (moderate/mobile) â capture commercial surplus from cost reduction
 *   - catholic_church_hierarchy: Primary target (institutional/constrained) â loses information monopoly to mechanical reproduction
 *   - manuscript_copyists: Secondary target (powerless/trapped) â displaced by technological substitution
 *   - economic_historians: Analytical observer (analytical/analytical) â empirically assess production cost differentials
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__technological_determinism_reading, 0.15).
domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, 0.1).
domain_priors:theater_ratio(technology_reformation_causality__technological_determinism_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__technological_determinism_reading, mountain).
narrative_ontology:human_readable(technology_reformation_causality__technological_determinism_reading, "Technological Determinism Reading: Printing Press as Inevitable Cause of Reformation").
narrative_ontology:topic_domain(technology_reformation_causality__technological_determinism_reading, "history/religious_history/media_studies").

domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__technological_determinism_reading, '966f0847-12b1-4162-9d96-81627432e4c4').
narrative_ontology:cs_kernel_codification('966f0847-12b1-4162-9d96-81627432e4c4', distributed).
narrative_ontology:cs_authority_grounding('966f0847-12b1-4162-9d96-81627432e4c4', expertise).
narrative_ontology:cs_interpretation_layer_present('966f0847-12b1-4162-9d96-81627432e4c4').
narrative_ontology:cs_reading_relation('966f0847-12b1-4162-9d96-81627432e4c4', technology_reformation_causality__beneficiary_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('966f0847-12b1-4162-9d96-81627432e4c4', technology_reformation_causality__co_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('966f0847-12b1-4162-9d96-81627432e4c4', foundational, technology_autonomously_determines).
narrative_ontology:cs_axiom_status(technology_autonomously_determines, holdable).
narrative_ontology:cs_axiom_grounding('966f0847-12b1-4162-9d96-81627432e4c4', technology_autonomously_determines, empirically_contingent).
narrative_ontology:cs_axiom('966f0847-12b1-4162-9d96-81627432e4c4', foundational, social_agents_are_downstream_adapters).
narrative_ontology:cs_axiom_status(social_agents_are_downstream_adapters, holdable).
narrative_ontology:cs_axiom_grounding('966f0847-12b1-4162-9d96-81627432e4c4', social_agents_are_downstream_adapters, empirically_contingent).
narrative_ontology:cs_reference_frame('966f0847-12b1-4162-9d96-81627432e4c4', technological_autonomy).
narrative_ontology:cs_drift_state('966f0847-12b1-4162-9d96-81627432e4c4', post_social_history_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('966f0847-12b1-4162-9d96-81627432e4c4', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, reform_movements).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, vernacular_printers).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, catholic_church_hierarchy).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, manuscript_copyists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate downstream of printing technology, using mass-produced vernacular scripture and theological pamphlets to disseminate doctrine across regions at a scale impossible under manuscript production. Their strategic options are bounded by the physical affordances of the press.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, reform_movements, beneficiary,
    moderate, biographical, constrained, continental).

% Commercialize the production of vernacular and reformist texts, capturing economic surplus from the dramatic reduction in per-unit reproduction costs relative to scribal copying.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, vernacular_printers, beneficiary,
    moderate, biographical, mobile, regional).

% Lose the monopoly on doctrinal interpretation and scriptural dissemination as identical vernacular texts circulate outside ecclesiastical control. Cannot exit the technological reality of mechanical reproduction.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, catholic_church_hierarchy, payer,
    institutional, generational, constrained, continental).

% Bear the economic and vocational costs of technological displacement as demand for hand-copied texts collapses; specialized scribal skills lose market value with no alternative path.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, manuscript_copyists, payer,
    powerless, immediate, trapped, regional).

% Measure and compare production costs between manuscript and print economies, providing empirical estimates of the physical cost reduction that underwrites technological determinism claims.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mass reproduction of identical texts across geographic space without centralized institutional control, solving the physical bottleneck of manual scribal copying.
% TRANSFER_FUNCTION: Moves text reproduction capacity from centralized ecclesiastical scriptoria to decentralized print shops; moves doctrinal dissemination control away from the Catholic Church toward any agent with access to a press.
% ABSENT_VOICES: Illiterate lay populations structurally excluded from the textual public sphere despite vernacular printing; women in enclosed religious orders losing institutional scriptorial roles; Eastern Orthodox and non-Latin Christian communities outside the initial print geography.
% DISAPPEARANCE_RATIONALE: If the production-cost reduction and mass reproduction capacity of movable-type printing vanished, vernacular scripture distribution would collapse back to manuscript scale; the decentralized, rapid theological mobilization of the Reformation would be structurally impossible, and the Church's information monopoly would reconstitute.
% FOUNDING_PROBLEM: The physical and economic impossibility of producing and distributing sufficient identical texts to sustain a mass theological movement against an institution controlling manuscript reproduction.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians of the book trade corroborate the magnitude of production cost reduction from outside the benefiting reformer parties; Catholic polemicists and Index compilers at the time corroborate the Church's perception of lost monopoly from an opposed seat.
narrative_ontology:disappearance_verdict(technology_reformation_causality__technological_determinism_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__technological_determinism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__technological_determinism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_reformation_causality__technological_determinism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__technological_determinism_reading, 0.15, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__technological_determinism_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, ExtMetricName, E),
    domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(technology_reformation_causality__technological_determinism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint is framed as a physical production-cost floor rather than an active extraction mechanism; suppression is low (0.10) because the press operates through physical possibility rather than coercion. Theater ratio is near-zero (0.05) as there is no performative maintenance. Accessibility collapse is high (0.88) because once print exists, manuscript alternatives collapse for mass distribution. Resistance is moderate-low (0.20): the Church resisted the consequences but could not resist the physics. Metrics are authored independently of the mountain claim to preserve divergence signal.
 *
 * PERSPECTIVAL GAP:
 *   Reformers and printers experience the press as an affordance that subsidizes their scale; the Church experiences it as an external force extracting its monopoly control. Copyists experience direct economic displacement. The engine will compute divergent per-seat classifications: beneficiaries near the subsidy end (low d), victims near the target end (high d), with the analytical seat seeing the full structural picture.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (reform_movements, vernacular_printers) derive low directionality because the constraint structurally subsidizes their capacity; victims (catholic_church_hierarchy, manuscript_copyists) derive high directionality because the same physical constraint extracts from their established position. The Church's institutional power is overridden by the universal scope of the technology, pushing its d toward the target end despite high global power.
 *
 * MANDATROPHY ANALYSIS:
 *   The mountain classification prevents mislabeling the press as a snare or tangled rope in the technological determinism reading: the press is not actively enforced by an agenda-setter, and its operation does not require suppressing alternatives (manuscript copying remains physically possible but economically non-viable). However, declaring beneficiaries and victims acknowledges that the 'mountain' framing may naturalize gains that are structurally contingent, routing the story through FSM to test whether the mountain claim survives or collapses into a different type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_tool,
    'Is the printing press a physical constraint with inevitable historical effects analogous to natural law, or a constructed tool whose effects depend entirely on social deployment choices?',
    'Comparative historiography: examine print cultures (e.g., Chinese woodblock, Ottoman print bans) where the same production-cost reduction did not produce Reformation-equivalent outcomes.',
    'If the effect is contingent on social context, the mountain claim is a false summit and the engine should reclassify toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_tool, conceptual, 'Whether printing press causality is physical inevitability or constructed narrative').

omega_variable(
    cost_reduction_inevitability,
    'Does production cost reduction inevitably undermine centralized religious authority, or is this correlation specific to the Latin Christian institutional context of the early sixteenth century?',
    'Counterfactual and comparative historical analysis across religious and political systems with and without print technology.',
    'If contingent, the constraint''s effective extractiveness is context-dependent rather than physically determined, undermining the mountain classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_reduction_inevitability, empirical, 'Whether cost reduction necessarily produces institutional destabilization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__technological_determinism_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_ref_td_tr_t0, technology_reformation_causality__technological_determinism_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tech_ref_td_tr_t50, technology_reformation_causality__technological_determinism_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(tech_ref_td_tr_t100, technology_reformation_causality__technological_determinism_reading, theater_ratio, 100, 0.06).

% Extraction over time
narrative_ontology:measurement(tech_ref_td_be_t0, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(tech_ref_td_be_t50, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 50, 0.13).
narrative_ontology:measurement(tech_ref_td_be_t100, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(tech_ref_td_su_t0, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(tech_ref_td_su_t50, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 50, 0.08).
narrative_ontology:measurement(tech_ref_td_su_t100, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 100, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, beneficiary_agency_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, co_constitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the technology_reformation_causality kernel. It is structurally paired with beneficiary_agency_reading and co_constitution_reading as a constraint family. Each reading carries a distinct epsilon and stakeholder geometry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
