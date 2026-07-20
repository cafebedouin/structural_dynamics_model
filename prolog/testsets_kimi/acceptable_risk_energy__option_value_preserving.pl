% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__option_value_preserving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__option_value_preserving, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: acceptable_risk_energy__option_value_preserving
 *   human_readable: Option-Preserving Acceptable Risk in Energy Portfolio Planning
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the option-value-preserving reading of
 *   the acceptable_risk_energy kernel. Under this reading, energy policy
 *   should maintain nuclear, fossil, and renewable pathways in a reversible,
 *   diversified portfolio because irreducible uncertainty about future
 *   technology costs, climate sensitivity, and geopolitical conditions makes
 *   premature closure a permanently irreversible mistake. The constraint
 *   operates through capacity markets, strategic reserves, and
 *   technology-neutral reliability mandates administered by grid regulators.
 *   Incumbent fossil and nuclear operators capture concentrated financial
 *   flows from this arrangement, while ratepayers finance the preservation
 *   and rapid-transition advocates bear opportunity costs in delayed
 *   decarbonization and extended nuclear risk exposure. The constraint
 *   moderately suppresses both catastrophic-tail-dominant and
 *   expected-value-dominant policy extremes, framing them as reckless
 *   single-pathway bets.
 *
 * KEY AGENTS:
 *   - Grid regulators (institutional/agenda_setter): Administer capacity markets and reliability standards, interpreting deep uncertainty as mandating pathway diversity.
 *   - Incumbent pathway operators (powerful/beneficiary): Fossil and nuclear generators receiving capacity payments justified as preserving essential options.
 *   - Ratepayers (moderate/payer): Captive retail customers funding incumbent preservation through regulated surcharges.
 *   - Climate mitigation advocates (organized/payer): Bear opportunity costs of delayed fossil retirement and suppressed rapid decarbonization.
 *   - Nuclear risk advocates (organized/payer): Bear opportunity costs of continued exposure to catastrophic tail risks from retained nuclear fleets.
 *   - Decision theorists (analytical/observer): Provide intellectual framework of real options under deep uncertainty without capturing rents.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__option_value_preserving, 0.45).
domain_priors:suppression_score(acceptable_risk_energy__option_value_preserving, 0.52).
domain_priors:theater_ratio(acceptable_risk_energy__option_value_preserving, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, extractiveness, 0.45).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__option_value_preserving, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__option_value_preserving, "Option-Preserving Acceptable Risk in Energy Portfolio Planning").
narrative_ontology:topic_domain(acceptable_risk_energy__option_value_preserving, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__option_value_preserving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__option_value_preserving, '5005b6e8-c3d9-4a9d-af1c-c3d642fd5617').
narrative_ontology:cs_kernel_codification('5005b6e8-c3d9-4a9d-af1c-c3d642fd5617', formalized).
narrative_ontology:cs_authority_grounding('5005b6e8-c3d9-4a9d-af1c-c3d642fd5617', expertise).
narrative_ontology:cs_interpretation_layer_present('5005b6e8-c3d9-4a9d-af1c-c3d642fd5617').
narrative_ontology:cs_reading_relation('5005b6e8-c3d9-4a9d-af1c-c3d642fd5617', acceptable_risk_energy__catastrophic_tail_dominant, influences).
narrative_ontology:cs_reading_relation('5005b6e8-c3d9-4a9d-af1c-c3d642fd5617', acceptable_risk_energy__expected_value_dominant, influences).
narrative_ontology:cs_axiom('5005b6e8-c3d9-4a9d-af1c-c3d642fd5617', foundational, irreversible_closure_unwarranted).
narrative_ontology:cs_axiom_status(irreversible_closure_unwarranted, holdable).
narrative_ontology:cs_axiom_grounding('5005b6e8-c3d9-4a9d-af1c-c3d642fd5617', irreversible_closure_unwarranted, instrumental).
narrative_ontology:cs_axiom('5005b6e8-c3d9-4a9d-af1c-c3d642fd5617', foundational, deep_uncertainty_persistent).
narrative_ontology:cs_axiom_status(deep_uncertainty_persistent, holdable).
narrative_ontology:cs_axiom_grounding('5005b6e8-c3d9-4a9d-af1c-c3d642fd5617', deep_uncertainty_persistent, empirically_contingent).
narrative_ontology:cs_reference_frame('5005b6e8-c3d9-4a9d-af1c-c3d642fd5617', reversible_pathway_equilibrium).
narrative_ontology:cs_drift_state('5005b6e8-c3d9-4a9d-af1c-c3d642fd5617', contemporary_transition_policy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5005b6e8-c3d9-4a9d-af1c-c3d642fd5617', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, incumbent_pathway_operators).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, ratepayers).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, climate_mitigation_advocates).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, nuclear_risk_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer capacity markets, reliability standards, and strategic reserve frameworks that mandate preservation of multiple generation technologies. They set the rules for what counts as acceptable risk to system adequacy and interpret deep uncertainty as requiring pathway diversity.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, grid_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Own and operate fossil and nuclear generation assets that receive capacity payments and grid service contracts justified as preserving system flexibility. Their revenue streams depend directly on the constraint's continued classification of their assets as essential reversible options.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, incumbent_pathway_operators, beneficiary,
    powerful, generational, constrained, national).

% Pay regulated retail rates that include capacity charges and stranded-cost recovery mechanisms funding incumbent plant preservation. They cannot opt out of the regulated grid or the technology-neutral reliability mandate.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, ratepayers, payer,
    moderate, biographical, constrained, national).

% Bear the opportunity cost of cumulative emissions from delayed fossil fuel retirement, which the option-value framework treats as a reversible future decision rather than an urgent present harm. Their preferred accelerated decarbonization pathway is institutionally suppressed as an illegitimate extreme.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, climate_mitigation_advocates, payer,
    organized, generational, constrained, global).

% Bear the opportunity cost of continued exposure to catastrophic tail risks from retained nuclear fleets. The constraint treats permanent nuclear closure as an irreversible mistake to be avoided, suppressing their safety-first framing.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, nuclear_risk_advocates, payer,
    organized, generational, constrained, national).

% Develop and publish real options and robust decision-making frameworks that treat pathway preservation as rational under deep uncertainty. They do not administer the constraint or capture its financial flows, but their analytical legitimacy is cited by regulators to justify the framework.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, decision_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__option_value_preserving, incumbent_pathway_operators).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__option_value_preserving, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under irreducible uncertainty about future technology costs, climate sensitivity, and energy demand, maintaining a diversified and reversible generation portfolio avoids lock-in to a single pathway that might prove suboptimal or infeasible once uncertainty resolves.
% TRANSFER_FUNCTION: Moves current financial resources from ratepayers to incumbent generators via capacity payments and regulatory forbearance, and imposes opportunity costs on rapid-transition advocates by delaying closure timelines and suppressing single-pathway policy extremes.
% ABSENT_VOICES: Future generations who will inherit either the climate costs of delayed closure or the benefits of preserved flexibility have no seat at the decision table. Additionally, proponents of rapid single-pathway transition (complete renewable electrification or complete nuclearization) are present in discourse but structurally marginalized as extremists.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, capacity payments would cease, uneconomic fossil and nuclear plants would retire faster, grid planning would shift toward whichever single pathway dominated local politics, and the institutional machinery of technology-neutral reliability would collapse into explicit technology-picking.
% FOUNDING_PROBLEM: Energy policy in the twentieth and early twenty-first centuries faced repeated forecasting failures and technology lock-ins (e.g., overbuilt nuclear in the 1970s, missed renewables transitions). The option-value approach was built to prevent premature commitment to a single generation paradigm before technological and climatic uncertainties resolved.
% FOUNDING_PROBLEM_CORROBORATION: Energy historians and decision theorists outside the benefiting industries attest to the repeated lock-in failures. However, climate scientists and ecological economists attest that uncertainty about fossil fuel harm has narrowed sufficiently that the founding problem of 'deep uncertainty' is partially resolved, undermining the continued need for pathway neutrality.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__option_value_preserving, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__option_value_preserving, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__option_value_preserving, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_energy__option_value_preserving, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__option_value_preserving, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__option_value_preserving_tests).
:- end_tests(acceptable_risk_energy__option_value_preserving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the extraction is structurally diffuse: ratepayers pay explicit surcharges, while advocates bear opportunity costs from delayed transitions. Suppression is moderate (0.52) because the constraint must actively marginalize both abolitionist extremes but cannot fully silence them in democratic discourse. Theater ratio (0.30) reflects growing performative maintenance of 'strategic reserve' rhetoric for assets that are economically obsolete. Accessibility collapse (0.40) is partial: single-pathway alternatives remain visible and debated but are institutionally disadvantaged. Resistance (0.50) is substantial because both extremes actively contest the middle position. Temporal measurements share a single grid; extractiveness rises as capacity markets mature from genuine reliability instruments to incumbent rent channels.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator and incumbent operator seats, the constraint is genuine coordination against technology lock-in. From the climate and nuclear-risk advocate seats, the same structure is extraction that externalizes irreversible harms to preserve theoretically reversible options. From the ratepayer seat, it is an opaque surcharge with unproven future benefit. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Grid regulators and incumbent operators sit near the beneficiary end: regulators set rules without bearing direct costs, while operators collect capacity payments. Ratepayers sit near the target end because they pay the surcharge with no offsetting benefit. Climate mitigation and nuclear-risk advocates sit near the target end because they bear the opportunity costs of suppressed transition pathways. Decision theorists sit near the symmetric/analytical pole: they provide legitimizing concepts but neither pay nor collect.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading prevents mislabeling by separating the coordination function (real options under deep uncertainty) from the extraction function (incumbent capture via capacity payments). If the underlying uncertainty had clearly resolved, the coordination function would be dead and the constraint would degrade toward a piton or snare; the contested founding_problem_status keeps the tangled_rope classification honest. The mandate is not yet resolved because the uncertainty claim remains live in policy discourse, even as empirical evidence narrows.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deep_uncertainty_resolved,
    'Has deep uncertainty about future technology costs, climate sensitivity, and energy demand actually narrowed enough to render option-value preservation wasteful?',
    'Compare integrated assessment model confidence intervals and technology cost distributions over two decades; if distributions have tightened significantly, the option value has fallen.',
    'If uncertainty has resolved, the coordination function is hollow and the constraint shifts toward snare or piton classification; if uncertainty persists, tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deep_uncertainty_resolved, empirical, 'Whether deep uncertainty persists or has narrowed').

omega_variable(
    pathway_viability_genuineness,
    'Are the preserved pathways actually viable reversible options, or are they zombie assets kept alive purely for incumbent capture?',
    'Compare technical and economic viability of mothballed versus actively maintained plants; analyze restart costs, licensing timelines, and fuel supply chains to determine whether genuine future flexibility exists.',
    'If plants are not actually restartable or flexible, the coordination function is theater and the constraint shifts toward snare; if genuine flexibility exists, the coordination function is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pathway_viability_genuineness, empirical, 'Whether preserved pathways are genuine options or rent-seeking theater').

omega_variable(
    option_value_reading_boundary,
    'Does the option-value reading of acceptable risk represent a structurally distinct constraint, or does it collapse into expected-value optimization once uncertainty distributions are specified?',
    'Formal decision-theoretic proof or model showing whether real-options valuation yields materially different pathway rankings than standard expected mortality or harm minimization.',
    'If the readings collapse into one formula, the kernel decomposition fails and Îµ-invariance is violated; if distinct, the three-reading family is structurally valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(option_value_reading_boundary, conceptual, 'Structural boundary between option-value and expected-value readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__option_value_preserving, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acceptable_risk_energy_opt_tr_t0, acceptable_risk_energy__option_value_preserving, theater_ratio, 0, 0.1).
narrative_ontology:measurement(acceptable_risk_energy_opt_tr_t6, acceptable_risk_energy__option_value_preserving, theater_ratio, 6, 0.15).
narrative_ontology:measurement(acceptable_risk_energy_opt_tr_t12, acceptable_risk_energy__option_value_preserving, theater_ratio, 12, 0.2).
narrative_ontology:measurement(acceptable_risk_energy_opt_tr_t18, acceptable_risk_energy__option_value_preserving, theater_ratio, 18, 0.25).
narrative_ontology:measurement(acceptable_risk_energy_opt_tr_t24, acceptable_risk_energy__option_value_preserving, theater_ratio, 24, 0.28).
narrative_ontology:measurement(acceptable_risk_energy_opt_tr_t30, acceptable_risk_energy__option_value_preserving, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(acceptable_risk_energy_opt_be_t0, acceptable_risk_energy__option_value_preserving, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(acceptable_risk_energy_opt_be_t6, acceptable_risk_energy__option_value_preserving, base_extractiveness, 6, 0.3).
narrative_ontology:measurement(acceptable_risk_energy_opt_be_t12, acceptable_risk_energy__option_value_preserving, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(acceptable_risk_energy_opt_be_t18, acceptable_risk_energy__option_value_preserving, base_extractiveness, 18, 0.4).
narrative_ontology:measurement(acceptable_risk_energy_opt_be_t24, acceptable_risk_energy__option_value_preserving, base_extractiveness, 24, 0.42).
narrative_ontology:measurement(acceptable_risk_energy_opt_be_t30, acceptable_risk_energy__option_value_preserving, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(acceptable_risk_energy_opt_su_t0, acceptable_risk_energy__option_value_preserving, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(acceptable_risk_energy_opt_su_t6, acceptable_risk_energy__option_value_preserving, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(acceptable_risk_energy_opt_su_t12, acceptable_risk_energy__option_value_preserving, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(acceptable_risk_energy_opt_su_t18, acceptable_risk_energy__option_value_preserving, suppression_requirement, 18, 0.48).
narrative_ontology:measurement(acceptable_risk_energy_opt_su_t24, acceptable_risk_energy__option_value_preserving, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(acceptable_risk_energy_opt_su_t30, acceptable_risk_energy__option_value_preserving, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__option_value_preserving, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy__expected_value_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the acceptable_risk_energy kernel. The three readings (option_value_preserving, catastrophic_tail_dominant, expected_value_dominant) are structurally distinct claims with different epsilon values and victim sets. They compete for institutional dominance in energy risk governance and alter each other's legitimacy conditions, but are not causal descendants.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
