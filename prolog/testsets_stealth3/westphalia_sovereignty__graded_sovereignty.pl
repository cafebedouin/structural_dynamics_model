% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__graded_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__graded_sovereignty, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: westphalia_sovereignty__graded_sovereignty
 *   human_readable: Graded Sovereignty - Capacity-Calibrated Intervention Legitimacy
 *   domain: political/international-law/state-systems
 *
 * SUMMARY:
 *   This story instantiates the graded_sovereignty reading of the
 *   westphalia_sovereignty kernel: territorial authority treated as a
 *   measurable spectrum, with intervention legitimacy scaling to capacity
 *   deficits. The standing arrangement under contest - and the epsilon
 *   referent - is the existing capacity-grading regime: governance indicator
 *   production, ranking publication, loan conditionality, and the
 *   intervention precedents that cited deficit designations. The regime
 *   presents itself as technical assessment; the structural delta is a de
 *   facto tiered state system in which the evaluators are insulated from
 *   evaluation. Constraint-family decomposition applies:
 *   absolute_non_intervention and conditional_responsibility are separate
 *   stories with their own epsilon values and victim sets, linked through
 *   network.affects_constraints. Claim and metrics are authored
 *   independently: the reading is CLAIMED as tangled_rope because both a
 *   genuine coordination function (a shared yardstick making allocation
 *   across heterogeneous states possible) and asymmetric extraction
 *   (evaluator-written metrics, self-insulation of strong states,
 *   deficit-licensed intervention) are structurally present; the metrics
 *   describe substantially extractive, actively enforced operation without
 *   being tuned to any predicted engine verdict.
 *
 * KEY AGENTS:
 *   - - international_financial_institutions: Agenda-setting evaluator (institutional/arbitrage) - produces the capacity metrics and writes the conditions attached to them
 *   - - intervention_capable_powers: Primary beneficiary (institutional/arbitrage) - converts other states' deficits into warrants for sanctions and intervention
 *   - - high_capacity_states: Insulated beneficiary (institutional/arbitrage) - top scores function as sovereignty insurance
 *   - - low_capacity_states: Primary target (powerless/trapped) - bears continuous assessment, conditionality, and licensed oversight
 *   - - populations_of_oversight_targets: Secondary target (powerless/trapped) - lives under externally shaped arrangements with mediated consent
 *   - - middle_power_states: Dual-positioned (moderate/constrained) - partial insulation plus permanent downgrade exposure
 *   - - nonaligned_movement_members: Excluded critic (organized/constrained) - objects to the tiering without a seat in metric design
 *   - - international_law_scholars: Analytical observer (analytical/analytical) - documents the gap between charter doctrine and graded practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, 0.68).
domain_priors:suppression_score(westphalia_sovereignty__graded_sovereignty, 0.62).
domain_priors:theater_ratio(westphalia_sovereignty__graded_sovereignty, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, extractiveness, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__graded_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__graded_sovereignty, "Graded Sovereignty - Capacity-Calibrated Intervention Legitimacy").
narrative_ontology:topic_domain(westphalia_sovereignty__graded_sovereignty, "political/international-law/state-systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__graded_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__graded_sovereignty, 'bf037657-6a5c-43e0-8356-27772909bf2c').
narrative_ontology:cs_kernel_codification('bf037657-6a5c-43e0-8356-27772909bf2c', distributed).
narrative_ontology:cs_authority_grounding('bf037657-6a5c-43e0-8356-27772909bf2c', expertise).
narrative_ontology:cs_interpretation_layer_present('bf037657-6a5c-43e0-8356-27772909bf2c').
narrative_ontology:cs_reading_relation('bf037657-6a5c-43e0-8356-27772909bf2c', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('bf037657-6a5c-43e0-8356-27772909bf2c', westphalia_sovereignty__conditional_responsibility, coexists_with).
narrative_ontology:cs_axiom('bf037657-6a5c-43e0-8356-27772909bf2c', foundational, intervention_legitimacy_tracks_capacity_deficit).
narrative_ontology:cs_axiom_status(intervention_legitimacy_tracks_capacity_deficit, holdable).
narrative_ontology:cs_axiom_grounding('bf037657-6a5c-43e0-8356-27772909bf2c', intervention_legitimacy_tracks_capacity_deficit, empirically_contingent).
narrative_ontology:cs_axiom('bf037657-6a5c-43e0-8356-27772909bf2c', foundational, sovereignty_is_measurable_performance_not_juridical_status).
narrative_ontology:cs_axiom_status(sovereignty_is_measurable_performance_not_juridical_status, holdable).
narrative_ontology:cs_axiom_grounding('bf037657-6a5c-43e0-8356-27772909bf2c', sovereignty_is_measurable_performance_not_juridical_status, empirically_contingent).
narrative_ontology:cs_reference_frame('bf037657-6a5c-43e0-8356-27772909bf2c', sovereignty_as_scalar_capacity_spectrum).
narrative_ontology:cs_drift_state('bf037657-6a5c-43e0-8356-27772909bf2c', contemporary_multipolar_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('bf037657-6a5c-43e0-8356-27772909bf2c', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, international_financial_institutions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, intervention_capable_powers).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, high_capacity_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, low_capacity_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, populations_of_oversight_targets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, middle_power_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, middle_power_states).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__graded_sovereignty, capacity_metrics_measure_governance).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__graded_sovereignty, deficit_calibrated_intervention_effectiveness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce the governance indicators, country assessments, and program reviews through which credit, aid, and eligibility flow; attach policy conditions to disbursements and publish the rankings that other actors cite. Voting weights and staffing skew toward high-capacity states. They define and can revise the assessment methodology; no external forum compels a methodology they resist.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, international_financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold the military, financial, and diplomatic capacity to act on deficit designations. The grading framework converts other states' low scores into warrants for sanctions, no-fly zones, and transitional administrations, supplying a legitimacy vocabulary for actions their own domestic arrangements are never equivalently scored against.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, intervention_capable_powers, beneficiary,
    institutional, generational, arbitrage, global).

% Score near the top of every index; evaluation functions as sovereignty insurance, since their territorial authority is presumptively intact and outside scrutiny. They help fund and staff the evaluation bodies and accept occasional downgrades as reputational noise with no oversight consequences attached.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, high_capacity_states, beneficiary,
    institutional, generational, arbitrage, global).

% Live under continuous assessment they did not design and cannot veto. Policy choices arrive pre-shaped by loan conditions and benchmark reviews; deficit designations license external administration of parts of their affairs, from central banks to customs. Exit would mean leaving international credit, aid, and recognized statehood - unavailable at any acceptable price.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, low_capacity_states, payer,
    powerless, biographical, trapped, national).

% Live under arrangements negotiated between external authorities and their own governments: austerity packages, intervention disruption, and years-long transitional administrations. Their consent is mediated entirely by others; they bear the daily costs of both the deficits being scored and the responses the scores justify.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, populations_of_oversight_targets, payer,
    powerless, immediate, trapped, local).

% Sit mid-table in the rankings. They obtain partial insulation from intervention and access to programs and credit, but live under persistent downgrade risk that stronger actors can activate; they comply pre-emptively to stay above thresholds where oversight begins. Their position is genuinely mixed: the same scores that expose them also route resources to them.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, middle_power_states, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__graded_sovereignty, middle_power_states, beneficiary).

% Caucus of states that read the tiering as renewed hierarchy and propose restoring formal sovereign equality or building alternative assessment forums. They hold assembly votes and summit communiques but no seat where indicators are designed, weighted, or revised; their objections register as rhetoric outside the technical process that allocates.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, nonaligned_movement_members, excluded,
    organized, generational, constrained, continental).

% Track the doctrinal movement from juridical equality toward graded practice, documenting the widening gap between charter text and operational behavior. They publish critiques and histories but hold no seat in allocation decisions and no lever over methodology.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__graded_sovereignty, international_financial_institutions).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__graded_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common, if contested, yardstick for comparing nearly two hundred heterogeneous governments - enabling aid targeting, credit pricing, treaty-compliance assessment, and peacekeeping deployment decisions that some shared standard makes possible at all.
% TRANSFER_FUNCTION: Moves policy discretion over domestic arrangements from low-scoring states to external evaluation authorities and capable powers, and moves aid and credit toward low scorers on conditions the evaluators themselves write.
% ABSENT_VOICES: The evaluated states have little seat where indicators are designed; populations of administered territories are represented only indirectly through intervening agencies; postcolonial and G77 critics of the tiering stand outside the technical bodies that produce the scores their objections concern.
% DISAPPEARANCE_RATIONALE: Overnight loss of the grading regime would strand aid allocation, credit pricing, and intervention authorization without their current legitimating frame: lenders would fall back on narrower commercial judgment, security organs on ad hoc great-power justification, and weak states would regain a formal but contested inviolability. The whole architecture of conditional legitimacy would have to rebuild around one of the rival readings of the kernel.
% FOUNDING_PROBLEM: After the Cold War, state collapse in Somalia, the Balkans, and Rwanda exposed the gap between formal sovereign equality and actual governing capacity, and the system needed a principled answer to when territorial inviolability should yield to external action.
% FOUNDING_PROBLEM_CORROBORATION: UN peacekeeping reporting and humanitarian-agency assessments attest that state failure and capacity collapse persist; the comparative politics literature on state fragility corroborates independently of any benefiting party; G77 statements and postcolonial scholarship - seated outside the beneficiary set - attest the problem is live while disputing that the grading regime addresses it rather than exploiting it.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__graded_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__graded_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__graded_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalia_sovereignty__graded_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__graded_sovereignty, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__graded_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__graded_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the regime transfers real decision authority - budget, monetary, security policy - from scored states to unevaluated evaluators, and the transfer is decoupled from the targets' consent. It sits below snare levels because genuine goods flow through the same pipes (aid reaches low scorers, credit prices off shared data, peacekeeping deploys against verified collapse) and because strong-state sovereignty is insulated rather than harvested. Suppression (0.62) is structural: exit from the evaluation regime is effectively impossible because credit, aid, and recognized legitimacy all route through it, while overt coercion is episodic (interventions, sanctions) rather than constant. Theater ratio (0.38) reflects a real analytic core in indicator production surrounded by a growing performative layer - rankings consumed as legitimation ritual, benchmarks maintained after the decisions they nominally inform have moved elsewhere. Accessibility collapse (0.45): alternatives persist (South-South lending, regional development banks, nonaligned forums) but are constrained by the same credit and legitimacy dependencies the regime controls. Resistance (0.55): sustained G77 critique, BRICS institution-building, and sovereignty-resurgence diplomacy meet the regime continuously without displacing it. The temporal series run on one shared grid (t=0,6,12,18,24,30) with all three metrics authored at every point; the rising suppression_requirement series is authored deliberately because the story traces enforcement-capacity change - ad hoc humanitarian intervention hardened into institutionalized conditionality, sanctions regimes, and protection mandates over the interval. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the international_financial_institutions seat the arrangement is a technical allocation framework it administers and can revise - coordination dominant. From low_capacity_states the identical structure operates as continuous external supervision of domestic policy - extraction dominant. intervention_capable_powers experience it as a legitimacy converter that prices other states' failures; high_capacity_states experience it as insurance they never cash. The same-level lateral dynamic is sharpest here: all states are formally equal members of one system, yet exit options differ radically - strong states hold arbitrage (they fund, staff, and write the metrics), weak states are trapped, and middle powers live in the constrained band between. The evaluation bodies also show institutional identity fusion: methodology revision threatens the organizations' standing and self-conception as neutral arbiters, so drift in the metrics is absorbed by interpretation layers rather than surfaced as revision.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation: international_financial_institutions, intervention_capable_powers, and high_capacity_states sit near the beneficiary end (d low, effective extraction damped or inverted into subsidy); low_capacity_states and populations_of_oversight_targets sit near the full-target end (trapped exit pushes d high, amplified further by the global scope of verification difficulty for the institutional seats versus the national scope where extraction lands). One override is authored: middle_power_states hold power atom 'moderate', and the derivation from their primary payer role with constrained exit would place them around 0.65-0.7, but their true structural position is near-symmetric - the same scores that expose them to downgrade also route resources and partial insulation to them. The override sets d=0.5 for that atom; no other stakeholder occupies the moderate atom, so the correction touches only this seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - deciding when inviolability yields amid real state failure - remains live: fragile states, capacity collapse, and atrocity risk persist, corroborated from outside the benefiting parties. The mismatch consumer therefore reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds consistency: no zombie-mandate flag fires. Mandatrophy is not resolved, but the rising theater_ratio series is the watch item: as rankings become ends in themselves and benchmarks outlive the decisions they informed, the coordination half of the tangled rope atrophies while the extraction half persists - the classic drift path from tangled_rope toward piton or snare. The tangled_rope classification prevents both mislabels available here: a pure-snare reading would erase the real allocation function that aid-dependent states would mourn, and a pure-rope reading would erase the hierarchy that the evaluated states did not consent to and cannot exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the westphalia_sovereignty kernel (graded_sovereignty); how would instantiating a sibling reading change the structural classification?',
    'Cross-reading comparison across the constraint family: author and compile absolute_non_intervention and conditional_responsibility as separate stories and compare computed per-seat classifications over the shared referent.',
    'Under absolute_non_intervention the beneficiary/victim structure inverts: no seat collects from evaluation, intervention is per se illegitimate, and epsilon collapses toward negligible. Under conditional_responsibility the victim set narrows to atrocity-failing states and the scalar gradient disappears, removing the tiering extraction channel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification: sibling readings of the same kernel instantiate different constraints with different epsilon and different victim sets.').

omega_variable(
    metric_design_capture,
    'Do the capacity metrics embed evaluator interests - self-assessed strong states, donor-prioritized indicators, methodology weighted toward what evaluators can observe?',
    'Audit of indicator construction and governance weighting, plus out-of-sample validation of metric scores against independent outcome measures (service delivery, violence rates, fiscal capacity) uncontaminated by evaluator selection.',
    'Confirmed capture raises effective extraction above the authored base and supports drift toward snare; refutation supports treating a larger share of measured extraction as genuine coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_design_capture, empirical, 'Whether the measurement apparatus is neutral or structurally biased toward its operators.').

omega_variable(
    assessment_intervention_separability,
    'Is the capacity-assessment function structurally separable from the intervention-licensing function, or does the same act of scoring necessarily convert deficits into warrants?',
    'Examine episodes and domains where assessment operates without licensing consequences (pure technical assistance relationships, statistical capacity building) and compare extraction profiles against licensing-linked assessment.',
    'If separable, the licensing layer is pure extraction riding on a real coordination function and could be stripped without losing the allocation good; if inseparable, part of the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(assessment_intervention_separability, conceptual, 'Whether coordination and extraction components of the grading regime are structurally separable.').

omega_variable(
    hierarchy_naturalness,
    'Is functional differentiation of territorial authority an emergent property of any anarchic system containing heterogeneous states (mountain-like), or a constructed hierarchy maintained by evaluator power and funding control?',
    'Compare state-system episodes with and without an institutionalized evaluation apparatus; trace whether capacity-correlated differentiation persists when the apparatus is absent or contested, as in periods of multipolar sovereignty assertion.',
    'If emergent, the arrangement approaches a natural-law profile and extraction readings overstate agency; if constructed, the false-summit signature applies and the hierarchy is a maintainable artifact of identifiable beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hierarchy_naturalness, conceptual, 'Natural-law versus constructed-hierarchy ambiguity in the sovereignty gradient.').

omega_variable(
    internalized_metric_frames,
    'Is weak-state compliance driven by structural dependency alone, or also by elite internalization of the evaluators'' frames - ministries reformulating domestic policy in the metrics'' own vocabulary?',
    'Post-conditionality discourse analysis: track whether evaluator frames persist in target-state policy documents after leverage is removed, and compare against states that exited programs.',
    'An internalized component means effective suppression exceeds the structural measure - targets carry the constraint into their own planning after external pressure lifts, raising persistence and lowering measured resistance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_metric_frames, empirical, 'Structural versus internalized suppression in target-state compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__graded_sovereignty, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__graded_sovereignty, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(west_tr_t0, observed).
narrative_ontology:measurement(west_tr_t6, westphalia_sovereignty__graded_sovereignty, theater_ratio, 6, 0.25).
narrative_ontology:measurement_basis(west_tr_t6, observed).
narrative_ontology:measurement(west_tr_t12, westphalia_sovereignty__graded_sovereignty, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(west_tr_t12, observed).
narrative_ontology:measurement(west_tr_t18, westphalia_sovereignty__graded_sovereignty, theater_ratio, 18, 0.31).
narrative_ontology:measurement_basis(west_tr_t18, observed).
narrative_ontology:measurement(west_tr_t24, westphalia_sovereignty__graded_sovereignty, theater_ratio, 24, 0.35).
narrative_ontology:measurement_basis(west_tr_t24, observed).
narrative_ontology:measurement(west_tr_t30, westphalia_sovereignty__graded_sovereignty, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(west_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(west_be_t0, observed).
narrative_ontology:measurement(west_be_t6, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 6, 0.5).
narrative_ontology:measurement_basis(west_be_t6, observed).
narrative_ontology:measurement(west_be_t12, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 12, 0.56).
narrative_ontology:measurement_basis(west_be_t12, observed).
narrative_ontology:measurement(west_be_t18, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 18, 0.61).
narrative_ontology:measurement_basis(west_be_t18, observed).
narrative_ontology:measurement(west_be_t24, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(west_be_t24, observed).
narrative_ontology:measurement(west_be_t30, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(west_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(west_su_t0, observed).
narrative_ontology:measurement(west_su_t6, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 6, 0.46).
narrative_ontology:measurement_basis(west_su_t6, observed).
narrative_ontology:measurement(west_su_t12, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 12, 0.52).
narrative_ontology:measurement_basis(west_su_t12, observed).
narrative_ontology:measurement(west_su_t18, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 18, 0.57).
narrative_ontology:measurement_basis(west_su_t18, observed).
narrative_ontology:measurement(west_su_t24, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 24, 0.6).
narrative_ontology:measurement_basis(west_su_t24, observed).
narrative_ontology:measurement(west_su_t30, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(west_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__graded_sovereignty, resource_allocation).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, conditional_responsibility).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the westphalia_sovereignty kernel per the epsilon-invariance principle: the colloquial label 'sovereignty' covers three structurally distinct claims held by different parties. This file instantiates graded_sovereignty and authors epsilon for the standing capacity-grading arrangement (indices, conditionalities, deficit-calibrated intervention practice) as seen from that reading. absolute_non_intervention instantiates the categorical-inviolability claim (epsilon near zero for the inviolability norm itself); conditional_responsibility instantiates the threshold-forfeiture claim (victims limited to atrocity-failing states). Each sibling is a separate story with its own epsilon, beneficiaries, and victims; this story links to both as family members. Upstream/downstream: the graded reading's metric apparatus supplies the evidentiary surface the conditional reading's early-warning mechanisms consume, while the absolute reading supplies the rhetorical baseline both others define themselves against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__graded_sovereignty, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
