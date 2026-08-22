% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__hybrid_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__hybrid_dependency, []).

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
 *   constraint_id: competence_exercise_requirement__hybrid_dependency
 *   human_readable: Mandatory Hybrid Competence Exercise Regime (Simulation Foundation Plus Real-World Anchoring)
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   In commercial aviation, no crew may carry revenue passengers without a
 *   current record of prescribed exercise: recurrent full-flight simulator
 *   cycles built around line-oriented scenarios, periodic line checks flown
 *   under observation, minimum recent-experience requirements, and, layered
 *   on top by operators and industry programs, non-jeopardy line audits in
 *   which trained observers ride normal flights and record crew performance
 *   without consequence to the crew. Aviation regulators administer the
 *   arrangement, carriers fund it, certified training organizations deliver
 *   it, insurers consume its outputs as priced assurance, and accident
 *   investigators audit it retrospectively against outcomes. This file
 *   instantiates the hybrid_dependency reading of the
 *   competence_exercise_requirement kernel (see kernel_context); the sibling
 *   readings are separate constraint files linked through the network
 *   section, and this file's epsilon refers solely to the standing mandatory
 *   hybrid regime. The claimed type and the metrics are authored
 *   independently: the claim states what this reading takes the arrangement
 *   structurally to be, and the metrics describe its observed operation.
 *
 * KEY AGENTS:
 *   - aviation_regulators: agenda setter (institutional/constrained) - specifies the mandated exercise mix, enforces it, collects legitimacy and budget from administration
 *   - approved_training_organizations: primary beneficiary (powerful/arbitrage) - sells mandated device hours behind a certification wall
 *   - major_network_carriers: payer with material return flow (powerful/constrained) - funds the largest share, receives insurability and floor protection
 *   - regional_and_low_cost_carriers: disproportionate payer (moderate/constrained) - same mandate on the thinnest margins
 *   - line_pilots: dual-positioned bearer (organized/identity_locked) - supplies time and performance, receives licensure and skill
 *   - aviation_insurers: beneficiary (powerful/mobile) - prices the risk the regime suppresses
 *   - flying_public: diffuse beneficiary and indirect payer (powerless/mobile) - receives the safety margin, pays through fares
 *   - airline_safety_departments: internal beneficiary (moderate/identity_locked) - administers the observation apparatus its standing depends on
 *   - accident_investigators: analytical observer (institutional/analytical) - attests whether exercise maps to outcomes
 *   - small_charter_operators: excluded voice (moderate/trapped) - bears the worst cost ratios with no seat in standard-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, 0.32).
domain_priors:suppression_score(competence_exercise_requirement__hybrid_dependency, 0.55).
domain_priors:theater_ratio(competence_exercise_requirement__hybrid_dependency, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, extractiveness, 0.32).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__hybrid_dependency, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__hybrid_dependency, "Mandatory Hybrid Competence Exercise Regime (Simulation Foundation Plus Real-World Anchoring)").
narrative_ontology:topic_domain(competence_exercise_requirement__hybrid_dependency, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__hybrid_dependency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__hybrid_dependency, 'a41afabf-4400-49c7-9519-a17bfe10c7ab').
narrative_ontology:cs_kernel_codification('a41afabf-4400-49c7-9519-a17bfe10c7ab', formalized).
narrative_ontology:cs_authority_grounding('a41afabf-4400-49c7-9519-a17bfe10c7ab', expertise).
narrative_ontology:cs_interpretation_layer_present('a41afabf-4400-49c7-9519-a17bfe10c7ab').
narrative_ontology:cs_reading_relation('a41afabf-4400-49c7-9519-a17bfe10c7ab', competence_exercise_requirement__simulation_as_adequate_exercise, coexists_with).
narrative_ontology:cs_reading_relation('a41afabf-4400-49c7-9519-a17bfe10c7ab', competence_exercise_requirement__catastrophe_as_necessary_anchor, influences).
narrative_ontology:cs_axiom('a41afabf-4400-49c7-9519-a17bfe10c7ab', foundational, dual_component_exercise_necessity).
narrative_ontology:cs_axiom_status(dual_component_exercise_necessity, holdable).
narrative_ontology:cs_axiom_grounding('a41afabf-4400-49c7-9519-a17bfe10c7ab', dual_component_exercise_necessity, empirically_contingent).
narrative_ontology:cs_axiom('a41afabf-4400-49c7-9519-a17bfe10c7ab', foundational, routine_operations_as_anchoring_source).
narrative_ontology:cs_axiom_status(routine_operations_as_anchoring_source, holdable).
narrative_ontology:cs_axiom_grounding('a41afabf-4400-49c7-9519-a17bfe10c7ab', routine_operations_as_anchoring_source, instrumental).
narrative_ontology:cs_reference_frame('a41afabf-4400-49c7-9519-a17bfe10c7ab', simulation_foundation_with_periodic_real_anchoring).
narrative_ontology:cs_drift_state('a41afabf-4400-49c7-9519-a17bfe10c7ab', competency_based_training_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('a41afabf-4400-49c7-9519-a17bfe10c7ab', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, aviation_regulators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, approved_training_organizations).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, aviation_insurers).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, flying_public).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, regional_and_low_cost_carriers).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, line_pilots).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, major_network_carriers).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, line_pilots).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, airline_safety_departments).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, flying_public).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, major_network_carriers).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__hybrid_dependency, proficiency_decay_without_exercise_hypothesis).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__hybrid_dependency, line_oriented_scenario_transfer_effectiveness).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__hybrid_dependency, crew_resource_management_performance_effect).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and enforces the rules specifying what exercise a crew must complete to keep operating: recurrent simulator cycles, line checks, recent-experience minimums, and approval of each operator's training program. Collects fees, staffing, and political legitimacy from administering the system, and answers to legislatures and the public after accidents. Statute obliges the office to hold the oversight role, and post-accident accountability punishes relaxation far more severely than rigor.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, aviation_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Build and operate the certified simulators and training centers that every carrier must purchase time from. Certification standards limit who may supply the mandated device categories, so demand arrives by rule rather than by competitive sale. Revenue scales with mandated hours; the largest firms diversify across airlines, regions, and adjacent markets such as defense and business aviation if any single customer base tightens.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, approved_training_organizations, beneficiary,
    powerful, biographical, arbitrage, global).

% Price hull and liability risk for the sector; the documented exercise record of each fleet is a primary input to premium and coverage decisions. Gain when the pooled risk improves. Can reprice annually, restrict terms, or withdraw from poorly evidenced lines of business, which gives them leverage over operator training policy without administering any of it.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, aviation_insurers, beneficiary,
    powerful, generational, mobile, global).

% Receive the safety margin that a maintained crew-competence floor provides, and pay for it indirectly through fares that embed training and compliance costs. Have no individual seat in rulemaking; their interest is mediated entirely by the regulator's statutory mandate and by carrier reputational competition. Can choose carriers or other transport modes but cannot opt out of the system any particular flight operates in.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, flying_public, beneficiary,
    powerless, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, flying_public, payer).

% Fund the largest share of the mandated spend: device hours, instructor staffing, audit programs, and the scheduling overhead of pulling crews off revenue duties. Receive in return a defensible competence floor, continued insurability, portable credentials for their crews, and protection from competitors who might otherwise cut training spend to win on price. Exit would mean surrendering operating certificates, slots, and hub infrastructure.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, major_network_carriers, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, major_network_carriers, beneficiary).

% Carry the same per-crew exercise mandate on the thinnest margins in scheduled service. Fixed compliance components, such as audit programs and training-infrastructure access, scale badly below fleet-size thresholds, so the mandate consumes a larger revenue share than for major carriers. Contract training out, consolidate, or exit the market; continuing independent operation at sub-scale compliance cost is not realistically available.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, regional_and_low_cost_carriers, payer,
    moderate, biographical, constrained, regional).

% Supply the time and performance the regime runs on: recurrent simulator sessions, line checks flown under observation, audit legs carrying observers in jumpseats, and the record-keeping that keeps licenses current. Bear schedule disruption, preparation load, and, in some jurisdictions, uncompensated training time; unions bargain over compensation and over the jeopardy rules attached to checks. Receive license validity, maintained skill, and the professional standing of holding a current type rating. Leaving the cycle means leaving the profession and the identity built around it.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, line_pilots, payer,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, line_pilots, beneficiary).

% Run the internal observation and audit programs, aggregate the resulting data, and translate findings into scenario and curriculum changes. Departmental headcount, budget, and standing inside the company track the size of the exercise apparatus, and the department's organizational self-concept is bound to administering it. Their internal leverage depends on the programs continuing.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, airline_safety_departments, beneficiary,
    moderate, biographical, identity_locked, national).

% Reconstruct accidents and serious incidents, including the training, currency, and audit histories of the crews involved, and publish findings that feed back into rulemaking and program design. Hold no financial stake in the regime's budget or continuation. Their reports are the principal outside check on whether mandated exercise maps onto line performance.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, accident_investigators, observer,
    institutional, civilizational, analytical, global).

% Operate under the same exercise mandates at the worst cost ratios in the sector while having no seat in the working groups where the standards are drafted. Compliance consumes a larger share of revenue than for any larger carrier class. Selling the certificate or leaving charter operations are the only realistic paths; the mandate itself is not negotiable for them.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, small_charter_operators, excluded,
    moderate, immediate, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__hybrid_dependency, approved_training_organizations).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__hybrid_dependency, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a verifiable competence floor under a very large volume of revenue flights: concentrates expensive exercise infrastructure (full-flight simulators, standardized line-oriented scenarios, observation programs) so that no single operator must build its own, prevents competitive erosion of training investment by making the floor universal, and standardizes the evidence of proficiency so that licenses and type ratings carry comparable meaning across carriers and borders.
% TRANSFER_FUNCTION: Moves money and time from airline operating budgets and pilot duty days to training organizations, device providers, oversight staffing, and internal audit programs; moves assurance upward from crews through operators to regulators, insurers, and the traveling public; moves operational data downward from line observations back into scenario design.
% ABSENT_VOICES: Small charter and nonscheduled operators bear the steepest cost ratios and have no seat in the working groups where exercise standards are drafted. Frontline crews in non-union environments have no bargaining channel over uncompensated training time or check-jeopardy rules. Fare-paying passengers, as indirect cost-bearers, appear only through the regulator's statutory representation of their interest.
% DISAPPEARANCE_RATIONALE: Training investment would immediately diverge along carrier economics, with the cheapest operators cutting deepest; insurers would reprice or withdraw from poorly evidenced fleets; license portability would fragment as authorities could no longer trust a common exercise standard; and the competence floor would become heterogeneous within a few command cycles, with the decay invisible until it surfaced as events.
% FOUNDING_PROBLEM: Commercial aviation grew faster than any mechanism for keeping distributed crews proficient: proficiency decayed invisibly between checks, classroom instruction did not exercise line-relevant skills such as crew coordination and threat management, and competitive pressure pushed each carrier's training spend toward the minimum it could defend. A sequence of accidents attributed to skill fade and crew-performance failures drove construction of a mandated, recurring, scenario-based exercise regime with line observation layered on top.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation boards, which sit outside the benefiting parties, continue to attribute events to skill fade, automation surprise, and training gaps in public findings; actuarial loss data independently shows the risk profile the floor suppresses; peer-reviewed human-factors literature documents decay without exercise. Regulators and the training industry also attest that the problem is live, but the investigative and actuarial sources corroborate it from outside the beneficiary set.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__hybrid_dependency, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__hybrid_dependency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__hybrid_dependency, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_requirement__hybrid_dependency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__hybrid_dependency, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__hybrid_dependency_tests).
:- end_tests(competence_exercise_requirement__hybrid_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness of 0.32 prices a real but bounded rent layer: most mandated spend purchases genuine capability in device hours, instructor time, and observation data, but certification standards restrict who may supply the mandated device categories, hour-floor proxies persist after competency-evidence methods arrived, and the oversight apparatus rewards visible activity alongside measured competence. Suppression of 0.55 reflects the legal architecture: no carrier may operate outside the mandated mix, variances are discretionary and slow, and the enforcement machinery of program approval, ramp checks, and certificate action is permanent infrastructure. Per the framework's division of labor, suppression is authored as a raw structural property and is not scaled by power or scope; extractiveness is the quantity the engine scales through directionality and spatial scope. Theater of 0.27 captures partial ritualization, including known check profiles, coached maneuvers, and slide-delivered human-factors modules, set against observation programs that still sample real line operations. Accessibility collapse of 0.48: within any single jurisdiction the mandated mix is fixed, but competency-evidence reform pathways and cross-jurisdiction variance keep alternatives partly reachable. Resistance of 0.35: continuous cost grievance and union bargaining over training compensation, but no serious abolition politics, because post-accident accountability makes opposition to the floor professionally dangerous for anyone in the system. Coordination type is declared as identity_coordination because the regime's primary coordination function is maintaining the meaning of the license boundary: what a current type rating certifies about its holder. The temporal series shares one six-point grid across all tracked metrics. The suppression trajectory models the post-accident ratchet, in which enforcement capacity steps up after salient events and plateaus between them; theater rises with institutionalization and dips late as competency-evidence reforms push back on hour-proxy compliance.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently. From the regulator's chair the arrangement is the core function of the office, a floor it is statutorily obliged to hold. From the major-carrier chair it is a bundled purchase: heavy compliance cost wrapped around insurability and protection from undercutting. From the training organization's chair it approaches a pure benefit, demand arriving by rule behind a certification wall. From the line pilot's chair it splits: the license and the skill are personally owned goods the cycle maintains, while the proof-of-competence rituals land as uncompensated burden. From the investigator's chair the operative question is purely empirical, whether mandated exercise maps to line performance, and it is the one question none of the funding seats systematically asks. The engine computes these divergences from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place regulators, training organizations, insurers, and the flying public near the subsidized pole: the arrangement delivers them mandate legitimacy, captive demand, priced risk, and safety margin respectively, at little direct cost borne through the structure. Carriers are declared victims but sit well short of the full-target pole: they fund the largest share yet recover part of the transfer as insurability, portable crew credentials, and defense against training-cutting competition, so the derivation should land them mid-range rather than at the target extreme. Line pilots are dual-declared and pinned near symmetric by opposing forces: they pay in time and jeopardy and are paid back in license validity and maintained skill. Their identity lock removes the exit mobility that would otherwise dampen effective extraction on that seat, while their organized power, union bargaining over compensation and jeopardy rules, moderates realized extraction relative to an unorganized seat at the same structural position. Spatial scope amplifies the carrier and pilot seats modestly: the regime operates globally, and verifying exercise quality at global scope is difficult enough that paper compliance substitutes for observed competence at the margins.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, invisible proficiency decay across a distributed fleet under competitive cost pressure, remains live, so the arrangement as a whole has not outlived its mandate and no mandatrophy is declared. The classification work happens at the component level. Reading the whole regime as pure extraction would erase the floor-keeping function that investigative findings and loss data corroborate from outside the beneficiary set; reading it as pure coordination would launder the certification-wall rents and the visible-activity bias that the theater trajectory records. The tangled-rope claim holds both in view: a coordination function that requires active enforcement and carries a persistent, measurable extraction layer. The mismatch consumer's inputs align here, founding_problem_status live with disappearance verdict world_rearranges, so no zombie flag is expected. The component-level risk, hour-floor proxies surviving their own evidence base, is carried by the theater_ratio series rather than by a mandatrophy declaration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates the hybrid_dependency reading of the competence_exercise_requirement kernel; how would the computed classification shift if the sibling readings were instantiated at matched structural data?',
    'Generate the sibling stories (simulation_as_adequate_exercise, catastrophe_as_necessary_anchor) with their own beneficiary/victim structures and epsilon referents, then compare computed per-seat types across the family.',
    'Under the simulation-adequate sibling, the real-aircraft-time component loses its necessity warrant and the regime''s cost structure reads increasingly as legacy rent collection, pushing extraction upward and the type toward the snare side. Under the catastrophe-anchor sibling, the entire routine exercise apparatus reads as insufficient performance, pushing theater_ratio past 0.5 and the type toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this constraint is one reading of a contested kernel; sibling readings are separate files.').

omega_variable(
    hybrid_necessity_empirical_basis,
    'Is the dual-component claim (simulation necessary AND real-world anchoring necessary) empirically settled, or does the necessity of real anchoring rest on tradition and post-accident politics?',
    'Longitudinal controlled comparison of hybrid-regime cohorts against simulation-heavy cohorts on line-relevant performance measures and documented skill-fade rates, insulated from carrier cost incentives.',
    'If simulation alone sustains competence, the real-aircraft mandates are dead-weight cost and the authored epsilon understates extraction; if real anchoring is irreplaceable, the current epsilon already prices the coordination correctly and the rent layer is the only excess.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_necessity_empirical_basis, empirical, 'Whether the hybrid composition claim is an established property of skill maintenance or a constructed bargain.').

omega_variable(
    training_rent_layer_magnitude,
    'How much of the mandated training spend exceeds the competitive price of equivalent instruction and device time?',
    'Benchmark certified-device hourly pricing against open-market equivalents in adjacent sectors; analyze the certification barrier''s effect on supplier entry and price.',
    'Sizes the extraction arm that separates the tangled-rope reading from a near-pure coordination reading; a negligible rent layer would support reclassification toward rope at review.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(training_rent_layer_magnitude, empirical, 'Magnitude of the captive-market rent component inside the compliance cost.').

omega_variable(
    pilot_identity_lock_depth,
    'Is line-pilot acquiescence to the recurrent check cycle a revealed preference for maintained licensure, or identity fusion with the checked-out professional self?',
    'Attitude trajectories across license lapse, furlough, and retirement; comparison of burden tolerance between pilots inside the cycle and recently exited pilots.',
    'If fusion is substantial, the pilot seat''s modeled exit mobility overstates real options and effective extraction on that seat runs higher than the structural derivation suggests; if preference-driven, the seat behaves as a conventional constrained payer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pilot_identity_lock_depth, conceptual, 'Depth of the identity-lock binding the pilot seat to the exercise cycle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__hybrid_dependency, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__hybrid_dependency, theater_ratio, 0, 0.18).
narrative_ontology:measurement(comp_tr_t6, competence_exercise_requirement__hybrid_dependency, theater_ratio, 6, 0.21).
narrative_ontology:measurement(comp_tr_t12, competence_exercise_requirement__hybrid_dependency, theater_ratio, 12, 0.24).
narrative_ontology:measurement(comp_tr_t18, competence_exercise_requirement__hybrid_dependency, theater_ratio, 18, 0.27).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_requirement__hybrid_dependency, theater_ratio, 24, 0.29).
narrative_ontology:measurement(comp_tr_t30, competence_exercise_requirement__hybrid_dependency, theater_ratio, 30, 0.27).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(comp_be_t6, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 6, 0.24).
narrative_ontology:measurement(comp_be_t12, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 12, 0.26).
narrative_ontology:measurement(comp_be_t18, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 18, 0.28).
narrative_ontology:measurement(comp_be_t24, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 24, 0.3).
narrative_ontology:measurement(comp_be_t30, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 30, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(comp_su_t6, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 6, 0.46).
narrative_ontology:measurement(comp_su_t12, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(comp_su_t18, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 18, 0.54).
narrative_ontology:measurement(comp_su_t24, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(comp_su_t30, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__hybrid_dependency, identity_coordination).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement__simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement__catastrophe_as_necessary_anchor).

% DUAL FORMULATION NOTE:
% Constraint family: competence_exercise_requirement decomposes into three structurally distinct readings because the colloquial question of what keeps crews competent conflates claims with different epsilon values, beneficiary structures, and failure modes. This file (hybrid_dependency) authors epsilon for the standing mandatory hybrid regime as the hybrid reading assesses it. The simulation_as_adequate_exercise sibling authors epsilon for the pure-simulation arrangement it advocates and would treat the real-aircraft component as legacy cost; the catastrophe_as_necessary_anchor sibling authors epsilon for the event-driven learning arrangement and treats routine exercise as insufficient. The hybrid regime's data-collection machinery feeds the evidence base both siblings argue from, so this file links to both, and each family member links to the others via affects_constraints per family rules.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
