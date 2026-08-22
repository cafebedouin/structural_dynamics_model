% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, []).

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
 *   constraint_id: catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading
 *   human_readable: Simulation Exercises as Catastrophe-Equivalent Practice (Sufficiency Reading)
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   A standing arrangement in high-reliability industries (commercial
 *   aviation, nuclear power, chemical processing, increasingly healthcare)
 *   treats scheduled simulation exercises — full-flight simulator sessions,
 *   control-room emergency drills, tabletop exercises — as sufficient
 *   practice for catastrophic contingencies, such that operators maintain
 *   certification and regulators verify competence without waiting for real
 *   events. This story instantiates ONE reading of the contested kernel
 *   catastrophe_proxy_sufficiency: the
 *   simulation_as_proxy_catastrophe_reading, which holds the equivalence
 *   categorical and indefinite. Under that reading's own lights the
 *   arrangement is a genuine coordination mechanism: it solves the rare-event
 *   rehearsal problem no industry can solve privately, distributes maintained
 *   competence broadly, and gives regulators an auditable standard.
 *   Extraction is residual (compliance overhead, mandated-vendor margins),
 *   suppression is limited to license-backed mandates participants largely
 *   consent to, and no victim class is identified — the reading's bet is that
 *   competence really is maintained. The claim/metric relationship is
 *   authored independently: claimed_type rope reflects this reading's
 *   structural assessment; the metrics describe a healthy-but-not-spotless
 *   arrangement whose theater stratum is slowly thickening. Sibling readings
 *   (catastrophe_necessity_reading, hybrid_degradation_reading,
 *   simulation_fidelity_threshold) are separate constraints over the same
 *   referent arrangement — see network.dual_formulation_note and the kernel
 *   omegas. KEY AGENTS (by structural relationship): - safety_regulators:
 *   Agenda-setter and beneficiary (institutional/constrained) — writes the
 *   equivalence standard, audits drill records, collects liability protection
 *   - licensed_high_reliability_operators: Beneficiary-payer
 *   (institutional/trapped) — funds simulators and drill time, retains
 *   licensure through demonstrated drill performance - line_pilot_workforces:
 *   Primary practice beneficiary (organized/constrained) — emergency
 *   proficiency maintained on simulator cycles -
 *   simulation_training_industry: Commercial beneficiary (organized/mobile) —
 *   receives the mandated training spend - flying_public: Diffuse beneficiary
 *   (powerless/trapped) — flies under simulator-certified crews it cannot
 *   inspect - host_communities_near_hazardous_facilities: Local beneficiary
 *   (moderate/trapped) — protected by drilled off-site response -
 *   small_regional_carriers: Excluded voice (moderate/constrained) — priced
 *   at the edge of fidelity compliance, outside standard-setting rooms -
 *   hro_research_community: Analytical observer (analytical/analytical) —
 *   supplies the transfer and skill-decay evidence both this reading and its
 *   rivals rest on
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.22).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.45).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, "Simulation Exercises as Catastrophe-Equivalent Practice (Sufficiency Reading)").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, '209e8ffb-61d9-4372-b76d-f032a2c007ae').
narrative_ontology:cs_kernel_codification('209e8ffb-61d9-4372-b76d-f032a2c007ae', formalized).
narrative_ontology:cs_authority_grounding('209e8ffb-61d9-4372-b76d-f032a2c007ae', expertise).
narrative_ontology:cs_interpretation_layer_present('209e8ffb-61d9-4372-b76d-f032a2c007ae').
narrative_ontology:cs_reading_relation('209e8ffb-61d9-4372-b76d-f032a2c007ae', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('209e8ffb-61d9-4372-b76d-f032a2c007ae', catastrophe_proxy_sufficiency__hybrid_degradation_reading, forecloses).
narrative_ontology:cs_reading_relation('209e8ffb-61d9-4372-b76d-f032a2c007ae', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('209e8ffb-61d9-4372-b76d-f032a2c007ae', foundational, simulation_rehearsal_transfers_to_catastrophe_performance).
narrative_ontology:cs_axiom_status(simulation_rehearsal_transfers_to_catastrophe_performance, holdable).
narrative_ontology:cs_axiom_grounding('209e8ffb-61d9-4372-b76d-f032a2c007ae', simulation_rehearsal_transfers_to_catastrophe_performance, empirically_contingent).
narrative_ontology:cs_axiom('209e8ffb-61d9-4372-b76d-f032a2c007ae', foundational, scenario_encodability_of_operational_competence).
narrative_ontology:cs_axiom_status(scenario_encodability_of_operational_competence, holdable).
narrative_ontology:cs_axiom_grounding('209e8ffb-61d9-4372-b76d-f032a2c007ae', scenario_encodability_of_operational_competence, empirically_contingent).
narrative_ontology:cs_reference_frame('209e8ffb-61d9-4372-b76d-f032a2c007ae', categorical_simulation_equivalence).
narrative_ontology:cs_drift_state('209e8ffb-61d9-4372-b76d-f032a2c007ae', contemporary_post_automation_complacency_findings, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('209e8ffb-61d9-4372-b76d-f032a2c007ae', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, safety_regulators).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, licensed_high_reliability_operators).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, line_pilot_workforces).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_training_industry).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, flying_public).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, host_communities_near_hazardous_facilities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, licensed_high_reliability_operators).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_transfer_sufficiency_hypothesis).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, hro_collective_mindfulness_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and revises the recurrent-training rules that define how much simulator credit counts toward qualification, audits operators' drill records, and suspends certificates when currency lapses. After any incident, the drill record doubles as the agency's demonstration of diligence. The agency cannot walk away from its oversight mandate, and its standards propagate internationally through harmonization bodies.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, safety_regulators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, safety_regulators, beneficiary).

% Buys or leases full-motion simulators, schedules recurring drill cycles across shifts, and presents scored drill performance at certificate renewal. The spend is large and recurring — simulator acquisition, instructor time, operational downtime — but the alternative to a current drill record is losing the operating certificate. An airline or nuclear station certificate is tied to its jurisdiction; relocation or exit is not a live option.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, licensed_high_reliability_operators, beneficiary,
    institutional, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, licensed_high_reliability_operators, payer).

% Returns to the simulator on fixed cycles to rehearse engine failures, windshear, rejected takeoffs, and coordinated emergency procedures that line operations almost never present. Currency earned at one employer carries across the industry, so changing employers does not mean leaving the regime. Unions bargain over session density and fatigue but not over the principle of simulator-based currency.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, line_pilot_workforces, beneficiary,
    organized, biographical, constrained, global).

% Designs and manufactures full-flight simulators and control-room replicas, licenses scenario libraries, and supplies instructor services. Its order book tracks the equivalence standard's reach: every expansion of simulator credit converts directly into equipment and curriculum sales. Customers are concentrated and regulated, so the industry's fortunes rise and fall with standard-setting it attends but does not control.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_training_industry, beneficiary,
    organized, biographical, mobile, global).

% Boards aircraft operated by crews whose emergency currency is documented in simulator records rather than in survived emergencies. Passengers cannot inspect training files, compare operators' drill quality, or price the difference; their protection is mediated entirely by the regulator's acceptance of the drill record.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, flying_public, beneficiary,
    powerless, immediate, trapped, global).

% Lives downwind or downstream of plants whose operating staff rehearse severe-accident sequences in replica control rooms and whose off-site responders join joint evacuation and release drills. Community members serve as volunteer casualties and evacuees in some exercises. Moving the hazard away is not possible; moving away from it is costly and partial.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, host_communities_near_hazardous_facilities, beneficiary,
    moderate, generational, trapped, local).

% Operates smaller fleets and older aircraft types for which full-fidelity simulator access is thinnest and per-seat compliance cost highest. Standard-setting rooms are populated by major carriers, manufacturers, and regulators; the small operators' proposals — phased fidelity tiers, shared regional simulator pools, grandfathered waivers — rarely reach the agenda. Their practical choice is absorbing disproportionate compliance costs or shrinking.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, small_regional_carriers, excluded,
    moderate, biographical, constrained, regional).

% Measures whether rehearsed performance predicts real performance: transfer-effectiveness studies, skill-decay curves, post-incident comparisons of simulator-trained versus event-experienced crews. Publishes findings that both this reading's adequacy claims and its rivals' challenges draw upon. Holds no enforcement power and collects no compliance revenue.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, hro_research_community, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_training_industry).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the rare-event rehearsal problem at industry scale: catastrophic contingencies are too infrequent and too dangerous to practice live, so standardized simulated rehearsals let thousands of crews and control-room teams keep emergency skills current simultaneously, while giving regulators one auditable competence standard in place of unverifiable individual experience.
% TRANSFER_FUNCTION: Moves recurring compliance spending from licensed operators to simulator manufacturers, scenario licensors, and instructor services; moves crew duty time into rehearsal; and moves assurance upward — drill records flow to regulators, and regulator acceptance flows outward as public trust and insurance terms.
% ABSENT_VOICES: Small regional carriers sit outside the standard-setting rooms where equivalence and fidelity requirements are fixed; veteran operators whose emergency learning came from real events hold testimony about what rehearsal cannot reproduce that the equivalence premise discounts by construction; front-line crew who experience some mandated drills as record-keeping have no channel that distinguishes their compliance from their assent. Each would alter the standard's content if seated.
% DISAPPEARANCE_RATIONALE: Overnight removal of the equivalence rule would strip certification regimes of their only scalable competence instrument: regulators could no longer verify currency, insurers would reprice or withdraw coverage pending some replacement assurance, simulator demand would collapse, and operators would face an impossible choice between unschedulable real-event exposure and uncertifiable competence. Licensing, training economies, and insurance markets would all reorganize around whatever partial substitute emerged.
% FOUNDING_PROBLEM: Mid-century high-reliability industries faced a paradox: emergency competence decays without practice, but the events that practice most needs — fires, failures, blowdowns, wind-shear encounters — are too rare and too dangerous to arrange. Early simulator adoption (airline flight simulators from the 1950s-60s, nuclear control-room simulators accelerated after Three Mile Island) was built to supply rehearsal without catastrophe.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: accident-investigation reports (NTSB, BEA, IAEA follow-ups) repeatedly attribute mishandled emergencies to skill decay in crews without recent practice; laboratory and field skill-decay research documents procedural forgetting curves independent of any regulator's interest; and pre-simulator-era loss records reviewed by independent historians show the decay problem the arrangement was built for. No corroborating source sits inside the regulator-operator-vendor beneficiary set.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_tests).
:- end_tests(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.22: under this reading the arrangement transfers little uncompensated value — mandated simulator spend buys a real service, and the residual is compliance overhead plus vendor margin on a captive demand curve. The series rises gently (0.12 to 0.22) as compliance bureaucracies thicken, not as rents deepen. Suppression 0.45: the mandate is real (certificate suspension for lapsed drill currency) but sits alongside genuine alternatives (academic routing, on-the-job credit, grandfathering) and broad consent; authored as a raw structural property, unscaled by power or scope. Theater_ratio 0.36 and rising: the functional core (line-oriented flight training, emergency-procedure cycles, scored scenario runs) remains demanding, but a record-satisfying stratum — annual tabletop rituals, box-checked drill matrices — thickens as programs institutionalize. Accessibility_collapse 0.45: understanding the equivalence rule does not close alternatives — real-event exposure remains theoretically open but practically unschedulable, and academic routes persist — so collapse is moderate, rope-typical. Resistance 0.20: historical dissent (zero-flight-time training controversies, union session-load bargaining, small-carrier cost objections) is real but marginal. All three tracked series share one six-point grid (1975-2026); the suppression series' step shape tracks disaster-driven mandate hardening (post-Three-Mile-Island, post-9/11 training and security reviews, post-Fukushima drill expansions) — external shocks, not intermittent reinforcement, so no cyclical signature is claimed. Coordination type identity_coordination is declared because the arrangement's dominant function is boundary maintenance of the qualified-operator status (who may hold the certificate), not resource allocation or information standard-setting per se.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator seat the arrangement is diligent governance with a defensible audit trail; from the operator seat it is a large but license-preserving cost; from the pilot seat it is career-sustaining rehearsal; from the passenger seat it is invisible assurance; from the small-carrier seat it is a cost wall sized to someone else's fleet. Same structure, five differently experienced arrangements. The engine computes these per-seat classifications from the power/exit/role data; nothing in the authored claim adjudicates them, and divergence between the authored rope claim and any computed seat type is measurement, not error.
 *
 * DIRECTIONALITY LOGIC:
 *   Every seated actor declares a beneficiary relationship, so the derivation chain places all seats near the subsidized end: regulators (d near 0 — collect liability protection and audit authority), operators (dual-positioned beneficiary/payer, deriving mid-low — they fund the regime but receive license retention and avoided-loss protection in return), workforces and publics (d near 0 — competence maintained at no direct charge to them). No agent derives near the full-target end, so effective extraction stays low across seats and the rope profile survives per-seat computation. The excluded small-carrier seat is commentary-grade (R3): it feeds the consensus-provenance check, never a classification override — but the uniform_standard_cost_incidence omega probes whether a real victim set hides behind the no-victim claim. Receipt surface: the mandated spend the arrangement generates lands demonstrably on simulation_training_industry (equipment, curriculum, instructor revenue), so gain_flow names that seat rather than asserting diffuseness; fixing_cost is prohibitive because the agenda-setter has no substitute competence instrument at comparable cost, and under this reading removal would purchase no benefit worth that price.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — emergency competence decays without rehearsal, and catastrophes cannot be scheduled — is corroborated as live from outside the beneficiary set (accident-investigation findings, skill-decay literature), so no mandatrophy resolution is declared. The rope classification guards against two mislabels: reading mandated training as a vendor-regulator extraction scheme (under this reading the coordination function is real and broadly beneficial) and reading it as inertial theater (the function has not atrophied; theater_ratio 0.36 sits atop a functionally demanding core). The threat vector this reading must watch is Goodhart drift, visible in the theater series: if record-satisfying drills displace stress-valid ones, the arrangement slides toward theatrical maintenance while the indefinite-sufficiency claim continues to shield it — the measurement series exists to catch that slide early. If the founding problem ever died (competence shown self-maintaining through routine operations), the arrangement would reclassify toward piton; nothing in the current record supports that.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    one_reading_of_catastrophe_proxy_kernel,
    'This constraint instantiates only the simulation_as_proxy_catastrophe_reading of kernel catastrophe_proxy_sufficiency; what would the standing arrangement''s classification become under each sibling reading?',
    'Generate the three sibling stories (catastrophe_necessity_reading, hybrid_degradation_reading, simulation_fidelity_threshold) and compare computed types over the same referent arrangement.',
    'Under catastrophe_necessity_reading the same arrangement computes as facade — training theater masking lost competence, with future exposed publics as victims; under hybrid_degradation_reading as a hybrid carrying real procedural coordination plus deferred degradation cost; under simulation_fidelity_threshold as a contingent coordination mechanism whose character tracks fidelity investment. The disagreement is located in the sufficiency quantifier and the scope of ''competence''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(one_reading_of_catastrophe_proxy_kernel, conceptual, 'Committer-frame registration: per-reading classifications over one shared referent arrangement.').

omega_variable(
    reading_scope_of_competence,
    'Does ''operational competence'' in this reading''s sufficiency claim span only procedural and task competences, or also tacit judgment and stress-response capacity? The categorical reading is stable only under the narrower scoping; under broad scoping it collides head-on with the hybrid_degradation_reading.',
    'Delimit the competence taxonomy the certification regime actually tests (scenario-scored items) against what incident analyses show degrading; map coverage and gaps.',
    'Narrow scoping keeps this reading coherent and the arrangement rope-classified; broad scoping forces merger with hybrid_degradation_reading, importing deferred victims and raising effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_scope_of_competence, conceptual, 'Scope ambiguity in the sufficiency quantifier is where this reading meets its strongest sibling.').

omega_variable(
    indefinite_horizon_unfalsifiability,
    'The claim asserts sufficiency ''indefinitely'', but no finite observation window can confirm indefinite sufficiency; does generational turnover of scenario authors and instructors silently erode scenario validity?',
    'Multi-cohort longitudinal studies linking scenario-library age and freshness to real-event performance deltas; audits of scenario libraries for fossilized assumptions.',
    'If erosion is real, the arrangement carries deferred victims (future operators and publics) and drifts from rope toward a hybrid with deferred extraction; the ''indefinitely'' quantifier is the load-bearing wall of this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indefinite_horizon_unfalsifiability, empirical, 'Indefinite-horizon claim outruns any available evidence window.').

omega_variable(
    liability_armor_vs_verification,
    'Is the drill record functioning primarily as verification of competence, or as legal armor demonstrating diligence after the fact?',
    'Compare regulatory and litigation outcomes post-incident between operators with equivalent drill records but divergent real performance; test whether drill-record weight in enforcement scales with litigation exposure rather than measured competence.',
    'Armor-dominance raises theater_ratio above authored levels and pushes the arrangement toward theatrical maintenance; verification-dominance supports the rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_armor_vs_verification, empirical, 'Dual-use ambiguity of drill documentation.').

omega_variable(
    uniform_standard_cost_incidence,
    'Does the uniform equivalence standard distribute compliance costs regressively enough to constitute a hidden victim set — small operators priced below the fidelity threshold the standard presumes?',
    'Cost-per-revenue-hour analysis of simulator compliance across carrier size classes; attrition analysis of small operators following standard-tightening episodes.',
    'A systematic casualty class among small operators would add victims, creating reclassification pressure away from the no-victim rope profile and validating the excluded-seat testimony.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uniform_standard_cost_incidence, empirical, 'Hidden victim-set probe behind the no-victim claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 1975, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t1975, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(cata_tr_t1985, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 1985, 0.17).
narrative_ontology:measurement(cata_tr_t1995, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 1995, 0.23).
narrative_ontology:measurement(cata_tr_t2005, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(cata_tr_t2015, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 2015, 0.33).
narrative_ontology:measurement(cata_tr_t2026, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 2026, 0.36).

% Extraction over time
narrative_ontology:measurement(cata_be_t1975, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 1975, 0.12).
narrative_ontology:measurement(cata_be_t1985, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 1985, 0.14).
narrative_ontology:measurement(cata_be_t1995, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 1995, 0.16).
narrative_ontology:measurement(cata_be_t2005, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 2005, 0.18).
narrative_ontology:measurement(cata_be_t2015, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 2015, 0.2).
narrative_ontology:measurement(cata_be_t2026, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 2026, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t1975, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 1975, 0.08).
narrative_ontology:measurement(cata_su_t1985, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 1985, 0.26).
narrative_ontology:measurement(cata_su_t1995, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement(cata_su_t2005, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 2005, 0.42).
narrative_ontology:measurement(cata_su_t2015, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 2015, 0.44).
narrative_ontology:measurement(cata_su_t2026, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 2026, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% 'Simulation keeps us competent' is a colloquial label covering four structurally distinct claims about the kernel catastrophe_proxy_sufficiency. This file instantiates the categorical-sufficiency reading (simulation IS catastrophe-equivalent, indefinitely) and authors epsilon for the standing arrangement — mandated simulation-based competence maintenance — by that reading's own lights. The siblings author their own epsilon over the same referent: catastrophe_necessity_reading (only real events confer the irreducible stress and uncertainty; simulation is cover), hybrid_degradation_reading (procedural competence maintained, tacit and stress-response capacity degrade generationally), simulation_fidelity_threshold (sufficiency is technology-contingent, not categorical). Per the epsilon-invariance principle these are separate constraints linked as a family, not one constraint with a measurement dial. Upstream/downstream: the necessity reading functions as the skeptical baseline the others must answer, and the threshold reading mediates between the categorical and hybrid positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
