% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__hybrid_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Hybrid Competence Exercise Requirement (Simulation Foundation Plus Periodic Real-World Anchoring)
 *   domain: safety_engineering/organizational_learning/high_reliability_operations
 *
 * SUMMARY:
 *   The standing arrangement under contest is the mandated hybrid exercise
 *   regimen for commercial flight crews: a regulator-set schedule combining
 *   recurrent full-flight simulator events with real-world anchoring — line
 *   checks flown in revenue service, non-jeopardy line audits, and minimum
 *   actual-aircraft recency — enforced through certificates and program
 *   approvals. Around the schedule sits a training economy: simulator
 *   manufacturers and training-center operators whose order books track the
 *   mandated hours, operators who fund the programs, pilots who live on the
 *   schedule, and insurers who price against its outputs. This story is one
 *   member of a decomposed constraint family (see
 *   network.dual_formulation_note) and authors the hybrid arrangement as the
 *   hybrid_dependency reading assesses it. Claim and metrics are authored
 *   independently: the claimed type records the structural read; the metric
 *   series record observed operation on a shared six-point grid spanning
 *   roughly 1990-2020 (t = years since 1990).
 *
 * KEY AGENTS:
 *   - aviation_regulators: agenda setter (institutional/constrained) — sets and enforces the exercise schedule, approves programs, answers for outcomes
 *   - major_network_airlines: primary payer with secondary beneficiary position (institutional/arbitrage) — funds programs, gains system-level reliability
 *   - small_regional_operators: disproportionate cost bearer (moderate/constrained) — fixed training costs concentrate on thin margins
 *   - line_pilots: recurring cost bearer and protected party (organized/constrained) — lives on the schedule, union-represented
 *   - ab_initio_pay_to_fly_pilots: front-loaded cost bearer (powerless/trapped) — buys qualification before employment
 *   - simulator_training_industry: mandated-demand collector (institutional/arbitrage) — converts the schedule into contracted hours
 *   - flying_public: diffuse safety beneficiary (powerless/constrained) — receives the output, pays through fares
 *   - aviation_insurers: risk-price beneficiary (institutional/arbitrage) — prices against the regime's statistics
 *   - flight_safety_researchers: analytical observer — measures skill decay and program effects, collects no program revenue
 *   - developing_market_operators: excluded voice — must meet the baseline but rarely shapes it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, 0.32).
domain_priors:suppression_score(competence_exercise_requirement__hybrid_dependency, 0.42).
domain_priors:theater_ratio(competence_exercise_requirement__hybrid_dependency, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, extractiveness, 0.32).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, resistance, 0.33).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__hybrid_dependency, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__hybrid_dependency, "Hybrid Competence Exercise Requirement (Simulation Foundation Plus Periodic Real-World Anchoring)").
narrative_ontology:topic_domain(competence_exercise_requirement__hybrid_dependency, "safety_engineering/organizational_learning/high_reliability_operations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__hybrid_dependency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__hybrid_dependency, '5fcccf53-3355-4f9c-a36f-2644dd6fb3cf').
narrative_ontology:cs_kernel_codification('5fcccf53-3355-4f9c-a36f-2644dd6fb3cf', formalized).
narrative_ontology:cs_authority_grounding('5fcccf53-3355-4f9c-a36f-2644dd6fb3cf', expertise).
narrative_ontology:cs_interpretation_layer_present('5fcccf53-3355-4f9c-a36f-2644dd6fb3cf').
narrative_ontology:cs_reading_relation('5fcccf53-3355-4f9c-a36f-2644dd6fb3cf', competence_exercise_requirement__simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_reading_relation('5fcccf53-3355-4f9c-a36f-2644dd6fb3cf', competence_exercise_requirement__catastrophe_as_necessary_anchor, forecloses).
narrative_ontology:cs_axiom('5fcccf53-3355-4f9c-a36f-2644dd6fb3cf', foundational, real_world_anchoring_necessary_for_competence).
narrative_ontology:cs_axiom_status(real_world_anchoring_necessary_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('5fcccf53-3355-4f9c-a36f-2644dd6fb3cf', real_world_anchoring_necessary_for_competence, empirically_contingent).
narrative_ontology:cs_axiom('5fcccf53-3355-4f9c-a36f-2644dd6fb3cf', foundational, benign_real_exposure_suffices_as_anchor).
narrative_ontology:cs_axiom_status(benign_real_exposure_suffices_as_anchor, holdable).
narrative_ontology:cs_axiom_grounding('5fcccf53-3355-4f9c-a36f-2644dd6fb3cf', benign_real_exposure_suffices_as_anchor, empirically_contingent).
narrative_ontology:cs_reference_frame('5fcccf53-3355-4f9c-a36f-2644dd6fb3cf', hybrid_exercise_baseline).
narrative_ontology:cs_drift_state('5fcccf53-3355-4f9c-a36f-2644dd6fb3cf', evidence_based_training_transition, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('5fcccf53-3355-4f9c-a36f-2644dd6fb3cf', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, flying_public).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, major_network_airlines).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, simulator_training_industry).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, aviation_regulators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, aviation_insurers).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, small_regional_operators).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, ab_initio_pay_to_fly_pilots).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, line_pilots).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, major_network_airlines).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, line_pilots).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__hybrid_dependency, skill_decay_without_periodic_exercise).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__hybrid_dependency, partial_simulation_transfer_limits).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the minimum recurrent exercise schedule for commercial crews: simulator events per year, line checks, audit programs, and minimum real-aircraft recency. Approve operator training programs, credential instructors and check airmen, and can suspend crews or revoke certificates for non-compliance. Fund oversight from budget appropriations and cannot delegate away the safety outcome they answer for.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, aviation_regulators, agenda_setter,
    institutional, generational, constrained, global).

% Operate large fleets whose crews cycle through simulator centers and line audits on the regulator's schedule. Bear direct program costs — simulator block hours, instructor payroll, training downtime — and recover them through fares and network reliability. Can restructure programs in-house, shift registrations across jurisdictions, and lobby rulemakings through industry bodies; cannot opt out of the regime while holding an operating certificate.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, major_network_airlines, payer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, major_network_airlines, beneficiary).

% Run thinner margins and smaller fleets, so fixed training costs — simulator leases, travel to training centers, crew downtime — fall harder per revenue hour. Buy simulator time in blocks from the same large providers everyone else uses. Leaving the regime means leaving scheduled commercial aviation.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, small_regional_operators, payer,
    moderate, biographical, constrained, regional).

% Recurrent simulator events, proficiency checks, and line audits punctuate every career year. Union-negotiated provisions govern pay during training days; failing a check suspends flying status until retrained. Seniority systems and type-specific qualifications make switching employers or aircraft types slow and expensive, so most ride out the schedule within one career.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, line_pilots, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, line_pilots, beneficiary).

% Enter the profession by purchasing type ratings and line experience under pay-to-fly or bonded-training schemes before first airline employment. The qualification ladder is long and front-loaded with personal debt; walking away forfeits the investment, and the credentials have little resale value outside aviation.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, ab_initio_pay_to_fly_pilots, payer,
    powerless, immediate, trapped, global).

% Manufactures full-flight simulators and operates training centers selling block hours to operators worldwide. Every crew-recurrency schedule converts directly into contracted simulator hours and instructor days. Revenue is diversified across regions and adjacent industries, and capacity expands where demand concentrates.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, simulator_training_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Buys tickets priced to include the training system's costs and receives its safety output. Has no seat in training-rule proceedings and no practical ability to verify any individual crew's exercise history; the realistic alternative to flying particular routes is not flying.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, flying_public, beneficiary,
    powerless, immediate, constrained, global).

% Price hull and liability coverage against fleet-level accident statistics that the training regime shapes. Require evidence of compliant training programs as a condition of coverage and adjust premiums when programs lapse. Re-underwrite globally and can withdraw from markets.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, aviation_insurers, beneficiary,
    institutional, generational, arbitrage, global).

% Study line operations through observation programs and incident databases, publish error and skill-decay findings, and advise regulators and operators on program design. Hold no enforcement power and collect no program revenue; their standing rests on methods and data access.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, flight_safety_researchers, observer,
    analytical, civilizational, analytical, global).

% Must meet internationally harmonized exercise standards with thinner domestic training infrastructure, importing simulator hours and examiner availability at premium cost. Standard-setting consultations are dominated by larger foreign operators and authorities; their capacity constraints rarely shape the baseline they must meet.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, developing_market_operators, excluded,
    moderate, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__hybrid_dependency, simulator_training_industry).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__hybrid_dependency, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains and verifies crew competence at fleet scale despite skill decay that is invisible from the outside: schedules a shared exercise baseline so a proficiency claim means the same thing at every operator, spreads the cost of rare-event rehearsal that no single airline would optimally fund alone, and gives insurers and the public a common assurance signal.
% TRANSFER_FUNCTION: Moves money and crew time from operators and pilots (ultimately fares) to simulator providers, training organizations, and oversight budgets; moves verified-competence assurance back to operators, insurers, and the traveling public.
% ABSENT_VOICES: Fare-paying passengers have no seat in training-rule dockets; small and developing-market operators participate through associations weighted toward larger members; non-unionized pilots lack bargaining power over how training costs are allocated. Unanimity around the current schedule partly reflects who was in the room when the hours were set.
% DISAPPEARANCE_RATIONALE: If the hybrid exercise schedule vanished overnight, operators would not simply save the money: insurers would impose private training covenants within renewal cycles, lessors and lenders would attach conditions, and competence verification would reorganize around contractual and insurance substitutes with less standardization and weaker comparability across operators.
% FOUNDING_PROBLEM: Jet-era accident clusters showed that classroom instruction plus ordinary line flying could neither safely rehearse rare emergencies nor detect slow skill decay, while early simulators could not reproduce line conditions. The regime was built to rehearse the unrehearsable without risking aircraft or passengers while keeping skills anchored to real operations.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation reports (NTSB, BEA, AAIB) and published line-observation error studies — sources outside the operating, training, and regulatory beneficiary set — continue to document skill-decay and transfer-fidelity effects; no party outside the arrangement disputes that the underlying decay-and-rehearsal problem persists, though parties disagree over the required mix of simulator and line exposure.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__hybrid_dependency, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__hybrid_dependency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__hybrid_dependency, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
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
 *   Extractiveness sits moderate (0.32 at interval end) rather than low because the mandated schedule converts directly into contracted revenue — simulator block hours, examiner days, program fees — and part of that conversion prices above marginal service cost, while fixed-cost burdens land unevenly on small operators and qualification costs land front-loaded on entrant pilots. It stops well short of high because the dominant share of program cost purchases real rehearsal capacity and the safety output is broadly delivered. Suppression (0.42) is structural: the schedule binds through certificate consequences and program approvals, and unilateral departure is unavailable to any certified operator; it is moderated by approved variation paths (data-driven program tailoring) that keep the regime revisable. Theater (0.24) concentrates in high-stakes check events, where teaching-to-the-check is documented, while non-jeopardy line audits remain comparatively low-theater information sources. The three temporal series share one six-point grid, as required: the suppression series traces an enforcement build-up through the interval's middle decades followed by modest relaxation as approval pathways matured — the story tracks enforcement-capacity change, so suppression_requirement is authored rather than left static. Suppression is authored as a raw structural property; only extractiveness is scaled downstream by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same schedule. From the regulator's chair the regimen is legitimate standard-setting it answers for; from the major operators' chair it is a manageable program cost embedded in network economics; from the small operator's and entrant pilot's chairs the same fixed costs arrive concentrated and unavoidable; from the training industry's chair the schedule is demand; from the researcher's chair it is a natural experiment in skill maintenance. Line pilots add an identity dimension: seniority systems and type-specific qualifications fuse career trajectory to the schedule, so even cost-bearing seats defend the arrangement that bills them. The engine computes these divergences from the structural declarations; nothing in the claimed type adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The flying public and insurers are declared beneficiaries: safety output and priced risk flow to them at near-zero marginal burden. The training industry is a beneficiary in the strict sense — it collects from the schedule without administering it — and is the seat where the arrangement's gains demonstrably accrue. Major operators and line pilots are declared payers carrying secondary beneficiary positions (system-level accident avoidance, standardized employability). Small operators and pay-to-fly entrants are victims: they bear the schedule's costs steeply without commensurate offsetting collection. Regulators administer and enforce while collecting authority and answering for outcomes — a mixed position the beneficiary declaration only partly captures, which is why their effective position is expected to compute nearer symmetric than the raw beneficiary listing suggests.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rehearsing the unrehearsable and detecting invisible decay — remains live and is corroborated from outside the beneficiary set, so no obsolescence flag is warranted: status live paired with verdict world_rearranges leaves the mismatch consumer quiet. The classification work runs in both directions: the genuine coordination core must not be misread as pure extraction merely because a mandated market surrounds it, and the mandated market's rents must not launder the whole arrangement into pure coordination. Naming both the coordinated population and the paying seats inside one structure is exactly what the tangled-rope declaration forces; the temporal series then lets drift detection watch which component grows.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the hybrid_dependency reading of the competence_exercise_requirement kernel; what would the sibling readings change structurally?',
    'Cross-reading comparison of the sibling constraint files: simulation_as_adequate_exercise removes the real-aircraft anchoring mandates (shrinking the real-exercise cost layer and the victim set); catastrophe_as_necessary_anchor escalates exercise demands toward continuous line exposure and collides with the ethical and practical limits of staging catastrophic experience.',
    'Under the sim-adequate sibling, epsilon falls toward the coordination floor and the arrangement trends toward pure coordination; under the catastrophe sibling, epsilon rises and the impossibility of manufactured catastrophe pushes toward enforced scarcity. This file''s classification holds only for the hybrid reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one reading of a three-reading kernel.').

omega_variable(
    real_anchoring_dose_response,
    'How much periodic real-aircraft exposure does competence anchoring actually require, and does the currently mandated dose match it?',
    'Longitudinal line-observation and skill-decay studies comparing cohorts across approved program variants with different real-aircraft doses; natural experiments from jurisdictions approving reduced-recency programs.',
    'If a smaller dose suffices, the real-aircraft component carries excess mandated cost and epsilon falls; if the current dose is already minimal, the anchoring layer is efficiently sized and residual extraction is confined to the training market.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_anchoring_dose_response, empirical, 'Dose-response uncertainty in the real-world anchoring component.').

omega_variable(
    mandated_market_capture_share,
    'What fraction of training-economy revenue reflects market power over captive mandated demand rather than the cost of delivering simulator and instructional capacity?',
    'Benchmarking simulator hour rates against ownership, depreciation, and operating costs; entry and margin analysis in the training-center market; disclosure of provider contract terms.',
    'A high capture share deepens the extraction layer and pushes the computed type toward the snare boundary; a low share collapses the residual to ordinary procurement cost and pulls toward pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandated_market_capture_share, empirical, 'Degree of rent capture inside the mandated training market.').

omega_variable(
    fixed_cost_incidence_framing,
    'Is the disproportionate burden on small operators and entrant pilots a defect of the exercise schedule itself, or of the surrounding market and labor structure?',
    'Comparative analysis of jurisdictions that pool or subsidize training capacity versus those that leave cost incidence to the market, holding the exercise schedule constant.',
    'If incidence follows market structure, the schedule''s own epsilon is lower than measured and remedies belong elsewhere; if schedule design drives incidence, program-design remedies are correctly aimed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fixed_cost_incidence_framing, conceptual, 'Attribution of burden concentration between the regimen and its environment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__hybrid_dependency, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cer_hybrid_tr_t0, competence_exercise_requirement__hybrid_dependency, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(cer_hybrid_tr_t0, observed).
narrative_ontology:measurement(cer_hybrid_tr_t6, competence_exercise_requirement__hybrid_dependency, theater_ratio, 6, 0.15).
narrative_ontology:measurement_basis(cer_hybrid_tr_t6, observed).
narrative_ontology:measurement(cer_hybrid_tr_t12, competence_exercise_requirement__hybrid_dependency, theater_ratio, 12, 0.17).
narrative_ontology:measurement_basis(cer_hybrid_tr_t12, observed).
narrative_ontology:measurement(cer_hybrid_tr_t18, competence_exercise_requirement__hybrid_dependency, theater_ratio, 18, 0.2).
narrative_ontology:measurement_basis(cer_hybrid_tr_t18, observed).
narrative_ontology:measurement(cer_hybrid_tr_t24, competence_exercise_requirement__hybrid_dependency, theater_ratio, 24, 0.22).
narrative_ontology:measurement_basis(cer_hybrid_tr_t24, observed).
narrative_ontology:measurement(cer_hybrid_tr_t30, competence_exercise_requirement__hybrid_dependency, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(cer_hybrid_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(cer_hybrid_be_t0, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(cer_hybrid_be_t0, observed).
narrative_ontology:measurement(cer_hybrid_be_t6, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 6, 0.23).
narrative_ontology:measurement_basis(cer_hybrid_be_t6, observed).
narrative_ontology:measurement(cer_hybrid_be_t12, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 12, 0.26).
narrative_ontology:measurement_basis(cer_hybrid_be_t12, observed).
narrative_ontology:measurement(cer_hybrid_be_t18, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 18, 0.29).
narrative_ontology:measurement_basis(cer_hybrid_be_t18, observed).
narrative_ontology:measurement(cer_hybrid_be_t24, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 24, 0.31).
narrative_ontology:measurement_basis(cer_hybrid_be_t24, observed).
narrative_ontology:measurement(cer_hybrid_be_t30, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 30, 0.32).
narrative_ontology:measurement_basis(cer_hybrid_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(cer_hybrid_su_t0, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(cer_hybrid_su_t0, observed).
narrative_ontology:measurement(cer_hybrid_su_t6, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 6, 0.35).
narrative_ontology:measurement_basis(cer_hybrid_su_t6, observed).
narrative_ontology:measurement(cer_hybrid_su_t12, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 12, 0.4).
narrative_ontology:measurement_basis(cer_hybrid_su_t12, observed).
narrative_ontology:measurement(cer_hybrid_su_t18, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 18, 0.44).
narrative_ontology:measurement_basis(cer_hybrid_su_t18, observed).
narrative_ontology:measurement(cer_hybrid_su_t24, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 24, 0.43).
narrative_ontology:measurement_basis(cer_hybrid_su_t24, observed).
narrative_ontology:measurement(cer_hybrid_su_t30, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(cer_hybrid_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__hybrid_dependency, identity_coordination).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement__simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement__catastrophe_as_necessary_anchor).

% DUAL FORMULATION NOTE:
% The colloquial label 'how must crew competence be exercised to persist' covers three structurally distinct claims with different epsilon, different beneficiary structures, and different failure modes: simulation_as_adequate_exercise (epsilon low; coordination framing; disputes center on transfer fidelity), hybrid_dependency (this file; epsilon moderate; a coordination core plus a mandated-market extraction layer), and catastrophe_as_necessary_anchor (epsilon high under its own lights; demands ethically fraught exercise). The upstream empirical record (skill-decay and transfer studies) is cited by all three readings; family links run through network.affects_constraints. Each file authors one stable epsilon for its own reading per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
