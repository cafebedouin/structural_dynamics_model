% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__simulation_as_sufficient
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__simulation_as_sufficient, []).

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
 *   constraint_id: competence_retention_exercise__simulation_as_sufficient
 *   human_readable: Simulation-Sufficiency Standard for Catastrophe-Avoidance Competence
 *   domain: institutional/epistemic — safety engineering and organizational learning
 *
 * SUMMARY:
 *   Across aviation, nuclear operations, surgery, and emergency response,
 *   high-fidelity simulation became the primary mechanism for maintaining
 *   catastrophe-avoidance competence, and the equivalence premise — that the
 *   cognitive and procedural demands of simulation are structurally
 *   equivalent to real events — was written into the certification standards
 *   that allocate training mandates, define passing criteria, and justify
 *   recurrent-evaluation regimes. This story instantiates ONE reading of the
 *   competence_retention_exercise kernel: simulation_as_sufficient, which
 *   holds the equivalence premise and treats the resulting regime as genuine
 *   competence maintenance with ordinary coordination costs. Per Rule 1 the
 *   contest is NOT described inside the constraint; the sibling readings
 *   (catastrophe_as_necessary, near_miss_as_bridge) are separate constraint
 *   files linked via network.affects_constraints, each carrying its own
 *   epsilon and victim set. The claim/metric split is deliberate:
 *   claimed_type is tangled_rope as an independent structural judgment, while
 *   the metrics describe the regime's actual operation — genuine,
 *   irreplaceable rehearsal coordination accreting mandate rents, metric
 *   displacement, and budgetary crowding-out of field-learning channels.
 *   Divergence between the authored claim and any per-seat computation is the
 *   measurement, not an error.
 *
 * KEY AGENTS:
 *   - - certifying_bodies: Agenda setter (institutional / identity_locked) — writes and administers the standards that make simulator performance the certified measure of readiness; cannot step outside the statutory role its identity is fused with
 *   - - simulator_vendors_and_training_industry: Primary beneficiary (powerful / arbitrage) — sells simulators, scenario content, and instruction into regulation-created demand from a concentrated global supplier base
 *   - - hro_training_departments: Beneficiary and day-to-day administrator (organized / constrained) — runs academies, produces the audit trail, gains budget standing and legal defensibility while absorbing compliance costs
 *   - - simulator_instructors_check_airmen: Beneficiary-administrator (organized / mobile) — delivers and scores sessions; livelihood scales with mandated volume
 *   - - line_operators: Primary target (organized / constrained) — recertification and promotion ride on simulator scores; field experience enters only through debrief instruments
 *   - - near_miss_investigation_programs: Secondary target (moderate / identity_locked) — the field-learning channel competing for budget against simulation procurement
 *   - - small_regional_operators: Secondary target (moderate / constrained) — buys scarce simulator hours at distant centers with least influence over standards
 *   - - flying_public_and_host_communities: Diffuse beneficiary with residual-risk exposure (powerless / trapped) — receives the safety margin, bears tail risk, holds no standards seat
 *   - - veteran_practitioners_doubting_equivalence: Excluded voice (moderate / identity_locked) — attests startle/consequence gaps from experience with few formal channels
 *   - - academic_hro_researchers: Analytical observer (analytical / analytical) — external check via transfer-of-training and mindfulness literatures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, 0.45).
domain_priors:suppression_score(competence_retention_exercise__simulation_as_sufficient, 0.4).
domain_priors:theater_ratio(competence_retention_exercise__simulation_as_sufficient, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, extractiveness, 0.45).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__simulation_as_sufficient, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__simulation_as_sufficient, "Simulation-Sufficiency Standard for Catastrophe-Avoidance Competence").
narrative_ontology:topic_domain(competence_retention_exercise__simulation_as_sufficient, "institutional/epistemic — safety engineering and organizational learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__simulation_as_sufficient).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__simulation_as_sufficient, '4135d70b-f2ef-4c6d-b578-faf5845cda9d').
narrative_ontology:cs_kernel_codification('4135d70b-f2ef-4c6d-b578-faf5845cda9d', formalized).
narrative_ontology:cs_authority_grounding('4135d70b-f2ef-4c6d-b578-faf5845cda9d', expertise).
narrative_ontology:cs_interpretation_layer_present('4135d70b-f2ef-4c6d-b578-faf5845cda9d').
narrative_ontology:cs_reading_relation('4135d70b-f2ef-4c6d-b578-faf5845cda9d', competence_retention_exercise__catastrophe_as_necessary, forecloses).
narrative_ontology:cs_reading_relation('4135d70b-f2ef-4c6d-b578-faf5845cda9d', competence_retention_exercise__near_miss_as_bridge, forecloses).
narrative_ontology:cs_axiom('4135d70b-f2ef-4c6d-b578-faf5845cda9d', foundational, simulation_fidelity_confers_genuine_competence).
narrative_ontology:cs_axiom_status(simulation_fidelity_confers_genuine_competence, holdable).
narrative_ontology:cs_axiom_grounding('4135d70b-f2ef-4c6d-b578-faf5845cda9d', simulation_fidelity_confers_genuine_competence, empirically_contingent).
narrative_ontology:cs_axiom('4135d70b-f2ef-4c6d-b578-faf5845cda9d', secondary, simulator_metrics_track_real_event_readiness).
narrative_ontology:cs_axiom_status(simulator_metrics_track_real_event_readiness, holdable).
narrative_ontology:cs_axiom_grounding('4135d70b-f2ef-4c6d-b578-faf5845cda9d', simulator_metrics_track_real_event_readiness, empirically_contingent).
narrative_ontology:cs_reference_frame('4135d70b-f2ef-4c6d-b578-faf5845cda9d', simulation_fidelity_equivalence_standard).
narrative_ontology:cs_drift_state('4135d70b-f2ef-4c6d-b578-faf5845cda9d', post_startle_surprise_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4135d70b-f2ef-4c6d-b578-faf5845cda9d', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, simulator_vendors_and_training_industry).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, certifying_bodies).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, hro_training_departments).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, flying_public_and_host_communities).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, line_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, near_miss_investigation_programs).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, small_regional_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, simulator_instructors_check_airmen).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, flying_public_and_host_communities).
narrative_ontology:constraint_vindicates(competence_retention_exercise__simulation_as_sufficient, simulation_transfer_equivalence_hypothesis).
narrative_ontology:constraint_vindicates(competence_retention_exercise__simulation_as_sufficient, quantified_competence_measurement_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish and administer the training regulations and accreditation standards that define simulator hours, scenario banks, and passing scores as the route to certified readiness. Staff the working groups that decide what counts as equivalent fidelity. Funded by fees and appropriations tied to running the certification system; the agency's self-concept is bound to the standards apparatus it administers, and it cannot step outside the statutory role that role defines.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, certifying_bodies, agenda_setter,
    institutional, generational, identity_locked, national).

% Design and sell full-fidelity simulators, scenario software, and instructor services to every organization the standards bind. Order books follow mandate expansions rather than customer preference, and the supplier base is a small set of global firms diversified across civil and military contracts. Revenue depends on continued official acceptance of simulator performance as the measure of readiness.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, simulator_vendors_and_training_industry, beneficiary,
    powerful, generational, arbitrage, global).

% Run the internal academies and simulation centers, schedule recurrent cycles, and produce the audit records that demonstrate compliance. Documented simulator hours buy budget standing and legal defensibility after adverse events; the same departments also absorb the capital, scheduling, and instructor costs the standards impose.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, hro_training_departments, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, hro_training_departments, agenda_setter).

% Deliver scenario sessions and score crew performance against the published metrics. Salaries and professional standing scale with mandated training volume; they simultaneously face pressure to produce passing results and crews who contest scores, and they can move between competing training centers.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, simulator_instructors_check_airmen, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, simulator_instructors_check_airmen, agenda_setter).

% Fly the line, staff the control rooms, and operate the theaters. Recertification and, increasingly, promotion ride on recurrent simulator scores, and career consequences attach to those scores. Their experiential knowledge of how real events differ from scripted ones enters the process mainly through structured debrief forms completed after sessions.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, line_operators, payer,
    organized, biographical, constrained, global).

% Collect, code, and analyze incident reports and minor failures from live operations — historically the channel through which organizations noticed drift between trained conditions and actual ones. Compete for budgets against simulation procurement; several long-standing reporting programs have been folded into simulator-feedback loops or defunded following adoption of the sufficiency standard, while the programs' staff remain committed to the field-learning mission that gave the programs their purpose.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, near_miss_investigation_programs, payer,
    moderate, generational, identity_locked, continental).

% Purchase simulator access by the hour at distant training centers, often sharing scarce devices with larger carriers' overflow. Fixed mandated training costs weigh heaviest on thin margins, and these operators hold few seats on the committees that design scenarios or set passing standards.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, small_regional_operators, payer,
    moderate, biographical, constrained, regional).

% Receive whatever safety margin the competence regime produces and bear the residual risk of any gap between rehearsed and actual performance. Individuals cannot opt out of shared airspace or plant siting, hold no seat in standards-setting, and typically learn of competence gaps only through accident reports.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, flying_public_and_host_communities, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, flying_public_and_host_communities, payer).

% Long-service captains, shift supervisors, and senior clinicians who describe from experience the difference between the simulation room and the event — startle, consequence, irreversibility — and find few formal channels to press that testimony into standards revision. Their critiques circulate in trade press, retirement speeches, and informal mentoring.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, veteran_practitioners_doubting_equivalence, excluded,
    moderate, biographical, identity_locked, national).

% Study how organizations detect and correct drift, comparing simulation-led regimes with incident-led ones across industries. Neither fund nor staff the training system; the transfer-of-training and collective-mindfulness literatures constitute the main external check on the sufficiency claim.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, academic_hro_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a repeatable, standardized, physically safe way to rehearse catastrophic failure modes that cannot be rehearsed live, and a shared measurable benchmark for declaring crews, teams, and license-holders ready.
% TRANSFER_FUNCTION: Moves training dollars from regulated organizations and public budgets to simulator manufacturers and training providers under mandate-backed demand; moves evaluation authority from accumulated field experience to simulator scoring; converts operational risk narratives into auditable training records.
% ABSENT_VOICES: Veteran practitioners who describe the difference between the simulation room and the event, and near-miss specialists whose programs lost standing, sit outside the standards committees where fidelity equivalence is defined. Line crews enter chiefly through structured debrief instruments rather than agenda-setting roles.
% DISAPPEARANCE_RATIONALE: If the sufficiency standard vanished overnight, certification systems would lose their primary instrument, license pipelines would stall pending replacement criteria, training markets would reprice around uncertain demand, and organizations would reopen field and incident-learning channels that the standard had displaced — the competence-retention economy reorganizes around whatever signal source the successor standard names.
% FOUNDING_PROBLEM: Catastrophic failure modes are too rare and too lethal for on-the-job mastery: early aviation, reactor, and surgical practice consumed crews, patients, and plants during competence acquisition, so a safe rehearsal substitute for real events had to be found.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation boards, insurers, and academic safety science — none of whom collect from the training market — attest that the rehearsal problem is real and ongoing. The sibling positions likewise concede simulation's necessity; the live dispute across the kernel is whether simulation is sufficient, not whether the founding problem exists.
narrative_ontology:disappearance_verdict(competence_retention_exercise__simulation_as_sufficient, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__simulation_as_sufficient, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__simulation_as_sufficient, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_retention_exercise__simulation_as_sufficient, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__simulation_as_sufficient, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__simulation_as_sufficient_tests).
:- end_tests(competence_retention_exercise__simulation_as_sufficient_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.45 is a reading-indexed value over the standing simulation-centered arrangement: even by this reading's own lights the regime concedes mandate-inelastic demand feeding a concentrated vendor oligopoly, regressive hourly pricing that weighs hardest on small operators, and budgetary displacement of near-miss channels — while holding the core exchange fair, since the rehearsal function has no substitute at any price. Suppression 0.40 is a raw structural property (unscaled by power or scope in the engine): alternatives are neither banned nor free — near-miss programs were defunded and folded into simulator feedback loops, veteran dissent was channeled out of committee rooms, but incident-reporting traditions survive in places. Theater 0.32: most session activity is functionally real, with a growing scored-procedure shell as automated grading rewards script execution over judgment. Accessibility_collapse 0.52 and resistance 0.45 reflect partial alternative-collapse and sustained academic, union, and practitioner pushback rather than either natural-law closure or open contest. Enforcement capacity genuinely expanded over the interval (mandates broadened from initial qualification to perpetual recurrent cycles across new industries), so suppression_requirement is tracked temporally with a rising-then-plateauing series; extraction and theater climbed alongside it. All series share one seven-point grid so no metric is sampled against another's end state. Receipt surface: the gains demonstrably land at the vendor seat (mandate-backed demand, concentrated supply, price-inelastic buyers), so gain_flow names simulator_vendors_and_training_industry rather than 'diffuse'; fixing_cost is prohibitive because removal means unwinding certification frameworks, sunk simulator fleets, and international harmonization agreements against diffuse, hard-to-attribute benefits. Trajectories are monotone ratchets, not cycles — no intermittent-reinforcement dynamic is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the certifying and vendor positions the arrangement is professionalized competence infrastructure they built, administer, and profit from legitimately; from the line-operator and small-operator positions the same structure operates as score-governed evaluation with career consequences attached and pricing that ignores ability to pay; from the near-miss program's position it is the displacement of a whole epistemic channel; from the flying public's position it is an invisible bargain trading rehearsed assurance for unmodeled tail risk. The engine computes this divergence from the structural data — power atoms, exit options, and role declarations — and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulator vendors sit nearest the beneficiary pole: pure collectors with arbitrage-grade diversification and no compliance burden of their own. Certifying bodies collect authority, fees, and jurisdiction rather than training revenue — low d but nonzero through budget dependence on the system they certify. Training departments mix collection (defensibility, budget standing) with payment (capital, scheduling), placing them modestly up from the pole. Flying publics derive subsidy (the safety margin) with a tail-risk uptick carried by their secondary payer position. Line operators and small operators sit near the full-target end: they bear the transfers, their exit is constrained by licensure and market position, and their voice is proceduralized. Near-miss investigation programs bear the constraint's suppressive edge — their decline is the counterfactual the equivalence claim enforces — giving them high effective extraction despite modest formal power. Veterans, as an excluded voice, hold no directional seat but document where the conversation's boundaries were drawn.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — safe rehearsal of the unrehearsable — is live and externally corroborated, so no obsolescence flag is available and none is claimed. The tangled_rope classification is what prevents mislabeling in both directions: a snare reading would erase the irreplaceable coordination function (no alternative rehearses catastrophic scenarios safely at any cost, which even the sibling readings concede), while a rope reading would erase the mandate rents, the regressive pricing structure, and the budgetary strangulation of field-learning channels that the same standard produces. The temporal series show extraction, theater, and enforcement intensity rising together over the interval — the signature of coordination accreting extraction, not of a mandate outliving its problem. Mandatrophy language stays reserved for the piton case: nothing here is maintained theatrically after functional death.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the simulation_as_sufficient reading of the competence_retention_exercise kernel; what structural changes would the sibling readings (catastrophe_as_necessary, near_miss_as_bridge) introduce?',
    'Author the sibling stories as separate files and compare victim sets, epsilon, and enforcement surfaces across the three; the disagreement is located precisely in the equivalence premise — whether any extra-simulational signal is required for genuine competence.',
    'Under catastrophe_as_necessary the target set shifts outward — populations exposed to preventable disasters bear the cost of learning-by-disaster — and extraction externalizes beyond the training market entirely; under near_miss_as_bridge the contested surface moves to incident-reporting labor and the evidential weight of degraded real-world signals. Victim-set and epsilon differences across the family are the classification data.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one of three mutually exclusive readings; foreclosure edges authored in cs_structure.').

omega_variable(
    transfer_of_training_partiality,
    'How much of simulator-exercised competence survives contact with genuine startle, surprise, and irreversible consequence?',
    'Longitudinal linkage of simulator scores to subsequent line-event outcomes; controlled startle-inoculation trials; natural experiments where simulator currency lapsed fleet-wide (e.g., pandemic-era access restrictions) with outcome comparison against matched periods.',
    'Materially partial transfer undermines the foundational axiom on which mandates are justified; the mandate layer would then read as rent-bearing rather than readiness-bearing, and classification pressure shifts toward the extractive pole. Full transfer would vindicate the reading''s coordination account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_of_training_partiality, empirical, 'Whether cognitive and procedural fidelity transfers completely to real events.').

omega_variable(
    goodhart_metric_displacement,
    'Do simulator metrics measure generalized catastrophe-avoidance competence, or scenario-specific test-taking skill?',
    'Perturbation studies inserting novel, unrehearsed failure modes into scored scenarios and comparing performance against rehearsed-script variants; analysis of instructor score distributions before and after automated grading adoption.',
    'If metrics reward preparation-for-the-known-scenario, the measurement layer functions partly as credentialing ritual, the theater_ratio is understated, and the doctrine of quantified competence measurement is vindicated only nominally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(goodhart_metric_displacement, empirical, 'Metric validity under Goodhart pressure from high career stakes attached to scores.').

omega_variable(
    displacement_vs_merit_of_field_channels,
    'Were near-miss and field-learning channels reduced on merit, or crowded out budgetarily once the sufficiency standard was adopted?',
    'Budget and headcount histories of incident-reporting programs immediately before and after simulation-mandate adoption; matched-organization comparisons where both channels were retained.',
    'Merit-based reduction supports the reading''s coordination account and lowers the suppression attributable to the claim; fiscal crowding-out raises it and implicates the standard itself as the suppressing force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_vs_merit_of_field_channels, empirical, 'Cause of complementary-channel decline: epistemic inferiority or budgetary displacement.').

omega_variable(
    framing_claim_vs_infrastructure,
    'Is the constraint the institutionalized equivalence claim, or the simulation infrastructure regime that claim licenses?',
    'Counterfactual construction: strip the equivalence claim from the standards while leaving hardware and hours in place — if mandates, pricing power, and channel displacement persist, the claim is the operative constraint; the framing choice was guided by the fact that the claim, not the equipment, adjudicates legitimacy and allocates budgets.',
    'An infrastructural framing lowers epsilon toward coordination cost and pulls classification toward rope; the claim-framing keeps mandate rents and channel displacement visible. Classification differs across framings, so the choice is recorded rather than assumed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_claim_vs_infrastructure, conceptual, 'CS-framing under-determination: claim-layer kernel versus hardware-layer kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__simulation_as_sufficient, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t6, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 6, 0.18).
narrative_ontology:measurement_basis(comp_tr_t6, observed).
narrative_ontology:measurement(comp_tr_t12, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 12, 0.21).
narrative_ontology:measurement_basis(comp_tr_t12, observed).
narrative_ontology:measurement(comp_tr_t18, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 18, 0.24).
narrative_ontology:measurement_basis(comp_tr_t18, observed).
narrative_ontology:measurement(comp_tr_t24, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 24, 0.27).
narrative_ontology:measurement_basis(comp_tr_t24, observed).
narrative_ontology:measurement(comp_tr_t30, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(comp_tr_t30, observed).
narrative_ontology:measurement(comp_tr_t36, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 36, 0.32).
narrative_ontology:measurement_basis(comp_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 0, 0.26).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t6, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 6, 0.31).
narrative_ontology:measurement_basis(comp_be_t6, observed).
narrative_ontology:measurement(comp_be_t12, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 12, 0.35).
narrative_ontology:measurement_basis(comp_be_t12, observed).
narrative_ontology:measurement(comp_be_t18, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 18, 0.39).
narrative_ontology:measurement_basis(comp_be_t18, observed).
narrative_ontology:measurement(comp_be_t24, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 24, 0.42).
narrative_ontology:measurement_basis(comp_be_t24, observed).
narrative_ontology:measurement(comp_be_t30, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 30, 0.44).
narrative_ontology:measurement_basis(comp_be_t30, observed).
narrative_ontology:measurement(comp_be_t36, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 36, 0.45).
narrative_ontology:measurement_basis(comp_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t6, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 6, 0.29).
narrative_ontology:measurement_basis(comp_su_t6, observed).
narrative_ontology:measurement(comp_su_t12, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 12, 0.33).
narrative_ontology:measurement_basis(comp_su_t12, observed).
narrative_ontology:measurement(comp_su_t18, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 18, 0.37).
narrative_ontology:measurement_basis(comp_su_t18, observed).
narrative_ontology:measurement(comp_su_t24, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 24, 0.39).
narrative_ontology:measurement_basis(comp_su_t24, observed).
narrative_ontology:measurement(comp_su_t30, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 30, 0.4).
narrative_ontology:measurement_basis(comp_su_t30, observed).
narrative_ontology:measurement(comp_su_t36, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 36, 0.4).
narrative_ontology:measurement_basis(comp_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__simulation_as_sufficient, identity_coordination).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__catastrophe_as_necessary).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'simulation-based competence retention' conflates three structurally distinct arrangements, each with its own epsilon and victim set. Historical influence runs catastrophe_as_necessary (the pre-simulation baseline, where learning-by-disaster was the accepted tuition) -> near_miss_as_bridge (which accepts simulation as primary but demands real-world validation input) -> simulation_as_sufficient (this file, which denies any extra-simulational requirement). Each successor reading was argued as a correction of its predecessor's insufficiency, so upstream stories are cited as evidence by downstream advocacy. This file links both siblings via affects_constraints; the sibling files reciprocate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
