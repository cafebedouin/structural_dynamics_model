% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__hybrid_near_miss_learning
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__hybrid_near_miss_learning, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: catastrophe_avoidance_retention__hybrid_near_miss_learning
 *   human_readable: Distributed Near-Miss Learning for Competence Retention in High-Reliability Systems
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   High-reliability industries (aviation, nuclear power, maritime) maintain
 *   competence against catastrophic failure modes through distributed
 *   learning networks that collect, normalize, and disseminate near-miss
 *   incidents across organizational boundaries. The constraint is the
 *   standing arrangement under which individual organizations participate in
 *   incident-sharing networks, implement shared training protocols, and grant
 *   immunity to reporters in exchange for regulatory compliance and access to
 *   the collective database. This reading—the 'hybrid near-miss learning'
 *   reading—asserts that competence retention depends on neither pure
 *   catastrophe (which is too rare and too costly) nor pure simulation (which
 *   cannot calibrate to unknown scenarios without external grounding), but on
 *   a hybrid arrangement where near-misses and foreign incidents provide the
 *   calibration signal and high-realism drills embed the distributed lessons.
 *   The measurement series shows extractiveness rising from 0.38 to 0.65
 *   mid-interval as regulatory mandates extend network participation, then
 *   declining slightly as the arrangement normalizes and resistance
 *   stabilizes. Theater rises initially as compliance bureaucracy
 *   accumulates, then stabilizes as the coordination function reasserts
 *   itself. Suppression is moderate throughout—the arrangement is held by
 *   regulatory mandate and by the genuine benefit of access to incident data,
 *   not by coercive force alone.
 *
 * KEY AGENTS:
 *   - System Safety Organizations (agenda-setters): convene and maintain the incident-reporting networks; set standards for reportable taxonomy; manage immunity protections.
 *   - Individual Organizations (payers): bear the cost of participation—staff time, operational transparency, implementation of recommendations, competitive vulnerability from shared data.
 *   - Front-Line Operators (beneficiary/payer, identity-locked): gain access to distributed incident scenarios through simulator training; also internalize blame for incidents they did not cause.
 *   - Regulatory Authorities (agenda-setter/beneficiary): enforce mandates; grant immunity; benefit from reduced catastrophic incidents.
 *   - Competing Proprietary Vendors (payers): lose market share when operators train on shared scenarios instead of proprietary customizations.
 *   - Catastrophe Survivors (excluded): would demand transparency and liability; structurally incompatible with immunity protections.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.62).
domain_priors:suppression_score(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.41).
domain_priors:theater_ratio(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, extractiveness, 0.62).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__hybrid_near_miss_learning, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__hybrid_near_miss_learning, "Distributed Near-Miss Learning for Competence Retention in High-Reliability Systems").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__hybrid_near_miss_learning, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__hybrid_near_miss_learning).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__hybrid_near_miss_learning, '3c749f8f-c1af-449f-b91d-155a0212bede').
narrative_ontology:cs_kernel_codification('3c749f8f-c1af-449f-b91d-155a0212bede', distributed).
narrative_ontology:cs_authority_grounding('3c749f8f-c1af-449f-b91d-155a0212bede', practice).
narrative_ontology:cs_interpretation_layer_present('3c749f8f-c1af-449f-b91d-155a0212bede').
narrative_ontology:cs_reading_relation('3c749f8f-c1af-449f-b91d-155a0212bede', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, coexists_with).
narrative_ontology:cs_reading_relation('3c749f8f-c1af-449f-b91d-155a0212bede', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, coexists_with).
narrative_ontology:cs_axiom('3c749f8f-c1af-449f-b91d-155a0212bede', foundational, near_miss_sufficiency_for_competence).
narrative_ontology:cs_axiom_status(near_miss_sufficiency_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('3c749f8f-c1af-449f-b91d-155a0212bede', near_miss_sufficiency_for_competence, empirically_contingent).
narrative_ontology:cs_axiom('3c749f8f-c1af-449f-b91d-155a0212bede', secondary, distributed_learning_mechanism_viable).
narrative_ontology:cs_axiom_status(distributed_learning_mechanism_viable, holdable).
narrative_ontology:cs_axiom_grounding('3c749f8f-c1af-449f-b91d-155a0212bede', distributed_learning_mechanism_viable, empirically_contingent).
narrative_ontology:cs_reference_frame('3c749f8f-c1af-449f-b91d-155a0212bede', hybrid_distributed_learning_sufficiency).
narrative_ontology:cs_drift_state('3c749f8f-c1af-449f-b91d-155a0212bede', contemporary_incident_network_maturity, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3c749f8f-c1af-449f-b91d-155a0212bede', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, system_safety_organizations).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, incident_reporting_networks).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, individual_organizations_bearing_reporting_cost).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, competing_proprietary_data_frameworks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, front_line_operators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, regulatory_authorities).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, front_line_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convene incident-reporting networks (aviation, nuclear, maritime) that collect, normalize, and disseminate near-miss data across organizational boundaries. They set the standards for what constitutes reportable incident taxonomy, who has access to what-level detail, and what immunity protections govern reporters. They benefit from maintaining the network because their legitimacy and funding rest on the network's existence and perceived effectiveness.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, system_safety_organizations, agenda_setter,
    organized, generational, mobile, global).

% Individual airlines, hospitals, nuclear stations bear the cost of participating: staff time to investigate incidents, document findings in standardized formats, share sensitive operational data that competes with proprietary training data and operational secrets, and implement recommendations that may require procedural or capital changes. Exit costs are high because regulatory compliance often mandates participation; reputation costs of non-participation are severe.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, individual_organizations_bearing_reporting_cost, payer,
    moderate, biographical, constrained, regional).

% Pilots, nurses, reactor operators, ship captains gain access to near-miss incident data from across their industry, which becomes embedded in high-realism drills and simulator training. This distributed learning allows them to encounter scenarios and corrective approaches they would never encounter in their individual organization alone. They also bear the cost of the extended training time and the identity-fusion risk of treating incident scenarios as personally redemptive (internalizing blame for incidents they did not cause).
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, front_line_operators, beneficiary,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, front_line_operators, payer).

% Commercial training vendors, proprietary safety consulting firms, and internal organizational knowledge systems compete with the shared incident network. They lose market share and pricing power when operators train on openly-shared, standardized scenarios instead of proprietary customized simulations. Their path to exit is to argue that proprietary frameworks better protect confidentiality or deliver faster updates.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, competing_proprietary_data_frameworks, payer,
    powerful, biographical, constrained, global).

% Enforce reporting mandates and grant the immunity protections that enable organizations to share incident data without fear of liability. They benefit from robust incident networks because competence maintenance through learning reduces catastrophic incidents, which reduces their enforcement burden and public scrutiny. They share authority with the safety organizations over what gets reported and how.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, regulatory_authorities, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, regulatory_authorities, beneficiary).

% Have experienced the failure the constraint is designed to prevent. They are structurally excluded from incident-network governance because their testimony and demands for accountability cut against the immunity protections that enable open reporting. Where they gain voice (through litigation, legislation, or public pressure), they typically argue for more aggressive liability rather than more generous immunity.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_survivors_and_bereaved, excluded,
    powerless, biographical, trapped, global).

% Analyze incident-network data to understand error propagation, recovery mechanisms, and organizational resilience patterns. They have access to aggregated and de-identified data, which constrains the specificity and timeliness of their observations. They can publish findings but cannot attribute incidents to specific organizations without breaking confidentiality.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, academic_safety_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__hybrid_near_miss_learning, system_safety_organizations).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__hybrid_near_miss_learning, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the competence-maintenance problem for high-reliability systems where any single organization's experience is insufficient to encounter and correct rare but catastrophic failure modes. By pooling near-miss incidents across organizations and drilling on their lessons, the industry maintains competence against scenarios no individual operator will encounter in their working lifetime.
% TRANSFER_FUNCTION: Moves operational secrecy and proprietary competitive advantage from individual organizations and vendors to shared incident networks and regulatory authorities. Organizations transfer detailed incident reports and corrective-action data; they receive access to a curated, standardized scenario library and the reputational/regulatory benefit of participation.
% ABSENT_VOICES: Catastrophe survivors and bereaved are structurally excluded because immunity protections cannot coexist with their demands for transparent accountability. They would argue for mandatory disclosure, individual liability, and prioritizing investigation and punishment over confidential learning. That objection is irreconcilable with the reporting-network frame.
% DISAPPEARANCE_RATIONALE: If distributed near-miss learning networks dissolved, organizations would retreat to internal simulation and catastrophe-driven correction. Competence maintenance would depend either on actual incidents (catastrophe as necessary selector) or on faith that internal drills matched real-world scenarios without external calibration. High-reliability industries would see competence atrophy and incident rates would rise. Regulatory pressure would mount, and—eventually—a major catastrophe would catalyze reconstruction of something similar to the existing networks.
% FOUNDING_PROBLEM: Catastrophic failure in high-reliability systems (aviation, nuclear, maritime) requires competence maintained at the level of 'one mistake in a million hours.' No single organization accumulates enough operational experience to encounter and survive the full spectrum of failure modes. Individual organizations cannot invest in training for scenarios they have never experienced. Internal incident data is biased toward the organization's own vulnerabilities.
% FOUNDING_PROBLEM_CORROBORATION: Aviation industry attests the founding problem persists (accidents from scenarios aviators trained on via shared networks are rare; accidents from unshared scenarios are not rare). Nuclear industry, maritime, and healthcare researchers independently corroborate the competence-maintenance problem. Regulatory agencies document that incident-reporting networks correlate with lower catastrophic-incident rates. The shared framing is attested to from outside the immediate beneficiary set (independent researchers, competing safety vendors, regulatory authorities not operating the networks themselves).
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__hybrid_near_miss_learning, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__hybrid_near_miss_learning, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__hybrid_near_miss_learning, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is Tangled Rope: it coordinates genuine safety learning (the coordination function) AND extracts organizational autonomy, proprietary data, and competitive advantage (the asymmetric extraction). Extractiveness rises mid-interval because regulatory mandates extend the network's reach and tighten participation standards, forcing organizations that previously had looser compliance to invest more heavily in standardized reporting and training infrastructure. Theater is lower than suppression because the coordination function is real—incident networks demonstrably reduce catastrophic incidents—so the arrangement is not held purely by performative maintenance. Suppression remains moderate because regulatory mandate provides the enforcement backbone; the benefiting organizations (safety networks, regulators) have institutional capacity to maintain it; and the payers have constrained exit (regulatory compliance costs of non-participation are high, but operational benefits of participation are real). The measurement grid is a single shared time axis; every metric is authored at every time point from the same observational stance.
 *
 * PERSPECTIVAL GAP:
 *   From the System Safety Organizations' and Regulators' seat, the arrangement is genuine coordination solving an irreducible problem: competence maintenance in high-reliability systems. From the individual participating organization's seat (especially smaller organizations with less internal incident history), the arrangement is also a transfer mechanism—they give up proprietary incident data and competitive advantage in exchange for access to others' data, which may asymmetrically benefit larger organizations that have more incidents to share. From the front-line operator's seat, the arrangement is primarily a beneficiary position—they gain access to scenario training they could never access internally—but it also carries identity-fusion risk: they may internalize blame for incidents their organization encountered, treating the incident as a personal corrective opportunity rather than an organizational systems problem. From the Competing Proprietary Vendor's seat, the arrangement is pure extraction: it channels training investment away from their commercial products toward shared networks. The engine computes these per-seat divergences from the structural data (power, exit, beneficiary/victim declarations); the Tangled Rope claim asserts that the constraint genuinely coordinates while genuinely extracting, and that the asymmetry is not a defect but the structural price of the coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   System Safety Organizations sit at d ≈ 0.15–0.25 (they are institutional agenda-setters, mobile, beneficiaries; directionality points toward the beneficiary end). Individual organizations sit at d ≈ 0.55–0.65 (they are moderate power, constrained exit, victims; they are coordinated AND pay a transfer; the asymmetry is real). Front-line operators sit at d ≈ 0.40–0.50 (they are powerless, identity-locked, beneficiaries with an indirect cost; the identity-lock is critical because exit for them means leaving the profession, not just leaving one employer). Regulatory Authorities sit at d ≈ 0.35–0.45 (they are institutional, mobile, beneficiary/agenda-setter; they set the rules but also collect legitimacy from the network's success). Competing Vendors sit at d ≈ 0.75–0.85 (they are powerful, trapped (the regulatory regime forecloses their market), victims; they are extracted from without being coordinated). The directionality logic is: beneficiaries (safety orgs, regulators, operators) have d pointing toward lower extraction; payers (individual orgs, vendors) have d pointing toward higher extraction; the distribution of d values is asymmetric, which is the structural hallmark of Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and attestable: competence maintenance in high-reliability systems IS a persistent, irreducible problem. The coordination function is real and measurable: incident networks demonstrably correlate with lower catastrophic incident rates, independent of confounding variables. The extraction is also real: organizations pay measurable costs (staff time, operational transparency, proprietary data transfer, implementation burden) to participate. The Tangled Rope classification prevents misreading this as pure coordination (which would ignore the extraction and asymmetry) or as pure snare (which would miss the genuine coordination function and the real safety benefits to operators and the public). The constraint is not mandatrophic—the mandate (competence maintenance) is still live, still necessary, and still served by this arrangement. The measurement series shows extractiveness stable after mid-interval, not rising indefinitely, which suggests the arrangement has found a plateau where the coordination benefit and the extraction cost are both sustained without acceleration. If extractiveness were rising sharply while theater remained low, or if founding_problem_status had become 'dead,' that would signal mandatrophy. This reading does not present that picture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    near_miss_vs_catastrophe_equivalence,
    'Do near-miss incidents provide functionally equivalent learning to actual catastrophic events for competence maintenance? Can competence be maintained through incident-network learning alone, without occasional actual catastrophes to calibrate the learning?',
    'Long-term empirical study: industries with strong incident networks that have avoided catastrophes for decades (aviation) vs. industries with weaker networks that have experienced periodic catastrophes despite incident reporting (healthcare, industrial). Multivariate analysis controlling for technology maturity, regulatory regime, and organizational scale.',
    'If near-misses are truly equivalent, this reading is validated and the constraint can be classified as pure coordination-plus-extraction (Tangled Rope). If occasional catastrophes are necessary for competence recalibration, the reading is incomplete—the constraint depends on tacitly accepting catastrophe risk as calibration cost, which reframes the founding problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(near_miss_vs_catastrophe_equivalence, empirical, 'Whether near-miss learning can sustain competence without actual catastrophic events.').

omega_variable(
    immunity_protection_collapse,
    'If immunity protections are eroded through litigation, legislation, or accountability pressure, does incident reporting remain robust? Or does participation collapse as organizations retreat to liability avoidance?',
    'Jurisdictional variation: regions that maintain strong immunity (aviation in most countries) vs. regions where immunity has been weakened (medical error reporting in some U.S. states where discovery rules apply). Comparative incident-reporting rates, quality of reported data, and rate of organizational participation.',
    'If immunity is necessary for reporting robustness, the constraint is held by a legal protection that can be withdrawn. The arrangement is more fragile than it appears. If reporting remains robust despite eroded immunity, the coordination benefit may be sufficient to sustain participation even without legal shelter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immunity_protection_collapse, empirical, 'Whether immunity protections are structural necessity or merely convenience.').

omega_variable(
    competing_reading_foreclosure,
    'Does this hybrid reading logically foreclose the ''catastrophe_as_necessary_selector'' reading, or do both coexist as live positions?',
    'Textual analysis of the original catastrophe-selector thesis and interviews with its advocates; determination of whether they claim necessity or sufficiency.',
    'If foreclosed: this reading (hybrid) and that reading (catastrophe-selector) cannot both be true in any single framework; only one reading can be the correct account of competence maintenance. If coexists: both are live, held by different organizations/philosophies, and the kernel contest is genuinely unresolved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competing_reading_foreclosure, conceptual, 'Whether this reading forecloses the necessity-of-catastrophe reading or merely competes with it.').

omega_variable(
    identity_lock_suppression_mechanism,
    'Is the suppression measured in front-line operators'' participation (they report to and train on incidents they did not personally cause) structural (they lack alternative career paths) or internalized (they have fused their professional identity with the blame/correction cycle)?',
    'Post-exit trajectory study: when operators transition to training roles, leave the industry, or move to organizations with different incident-reporting cultures, does the suppression persist? If internalized, the suppression travels with them; if structural, it dissolves with the exit from the regulatory regime.',
    'If internalized: the identity-lock is a form of distributed psychological coercion embedded in professional identity, which increases the effective suppression beyond what the structural measures show. If structural: the suppression is institutional/regulatory, and exit-seeking operators can escape it. The distinction affects whether the Tangled Rope classification captures the full extraction dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_suppression_mechanism, empirical, 'Whether operator suppression is structural or internalized.').

omega_variable(
    reading_vs_catastrophe_selector_coexistence,
    'This reading claims hybrid near-miss learning is sufficient; the catastrophe-as-necessary-selector reading claims only catastrophe is sufficient. Can aviation be proof of this reading''s validity if aviation has avoided major catastrophes? Or does the absence of catastrophes prove nothing—it merely shows nothing has tested the hypothesis.',
    'Counterfactual reasoning: if aviation had experienced a major catastrophe despite incident-network learning, would that falsify this reading or merely show that incident networks are necessary-but-not-sufficient? The reading''s scope is ''competence is maintained via [hybrid learning]'' — success requires showing that competence metrics (error detection, crew resource management, simulator performance) are sustained over decades without catastrophic events that test the full system.',
    'If the absence of catastrophes is proof of sufficiency: this reading''s empirical claim can be validated and it forecloses the necessity interpretation of the catastrophe-selector reading. If the absence proves nothing: both readings remain live because catastrophe-as-selector is compatible with ''we haven''t had a big one yet, so we don''t know if learning alone would sustain competence if we did.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_catastrophe_selector_coexistence, conceptual, 'Whether the absence of catastrophes in incident-network-rich industries proves the sufficiency of hybrid learning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t5, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 5, 0.15).
narrative_ontology:measurement(cata_tr_t10, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 10, 0.19).
narrative_ontology:measurement(cata_tr_t15, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 15, 0.24).
narrative_ontology:measurement(cata_tr_t20, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 20, 0.27).
narrative_ontology:measurement(cata_tr_t25, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 25, 0.29).
narrative_ontology:measurement(cata_tr_t30, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 30, 0.31).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cata_be_t5, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(cata_be_t10, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(cata_be_t15, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(cata_be_t20, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(cata_be_t25, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(cata_be_t30, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(cata_su_t5, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 5, 0.34).
narrative_ontology:measurement(cata_su_t10, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(cata_su_t15, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 15, 0.39).
narrative_ontology:measurement(cata_su_t20, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(cata_su_t25, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(cata_su_t30, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 40, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__hybrid_near_miss_learning, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.12).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_as_necessary_selector).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, simulation_as_proxy_catastrophe).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_avoidance_retention kernel. The sibling readings catastrophe_as_necessary_selector and simulation_as_proxy_catastrophe are separate constraint stories with different ε values, different beneficiary/victim structures, and different classifications. All three readings share the same referent (the kernel commitment to competence maintenance) but differ on which mechanism is sufficient and necessary. This reading asserts the sufficiency of hybrid distributed learning; the siblings assert the necessity of catastrophe or the equivalence of simulation. The three stories are linked via network.affects_constraints; decomposition is documented in each story's commentary.kernel_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_avoidance_retention__hybrid_near_miss_learning, powerless, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
