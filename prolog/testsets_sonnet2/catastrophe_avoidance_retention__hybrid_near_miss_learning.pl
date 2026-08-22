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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: catastrophe_avoidance_retention__hybrid_near_miss_learning
 *   human_readable: Distributed Near-Miss / Foreign-Incident / High-Realism-Drill Learning System
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story instantiates the hybrid-near-miss-learning reading of the
 *   catastrophe-avoidance-retention kernel: the claim that competence in
 *   high-reliability systems (aviation, medicine, nuclear operations) is
 *   maintained neither by simulation alone nor by actual catastrophe alone,
 *   but by a distributed network of near-miss reports, foreign-incident
 *   dissemination, and high-realism drills that together approximate the
 *   statistical and psychological force of catastrophe without paying its
 *   cost. The reading's expected structural delta is that success depends on
 *   the strength of cross-organizational incident-sharing infrastructure —
 *   aviation's ASRS/ICAO exchange model succeeds where medicine's more
 *   hierarchical, litigation-averse reporting culture partially fails,
 *   holding simulation technology roughly constant across both domains. This
 *   is a Tangled Rope: the coordination function (aggregating rare precursor
 *   events into shared learning) is genuine and the beneficiaries (traveling
 *   public, patients, frontline operators generally) are real, but the
 *   extraction runs through the individual reporter who bears disclosure
 *   risk, and requires active enforcement (protected reporting statutes,
 *   no-blame policy mandates, mandatory drill certification) to keep the
 *   system from collapsing back into pure catastrophe-as-teacher.
 *
 * KEY AGENTS:
 *   - safety_regulators: administer the incident-sharing and drill infrastructure
 *   - traveling_public/patients: passive beneficiaries with no visibility into system function
 *   - frontline_reporters/junior_clinicians/whistleblower_engineers: bear the individual cost of disclosure
 *   - industry_associations: set standards determining whether the hybrid model actually operates
 *   - comparative_safety_researchers: analytical observers of the aviation/medicine divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.42).
domain_priors:suppression_score(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.38).
domain_priors:theater_ratio(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__hybrid_near_miss_learning, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__hybrid_near_miss_learning, "Distributed Near-Miss / Foreign-Incident / High-Realism-Drill Learning System").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__hybrid_near_miss_learning, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__hybrid_near_miss_learning).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'e8cdef6c-ad73-4b3b-9ee3-d5468d434674').
narrative_ontology:cs_kernel_codification('e8cdef6c-ad73-4b3b-9ee3-d5468d434674', distributed).
narrative_ontology:cs_authority_grounding('e8cdef6c-ad73-4b3b-9ee3-d5468d434674', practice).
narrative_ontology:cs_interpretation_layer_present('e8cdef6c-ad73-4b3b-9ee3-d5468d434674').
narrative_ontology:cs_reading_relation('e8cdef6c-ad73-4b3b-9ee3-d5468d434674', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, influences).
narrative_ontology:cs_reading_relation('e8cdef6c-ad73-4b3b-9ee3-d5468d434674', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, influences).
narrative_ontology:cs_axiom('e8cdef6c-ad73-4b3b-9ee3-d5468d434674', foundational, network_strength_determines_retention).
narrative_ontology:cs_axiom_status(network_strength_determines_retention, holdable).
narrative_ontology:cs_axiom_grounding('e8cdef6c-ad73-4b3b-9ee3-d5468d434674', network_strength_determines_retention, empirically_contingent).
narrative_ontology:cs_axiom('e8cdef6c-ad73-4b3b-9ee3-d5468d434674', foundational, neither_simulation_nor_catastrophe_alone_sufficient).
narrative_ontology:cs_axiom_status(neither_simulation_nor_catastrophe_alone_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('e8cdef6c-ad73-4b3b-9ee3-d5468d434674', neither_simulation_nor_catastrophe_alone_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('e8cdef6c-ad73-4b3b-9ee3-d5468d434674', post_war_aviation_incident_sharing_model).
narrative_ontology:cs_drift_state('e8cdef6c-ad73-4b3b-9ee3-d5468d434674', contemporary_cross_industry_adoption, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e8cdef6c-ad73-4b3b-9ee3-d5468d434674', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, traveling_public).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, patients).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_operators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_regulators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_reporters).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, junior_clinicians).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, whistleblower_engineers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, industry_associations).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, simulation_training_vendors).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_operators).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__hybrid_near_miss_learning, distributed_incident_learning_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and mandate the reporting, drill, and cross-industry information-sharing infrastructure (e.g. ASRS-style voluntary reporting, foreign-incident bulletins, simulator recertification cycles). They collect the aggregated safety benefit and legitimacy of a functioning system but bear none of the reporting cost directly.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_regulators, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_regulators, beneficiary).

% Rely entirely on the competence-retention system working without any visibility into it; cannot audit whether the airline or hospital they use participates meaningfully in near-miss sharing. Benefit passively when the system functions, bear catastrophic cost when it fails silently.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, traveling_public, beneficiary,
    powerless, immediate, trapped, global).

% Depend on clinicians whose competence is retained (or not) through morbidity-and-mortality conferences, simulation training, and incident review, but have no way to know whether their institution actually participates in cross-organizational learning networks as aviation does, or merely performs compliance.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, patients, beneficiary,
    powerless, immediate, trapped, national).

% Pilots, controllers, surgeons, nurses who both generate the raw material of the learning system (near misses, drill performance) and benefit from others' reported incidents. Their exit from the reporting obligation is constrained by professional norms and licensing requirements, not by free choice.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_operators, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_operators, payer).

% The individual who files the near-miss report or admits the drill failure bears the personal career risk, stigma, or blame exposure so the system as a whole can learn. Protection against retaliation is uneven and depends heavily on the strength of the reporting culture in their specific organization.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_reporters, payer,
    powerless, biographical, trapped, national).

% Bear the brunt of hierarchical suppression when incident review in medicine (unlike aviation) is filtered through attending-physician gatekeeping; their honest reporting of near misses can be professionally costly in a specialty or hospital culture without strong no-blame norms.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, junior_clinicians, payer,
    powerless, biographical, trapped, national).

% Engineers or technicians who escalate a near-miss finding beyond their immediate chain of command absorb retaliation risk (reassignment, blacklisting) when the organization's actual culture punishes disclosure despite official no-blame policy.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, whistleblower_engineers, payer,
    moderate, biographical, constrained, national).

% Aviation bodies (ICAO, IATA) and medical bodies (specialty boards) administer the cross-organizational incident-sharing protocols that make the hybrid learning model work or fail; they set standards for what counts as a reportable near miss and how foreign incidents get disseminated, and their institutional prestige rides on the system appearing to function.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, industry_associations, agenda_setter,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, industry_associations, beneficiary).

% Sell high-realism simulators and drill programs; benefit from the hybrid model's premise that simulation is necessary-but-not-sufficient, since it legitimizes continued investment in simulator fidelity without displacing the incident-sharing function that keeps demand for their product tied to actual operational risk.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, simulation_training_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Study why aviation's cross-organizational learning network (ASRS, ICAO incident exchange) sustains competence while medicine's more fragmented, hierarchical, litigation-averse reporting culture does not replicate the same retention effect, holding simulation fidelity roughly constant across both domains.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, comparative_safety_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates dispersed near-miss reports, foreign-incident bulletins, and high-realism drill outcomes across many organizations so that any single organization's rare catastrophic-precursor events become shared learning material for the whole industry, substituting distributed vicarious experience for the actual catastrophes that would otherwise be the only teacher.
% TRANSFER_FUNCTION: Moves the cost of learning (career risk, stigma, disclosure burden) from the industry collectively to the individual frontline reporter who admits the near miss, while moving the benefit (retained competence, avoided catastrophe) to the traveling public, patients, and the institutions whose safety records improve without them bearing the individual disclosure cost.
% ABSENT_VOICES: Individual clinicians and technicians in weak-reporting-culture institutions who would testify that the no-blame policy is aspirational rather than operative are structurally absent from the aggregate statistics that make the system look like it is working; their silence (non-reporting) is invisible to the very metric meant to detect system failure.
% DISAPPEARANCE_RATIONALE: If distributed incident-sharing infrastructure vanished, aviation-style industries would lose their primary non-catastrophic competence-retention channel and would either need to substitute pure simulation (untested at scale for producing equivalent vigilance) or would drift toward the catastrophe-as-selector regime by default, with a measurable rise in fatal incidents over the following years — the arrangement is load-bearing, not decorative.
% FOUNDING_PROBLEM: Catastrophic failures in aviation (and later medicine) were recognized as too costly, too rare, and too idiosyncratic individually to serve as a reliable teacher; the founding insight (post-WWII aviation safety movement, later adopted in healthcare via the 1999 To Err Is Human report) was that aggregating near misses across many organizations could approximate the statistical power of catastrophe without paying its cost.
% FOUNDING_PROBLEM_CORROBORATION: Independent comparative safety researchers (e.g. NTSB historical analyses, healthcare patient-safety scholars outside hospital administration) attest the founding problem remains live: aviation's incident-sharing rate correlates with its declining fatal-accident rate, while medicine's much lower voluntary-reporting rate is independently documented by researchers unaffiliated with hospital risk-management departments, who are the ones with incentive to claim the system already works.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__hybrid_near_miss_learning, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__hybrid_near_miss_learning, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).
:- end_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the system genuinely distributes learning benefit widely while concentrating disclosure cost narrowly on individual reporters — this is real but not severe extraction, consistent with tangled_rope rather than snare. Suppression (0.38) reflects the structural and cultural barriers reporters face (blame culture, hierarchy, litigation fear) that vary sharply by industry; this is the mechanism the sibling reading catastrophe_as_necessary_selector implicitly denies exists at all. Theater ratio rises modestly over the interval (0.15→0.30) as institutions increasingly perform compliance with reporting mandates (checkbox incident logs, pro-forma M&M conferences) without the underlying no-blame culture that makes the aviation model work — this is the drift the reading predicts will separate aviation-like successes from medicine-like failures.
 *
 * PERSPECTIVAL GAP:
 *   From the safety-regulator seat, this looks like functioning coordination: incident rates fall, the system appears to work. From the frontline-reporter seat in a weak-culture institution, the same structure looks like a demand to personally absorb risk for a benefit that flows to people who will never know your name. The engine should compute these as structurally different seat classifications from the same base data — that divergence is the point of the tangled_rope claim, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Safety regulators and industry associations are agenda-setters who administer the system and collect its legitimacy without bearing individual disclosure risk — low d, beneficiary end. Traveling public and patients are diffuse beneficiaries with no exit and no visibility — near-symmetric but trapped. Frontline reporters, junior clinicians, and whistleblower engineers are the structural payers: they generate the raw material the system needs but absorb career and reputational risk with constrained or trapped exit — high d, target end. Frontline operators generally sit dual-role: they benefit from others' shared incidents but pay when it is their own incident to report.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (catastrophes are too costly and too rare to serve as a reliable teacher) remains live by outside corroboration, which blocks a mandatrophy verdict at the story level — this is not a constraint whose function has vanished while its apparatus persists. But the rising theater ratio is the leading indicator to watch: if the reporting infrastructure hardens into compliance theater without the underlying psychological safety that makes disclosure costless, the system risks quietly reverting to the catastrophe-as-selector regime while still claiming hybrid-learning legitimacy — a piton in aviation's institutional clothing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aviation_medicine_divergence_mechanism,
    'Is the aviation/medicine divergence in competence retention actually explained by incident-sharing network strength, or by confounds like error visibility (crashes are public, misdiagnoses are private), regulatory structure, or professional culture unrelated to reporting infrastructure per se?',
    'Comparative natural experiment: hospital systems and specialties that adopt aviation-style protected no-blame reporting (e.g. some anesthesiology departments, some NHS trusts) can be compared against matched control institutions on incident rates and reporting volume over a multi-year window, controlling for public visibility of errors.',
    'If the divergence is genuinely explained by network strength, this validates the hybrid reading''s central causal claim and argues for investment in cross-organizational reporting infrastructure as the primary lever. If confounds dominate, the hybrid reading''s policy implications weaken substantially even if its descriptive claim about aviation stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aviation_medicine_divergence_mechanism, empirical, 'Whether incident-sharing network strength is the true causal driver of the aviation/medicine competence-retention gap, or a confound.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the three kernel readings disagree — is it an empirical question (does simulation fidelity alone produce equivalent vigilance to real incident exposure?) or a normative question (is it acceptable to accept a higher catastrophe rate in exchange for lower individual disclosure cost)?',
    'Decompose the disagreement: the simulation_as_proxy_catastrophe reading and this reading disagree empirically (testable via longitudinal comparison of simulation-heavy vs. incident-sharing-heavy organizations); the catastrophe_as_necessary_selector reading and this reading disagree partly normatively (is it legitimate to try to avoid the trauma that reading treats as necessary) as well as empirically.',
    'If the disagreement with simulation_as_proxy_catastrophe is purely empirical, it is resolvable by evidence and one reading should eventually be abandoned. If the disagreement with catastrophe_as_necessary_selector is partly normative, no amount of evidence fully resolves it — the three readings may remain permanently coexisting rather than one being vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Whether the kernel contest is empirically resolvable or contains an irreducible normative disagreement among the three readings.').

omega_variable(
    reporter_suppression_mechanism,
    'Is the suppression frontline reporters experience structural (legal liability exposure, absence of protected-reporting statutes) or internalized (professional identity that treats admitting error as personal failure, persisting even where formal no-blame protections exist)?',
    'Track reporting rates before and after protected-reporting statute implementation in a given institution or jurisdiction; a rate increase indicates the suppression was primarily structural, while persistent underreporting after legal protection indicates a substantial internalized component.',
    'If internalized, the effective suppression frontline reporters carry exceeds what the structural suppression metric alone would predict, and policy interventions limited to legal protection would be insufficient without accompanying culture change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reporter_suppression_mechanism, empirical, 'Whether frontline reporter suppression is structural (legal/institutional) or internalized (professional identity), or both.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t8, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 8, 0.18).
narrative_ontology:measurement(cata_tr_t16, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 16, 0.22).
narrative_ontology:measurement(cata_tr_t24, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 24, 0.26).
narrative_ontology:measurement(cata_tr_t32, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 32, 0.28).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cata_be_t8, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(cata_be_t16, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(cata_be_t24, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(cata_be_t32, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cata_su_t8, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(cata_su_t16, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 16, 0.31).
narrative_ontology:measurement(cata_su_t24, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 24, 0.34).
narrative_ontology:measurement(cata_su_t32, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 32, 0.36).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__hybrid_near_miss_learning, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_as_necessary_selector).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the catastrophe_avoidance_retention kernel. simulation_as_proxy_catastrophe claims high-fidelity drills alone suffice; catastrophe_as_necessary_selector claims only actual catastrophe provides adequate selection pressure; this reading (hybrid_near_miss_learning) claims neither alone suffices and that a distributed incident-sharing network is the operative mechanism. Each reading is authored as its own constraint with its own ε and stakeholder structure per the ε-invariance principle; they are linked here rather than merged because they make structurally distinct, differently falsifiable claims about the same underlying phenomenon of competence retention in high-reliability organizations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
