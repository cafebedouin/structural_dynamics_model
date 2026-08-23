% ============================================================================
% CONSTRAINT STORY: ai_human_relationship__technocratic_optimization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_human_relationship__technocratic_optimization, []).

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
 *   constraint_id: ai_human_relationship__technocratic_optimization
 *   human_readable: Technocratic Optimization Reading: Human Value Indexed to Productivity and Optimization Potential
 *   domain: technology ethics/political theology/economic
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   ai_human_relationship: the technocratic_optimization reading, under which
 *   AI is an instrument of efficiency maximization and human value is
 *   measured by productivity and optimization potential. The standing
 *   arrangement under contest is the actually-existing deployment of
 *   algorithmic systems under that frame: automated hiring screens,
 *   productivity surveillance, algorithmic work management, scored access to
 *   credit and welfare, and throughput-optimized care allocation. Under this
 *   reading's own frame, persons appear as data profiles whose social weight
 *   is their ranked output, which is precisely what makes the arrangement's
 *   extraction legible: exclusion of 'inefficient' populations, work
 *   subordinated to machine pace, and concentration of decision authority in
 *   algorithmic gatekeepers. Per the epsilon-referent rule, epsilon is
 *   authored for THIS standing arrangement as this reading constitutes it;
 *   the sibling readings (instrumental_subsidiarity, incarnational_humanism)
 *   are separate constraint files over related deployments and are linked via
 *   network.affects_constraints, not described inside this one. Claim and
 *   metrics are independent authored facts: the type is claimed from
 *   structure, the metrics from described operation.
 *
 * KEY AGENTS:
 *   - algorithmic_gatekeeper_firms: agenda-setter (institutional/arbitrage) — designs the objective functions, collects data assets and margin
 *   - enterprise_adopters: beneficiary with payer underside (institutional/constrained) — collects productivity gains, bears lock-in
 *   - shareholders_and_capital_owners: beneficiary (powerful/arbitrage) — collects efficiency rents with no operational exposure
 *   - algorithmically_managed_workers: primary target (powerless/constrained) — bears pacing, surveillance, and metric-ranked evaluation
 *   - low_productivity_screened_persons: target and structurally excluded voice (powerless/trapped) — filtered out with no hearing
 *   - elderly_and_disabled_care_recipients: target (powerless/trapped) — deprioritized as unquantifiable cost centers
 *   - technology_regulators: analytical observer (institutional/analytical) — audits and can impose remedies
 *   - ml_engineering_professionals: analytical observer (organized/mobile) — implements objectives, interprets benchmarks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__technocratic_optimization, 0.76).
domain_priors:suppression_score(ai_human_relationship__technocratic_optimization, 0.74).
domain_priors:theater_ratio(ai_human_relationship__technocratic_optimization, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__technocratic_optimization, extractiveness, 0.76).
narrative_ontology:constraint_metric(ai_human_relationship__technocratic_optimization, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(ai_human_relationship__technocratic_optimization, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__technocratic_optimization, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ai_human_relationship__technocratic_optimization, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__technocratic_optimization, tangled_rope).
narrative_ontology:human_readable(ai_human_relationship__technocratic_optimization, "Technocratic Optimization Reading: Human Value Indexed to Productivity and Optimization Potential").
narrative_ontology:topic_domain(ai_human_relationship__technocratic_optimization, "technology ethics/political theology/economic").

domain_priors:requires_active_enforcement(ai_human_relationship__technocratic_optimization).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__technocratic_optimization, 'd366719f-3540-4fe9-bad9-f1e5b77ced40').
narrative_ontology:cs_kernel_codification('d366719f-3540-4fe9-bad9-f1e5b77ced40', formalized).
narrative_ontology:cs_authority_grounding('d366719f-3540-4fe9-bad9-f1e5b77ced40', expertise).
narrative_ontology:cs_interpretation_layer_present('d366719f-3540-4fe9-bad9-f1e5b77ced40').
narrative_ontology:cs_reading_relation('d366719f-3540-4fe9-bad9-f1e5b77ced40', ai_human_relationship__incarnational_humanism, forecloses).
narrative_ontology:cs_reading_relation('d366719f-3540-4fe9-bad9-f1e5b77ced40', ai_human_relationship__instrumental_subsidiarity, influences).
narrative_ontology:cs_axiom('d366719f-3540-4fe9-bad9-f1e5b77ced40', foundational, human_worth_equals_measurable_output).
narrative_ontology:cs_axiom_status(human_worth_equals_measurable_output, holdable).
narrative_ontology:cs_axiom_grounding('d366719f-3540-4fe9-bad9-f1e5b77ced40', human_worth_equals_measurable_output, empirically_contingent).
narrative_ontology:cs_axiom('d366719f-3540-4fe9-bad9-f1e5b77ced40', foundational, efficiency_maximization_is_supreme_objective).
narrative_ontology:cs_axiom_status(efficiency_maximization_is_supreme_objective, holdable).
narrative_ontology:cs_axiom_grounding('d366719f-3540-4fe9-bad9-f1e5b77ced40', efficiency_maximization_is_supreme_objective, instrumental).
narrative_ontology:cs_reference_frame('d366719f-3540-4fe9-bad9-f1e5b77ced40', efficiency_maximized_social_order).
narrative_ontology:cs_drift_state('d366719f-3540-4fe9-bad9-f1e5b77ced40', contemporary_algorithmic_accountability_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d366719f-3540-4fe9-bad9-f1e5b77ced40', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__technocratic_optimization, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__technocratic_optimization, algorithmic_gatekeeper_firms).
narrative_ontology:constraint_beneficiary(ai_human_relationship__technocratic_optimization, enterprise_adopters).
narrative_ontology:constraint_beneficiary(ai_human_relationship__technocratic_optimization, shareholders_and_capital_owners).
narrative_ontology:constraint_victim(ai_human_relationship__technocratic_optimization, algorithmically_managed_workers).
narrative_ontology:constraint_victim(ai_human_relationship__technocratic_optimization, low_productivity_screened_persons).
narrative_ontology:constraint_victim(ai_human_relationship__technocratic_optimization, elderly_and_disabled_care_recipients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_human_relationship__technocratic_optimization, enterprise_adopters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and operate the scoring, ranking, and optimization systems through which hiring, credit, welfare, and work assignment are mediated. They set the objective functions, define what counts as productive output, collect the behavioral data those systems generate as proprietary training assets, and capture the margin created by matched efficiency. They can relocate models and jurisdictions, and they largely shape the audit standards their own systems are measured against.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, algorithmic_gatekeeper_firms, agenda_setter,
    institutional, generational, arbitrage, global).

% Deploy the optimization systems across their operations and collect measurable productivity gains: lower screening costs, higher throughput, tighter scheduling. They also pay licensing and integration costs, absorb regulatory exposure, and face deep switching costs once workflows, vendor contracts, and data pipelines are built around a gatekeeper's stack.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, enterprise_adopters, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__technocratic_optimization, enterprise_adopters, payer).

% Hold equity in the firms that operate and adopt the systems and collect the efficiency rents as returns. They bear no operational exposure to the systems' effects on workers or screened-out persons and can move capital between sectors and jurisdictions freely.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, shareholders_and_capital_owners, beneficiary,
    powerful, generational, arbitrage, global).

% Work under continuous metric surveillance: task assignment, pacing, evaluation, and dismissal are set by optimization systems tuned to throughput. Their behavioral data feeds the very profiles that rank them. Leaving one employer usually means entering another running comparable systems, since algorithmic management has become the sectoral default in logistics, ride-hailing, content moderation, and increasingly white-collar work.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, algorithmically_managed_workers, payer,
    powerless, biographical, constrained, global).

% Are filtered out by automated hiring screens, credit scores, and welfare-eligibility models that classify them as low-fit, high-risk, or insufficiently productive. The classification is typically invisible to them, built on proxies they cannot inspect, and appealable only through channels the system operator designed. They experience the constraint as pure exclusion: no hearing, no negotiated role, no exit into the formal economy the systems gatekeep.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, low_productivity_screened_persons, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__technocratic_optimization, low_productivity_screened_persons, excluded).

% Enter the systems' field as cost centers: their care needs resist quantification as output, so throughput-optimized allocation deprioritizes them. Staffing ratios, visit lengths, and benefit determinations are tuned to measurable productivity, and their position at the bottom of the optimization ranking is structural rather than adjudicated. They cannot exit dependency on allocated care.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, elderly_and_disabled_care_recipients, payer,
    powerless, biographical, trapped, national).

% Audit, litigate, and legislate around automated decision systems: bias audits, transparency mandates, human-review requirements. They take evidence from every other seat, commission economic and technical analysis, and can impose remedies that alter what the objective functions are permitted to optimize over.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, technology_regulators, observer,
    institutional, generational, analytical, continental).

% Build and maintain the systems: translate institutional goals into loss functions, tune thresholds, interpret benchmark results for adopters and auditors. Some work to embed fairness constraints; all operate inside objective functions they did not ultimately choose. Their skills are portable across employers, which gives them more exit than any payer seat but less agenda authority than the firms they serve.
narrative_ontology:constraint_stakeholder(ai_human_relationship__technocratic_optimization, ml_engineering_professionals, observer,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_human_relationship__technocratic_optimization, algorithmic_gatekeeper_firms).
narrative_ontology:fixing_cost_class(ai_human_relationship__technocratic_optimization, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves large-scale matching and allocation problems that exceed human administrative capacity: assigning tasks, screening applicants, pricing credit, and distributing services across millions of cases using uniform, continuously updated measurable criteria.
% TRANSFER_FUNCTION: Moves decision authority, behavioral data, and productivity surplus from workers and service recipients to the firms that operate the systems and their investors; converts persons' time, attention, and life-data into proprietary training assets and ranked profiles.
% ABSENT_VOICES: Screened-out applicants and denied claimants have no seat in system design; their objections surface only as aggregate appeal statistics inside channels the operators built. Care recipients whose needs resist quantification are likewise unrepresented in the objective functions that allocate to them.
% DISAPPEARANCE_RATIONALE: If the frame and its enforcing systems vanished overnight, hiring, credit, welfare administration, and logistics would reorganize around human-administered or differently-objectived processes; gatekeeper revenue streams and data-asset valuations would collapse; managed workers would regain pacing authority; and currently screened-out populations would re-enter decision processes that must now give reasons a person can answer.
% FOUNDING_PROBLEM: Industrial and then digital-scale operations outpaced human administrative capacity: too many decisions, too much data, inconsistent evaluation across millions of cases. The efficiency frame promised scarce-resource coordination at scale, and extended that promise into a general doctrine that measurable output measures worth.
% FOUNDING_PROBLEM_CORROBORATION: Operations-research and logistics economics literature attests the scale-coordination problem from outside the benefiting parties, and it remains real. No source outside the gatekeeping firms attests that solving it requires indexing human worth to output; audit studies, worker testimony, and regulatory findings uniformly locate that extension as a design choice of the beneficiaries themselves.
narrative_ontology:disappearance_verdict(ai_human_relationship__technocratic_optimization, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__technocratic_optimization, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__technocratic_optimization, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ai_human_relationship__technocratic_optimization, 'none', 1).
narrative_ontology:epsilon_provenance(ai_human_relationship__technocratic_optimization, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_human_relationship__technocratic_optimization_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_human_relationship__technocratic_optimization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_human_relationship__technocratic_optimization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.76) because the frame decouples evaluation from dignity: worth is set by ranked output, so exclusion of whole populations is a designed feature, not leakage. Suppression is high (0.74) because persistence depends on active machinery — continuous monitoring, operator-designed appeal channels, contractual and technical lock-in — not on participant preference. Theater ratio is moderate (0.31): the optimization function is real and does coordinate at scale, but a growing share of activity is Goodharted metric performance (dashboard cultivation, benchmark gaming) rather than the coordination it displays. Accessibility collapse is moderate (0.50): alternatives persist in regulated sectors, cooperatives, and the informal economy, but within platform-mediated labor markets the algorithmic default has collapsed practical alternatives. Resistance is substantial (0.60): union campaigns, algorithmic-accountability litigation, EU-level regulation, and documented-harm scholarship. The temporal series run on one shared grid (T=0..30, five-year steps) so every tracked metric is authored at every examined point; trajectories are monotonic, not cyclical — extraction accumulates as the frame spreads from logistics into hiring, credit, welfare, and care, and the suppression series deliberately tracks enforcement-capacity build-out (monitoring infrastructure, API lock-in, litigation-chilling terms), which is why suppression_requirement is authored despite a static-scalar picture elsewhere.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the gatekeeper seat the arrangement is genuine coordination it built: it solves matching problems no human bureaucracy can, and the frame's anthropology is simply the price function of that solution. From the managed-worker seat the same structure is a pace dictatorship that ranks the person and owns the profile. From the screened-out seat it is not experienced as a constraint on action at all but as a wall: exclusion without encounter. The engine computes this divergence from power, exit, and directionality; nothing in the authored claim adjudicates between the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Gatekeeper firms sit near the beneficiary pole (d near 0): they collect margin and data assets and hold arbitrage-grade exit, which damps their effective extraction toward subsidy. Shareholders sit similarly near zero with full mobility. Enterprise adopters derive low d from their beneficiary declaration, but their constrained exit (sunk integration, vendor lock-in) keeps them from the arbitrage pole — they are coordinated participants who also pay the gatekeepers. Managed workers carry high d amplified by constrained exit: every alternative employer runs comparable systems, so exit does not escape the constraint. Screened-out persons and care recipients sit nearest the full-target pole: trapped exit, no channel to renegotiate their classification. Regulators and engineers occupy analytical/mobile observational positions with negligible direct extraction flow. Scope is global for the gatekeeping structure, which raises verification difficulty and modestly amplifies effective extraction on the target seats; suppression, by contrast, is authored as a raw structural property and is not scaled.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope guards against both symmetrical errors. Calling it a snare would erase the genuine scale-coordination function — task matching, credit pricing, service allocation at volumes human administration cannot process — which would survive any dignity-preserving reform and which reformers need not destroy. Calling it a rope would erase the documented asymmetric extraction: the anthropological reduction of persons to profiles, the exclusion of 'inefficient' populations, and the transfer of decision authority to gatekeepers, all of which ride on the coordination function rather than constituting it. On the genealogy interview, the founding problem (coordination at scale) is live, so the arrangement is not mandatrophy-resolved; but the frame exhibits mandate creep — the efficiency objective originally warranted for logistics now governs person-evaluation, an extension no outside party attests was ever required by the founding problem. That gap between live founding problem and expanded mandate is exactly the structure the mismatch consumer checks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (technocratic_optimization) of the kernel ai_human_relationship; what structurally changes if a sibling reading governs instead?',
    'Comparative classification across the three reading files: instrument the same deployments under instrumental_subsidiarity and incarnational_humanism framings and diff the computed victim sets, directionalities, and types.',
    'Under instrumental_subsidiarity the extraction re-reads as governance failure over a neutral tool (victim sets shrink to enforcement gaps); under incarnational_humanism the same deployments read as rights violations with a materially larger victim set including all profile-subjected persons. Only this reading makes the productivity-worth index itself the load-bearing structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: which reading of the AI-human-relationship kernel this story instantiates and what siblings would change.').

omega_variable(
    productivity_measure_validity,
    'Does output-indexed measurement track human value, or does it systematically misvalue care work, maintenance, presence, and creativity — the activities least reducible to ranked throughput?',
    'Longitudinal outcome audits comparing domains where metric ranking governs allocation against matched domains where it does not, tracking both allocative outcomes and dignitary-harm indicators.',
    'If the measure is systematically invalid, the frame''s ''optimization'' is misallocation plus dignitary harm, pushing the constraint toward snare; if substantially valid within bounded domains, the tangled_rope reading holds and reform targets threshold-setting rather than the frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(productivity_measure_validity, empirical, 'Whether the productivity-worth equivalence at the frame''s core is empirically defensible.').

omega_variable(
    exclusion_threshold_opacity,
    'Where do the systems draw the ''efficient enough'' line, and is the threshold set by validated evidence or by rent defense and cost-shifting?',
    'Regulatory discovery compelling threshold documentation and validation studies; comparison of thresholds across firms facing different competitive pressures.',
    'Evidence-set thresholds localize the extraction to miscalibration; rent-set thresholds generalize it, supporting reclassification toward snare and mandated human review.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_threshold_opacity, empirical, 'Opacity of the exclusion boundary that determines who counts as optimizable.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression borne by managed workers structural (income dependence on platform-mediated labor, sector-wide algorithmic defaults) or internalized (self-optimization culture in which workers adopt the metric as self-worth)?',
    'Post-exit suppression trajectory: track workers who leave algorithmically managed employment; if metric-indexed self-evaluation and pace normalization persist after exit, the internalized component is substantial.',
    'If internalized, effective suppression exceeds the structural measure — targets carry the constraint with them — raising the true cost of exit and strengthening the case that exit options are weaker than they appear.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism of worker-side suppression.').

omega_variable(
    inevitability_vs_rent_defense,
    'Is the frame''s spread driven by demonstrated allocative superiority, or by network effects, data-asset accumulation, and rent defense that would persist even if the frame were allocatively inferior?',
    'Compare adoption and retention patterns in jurisdictions with mandated human review and metric disclosure against unrestricted jurisdictions; test whether performance deltas survive transparency.',
    'If rent defense dominates, the constraint''s persistence is enforced rather than earned, supporting snare-leaning recomputation; if superiority dominates, the coordination floor is doing real work and the tangled_rope reading is stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(inevitability_vs_rent_defense, conceptual, 'Whether the frame persists by merit or by lock-in — the naturalness question for this constructed constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__technocratic_optimization, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aihr_techopt_tr_t0, ai_human_relationship__technocratic_optimization, theater_ratio, 0, 0.12).
narrative_ontology:measurement(aihr_techopt_tr_t5, ai_human_relationship__technocratic_optimization, theater_ratio, 5, 0.15).
narrative_ontology:measurement(aihr_techopt_tr_t10, ai_human_relationship__technocratic_optimization, theater_ratio, 10, 0.19).
narrative_ontology:measurement(aihr_techopt_tr_t15, ai_human_relationship__technocratic_optimization, theater_ratio, 15, 0.23).
narrative_ontology:measurement(aihr_techopt_tr_t20, ai_human_relationship__technocratic_optimization, theater_ratio, 20, 0.26).
narrative_ontology:measurement(aihr_techopt_tr_t25, ai_human_relationship__technocratic_optimization, theater_ratio, 25, 0.29).
narrative_ontology:measurement(aihr_techopt_tr_t30, ai_human_relationship__technocratic_optimization, theater_ratio, 30, 0.31).

% Extraction over time
narrative_ontology:measurement(aihr_techopt_be_t0, ai_human_relationship__technocratic_optimization, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(aihr_techopt_be_t5, ai_human_relationship__technocratic_optimization, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(aihr_techopt_be_t10, ai_human_relationship__technocratic_optimization, base_extractiveness, 10, 0.57).
narrative_ontology:measurement(aihr_techopt_be_t15, ai_human_relationship__technocratic_optimization, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(aihr_techopt_be_t20, ai_human_relationship__technocratic_optimization, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(aihr_techopt_be_t25, ai_human_relationship__technocratic_optimization, base_extractiveness, 25, 0.72).
narrative_ontology:measurement(aihr_techopt_be_t30, ai_human_relationship__technocratic_optimization, base_extractiveness, 30, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(aihr_techopt_su_t0, ai_human_relationship__technocratic_optimization, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(aihr_techopt_su_t5, ai_human_relationship__technocratic_optimization, suppression_requirement, 5, 0.46).
narrative_ontology:measurement(aihr_techopt_su_t10, ai_human_relationship__technocratic_optimization, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(aihr_techopt_su_t15, ai_human_relationship__technocratic_optimization, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(aihr_techopt_su_t20, ai_human_relationship__technocratic_optimization, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(aihr_techopt_su_t25, ai_human_relationship__technocratic_optimization, suppression_requirement, 25, 0.69).
narrative_ontology:measurement(aihr_techopt_su_t30, ai_human_relationship__technocratic_optimization, suppression_requirement, 30, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__technocratic_optimization, resource_allocation).
narrative_ontology:affects_constraint(ai_human_relationship__technocratic_optimization, ai_human_relationship__instrumental_subsidiarity).
narrative_ontology:affects_constraint(ai_human_relationship__technocratic_optimization, ai_human_relationship__incarnational_humanism).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the AI-human relationship' decomposes into three structurally distinct readings of one kernel. This file (technocratic_optimization) carries the productivity-worth index as load-bearing structure and hence the highest epsilon of the family; instrumental_subsidiarity strips the index and reads the same deployments as governance problems over neutral tools; incarnational_humanism denies the index outright and reads the deployments as dignity violations. Upstream/downstream: this reading creates structural pressure on the instrumental reading (efficiency systems reshape what 'proper governance' means, from ex ante rules to metric auditing) and stands in logical contradiction with the incarnational reading's core anthropological premise. Family members link via affects_constraints; each file holds its own stable epsilon per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
