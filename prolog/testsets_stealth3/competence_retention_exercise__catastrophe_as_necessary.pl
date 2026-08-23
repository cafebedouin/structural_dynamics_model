% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__catastrophe_as_necessary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__catastrophe_as_necessary, []).

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
 *   constraint_id: competence_retention_exercise__catastrophe_as_necessary
 *   human_readable: Catastrophe-as-Necessary Doctrine of Competence Maintenance
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   A widely held doctrine in high-hazard industries holds that only actual
 *   catastrophic events supply the organizational learning and visceral
 *   stakes needed to keep catastrophe-avoidance competence genuine;
 *   simulation, on this view, is rehearsal, never the real thing.
 *   Operationalized, the doctrine organizes competence maintenance around an
 *   event-tuition regime: incident-free periods are read as danger signs,
 *   real disasters as authoritative teachers, and post-catastrophe
 *   investigation as the privileged knowledge pipeline. The claim/metric gap
 *   is deliberate and load-bearing: the constraint is CLAIMED as tangled_rope
 *   from the authoring seat — a genuine epistemic core (some failure
 *   knowledge is available only at full stakes) wrapped around an asymmetric
 *   transfer (catastrophic losses priced as tuition fall on those exposed,
 *   while legitimacy, budget flexibility, and paid analysis accrue elsewhere)
 *   — while the metrics are authored as independent descriptive facts about
 *   how the regime actually operates. The engine computes per-seat
 *   classifications from the structural data; divergence between any seat's
 *   computed type and the authored claim is the measurement, not an error.
 *   Epsilon's referent is the standing event-tuition arrangement itself,
 *   assessed by this reading's own lights: even a reading that endorses the
 *   doctrine must price the identifiable parties who pay in blood against the
 *   parties who collect the doctrine's institutional receipts. KEY AGENTS (by
 *   structural relationship): - senior_operators_and_executives: Primary
 *   beneficiary and agenda-setter (institutional/mobile) — sets simulation
 *   budgets, accepts residual risk, controls the post-catastrophe narrative -
 *   frontline_operators: Primary target, doubly positioned
 *   (moderate/identity_locked) — absorbs the bodily risk the doctrine prices
 *   as pedagogically necessary while converting survived catastrophes into
 *   professional standing - exposed_public: Pure target (powerless/trapped) —
 *   bears catastrophic losses it never consented to price as tuition -
 *   catastrophe_investigation_sector: Secondary beneficiary
 *   (organized/mobile) — investigators, consultants, and litigation experts
 *   whose market the catastrophe-reform cycle replenishes -
 *   simulation_first_advocates: Excluded voice (organized/constrained) —
 *   human-factors researchers and simulator vendors whose programs the
 *   doctrine discounts as 'not the real thing' - safety_regulators:
 *   Analytical observer (institutional/analytical) — codifies each
 *   catastrophe's lessons into mandates, ratifying the event-driven cycle
 *
 * KEY AGENTS:
 *   - - senior_operators_and_executives: Primary beneficiary/agenda-setter (institutional/mobile) — books catastrophic losses as tuition, defers preventive capital, controls post-catastrophe narrative
 *   - - frontline_operators: Primary target, doubly positioned (moderate/identity_locked) — bears the bodily risk; collects status and skill from survived events
 *   - - exposed_public: Pure target (powerless/trapped) — passengers, neighbors of hazardous infrastructure, patients; no seat in the competence debate
 *   - - catastrophe_investigation_sector: Secondary beneficiary (organized/mobile) — collects fees and standing from each catastrophe's aftermath
 *   - - simulation_first_advocates: Excluded voice (organized/constrained) — publishes and testifies but does not set training budgets
 *   - - safety_regulators: Analytical observer (institutional/analytical) — converts each catastrophe's lessons into mandates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__catastrophe_as_necessary, 0.42).
domain_priors:suppression_score(competence_retention_exercise__catastrophe_as_necessary, 0.58).
domain_priors:theater_ratio(competence_retention_exercise__catastrophe_as_necessary, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, extractiveness, 0.42).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(competence_retention_exercise__catastrophe_as_necessary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__catastrophe_as_necessary, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__catastrophe_as_necessary, "Catastrophe-as-Necessary Doctrine of Competence Maintenance").
narrative_ontology:topic_domain(competence_retention_exercise__catastrophe_as_necessary, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__catastrophe_as_necessary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__catastrophe_as_necessary, 'd350931d-d70d-4477-98a3-a72681b3bdf1').
narrative_ontology:cs_kernel_codification('d350931d-d70d-4477-98a3-a72681b3bdf1', distributed).
narrative_ontology:cs_authority_grounding('d350931d-d70d-4477-98a3-a72681b3bdf1', practice).
narrative_ontology:cs_interpretation_layer_present('d350931d-d70d-4477-98a3-a72681b3bdf1').
narrative_ontology:cs_reading_relation('d350931d-d70d-4477-98a3-a72681b3bdf1', competence_retention_exercise__simulation_as_sufficient, coexists_with).
narrative_ontology:cs_reading_relation('d350931d-d70d-4477-98a3-a72681b3bdf1', competence_retention_exercise__near_miss_as_bridge, coexists_with).
narrative_ontology:cs_axiom('d350931d-d70d-4477-98a3-a72681b3bdf1', foundational, genuine_competence_requires_real_catastrophe_feedback).
narrative_ontology:cs_axiom_status(genuine_competence_requires_real_catastrophe_feedback, holdable).
narrative_ontology:cs_axiom_grounding('d350931d-d70d-4477-98a3-a72681b3bdf1', genuine_competence_requires_real_catastrophe_feedback, empirically_contingent).
narrative_ontology:cs_axiom('d350931d-d70d-4477-98a3-a72681b3bdf1', foundational, visceral_stakes_irreducible_to_simulation).
narrative_ontology:cs_axiom_status(visceral_stakes_irreducible_to_simulation, holdable).
narrative_ontology:cs_axiom_grounding('d350931d-d70d-4477-98a3-a72681b3bdf1', visceral_stakes_irreducible_to_simulation, empirically_contingent).
narrative_ontology:cs_axiom('d350931d-d70d-4477-98a3-a72681b3bdf1', secondary, incident_free_periods_breed_hidden_competence_decay).
narrative_ontology:cs_axiom_status(incident_free_periods_breed_hidden_competence_decay, holdable).
narrative_ontology:cs_axiom_grounding('d350931d-d70d-4477-98a3-a72681b3bdf1', incident_free_periods_breed_hidden_competence_decay, empirically_contingent).
narrative_ontology:cs_reference_frame('d350931d-d70d-4477-98a3-a72681b3bdf1', catastrophe_tempered_competence_standard).
narrative_ontology:cs_drift_state('d350931d-d70d-4477-98a3-a72681b3bdf1', contemporary_high_fidelity_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d350931d-d70d-4477-98a3-a72681b3bdf1', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__catastrophe_as_necessary, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, senior_operators_and_executives).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, catastrophe_investigation_sector).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, frontline_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__catastrophe_as_necessary, exposed_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__catastrophe_as_necessary, frontline_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets training budgets, approves residual-risk acceptance, and controls the story told after every disaster. The doctrine lets them book catastrophic losses as tuition rather than governance failure, defer expensive preventive capital, and let accountability age past their own career horizon; they are rarely in the seat when the next catastrophe lands. Moving between firms carries their reputations with them.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, senior_operators_and_executives, agenda_setter,
    institutional, biographical, mobile, global).

% Crew the trains, plants, aircraft, and units where failures become funerals. They absorb the bodily risk the doctrine counts as pedagogically necessary, and veterans convert survived catastrophes into professional standing — 'real-world judgment' — that the doctrine guarantees outranks simulator fluency. Leaving the profession means discarding an identity built on scar tissue; staying means betting their bodies on the next tuition event.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, frontline_operators, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__catastrophe_as_necessary, frontline_operators, beneficiary).

% Passengers, residents along rail and chemical corridors, patients. They bear the catastrophic losses the regime treats as the price of genuine competence, without consenting to that pricing and without a seat where competence policy is argued. Exit means not flying, not living downstream, not being treated — no realistic option.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, exposed_public, payer,
    powerless, immediate, trapped, regional).

% Accident investigators, safety consultancies, litigation experts, and the journals and curricula that feed on each disaster's report. Every catastrophe generates hearings, expert fees, and revised standards; the doctrine guarantees the raw material keeps arriving. A world of prevented catastrophes would flatten their demand curve, so their livelihoods ride on the cycle continuing.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, catastrophe_investigation_sector, beneficiary,
    organized, biographical, mobile, global).

% Human-factors researchers, simulator vendors, and high-reliability-organization scholars who argue that high-fidelity simulation plus near-miss analytics can carry competence maintenance. Inside operator organizations their proposals are discounted as 'not the real thing'; they publish and testify but do not set training budgets, and their standing inside the firms they study depends on not being branded simulator-bound.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, simulation_first_advocates, excluded,
    organized, generational, constrained, global).

% Write and enforce the rules, and after each disaster codify its lessons into mandates, implicitly ratifying the event-driven learning cycle. They adjudicate between preventive simulation mandates and experiential-learning norms, and their posture tends to follow whichever reading currently dominates the professional conversation.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__catastrophe_as_necessary, safety_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__catastrophe_as_necessary, senior_operators_and_executives).
narrative_ontology:fixing_cost_class(competence_retention_exercise__catastrophe_as_necessary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns the organization around a shared account of where competence comes from: real events as authoritative feedback, incident-free periods as danger zones, and post-catastrophe investigation as the knowledge pipeline. Solves the collective-action problem of complacency during quiet stretches, when nothing punishes overconfidence until it is too late.
% TRANSFER_FUNCTION: Moves the costs of catastrophic failure — casualties, environmental damage, liability — onto frontline workers and the exposed public, while moving the returns (institutional legitimacy preserved through lessons-learned framing, deferred prevention and simulation expenditure, blame deflected past career horizons, and a steady stream of paid investigation work) to executives and the catastrophe-analysis sector.
% ABSENT_VOICES: Simulation-first researchers and near-miss program advocates are present in the literature but structurally absent from operator budget rooms, where the doctrine dismisses their programs as categorically inferior. Future victims of the next 'necessary' catastrophe have no seat at all — the doctrine spends their lives as prospective tuition before they exist.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, simulation budgets would be judged on transfer-validity evidence rather than dismissed categorically, near-miss programs would gain standing and funding, the investigation sector's demand curve would flatten, and executive accountability frameworks would lose the tuition framing and face negligence questions directly. Many arrangements — budget lines, career hierarchies, investigation institutions, regulatory posture — currently depend on the doctrine and would reorganize around its absence.
% FOUNDING_PROBLEM: Early industrial systems failed in ways no one could anticipate or rehearse; real disasters were the only source of information about how complex systems break, and entire safety disciplines — accident investigation, post-mortem redesign, block signaling after railway disasters — were built on that scarcity.
% FOUNDING_PROBLEM_CORROBORATION: Aviation and chemical-industry accident archives corroborate that specific catastrophes produced reforms unavailable beforehand — the historical problem was real. But human-factors transfer-of-training research and high-reliability-organization field studies, sources outside the benefiting parties, attest that the founding scarcity is receding as simulation fidelity and near-miss analytics mature. Corroboration exists for the founding problem's past liveness; its continuing liveness is disputed from outside the beneficiary set, which is precisely the contest the sibling readings register.
narrative_ontology:disappearance_verdict(competence_retention_exercise__catastrophe_as_necessary, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__catastrophe_as_necessary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__catastrophe_as_necessary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_retention_exercise__catastrophe_as_necessary, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__catastrophe_as_necessary, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__catastrophe_as_necessary_tests).
:- end_tests(competence_retention_exercise__catastrophe_as_necessary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.42: the regime's transfers are real but partly floor-priced — the reading itself grants that some learning is available only at full stakes, so not all borne cost counts as overhead; what remains extractive is the margin of catastrophe that feasible prevention or simulation would have caught but the doctrine excused as tuition. Suppression 0.58 and rising: enforcement is cultural and budgetary rather than legal — simulation proposals dismissed as categorically inferior, near-miss programs starved, dissenters labeled simulator-bound — and the requirement grows as simulation fidelity improves, because dismissing a stronger rival takes more work than dismissing a weak one. Theater_ratio 0.40 and rising: the lessons-learned apparatus (memorials, mandatory modules, safety stand-downs) increasingly ritualizes the catastrophe pedagogy it commemorates. Accessibility_collapse 0.60: inside the doctrine's frame, alternatives collapse hard — if only real events teach, simulation investment and near-miss analytics are second-class by definition — but the frame itself is openly contested, so meta-level alternatives persist. Resistance 0.55: the sibling readings are the organized resistance, backed by transfer-of-training research and high-reliability-organization scholarship. Cyclical dynamics: all three series oscillate around a rising trend on a catastrophe-reform-decay cycle — each catastrophe publicly vindicates the doctrine, spiking its enforcement and burden; between catastrophes memory fades and its grip loosens. The oscillation is not noise: intermittent reinforcement on a variable-ratio schedule (unpredictable catastrophes) is precisely what makes the belief unusually resistant to extinction, so the cycle is itself part of the persistence mechanism. Base properties were measured at a recent inter-catastrophe point (trough following reform decay, t=40). Suppression decomposes roughly 60% structural (budget gates, career gates, procurement standards) and 40% internalized (veteran identity fusion with scar-tissue credentials); the split is carried as an omega. Receipt surface: the doctrine's principal gains — deferred capital, blame deferral past career horizons, narrative control — demonstrably land on the executive seat, so gain_flow names senior_operators_and_executives rather than diffuse; the investigation sector collects fees but does not capture the core gain. Fixing cost: replacing event-forged competence with validated simulation at equal system-level fidelity is an unclosed research and certification frontier, so relative to any single actor's benefit horizon the transition cost is prohibitive, even though the long-run social calculus favors it.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently. From the executive seat the regime is prudent epistemics: honest acknowledgment that simulators fail by omission, with losses booked as the price of real knowledge. From the frontline and public seats the same structure is a risk transfer: their bodies supply the training data, and the tuition framing converts their losses from governance failures into curriculum. The veteran operators are the hinge — they pay in blood and collect in status simultaneously, so their computed position sits between full target and beneficiary (override to 0.72). Identity-lock mechanics: for the veteran cohort the doctrine is constitutive of professional self-concept — 'real-world judgment' earned through survived catastrophe is the currency the doctrine guarantees outranks simulator fluency — so exit is identity death, not job change; ideological and institutional fusion reinforce the professional layer. If the identity frame broke — a seniority cohort raised entirely in high-fidelity simulation reaching command positions — the doctrine's enforcement would collapse from inside, because the status hierarchy it maintains would lose its holders. Regulators observe and ratify; the investigation sector collects without running anything.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (senior_operators_and_executives, catastrophe_investigation_sector) drive low directionalities for those seats; victim declarations (frontline_operators, exposed_public) drive high ones. Exposed_public derives nearest full-target: trapped exit, no offsetting benefit, losses fully externalized onto it. Frontline_operators would derive near-full-target from the victim declaration plus identity_locked exit, but the derivation misses the doctrine's payment to them — guaranteed professional standing over simulator-trained staff, plus genuine skill consolidation from survived hard events — so an explicit override sets d=0.72 for the moderate power atom (frontline_operators is the only moderate-power agent in the story, so the override has no collateral effect). Executives derive low but not floor-level: they carry reputational tail risk and occasional personal exposure, keeping them off the beneficiary extreme. The investigation sector derives near-floor: fees and standing flow in, operational risk does not. Suppression is authored as a raw structural property and is deliberately unscaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — no substitute for real-event data about how complex systems fail — was genuinely live when the safety disciplines formed; whether it remains live is exactly what the sibling readings dispute, so the genealogy is authored contested rather than dead. The tangled_rope classification prevents mandatrophy mislabeling in both directions: calling the regime a rope would erase the blood-priced asymmetry between who pays and who collects; calling it a snare would erase the irreducible core (unknown unknowns and organizational behavior under genuine consequence are not fully simulable, so some real-event epistemic value survives even the strongest simulation reading). If transfer-of-training research eventually closes the system-level fidelity gap, the coordination half atrophies while the tuition framing persists — at that point the regime drifts toward inertial or cover-story territory and the founding_problem_status x disappearance_verdict mismatch becomes the live tripwire. Mandatrophy is not yet declared: the epistemic core is diminished but not extinguished.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading — catastrophe_as_necessary — of the competence_retention_exercise kernel; what would the sibling readings change structurally, and where exactly does the disagreement sit?',
    'No empirical resolution; resolved by adopting a reading. Sibling adoption paths: simulation_as_sufficient collapses the victim set (no one need bear real losses for competence) and drops epsilon toward the coordination-cost floor; near_miss_as_bridge shrinks the victim set to near-miss participants and re-prices catastrophe as failure of the bridge program rather than tuition.',
    'Classification is reading-indexed: under simulation_as_sufficient this arrangement computes as cover-story risk transfer with no necessity defense; under near_miss_as_bridge as transitional; under this reading as a hybrid with a defended epistemic core. The disagreement is located at the sufficiency/exclusivity premise — whether real-event feedback is substitutable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of three live readings of the competence-retention kernel; disagreement located at the sufficiency/exclusivity premise.').

omega_variable(
    necessity_claim_falsifiability,
    'Is the exclusivity premise — that only real catastrophes supply competence-critical learning — empirically decidable?',
    'Longitudinal matched-cohort comparison of organizations with equivalent simulation regimes but different real-event exposure, plus transfer-of-training validity studies at system level (organizational stress behavior under genuine consequence), not merely procedural fidelity.',
    'If simulation-plus-near-miss cohorts match real-event cohorts on subsequent incident outcomes, the necessity premise fails, the regime loses its coordination defense, and classification shifts toward pure extraction; if real-event cohorts show an irreducible advantage, the coordination function strengthens and the hybrid reading stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_claim_falsifiability, empirical, 'Decidability of the doctrine''s core empirical claim.').

omega_variable(
    survivorship_bias_in_vindication,
    'How much of the doctrine''s supporting evidence is survivorship bias — organizations that absorbed a catastrophe and survived telling ''we learned'' stories, while catastrophe-free organizations credit vigilance rather than missing tuition?',
    'Base-rate analysis of post-event safety performance across catastrophe-exposed versus catastrophe-free organizations, controlling for hazard profile, scale, and reporting culture.',
    'If vindication is largely a selection effect, the doctrine''s evidential floor drops out and its persistence reads as interest-driven (blame deflection, budget avoidance), pulling classification toward pure extraction; if exposed organizations genuinely outperform, the epistemic core is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survivorship_bias_in_vindication, empirical, 'Whether the doctrine''s vindication record survives de-biasing.').

omega_variable(
    tuition_vs_negligence_relabeling,
    'When organizations invoke the doctrine after a catastrophe, is the loss irreducible tuition (no feasible anticipation existed) or negligence relabeled?',
    'Counterfactual-prevention audit per invoked catastrophe: whether existing simulation scenarios, near-miss signals, or known precursors would have predicted the failure mode.',
    'A high relabeling rate exposes the doctrine as an accountability shield — cost-bearing without epistemic necessity — shifting effective burden upward and classification toward pure extraction; a low rate confirms a genuine residue of only-at-full-stakes learning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tuition_vs_negligence_relabeling, empirical, 'Audit separating genuine epistemic necessity from post-hoc excuse.').

omega_variable(
    authority_framing_underdetermination,
    'Is practice the only defensible authority_grounding for this reading''s enforcement surface, or does an expertise framing (credentialed high-reliability scholarship adjudicating) fit the same doctrine equally?',
    'Trace whose interpretations actually gate budgets and careers: if debrief culture and veteran standing gate, practice holds; if peer-reviewed findings and certified standards gate, expertise holds.',
    'Under the expertise framing the interpretive layer is the published literature rather than debrief culture, the doctrine''s authority narrows to citable findings, and its cultural enforcement surface — and the measured suppression riding on it — shrinks; classification of the enforcement machinery could shift accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_framing_underdetermination, conceptual, 'CS-framing under-determination: practice-grounded versus expertise-grounded authority for the same doctrine.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (budget gates, career gates, procurement standards) or internalized (veteran identity fusion that makes simulation-equivalence unthinkable)?',
    'Post-doctrine cohort trajectory: compare doctrine adherence and incident response between cohorts raised entirely in high-fidelity simulation and cohorts carrying scar-tissue credentials, after structural gates are removed.',
    'If suppression persists after the gates open, the internalized share exceeds the estimated 60/40 split and the constraint outlives its enforcement infrastructure; if it evaporates, suppression was purely structural and removal is cheap once the frame breaks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized composition of the doctrine''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__catastrophe_as_necessary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(comp_tr_t5, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 5, 0.27).
narrative_ontology:measurement(comp_tr_t10, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 10, 0.25).
narrative_ontology:measurement(comp_tr_t15, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 15, 0.31).
narrative_ontology:measurement(comp_tr_t20, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 20, 0.29).
narrative_ontology:measurement(comp_tr_t25, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 25, 0.34).
narrative_ontology:measurement(comp_tr_t30, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 30, 0.33).
narrative_ontology:measurement(comp_tr_t35, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 35, 0.38).
narrative_ontology:measurement(comp_tr_t40, competence_retention_exercise__catastrophe_as_necessary, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(comp_be_t5, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(comp_be_t10, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(comp_be_t15, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(comp_be_t20, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(comp_be_t25, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 25, 0.41).
narrative_ontology:measurement(comp_be_t30, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 30, 0.36).
narrative_ontology:measurement(comp_be_t35, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 35, 0.44).
narrative_ontology:measurement(comp_be_t40, competence_retention_exercise__catastrophe_as_necessary, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(comp_su_t5, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 5, 0.47).
narrative_ontology:measurement(comp_su_t10, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 10, 0.43).
narrative_ontology:measurement(comp_su_t15, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(comp_su_t20, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(comp_su_t25, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 25, 0.53).
narrative_ontology:measurement(comp_su_t30, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 30, 0.49).
narrative_ontology:measurement(comp_su_t35, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 35, 0.56).
narrative_ontology:measurement(comp_su_t40, competence_retention_exercise__catastrophe_as_necessary, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__catastrophe_as_necessary, identity_coordination).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__catastrophe_as_necessary, near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how organizations keep catastrophe-avoidance competence sharp' decomposes into three readings of the competence_retention_exercise kernel, each with its own stable epsilon and victim structure. This file instantiates catastrophe_as_necessary (referent: the event-tuition regime; epsilon 0.42 — genuine epistemic core wrapped around asymmetric transfer). simulation_as_sufficient (referent: a simulation-carried competence regime; epsilon near the coordination-cost floor if its premise holds) and near_miss_as_bridge (referent: a simulator-plus-near-miss validation regime; intermediate epsilon) are separate stories linked here. The catastrophe reading is historically upstream and culturally dominant; it exerts budgetary and legitimacy pressure on the newer readings without logically eliminating them, because the underlying exposure mix is continuous.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_retention_exercise__catastrophe_as_necessary, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
