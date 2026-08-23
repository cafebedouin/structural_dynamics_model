% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__near_miss_as_bridge
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__near_miss_as_bridge, []).

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
 *   constraint_id: competence_retention_exercise__near_miss_as_bridge
 *   human_readable: Near-Miss Feedback Bridge for Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   High-hazard industries (aviation, nuclear operation, anesthesia, chemical
 *   processing) face a structural dilemma: the catastrophes their competence
 *   exists to prevent are too rare and too costly to learn from directly, yet
 *   that competence decays without exercise. The arrangement under contest
 *   answers with a hybrid: simulators carry routine skill preservation, while
 *   a standing apparatus of near-miss reporting, investigation, and
 *   lesson-integration supplies real-world validation and correction of
 *   simulator content. This story instantiates the near_miss_as_bridge
 *   reading of that arrangement — the claim that the near-miss stream
 *   suffices as the real-world half of the hybrid, making catastrophe neither
 *   necessary nor sufficient. The claim and the metrics are independent
 *   authored facts: the claimed type states what I believe is structurally
 *   true of the arrangement (a genuine coordination core with real asymmetric
 *   extraction riding on it), and the metrics state what I believe is
 *   descriptively true of its actual operation; the engine computes per-seat
 *   classifications from the structural data, and any divergence between
 *   claim and computed type is signal, not error. The epsilon referent is the
 *   standing near-miss-based learning arrangement as it actually operates,
 *   assessed by this reading's own lights — not the catastrophe-anchored or
 *   simulation-only alternatives.
 *
 * KEY AGENTS:
 *   - safety_management_departments: agenda-setter (organized/mobile) — administers reporting intake, convenes investigation boards, decides which findings become simulator scenarios or procedure changes; the apparatus is their portfolio
 *   - senior_operating_executives: beneficiary (powerful/mobile) — fund the loop and consume its assurance artifacts (dashboards, audit results, learning-organization credentials) without touching operational detail
 *   - simulator_training_vendors: beneficiary (organized/arbitrage) — sell scenario libraries, fidelity upgrades, and analytics integrations; each investigated incident generates a procurement justification
 *   - safety_consultancies: beneficiary (organized/arbitrage) — bill for reporting-culture audits, investigation facilitation, and maturity certification; complexity of the loop is their revenue base
 *   - aviation_safety_regulators: agenda-setter and beneficiary (institutional/mobile) — mandate occurrence reporting, operate confidential-reporting programs, collect the aggregate data stream and the legitimacy of their oversight mandate
 *   - frontline_incident_reporters: primary bearer of costs (moderate/constrained) — pilots, nurses, control-room operators who disclose their own and colleagues' errors; they spend debrief hours and carry residual legal and reputational exposure, while gaining the marginal safety the aggregate signal buys
 *   - small_regional_operators: bearer of costs (moderate/constrained) — regional carriers, community hospitals, small chemical facilities bearing fixed compliance costs without in-house expertise
 *   - endangered_public: excluded (powerless/trapped) — passengers, patients, and fence-line communities who carry the tail risk the apparatus exists to suppress and are absent from the rooms where reporting thresholds and investigation priorities are set
 *   - hro_research_community: analytical observer — scholars of high-reliability organizing and accident forensics who see the full structure, including its normalization-of-deviance failure mode, and hold no operational stake in any particular program's continuation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__near_miss_as_bridge, 0.38).
domain_priors:suppression_score(competence_retention_exercise__near_miss_as_bridge, 0.32).
domain_priors:theater_ratio(competence_retention_exercise__near_miss_as_bridge, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, extractiveness, 0.38).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(competence_retention_exercise__near_miss_as_bridge, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__near_miss_as_bridge, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__near_miss_as_bridge, "Near-Miss Feedback Bridge for Competence Retention").
narrative_ontology:topic_domain(competence_retention_exercise__near_miss_as_bridge, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__near_miss_as_bridge).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__near_miss_as_bridge, 'c5ca7991-1431-4443-ab8e-7a40ba4ecfc1').
narrative_ontology:cs_kernel_codification('c5ca7991-1431-4443-ab8e-7a40ba4ecfc1', distributed).
narrative_ontology:cs_authority_grounding('c5ca7991-1431-4443-ab8e-7a40ba4ecfc1', expertise).
narrative_ontology:cs_interpretation_layer_present('c5ca7991-1431-4443-ab8e-7a40ba4ecfc1').
narrative_ontology:cs_reading_relation('c5ca7991-1431-4443-ab8e-7a40ba4ecfc1', competence_retention_exercise__simulation_as_sufficient, forecloses).
narrative_ontology:cs_reading_relation('c5ca7991-1431-4443-ab8e-7a40ba4ecfc1', competence_retention_exercise__catastrophe_as_necessary, forecloses).
narrative_ontology:cs_axiom('c5ca7991-1431-4443-ab8e-7a40ba4ecfc1', foundational, subcatastrophic_evidence_transfers_to_catastrophic_regimes).
narrative_ontology:cs_axiom_status(subcatastrophic_evidence_transfers_to_catastrophic_regimes, holdable).
narrative_ontology:cs_axiom_grounding('c5ca7991-1431-4443-ab8e-7a40ba4ecfc1', subcatastrophic_evidence_transfers_to_catastrophic_regimes, empirically_contingent).
narrative_ontology:cs_axiom('c5ca7991-1431-4443-ab8e-7a40ba4ecfc1', foundational, visceral_stakes_not_required_for_competence_maintenance).
narrative_ontology:cs_axiom_status(visceral_stakes_not_required_for_competence_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('c5ca7991-1431-4443-ab8e-7a40ba4ecfc1', visceral_stakes_not_required_for_competence_maintenance, empirically_contingent).
narrative_ontology:cs_reference_frame('c5ca7991-1431-4443-ab8e-7a40ba4ecfc1', hybrid_sim_nearmiss_exercise_regime).
narrative_ontology:cs_drift_state('c5ca7991-1431-4443-ab8e-7a40ba4ecfc1', contemporary_post_columbia_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('c5ca7991-1431-4443-ab8e-7a40ba4ecfc1', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, safety_management_departments).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, senior_operating_executives).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, simulator_training_vendors).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, safety_consultancies).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, aviation_safety_regulators).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, frontline_incident_reporters).
narrative_ontology:constraint_victim(competence_retention_exercise__near_miss_as_bridge, small_regional_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__near_miss_as_bridge, frontline_incident_reporters).
narrative_ontology:constraint_vindicates(competence_retention_exercise__near_miss_as_bridge, hro_collective_mindfulness_doctrine).
narrative_ontology:constraint_vindicates(competence_retention_exercise__near_miss_as_bridge, precursor_based_risk_modeling).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the reporting intake, convene investigation boards, decide which findings become simulator scenarios or procedure changes, and publish safety-performance metrics. The apparatus is their portfolio: headcount, budget, and professional standing scale with its perceived importance. They can move between organizations carrying their methods, so their stake is in the system's continuation rather than in any single firm's.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, safety_management_departments, agenda_setter,
    organized, biographical, mobile, national).

% Fund the apparatus and consume its assurance output — dashboards, audit results, learning-organization credentials — without touching operational detail. The system converts diffuse operational risk into legible governance artifacts they can present to boards and regulators. Their exit is ordinary executive mobility; the arrangement follows them as industry norm rather than binding any single career.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, senior_operating_executives, beneficiary,
    powerful, biographical, mobile, global).

% Sell scenario libraries, fidelity upgrades, and analytics integrations whose sales case rests on the near-miss stream: each investigated incident generates a procurement justification. They serve many industries and clients simultaneously, so any single customer's program is replaceable revenue and no one operator's choices bind them.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, simulator_training_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Contract to audit reporting cultures, facilitate investigations, and certify maturity models. Their billings depend on the apparatus remaining complex enough to require outside experts, so simplification of the learning loop is a direct revenue threat. Like the vendors they serve many clients across sectors and can redirect effort if any one program falters.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, safety_consultancies, beneficiary,
    organized, biographical, arbitrage, global).

% Mandate occurrence reporting, operate confidential-reporting programs, and set the investigative standards the whole loop must satisfy. They collect the aggregate data stream and the institutional justification for their oversight mandate. Officials rotate between agency and industry posts, giving them mobility the mandate itself does not restrict.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, aviation_safety_regulators, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__near_miss_as_bridge, aviation_safety_regulators, beneficiary).

% Pilots, nurses, control-room operators, and drivers who detect and disclose their own and colleagues' errors. They spend debrief hours, accept interview scrutiny, and carry residual legal and reputational exposure when confidentiality frays; in exchange they work inside systems made marginally safer by the aggregate signal. Leaving the industry means abandoning licensed careers built on years of training, so departure is rarely a realistic option, and their collective leverage runs through unions and professional associations rather than individual choice.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, frontline_incident_reporters, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__near_miss_as_bridge, frontline_incident_reporters, beneficiary).

% Regional carriers, community hospitals, and small chemical facilities bear the fixed costs of compliant reporting and investigation capability without in-house expertise, buying it dearly from the consultant market. Regulatory obligation pins them to participation, and exiting the sector means liquidating specialized assets at a loss.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, small_regional_operators, payer,
    moderate, biographical, constrained, regional).

% Passengers, patients, and fence-line communities carry the tail risk the entire apparatus exists to suppress. They are absent from the rooms where reporting thresholds, confidentiality scopes, and investigation priorities are set; their only levers are aggregate market choices and political pressure after visible failures, both slow and blunt.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, endangered_public, excluded,
    powerless, generational, trapped, global).

% Scholars of high-reliability organizing and accident forensics who study the loop from outside it. They document both its learning yields and its failure modes — normalization of deviance, precursor blindness — and hold no operational stake in whether any particular program continues or dies.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__near_miss_as_bridge, hro_research_community, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__near_miss_as_bridge, safety_management_departments).
narrative_ontology:fixing_cost_class(competence_retention_exercise__near_miss_as_bridge, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates dispersed, individually-costly observations of failure precursors into a shared organizational learning stream, and routes that stream into simulator scenario revision and procedure change before rare events recur.
% TRANSFER_FUNCTION: Moves disclosure labor, investigative attention, and compliance spending from frontline operators and operating units to safety-management institutions and their vendor ecosystem; moves risk information upward and revised training content back down to the line.
% ABSENT_VOICES: Endangered publics — passengers, patients, fence-line communities — are absent from the rooms where reporting thresholds and investigation priorities are set. Inside organizations, the skeptical constituency of engineers who doubt that sub-catastrophic evidence suffices tends to be managed as a morale risk rather than seated as a technical position; their objection survives in the catastrophe_as_necessary sibling reading rather than in this arrangement's own deliberations.
% DISAPPEARANCE_RATIONALE: Overnight removal of the near-miss loop would not restore a prior equilibrium: organizations would lose their principal pre-catastrophe correction channel. Simulator scenarios would stagnate at their last validated state, latent deviations would accumulate unremarked until a catastrophe forced learning, or firms would rebuild expensive substitutes — full-scale drills, third-party audit regimes, catastrophe-anchored exercises. Either way the safety economy rearranges around the loss, which is why every seated party defends some version of the loop even while fighting over who pays for it.
% FOUNDING_PROBLEM: Rare catastrophic failures are too infrequent and too costly to learn from directly, yet competence at avoiding them decays without exercise. The arrangement was built to solve exactly this: harvest near-misses and minor failures as cheap, frequent proxies for catastrophe, and use them to validate and refresh simulator training so that the rare event finds a practiced organization.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: state accident-investigation bodies (NTSB, CSB) repeatedly document missed or discounted precursors in major-accident reports; the academic high-reliability literature (Vaughan's normalization-of-deviance studies, Weick and Sutcliffe's reliability analyses) attests both the learning gap and its difficulty; insurer loss data and frontline union testimony on reporting climates independently confirm the rare-event learning problem remains open. None of these corroborating sources sits inside the safety-management, vendor, or consultancy beneficiary set.
narrative_ontology:disappearance_verdict(competence_retention_exercise__near_miss_as_bridge, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__near_miss_as_bridge, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__near_miss_as_bridge, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_retention_exercise__near_miss_as_bridge, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__near_miss_as_bridge, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__near_miss_as_bridge_tests).
:- end_tests(competence_retention_exercise__near_miss_as_bridge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38): the arrangement's costs are real but partly the price of its own function — disclosure labor, investigative attention, and compliance spending are genuine burdens, and a bureaucratic-plus-vendor rent layer has grown on top, but the arrangement is not primarily a transfer machine. Suppression (0.32) is a raw structural property, unscaled by power or scope: it reflects mandated reporting obligations, disciplinary backstops where just-culture frays, and the career consequences of being the named subject of an investigation — not the totalizing closure of a capture regime, since organizations retain real choice over exercise mix. Theater_ratio (0.42) is the most troubling number: reporting volume has outrun analysis capacity across the sector, so a large share of activity is filing, dashboarding, and ritual 'lessons learned' publication that changes no scenario and no procedure; the trajectory series shows this growing monotonically. Accessibility_collapse is low (0.30): alternatives to near-miss-based learning (heavier simulation, full-scale drills, catastrophe-anchored learning, external auditing) remain visible and choosable — the arrangement competes rather than forecloses. Resistance (0.45) is moderate and structural: production units resist investigation downtime, managers resist findings implying costly change, and frontline reporting rates sag wherever blame expectations revive. The measurement series run on one shared seven-point grid so every tracked metric is authored at every examined time point; the suppression_requirement series traces a real enforcement-capacity arc (voluntary programs, then mandated reporting building enforcement through the middle of the interval, then routinization as digital intake replaced active enforcement), which is why it is authored rather than left to the scalar. Identity-lock dynamics bind the reporter seat: for licensed professionals, disclosing one's own errors is constitutive of professional identity ('good pilots report'), so reporting willingness rests partly on a fused identity frame; if that frame broke, disclosure would collapse to bare instrumental calculus and the loop's input signal would degrade faster than any structural protection could repair.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the structural data is built to let them. From the safety-management seat the arrangement is professional craft and mission — the thing its members are for — and reads as near-pure coordination. From the reporter seat the same structure is disclosure burden, interview scrutiny, and residual exposure, reading as substantially extractive despite the safety it purchases. From the vendor and consultancy seats it is a demand engine whose health is measured in procurement cycles. From the excluded public seat it is an invisible machine that prices their tail risk without their voice. The engine computes these divergences from power, exit, and role declarations; nothing in the authored claim adjudicates them. The countervailing power sits in reporter coalitions — unions and professional associations that can bargain over just-culture rules and confidentiality scope — which is why the reporter seat is authored at moderate power rather than powerless despite individual fragility.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionalities: safety_management_departments (administers and collects), senior_operating_executives (consume assurance), simulator_training_vendors and safety_consultancies (monetize the loop, with arbitrage-grade exit pushing them toward the beneficiary end), and aviation_safety_regulators (dual-positioned — they set the mandate and collect data and mandate-legitimacy, so their derived directionality sits mildly on the beneficiary side of symmetric). Victim declarations drive high directionalities: frontline_incident_reporters bear the disclosure costs directly, though their secondary benefit (the safety bought, the just-culture protection) pulls them back from the full-target end toward roughly 0.6; small_regional_operators bear regressive fixed costs with constrained exit and sit nearer the full-target end. The endangered_public is excluded rather than coordinated: they bear tail risk at maximum scope with trapped exit, but sit outside the enforcement surface entirely — the arrangement neither extracts from them through its mechanisms nor serves them as a seated party. No directionality_overrides are authored: the beneficiary/victim declarations plus exit atoms already separate the seats correctly, and the one genuinely dual-positioned agent (the regulator) is handled by its secondary_role rather than by overriding the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — rare-event learning under competence decay — is live, so no mandatrophy is declared: the arrangement has not outlived its function. The classification work is preventive. Reading the arrangement as pure coordination (a rope) would erase the reporter-borne costs and the bureaucratic/vendor rent layer that the beneficiary/victim data plainly show; reading it as pure extraction (a snare) would erase the genuine collective-action achievement — dispersed, individually-costly precursor knowledge converted into shared competence — without which no one would defend the apparatus at all. Tangled_rope holds both truths in one structure: the same reporting pipeline that aggregates the learning signal also transfers its costs onto the people least positioned to refuse. The forward risk is drift toward the piton pole: the theater_ratio series climbs monotonically as intake outpaces analysis, and if that continues the arrangement's functional core thins while its ceremonial shell thickens — an administrator could rebalance it, but the cost of getting restructuring wrong (chilling the fragile disclosure willingness) currently exceeds what any single seat bears from the imbalance, which is why fixing_cost is authored prohibitive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precursor_sample_bias,
    'Do reported near-misses constitute a representative sample of catastrophe precursors, or are they biased toward visible, survivable, individually-attributable failure modes — leaving latent systemic paths invisible to the bridge?',
    'Retrospective reconstruction of major accidents (Challenger-, Columbia-, Deepwater-Horizon-class events): code what fraction had antecedents present in the organization''s own near-miss stream but unweighted, unmodeled, or discounted at the time.',
    'If a large fraction of catastrophes had uncaptured or discounted precursors, the sufficiency claim fails in its strong form and the reading must either narrow (the bridge covers only common-mode, well-instrumented failures) or collapse toward heavier real-world exercise regimes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precursor_sample_bias, empirical, 'Whether the near-miss sample spans the catastrophe precursor space or systematically misses latent paths.').

omega_variable(
    normalization_of_deviance_drift,
    'Does surviving repeated near-misses without catastrophe teach organizations that the underlying deviations are safe — converting the bridge into a rationalization engine (Vaughan''s normalization of deviance) rather than a corrective one?',
    'Longitudinal coding of investigation dispositions: track whether recurring near-miss classes migrate from corrective-action classifications to accepted-practice classifications over time, and whether simulator scenario envelopes move toward or away from the normalized deviation.',
    'If normalization dominates, the loop''s effective learning yield decays even as report volume grows; theater_ratio understates the dysfunction and the reading''s sufficiency claim erodes from within rather than by external refutation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normalization_of_deviance_drift, empirical, 'Whether the absence of catastrophe corrodes the bridge''s corrective force over time.').

omega_variable(
    kernel_reading_sufficiency_contest,
    'This constraint is the near_miss_as_bridge reading of the competence_retention_exercise kernel: is sub-catastrophic real-world feedback genuinely sufficient to validate and update simulator training, or does the claim hold only by presupposing the simulation component it claims to complement? The sibling readings deny different halves: simulation_as_sufficient denies that real-world validation is needed at all; catastrophe_as_necessary denies that anything short of catastrophe carries the stakes required for durable competence.',
    'Comparative study of organizations operating different exercise mixes (simulation-dominant, near-miss-dominant, catastrophe-anchored) with matched hazard profiles, measuring competence retention under surprise perturbation exercises rather than scheduled evaluations.',
    'Resolution toward simulation_as_sufficient would deflate the investigation apparatus''s necessity and shrink its beneficiary structure toward pure ceremony; resolution toward catastrophe_as_necessary would invert the current beneficiary map and mark the bridge as dangerously insufficient for the hazards it governs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_sufficiency_contest, conceptual, 'Committer structure: which reading of the competence-retention kernel governs, and where the disagreement is located — the epistemic status of sub-catastrophic evidence as a validator of catastrophic-regime competence.').

omega_variable(
    reporter_suppression_mechanism,
    'Is under-reporting driven by structural exposure (legal discovery, scheduling consequences, license risk) or by internalized blame expectations that persist after formal protections are granted?',
    'Natural experiments where legal immunity or de-identification was introduced (confidential reporting programs modeled on ASRS/ASAP): if reporting rates recover fully and durably relative to anonymous channels, suppression was structural; if a persistent gap remains, the residue is internalized.',
    'If internalized, structural protections overstate the system''s health — measured suppression understates the true barrier to disclosure, and adding enforcement mandates would increase gaming rather than restore signal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reporter_suppression_mechanism, empirical, 'Structural versus internalized suppression of the disclosure the bridge runs on.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__near_miss_as_bridge, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(near_miss_bridge_tr_t0, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(near_miss_bridge_tr_t0, observed).
narrative_ontology:measurement(near_miss_bridge_tr_t6, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 6, 0.22).
narrative_ontology:measurement_basis(near_miss_bridge_tr_t6, observed).
narrative_ontology:measurement(near_miss_bridge_tr_t12, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 12, 0.27).
narrative_ontology:measurement_basis(near_miss_bridge_tr_t12, observed).
narrative_ontology:measurement(near_miss_bridge_tr_t18, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 18, 0.31).
narrative_ontology:measurement_basis(near_miss_bridge_tr_t18, observed).
narrative_ontology:measurement(near_miss_bridge_tr_t24, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 24, 0.35).
narrative_ontology:measurement_basis(near_miss_bridge_tr_t24, observed).
narrative_ontology:measurement(near_miss_bridge_tr_t30, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 30, 0.39).
narrative_ontology:measurement_basis(near_miss_bridge_tr_t30, observed).
narrative_ontology:measurement(near_miss_bridge_tr_t36, competence_retention_exercise__near_miss_as_bridge, theater_ratio, 36, 0.42).
narrative_ontology:measurement_basis(near_miss_bridge_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(near_miss_bridge_be_t0, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(near_miss_bridge_be_t0, observed).
narrative_ontology:measurement(near_miss_bridge_be_t6, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 6, 0.33).
narrative_ontology:measurement_basis(near_miss_bridge_be_t6, observed).
narrative_ontology:measurement(near_miss_bridge_be_t12, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 12, 0.36).
narrative_ontology:measurement_basis(near_miss_bridge_be_t12, observed).
narrative_ontology:measurement(near_miss_bridge_be_t18, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 18, 0.38).
narrative_ontology:measurement_basis(near_miss_bridge_be_t18, observed).
narrative_ontology:measurement(near_miss_bridge_be_t24, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 24, 0.4).
narrative_ontology:measurement_basis(near_miss_bridge_be_t24, observed).
narrative_ontology:measurement(near_miss_bridge_be_t30, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 30, 0.39).
narrative_ontology:measurement_basis(near_miss_bridge_be_t30, observed).
narrative_ontology:measurement(near_miss_bridge_be_t36, competence_retention_exercise__near_miss_as_bridge, base_extractiveness, 36, 0.38).
narrative_ontology:measurement_basis(near_miss_bridge_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(near_miss_bridge_su_t0, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(near_miss_bridge_su_t0, observed).
narrative_ontology:measurement(near_miss_bridge_su_t6, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 6, 0.28).
narrative_ontology:measurement_basis(near_miss_bridge_su_t6, observed).
narrative_ontology:measurement(near_miss_bridge_su_t12, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 12, 0.34).
narrative_ontology:measurement_basis(near_miss_bridge_su_t12, observed).
narrative_ontology:measurement(near_miss_bridge_su_t18, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 18, 0.36).
narrative_ontology:measurement_basis(near_miss_bridge_su_t18, observed).
narrative_ontology:measurement(near_miss_bridge_su_t24, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 24, 0.35).
narrative_ontology:measurement_basis(near_miss_bridge_su_t24, observed).
narrative_ontology:measurement(near_miss_bridge_su_t30, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 30, 0.33).
narrative_ontology:measurement_basis(near_miss_bridge_su_t30, observed).
narrative_ontology:measurement(near_miss_bridge_su_t36, competence_retention_exercise__near_miss_as_bridge, suppression_requirement, 36, 0.32).
narrative_ontology:measurement_basis(near_miss_bridge_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__near_miss_as_bridge, information_standard).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise__simulation_as_sufficient).
narrative_ontology:affects_constraint(competence_retention_exercise__near_miss_as_bridge, competence_retention_exercise__catastrophe_as_necessary).

% DUAL FORMULATION NOTE:
% The colloquial question 'how do organizations retain catastrophe-avoidance competence?' decomposes into three structurally distinct arrangements, not one constraint viewed from angles. competence_retention_exercise__simulation_as_sufficient is the upstream substrate (simulators as the exercise medium, beneficiaries concentrated in fidelity economics); this file, near_miss_as_bridge, is the validation loop coupling real sub-catastrophic events to simulator content, adding an investigation-apparatus beneficiary set and reporter-borne costs; competence_retention_exercise__catastrophe_as_necessary is the displaced incumbent that prices learning in disaster currency. Their epsilon values differ because their beneficiary/victim maps differ. This story links both siblings via affects_constraints: the simulation reading supplies the medium this reading validates, and the catastrophe reading is the traditional baseline whose legitimacy this reading erodes without logically engaging its evidence base.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
