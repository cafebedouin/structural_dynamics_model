% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__hybrid_degradation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__hybrid_degradation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: catastrophe_proxy_sufficiency__hybrid_degradation_reading
 *   human_readable: Simulation-Based Catastrophe Proxy Regime (Hybrid Degradation Reading)
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   Since the late 1960s, high-reliability industries — aviation first, then
 *   nuclear power, medicine, offshore energy — have progressively replaced
 *   scarce real-event experience with mandated, simulator-based training and
 *   certification. Simulator hours and recurrent check results now constitute
 *   the qualifying evidence for licenses and continued employment; full-scale
 *   rehearsal of genuine emergencies has become administratively impossible
 *   and legally hazardous, so the simulation regime is the only rehearsal
 *   most practitioners ever get. This story instantiates the
 *   hybrid_degradation_reading of the catastrophe_proxy_sufficiency kernel
 *   (see kernel_context; sibling readings are separate files linked in
 *   network.affects_constraints). On this reading the arrangement genuinely
 *   maintains procedural competence at scale — that is its real coordination
 *   work — while tacit knowledge (pattern recognition under ambiguity,
 *   crew-resource judgment) and stress-response capacity decay across
 *   personnel generations, because no simulated scenario carries real
 *   consequence and the regime's own metrics cannot register the decay. The
 *   epsilon referent is the standing simulation-substitution arrangement as
 *   this reading assesses it, not any mixed-exposure alternative this reading
 *   would endorse. Claimed type and metrics are authored independently: the
 *   structure is claimed as a hybrid coordination/extraction arrangement, and
 *   the metrics describe its actual operation — rising displacement of
 *   supplementary by substitutive simulation, hardening enforcement, and a
 *   growing performative share in recurrent training.
 *
 * KEY AGENTS:
 *   - regulatory_certification_authorities: agenda-setter (institutional/constrained) — writes and enforces the equivalence rules, absorbs post-failure blame
 *   - certification_training_industry: primary beneficiary (organized/mobile) — collects recurring mandated training revenue
 *   - regulated_operators: secondary beneficiary and payer (powerful/constrained) — buys compliance cheaper than real-event exposure, cannot leave the regime
 *   - frontline_operating_crews: primary payer (moderate/constrained) — procedural skill maintained, unsimulated judgment carries the decay
 *   - rare_event_exposed_public: ultimate payer (powerless/trapped, generational horizon) — bears realized tail outcomes
 *   - hro_safety_research_community: analytical observer (analytical/analytical) — publishes the decay evidence, holds no rule-setting authority
 *   - systemic_safety_margin: non-agent bearer, listed for completeness — the aggregate practiced-judgment reserve drawn down silently
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.64).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.58).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "Simulation-Based Catastrophe Proxy Regime (Hybrid Degradation Reading)").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__hybrid_degradation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__hybrid_degradation_reading, '2254df54-b0bd-4d0a-b7db-fc620c163626').
narrative_ontology:cs_kernel_codification('2254df54-b0bd-4d0a-b7db-fc620c163626', distributed).
narrative_ontology:cs_authority_grounding('2254df54-b0bd-4d0a-b7db-fc620c163626', distributed).
narrative_ontology:cs_reading_relation('2254df54-b0bd-4d0a-b7db-fc620c163626', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, forecloses).
narrative_ontology:cs_reading_relation('2254df54-b0bd-4d0a-b7db-fc620c163626', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('2254df54-b0bd-4d0a-b7db-fc620c163626', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('2254df54-b0bd-4d0a-b7db-fc620c163626', foundational, differential_competence_decay).
narrative_ontology:cs_axiom_status(differential_competence_decay, holdable).
narrative_ontology:cs_axiom_grounding('2254df54-b0bd-4d0a-b7db-fc620c163626', differential_competence_decay, empirically_contingent).
narrative_ontology:cs_axiom('2254df54-b0bd-4d0a-b7db-fc620c163626', secondary, procedural_layer_simulation_maintained).
narrative_ontology:cs_axiom_status(procedural_layer_simulation_maintained, holdable).
narrative_ontology:cs_axiom_grounding('2254df54-b0bd-4d0a-b7db-fc620c163626', procedural_layer_simulation_maintained, empirically_contingent).
narrative_ontology:cs_reference_frame('2254df54-b0bd-4d0a-b7db-fc620c163626', simulation_primary_training_regime).
narrative_ontology:cs_drift_state('2254df54-b0bd-4d0a-b7db-fc620c163626', contemporary_post_pandemic_skill_fade_evidence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2254df54-b0bd-4d0a-b7db-fc620c163626', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_training_industry).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulated_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, frontline_operating_crews).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, rare_event_exposed_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulated_operators).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulated_experience_equivalence_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__hybrid_degradation_reading, compliance_metric_sufficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and administers the rules defining what counts as qualifying experience for licenses and certificates in aviation, nuclear operations, medicine, and comparable fields. Accepts documented simulator hours and recurrent check results as proof of continued proficiency. Funded by fees and appropriations, measured on throughput and compliance rates, and absorbs political blame after rare publicized failures. Rewriting the equivalence rules would mean reopening international harmonization agreements and re-litigating cost-benefit baselines with every affected industry.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulatory_certification_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Designs, builds, and delivers the simulator devices and recurrent courses the rules require: full-flight simulators, control-room replica trainers, mannequin-based clinical scenarios. Revenue recurs by design because qualifications expire on fixed cycles and must be renewed. Customers are concentrated and regulated, but the same product lines sell across aviation, energy, healthcare, and maritime markets, so a downturn in one sector can be absorbed by shifting sales effort elsewhere.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_training_industry, beneficiary,
    organized, biographical, mobile, global).

% Airlines, nuclear utilities, hospital groups, and comparable firms must show their people hold current qualifications to insure, charter, or license their operations. Buying the mandated training package is far cheaper than staging full-scale emergency exercises or releasing crews for real-event exposure, and a binder of completed check results is the artifact regulators, insurers, and courts accept. They pay the training invoices and lobby the rule-writers over syllabus content, but cannot decline the package without grounding fleets or idling plants.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulated_operators, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulated_operators, payer).

% Pilots, reactor operators, surgical teams, and their equivalents occupy the simulator seats on the mandated cycle. Routine procedures stay sharp; the scenarios repeat until they are familiar, and everyone in the room knows the envelope of what can be thrown at them. Their licenses depend on passing, so opting out is unavailable while they hold the job. When a genuinely novel, consequential event arrives, they meet it with whatever unsimulated judgment they have accumulated — exactly the portion of their skill the cycle never exercises.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, frontline_operating_crews, payer,
    moderate, biographical, constrained, global).

% Passengers, plant neighbors, and patients carry the consequences of how the last link of the chain performs on the day a real event arrives. They have no seat in syllabus design, no visibility into what training covers versus what it cannot, and no practical way to exit air travel, energy systems, or hospitals. Their exposure is realized rarely, catastrophically, and disproportionately far in the future relative to the decisions that shape it.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, rare_event_exposed_public, payer,
    powerless, generational, trapped, global).

% Human-factors and high-reliability researchers study skill decay, stress inoculation, and transfer-of-training. They publish the longitudinal and post-incident evidence on what simulation does and does not preserve, advise inquiries after failures, and hold consultative access to rule-making dockets without any authority to set syllabus or equivalence policy.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, hro_safety_research_community, observer,
    analytical, generational, analytical, global).

% Non-agent entity listed for completeness: the aggregate reserve of practiced judgment and stress-tested response that a generation of crews would otherwise carry into rare events. It is drawn down silently whenever routine metrics look healthy, and its depletion becomes visible only when a real event draws on it.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, systemic_safety_margin, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(catastrophe_proxy_sufficiency__hybrid_degradation_reading, systemic_safety_margin).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__hybrid_degradation_reading, certification_training_industry).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__hybrid_degradation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective problem: the events that build deep operational competence are too rare, dangerous, and expensive to experience on purpose, so rehearsal must be manufactured. A standardized simulator regime lets thousands of crews per year practice abnormal and emergency procedures safely, uniformly, and repeatably, and gives regulators and insurers a single auditable standard of demonstrated proficiency across millions of practitioners who would otherwise be unevaluable.
% TRANSFER_FUNCTION: Moves mandated training spending from operators (and onward to fares, rates, and premiums) to training providers and simulator manufacturers on a legally enforced renewal cycle; moves documented proficiency evidence from crews to regulators and insurers; and moves unrehearsed-response exposure forward in time — from the present balance sheets that book the savings to future operations and bystanders who settle it on the day a real event arrives.
% ABSENT_VOICES: Future crews and future passengers — the seats that settle the deferred exposure — are structurally absent: nobody represents a cohort that has not yet been hired or flown. Safety researchers hold consultative seats in docket processes but no vote on equivalence rules; crew unions negotiate hours and scheduling but not syllabus content or what counts as qualifying experience; scenario designers from outside the vendor ecosystem are rarely admitted.
% DISAPPEARANCE_RATIONALE: Overnight removal stalls every licensing pipeline that depends on documented recurrent proficiency; insurers withdraw or reprice coverage they can no longer underwrite against evidence; operators ground fleets and idle plants rather than run uncertified; the professions reorganize around apprenticeship chains and whatever real-event exposure can be lawfully arranged, at a fraction of current throughput. The rearrangement would be fast, expensive, and global.
% FOUNDING_PROBLEM: Jet-era and reactor-era accidents repeatedly involved crews meeting abnormal situations they had never experienced and were never drilled on, while the events that would teach them could not be staged without killing people. The arrangement was built to manufacture rehearsal for the unstageable event, and to give regulators a scalable way to certify that millions of practitioners had received it.
% FOUNDING_PROBLEM_CORROBORATION: Independent accident investigation boards (NTSB, BEA, and counterparts) attest the founding problem is real and recurring — their reports repeatedly cite crews executing procedures correctly while missing the situation's actual shape. The peer-reviewed transfer-of-training and skill-decay literature, largely outside industry funding, corroborates both halves: simulation transfers procedural skill well, and unpracticed judgment decays. What no source outside the beneficiary set attests is that the current scope of substitution — equivalence-counted simulator hours as sufficient experience — is adequate; that assertion is made only by parties paid inside the arrangement.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__hybrid_degradation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__hybrid_degradation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__hybrid_degradation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_proxy_sufficiency__hybrid_degradation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.64: the arrangement prices present training-cost savings and recurring certification revenue against a deferred, unpriced drawdown of response capacity that lands on crews and the public when a real event finally arrives; the transfer is real but rides on top of genuine rehearsal value, so it sits well below pure-extraction levels. Suppression 0.58: alternatives are barred less by force than by architecture — hour-counting equivalence rules, liability exposure for unsanctioned drills, insurance conditions — with an internalized component (simulator-calibrated confidence) routed to omega suppression_structural_vs_internalized. Theater_ratio 0.47: recurrent scenarios converge on a known envelope, crews rehearse the rehearsal, and a growing share of training activity demonstrates compliance rather than builds capability, while genuine procedural acquisition continues. Accessibility_collapse 0.52: real-event exposure is categorically unavailable — catastrophes cannot be scheduled — but partial substitutes (surprise-element full-mission exercises, live plant transients, stress-inoculation programs) survive at the margin, so alternatives are narrowed, not annihilated. Resistance 0.45: skill-fade literature, union complaints on training quality, and post-accident findings press against the regime, fragmented and lacking a decision seat. The measurement series share one grid (1968/1980/1992/2004/2016/2024) with every tracked metric authored at every point; suppression_requirement is tracked because this story specifically traces an enforcement ratchet — equivalence doctrine hardening from supplementary practice into mandatory recurrent gating — not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the collector seats should compute differently. From the training industry's seat the arrangement is a product line with guaranteed recurrence; from operators' seat it is the cheapest lawful path to insurable, licensable operation; from crews' seat it is a cycle that keeps procedures sharp while never exercising the judgment rare events demand; from the public's seat it is an invisible bet placed on their behalf with a generational settlement date. Same-level divergence: regulated_operators and certification_training_industry hold comparable nominal standing, but operators' exit is closed (production depends on the certificate) while trainers can move product across sectors — equal power, asymmetric exit. Crews' professional identity is partly fused with checkride-readiness — self-assessment runs on simulator-calibrated confidence — but the binding bar on exit is licensure rather than self-concept, so exit_options is authored constrained rather than identity_locked; the identity fusion feeds the internalized-suppression omega instead.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: certification_training_industry (mobile exit, organized power) sits near the beneficiary pole; regulated_operators (declared beneficiary with a secondary payer role, constrained exit) derives moderately beneficiary-side, reflecting net savings after invoice payment. Victim declarations drive the targets: frontline_operating_crews (constrained exit) and rare_event_exposed_public (trapped, generational horizon) sit near the target pole, the public furthest because neither visibility nor exit exists. The agenda-setter seat (regulatory_certification_authorities) declares no beneficiary or victim position, so an explicit override is authored: the derivation would otherwise fall back to a generic institutional default, but this seat nets slightly beneficiary-side of symmetric — it collects administrability and throughput from the arrangement it wrote while absorbing blame and political risk when degraded response meets a real event (d = 0.40). Scope amplification runs through the stakeholders' spatial scopes: the public's global scope makes verification of the deferred drawdown hardest exactly where the cost lands. Suppression is authored as a raw structural property and is not scaled; only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — safe rehearsal of events too rare and dangerous to experience — is live, and the arrangement is nowhere near vestigial: remove it and licensing pipelines, insurance, and proficiency assurance collapse (world_rearranges). Mandatrophy is therefore not resolved and no sunset clause is authored. The classification work here is anti-mislabeling in both directions: a pure-extraction reading (the kernel's necessity sibling) would erase the real, scalable rehearsal function that keeps routine procedures sharp for millions of practitioners; a pure-coordination reading (the proxy sibling) would erase the silent, metric-invisible transfer of response capacity from the present to an unbargaining future. The hybrid structure keeps both halves on the books, and the generational decay clock is why the extraction half evades ordinary oversight: every metric the regime publishes refreshes on a training-cycle cadence, while the thing being drawn down matures on a cohort cadence no dashboard samples.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_status,
    'This constraint is the hybrid_degradation_reading of the catastrophe_proxy_sufficiency kernel. Would adopting a sibling reading — simulation_as_proxy_catastrophe, catastrophe_necessity, or simulation_fidelity_threshold — change the structural classification, and where exactly is the disagreement located?',
    'Longitudinal, layer-resolved competence measurement (procedural vs. tacit vs. stress-response) across cohorts trained exclusively in simulation: flat profiles support the proxy reading; uniform decay supports the necessity reading; decay abolished by fidelity upgrades supports the threshold reading; layer-specific generational decay supports this reading.',
    'The proxy sibling collapses this story toward a pure coordination account (measured extraction was decay-blindness); the necessity sibling collapses it toward pure extraction cover (no genuine coordination function survives); the threshold sibling converts the decay into a fixable engineering parameter and the arrangement into transitional support pending fidelity investment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_status, conceptual, 'Which reading of the catastrophe-proxy kernel holds determines whether the arrangement coordinates, extracts, or transitions.').

omega_variable(
    tacit_layer_decay_reality,
    'Is generational decay of tacit knowledge and stress-response capacity under simulation-only regimes real, or an artifact of inference from rare post-hoc failures that selection effects could explain?',
    'Cohort-tracking studies comparing first-real-event performance of simulation-intensive versus mixed-exposure cohorts; natural experiments such as post-pandemic return-to-service skill-fade records and nuclear crew performance during unprecedented transients.',
    'Confirmed decay raises effective extraction on crews and public and hardens the victim declarations; refuted decay lowers extractiveness toward a coordination-dominant profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_layer_decay_reality, empirical, 'Whether the decay mechanism this reading asserts is measurable outside anecdote.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression keeping alternatives to simulation-only qualification off the table structural (hour-counting equivalence rules, liability architecture, insurance conditions) or internalized (crews and managers calibrated to treat comfortable, repeatable simulator performance as evidence of readiness)?',
    'Observe behavior where structural bars lift: jurisdictions or firms adopting surprise-element full-mission training or real-transient exposure — if demand stays flat absent mandate, internalization dominates; if demand surges, structural bars dominate.',
    'Internalization-dominant suppression persists even after equivalence rules are rewritten, so removing the written rule understates remaining suppression; structural-dominant suppression falls quickly with rule change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of suppression between written-rule barriers and simulator-calibrated overconfidence.').

omega_variable(
    training_revenue_rent_share,
    'How much of certification-industry revenue is rent above the genuine cost of delivering competent rehearsal, versus pass-through of real delivery cost?',
    'Cost decomposition of simulator provision and course delivery against mandated pricing; comparison of pricing in mandated versus voluntary training markets.',
    'A high rent share concentrates gain receipt in the training-industry seat and strengthens the extraction reading; a near-zero rent share relocates the accrual toward operators'' liability and compliance savings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(training_revenue_rent_share, empirical, 'Rent-versus-cost split inside the mandated training revenue stream.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 1968, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_proxy_hybrid_tr_t1968, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 1968, 0.15).
narrative_ontology:measurement_basis(catastrophe_proxy_hybrid_tr_t1968, observed).
narrative_ontology:measurement(catastrophe_proxy_hybrid_tr_t1980, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 1980, 0.22).
narrative_ontology:measurement_basis(catastrophe_proxy_hybrid_tr_t1980, observed).
narrative_ontology:measurement(catastrophe_proxy_hybrid_tr_t1992, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 1992, 0.3).
narrative_ontology:measurement_basis(catastrophe_proxy_hybrid_tr_t1992, observed).
narrative_ontology:measurement(catastrophe_proxy_hybrid_tr_t2004, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 2004, 0.37).
narrative_ontology:measurement_basis(catastrophe_proxy_hybrid_tr_t2004, observed).
narrative_ontology:measurement(catastrophe_proxy_hybrid_tr_t2016, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 2016, 0.43).
narrative_ontology:measurement_basis(catastrophe_proxy_hybrid_tr_t2016, observed).
narrative_ontology:measurement(catastrophe_proxy_hybrid_tr_t2024, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 2024, 0.47).
narrative_ontology:measurement_basis(catastrophe_proxy_hybrid_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(catastrophe_proxy_hybrid_be_t1968, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 1968, 0.3).
narrative_ontology:measurement_basis(catastrophe_proxy_hybrid_be_t1968, observed).
narrative_ontology:measurement(catastrophe_proxy_hybrid_be_t1980, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 1980, 0.38).
narrative_ontology:measurement_basis(catastrophe_proxy_hybrid_be_t1980, observed).
narrative_ontology:measurement(catastrophe_proxy_hybrid_be_t1992, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 1992, 0.46).
narrative_ontology:measurement_basis(catastrophe_proxy_hybrid_be_t1992, observed).
narrative_ontology:measurement(catastrophe_proxy_hybrid_be_t2004, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 2004, 0.54).
narrative_ontology:measurement_basis(catastrophe_proxy_hybrid_be_t2004, observed).
narrative_ontology:measurement(catastrophe_proxy_hybrid_be_t2016, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 2016, 0.6).
narrative_ontology:measurement_basis(catastrophe_proxy_hybrid_be_t2016, observed).
narrative_ontology:measurement(catastrophe_proxy_hybrid_be_t2024, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 2024, 0.64).
narrative_ontology:measurement_basis(catastrophe_proxy_hybrid_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(catastrophe_proxy_hybrid_su_t1968, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 1968, 0.25).
narrative_ontology:measurement_basis(catastrophe_proxy_hybrid_su_t1968, observed).
narrative_ontology:measurement(catastrophe_proxy_hybrid_su_t1980, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement_basis(catastrophe_proxy_hybrid_su_t1980, observed).
narrative_ontology:measurement(catastrophe_proxy_hybrid_su_t1992, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 1992, 0.44).
narrative_ontology:measurement_basis(catastrophe_proxy_hybrid_su_t1992, observed).
narrative_ontology:measurement(catastrophe_proxy_hybrid_su_t2004, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 2004, 0.51).
narrative_ontology:measurement_basis(catastrophe_proxy_hybrid_su_t2004, observed).
narrative_ontology:measurement(catastrophe_proxy_hybrid_su_t2016, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 2016, 0.55).
narrative_ontology:measurement_basis(catastrophe_proxy_hybrid_su_t2016, observed).
narrative_ontology:measurement(catastrophe_proxy_hybrid_su_t2024, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 2024, 0.58).
narrative_ontology:measurement_basis(catastrophe_proxy_hybrid_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__hybrid_degradation_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency__catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency__simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% The colloquial question 'can simulation replace real catastrophes for competence maintenance?' decomposes into four structurally distinct claims, each with its own epsilon, beneficiary/victim structure, and classification: this file instantiates the hybrid_degradation_reading only; the proxy-sufficiency, catastrophe-necessity, and fidelity-threshold readings are separate files. Edges here are constraint-family links documenting the decomposition, not contamination claims. Epsilon differs across the family because the referent differs — what simulation must suffice FOR — not because any reading measures the same arrangement with a different observable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__hybrid_degradation_reading, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
