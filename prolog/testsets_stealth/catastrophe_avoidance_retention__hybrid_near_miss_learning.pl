% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__hybrid_near_miss_learning
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: catastrophe_avoidance_retention__hybrid_near_miss_learning
 *   human_readable: Hybrid Near-Miss Learning Regime for Catastrophe-Avoidance Competence
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   The standing arrangement under contest is the institutionalized hybrid
 *   learning regime of high-hazard industries, paradigmatically aviation:
 *   confidential near-miss reporting channels (ASRS-style), mandatory
 *   occurrence reporting, flight-data monitoring pooled across fleets,
 *   line-operations safety audits, recurrent full-mission simulation,
 *   structured study of other operators' and other countries' accidents, and
 *   manufacturer service-difficulty loops that convert reported events into
 *   fleet-wide directives. The regime's claim is that no single organization
 *   experiences enough rare failures to learn from, so competence must be
 *   assembled from everyone's near-misses, everyone else's catastrophes, and
 *   synthetic practice calibrated against both. Its sharpest contrast case is
 *   medicine, where malpractice liability and discipline exposure seal error
 *   data inside institutions and the pooled-learning loop never closes. This
 *   file instantiates ONE reading of the catastrophe_avoidance_retention
 *   kernel (hybrid_near_miss_learning); the sibling readings are separate
 *   constraints with separate epsilon values: simulation_as_proxy_catastrophe
 *   authors epsilon for drill-mandate arrangements,
 *   catastrophe_as_necessary_selector authors epsilon for post-disaster
 *   reform cycles, and this story authors epsilon for the standing hybrid
 *   regime itself, assessed by this reading's own lights. The claimed type
 *   and the metrics are authored independently: the type from the structure
 *   (a real pooling function plus real contributor-side burdens held together
 *   by active enforcement), the metrics from the regime's observed operation.
 *
 * KEY AGENTS:
 *   - aviation_regulators: agenda-setter (institutional/constrained) — operates mandatory reporting frameworks, audits, and the investigative apparatus; cannot exit its statutory mandate
 *   - frontline_crews_and_controllers: primary contributor-payer (organized/constrained) — files the near-miss reports, flies the recurrent drills, carries residual legal exposure when confidentiality fails
 *   - major_network_carriers: principal data beneficiary (powerful/mobile) — largest fleets return the most value from pooled trends; funds the compliance apparatus
 *   - regional_affiliate_carriers: cost-bearing affiliate (moderate/trapped) — absorbs compliance weight against thin margins, receives proportionally less analytic return
 *   - airframe_manufacturers: design-loop beneficiary (institutional/arbitrage) — converts de-identified incident data into service bulletins and next-generation designs; sells globally
 *   - fare_paying_public: diffuse safety beneficiary (powerless/constrained) — travels under the residual risk the loop sustains; holds no governance seat
 *   - insurers_underwriters: market-side enforcer-beneficiary (institutional/mobile) — prices pooled loss data and conditions premiums on demonstrated compliance
 *   - low_sharing_industry_clinicians: excluded counterfactual seat (powerful/identity_locked) — competent, exposed, and structurally outside any shared pool
 *   - safety_science_community: analytical observer (analytical/analytical) — supplies the cross-industry comparisons the regime's justification rests on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.34).
domain_priors:suppression_score(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.42).
domain_priors:theater_ratio(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.37).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, extractiveness, 0.34).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0.37).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__hybrid_near_miss_learning, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__hybrid_near_miss_learning, "Hybrid Near-Miss Learning Regime for Catastrophe-Avoidance Competence").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__hybrid_near_miss_learning, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__hybrid_near_miss_learning).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__hybrid_near_miss_learning, '55400b4c-7b8c-42a2-adfa-683b473a7a1c').
narrative_ontology:cs_kernel_codification('55400b4c-7b8c-42a2-adfa-683b473a7a1c', distributed).
narrative_ontology:cs_authority_grounding('55400b4c-7b8c-42a2-adfa-683b473a7a1c', expertise).
narrative_ontology:cs_interpretation_layer_present('55400b4c-7b8c-42a2-adfa-683b473a7a1c').
narrative_ontology:cs_reading_relation('55400b4c-7b8c-42a2-adfa-683b473a7a1c', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, coexists_with).
narrative_ontology:cs_reading_relation('55400b4c-7b8c-42a2-adfa-683b473a7a1c', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, forecloses).
narrative_ontology:cs_axiom('55400b4c-7b8c-42a2-adfa-683b473a7a1c', foundational, rare_event_competence_requires_cross_organizational_pooling).
narrative_ontology:cs_axiom_status(rare_event_competence_requires_cross_organizational_pooling, holdable).
narrative_ontology:cs_axiom_grounding('55400b4c-7b8c-42a2-adfa-683b473a7a1c', rare_event_competence_requires_cross_organizational_pooling, empirically_contingent).
narrative_ontology:cs_axiom('55400b4c-7b8c-42a2-adfa-683b473a7a1c', foundational, real_incident_signal_irreducible_to_simulation).
narrative_ontology:cs_axiom_status(real_incident_signal_irreducible_to_simulation, holdable).
narrative_ontology:cs_axiom_grounding('55400b4c-7b8c-42a2-adfa-683b473a7a1c', real_incident_signal_irreducible_to_simulation, empirically_contingent).
narrative_ontology:cs_reference_frame('55400b4c-7b8c-42a2-adfa-683b473a7a1c', distributed_near_miss_learning_loop).
narrative_ontology:cs_drift_state('55400b4c-7b8c-42a2-adfa-683b473a7a1c', contemporary_compliance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('55400b4c-7b8c-42a2-adfa-683b473a7a1c', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, fare_paying_public).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, aviation_regulators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, airframe_manufacturers).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, major_network_carriers).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, insurers_underwriters).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_crews_and_controllers).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, regional_affiliate_carriers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_crews_and_controllers).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, major_network_carriers).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__hybrid_near_miss_learning, just_culture_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__hybrid_near_miss_learning, high_reliability_organization_theory).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__hybrid_near_miss_learning, near_miss_predictive_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the mandatory occurrence-reporting frameworks, certify training programs, audit operator safety-management systems, and run the investigative apparatus that turns reported events into binding directives. Fund and host confidential reporting channels. Their discretion is over the intensity and scope of the regime, not its existence: treaty obligations and statutory duties collapse if the reporting mandate is abandoned.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, aviation_regulators, agenda_setter,
    institutional, generational, constrained, global).

% File the near-miss reports, fly the recurrent simulator sessions, submit to line checks, and supply the raw material of the learning loop. Carry residual legal and career exposure when reports leak into disciplinary or litigation contexts despite confidentiality undertakings, and absorb the schedule burden of recurring drills. Union agreements secure just-culture clauses; leaving the profession forfeits licensed careers built over decades.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_crews_and_controllers, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_crews_and_controllers, beneficiary).

% Operate the largest fleets, so pooled incident data and fleet-wide trend analysis return the most value to them; they also fund flight-data-monitoring programs, audit overhead, and drill hours at scale. Can shift registrations between jurisdictions or restructure alliance commitments when rules bind too tightly, though network contracts and slot portfolios anchor them in place.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, major_network_carriers, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, major_network_carriers, payer).

% Fly feeder routes under major-carrier brands, absorbing safety-management and training compliance costs that weigh far heavier against thin margins, while receiving proportionally less analytic value from pooled data than the majors who aggregate it. Code-share dependence, pilot-pipeline contracts, and brand requirements leave little room to withdraw from the shared framework.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, regional_affiliate_carriers, payer,
    moderate, biographical, trapped, regional).

% Receive de-identified incident and service-difficulty data that feeds design fixes, service bulletins, and next-generation programs. Sell into every jurisdiction, so they can prioritize markets with lighter reporting demands when domestic regimes tighten, and negotiate data-access terms from a position few counterparties match.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, airframe_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% Travel under whatever residual-risk level the learning loop sustains. Have no seat in reporting-governance bodies and no practical ability to evaluate operator safety cultures, choosing among carriers on price and schedule while the safety margin is produced invisibly above them.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, fare_paying_public, beneficiary,
    powerless, biographical, constrained, global).

% Price hull and liability cover using pooled loss data, condition premiums on demonstrated training and reporting compliance, and thereby push operators toward the shared framework without holding any statutory mandate. Can redeploy capital across lines and jurisdictions if aviation risk pricing turns unattractive.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, insurers_underwriters, beneficiary,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, insurers_underwriters, agenda_setter).

% Work in a sector where malpractice discovery rules and professional-discipline exposure punish the disclosure of near-misses, so error data stays sealed inside institutions and the pooled-learning loop never closes. Professional identity ties speaking up to personal culpability rather than system learning. Would contribute to and benefit from an aviation-style pool, but sit outside it, and leaving the profession is not a live option for careers decades deep.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, low_sharing_industry_clinicians, excluded,
    powerful, biographical, identity_locked, national).

% Studies which industries retain rare-event competence and why, publishes the cross-industry comparisons the regime's justification rests on, and owes no operational stake to any answer. Supplies the external corroboration that distinguishes the regime's self-description from its performance.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_science_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__hybrid_near_miss_learning, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__hybrid_near_miss_learning, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools statistically rare operational events across organizational boundaries so that no single operator must suffer a catastrophe to learn; standardizes investigation, anonymization, and dissemination so that one actor's near-miss becomes every actor's procedure change before it becomes anyone's second occurrence.
% TRANSFER_FUNCTION: Moves incident narratives, flight-data trends, and drill findings from frontline reporters and individual operators into shared databases, regulatory directives, and manufacturer design loops; moves drill hours, reporting labor, and compliance spending from crews and operators into the system; moves advisories, procedure changes, and hardware fixes back out to entire fleets regardless of who contributed the triggering event.
% ABSENT_VOICES: Clinicians in low-reporting sectors are kept out by malpractice liability and discipline exposure and would contest the regime's boundaries; frontline reporters in blame-oriented organizations decline to speak and would contest its just-culture guarantees; passengers hold no seat in any reporting-governance body and would contest the allocation of the safety margin they underwrite with their presence.
% DISAPPEARANCE_RATIONALE: Overnight loss of the sharing networks would strand every operator with its own thin event sample: near-miss lessons would stop crossing organizational borders, drill curricula would lose their real-world calibration, and each jurisdiction would drift toward whichever substitute it favors — simulation-only certification or relearning through its own fatalities. Fleet-wide procedure change would again await body-count thresholds, and the aviation/medicine gap in retained competence would invert or vanish depending on which substitute each sector picked.
% FOUNDING_PROBLEM: Jet-age accident sequences showed that the events worth learning from are too rare and too expensive for any single organization to accumulate: an operator could fly for decades between instructive failures, and waiting for its own catastrophe was itself the failure mode the arrangement was built to abolish.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: statutory accident-investigation boards, whose independence is mandated, repeatedly attribute accident chains to lessons that existed elsewhere in the system but never crossed over; independent safety-science literature documents the cross-industry contrast in retained competence; public-health research records the repeat-harm patterns of sectors lacking such pools. Regulator and carrier attestations are treated as interested testimony and weighted accordingly.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__hybrid_near_miss_learning, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__hybrid_near_miss_learning, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.34, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness sits at 0.34: the regime takes real things from real people — reporting labor, drill hours, compliance spending, and residual legal exposure when anonymity fails — but most of what it takes is recycled into the pool the contributors draw on, so the take is closer to coordination cost than rent. Suppression at 0.42 reflects a regime that must be actively held together: mandatory occurrence reporting with penalties, insurer premium conditions, audit regimes, and just-culture rules that must be defended continuously against blame pressure after every visible accident. Alternatives are not collapsed (0.28): internal-only learning, simulation-heavy programs, and wait-for-the-accident postures all remain live, which is precisely why the kernel is contested. Resistance at 0.52 is the observable friction of the regime: reporting hesitancy, data-hoarding between competitors, drill cynicism, and periodic refusal to share after litigation threats. Theater at 0.37 is the compliance layer's growing share — checkbox drills, SMS paperwork, ritual reports — real but not yet dominant. The temporal series run on one shared seven-point grid (every tracked metric authored at every point) so no end-state value is silently substituted into earlier rows. The series are monotonic rather than oscillatory because the salience cycle (post-accident surge, then relaxation) averages out on an eight-year grid; the cycle is documented in the omegas and commentary rather than faked with sawtooth values. The suppression_requirement series is authored deliberately: the story specifically tracks enforcement build-up from voluntary, lightweight reporting toward mandated, audited, penalty-backed participation, which is an enforcement-capacity trajectory, not a static picture.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the finding. From the regulator and manufacturer seats the regime is a functioning instrument they operate and feed: compliance metrics are green, advisories flow, and the burden is invisible from above. From the crew seat the same structure is a standing obligation with a tail risk — every report is a gift to the system that could become an exhibit in a courtroom or a disciplinary file, and every recurrent drill is a scheduled reminder of what is expected. From the regional-affiliate seat it is a fixed cost scaled to someone else's fleet size. From the excluded clinician seat it is a visible alternative world: the same professionals, facing the same rare failures, without the pool. The engine computes these per-seat classifications from the structural data; nothing in the claimed type adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: fare_paying_public, aviation_regulators, airframe_manufacturers, major_network_carriers, insurers_underwriters — each collects from the pooling function (safety margin, system visibility, design input, analytic return, priced risk) without supplying the raw incident signal themselves. Victims declared: frontline_crews_and_controllers and regional_affiliate_carriers — the parties through whom the regime's costs are borne: reporting labor and exposure for the former, disproportionate compliance weight for the latter. The derivation places beneficiaries near the subsidy end and victims near the target end, with one correction: the sole organized seat (unionized frontline crews) is victim-declared, which alone would drive its directionality toward full target, but negotiated just-culture protections, direct safety returns from the pool they feed, and union standing place their net position only modestly past symmetric — hence the directionality override for the organized power atom at 0.60. Exit structure differentiates otherwise similar actors: major carriers are mobile (flag-shopping, alliance restructuring) where regional affiliates are trapped by code-share dependence; manufacturers hold arbitrage-grade exit into lighter jurisdictions; clinicians are identity_locked — exit from the profession dissolves a career and a self-concept built on it, which is why their exclusion persists despite their power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, not memorial: rare events stay rare, and each technology generation manufactures new ones (automation surprise, energy-dense battery cargo, disrupted operations at scale) that no operator's own event base will teach. Mandatrophy is therefore not resolved and no sunset is declared. The classification work this story performs is double-edged: reading the regime as pure coordination erases the documented contributor-side asymmetries (reporter exposure, affiliate cost weight) that its enforcement machinery exists to hold in place; reading it as pure extraction erases the pooling function that no current alternative replicates and that the excluded sectors demonstrably lack. The tangled_rope claim holds both halves in one structure. The R5 mismatch consumer should find no zombie signature here: founding_problem_status is live and disappearance_verdict is world_rearranges, so the arrangement's persistence tracks its function rather than outliving it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates only the hybrid_near_miss_learning reading of the catastrophe_avoidance_retention kernel: is distributed near-miss, foreign-incident, and high-realism-drill learning the load-bearing mechanism of competence retention, or would the sibling readings (simulation_as_proxy_catastrophe: high-fidelity drills alone suffice; catastrophe_as_necessary_selector: only actual catastrophes supply the selection pressure) describe the same retention record equally well?',
    'Cross-industry retention studies comparing sectors matched on capital depth and regulatory capacity but differing on incident-sharing density; adoption discontinuities where a sector stands up an ASRS-style pool and its subsequent rare-event trajectory is tracked.',
    'If simulation alone suffices, this regime''s reporting and enforcement overhead is excess coordination cost and the simulation sibling inherits the classification; if catastrophe selection is necessary, this regime merely delays decay and its contributors are funding delay rather than retention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of the catastrophe_avoidance_retention kernel; the disagreement is located in which mechanism (pooled real-world signal vs. synthetic practice vs. mortality salience) actually retains rare-event competence.').

omega_variable(
    cross_industry_causal_attribution,
    'Does dense incident-sharing cause superior rare-event competence, or do both co-vary with third factors (capital depth, regulatory capacity, fleet homogeneity) that independently maintain competence?',
    'Matched-pair comparisons of comparable operators inside and outside sharing pools; natural experiments where pools expand membership mid-period.',
    'If confounded, the regime''s coordination claim weakens and its enforcement skeleton carries more of the classification weight, shifting the computed type toward enforcement-led extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_industry_causal_attribution, empirical, 'Whether the aviation-success/medicine-failure contrast isolates incident-sharing as the operative variable.').

omega_variable(
    clinician_exclusion_barrier_type,
    'In low-sharing sectors such as medicine, is non-participation held up by structural barriers (malpractice discovery rules, discipline exposure) or internalized ones (professional identity equating error disclosure with personal culpability)?',
    'Jurisdictions that enacted statutory report-protection for clinicians: if reporting volume rises and persists after legal protection lands, barriers were structural; if it stays flat, they are internalized.',
    'Structural means the excluded seat is recoverable by legislation and the network is extensible; internalized means the regime does not transplant without identity-level change and the reading''s delta claim narrows to sectors already holding just-culture identities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clinician_exclusion_barrier_type, empirical, 'Structural vs. internalized nature of the barrier keeping the medicine counterfactual outside the pool.').

omega_variable(
    pooled_return_distribution,
    'Do frontline reporters and small affiliate carriers receive returns proportional to what they contribute, or do pooled-data gains concentrate among large carriers and manufacturers?',
    'Value-flow analysis tracing advisory uptake and fleet-tailored interventions by operator size; reporter surveys on perceived reciprocity between contribution and received protection.',
    'Concentration would sharpen the extraction asymmetry and push contributor seats toward target positions in per-seat computation; proportionality supports the coordination-dominant reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pooled_return_distribution, empirical, 'Symmetry of benefit distribution across contributors to the shared incident pool.').

omega_variable(
    drill_fidelity_threshold,
    'Above what fidelity and scenario unpredictability does a drill constitute genuine practice rather than rehearsed performance, and does the current recurrent-training mix clear that threshold?',
    'Transfer studies comparing crew performance on novel failure events between cohorts trained at differing simulator fidelity and scenario variability.',
    'If most mandated drills sit below threshold, the performative share of the regime is larger than measured and the simulation sibling reading gains ground on its own terms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(drill_fidelity_threshold, conceptual, 'Where the realism boundary between practice and rehearsal sits, and whether routine training clears it.').

omega_variable(
    goodhart_drift_displacement,
    'Is rising formalization (SMS paperwork, checkbox drills, reporting quotas) displacing functional learning, or accumulating alongside it?',
    'Compare reporting-volume and corrective-action closure curves against leading-indicator safety outcomes over the same window; interview safety officers on whether closed findings correspond to changed practice.',
    'Displacement would date a transition toward inertial maintenance with the function gone; accumulation supports continued coordination dominance of the regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(goodhart_drift_displacement, empirical, 'Whether the growing compliance layer substitutes for or supplements the underlying learning loop.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catav_hybrid_tr_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(catav_hybrid_tr_t0, observed).
narrative_ontology:measurement(catav_hybrid_tr_t8, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 8, 0.17).
narrative_ontology:measurement_basis(catav_hybrid_tr_t8, observed).
narrative_ontology:measurement(catav_hybrid_tr_t16, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 16, 0.21).
narrative_ontology:measurement_basis(catav_hybrid_tr_t16, observed).
narrative_ontology:measurement(catav_hybrid_tr_t24, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 24, 0.26).
narrative_ontology:measurement_basis(catav_hybrid_tr_t24, observed).
narrative_ontology:measurement(catav_hybrid_tr_t32, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 32, 0.3).
narrative_ontology:measurement_basis(catav_hybrid_tr_t32, observed).
narrative_ontology:measurement(catav_hybrid_tr_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 40, 0.34).
narrative_ontology:measurement_basis(catav_hybrid_tr_t40, observed).
narrative_ontology:measurement(catav_hybrid_tr_t48, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 48, 0.37).
narrative_ontology:measurement_basis(catav_hybrid_tr_t48, observed).

% Extraction over time
narrative_ontology:measurement(catav_hybrid_be_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(catav_hybrid_be_t0, observed).
narrative_ontology:measurement(catav_hybrid_be_t8, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 8, 0.23).
narrative_ontology:measurement_basis(catav_hybrid_be_t8, observed).
narrative_ontology:measurement(catav_hybrid_be_t16, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 16, 0.26).
narrative_ontology:measurement_basis(catav_hybrid_be_t16, observed).
narrative_ontology:measurement(catav_hybrid_be_t24, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 24, 0.29).
narrative_ontology:measurement_basis(catav_hybrid_be_t24, observed).
narrative_ontology:measurement(catav_hybrid_be_t32, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 32, 0.31).
narrative_ontology:measurement_basis(catav_hybrid_be_t32, observed).
narrative_ontology:measurement(catav_hybrid_be_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 40, 0.33).
narrative_ontology:measurement_basis(catav_hybrid_be_t40, observed).
narrative_ontology:measurement(catav_hybrid_be_t48, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 48, 0.34).
narrative_ontology:measurement_basis(catav_hybrid_be_t48, observed).

% Suppression requirement over time
narrative_ontology:measurement(catav_hybrid_su_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0, 0.26).
narrative_ontology:measurement_basis(catav_hybrid_su_t0, observed).
narrative_ontology:measurement(catav_hybrid_su_t8, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 8, 0.3).
narrative_ontology:measurement_basis(catav_hybrid_su_t8, observed).
narrative_ontology:measurement(catav_hybrid_su_t16, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 16, 0.33).
narrative_ontology:measurement_basis(catav_hybrid_su_t16, observed).
narrative_ontology:measurement(catav_hybrid_su_t24, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 24, 0.36).
narrative_ontology:measurement_basis(catav_hybrid_su_t24, observed).
narrative_ontology:measurement(catav_hybrid_su_t32, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 32, 0.39).
narrative_ontology:measurement_basis(catav_hybrid_su_t32, observed).
narrative_ontology:measurement(catav_hybrid_su_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 40, 0.41).
narrative_ontology:measurement_basis(catav_hybrid_su_t40, observed).
narrative_ontology:measurement(catav_hybrid_su_t48, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 48, 0.42).
narrative_ontology:measurement_basis(catav_hybrid_su_t48, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__hybrid_near_miss_learning, information_standard).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_as_necessary_selector).

% DUAL FORMULATION NOTE:
% Constraint family note: the colloquial question 'what keeps rare-event competence alive?' decomposes into three structurally distinct arrangements, not one constraint viewed from angles. This file instantiates hybrid_near_miss_learning and authors epsilon for the standing hybrid regime (moderate, coordination-dominated, contributor-burdened). simulation_as_proxy_catastrophe authors epsilon for drill-mandate and simulator-certification arrangements, where the contested element is whether synthetic practice transfers; catastrophe_as_necessary_selector authors epsilon for post-disaster reform cycles, where the contested element is whether only trauma-grade events reset organizational behavior. The epsilon values differ widely across the family because the arrangements differ; linking them via affects_constraints lets contamination analysis track, for example, whether rising drill theater in this regime strengthens the simulation sibling's claim or undermines it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_avoidance_retention__hybrid_near_miss_learning, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
