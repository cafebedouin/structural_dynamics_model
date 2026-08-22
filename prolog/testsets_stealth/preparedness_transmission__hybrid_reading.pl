% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__hybrid_reading, []).

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
 *   constraint_id: preparedness_transmission__hybrid_reading
 *   human_readable: Stratified Preparedness Transmission Regime (Hybrid Reading)
 *   domain: disaster risk management/institutional memory/civil defense
 *
 * SUMMARY:
 *   A statutory civil-defense and disaster-preparedness regime transmits
 *   catastrophe-response capability across generations through two channels:
 *   a physical-infrastructure channel (inspection cadres, licensed operators,
 *   apprenticeship pipelines for levees, grids, water, transport) and a
 *   civilian-coordination channel (evacuation drills, tabletop exercises,
 *   public campaigns). This story instantiates the HYBRID READING of the
 *   preparedness_transmission kernel: the two channels have diverged. The
 *   physical channel still transmits live competence — operators are
 *   exercised against real failure modes and fail visibly when incompetent.
 *   The civilian channel has decayed into completion-count performance:
 *   drills occur, reports accumulate, budgets follow, but the coordination
 *   knowledge the drills nominally transmit no longer reaches or takes root
 *   in the population, and the volunteer networks that once carried it have
 *   been crowded out. The expected structural delta is that infrastructure
 *   performs as designed while evacuation and coordination fail under stress
 *   — the break sits in the coordination layer, not the physical layer.
 *   Sibling readings (uniform liveness; uniform hollowing) are separate
 *   constraint files; this file authors only the hybrid reading's epsilon
 *   over the shared referent, the standing drill-and-inspection regime. Claim
 *   and metrics are independent authored facts: claimed_type tangled_rope is
 *   asserted from the structure (genuine coordination function plus
 *   asymmetric extraction plus active enforcement); the metric values
 *   describe the regime's observed operation.
 *
 * KEY AGENTS:
 *   - infrastructure_operating_agencies: agenda-setting administrator of the physical-transmission layer (institutional/constrained) — runs the inspection cadres and operator pipelines that demonstrably reproduce competence
 *   - licensed_engineering_professions: primary beneficiary of the functioning physical layer (organized/mobile) — collects standing, employment, and authority from live competence transmission
 *   - emergency_management_agencies: primary beneficiary and de facto agenda-setter of the civilian-facing layer (institutional/identity_locked) — collects budget and legitimacy from drill completion while transmission decays underneath
 *   - hazard_zone_residents: primary target (powerless/trapped) — bears unpriced coordination risk under formal coverage they cannot execute or exit
 *   - neighborhood_mutual_aid_networks: secondary target and structurally excluded voice (powerless/constrained) — crowded-out former carrier of the civilian coordination knowledge
 *   - post_disaster_inquiry_commissions: analytical observer (institutional/analytical) — documents the layer split after each failure without altering the incentive structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, 0.62).
domain_priors:suppression_score(preparedness_transmission__hybrid_reading, 0.48).
domain_priors:theater_ratio(preparedness_transmission__hybrid_reading, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, theater_ratio, 0.54).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_transmission__hybrid_reading, "Stratified Preparedness Transmission Regime (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_transmission__hybrid_reading, "disaster risk management/institutional memory/civil defense").

domain_priors:requires_active_enforcement(preparedness_transmission__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__hybrid_reading, 'b6027c1d-51e4-4db9-a9a5-310a7c2b3831').
narrative_ontology:cs_kernel_codification('b6027c1d-51e4-4db9-a9a5-310a7c2b3831', formalized).
narrative_ontology:cs_authority_grounding('b6027c1d-51e4-4db9-a9a5-310a7c2b3831', practice).
narrative_ontology:cs_interpretation_layer_present('b6027c1d-51e4-4db9-a9a5-310a7c2b3831').
narrative_ontology:cs_reading_relation('b6027c1d-51e4-4db9-a9a5-310a7c2b3831', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('b6027c1d-51e4-4db9-a9a5-310a7c2b3831', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_axiom('b6027c1d-51e4-4db9-a9a5-310a7c2b3831', foundational, layer_stratified_transmission).
narrative_ontology:cs_axiom_status(layer_stratified_transmission, holdable).
narrative_ontology:cs_axiom_grounding('b6027c1d-51e4-4db9-a9a5-310a7c2b3831', layer_stratified_transmission, empirically_contingent).
narrative_ontology:cs_axiom('b6027c1d-51e4-4db9-a9a5-310a7c2b3831', foundational, civilian_coordination_knowledge_decayed).
narrative_ontology:cs_axiom_status(civilian_coordination_knowledge_decayed, holdable).
narrative_ontology:cs_axiom_grounding('b6027c1d-51e4-4db9-a9a5-310a7c2b3831', civilian_coordination_knowledge_decayed, empirically_contingent).
narrative_ontology:cs_reference_frame('b6027c1d-51e4-4db9-a9a5-310a7c2b3831', full_spectrum_transmission_baseline).
narrative_ontology:cs_drift_state('b6027c1d-51e4-4db9-a9a5-310a7c2b3831', contemporary_post_failure_inquiry_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b6027c1d-51e4-4db9-a9a5-310a7c2b3831', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__hybrid_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, infrastructure_operating_agencies).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, licensed_engineering_professions).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, emergency_management_agencies).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, hazard_zone_residents).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, neighborhood_mutual_aid_networks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and operate levees, grids, water systems, and transport infrastructure; set inspection cadres and certify operator competence through apprenticeship and licensure pipelines. The physical-transmission layer they administer demonstrably reproduces competence: operators are exercised against real failure modes and pass or fail on hard performance criteria. Dissolving the pipeline would leave them unable to staff critical facilities, so they are bound into maintaining it.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, infrastructure_operating_agencies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__hybrid_reading, infrastructure_operating_agencies, beneficiary).

% Professional bodies and credentialed engineers whose competence is continuously re-validated through licensure renewal, peer review, and failure-driven revision of practice. They collect standing, employment, and epistemic authority from the functioning physical layer of the transmission regime, and their skills are portable across employers and jurisdictions.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, licensed_engineering_professions, beneficiary,
    organized, generational, mobile, global).

% Run the civilian-facing exercise calendar: evacuation drills, tabletop exercises, public-awareness campaigns. Budget lines and political credit attach to drill completion counts and after-action report volume rather than to measured civilian capability. Successive leadership cohorts have risen through the exercise bureaucracy, and the agency's self-concept is fused with running drills. Reorienting toward outcome-measured community coordination would invalidate the career ladder and the institutional identity built on it.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, emergency_management_agencies, beneficiary,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__hybrid_reading, emergency_management_agencies, agenda_setter).

% Live in floodplains, wildfire interfaces, and seismic zones under formal coverage of evacuation plans and periodic drills they may briefly attend or only hear announced. They carry the unpriced risk that the plans no longer correspond to anything they could actually execute under stress. Moving out of the hazard zone is economically blocked for most.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, hazard_zone_residents, payer,
    powerless, biographical, trapped, regional).

% Volunteer coordinators and block-level networks that once received civil-defense training, equipment caches, and formal integration with official response channels. Now largely unfunded and unacknowledged: the official exercise channel absorbed the legitimacy and budget that once flowed to them, and drill design does not seat them at the table. They continue operating on shrinking voluntary effort and would contest the official account of civilian readiness if given a forum.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, neighborhood_mutual_aid_networks, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__hybrid_reading, neighborhood_mutual_aid_networks, excluded).

% Convene after major failures, take testimony from every other seat, and document the gap between exercised procedure and realized coordination. Their reports feed back into exercise design and after-action templates without altering the completion-metric incentive structure that governs agency advancement.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, post_disaster_inquiry_commissions, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__hybrid_reading, emergency_management_agencies).
narrative_ontology:fixing_cost_class(preparedness_transmission__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a standing intergenerational pipeline that reproduces the competence to keep physical life-support systems running under catastrophic stress: inspection cadres, licensed operators, and exercised maintenance routines for levees, grids, water, and transport. On the civilian side it maintains a scheduled exercise calendar intended to reproduce evacuation and self-coordination capability in the population.
% TRANSFER_FUNCTION: Moves public preparedness budgets, staffing, and institutional legitimacy toward the agencies and professions that operate the transmission regime; moves the administrative record and public appearance of civilian readiness onto hazard-zone residents without moving corresponding coordination capability to them.
% ABSENT_VOICES: Neighborhood mutual aid networks and hazard-zone residents themselves — the people the civilian-coordination layer is nominally for — are not seated in drill design, exercise evaluation, or budget allocation. Post-disaster inquiry commissions speak for them only retrospectively, after the coordination failure has already been paid for in lives.
% DISAPPEARANCE_RATIONALE: If the transmission regime vanished overnight, the physical layer's absence would rearrange the world quickly: inspection cadres and operator pipelines would need rebuilding before infrastructure staffing gaps became failures, and utilities and public works would lose their competence-reproduction machinery. The civilian layer's absence would be quieter — the drill calendar and its credit economy would stop — but the formal preparedness apparatus that jurisdictions cite in land-use and insurance decisions would collapse with it.
% FOUNDING_PROBLEM: Mid-century civil defense was built to solve a two-part problem: how does a society preserve, across generations, both the capability to keep physical systems running under sudden catastrophe and the capability of ordinary civilians to coordinate their own movement out of harm's way?
% FOUNDING_PROBLEM_CORROBORATION: Post-disaster inquiry commissions — outside the benefiting parties — repeatedly attest that the civilian-coordination half of the founding problem is unresolved, documenting evacuation failures in which exercised procedures did not translate into executed movements. Engineering professional bodies, also outside the beneficiary set of the civilian layer, independently attest the physical half remains live through continuing-competence requirements and failure-driven standards revision. The arrangement's own agencies attest liveness via drill records; that attestation is discounted as self-interested.
narrative_ontology:disappearance_verdict(preparedness_transmission__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_transmission__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__hybrid_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_transmission__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.62 is authored for this reading's lights over the fixed referent (the standing regime): the physical layer genuinely delivers, which pulls the aggregate down from snare territory, while the civilian layer extracts assurance without capability, which pushes it well above rope territory. Suppression 0.48 is structural rather than overtly coercive: budget capture, a legitimacy monopoly over 'official' preparedness, liability and integration barriers facing independent networks, and a compliance-reporting ratchet — no one is forced to drill at gunpoint, but the alternative channels have been starved of the resources that legitimacy monopolies control. Theater_ratio 0.54 sits just past the Goodhart threshold, encoding the stratification: civilian-side exercise activity is majority-performative under completion-count incentives, physical-side inspection activity is majority-functional under hard failure feedback, and the resource-weighted aggregate crosses 0.5 late in the interval. Accessibility_collapse 0.42: alternatives (mutual aid, independent training organizations) persist and are knowable, but they are marginal — unfunded, unintegrated, and denied the official-channel legitimacy that determines whether residents treat them as real. Resistance 0.35: episodic rather than sustained — post-failure inquiry pressure, occasional local official pushback against unfunded mandates — because the concentrated beneficiaries defend the regime while the principal payers do not yet perceive the extraction. All three metric series run on one shared time grid (T=0..50, mapping approximately 1975..2025) with every metric authored at every point; the trajectories are monotonic drift, not oscillation — post-disaster attention spikes temporarily raise scrutiny but the completion-metric incentive structure reabsorbs each cycle, which is why no cyclical measurement pattern is authored. The suppression series is included because the story specifically traces enforcement-form change: demobilization of wartime-style mobilization authority (falling to t=10) followed by hardening of audit-and-compliance machinery around drill completion (rising thereafter) — a shift in enforcement form at roughly stable net intensity.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the stratification is precisely why. From the emergency_management_agencies seat, the arrangement is a functioning program the agency staffs, measures, and reports on — the drill calendar is its lived reality and its identity. From the hazard_zone_residents seat, the civilian layer operates as assurance-extraction: residents supply compliance presence and political cover and receive confidence that does not survive contact with an actual evacuation. From the licensed_engineering_professions seat, the same overall regime is genuine professional reproduction — their feedback loops are hard (structures fail, systems trip), so their experience of the regime is of a discipline that works. Two institutionally-powered actors at nominally equal standing (operating agencies versus emergency management) diverge because their verification environments differ: physical failure is immediate and undeniable, coordination failure is deferred and deniable until a catastrophe settles it. The inquiry commissions see the split from outside. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the three beneficiary seats: licensed_engineering_professions sit nearest the beneficiary pole (pure gain, mobile exit — arbitrage-grade portability of their competence), infrastructure_operating_agencies slightly less so (they collect standing but also bear the real cost of maintaining the pipeline), emergency_management_agencies low-d on paper but effectively the capturers of the civilian layer's gains. Victim declarations drive high directionality for the two victim seats: hazard_zone_residents sit nearest the target pole (trapped exit, full bearing of the unpriced risk, identity-neutral), neighborhood_mutual_aid_networks high but marginally discounted — they are crowded out and dispossessed rather than directly harvested, sitting partly outside the extraction loop they nonetheless pay for. The vindicated proposition infrastructure_reliability_doctrine collects no rents and is listed as vindicated, not as beneficiary: the regime's operation continuously validates the doctrine that engineered physical competence constitutes preparedness, which is exactly the belief that conceals the civilian-layer decay.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is split-brained under this reading, and the classification is chosen to prevent mislabeling in both directions. A pure-snare reading would erase the genuine engineering coordination function — levee inspections and operator pipelines are not ritual, and treating the whole regime as cover-story extraction would misdirect reform at the layer that works. A pure-rope reading would erase the civilian-layer extraction — treating drill completion as protection is precisely the error the inquiry record documents. Tangled_rope holds both facts: coordination that genuinely reproduces physical competence, extraction that converts civilian preparedness mandates into agency budget and legitimacy while the protected capability decays. The R5 interview sharpens this: founding_problem_status contested (physical half live, civilian half dead-in-practice) paired with disappearance_verdict world_rearranges correctly avoids the zombie flag — the arrangement still carries real load, unevenly distributed across its own layers. Watch item for downstream analysis: the theater_ratio series crosses 0.5 at approximately t=45, marking the civilian layer's Goodhart transition; if civilian-side extraction continues accumulating, per-seat computation should drift the civilian layer toward snare while the physical layer holds at rope — the stratification IS the seat divergence, made temporal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_underdetermination,
    'Does the drill-and-inspection record reflect stratified transmission (this hybrid reading), uniform hollowing (husk_reading), or uniform liveness (competence_reading)? This constraint is one reading of the preparedness_transmission kernel; the sibling readings instantiate different constraints with different epsilon values over the same referent.',
    'Layer-resolved outcome audits across the last N major activations: compare physical-layer performance (systems operated as designed, operators executed correctly) against civilian-coordination performance (evacuations executed as planned, self-coordination achieved) on the same events. A persistent split confirms this reading; uniform failure supports husk_reading; uniform success supports competence_reading.',
    'If husk_reading is correct, both layers reclassify toward snare/piton and this file''s epsilon materially understates extraction; if competence_reading is correct, the regime reclassifies toward rope and this file''s epsilon overstates extraction. The stratification claim itself is what routes the D5 break to the coordination layer rather than the physical layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_underdetermination, empirical, 'Which reading of the preparedness_transmission kernel the layer-resolved empirical record supports.').

omega_variable(
    transmission_break_vs_activation_break,
    'Is the civilian-layer failure a transmission break (coordination knowledge was never passed to the current generation of residents and volunteers) or an activation break (knowledge exists in pockets but cannot be mobilized under stress)?',
    'Instrumented full-scale evacuation exercises scored on civilian execution rather than attendance: pre-exercise knowledge sampling of participants distinguishes never-transmitted from transmitted-but-unmobilized.',
    'A transmission break calls for pipeline rebuilding (a scaffold-shaped remedy with a sunset); an activation break calls for command-and-control and mobilization reform, leaving the extraction assessment largely unchanged. The two diagnoses fund very different fixes and misdiagnosis wastes the reform window between catastrophes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_break_vs_activation_break, empirical, 'Whether the coordination-layer D5 break is in knowledge transmission or in stress activation.').

omega_variable(
    decay_cause_attribution,
    'Is the civilian-coordination decay an inevitable consequence of peacetime (skill fade without catastrophic feedback, structural) or a policy artifact of funding formulas, completion-metric incentives, and legitimacy concentration (constructed)?',
    'Cross-jurisdiction comparison where funding structures and outcome metrics differ: jurisdictions that fund outcome-measured community coordination against otherwise comparable completion-metric jurisdictions, controlling for hazard profile.',
    'If structural, the arrangement sits closer to rope-with-costs and the decay carries lower culpability weighting; if constructed, the extraction is produced by the regime''s own incentive design, supporting snare-leaning per-seat computation on the civilian layer and directing reform at the metric structure rather than at apathy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decay_cause_attribution, conceptual, 'Whether the coordination-knowledge decay is structural skill fade or regime-produced extraction.').

omega_variable(
    crowding_out_vs_neglect,
    'Does the official regime actively degrade independent civilian coordination (legitimacy monopoly, liability and insurance barriers, integration refusal) or merely fail to sustain it while decaying on its own?',
    'Historical reconstruction of mutual-aid network funding, training access, and official-integration timelines against the regime''s expansion; natural experiments where official channels collapsed and independent networks either recovered or did not.',
    'Active degradation raises the suppression attributable to the regime and increases effective extraction for resident and mutual-aid seats; passive neglect lowers it and shifts remedial responsibility toward reinvestment rather than dismantling barriers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(crowding_out_vs_neglect, empirical, 'Whether the regime suppresses alternatives or merely neglects them — the counterfactual baseline for civilian coordination capacity is genuinely hard to establish.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(preparedness_tx_hybrid_tr_t0, preparedness_transmission__hybrid_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(preparedness_tx_hybrid_tr_t10, preparedness_transmission__hybrid_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(preparedness_tx_hybrid_tr_t20, preparedness_transmission__hybrid_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(preparedness_tx_hybrid_tr_t30, preparedness_transmission__hybrid_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(preparedness_tx_hybrid_tr_t40, preparedness_transmission__hybrid_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(preparedness_tx_hybrid_tr_t50, preparedness_transmission__hybrid_reading, theater_ratio, 50, 0.54).

% Extraction over time
narrative_ontology:measurement(preparedness_tx_hybrid_be_t0, preparedness_transmission__hybrid_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(preparedness_tx_hybrid_be_t10, preparedness_transmission__hybrid_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(preparedness_tx_hybrid_be_t20, preparedness_transmission__hybrid_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(preparedness_tx_hybrid_be_t30, preparedness_transmission__hybrid_reading, base_extractiveness, 30, 0.51).
narrative_ontology:measurement(preparedness_tx_hybrid_be_t40, preparedness_transmission__hybrid_reading, base_extractiveness, 40, 0.57).
narrative_ontology:measurement(preparedness_tx_hybrid_be_t50, preparedness_transmission__hybrid_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(preparedness_tx_hybrid_su_t0, preparedness_transmission__hybrid_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(preparedness_tx_hybrid_su_t10, preparedness_transmission__hybrid_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(preparedness_tx_hybrid_su_t20, preparedness_transmission__hybrid_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(preparedness_tx_hybrid_su_t30, preparedness_transmission__hybrid_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement(preparedness_tx_hybrid_su_t40, preparedness_transmission__hybrid_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(preparedness_tx_hybrid_su_t50, preparedness_transmission__hybrid_reading, suppression_requirement, 50, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__competence_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'preparedness transmission' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle. This file is the hybrid reading (stratified: physical layer live, civilian layer decayed; epsilon 0.62; tangled_rope). preparedness_transmission__competence_reading authors the uniform-liveness claim (lower epsilon, rope-leaning); preparedness_transmission__husk_reading authors the uniform-hollowing claim (higher epsilon, snare/piton-leaning). Each reading is epsilon-invariant over the shared referent — the standing statutory drill-and-inspection regime — and the files are linked so drift and contamination analyses compare readings rather than averaging them. The upstream/downstream structure runs through the shared empirical record: whichever reading the layer-resolved activation audits support constrains the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
