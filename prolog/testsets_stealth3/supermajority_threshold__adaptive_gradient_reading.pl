% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__adaptive_gradient_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__adaptive_gradient_reading, []).

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
 *   constraint_id: supermajority_threshold__adaptive_gradient_reading
 *   human_readable: Supermajority Threshold as Calibratable Instrument (Adaptive Gradient Reading)
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   Constitutional orders fix a supermajority bar — two-thirds,
 *   three-quarters, concurrent bicameral passes — that constitutional
 *   amendments must clear. The bars were set at founding moments by the
 *   actors they protected, and almost no jurisdiction has ever retuned one
 *   against measured evidence about how fast social consensus actually forms
 *   or what a failed reversal would cost. This story instantiates the
 *   ADAPTIVE GRADIENT READING of the supermajority-threshold kernel: on this
 *   reading the threshold is not intrinsically sacred or inherently predatory
 *   — it is a functional instrument whose legitimacy is contingent on
 *   calibration to consensus-formation rates and reversibility costs.
 *   Assessed by that reading's own lights, the standing arrangement shows a
 *   genuine filtering function still operating (changes backed by thin,
 *   transient support do fail) alongside accumulating miscalibration (durably
 *   supported changes fail too, and the blocking share converts into veto
 *   rent). The epsilon referent is the standing arrangement — the actual
 *   thresholds as they operate — never the evidence-tuned regime this reading
 *   would install. Sibling readings (consensus-safeguard, minoritarian-veto)
 *   are separate constraint files sharing the kernel; they are not folded
 *   into this classification. The claim/metric gap is deliberate: the type is
 *   claimed from structure (hybrid coordination-plus-extraction), the metrics
 *   are authored from descriptive operation, and the engine computes per-seat
 *   classifications independently.
 *
 * KEY AGENTS:
 *   - amendment_gatekeeping_institutions: Agenda-setting administrator (institutional/constrained) — counts, certifies, and defends the procedure but cannot reset its value without meeting it
 *   - status_quo_blocking_minorities: Primary beneficiary (organized/constrained) — converts blocking-share size into full veto over any opposed proposal
 *   - long_horizon_constitutional_stakeholders: Secondary beneficiary (powerful/arbitrage) — collects predictability rents; capital retains jurisdictional exit
 *   - durable_reform_coalitions: Primary target (organized/trapped) — sustains cross-cycle majority support yet cannot clear the bar; no alternate route exists
 *   - general_electorate: Dual-positioned beneficiary/payer (organized/constrained) — purchases stability, pays in blocked durable reforms
 *   - future_citizens: Excluded bearer (powerless/trapped) — inherits the frozen settlement with no seat in any count
 *   - institutional_design_scholars: Analytical observer (analytical/analytical) — measures the calibration gap, holds no vote
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__adaptive_gradient_reading, 0.61).
domain_priors:suppression_score(supermajority_threshold__adaptive_gradient_reading, 0.48).
domain_priors:theater_ratio(supermajority_threshold__adaptive_gradient_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__adaptive_gradient_reading, tangled_rope).
narrative_ontology:human_readable(supermajority_threshold__adaptive_gradient_reading, "Supermajority Threshold as Calibratable Instrument (Adaptive Gradient Reading)").
narrative_ontology:topic_domain(supermajority_threshold__adaptive_gradient_reading, "political/constitutional").

domain_priors:requires_active_enforcement(supermajority_threshold__adaptive_gradient_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__adaptive_gradient_reading, '997669c1-7074-4a79-8db2-de0f75ae30bb').
narrative_ontology:cs_kernel_codification('997669c1-7074-4a79-8db2-de0f75ae30bb', formalized).
narrative_ontology:cs_authority_grounding('997669c1-7074-4a79-8db2-de0f75ae30bb', lineage).
narrative_ontology:cs_interpretation_layer_present('997669c1-7074-4a79-8db2-de0f75ae30bb').
narrative_ontology:cs_reading_relation('997669c1-7074-4a79-8db2-de0f75ae30bb', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('997669c1-7074-4a79-8db2-de0f75ae30bb', supermajority_threshold__minoritarian_veto_reading, influences).
narrative_ontology:cs_axiom('997669c1-7074-4a79-8db2-de0f75ae30bb', foundational, threshold_legitimacy_is_performance_contingent).
narrative_ontology:cs_axiom_status(threshold_legitimacy_is_performance_contingent, holdable).
narrative_ontology:cs_axiom_grounding('997669c1-7074-4a79-8db2-de0f75ae30bb', threshold_legitimacy_is_performance_contingent, empirically_contingent).
narrative_ontology:cs_axiom('997669c1-7074-4a79-8db2-de0f75ae30bb', secondary, reversibility_costs_warrant_domain_variable_thresholds).
narrative_ontology:cs_axiom_status(reversibility_costs_warrant_domain_variable_thresholds, holdable).
narrative_ontology:cs_axiom_grounding('997669c1-7074-4a79-8db2-de0f75ae30bb', reversibility_costs_warrant_domain_variable_thresholds, instrumental).
narrative_ontology:cs_reference_frame('997669c1-7074-4a79-8db2-de0f75ae30bb', calibration_parity_with_consensus_formation).
narrative_ontology:cs_drift_state('997669c1-7074-4a79-8db2-de0f75ae30bb', contemporary_unaudited_threshold_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('997669c1-7074-4a79-8db2-de0f75ae30bb', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, status_quo_blocking_minorities).
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, long_horizon_constitutional_stakeholders).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, durable_reform_coalitions).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, future_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, general_electorate).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, general_electorate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Presiding officers, electoral commissions, and courts that count and certify supermajority votes, adjudicate validity disputes (what counts in the denominator, how abstentions are treated), and defend the amendment procedure against challenge. They administer the threshold daily but did not choose its value, and they cannot lower it without first assembling the very supermajority it demands.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, amendment_gatekeeping_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Regionally concentrated blocs, incumbent factions, or constitutionally privileged groups holding just over the blocking share. They need not agree on any affirmative program — only on stopping each proposed change. The threshold converts their minority size into full veto power over any constitutional proposal they oppose, and their position requires no effort beyond continued cohesion.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, status_quo_blocking_minorities, beneficiary,
    organized, generational, constrained, regional).

% Creditors, investors, and firms whose planning depends on the constitutional framework staying put. They collect predictability rents from the frozen settlement and hold an exit the voters lack: capital can relocate to jurisdictions whose frameworks remain favorable, repricing any polity whose threshold regime turns hostile to their positions.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, long_horizon_constitutional_stakeholders, beneficiary,
    powerful, generational, arbitrage, global).

% Cross-party and movement coalitions whose proposals command sustained majority support across multiple electoral cycles yet cannot clear the supermajority bar. There is no alternative institutional route to constitutional-level change; repeated near-misses consume their organizational resources and public credibility while the blocked demand persists.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, durable_reform_coalitions, payer,
    organized, biographical, trapped, national).

% Citizens receive predictability — the rules of political competition will not be rewritten after any single election — while bearing the deferred costs of reforms their own durable majorities cannot enact. Their principal lever is voice inside the same gated procedure; emigration is the only exit and is costly and selective.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, general_electorate, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__adaptive_gradient_reading, general_electorate, payer).

% People not yet enfranchised who will live under whatever constitutional settlement the current threshold freezes in place. They bear the ossification costs — outdated protections, unreformed structures, entitlements sized for a vanished demography — with no seat in any count held today.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, future_citizens, excluded,
    powerless, civilizational, trapped, national).

% Comparative constitutionalists and institutional economists who measure proposal persistence, amendment difficulty, and reversibility costs across jurisdictions, publish calibration analyses, and advise redesign commissions. They hold no vote in any amending body and no enforcement role; their influence runs entirely through argument and advisory access.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, institutional_design_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__adaptive_gradient_reading, status_quo_blocking_minorities).
narrative_ontology:fixing_cost_class(supermajority_threshold__adaptive_gradient_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Filters constitutional proposals so that only changes backed by unusually broad and durable support pass, protecting the constitutional framework from revision by transient majorities and giving long-horizon actors confidence that the rules of political competition will not shift beneath them.
% TRANSFER_FUNCTION: Moves constitutional veto power and agenda-setting leverage toward whichever coalition can reliably field the blocking share — typically geographically concentrated or institutionally entrenched minorities — and moves the cost of blocked change onto the coalitions and constituencies seeking it, with the deferred residue carried by the not-yet-enfranchised.
% ABSENT_VOICES: Future citizens who will inherit the entrenched settlement, and the constituencies of durably blocked reforms, are not seated anywhere in the calibration conversation. Threshold values were fixed at founding moments by the very actors they protected, and no standing body re-examines them against measured consensus-formation data; the scholars who produce such data hold advisory voice only.
% DISAPPEARANCE_RATIONALE: If the supermajority gate vanished overnight, constitutional amendment would track simple majorities: blocking minorities would lose their veto instantly, the backlog of durably supported but long-blocked reforms would pass in waves, long-horizon capital would reprice jurisdiction risk, and polities with genuinely thin consensus would face elevated constitutional churn. Arrangements across the whole political economy depend on the freeze the threshold maintains.
% FOUNDING_PROBLEM: Founding-generation fear that narrow, transient majorities would opportunistically rewrite the constitutional framework — expropriating property holders, dismantling religious or federal settlements, or converting temporary electoral luck into permanent structural advantage.
% FOUNDING_PROBLEM_CORROBORATION: Founding-debate records and ratification correspondence corroborate the original problem from outside any current beneficiary set. On its present status, comparative constitutional scholarship and reform-commission inquiries attest that transient-majority opportunism remains possible in some domains while ossification and veto capture now dominate in others — but no standing official body audits any threshold against measured consensus data, and the gatekeeping institutions themselves attest only that the original danger justifies the bar as set.
narrative_ontology:disappearance_verdict(supermajority_threshold__adaptive_gradient_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__adaptive_gradient_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__adaptive_gradient_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(supermajority_threshold__adaptive_gradient_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__adaptive_gradient_reading, 0.61, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__adaptive_gradient_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__adaptive_gradient_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__adaptive_gradient_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.61 because the standing arrangement's costs are real but bounded: the filtering function still removes genuinely transient proposals (a coordination service), while the miscalibration residue — durably supported reforms blocked, veto rents accruing to blocking shares — grows as societies change faster than their fixed bars. Suppression is 0.48 and is authored as a RAW structural property, unscaled: enforcement is procedural (counting rules, validity adjudication) rather than coercive elimination of alternatives, and ordinary legislation below the line remains available. Theater_ratio 0.32 reflects growing ceremonial weight — solemn joint sessions, anniversary conventions, the threshold as a symbol of seriousness — as the filtering function degrades relative to its founding-era load. Accessibility_collapse 0.52: understanding the threshold does not collapse alternatives generally (normal politics continues below the line), but for constitutionally-gated changes the collapse is near-total, averaging to moderate. Resistance 0.55: reform movements campaign against the bars, scholars publish recalibration cases, and some jurisdictions have adjusted related procedures — real but uncoordinated friction. The three measurement series run on ONE shared grid (points 0-60 step 10) so every metric is authored at every examined time point; the rising base_extractiveness trajectory models ossification accumulation, the rising theater trajectory models Goodhart drift of the solemnity ritual, and the mildly rising suppression_requirement tracks the documented intensification of judicial and procedural enforcement of amendment validity — an enforcement-capacity dynamic, not merely a shift in extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the blocking-minority seat the threshold is guardianship: their veto is experienced as the constitution working as designed, and their constrained exit is irrelevant because the arrangement protects them in place. From the durable-reform-coalition seat the identical structure is a wall: trapped exit, no alternate route, resources burned on near-misses. The gatekeeping institutions experience administration — procedure, not stakes. Long-horizon capital experiences cheap insurance, softened further by its arbitrage exit. The electorate straddles the line, buying stability with blocked-reform costs it only partially attributes to the threshold. The engine derives these divergent classifications from the declared power, exit, and role data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries drive low directionality: long_horizon_constitutional_stakeholders combine beneficiary position with arbitrage-grade exit, placing them nearest the full-beneficiary end; status_quo_blocking_minorities are beneficiaries whose advantage exists only inside the arrangement, so their constrained exit keeps them low-d but not at the floor. Declared victims drive high directionality: durable_reform_coalitions are trapped targets bearing the transfer directly; future_citizens are powerless and fully trapped, sitting at the full-target end with zero compensating benefit. The general electorate's dual declaration (beneficiary with payer secondary role) places it near symmetric — genuine coordination benefit, diffuse indirect cost. The gatekeeping institutions carry no beneficiary or victim declaration and resolve through the canonical fallback, which is appropriate for an administrator that collects no rents from the rate itself. Effective extraction is amplified for the trapped targets and damped for the arbitrage-capable beneficiary; scope amplification applies modestly at national scale.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's classification discipline cuts both ways. Against the rope mislabel: a pure-coordination verdict would erase the documented veto rents and the rising extraction trajectory — the tangled-rope structure keeps the asymmetric-transfer half visible. Against the snare mislabel: a pure-extraction verdict would erase the filtering function that still disqualifies transient proposals and the stability service long-horizon actors genuinely purchase. On mandatrophy proper: the founding problem (transient-majority opportunism) is contested rather than dead — it remains live in some domains while ossification dominates others — so the arrangement has NOT outlived its function wholesale and mandatrophy_resolved is deliberately not declared. The rising theater_ratio signals partial ritualization (solemnity persisting as the filter degrades) but the cost asymmetry has not reached piton shape: the gatekeepers could in principle change the bar, and the blocked coalitions are hurt enough to keep trying. The classification therefore resists both the flattering origin myth (pure safeguard) and the cynical one (pure veto machine).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Which reading of the supermajority-threshold kernel correctly identifies the standing arrangement''s structural nature — calibratable instrument (this file), intrinsic consensus safeguard, or entrenched veto machine?',
    'Compare the engine''s computed classifications across the three reading stories sharing kernel_id supermajority_threshold; convergent types indicate the readings index the same structure, divergent types locate the disagreement structurally in the differing beneficiary/victim and exit declarations.',
    'If the safeguard reading dominates, effective extraction falls (protection value outweighs blockage cost); if the veto reading dominates, extraction rises and the victim set widens to all majoritarian preferences. This story''s tangled-rope claim and mid-range epsilon are indexed to the adaptive-gradient seat specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame omega: this constraint is one reading of the supermajority_threshold kernel; sibling readings instantiate different constraints over the same referent.').

omega_variable(
    consensus_rate_measurability,
    'Can social consensus formation rates and reversibility costs be measured precisely enough to ground threshold tuning, or does the calibration criterion collapse into discretionary judgment dressed as evidence?',
    'Pilot calibration studies: retrospective fit of threshold levels to measured proposal-persistence curves across jurisdictions, followed by out-of-sample predictive tests of tuned-versus-fixed bars.',
    'If consensus dynamics are effectively unmeasurable, this reading''s legitimacy standard reduces to convention and the constraint reverts toward the safeguard reading''s footing; if measurable, evidence-based tuning becomes actionable and the standing arrangement''s misalignment becomes a quantifiable extraction figure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_rate_measurability, empirical, 'Whether the adaptive reading''s foundational axiom is empirically satisfiable at all.').

omega_variable(
    net_miscalibration_direction,
    'Is the standing arrangement net over-calibrated (ossification and veto extraction dominate) or net under-calibrated (instability exposure dominates) once all governed decision domains are aggregated?',
    'Domain-by-domain audit comparing actual threshold levels against measured consensus persistence and reversal costs, weighted by decision frequency and materiality.',
    'Net over-calibration supports the elevated extraction estimate and drift toward the snare pole; net under-calibration would lower effective extraction and strengthen the rope component, reversing the authored trajectory''s interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_miscalibration_direction, empirical, 'Direction of aggregate calibration error in the standing arrangement.').

omega_variable(
    uniform_threshold_domain_mismatch,
    'Can any single threshold value be simultaneously calibrated across decision domains with sharply different reversibility costs, or is uniform application itself the miscalibration?',
    'Comparative analysis of jurisdictions employing domain-variable thresholds (distinct bars for territorial, procedural, and rights-amendment classes) against uniform-threshold peers on stability and reform-throughput outcomes.',
    'If heterogeneity is irreducible, part of the measured extraction is attributable to the uniform-rule form rather than any particular value, shifting remediation from retuning the number to restructuring the rule — and weakening the case that any fixed bar can satisfy this reading''s legitimacy condition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(uniform_threshold_domain_mismatch, conceptual, 'Whether the calibration target is a number or a structure.').

omega_variable(
    self_referential_tuning_lock,
    'Does the self-referential character of threshold change (lowering the bar normally requires first clearing it) make evidence-based tuning practically unavailable, converting the calibration ideal into cover for entrenchment?',
    'Track all recorded instances of threshold adjustment across jurisdictions and classify their trigger: ordinary calibrated review versus founding moment, crisis, or post-conflict rewrite.',
    'If tuning is practically locked outside crises, the adaptive reading''s legitimacy condition is vacuous for the standing arrangement as it stands, its effective extraction should be assessed as untunable, and the classification should migrate toward the minoritarian reading''s terrain despite this reading''s framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_referential_tuning_lock, conceptual, 'Whether the tuning remedy this reading prescribes is structurally reachable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__adaptive_gradient_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(supe_tr_t10, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(supe_tr_t30, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(supe_tr_t50, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 50, 0.29).
narrative_ontology:measurement(supe_tr_t60, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 60, 0.32).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(supe_be_t10, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(supe_be_t30, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 30, 0.54).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 40, 0.57).
narrative_ontology:measurement(supe_be_t50, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 50, 0.59).
narrative_ontology:measurement(supe_be_t60, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 60, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(supe_su_t10, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(supe_su_t20, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(supe_su_t30, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(supe_su_t50, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 50, 0.46).
narrative_ontology:measurement(supe_su_t60, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 60, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__adaptive_gradient_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold__consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold__minoritarian_veto_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'supermajority requirement'. The single natural-language concept covers three structurally distinct claims about the same standing arrangement: intrinsic consensus safeguard, entrenched veto machine, and calibratable instrument. Per the epsilon-invariance principle these are authored as three separate stories sharing kernel_id supermajority_threshold, each with its own reading-indexed epsilon over the same referent, its own beneficiary/victim structure, and its own claimed type. This (adaptive-gradient) story is the upstream member in one respect: its measurement-and-tuning program generates the evidence that reshapes the operating environment of the other two readings. Edges here link the family for contamination-propagation and cross-reading comparison analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
