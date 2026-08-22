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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Distributed Near-Miss and Incident-Sharing Learning Network (Competence Retention)
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint concerns how safety-critical organizations retain
 *   operational competence over long stretches without catastrophic
 *   reinforcement. The hybrid reading holds that neither simulation alone nor
 *   disaster alone is sufficient: durable competence requires a distributed
 *   learning network pooling near-misses, foreign (industry-wide, even
 *   cross-national) incidents, and high-realism drills across many
 *   organizations simultaneously. Aviation's ASRS-style voluntary,
 *   non-punitive, cross-carrier reporting system is the paradigm success
 *   case; medicine's fragmented, liability-chilled incident reporting is the
 *   paradigm partial-failure case. The constraint is tangled rope: it
 *   genuinely coordinates competence retention across an industry
 *   (beneficiaries: organizations, regulators, the public) while extracting
 *   uncompensated risk and labor from the individuals who generate the
 *   reporting signal (junior operators, whistleblowers), and requires active
 *   enforcement (mandatory reporting rules, immunity statutes,
 *   drill-frequency mandates) to keep functioning against the pull toward
 *   paperwork theater or liability-driven silence.
 *
 * KEY AGENTS:
 *   - operating_organizations: agenda_setter/beneficiary (institutional/constrained) — run and fund the learning infrastructure
 *   - regulators: agenda_setter/beneficiary (institutional/analytical) — mandate reporting and drill standards
 *   - the_traveling_public: beneficiary (powerless/trapped) — receives safety benefit with no visibility into system health
 *   - frontline_reporting_workers: beneficiary/payer (moderate/constrained) — generate the reporting signal
 *   - junior_frontline_operators: payer (powerless/trapped) — bear the brunt of high-realism drills and scrutiny
 *   - whistleblower_reporters: payer (powerless/trapped) — absorb informal retaliation risk despite formal immunity
 *   - organizations_in_weak_sharing_industries: payer (organized/constrained) — pay in recurring preventable harm where the mechanism fails to generalize
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.42).
domain_priors:suppression_score(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.38).
domain_priors:theater_ratio(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__hybrid_near_miss_learning, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__hybrid_near_miss_learning, "Distributed Near-Miss and Incident-Sharing Learning Network (Competence Retention)").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__hybrid_near_miss_learning, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__hybrid_near_miss_learning).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'becf045c-7817-4075-9616-f2549cb24267').
narrative_ontology:cs_kernel_codification('becf045c-7817-4075-9616-f2549cb24267', distributed).
narrative_ontology:cs_authority_grounding('becf045c-7817-4075-9616-f2549cb24267', practice).
narrative_ontology:cs_interpretation_layer_present('becf045c-7817-4075-9616-f2549cb24267').
narrative_ontology:cs_reading_relation('becf045c-7817-4075-9616-f2549cb24267', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, coexists_with).
narrative_ontology:cs_reading_relation('becf045c-7817-4075-9616-f2549cb24267', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, influences).
narrative_ontology:cs_axiom('becf045c-7817-4075-9616-f2549cb24267', foundational, distributed_signal_pooling_substitutes_for_local_rarity).
narrative_ontology:cs_axiom_status(distributed_signal_pooling_substitutes_for_local_rarity, holdable).
narrative_ontology:cs_axiom_grounding('becf045c-7817-4075-9616-f2549cb24267', distributed_signal_pooling_substitutes_for_local_rarity, empirically_contingent).
narrative_ontology:cs_axiom('becf045c-7817-4075-9616-f2549cb24267', foundational, neither_simulation_nor_disaster_alone_is_sufficient).
narrative_ontology:cs_axiom_status(neither_simulation_nor_disaster_alone_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('becf045c-7817-4075-9616-f2549cb24267', neither_simulation_nor_disaster_alone_is_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('becf045c-7817-4075-9616-f2549cb24267', post_disaster_reactive_learning).
narrative_ontology:cs_drift_state('becf045c-7817-4075-9616-f2549cb24267', contemporary_high_reliability_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('becf045c-7817-4075-9616-f2549cb24267', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, operating_organizations).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, regulators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, the_traveling_public).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_reporting_workers).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, junior_frontline_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, whistleblower_reporters).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, organizations_in_weak_sharing_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_reporting_workers).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__hybrid_near_miss_learning, distributed_learning_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Airlines, hospitals, and utilities run the mandatory reporting programs, fund the drill infrastructure, and decide how findings from near-miss data get folded into training. They benefit from lower catastrophic loss rates and reduced liability, but bear real cost maintaining reporting pipelines, immunity agreements, and drill realism, and can be tempted to let the system atrophy into paperwork once a bad-event drought sets in.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, operating_organizations, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, operating_organizations, beneficiary).

% Aviation and safety regulators design the mandatory and voluntary reporting schemes (ASRS-style systems), grant non-punitive immunity to encourage disclosure, and mandate drill frequency and realism standards. They depend on the reporting network functioning honestly to do their job at all, and have no exit from needing it — but face pressure to weaken standards when industry lobbies for cost relief.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, regulators, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, regulators, beneficiary).

% Passengers, patients, and residents downstream of the safety-critical system have no visibility into whether the learning infrastructure is real or theatrical. They benefit enormously when it works and bear the catastrophic tail risk when it degrades, with essentially no ability to audit or influence it directly.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, the_traveling_public, beneficiary,
    powerless, biographical, trapped, national).

% Pilots, nurses, and control-room operators who file near-miss reports gain collective safety benefit and, ideally, individual immunity from discipline. But filing takes uncompensated time and carries residual career risk if immunity is imperfectly enforced or if their organization's culture punishes reporting informally despite the formal protections.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_reporting_workers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, frontline_reporting_workers, payer).

% New pilots, residents, and junior operators are the ones drilled hardest and whose near-misses are most scrutinized, since they generate the most learning signal. They bear the brunt of high-realism drills (stress, failure exposure on record) and are least protected if reporting culture is uneven across shifts or supervisors, while having the least power to change reporting norms.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, junior_frontline_operators, payer,
    powerless, biographical, trapped, national).

% Individuals who report incidents that implicate systemic failures rather than individual error absorb real professional risk when immunity protections are informally circumvented (subtle retaliation, career stalling) even though the formal architecture promises non-punitive treatment. Their exit is limited because leaving the profession forfeits the very expertise the system needs from them.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, whistleblower_reporters, payer,
    powerless, biographical, trapped, national).

% Industries like medicine that lack aviation-grade cross-organizational incident-sharing networks (fragmented by liability law, competitive secrecy, and litigation exposure) pay in recurring preventable harm because the distributed learning mechanism this constraint depends on never fully forms at the industry level, even though individual institutions may run internal M&M conferences.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, organizations_in_weak_sharing_industries, payer,
    organized, generational, constrained, national).

% Not a person but a structural feature: tort liability exposure is the primary reason medicine's incident-sharing lags aviation's. It is not consulted or reformed as part of this constraint's operation, even though it is the dominant reason the hybrid learning mechanism fails to generalize outside aviation-like industries with strong no-fault reporting cultures.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, malpractice_liability_system, excluded,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(catastrophe_avoidance_retention__hybrid_near_miss_learning, malpractice_liability_system).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_avoidance_retention__hybrid_near_miss_learning, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_avoidance_retention__hybrid_near_miss_learning, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem that neither disaster (too rare, too costly, and too late to learn efficiently) nor simulation alone (too artificial, missing real stakes and unknown-unknowns) generates enough signal to keep a safety-critical workforce competent. Distributed sharing of near-misses, foreign incidents, and high-realism drills pools rare signal across many organizations so no single one has to wait for its own disaster.
% TRANSFER_FUNCTION: Moves attention, uncompensated reporting labor, and career/reputational risk from the individuals who generate incident reports (often junior or whistleblowing workers) to the institutional and public beneficiaries (organizations, regulators, and the traveling public) who receive the aggregated safety improvement without bearing the individual exposure cost of disclosure.
% ABSENT_VOICES: Frontline workers in weak-sharing industries (nursing staff, junior residents in fragmented hospital systems) who would testify that informal retaliation persists despite formal non-punitive reporting policies are rarely centered in industry-level safety-culture assessments, which tend to survey management rather than the reporting workforce directly.
% DISAPPEARANCE_RATIONALE: If the distributed near-miss/incident-sharing/drill infrastructure vanished overnight, safety-critical industries would revert to learning primarily from their own catastrophes (or nothing), competence would decay faster in high-turnover roles, and the gap between strong-sharing industries (aviation) and weak-sharing industries (medicine) would widen further as the former lost its main advantage.
% FOUNDING_PROBLEM: Early safety-critical industries learned almost exclusively from their own catastrophic failures — a slow, costly, and often fatal feedback loop. The founding problem was building a competence-retention mechanism that did not require repeated disasters to generate learning signal.
% FOUNDING_PROBLEM_CORROBORATION: Independent aviation-safety researchers and comparative health-services researchers (outside both the airline industry and hospital administration) corroborate that the founding problem — insufficient natural catastrophic frequency to sustain competence via disaster alone — remains live, citing continuing divergence in outcomes between industries with mature incident-sharing infrastructure and those without it.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__hybrid_near_miss_learning, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__hybrid_near_miss_learning, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.42 at interval end) because the coordination function is genuine and substantial — this is not primarily an extraction vehicle — but a real transfer exists from individual reporters (who bear uncompensated disclosure risk) to institutional and diffuse public beneficiaries. Suppression is moderate (0.38) reflecting the active enforcement machinery (mandatory reporting rules, immunity statutes) required to keep the network functioning against organizational incentives to let it lapse into paperwork. Theater ratio rises modestly over the interval (0.15 to 0.28) reflecting a documented tendency for reporting programs to drift toward compliance-metric optimization (report counts, drill completion checkboxes) as the mechanism matures and initial founding urgency fades — a mild Goodhart drift, not yet dominant. Accessibility collapse is moderate (0.35): alternatives to distributed learning (isolated organizational learning, pure simulation, waiting for disaster) remain conceptually available and are in fact the live sibling readings in this kernel contest, so alternatives have not collapsed nearly as completely as they would for a genuine mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Operating organizations and regulators sit near the agenda-setting/beneficiary end: they design the reporting architecture, capture the aggregate safety and legitimacy benefit, and have institutional exit options (they can weaken standards if unwatched). The traveling public is a pure diffuse beneficiary with no exit and no visibility — high dependency, zero agency. Frontline reporting workers are dual-positioned: they benefit from the collective safety improvement their reports fund but pay the individual cost of generating that signal. Junior operators and whistleblowers are the clearest targets: trapped exit options (professional identity and labor-market dependence lock them in), powerless structural position, and they bear the concentrated cost (drill stress, retaliation risk) that the aggregate beneficiaries never directly experience. Organizations in weak-sharing industries are a structurally distinct victim class — they pay not through direct extraction but through the mechanism's failure to generalize to their liability environment.
 *
 * MANDATROPHY ANALYSIS:
 *   This is not stale mandatrophy: the founding problem (insufficient natural catastrophic frequency for competence retention) remains demonstrably live, corroborated by outside comparative research, and the mechanism continues to produce measurable divergence between strong- and weak-sharing industries. The classification as tangled rope rather than pure rope prevents mislabeling this as costless coordination — the coordination function is real, but real people (junior operators, whistleblowers) pay real individual costs the aggregate beneficiaries do not share, and calling it a pure rope would erase that transfer. Conversely, classifying it as snare would erase the genuine, well-corroborated coordination function that measurably reduces catastrophic outcomes where it operates well (aviation) versus where it does not (fragmented medicine) — the comparative evidence base is exactly what distinguishes this from pure extraction dressed as safety theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sharing_network_causal_sufficiency,
    'Does the cross-organizational sharing network itself cause the competence-retention advantage observed in aviation, or is it a marker of an underlying no-fault safety culture that would produce good outcomes regardless of the specific sharing mechanism?',
    'Comparative natural experiments: industries that adopt aviation-style reporting infrastructure without the underlying no-fault culture (e.g., partial hospital adoptions of ASRS-style systems under continued malpractice exposure) would show whether the mechanism transfers independent of culture.',
    'If the sharing network is causally sufficient, policy should prioritize building the infrastructure directly. If it is merely a marker of underlying culture, policy should prioritize liability reform (no-fault protections) as the deeper lever, and the mechanism modeled here would be downstream rather than primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sharing_network_causal_sufficiency, empirical, 'Whether incident-sharing infrastructure is causally sufficient or merely correlated with a deeper no-fault culture.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the disagreement between the three kernel readings (hybrid, simulation-as-proxy, catastrophe-as-necessary) actually live — is it about the empirical sufficiency of different signal sources, or about a deeper disagreement over whether competence can be maintained without lived stakes at all?',
    'This is committer structure, not resolvable within a single reading: the three readings could be reconciled or shown genuinely incompatible only by examining whether organizations using pure simulation (the sibling reading) ever match the safety records of hybrid-network industries at scale, and whether any hybrid-network industry has avoided all catastrophic reinforcement entirely.',
    'If simulation alone empirically matches hybrid outcomes in some domain, the hybrid reading''s claim that simulation is insufficient would be falsified for that domain, narrowing this reading''s scope. If catastrophe-as-necessary is correct that some irreducible chaos/mortality-salience signal is required, the hybrid reading''s claim that distributed near-miss sharing substitutes for it would be undermined.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Locates where the three sibling readings of the catastrophe_avoidance_retention kernel actually diverge.').

omega_variable(
    informal_retaliation_prevalence,
    'How prevalent is informal (non-statutory) retaliation against whistleblower reporters despite formal non-punitive immunity protections, across industries?',
    'Anonymous longitudinal surveys of frontline reporters tracking career outcomes post-disclosure, compared against matched non-reporting peers, within and across industries with varying immunity enforcement strength.',
    'High prevalence would indicate the extraction from whistleblower_reporters is more severe than the formal architecture suggests, pushing the effective classification closer to snare for that stakeholder seat specifically. Low prevalence would support the tangled_rope reading as currently authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(informal_retaliation_prevalence, empirical, 'Whether formal reporting immunity is undermined in practice by informal career retaliation.').


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
narrative_ontology:measurement(cata_tr_t16, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 16, 0.21).
narrative_ontology:measurement(cata_tr_t24, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 24, 0.24).
narrative_ontology:measurement(cata_tr_t32, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 32, 0.26).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 40, 0.28).

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
narrative_ontology:boltzmann_floor_override(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.12).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_as_necessary_selector).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the catastrophe_avoidance_retention kernel. simulation_as_proxy_catastrophe claims high-fidelity drills are functionally equivalent to real catastrophic events (lower authored extraction, closer to pure rope/mountain-adjacent). catastrophe_as_necessary_selector claims only actual disasters provide sufficient selection pressure (different beneficiary/victim structure — the 'victims' become those harmed in the necessary catastrophes themselves). This hybrid reading occupies a middle empirical position and is authored with its own distinct ε (0.42) reflecting the specific extraction embedded in its distributed-network mechanism (uncompensated reporting labor, uneven informal retaliation) — it does not average the siblings' values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
