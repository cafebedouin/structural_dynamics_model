% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__adaptive_gradient_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Supermajority Threshold as Calibratable Consensus Filter (Adaptive Gradient Reading)
 *   domain: constitutional theory/political economy/institutional design
 *
 * SUMMARY:
 *   Constitutional and legislative supermajority requirements (amendment
 *   thresholds, elevated passage rules for fundamental law) form a standing
 *   arrangement that filters which proposed changes take effect. This story
 *   instantiates ONE reading of that arrangement — the
 *   adaptive_gradient_reading — under which the threshold is a functional
 *   instrument whose legitimacy rests entirely on matching its height to
 *   measured social consensus formation rates and reversibility costs: too
 *   low and revision outruns durable agreement, too high and change is
 *   blocked after consensus has already formed. On this reading the standing
 *   arrangement's fixed, historically inherited numbers are largely untuned,
 *   and the gap between the fixed heights and the moving consensus landscape
 *   widens as preference aggregation accelerates and status-quo reversal
 *   costs grow. The epsilon referent is the standing fixed-threshold
 *   arrangement AS THIS READING ASSESSES IT — not the calibrated ideal the
 *   reading endorses. Per the claim/metric independence rule, the claimed
 *   type and the authored metrics are independent facts: the claim states
 *   what this reading believes is structurally true; the metrics describe the
 *   arrangement's actual operation.
 *
 * KEY AGENTS:
 *   - - blocking_minority_caucuses: Primary beneficiary (organized/constrained) — collects veto premiums, delay, and bargaining concessions whenever it holds a blocking position
 *   - - incumbent_officeholders: Dual-positioned beneficiary/payer (powerful/arbitrage) — protected from rapid reversal of their enactments, taxed by the gridlock the same elevation imposes on their agendas
 *   - - status_quo_organized_interests: Beneficiary (organized/mobile) — organized lobbies whose preferred policies are shielded from reversal by the elevated bar
 *   - - electoral_majorities_seeking_reform: Primary target (organized/constrained) — election-winning coalitions whose mandates stall at the threshold
 *   - - blocked_constitutional_reform_movements: Deep target (moderate/trapped) — movements whose objectives require textual amendment and have no alternative venue
 *   - - legislative_procedure_authorities: Agenda setter (institutional/constrained) — rules committees, presiding officers, and certifying courts that administer and enforce the threshold daily
 *   - - ordinary_unorganized_citizens: Excluded voice (powerless/trapped) — bears both ossification costs and instability risks with no seat at the threshold table
 *   - - comparative_constitutional_scholars: Analytical observer (analytical/analytical) — measures amendment difficulty and calibration gaps cross-nationally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__adaptive_gradient_reading, 0.6).
domain_priors:suppression_score(supermajority_threshold__adaptive_gradient_reading, 0.48).
domain_priors:theater_ratio(supermajority_threshold__adaptive_gradient_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__adaptive_gradient_reading, tangled_rope).
narrative_ontology:human_readable(supermajority_threshold__adaptive_gradient_reading, "Supermajority Threshold as Calibratable Consensus Filter (Adaptive Gradient Reading)").
narrative_ontology:topic_domain(supermajority_threshold__adaptive_gradient_reading, "constitutional theory/political economy/institutional design").

domain_priors:requires_active_enforcement(supermajority_threshold__adaptive_gradient_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__adaptive_gradient_reading, 'd071c6fd-fc6e-4bf5-9841-c5f16c2fded5').
narrative_ontology:cs_kernel_codification('d071c6fd-fc6e-4bf5-9841-c5f16c2fded5', formalized).
narrative_ontology:cs_authority_grounding('d071c6fd-fc6e-4bf5-9841-c5f16c2fded5', expertise).
narrative_ontology:cs_interpretation_layer_present('d071c6fd-fc6e-4bf5-9841-c5f16c2fded5').
narrative_ontology:cs_reading_relation('d071c6fd-fc6e-4bf5-9841-c5f16c2fded5', supermajority_threshold__consensus_safeguard_reading, influences).
narrative_ontology:cs_reading_relation('d071c6fd-fc6e-4bf5-9841-c5f16c2fded5', supermajority_threshold__minoritarian_veto_reading, coexists_with).
narrative_ontology:cs_axiom('d071c6fd-fc6e-4bf5-9841-c5f16c2fded5', foundational, threshold_legitimacy_is_calibration_performance).
narrative_ontology:cs_axiom_status(threshold_legitimacy_is_calibration_performance, holdable).
narrative_ontology:cs_axiom_grounding('d071c6fd-fc6e-4bf5-9841-c5f16c2fded5', threshold_legitimacy_is_calibration_performance, empirically_contingent).
narrative_ontology:cs_axiom('d071c6fd-fc6e-4bf5-9841-c5f16c2fded5', secondary, no_intrinsically_correct_threshold_height).
narrative_ontology:cs_axiom_status(no_intrinsically_correct_threshold_height, holdable).
narrative_ontology:cs_axiom_grounding('d071c6fd-fc6e-4bf5-9841-c5f16c2fded5', no_intrinsically_correct_threshold_height, empirically_contingent).
narrative_ontology:cs_reference_frame('d071c6fd-fc6e-4bf5-9841-c5f16c2fded5', calibrated_consensus_filter).
narrative_ontology:cs_drift_state('d071c6fd-fc6e-4bf5-9841-c5f16c2fded5', contemporary_fixed_threshold_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d071c6fd-fc6e-4bf5-9841-c5f16c2fded5', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, blocking_minority_caucuses).
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, incumbent_officeholders).
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, status_quo_organized_interests).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, electoral_majorities_seeking_reform).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, blocked_constitutional_reform_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, incumbent_officeholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rules committees, presiding officers, and certifying bodies administer the elevated-passage rules day to day: recognizing motions, ruling on cloture and quorum, certifying vote counts. They can adjust procedural variants at the margin (how delay is recognized, what counts as consideration) but the constitutional floor numbers sit beyond their reach. They operate inside the rulebook they enforce and collect no direct share of what the elevation shields or blocks.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, legislative_procedure_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Chamber minorities large enough to deny the elevated bar but short of majority. Whenever they hold the blocking position, concessions, amendments, and agenda deference flow to them in exchange for release; the premium is theirs for the duration of the block. Exit means surrendering that premium while out of power with no guarantee of recovering it — the position persists across alternation because each cohort expects to be the minority eventually.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, blocking_minority_caucuses, beneficiary,
    organized, biographical, constrained, national).

% Sitting legislators and executives whose enacted programs are shielded from rapid repeal by the elevated bar, and who can work the same procedures to move their own priorities. They simultaneously pay: the same elevation stalls their affirmative agendas and forces coalition prices on every initiative. Their position lets them shift between exploiting the shield and campaigning against it as circumstances favor.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, incumbent_officeholders, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__adaptive_gradient_reading, incumbent_officeholders, payer).

% Organized lobbies and industry coalitions whose preferred regulatory settlements stay in force because reversal cannot clear the elevated bar. They invest in defending the threshold itself and in cultivating blocking relationships. Exit is easy in the relevant sense: if one venue closes, they redeploy lobbying effort across committees, states, and levels of government.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, status_quo_organized_interests, beneficiary,
    organized, generational, mobile, national).

% Coalitions that won elections on platforms requiring statutory or constitutional change and find the mandate stalled at the elevated bar. Partial substitutes exist — ordinary statutes, state-level replication, incremental regulation — but none reaches the objective the majority actually voted for. Their recourse is waiting for another election whose results face the same bar.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, electoral_majorities_seeking_reform, payer,
    organized, biographical, constrained, national).

% Movements whose objectives require textual amendment — structural reforms, codified rights, franchise changes. No alternative venue reaches their target: statutes can be repealed, interpretations shifted, but the text moves only through the elevated process itself. Commitments span generations; supporters age into and out of the movement while the objective stays fixed behind the bar.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, blocked_constitutional_reform_movements, payer,
    moderate, generational, trapped, national).

% Citizens without lobbying capacity or institutional sponsorship who live under the policies the elevation freezes and bear the risks of the delays it imposes — unaddressed accumulating problems on one side, whiplash if the bar ever fails on the other. They participate through periodic elections whose mandates the process can render inert, and they have no seat where threshold heights are set or defended.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, ordinary_unorganized_citizens, excluded,
    powerless, generational, trapped, national).

% Researchers comparing amendment difficulty, passage rates, and institutional durability across jurisdictions; they publish calibration analyses, advise reform commissions, and document the gap between fixed thresholds and measured consensus dynamics. They hold no enforcement stake and their assessments bind no one.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__adaptive_gradient_reading, blocking_minority_caucuses).
narrative_ontology:fixing_cost_class(supermajority_threshold__adaptive_gradient_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Filters proposed fundamental changes so that only proposals sustaining broad support across an elevated bar take effect; forces coalition-building that surfaces minority objections before enactment; stabilizes long-horizon expectations by slowing reversal of settled arrangements.
% TRANSFER_FUNCTION: Moves effective agenda control from momentary simple majorities to whatever coalition can clear the elevated bar — in practice transferring veto power, delay, and bargaining premiums to blocking minorities and shielding incumbents' enactments, paid for by reform-seeking majorities and amendment-bound movements.
% ABSENT_VOICES: Ordinary unorganized citizens bear the ossification costs and instability risks but have no seat where thresholds are set or defended; future cohorts affected by frozen policy are represented by no one at the table; amendment-bound movements attend only as supplicants to a process they cannot staff.
% DISAPPEARANCE_RATIONALE: If the elevated thresholds vanished overnight, fundamental legislation and amendment would proceed on simple majorities: blocking minorities would lose their premiums and leverage immediately, incumbent protections against repeal would evaporate, amendment cycles would compress dramatically, and the political economy would reorganize around majority velocity — with correspondingly higher exposure to rapid reversal of anything enacted.
% FOUNDING_PROBLEM: Shielding fundamental law from transient majoritarian passion and factional capture: ensuring that constitutional-level change reflected broad, durable agreement rather than momentary coalitions, in an era when the drafters feared rapid factional swings.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship across interpretive camps corroborates that the founding-era fear of factional volatility was genuine and motivated the elevated bars — no beneficiary needed to assert it. Whether the problem persists at a scale matching the CURRENT heights is disputed: comparative constitutional research and amendment-frequency studies attest both that some filtering demand remains and that fixed thresholds now routinely overshoot measured consensus formation; state legislative applications for constitutional conventions and reform commission findings corroborate the overshoot side from outside the benefiting parties.
narrative_ontology:disappearance_verdict(supermajority_threshold__adaptive_gradient_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__adaptive_gradient_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__adaptive_gradient_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(supermajority_threshold__adaptive_gradient_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__adaptive_gradient_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.60: the reading locates substantial but not total extraction in the standing arrangement — the fixed heights block a growing class of changes that measured consensus has already formed, transferring agenda control to whoever holds blocking position, while a real filtering function absorbs part of the arrangement's operation. Suppression is 0.48 and is authored as a RAW STRUCTURAL PROPERTY, unscaled by power or scope (only extractiveness is scaled, by the engine, through directionality and scope): the threshold is procedurally enforced rather than coercively imposed, but alternatives for threshold-bound objectives are thin. Theater ratio is 0.38: genuine filtering and forced-coalition work continues, but a growing share of activity is performative obstruction — symbolic floor speeches, holds, and doomed votes staged for constituents rather than outcome-relevant screening. Accessibility collapse is low (0.32) because alternatives persist — ordinary majority statutes, state-level replication, interstate compacts, judicial interpretation — they simply cannot reach amendment-bound objectives. Resistance is 0.58: sustained reform movements, abolition campaigns against specific elevated rules, and a scholarly literature documenting the calibration gap. The temporal series run on ONE SHARED seven-point grid (T=0..60) with all three metrics authored at every point; the rising base_extractiveness trajectory models the widening gap between fixed thresholds and accelerating consensus formation, and the rising suppression_requirement series models enforcement hardening (norm-governed delay practices maturing into routinized, self-executing procedural blockade) — this is an enforcement-capacity story, so the series is authored deliberately rather than left to the scalar.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda_setter seat (legislative_procedure_authorities) the threshold is neutral machinery they administer without owning; from the beneficiary seats it is protection and leverage; from the payer seats it is a nullified mandate — an election won and then rendered inert at the floor; from the observer seat it is a measurable calibration error. Same rule, same chamber, four different operative realities. The engine computes per-seat classifications from the structural data (role, power, exit, scope); this story does not adjudicate the divergence, it supplies the data that produces it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidized end: blocking_minority_caucuses collect the veto premium directly (low d, further damped by their ability to alternate into majority position); status_quo_organized_interests collect shielding with mobile exit across venues (lowest d); incumbent_officeholders are genuinely dual-positioned — protection received, gridlock paid — so their effective d lands mid-range despite beneficiary role. Victims sit near the target end: electoral_majorities_seeking_reform bear the transfer with constrained exit (high d), and blocked_constitutional_reform_movements bear it with no exit at all (highest d — trapped, generational horizon, amendment-bound objectives). No directionality_overrides are authored: the role declarations plus differentiated exit options already separate the seats cleanly, and the only candidate override (marking electoral majorities as fuller targets than their organized power suggests) is already captured by their victim declaration plus constrained exit. Scope is national for domestic seats; the observer carries global scope, which the engine handles in its own scaling.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what prevents mislabeling in both directions. Reading the threshold as pure coordination (the defenders' framing) would erase the measured transfer from reform-seeking majorities to blocking positions — the too-high failure mode this reading names. Reading it as pure extraction (the indictment framing) would erase the real filtering function that occasionally catches genuinely transient proposals — the too-low failure mode. The gradient framing holds both: the SAME structure coordinates and extracts, and which dominates is a calibration question, not a fixed identity. On the R5 genealogy interview: the founding problem (shielding fundamental law from transient factional passion) is authored as CONTESTED, not dead — the underlying phenomenon persists, but whether the fixed heights address it or overshoot it is precisely what the parties dispute. Because status is contested rather than dead alongside a world_rearranges verdict, no dead-problem/zombie flag should fire: the arrangement persists because real arrangements depend on it, not as theater over a vanished mandate. The fixing_cost assessment (prohibitive) is grounded in the self-reference structure — the threshold governs its own revision — documented in the self_reference_revision_lock omega rather than asserted bare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates the adaptive_gradient_reading of the supermajority_threshold kernel; would the sibling readings (consensus_safeguard_reading, minoritarian_veto_reading) produce different epsilon, victim sets, and classifications over the same standing arrangement?',
    'Generate the sibling stories as separate files over the same structural facts and compare computed per-seat classifications; the disagreement is located in what grounds the threshold''s legitimacy (intrinsic protective function vs measurable calibration performance vs genealogy of entrenched privilege).',
    'The consensus_safeguard_reading would lower epsilon (threshold as protective certificate, victims confined to transient majorities); the minoritarian_veto_reading would raise epsilon toward pure extraction (beneficiaries recast as entrenched privilege, coordination function dismissed as cover). This file authors only the adaptive gradient''s values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    calibration_measurability,
    'Can social consensus formation rates and reversibility costs actually be measured precisely enough to tune thresholds, or does the evidence-based tuning premise fail at implementation?',
    'Pilot calibration programs in subnational or procedural venues: estimate consensus half-lives and policy reversal costs, attempt threshold adjustment, and audit whether adjusted thresholds track measured consensus better than fixed ones.',
    'If measurability fails, the reading loses its legitimacy ground and collapses toward whichever sibling supplies a non-empirical warrant; if it succeeds, fixed thresholds lose their defense-by-tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(calibration_measurability, empirical, 'Whether the reading''s foundational empirical premise is implementable.').

omega_variable(
    miscalibration_direction_ambiguity,
    'Is the standing arrangement''s dominant miscalibration too-high (change blocked after consensus has formed) or too-low (revision outrunning durable agreement), and does the answer vary by policy domain?',
    'Domain-resolved calibration audits: compare amendment and statute passage rates against survey-based consensus trajectories and reversal-cost estimates per domain (rights, fiscal, structural).',
    'Too-high dominance supports the extraction-weighted profile authored here; too-low dominance in key domains would shift the profile toward under-damped coordination and lower the measured burden on reform-seeking majorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(miscalibration_direction_ambiguity, conceptual, 'Which failure mode dominates the current calibration gap, per domain.').

omega_variable(
    self_reference_revision_lock,
    'Does the threshold''s application to its own revision make fixing cost categorically prohibitive, or only contingently so under current political alignment?',
    'Comparative analysis of jurisdictions that successfully recalibrated amendment thresholds (via constituent assemblies, staged amendment, or court-mediated reinterpretation) versus failed attempts.',
    'If categorical, the arrangement''s persistence is structurally guaranteed regardless of performance; if contingent, calibration reform is reachable and the prohibitive cost assessment is period-specific.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_reference_revision_lock, conceptual, 'Whether self-referential amendment locks are escapable in principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__adaptive_gradient_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sgt_adaptive_gradient_tr_t0, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(sgt_adaptive_gradient_tr_t10, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(sgt_adaptive_gradient_tr_t20, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(sgt_adaptive_gradient_tr_t30, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement(sgt_adaptive_gradient_tr_t40, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(sgt_adaptive_gradient_tr_t50, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 50, 0.36).
narrative_ontology:measurement(sgt_adaptive_gradient_tr_t60, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 60, 0.38).

% Extraction over time
narrative_ontology:measurement(sgt_adaptive_gradient_be_t0, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sgt_adaptive_gradient_be_t10, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(sgt_adaptive_gradient_be_t20, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(sgt_adaptive_gradient_be_t30, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement(sgt_adaptive_gradient_be_t40, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement(sgt_adaptive_gradient_be_t50, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(sgt_adaptive_gradient_be_t60, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 60, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(sgt_adaptive_gradient_su_t0, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(sgt_adaptive_gradient_su_t10, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement(sgt_adaptive_gradient_su_t20, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 20, 0.39).
narrative_ontology:measurement(sgt_adaptive_gradient_su_t30, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement(sgt_adaptive_gradient_su_t40, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(sgt_adaptive_gradient_su_t50, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 50, 0.47).
narrative_ontology:measurement(sgt_adaptive_gradient_su_t60, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 60, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__adaptive_gradient_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold__consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold__minoritarian_veto_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'supermajority requirement' decomposes into three structurally distinct claims with different epsilon values — the adaptive gradient reading (this file: legitimacy = calibration performance, mixed coordination/extraction profile), the consensus safeguard reading (legitimacy intrinsic to the elevation, low extraction, victims confined to transient majorities), and the minoritarian veto reading (coordination as cover for entrenched privilege, high extraction). The adaptive gradient reading is methodologically upstream: its measurement program re-parameterizes the disputes the other two readings conduct, so its edges run toward both siblings. Each member links the others via affects_constraints; no member hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
