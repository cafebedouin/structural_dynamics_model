% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__adaptive_gradient_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Supermajority Gate as Calibratable Instrument (Adaptive Gradient Reading)
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The standing arrangement under contest: constitutionally fixed
 *   supermajority gates (chamber fractions, state-ratification fractions,
 *   referendum quorums) that every piece of fundamental-law change must
 *   clear. This story instantiates the adaptive_gradient_reading of the
 *   supermajority_threshold kernel: the gate is a procedural instrument whose
 *   legitimacy is contingent on measured fit between the threshold value, the
 *   rate at which durable social consensus actually forms, and the
 *   reversibility cost of the decisions gated — not on intrinsic value.
 *   Assessed by that reading's own lights, the standing arrangement is an
 *   untuned instrument: threshold values inherited from founding bargains,
 *   never recalibrated against modern consensus-formation data,
 *   systematically over-blocking proposals with documented durable
 *   supermajority support while costing status quo holders nothing to block.
 *   The claim/metric split is deliberate: claimed_type states my structural
 *   belief (a real filtering function fused with asymmetric extraction); the
 *   metrics state the arrangement's descriptive operation; the engine
 *   computes per-seat classifications independently. The sibling readings are
 *   separate constraints in separate files; this story neither averages over
 *   them nor hedges across them. KEY AGENTS (by structural relationship): -
 *   status_quo_entrenchment_interests: Primary beneficiary
 *   (powerful/arbitrage) — collects continued entrenchment by assembling mere
 *   blocking minorities - constitutional_minorities: Secondary beneficiary
 *   (organized/constrained) — shielded incidentally, pays the same gate when
 *   it seeks change - amendment_proponents: Primary target
 *   (organized/constrained) — bears the elevated cost of fundamental change -
 *   blocked_durable_majorities: Diffuse target (powerless/trapped) —
 *   sustained preferences fail to convert into change -
 *   procedural_administering_institutions: Agenda-setter
 *   (institutional/identity_locked) — enforces the inherited value it cannot
 *   alter - contemporary_electorates: Excluded seat (moderate/trapped) —
 *   never consented to the specific values - comparative_design_scholars:
 *   Analytical observer (analytical/analytical) — produces calibration
 *   evidence no administrator must consume
 *
 * KEY AGENTS:
 *   - status_quo_entrenchment_interests: primary beneficiary — powerful, arbitrage exit, collects entrenchment rent at zero blocking cost
 *   - constitutional_minorities: secondary beneficiary — organized, constrained exit, shielded incidentally by the gate
 *   - amendment_proponents: primary target — organized, constrained exit, pays the gate's full price
 *   - blocked_durable_majorities: diffuse target — powerless, trapped, bears the gap between durable preference and admitted change
 *   - procedural_administering_institutions: agenda-setter — institutional, identity_locked, certifies and enforces the inherited value
 *   - contemporary_electorates: excluded — moderate, trapped, never ratified the specific threshold values
 *   - comparative_design_scholars: observer — analytical, global scope, supplies unused calibration evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__adaptive_gradient_reading, 0.58).
domain_priors:suppression_score(supermajority_threshold__adaptive_gradient_reading, 0.62).
domain_priors:theater_ratio(supermajority_threshold__adaptive_gradient_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__adaptive_gradient_reading, tangled_rope).
narrative_ontology:human_readable(supermajority_threshold__adaptive_gradient_reading, "Supermajority Gate as Calibratable Instrument (Adaptive Gradient Reading)").
narrative_ontology:topic_domain(supermajority_threshold__adaptive_gradient_reading, "political/constitutional").

domain_priors:requires_active_enforcement(supermajority_threshold__adaptive_gradient_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__adaptive_gradient_reading, '913bbdc9-832c-437a-9d45-581bfb151221').
narrative_ontology:cs_kernel_codification('913bbdc9-832c-437a-9d45-581bfb151221', formalized).
narrative_ontology:cs_authority_grounding('913bbdc9-832c-437a-9d45-581bfb151221', expertise).
narrative_ontology:cs_interpretation_layer_present('913bbdc9-832c-437a-9d45-581bfb151221').
narrative_ontology:cs_reading_relation('913bbdc9-832c-437a-9d45-581bfb151221', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('913bbdc9-832c-437a-9d45-581bfb151221', supermajority_threshold__minoritarian_veto_reading, coexists_with).
narrative_ontology:cs_axiom('913bbdc9-832c-437a-9d45-581bfb151221', foundational, threshold_legitimacy_is_performance_contingent).
narrative_ontology:cs_axiom_status(threshold_legitimacy_is_performance_contingent, holdable).
narrative_ontology:cs_axiom_grounding('913bbdc9-832c-437a-9d45-581bfb151221', threshold_legitimacy_is_performance_contingent, empirically_contingent).
narrative_ontology:cs_axiom('913bbdc9-832c-437a-9d45-581bfb151221', foundational, no_intrinsically_correct_threshold_value).
narrative_ontology:cs_axiom_status(no_intrinsically_correct_threshold_value, holdable).
narrative_ontology:cs_axiom_grounding('913bbdc9-832c-437a-9d45-581bfb151221', no_intrinsically_correct_threshold_value, instrumental).
narrative_ontology:cs_reference_frame('913bbdc9-832c-437a-9d45-581bfb151221', calibratable_procedural_instrument).
narrative_ontology:cs_drift_state('913bbdc9-832c-437a-9d45-581bfb151221', contemporary_mass_polling_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('913bbdc9-832c-437a-9d45-581bfb151221', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, status_quo_entrenchment_interests).
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, constitutional_minorities).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, amendment_proponents).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, blocked_durable_majorities).
narrative_ontology:constraint_vindicates(supermajority_threshold__adaptive_gradient_reading, supermajority_consensus_filtering_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Concentrated incumbents — economic, regional, partisan — whose advantages are embedded in the current constitutional settlement. The gate lets them preserve that settlement indefinitely by assembling only a blocking minority; they bear none of the coordination cost, need only prevent agreement, and remain free to pursue everything else in ordinary politics while the floor stays frozen.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, status_quo_entrenchment_interests, beneficiary,
    powerful, generational, arbitrage, national).

% Groups whose protections depend on entrenched provisions a transient hostile majority might strip. The raised cost of amendment shields them. The benefit is incidental — they do not run the gate and do not set its value — and when they pursue their own reforms they hit the identical wall everyone else does.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, constitutional_minorities, beneficiary,
    organized, generational, constrained, national).

% Cross-partisan coalitions campaigning for specific constitutional changes. They must sustain support across multiple chambers, multiple election cycles, and dispersed ratification bodies; proposals with years of durable polling majorities die procedurally. The alternatives are worse: statutes lack entrenchment, a constitutional convention is harder than the amendment route itself, and courts will not hear the question.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, amendment_proponents, payer,
    organized, biographical, constrained, national).

% Broad publics whose sustained preferences — documented across years of polling — never convert into constitutional change. Individually each bears almost nothing; collectively they bear the entire gap between what they durably want and what the gate admits. There is no exit: the gate is the only door to fundamental law, and its cost structure is spread so thinly per capita that coalition formation never crystallizes.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, blocked_durable_majorities, payer,
    powerless, generational, trapped, national).

% Presiding officers, clerks, and courts that count votes, certify compliance, and refuse end-runs around the procedure. They did not choose the threshold value and cannot change it, yet their authority is fused with enforcing whatever value stands — decades of doctrine presuppose the current numbers, and administering a different value would unravel the interpretive structure they have built their legitimacy on.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, procedural_administering_institutions, agenda_setter,
    institutional, generational, identity_locked, national).

% Living voters who inherited the specific threshold values without ever consenting to them. Every generation is bound by a number no living member chose, set in bargaining among people long dead. Seated at the table where the value was fixed, they would demand recalibration against current consensus-formation evidence; instead they encounter the value only as an immovable fact.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, contemporary_electorates, excluded,
    moderate, generational, trapped, national).

% Researchers who measure threshold performance across jurisdictions and decades: how durable the blocked proposals' support was, how fast consensus forms, what reversibility costs the gated decisions carry. They produce exactly the calibration evidence the gradient reading says legitimacy requires — and no administering body is obliged to read a word of it.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, comparative_design_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__adaptive_gradient_reading, status_quo_entrenchment_interests).
narrative_ontology:fixing_cost_class(supermajority_threshold__adaptive_gradient_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Filters proposals for fundamental legal change so that only those commanding unusually broad, cross-factional, cross-temporal support become entrenched; prevents each electoral cycle from rewriting the constitutional floor.
% TRANSFER_FUNCTION: Transfers effective control over fundamental law from momentary working majorities to any coalition large enough to deny the threshold — in practice, to status quo holders, who need only block. Moves the cost of change onto reform coalitions while leaving the cost of blocking at zero.
% ABSENT_VOICES: Contemporary electorates, who never consented to the specific values; future generations, bound by today's entrenchments; and reform movements locked out of the amendment conversation by the very gate they contest. Comparative-design evidence sits outside the room entirely — the people producing the calibration data the reading says legitimacy requires have no seat where the value is maintained.
% DISAPPEARANCE_RATIONALE: If the gate vanished overnight, fundamental law would track ordinary majorities: entrenchments would become revisable each cycle, blocking minorities would lose their veto outright, entrenched interests would have to defend their position politically instead of procedurally, and constitutional content would oscillate with electoral coalitions — including the instability the gate was built to prevent. Arrangements across every seat depend on the gate's existence, one way or the other.
% FOUNDING_PROBLEM: Post-ratification fear that transient majorities would dismantle the constitutional settlement before its value was proven; the threshold was built to require unusually broad agreement before any fundamental change.
% FOUNDING_PROBLEM_CORROBORATION: Ratification-era correspondence and founding-defense texts attest the founding fear itself, from outside today's beneficiary set. Comparative constitutional scholarship — external to all beneficiary seats — documents both residual volatility risk (the problem is not dead) and systematic over-blocking of durably supported proposals (the inherited values are not attested as calibrated). No source outside the beneficiary seats attests that the specific inherited threshold values fit current consensus-formation rates; that attestation exists only inside the seats the gate protects.
narrative_ontology:disappearance_verdict(supermajority_threshold__adaptive_gradient_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__adaptive_gradient_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__adaptive_gradient_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(supermajority_threshold__adaptive_gradient_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__adaptive_gradient_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction 0.58: the gate decouples the price of fundamental change from the breadth of actual support — proposals carrying years of supermajority polling die procedurally while blocking is free — but the gate also genuinely stops volatile proposals, so extraction is substantial without being dominant. Suppression 0.62 is authored as a RAW structural property, unscaled by power or scope (only extractiveness is scaled, by directionality and scope, in the engine): alternatives to the gate for constitutional-level change are few — conventions are harder than amendment, statutes lack entrenchment, courts are closed to end-runs — and enforcement machinery (certification rules, judicial gloss, rules-committee gatekeeping) actively forecloses routing around the value. Theater 0.34: deliberative ritual around thresholds is increasingly ceremonial while real filtering migrates to party discipline and doctrine. Accessibility_collapse 0.52: ordinary law remains fully available below the constitutional line, so alternatives collapse only for the gated class of decisions. Resistance 0.55: recurring amendment campaigns, convention-petition movements, and threshold-reform proposals press against the gate continuously. Coalition note: blocked_durable_majorities hold latent coalition power that their diffuseness currently fails to organize — the gate's cost structure is spread so thinly per capita that coalition formation is suppressed, which is a structural fact the engine should see in the powerless/trapped pairing. The measurement series run on ONE shared grid (t=0..60, step 10) with all three tracked metrics authored at every point; suppression_requirement is included because the narrative specifically tracks enforcement-machinery hardening (procedural certification thickening, judicial gloss accumulating) across the interval, not merely extraction drift.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the gate is settled law whose value is not in question — the job is administration, not design. From the beneficiary seats the same structure reads as either shield (constitutional minorities) or free option (entrenched interests, who risk nothing by blocking). From the payer seats the identical structure operates as a toll booth priced independently of support breadth: the broader and more durable the coalition, the more years of effort the gate consumes. The engine computes these divergent per-seat classifications from the power/exit/role data; the divergence between the safeguard-flavored experience of beneficiaries and the snare-flavored experience of payers is precisely the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Entrenched interests sit nearest the beneficiary pole: the gate subsidizes their position at zero marginal cost, and their arbitrage-grade exit (they can pursue ordinary-political aims freely while the gate guards the constitutional floor) pushes them further toward subsidy. Constitutional minorities derive real but incidental benefit — the declaration places them low-d, but their constrained exit keeps them short of full subsidy, and they flip toward target whenever they initiate change. Amendment proponents and blocked durable majorities sit near the full-target pole: they pay the gate's full price with constrained or no exit; the powerless/trapped combination maximizes effective extraction for the diffuse seat, amplified by national scope making true-consensus verification hard. Administering institutions are near-symmetric in flow terms but identity_locked — their professional authority is fused with enforcing whatever value stands — pinning their effective position to enforcement regardless of preference. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the correct relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what blocks both mislabelings the sibling readings embody. Reading the gate as pure rope (the safeguard sibling's temptation) erases the identifiable payers and the zero-cost blocking option; reading it as pure snare (the veto sibling's temptation) erases the genuine filtering function that stops volatile rewrites of fundamental law. On mandatrophy: the founding problem — transient-majority dismantling of a fragile settlement — is not dead, since volatility risk persists, but the specific inherited calibration is attested by no one outside the beneficiary seats; hence founding_problem_status=contested combined with disappearance_verdict=world_rearranges classifies the arrangement as functioning-under-contested-warrant rather than zombie. Trajectory: if consensus-rate measurement matures and shows the inherited values far from any defensible optimum, the rope component thins and the engine should drift this toward snare; if robust filtering value is demonstrated, it drifts toward rope. Either drift is data the corpus wants; the claim is authored independently of both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_frame_underdetermination,
    'Which reading of the supermajority_threshold kernel governs evaluation — this adaptive-gradient instrument frame, the consensus-safeguard intrinsic-value frame, or the minoritarian-veto capture frame?',
    'Not resolvable by data within this story; resolved comparatively across the three sibling stories'' structural outputs, since adopting a frame is a party-level commitment act rather than an empirical finding.',
    'Under the consensus-safeguard frame epsilon falls (filtering vindicated as intrinsically good); under the minoritarian-veto frame epsilon rises toward snare (the gate as privilege conversion); the tangled_rope verdict authored here holds only inside the gradient frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_frame_underdetermination, conceptual, 'This constraint is one reading of kernel supermajority_threshold; sibling readings instantiate different constraints with different victim sets and epsilon values.').

omega_variable(
    consensus_rate_measurability,
    'Can actual social consensus formation rates be measured precisely enough to ground legitimacy claims — and which measure (revealed durability of polled preferences, cross-cohort stability, elite-mass convergence) is the correct referent for calibration?',
    'Longitudinal opinion panels cross-validated against eventual amendment outcomes across jurisdictions; adversarial replication of durability metrics by independent research groups.',
    'If no measure stabilizes, the reading''s performance-based legitimacy collapses back toward the safeguard or veto frames; if one stabilizes, threshold review acquires an operational standard and recalibration becomes actionable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_rate_measurability, empirical, 'Whether the empirical foundation the reading rests on actually exists at usable precision.').

omega_variable(
    error_weighting_value_smuggle,
    'Calibration requires weighting over-blocking (ossification) against under-blocking (instability) — is that weighting a discoverable fact about reversibility costs, or a value choice dressed as technique?',
    'Run the calibration program under plural explicitly-declared weightings and compare recommended thresholds; the divergence map exposes how much of the recommendation is preference content.',
    'If the weighting is irreducibly preferential, part of the reading''s claimed technical legitimacy is preference in disguise, and the extraction attributable to the calibration program itself should be revised upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(error_weighting_value_smuggle, preference, 'Whether the tradeoff at the heart of evidence-based tuning is empirical or normative.').

omega_variable(
    shielding_beneficiary_composition,
    'Does the threshold''s shielding in practice protect rights-bearing minorities or entrenched economic and partisan interests predominantly?',
    'Code surviving-versus-amended constitutional provisions by beneficiary class across jurisdictions and decades; compare against simulated simple-majority counterfactual passage.',
    'If shielding accrues mostly to entrenched interests, the coordination component thins toward cover and the constraint trends snare; if rights-minority protection dominates, the rope component strengthens and the safeguard sibling gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shielding_beneficiary_composition, empirical, 'Who actually collects the gate''s protective output, which decides how genuine the coordination function is.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__adaptive_gradient_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(supe_tr_t0, observed).
narrative_ontology:measurement(supe_tr_t10, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(supe_tr_t10, observed).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement_basis(supe_tr_t20, observed).
narrative_ontology:measurement(supe_tr_t30, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(supe_tr_t30, observed).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement_basis(supe_tr_t40, observed).
narrative_ontology:measurement(supe_tr_t50, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 50, 0.32).
narrative_ontology:measurement_basis(supe_tr_t50, observed).
narrative_ontology:measurement(supe_tr_t60, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement_basis(supe_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(supe_be_t0, observed).
narrative_ontology:measurement(supe_be_t10, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(supe_be_t10, observed).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement_basis(supe_be_t20, observed).
narrative_ontology:measurement(supe_be_t30, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 30, 0.51).
narrative_ontology:measurement_basis(supe_be_t30, observed).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement_basis(supe_be_t40, observed).
narrative_ontology:measurement(supe_be_t50, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 50, 0.56).
narrative_ontology:measurement_basis(supe_be_t50, observed).
narrative_ontology:measurement(supe_be_t60, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(supe_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(supe_su_t0, observed).
narrative_ontology:measurement(supe_su_t10, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(supe_su_t10, observed).
narrative_ontology:measurement(supe_su_t20, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(supe_su_t20, observed).
narrative_ontology:measurement(supe_su_t30, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 30, 0.57).
narrative_ontology:measurement_basis(supe_su_t30, observed).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 40, 0.59).
narrative_ontology:measurement_basis(supe_su_t40, observed).
narrative_ontology:measurement(supe_su_t50, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 50, 0.61).
narrative_ontology:measurement_basis(supe_su_t50, observed).
narrative_ontology:measurement(supe_su_t60, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement_basis(supe_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__adaptive_gradient_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold__consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold__minoritarian_veto_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the supermajority requirement' covers three structurally distinct claims that decompose per the epsilon-invariance principle. This story (adaptive_gradient_reading) authors epsilon for the standing fixed-threshold arrangement AS SEEN BY the instrument frame: an untuned tool whose over-blocking is measurable extraction riding on a real filtering function. The consensus_safeguard_reading authors epsilon for the same arrangement as an intrinsic good (low extraction, filtering vindicated); the minoritarian_veto_reading authors it as privilege conversion (high extraction, coordination as cover). Same referent arrangement, reading-indexed epsilon values — three files, one kernel, linked edges in all three. Upstream/downstream: the safeguard reading is cited as justification by defenders of current values; the gradient reading's measurement program supplies the evidence base that the veto reading's critique consumes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
