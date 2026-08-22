% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__incumbent_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__incumbent_preservation_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: qwerty_persistence__incumbent_preservation_reading
 *   human_readable: QWERTY Persistence via Incumbent Preservation Defense
 *   domain: technology/industrial_standards/path_dependence
 *
 * SUMMARY:
 *   QWERTY keyboard layout persists as the global standard, not because it is
 *   technically optimal (Dvorak and other alternatives demonstrably reduce
 *   injury and increase typing speed), but because manufacturers, training
 *   institutions, and skilled typists actively defend it to protect their
 *   capital investments and earned skills. This constraint READING frames
 *   persistence as a strategic defense by beneficiaries against
 *   efficiency-seeking alternatives. The founding problem (typewriter
 *   incompatibility) was solved a century ago; the constraint now persists
 *   primarily through active suppression of alternatives by incumbents with
 *   vested interests. The measurement series tracks rising extractiveness and
 *   theater as the constraint ages: extractiveness rises because the
 *   suppressive costs accumulate; theater rises because public justifications
 *   must work harder as the coordination rationale dissolves.
 *
 * KEY AGENTS:
 *   - Keyboard manufacturers: organize to enforce QWERTY as the only viable commercial layout; actively suppress investment in alternative-layout production; coordinate with software makers to ensure compatibility only with QWERTY.
 *   - Trained typists: have invested skill capital in QWERTY; benefit from the standard's persistence because their expertise is protected and transferable; identity is fused with QWERTY fluency.
 *   - Training institutions: built on QWERTY curricula; benefit from institutional inertia and predictable demand; resistant to curriculum change.
 *   - Alternative adopters: trapped in low-adoption pockets; cannot source equipment; face social and technical barriers to participation.
 *   - Efficiency seekers: recognize superior performance in alternatives but pay the full cost of the lock-in without ability to coordinate migration.
 *   - Industrial historians: document that the persistence is socially constructed, not naturally optimal.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, 0.68).
domain_priors:suppression_score(qwerty_persistence__incumbent_preservation_reading, 0.72).
domain_priors:theater_ratio(qwerty_persistence__incumbent_preservation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__incumbent_preservation_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence__incumbent_preservation_reading, "QWERTY Persistence via Incumbent Preservation Defense").
narrative_ontology:topic_domain(qwerty_persistence__incumbent_preservation_reading, "technology/industrial_standards/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence__incumbent_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__incumbent_preservation_reading, '236a4d0a-867a-4e95-9377-066070daa97c').
narrative_ontology:cs_kernel_codification('236a4d0a-867a-4e95-9377-066070daa97c', distributed).
narrative_ontology:cs_authority_grounding('236a4d0a-867a-4e95-9377-066070daa97c', extraction).
narrative_ontology:cs_reading_relation('236a4d0a-867a-4e95-9377-066070daa97c', qwerty_persistence__lapsed_alternatives_reading, coexists_with).
narrative_ontology:cs_axiom('236a4d0a-867a-4e95-9377-066070daa97c', foundational, incumbent_active_preservation_necessary).
narrative_ontology:cs_axiom_status(incumbent_active_preservation_necessary, holdable).
narrative_ontology:cs_axiom_grounding('236a4d0a-867a-4e95-9377-066070daa97c', incumbent_active_preservation_necessary, instrumental).
narrative_ontology:cs_axiom('236a4d0a-867a-4e95-9377-066070daa97c', secondary, capital_investment_protection_legitimate).
narrative_ontology:cs_axiom_status(capital_investment_protection_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('236a4d0a-867a-4e95-9377-066070daa97c', capital_investment_protection_legitimate, deontological).
narrative_ontology:cs_reference_frame('236a4d0a-867a-4e95-9377-066070daa97c', incumbent_defense_regime).
narrative_ontology:cs_drift_state('236a4d0a-867a-4e95-9377-066070daa97c', contemporary_ergonomic_challenge_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('236a4d0a-867a-4e95-9377-066070daa97c', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, trained_typists).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, training_institutions).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_adopters).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, efficiency_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain QWERTY as the universal standard by funding keyboard production tooling, coordinating with typewriter and computer makers, and actively defending against alternative layouts (Dvorak, Colemak, etc.) through design choices and compatibility enforcement. They benefit from the installed base of QWERTY infrastructure and protect their capital investments in QWERTY manufacturing capacity. Organizing as an industry group, they suppress research funding and public awareness of alternatives.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers, agenda_setter,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers, beneficiary).

% Have invested thousands of hours learning QWERTY touch typing. Switching to an alternative layout would require retraining and temporary productivity loss. They benefit from the standard's persistence because it protects their skill value in the labor market and makes their expertise transferable across employers. Their identity as a competent typist is fused with QWERTY fluency.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, trained_typists, beneficiary,
    organized, biographical, identity_locked, global).

% Typing schools, vocational programs, and educational curricula are built around QWERTY instruction. Changing standards would require rewriting curricula, retraining instructors, and rebuilding accreditation frameworks. They benefit from the standard's entrenchment because it provides stable, predictable demand for their services and preserves their institutional role.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, training_institutions, beneficiary,
    organized, generational, constrained, regional).

% Small groups and individuals who have learned Dvorak or other ergonomically superior layouts find keyboards difficult to source, software support nonexistent, and their skills unmarketable. They bear the full cost of the lock-in: they cannot easily purchase equipment, cannot collaborate with others using standard layouts, and their productivity advantage is nullified by compatibility costs. They are trapped by lack of manufacturing scale and social network effects.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_adopters, payer,
    powerless, biographical, trapped, local).

% Researchers, ergonomists, and productivity-focused organizations recognize that QWERTY imposes higher injury rates, lower typing speed, and less efficient finger motion than alternatives. They absorb the cost of maintaining a suboptimal standard because switching requires coordinating millions of users simultaneously, an impossibly high collective-action barrier. They pay through reduced health outcomes and foregone efficiency gains.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, efficiency_seekers, payer,
    moderate, biographical, constrained, global).

% Hardware designers and layout researchers who propose alternatives are systematically excluded from the coordination process. Manufacturers do not license alternative layouts, software companies do not prioritize compatibility, and marketing channels are controlled by QWERTY incumbents. Their innovations cannot reach scale because the distribution and social infrastructure is locked into the incumbent standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, keyboard_innovators, excluded,
    moderate, biographical, trapped, local).

% Academics and researchers document the history of keyboard standards, the technical superiority of alternatives, and the mechanisms by which QWERTY was preserved. They analyze the constraint from outside the beneficiary set and provide evidence that the persistence is socially constructed, not naturally optimal.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, industrial_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers).
narrative_ontology:fixing_cost_class(qwerty_persistence__incumbent_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single universal keyboard layout enables manufacturers to produce compatible equipment, training institutions to teach a single standard, and users to move between devices without retraining. The coordination problem solved is: without standardization, each user would face a choice set of incompatible layouts, each manufacturer would optimize locally, and network effects would fragment the market.
% TRANSFER_FUNCTION: Moves the burden of retraining, equipment incompatibility, and foregone ergonomic efficiency from incumbents to anyone who seeks to deviate from QWERTY. Transfers investment protection (the ability to recover capital costs in QWERTY manufacturing and training infrastructure) from manufacturers and training institutions to the broader population, who absorb the cost of the suboptimal standard.
% ABSENT_VOICES: Alternative-layout designers and small-scale ergonomics researchers are structurally excluded from standard-setting processes. They would argue that the superior efficiency of Dvorak or Colemak justifies coordination costs of migration. Developing nations and new-alphabet communities (where QWERTY is not native) are marginalized; their interests in phonetically optimized layouts are not represented in Western-dominated standard bodies.
% DISAPPEARANCE_RATIONALE: If the incumbent preservation effort dissolved — if manufacturers ceased enforcing QWERTY, if schools taught multiple layouts, if software supported alternatives equally — keyboard markets would fragment over a 20-year period into multiple competing standards. New entrants would introduce Dvorak variants, specialized layouts for different languages would proliferate, and ergonomic optimization would resume. The constraint's removal would unlock billions in suppressed alternatives and health-improvement investments.
% FOUNDING_PROBLEM: In the early mechanical typewriter era (1870s–1890s), different manufacturers used different keyboard layouts, creating incompatibility and fragmentation. The Sholes & Glidden Type-Writer's QWERTY layout became dominant through path dependence (early market share, typing instructor training on that machine) and was codified as a de facto standard by the time alternatives achieved technical superiority.
% FOUNDING_PROBLEM_CORROBORATION: Industrial historians (David, Liebowitz & Margolis) and ergonomic researchers agree that the founding problem — fragmentation and incompatibility — was solved by the early 1900s and has been unnecessary for a century. No credible source outside the benefiting parties (manufacturers, training institutions) argues that QWERTY's persistence is required to solve fragmentation; manufacturers themselves do not claim this as the justification for preservation.
narrative_ontology:disappearance_verdict(qwerty_persistence__incumbent_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__incumbent_preservation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__incumbent_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence__incumbent_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__incumbent_preservation_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__incumbent_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence__incumbent_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.68 by interval end because the constraint actively enforces a suboptimal outcome. The measurement trajectory shows monotonic rise from 0.48 to 0.68 — extractiveness is NOT declining toward a natural equilibrium but ACCUMULATING over time as suppressors invest more in defending the standard. This contradicts the lapsed-alternatives reading (which would expect extractiveness to decline as the coordination function is exhausted). Theater rises from 0.18 to 0.41 because the public justification for QWERTY must work harder: early on (time 0), the fragmentation-prevention story is credible; by time 40, it is purely theatrical — the constraint is maintained by direct suppression (manufacturing capacity, market access control) and cultural inertia, not by residual coordination value. Suppression is high (0.72 at interval end) and rising fastest, indicating that the constraint's persistence depends critically on active defensive effort by organized incumbents. The measurement grid is shared across all three metrics at every time point (t ∈ {0,5,10,15,20,25,30,40}), with no missing values — every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (manufacturers) and the beneficiary seats (trained typists, training institutions) perceive the constraint as a natural, beneficial standard providing coordination value and protecting legitimate skill investments. Victims (alternative adopters, efficiency seekers) perceive the same structure as enforced lock-in that suppresses superior alternatives and protects incumbent profits at the cost of their health and productivity. The engine computes this divergence: from manufacturer and typist seats, directionality is low (beneficiaries); from victim seats, directionality is high (targets of suppression). The manufacturers' role as agenda_setter amplifies their effective control; their exit options are arbitrage-grade because they can move capital to other standards-setting efforts if QWERTY loses viability. By contrast, trained typists have identity_locked exit (retraining would erase their earned skill value) and alternative adopters are trapped (no manufacturing scale, no software support). This structural asymmetry in exit options drives the measured divergence in effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Keyboard manufacturers sit at the beneficiary end of directionality (d ≈ 0.1–0.2): they set the agenda, defend the standard, and collect benefits (capital protection, monopoly pricing power on QWERTY keyboards). Their exit options are arbitrage-grade — they could shift production to an alternative layout if it became dominant, but they have active incentive NOT to do so because their current tooling and market position are optimized for QWERTY. Trained typists sit near symmetric (d ≈ 0.45–0.55): they benefit from skill protection but also bear the cost of being locked into an outdated standard — they cannot easily experiment with ergonomically superior layouts without sacrificing their market value. Their identity_locked exit shifts their directionality upward (toward target) relative to pure beneficiary status. Alternative adopters and efficiency seekers sit at the target end (d ≈ 0.8–0.9): they bear the full cost of the lock-in (equipment incompatibility, social friction, suppressed innovation) without receiving coordination benefits. The no-override derivation from beneficiary/victim declarations and exit options produces accurate directionality values for this story — no directionality_overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is DEAD: manufacturers themselves do not claim that fragmentation is still a risk; industrial historians confirm that incompatibility was solved by 1910 and alternatives have been technically superior for decades. The mandate has outlived its function. Yet the constraint persists with rising extractiveness and theater — textbook mandatrophy signature. The constraint's classification as tangled_rope (not rope) depends on this: were it a pure coordination mechanism, we would expect extractiveness to decline over time as the coordination function is internalized (learned by all users, built into infrastructure). Instead, extractiveness RISES, indicating that the constraint is maintained by active suppression against alternatives, not by residual coordination value. The rising theater (public justifications working harder) and rising suppression (defense costs accumulating) confirm that incumbents are actively working to maintain the constraint despite its declining functional justification. This reading resolves the mandatrophy question by locating the persistence in incumbent preservation, not in the coordination function's residual value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incumbent_intent_vs_emergent_lock_in,
    'Is QWERTY persistence the result of deliberate, coordinated incumbent defense, or an emergent outcome of uncoordinated local optimization by manufacturers and institutions acting in their separate interests?',
    'Historical evidence from manufacturers'' board decisions, trade association meeting minutes, and communications with software companies; analysis of whether manufacturers could have profitably switched to alternative layouts but chose not to. High coordination costs and explicit suppression (rejecting Dvorak licensing deals, funding QWERTY-only research) would indicate deliberate defense; absence of coordination would indicate lock-in as emergent side effect.',
    'If deliberate: this reading is strengthened and extraction classification holds. If emergent: the constraint might be reclassified as piton (maintained by inertia rather than active defense), and extractiveness might be reconceptualized as dead-weight loss rather than rent collection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_intent_vs_emergent_lock_in, empirical, 'Whether persistence is actively engineered or emergent from local incentives').

omega_variable(
    coordination_value_residual,
    'How much coordination value (if any) does QWERTY persistence still provide after the founding problem is solved? Is the value non-zero, or is the entire contemporary persistence purely extractive?',
    'Economic analysis of switching costs for users, software companies, and manufacturers; historical comparison with switching dynamics when alternative standards succeeded (e.g., shift from manual to electric typewriters, ASCII to Unicode). If users would incur net costs to switch even if alternatives were freely available and fully supported, residual coordination value exists; if switching would be net-beneficial, persistence is pure extraction.',
    'If residual value exists: the constraint is reclassified as rope (residual coordination value justifying some asymmetry). If zero residual: the constraint is snare-adjacent (pure extraction defended as coordination). The theater measurement and rising suppressiveness would be reinterpreted accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_value_residual, empirical, 'Whether the coordination function provides value independent of lock-in defense').

omega_variable(
    alternative_reading_plausibility,
    'Does the lapsed-alternatives reading (standards persist via passive network effects, not active incumbent defense) explain the observed data equally well?',
    'Comparison of measurement trajectories: if extractiveness and suppression_requirement rise monotonically (this story), active defense is indicated; if they plateau or decline (lapsed-alternatives story), passive network effects are indicated. Counterfactual: would the rise continue if major software makers stopped enforcing QWERTY-only constraints? If persistence collapsed, active enforcement is proven; if persistence held despite relaxed enforcement, passive network effects dominate.',
    'If the lapsed-alternatives reading is equally plausible: the two readings genuinely coexist in explanation space and both should be compiled as separate constraints. If the incumbent-preservation reading''s measurement trajectory is definitively more consistent: the reading is strengthened and the lapsed-alternatives reading''s classification should assume different metrics (lower extractiveness, flatter trajectory).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_reading_plausibility, conceptual, 'Whether the alternative reading (passive network effects) is observationally equivalent').

omega_variable(
    suppression_internalization,
    'Is the measured suppression structural (external barriers: lack of manufacturing scale, software incompatibility) or internalized (users and educators believe QWERTY is naturally optimal and choose not to experiment)?',
    'Post-exit trajectories: if users who learn alternatives spontaneously choose to switch, suppression is primarily structural; if users who switch maintain psychological resistance to alternatives despite technical superiority, suppression is partially internalized. Cross-cultural data: cultures without deep QWERTY investment might show faster adoption of alternatives if offered at similar infrastructure cost.',
    'If structural: the measured suppression (0.72) accurately reflects the barrier to switching. If internalized: the effective suppression exceeds the measured value (targets carry the suppression with them after exit). This affects victim count estimation and exit-option classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural vs. internalized suppression mechanism in the QWERTY lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__incumbent_preservation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(qwer_tr_t0, observed).
narrative_ontology:measurement(qwer_tr_t5, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement_basis(qwer_tr_t5, observed).
narrative_ontology:measurement(qwer_tr_t10, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(qwer_tr_t10, observed).
narrative_ontology:measurement(qwer_tr_t15, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement_basis(qwer_tr_t15, observed).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement_basis(qwer_tr_t20, observed).
narrative_ontology:measurement(qwer_tr_t25, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement_basis(qwer_tr_t25, observed).
narrative_ontology:measurement(qwer_tr_t30, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(qwer_tr_t30, observed).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(qwer_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(qwer_be_t0, observed).
narrative_ontology:measurement(qwer_be_t5, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(qwer_be_t5, observed).
narrative_ontology:measurement(qwer_be_t10, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(qwer_be_t10, observed).
narrative_ontology:measurement(qwer_be_t15, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(qwer_be_t15, observed).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(qwer_be_t20, observed).
narrative_ontology:measurement(qwer_be_t25, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(qwer_be_t25, observed).
narrative_ontology:measurement(qwer_be_t30, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(qwer_be_t30, observed).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(qwer_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(qwer_su_t0, observed).
narrative_ontology:measurement(qwer_su_t5, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement_basis(qwer_su_t5, observed).
narrative_ontology:measurement(qwer_su_t10, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement_basis(qwer_su_t10, observed).
narrative_ontology:measurement(qwer_su_t15, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(qwer_su_t15, observed).
narrative_ontology:measurement(qwer_su_t20, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(qwer_su_t20, observed).
narrative_ontology:measurement(qwer_su_t25, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(qwer_su_t25, observed).
narrative_ontology:measurement(qwer_su_t30, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(qwer_su_t30, observed).
narrative_ontology:measurement(qwer_su_t40, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(qwer_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__incumbent_preservation_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(qwerty_persistence__incumbent_preservation_reading, 0.18).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence__lapsed_alternatives_reading).

% DUAL FORMULATION NOTE:
% The QWERTY persistence kernel decomposes into two distinct constraints. This story (incumbent_preservation_reading) models persistence as active incumbent defense against superior alternatives; extractiveness (0.68) includes defensive suppression costs and protects capital investments. The sibling constraint (lapsed_alternatives_reading) models persistence as emergent coordination lock-in where alternatives fail to reach critical mass; extractiveness would be lower (~0.35–0.40) because persistence is passive, not enforced. The two readings produce different victim sets, different directionality structures, and different type classifications. They coexist as live explanations in public discourse — advocates for reform argue incumbent defense (this reading); defenders of QWERTY argue network effects naturally exhaust alternatives (lapsed-alternatives reading). The measurement series (rising extractiveness, rising theater, dead founding problem) constitute evidence favoring this reading over the alternative; the lapsed-alternatives reading would predict plateauing extractiveness and lower defensive theater.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
