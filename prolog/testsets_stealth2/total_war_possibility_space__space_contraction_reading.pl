% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__space_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__space_contraction_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: total_war_possibility_space__space_contraction_reading
 *   human_readable: Total-War Possibility Space — Space-Contraction Reading
 *   domain: international relations/strategic studies/institutional history
 *
 * SUMMARY:
 *   Between August 1945 and the consolidation of second-strike forces, the
 *   option of total war between peer great powers left the planning space of
 *   every major military simultaneously. This story instantiates the
 *   space-contraction reading of that transformation: the removal is
 *   categorical, grounded in the material fact that thermonuclear exchange
 *   admits no theory of victory, and it therefore requires no enforcement, no
 *   custodian, and no maintenance — the condition stands the way a phase
 *   boundary stands. Its operation is registered negatively: mobilization
 *   doctrine disappeared, general-staff war-gaming of peer total war ceased,
 *   and strategic studies re-founded itself on limited war, deterrence
 *   management, and escalation control. The story is authored as one reading
 *   of a contested kernel; the committer structure — which kernel, which
 *   reading, what siblings would change — is routed to the omega variables
 *   and the kernel_context note, not averaged into the metrics. Claim and
 *   metrics are authored independently: the claim is mountain; the metrics
 *   describe near-zero extraction, near-zero theater, near-complete
 *   accessibility collapse, and negligible resistance — a profile the engine
 *   is free to confirm or overturn.
 *
 * KEY AGENTS:
 *   - - great_power_general_staffs: Primary cost-bearing seat (institutional/identity_locked) — bears the obsolescence of the profession's founding object; no compensating flow exists
 *   - - great_power_political_leaderships: Governed seat (institutional/trapped) — conducts strategy under permanent foreclosure of the decisive-war instrument
 *   - - mobilization_dependent_institutions: Concentrated historical cost-bearers (organized/trapped) — absorbed the dismantling of society-wide war preparation
 *   - - non_nuclear_allied_states: Incidental beneficiary (moderate/constrained) — collects security without operating anything; not a rent-collector
 *   - - great_power_civilian_populations: Universal diffuse beneficiary (powerless/trapped) — collects survival; exercised no choice in the trade
 *   - - strategic_studies_community: Analytical observer (organized/analytical) — maps the condition; collects relevance, enforces nothing
 *   - - nuclear_weapons_complex: Material-basis custodian in observational seat (institutional/analytical) — maintains the arsenals, administers nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.1).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.08).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.06).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.06).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Total-War Possibility Space — Space-Contraction Reading").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international relations/strategic studies/institutional history").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, '8b080d71-f9a3-43e0-87ba-c0dce946d3ce').
narrative_ontology:cs_kernel_codification('8b080d71-f9a3-43e0-87ba-c0dce946d3ce', distributed).
narrative_ontology:cs_authority_grounding('8b080d71-f9a3-43e0-87ba-c0dce946d3ce', distributed).
narrative_ontology:cs_reading_relation('8b080d71-f9a3-43e0-87ba-c0dce946d3ce', total_war_possibility_space__deterrence_equilibrium_reading, forecloses).
narrative_ontology:cs_reading_relation('8b080d71-f9a3-43e0-87ba-c0dce946d3ce', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('8b080d71-f9a3-43e0-87ba-c0dce946d3ce', foundational, no_theory_of_victory_exists).
narrative_ontology:cs_axiom_status(no_theory_of_victory_exists, holdable).
narrative_ontology:cs_axiom_grounding('8b080d71-f9a3-43e0-87ba-c0dce946d3ce', no_theory_of_victory_exists, empirically_contingent).
narrative_ontology:cs_axiom('8b080d71-f9a3-43e0-87ba-c0dce946d3ce', secondary, strategic_possibility_space_materially_bounded).
narrative_ontology:cs_axiom_status(strategic_possibility_space_materially_bounded, holdable).
narrative_ontology:cs_axiom_grounding('8b080d71-f9a3-43e0-87ba-c0dce946d3ce', strategic_possibility_space_materially_bounded, empirically_contingent).
narrative_ontology:cs_reference_frame('8b080d71-f9a3-43e0-87ba-c0dce946d3ce', post_hiroshima_material_fixity).
narrative_ontology:cs_drift_state('8b080d71-f9a3-43e0-87ba-c0dce946d3ce', contemporary_second_strike_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8b080d71-f9a3-43e0-87ba-c0dce946d3ce', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, non_nuclear_allied_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, great_power_civilian_populations).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, great_power_general_staffs).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, great_power_political_leaderships).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, mobilization_dependent_institutions).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, revolution_in_strategic_affairs_thesis).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, absolute_weapon_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherited a profession whose core object was planning decisive war between peer states — mobilization schedules, campaign designs, theories of victory. Since 1945 that object has been unavailable: staffs rewrote curricula around limited war, crisis management, and escalation control; mobilization tables became historical artifacts; the senior war-game cells stopped gaming peer total war decades ago. Members cannot leave the institutions whose founding function evaporated — retirement aside, the profession is the identity, and the identity now points at a hole.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, great_power_general_staffs, payer,
    institutional, generational, identity_locked, global).

% Command the armed instruments of major states and conduct strategy under a permanent foreclosure: no plan toward decisive victory over a peer state can be entertained, funded, or signaled. Crises are managed inside a box whose far wall is societal annihilation. The foreclosure cannot be exited — abandoning nuclear weapons would not restore the total-war option, it would only remove the guarantee that peers remain similarly boxed.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, great_power_political_leaderships, payer,
    institutional, generational, trapped, global).

% Draft apparatuses, war-college total-war faculties, industrial mobilization planning boards, and national emergency agencies were built to prepare whole societies for peer war. Their function was withdrawn within a generation of 1945: conscription wound down or ended, mobilization boards were disbanded or repurposed toward civil defense and continuity-of-government, and faculty lines shifted to deterrence and area studies. The people in them bore the adjustment without any compensating transfer.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, mobilization_dependent_institutions, payer,
    organized, biographical, trapped, national).

% Plan their defense on the presupposition that peer total war is off the table, and collect the resulting security without operating any part of the arrangement that produces it. Their exposure is subordination: crisis outcomes among nuclear patrons bind them without their consent, and extended-assurance politics prices their autonomy.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, non_nuclear_allied_states, beneficiary,
    moderate, generational, constrained, regional).

% Live inside the first strategic era in which a peer war's binding outcome is societal destruction, and collect the survival that follows from the destroyed option. They chose nothing: the trade of the total-war instrument for permanent annihilation-risk exposure was executed by elites across 1945-1962 and inherited by everyone since.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, great_power_civilian_populations, beneficiary,
    powerless, generational, trapped, global).

% Universities, independent research institutes, and specialist journals that formed around analyzing a strategic world from which total war had been removed. They map the condition's consequences, date its arrival, and argue its boundaries; the analytic terrain it opened sustains careers, journals, and funding lines. They enforce nothing and alter nothing material — the condition holds whether or not they describe it accurately.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, strategic_studies_community, observer,
    organized, biographical, analytical, global).

% Designs, builds, and maintains the arsenals whose effects constitute the material basis of the condition. Under this reading it administers no part of the condition itself — the impossibility requires no custodian — and its own planning documents increasingly treat peer nuclear war as a boundary condition to be avoided rather than an operable option to be refined.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, nuclear_weapons_complex, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_possibility_space__space_contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(total_war_possibility_space__space_contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removes the total-war branch from every great power's decision tree simultaneously, fixing a common outer boundary of the strategic game: no power must plan against a peer's pursuit of decisive societal victory, and military planning concentrates on limited war, crisis management, and escalation control.
% TRANSFER_FUNCTION: Nothing systematically transfers. The arrangement's operation destroys options rather than moving resources: planning vocations, mobilization capacity, and doctrinal freedom are dissipated where they stand, accruing to no receiving seat.
% ABSENT_VOICES: The pre-1945 strategic tradition — the total-war planning profession itself — was invalidated without a hearing; its practitioners adapted or retired rather than consenting. Civilian populations of the great powers were never consulted on the exchange of the total-war instrument for permanent annihilation-risk exposure. Non-nuclear states sit outside the great-power conversations in which the possibility space is defined.
% DISAPPEARANCE_RATIONALE: If total war re-entered the thinkable overnight — if a theory of victory somehow became available — every great-power military would rebuild mobilization and war-winning apparatus within a decade, alliance structures would re-price around conquest feasibility, and the entire post-1945 assumption that peer competition terminates in negotiation rather than annihilation would dissolve. The long peace's institutional architecture presupposes this condition at every joint.
% FOUNDING_PROBLEM: Not designed but crystallized: the arrangement emerged from the material fact demonstrated in August 1945 that peer societies could destroy one another faster than they could recover. The problem it resolved was the runaway escalatory trajectory of great-power war visible across 1914-1945 — each industrialized conflict wider and more destructive than the last, with no stabilizing terminus.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside any benefiting party: adversary archives agree — Soviet leadership records (Khrushchev's explicit renunciation of war as policy instrument, 1961) and American records (Eisenhower's refusal to treat nuclear war plans as usable policy) independently attest that both blocs treated peer total war as removed from operable planning, across opposing ideologies and interests. Diplomatic historians working the 1914-1945 record corroborate the founding problem's reality independent of any nuclear-establishment interest. No party profits from the attestation.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__space_contraction_reading, 0.1, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__space_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_possibility_space__space_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.10 at interval end) because the condition transfers nothing to anyone: the costs it imposes — foreclosed policy instruments, devalued planning vocations, dismantled mobilization apparatus — dissipate where they fall, and the measurement series shows the burden front-loaded in the first two decades (0.24 in 1945, as the total-war profession's capital was written off) decaying toward a stable floor as cohorts turned over. Suppression is authored near-zero (0.08) because nothing enforces the condition: no machinery punishes total-war theorizing, and the mild professional discount such theorizing attracts is cultural residue, not enforcement — accordingly no suppression_requirement series is authored, since the enforcement picture is static and the scalar carries it. Theater is near-zero (0.06) because nothing requires performing: commemorative ritual and exercise pageantry rose slightly in the series' back half, but they perform seriousness about adjacent questions (assurance, taboo) rather than maintaining this condition, which holds unperformed. Accessibility collapse is high (0.90): once weapons effects are understood, no doctrinal ingenuity reconstructs a winnable peer total war — alternatives do not merely look bad, they stop being formulable. Resistance is low (0.08): episodic war-fighting schools (early 1950s tactical nuclearism, early 1980s counterforce revival) met rapid professional dismissal and left no sustained program. Boltzmann coordination typing is deliberately omitted: the condition solves no collective-action problem through any mechanism — it deletes a branch of the game tree — so no coordination_type fits and none is declared. All temporal series share one nine-point grid (1945-2025 at decade steps) with every tracked metric authored at every point; the series are monotone rather than cyclical because there is no enforcement cycle to oscillate — periodic crises (Berlin, Cuba) raised the salience of the foreclosed option without moving the underlying condition the grid samples.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical material facts. From the general staffs' position the condition is a vocational catastrophe absorbed in silence — a profession whose object vanished mid-career for its senior cohort. From the leaderships' position it is a permanently locked door in their own house: an instrument they own and may never use. From the mobilization institutions' position it was a decommissioning notice. From the civilian and allied seats it is an invisible floor: nobody experiences the non-occurrence of total war as an event. From the analytical seat it is the founding terrain of a discipline. The staffs' seat is additionally identity-locked: professional self-concept was constituted through mastery of total-war planning, so the condition did not release its subjects — it hollowed them, and the atrophy completes only as generations without the inheritance replace those with it. Nothing in the structural data privileges one of these experiences; the engine derives per-seat classifications from power, exit, and position, and the divergence between the staffs' lived loss and the populations' unexperienced safety is the perspectival content this story exists to preserve.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries and no victims are declared in base_properties, deliberately. The condition's costs dissipate (obsolescence, decommissioning) and its benefits are universal and diffuse (continued existence); no seat receives a transfer, so the beneficiary/victim arrays — which drive the directionality derivation — are left empty rather than filled with seats that collect nothing collectible. The incidental beneficiaries seated on the stakeholder surface (non_nuclear_allied_states, great_power_civilian_populations) hold role=beneficiary because they genuinely collect security-grade goods without operating anything, but they are kept out of the beneficiaries array because collection is not capture: declaring them would assert a rent flow that does not exist and would misroute false-summit evaluation. With no structural asymmetry declared, derived directionality sits near symmetric for every seat, effective extraction tracks the low base rate everywhere, and the computed profile should land where the reading says it must: a condition nobody defends, nobody enforces, and nobody collects from. No directionality overrides are authored — with empty arrays there is no derivation to correct, and the coarse per-power-atom override surface could not distinguish the staffs' cost-bearing position from the leaderships' governed position without misapplying to both.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy barely arises for an emergent condition — there is no mandate to outlive its function, because there was never a mandate. The classification work is the reverse: preventing the long peace from being misread as a maintained arrangement. If the condition were maintained (as the sibling readings hold), its maintenance would be either an enforcement service (coordination with overhead) or a protection racket (extraction dressed as peace), and the theater and suppression scalars would need re-authoring upward. This story's low theater_ratio is therefore a substantive claim, not a default: it asserts that the ceremonies surrounding nuclear weapons maintain assurance and norms, not thinkability. The R5 fields register the residual dispute honestly — the founding problem's status is contested precisely because the sibling readings disagree about whether the runaway-war problem is closed, managed, or contained — and the corroboration entry cites adversary archives so the genealogy does not rest on any beneficiary's testimony. If the space-contraction reading is right, every maintenance apparatus is superfluous ceremony; if a sibling is right, this story has mismeasured theater and suppression, and the corpus comparison across the three files is the instrument that decides.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the total_war_possibility_space kernel correctly characterizes the removed option''s modal status — categorically impossible (this reading, space_contraction_reading), reachable-but-deterred (deterrence_equilibrium_reading), or normatively prohibited (nuclear_taboo_reading)?',
    'Adversarial comparison of the three stories'' distinctive predictions against the record: this reading predicts atrophy of the total-war planning apparatus, the deterrence reading predicts continuously maintained war-fighting readiness priced against mutual vulnerability, and the taboo reading predicts discourse patterns tracking norm enforcement rather than material capability.',
    'Resolution relocates the constraint across categories: confirmed material impossibility preserves the mountain profile; confirmed maintenance flips the story toward coordinated or enforced structures with real extraction, theater, and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame contest: this story is one reading of a three-reading kernel; the siblings are separate constraint files, not parameters of this one.').

omega_variable(
    categorical_boundary_location,
    'Is the removal categorical for all peer total war, or does some operable variant (counterforce-limited exchange, escalation-dominant short war) remain inside the planning space?',
    'Declassified war-game records and current doctrine review: if any staff cell gamed a peer total-war variant as operable rather than as boundary illustration, the categorical claim fails at that edge.',
    'A surviving operable variant contracts the constraint from categorical to conditional and shifts classification toward the deterrence reading''s territory; full categorical closure anchors the mountain profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_boundary_location, empirical, 'Edge location of the possibility-space contraction.').

omega_variable(
    atrophy_prediction_evidence,
    'Did the reading''s distinctive structural delta actually occur — mobilization doctrine disappearing, general-staff war-gaming of peer total war ceasing, and strategic studies shifting to sub-nuclear domains?',
    'Longitudinal institutional data: war-college curricula, staff organization charts, war-game topic distributions, and journal topic frequencies sampled across 1945-2025.',
    'Confirmed atrophy corroborates the categorical reading; sustained apparatus maintenance would indicate the option remained live and planned-for, supporting the deterrence reading instead.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(atrophy_prediction_evidence, empirical, 'Tests the reading''s own predicted institutional signature.').

omega_variable(
    material_versus_epistemic_closure,
    'Is the unthinkability wholly material, or does a maintained epistemic convention carry part of the closure — would total-war planning reformulate if the professional sanction against it were lifted?',
    'Natural experiments in doctrinal ferment (early 1950s tactical-nuclear war-fighting debates, early 1980s counterforce revival): whether loosened sanction produced operable total-war planning or only boundary illustration.',
    'If the sanction is load-bearing, part of the closure is socially maintained and the story''s suppression and theater scalars are understated; if wholly material, the mountain profile stands unqualified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_versus_epistemic_closure, conceptual, 'Distinguishes physical closure from professionally policed closure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twp_scr_tr_t1945, total_war_possibility_space__space_contraction_reading, theater_ratio, 1945, 0.03).
narrative_ontology:measurement_basis(twp_scr_tr_t1945, observed).
narrative_ontology:measurement(twp_scr_tr_t1955, total_war_possibility_space__space_contraction_reading, theater_ratio, 1955, 0.04).
narrative_ontology:measurement_basis(twp_scr_tr_t1955, observed).
narrative_ontology:measurement(twp_scr_tr_t1965, total_war_possibility_space__space_contraction_reading, theater_ratio, 1965, 0.04).
narrative_ontology:measurement_basis(twp_scr_tr_t1965, observed).
narrative_ontology:measurement(twp_scr_tr_t1975, total_war_possibility_space__space_contraction_reading, theater_ratio, 1975, 0.05).
narrative_ontology:measurement_basis(twp_scr_tr_t1975, observed).
narrative_ontology:measurement(twp_scr_tr_t1985, total_war_possibility_space__space_contraction_reading, theater_ratio, 1985, 0.05).
narrative_ontology:measurement_basis(twp_scr_tr_t1985, observed).
narrative_ontology:measurement(twp_scr_tr_t1995, total_war_possibility_space__space_contraction_reading, theater_ratio, 1995, 0.05).
narrative_ontology:measurement_basis(twp_scr_tr_t1995, observed).
narrative_ontology:measurement(twp_scr_tr_t2005, total_war_possibility_space__space_contraction_reading, theater_ratio, 2005, 0.05).
narrative_ontology:measurement_basis(twp_scr_tr_t2005, observed).
narrative_ontology:measurement(twp_scr_tr_t2015, total_war_possibility_space__space_contraction_reading, theater_ratio, 2015, 0.06).
narrative_ontology:measurement_basis(twp_scr_tr_t2015, observed).
narrative_ontology:measurement(twp_scr_tr_t2025, total_war_possibility_space__space_contraction_reading, theater_ratio, 2025, 0.06).
narrative_ontology:measurement_basis(twp_scr_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(twp_scr_be_t1945, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1945, 0.24).
narrative_ontology:measurement_basis(twp_scr_be_t1945, observed).
narrative_ontology:measurement(twp_scr_be_t1955, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1955, 0.2).
narrative_ontology:measurement_basis(twp_scr_be_t1955, observed).
narrative_ontology:measurement(twp_scr_be_t1965, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1965, 0.17).
narrative_ontology:measurement_basis(twp_scr_be_t1965, observed).
narrative_ontology:measurement(twp_scr_be_t1975, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1975, 0.15).
narrative_ontology:measurement_basis(twp_scr_be_t1975, observed).
narrative_ontology:measurement(twp_scr_be_t1985, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1985, 0.14).
narrative_ontology:measurement_basis(twp_scr_be_t1985, observed).
narrative_ontology:measurement(twp_scr_be_t1995, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1995, 0.13).
narrative_ontology:measurement_basis(twp_scr_be_t1995, observed).
narrative_ontology:measurement(twp_scr_be_t2005, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2005, 0.12).
narrative_ontology:measurement_basis(twp_scr_be_t2005, observed).
narrative_ontology:measurement(twp_scr_be_t2015, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2015, 0.11).
narrative_ontology:measurement_basis(twp_scr_be_t2015, observed).
narrative_ontology:measurement(twp_scr_be_t2025, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2025, 0.1).
narrative_ontology:measurement_basis(twp_scr_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_possibility_space__space_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% Family decomposition of the kernel total_war_possibility_space. The colloquial label 'the nuclear peace' (or 'the long peace') conflates three structurally distinct claims with different epsilons, cost-bearing sets, and maintenance profiles: material impossibility (this file), equilibrium under mutual vulnerability (deterrence_equilibrium_reading), and constructed normative prohibition (nuclear_taboo_reading). This reading is upstream: the material fact it formalizes is cited as evidence by both siblings, so its edges point to each. Per the epsilon-invariance principle the siblings are separate stories, not parameters of this one; cross-file comparison of their theater and suppression scalars is the instrument that adjudicates the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
