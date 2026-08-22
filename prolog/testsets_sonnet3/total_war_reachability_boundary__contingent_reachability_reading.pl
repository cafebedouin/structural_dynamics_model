% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contingent_reachability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contingent_reachability_reading, []).

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
 *   constraint_id: total_war_reachability_boundary__contingent_reachability_reading
 *   human_readable: Contingent Reachability of Total War Under Present Technological Equilibrium
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This story instantiates the contingent-reachability reading of the
 *   total-war-reachability-boundary kernel: total war's current
 *   unreachability is not a permanent contraction of the strategic
 *   possibility space (as the contraction_reading holds) nor merely a
 *   low-but-persistent probability within an unchanged reachable set (as the
 *   dropping_reading holds), but a scaffold — a temporary technological
 *   equilibrium (mutual survivable second-strike, unreliable counterforce,
 *   imperfect early warning and missile defense) that specific states are
 *   actively working to dismantle through counterforce precision, hypersonic
 *   delivery, cyber attacks on command-and-control, and missile defense
 *   investment. The theater_ratio is authored high in the early period
 *   (arms-control diplomacy, MAD doctrine, summit theater) precisely because
 *   the appearance of a settled boundary was itself part of what stabilized
 *   the postwar order even as the underlying technology quietly kept moving.
 *   As the interval progresses, theater_ratio falls and
 *   extractiveness/suppression rise as the destabilizing investments mature
 *   and become harder to conceal or reverse diplomatically.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, 0.42).
domain_priors:suppression_score(total_war_reachability_boundary__contingent_reachability_reading, 0.38).
domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contingent_reachability_reading, scaffold).
narrative_ontology:human_readable(total_war_reachability_boundary__contingent_reachability_reading, "Contingent Reachability of Total War Under Present Technological Equilibrium").
narrative_ontology:topic_domain(total_war_reachability_boundary__contingent_reachability_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__contingent_reachability_reading).
narrative_ontology:has_sunset_clause(total_war_reachability_boundary__contingent_reachability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contingent_reachability_reading, 'ea9ad5c0-7c9e-4c63-8030-7b8e26d566e7').
narrative_ontology:cs_kernel_codification('ea9ad5c0-7c9e-4c63-8030-7b8e26d566e7', distributed).
narrative_ontology:cs_authority_grounding('ea9ad5c0-7c9e-4c63-8030-7b8e26d566e7', distributed).
narrative_ontology:cs_reading_relation('ea9ad5c0-7c9e-4c63-8030-7b8e26d566e7', total_war_reachability_boundary__contraction_reading, influences).
narrative_ontology:cs_reading_relation('ea9ad5c0-7c9e-4c63-8030-7b8e26d566e7', total_war_reachability_boundary__dropping_reading, coexists_with).
narrative_ontology:cs_axiom('ea9ad5c0-7c9e-4c63-8030-7b8e26d566e7', foundational, reachability_boundary_is_technologically_contingent).
narrative_ontology:cs_axiom_status(reachability_boundary_is_technologically_contingent, holdable).
narrative_ontology:cs_axiom_grounding('ea9ad5c0-7c9e-4c63-8030-7b8e26d566e7', reachability_boundary_is_technologically_contingent, empirically_contingent).
narrative_ontology:cs_axiom('ea9ad5c0-7c9e-4c63-8030-7b8e26d566e7', secondary, technology_investment_is_boundary_relevant_not_merely_symbolic).
narrative_ontology:cs_axiom_status(technology_investment_is_boundary_relevant_not_merely_symbolic, holdable).
narrative_ontology:cs_axiom_grounding('ea9ad5c0-7c9e-4c63-8030-7b8e26d566e7', technology_investment_is_boundary_relevant_not_merely_symbolic, empirically_contingent).
narrative_ontology:cs_reference_frame('ea9ad5c0-7c9e-4c63-8030-7b8e26d566e7', cold_war_mutual_vulnerability_equilibrium).
narrative_ontology:cs_drift_state('ea9ad5c0-7c9e-4c63-8030-7b8e26d566e7', post_hypersonic_cyber_c2_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ea9ad5c0-7c9e-4c63-8030-7b8e26d566e7', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_counterforce_and_missile_defense).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_hypersonic_and_cyber_first_strike_capability).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, arms_control_verification_regime_administrators).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, global_civilian_populations).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, non_nuclear_states_within_strike_range).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, future_generations_under_reversed_boundary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pours resources into counterforce accuracy, missile defense, and command-and-control resilience specifically because the current boundary — where a disarming first strike or survivable retaliation is technically unreliable — is understood internally as an engineering problem, not a settled fact. Gains relative strategic position and negotiating leverage from every increment that narrows the technical gap, while publicly endorsing deterrence stability rhetoric that assumes the gap is fixed.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_counterforce_and_missile_defense, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_counterforce_and_missile_defense, agenda_setter).

% Develops hypersonic glide vehicles, cyber-attacks against nuclear command and control, and AI-assisted targeting explicitly to compress adversary decision time and degrade second-strike survivability. Benefits from the technology race itself regardless of whether the boundary actually moves, since the investment secures domestic budget share, alliance leverage, and industrial-base advantage.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, states_investing_in_hypersonic_and_cyber_first_strike_capability, beneficiary,
    institutional, generational, arbitrage, global).

% Administers treaty verification, satellite monitoring, and confidence-building measures premised on the current technical boundary holding. Their institutional relevance, budgets, and diplomatic standing depend on that boundary being treated as durable even as they privately track the erosion; they have strong incentive to manage disclosure of destabilizing developments gradually rather than declare the boundary already moving.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, arms_control_verification_regime_administrators, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__contingent_reachability_reading, arms_control_verification_regime_administrators, beneficiary).

% Bear the entire tail risk of the boundary reversing. Have no voice in the technology-race decisions, no ability to verify whether the boundary is holding or eroding, and no exit from the geography of exposure. If deterrence fails after the boundary moves, this population absorbs the consequence entirely; if it never moves, they absorb nothing but were never consulted on the wager either way.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, global_civilian_populations, payer,
    powerless, civilizational, trapped, universal).

% Sit inside the geographic envelope that a reachability reversal would place at risk without holding any of the capabilities driving the race. Can lobby, host early-warning infrastructure, or seek extended-deterrence guarantees, but cannot alter the underlying technological trajectory that determines whether the boundary holds for them.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, non_nuclear_states_within_strike_range, payer,
    moderate, generational, constrained, regional).

% Inherit whatever technological equilibrium the current investment race produces. Cannot participate in present decisions about R&D funding, arms control renewal, or verification regime design, yet will live inside — or be extinguished by — the boundary those decisions ultimately fix or unfix.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, future_generations_under_reversed_boundary, payer,
    powerless, civilizational, trapped, universal).

% Model whether counterforce, missile defense, and first-strike technologies are actually eroding mutual vulnerability or merely shifting perceived balance without changing underlying survivability. Their assessments feed into force-posture debates but do not themselves control funding or deployment decisions.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contingent_reachability_reading, strategic_studies_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None, structurally — this reading holds that what looks like a stable deterrence equilibrium is actually a technologically contingent truce: the appearance of a fixed boundary against total-war reachability lets states coordinate around arms control and crisis-management norms as if the underlying physics were settled, when in fact it is an artifact of the current state of counterforce, defense, and detection technology.
% TRANSFER_FUNCTION: Moves security (in the form of relative strategic advantage) from populations and non-nuclear states to the handful of technologically advanced states racing to shift the reachability boundary in their favor, while the catastrophic tail risk of a successful shift is transferred onto civilian populations globally who bear it involuntarily.
% ABSENT_VOICES: Global civilian populations, non-nuclear states, and future generations have no seat in the classified force-posture and R&D-investment decisions that determine whether the boundary holds or moves; the technologists and military planners driving the underlying technology race are rarely subject to public deliberation about whether the race itself is worth the systemic risk it reintroduces.
% DISAPPEARANCE_RATIONALE: If the current technological equilibrium (survivable second strike, unreliable counterforce, imperfect missile defense) vanished overnight in favor of reliable disarming first-strike capability, deterrence theorists disagree sharply on the consequence: some hold that crisis instability would spike catastrophically (world_rearranges toward heightened risk of preemption), others hold that new equilibria (e.g., mutual vulnerability at a different technological layer) would reconstitute quickly enough that the practical strategic environment would look similar (world_unchanged in effect, though not in mechanism). The contest is unresolved because no historical case of a full boundary reversal has occurred to observe.
% FOUNDING_PROBLEM: The problem this reading answers is: why does the post-1945 absence of great-power total war persist, and is that absence a permanent structural fact or a fragile technological accident? The reading was built to explain why deterrence theorists should treat 'total war is off the table' as a hypothesis under continuous technological pressure rather than a settled achievement.
% FOUNDING_PROBLEM_CORROBORATION: Independent strategic-studies literature (RAND, IISS, and academic nuclear-strategy scholarship outside any single state's defense establishment) documents active counterforce, hypersonic, and cyber-C2 programs explicitly framed by their own developers as efforts to alter strike reliability — corroboration exists from analysts and arms-control monitors who are not the states benefiting from the investment race, though full technical assessment is limited by classification.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contingent_reachability_reading, contested).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contingent_reachability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contingent_reachability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contingent_reachability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contingent_reachability_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).
:- end_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-to-rising (0.22 to a projected 0.52) because under this reading the arrangement is not yet a full extraction machine — it still performs a genuine coordination function (arms control, crisis stability norms) — but that function is being steadily undermined by asymmetric technological investment that the coordinating framework was not built to withstand. Suppression is lower than extractiveness throughout because this reading holds the boundary is eroding through capability development, not through coercive enforcement of the status quo; the classified nature of the relevant R&D programs is the main suppressive element, hence a moderate but rising suppression_requirement rather than a high one. Theater_ratio starts high (post-Cold War arms control diplomacy performed stability that the underlying physics was already starting to erode) and falls as the technological reality becomes harder to paper over with diplomatic ritual.
 *
 * DIRECTIONALITY LOGIC:
 *   States investing in counterforce, missile defense, hypersonic, and cyber-strike capability are coded as beneficiaries with arbitrage exit: they can walk away from arms-control commitments, redirect budgets, or exploit ambiguity in verification regimes, and they gain relative strategic position from every marginal erosion of the boundary regardless of whether it fully reverses. Verification-regime administrators are dual-coded (agenda_setter + beneficiary) because they set the diplomatic terms under which erosion is disclosed or managed but also derive institutional relevance from managing that disclosure gradually. Global populations, non-nuclear states in range, and future generations are coded as payers with trapped exit and civilizational/universal scope — under this reading, they hold no technological agency over the boundary and bear its entire catastrophic tail risk should it move.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold framing prevents mislabeling this arrangement as either a mountain (which would falsely naturalize the boundary as physics-fixed, hiding the ongoing technology race from scrutiny) or a pure snare (which would ignore the genuine coordination value that arms-control and crisis-stability norms still provide while the boundary holds). Scaffold classification requires a sunset — here the 'sunset' is not a declared date but a technological contingency: the arrangement's coordination function terminates whenever the underlying technical equilibrium it depends on breaks, and that could happen on a much shorter horizon than the diplomatic architecture assumes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reachability_kernel_reading_selection,
    'Is the post-1945 absence of great-power total war better modeled as a permanent contraction of the strategic possibility space (contraction_reading), a persistently low-but-unchanged-magnitude probability sustained by deterrence coordination (dropping_reading), or a technologically contingent equilibrium that could reverse with sufficient counterforce/missile-defense/cyber advancement (this reading)? These are not measurement variants of one claim — they carry different beneficiary structures, different ε trajectories, and different classifications (mountain-leaning vs. rope vs. scaffold).',
    'Long-run tracking of counterforce accuracy, missile defense intercept reliability, and nuclear command-and-control cyber-resilience against the threshold at which a disarming first strike becomes technically credible; if that threshold is approached without triggering crisis instability or renewed arms racing, the contingent-reachability reading gains support over the contraction reading.',
    'If contraction_reading is correct, current technology investment is largely wasted motion against an already-closed possibility space and this story''s scaffold classification is a category error. If this reading is correct, arms-control institutions built on contraction_reading''s assumptions are structurally exposed to a risk they are not designed to detect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reachability_kernel_reading_selection, conceptual, 'Which kernel reading of total-war reachability best describes the post-1945 strategic order — routes the committer-frame contest among sibling constraints.').

omega_variable(
    technology_race_beneficiary_intent,
    'Are the states investing in counterforce, hypersonic, and cyber-strike capability doing so BECAUSE they believe the reachability boundary is contingent and reversible (supporting this reading''s beneficiary structure), or are these investments better explained by bureaucratic/industrial momentum with no strategic theory behind them at all?',
    'Declassified doctrine documents, budget justification testimony, and internal strategic planning literature (to the extent available) that state or imply an explicit theory of first-strike or boundary-shifting intent, versus documents showing investment driven by service rivalry, industrial lobbying, or generic modernization without a boundary-shift theory.',
    'If investment is strategically intentional, the beneficiary declaration is well-founded and the scaffold''s fragility is a live policy concern. If investment is largely bureaucratic momentum without coherent boundary-shift theory, the beneficiary structure weakens and the constraint drifts closer to a pure inertial piton with diffuse rather than concentrated capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_race_beneficiary_intent, empirical, 'Whether the named technological beneficiaries act on an explicit boundary-reversal theory or on institutional momentum alone.').

omega_variable(
    verification_regime_disclosure_incentive,
    'Do arms-control verification administrators actually possess private knowledge that the technical boundary is eroding faster than publicly acknowledged, and if so, is their gradual-disclosure posture a legitimate diplomatic stabilization function or a self-interested delay serving institutional survival?',
    'Comparison of classified intelligence assessments (where later declassified) against contemporaneous public arms-control diplomatic statements, to identify gaps between known erosion and public framing.',
    'A wide gap would support recoding verification administrators more heavily toward beneficiary/agenda_setter self-interest; a narrow gap would support their coordination function as the primary explanation for their behavior.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(verification_regime_disclosure_incentive, empirical, 'Whether verification institutions withhold known boundary-erosion information for stabilizing or self-serving reasons.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contingent_reachability_reading, 1991, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1991, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1991, 0.72).
narrative_ontology:measurement_basis(tota_tr_t1991, observed).
narrative_ontology:measurement(tota_tr_t2001, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2001, 0.68).
narrative_ontology:measurement_basis(tota_tr_t2001, observed).
narrative_ontology:measurement(tota_tr_t2011, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2011, 0.65).
narrative_ontology:measurement_basis(tota_tr_t2011, observed).
narrative_ontology:measurement(tota_tr_t2018, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2018, 0.62).
narrative_ontology:measurement_basis(tota_tr_t2018, observed).
narrative_ontology:measurement(tota_tr_t2025, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2025, 0.61).
narrative_ontology:measurement_basis(tota_tr_t2025, observed).
narrative_ontology:measurement(tota_tr_t2035, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2035, 0.5).
narrative_ontology:measurement_basis(tota_tr_t2035, projected).

% Extraction over time
narrative_ontology:measurement(tota_be_t1991, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1991, 0.22).
narrative_ontology:measurement_basis(tota_be_t1991, observed).
narrative_ontology:measurement(tota_be_t2001, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2001, 0.27).
narrative_ontology:measurement_basis(tota_be_t2001, observed).
narrative_ontology:measurement(tota_be_t2011, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2011, 0.33).
narrative_ontology:measurement_basis(tota_be_t2011, observed).
narrative_ontology:measurement(tota_be_t2018, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2018, 0.38).
narrative_ontology:measurement_basis(tota_be_t2018, observed).
narrative_ontology:measurement(tota_be_t2025, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement_basis(tota_be_t2025, observed).
narrative_ontology:measurement(tota_be_t2035, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2035, 0.52).
narrative_ontology:measurement_basis(tota_be_t2035, projected).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1991, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1991, 0.28).
narrative_ontology:measurement_basis(tota_su_t1991, observed).
narrative_ontology:measurement(tota_su_t2001, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2001, 0.3).
narrative_ontology:measurement_basis(tota_su_t2001, observed).
narrative_ontology:measurement(tota_su_t2011, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2011, 0.32).
narrative_ontology:measurement_basis(tota_su_t2011, observed).
narrative_ontology:measurement(tota_su_t2018, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2018, 0.35).
narrative_ontology:measurement_basis(tota_su_t2018, observed).
narrative_ontology:measurement(tota_su_t2025, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2025, 0.38).
narrative_ontology:measurement_basis(tota_su_t2025, observed).
narrative_ontology:measurement(tota_su_t2035, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2035, 0.45).
narrative_ontology:measurement_basis(tota_su_t2035, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contingent_reachability_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__contingent_reachability_reading, 0.12).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__dropping_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language 'nuclear deterrence made total war unreachable' claim per the ε-invariance principle. contraction_reading treats the boundary as a near-permanent structural fact (mountain-leaning, low and stable ε). dropping_reading treats total war as remaining reachable with deterrence as ongoing rope-like coordination (moderate, stable ε tied to coordination-cost accounting). This reading (contingent_reachability_reading) treats the boundary as scaffold-like: contingent on a technological equilibrium under active pressure from destabilizing investment, with ε authored as rising over the interval rather than flat. All three share the same underlying kernel (total_war_reachability_boundary) but are authored as separate constraints with separate ε values and separate stakeholder structures, linked here via affects_constraints rather than merged into one measurement-dependent story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
