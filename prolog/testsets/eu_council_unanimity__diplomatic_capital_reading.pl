% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__diplomatic_capital_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__diplomatic_capital_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eu_council_unanimity__diplomatic_capital_reading
 *   human_readable: EU Council Unanimity as Diplomatic Capital Requirement
 *   domain: institutional/political
 *
 * SUMMARY:
 *   The EU Council's unanimity requirement mandates that all member states
 *   must consent before action on major policy areas — foreign policy,
 *   taxation, constitutional matters. This reading frames unanimity as a
 *   coordination cost that buys legitimacy: the negotiation process forces
 *   all parties to construct mutually acceptable solutions, generating buy-in
 *   that makes the resulting policy durable and harder to defect from later.
 *   This is distinct from readings that frame unanimity as (a) a
 *   sovereign-protection guarantee (sovereignty_guarantor_reading) or (b) a
 *   structural trap enabling minoritarian extraction (veto_trap_reading).
 *   This reading emphasizes the diplomatic-capital and legitimacy payoff of
 *   the process itself.
 *
 * KEY AGENTS:
 *   - consensus_builders (EU institutional actors, diplomats): accumulate diplomatic capital through negotiation; benefit from process legitimacy
 *   - time_constrained_deciders (national governments): pay in decision-making delay; crisis response slowed
 *   - majoritarian_coalition (large member states): blocked from speed, forced into compromise, but benefit from consensus legitimacy
 *   - small_member_states: protected by veto leverage, pay in compromise cost
 *   - obstructionist_holdouts: leverage through refusal; pay in isolation and reputational cost; identity-locked into participation
 *   - policy_beneficiaries (climate advocates, etc.): excluded from negotiation; bear delay cost without voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__diplomatic_capital_reading, 0.38).
domain_priors:suppression_score(eu_council_unanimity__diplomatic_capital_reading, 0.12).
domain_priors:theater_ratio(eu_council_unanimity__diplomatic_capital_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__diplomatic_capital_reading, tangled_rope).
narrative_ontology:human_readable(eu_council_unanimity__diplomatic_capital_reading, "EU Council Unanimity as Diplomatic Capital Requirement").
narrative_ontology:topic_domain(eu_council_unanimity__diplomatic_capital_reading, "institutional/political").

domain_priors:requires_active_enforcement(eu_council_unanimity__diplomatic_capital_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__diplomatic_capital_reading, 'f0331c43-cfb4-4b10-b44e-09a244728639').
narrative_ontology:cs_kernel_codification('f0331c43-cfb4-4b10-b44e-09a244728639', formalized).
narrative_ontology:cs_authority_grounding('f0331c43-cfb4-4b10-b44e-09a244728639', lineage).
narrative_ontology:cs_interpretation_layer_present('f0331c43-cfb4-4b10-b44e-09a244728639').
narrative_ontology:cs_reading_relation('f0331c43-cfb4-4b10-b44e-09a244728639', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0331c43-cfb4-4b10-b44e-09a244728639', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_axiom('f0331c43-cfb4-4b10-b44e-09a244728639', foundational, legitimacy_through_inclusion).
narrative_ontology:cs_axiom_status(legitimacy_through_inclusion, holdable).
narrative_ontology:cs_axiom_grounding('f0331c43-cfb4-4b10-b44e-09a244728639', legitimacy_through_inclusion, instrumental).
narrative_ontology:cs_axiom('f0331c43-cfb4-4b10-b44e-09a244728639', foundational, consensus_produces_durability).
narrative_ontology:cs_axiom_status(consensus_produces_durability, holdable).
narrative_ontology:cs_axiom_grounding('f0331c43-cfb4-4b10-b44e-09a244728639', consensus_produces_durability, empirically_contingent).
narrative_ontology:cs_reference_frame('f0331c43-cfb4-4b10-b44e-09a244728639', integration_by_consensus).
narrative_ontology:cs_drift_state('f0331c43-cfb4-4b10-b44e-09a244728639', post_lisbon_accelerated_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f0331c43-cfb4-4b10-b44e-09a244728639', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, consensus_builders).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, long_term_eu_project).
narrative_ontology:constraint_victim(eu_council_unanimity__diplomatic_capital_reading, time_constrained_deciders).
narrative_ontology:constraint_victim(eu_council_unanimity__diplomatic_capital_reading, majoritarian_coalition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, majoritarian_coalition).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, small_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__diplomatic_capital_reading, small_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__diplomatic_capital_reading, obstructionist_holdouts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% EU institutionalists, supranational bureaucrats, and diplomats skilled in iterative negotiation benefit from unanimity by building legitimacy through inclusion and buy-in. They accumulate diplomatic capital through the negotiation process itself and gain institutional durability from the consensus-generated legitimacy. The constraint rewards their core competency — finding packages that all can live with.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, consensus_builders, beneficiary,
    institutional, generational, arbitrage, continental).

% National governments facing urgent external pressures (migration crises, security threats, economic shocks) must still secure every member's agreement before acting. They bear the cost of delay: crises do not wait for consensus. A government that could act unilaterally or by simple majority under QMV would move faster. Instead, unanimity converts speed into a negotiating chip and makes decision-making a hostage to the slowest or most reluctant actor.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, time_constrained_deciders, payer,
    powerful, biographical, constrained, global).

% A coalition of member states that could form a voting majority under QMV finds its preferred policy blocked by unanimous-consent requirements when other states withhold support. They pay in terms of watered-down compromises and foregone speed. However, they also benefit from the legitimacy of consensus — a unanimously adopted policy is harder to reverse or blame-shift later than a narrowly-won majority decision.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, majoritarian_coalition, payer,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__diplomatic_capital_reading, majoritarian_coalition, beneficiary).

% Benefit from having a veto on decisions that would affect them (protection against majoritarian overriding). They pay when their veto forces compromises they would not have negotiated under QMV. The unanimity rule gives them leverage in proportion to their strategic negotiating position, not their economic size — a form of power equalization.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, small_member_states, beneficiary,
    moderate, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__diplomatic_capital_reading, small_member_states, payer).

% A member state (or small coalition) that refuses consent on an issue becomes the focus of intense diplomatic pressure and coalition-building. They bear the cost of isolation and reputational damage from blocking consensus, even as their refusal exercises leverage. Their identity as EU member obligates participation in negotiation but locks them into the legitimacy-building function — exit (leaving the EU) is presented as unthinkable.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, obstructionist_holdouts, payer,
    moderate, biographical, identity_locked, continental).

% Citizens and constituencies that would benefit from faster EU decision-making (e.g., climate action, migration policy, digital regulation) are not seated at the negotiation table and are excluded from the consensus-building process. They bear the cost of delay but have no say in it.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, policy_beneficiaries, excluded,
    powerless, biographical, trapped, continental).

% Professional diplomats manage the unanimity requirement operationally: they craft compromise language, broker package deals, orchestrate side agreements, and manage the temporal flow of negotiation. They administer the constraint and benefit from its existence — it makes their work central to EU governance.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, diplomatic_corps, agenda_setter,
    institutional, generational, analytical, continental).

% Non-EU actors (rival powers, hostile actors) observe EU slow decision-making under unanimity and calibrate their actions accordingly. Unanimity slows EU response to external shocks. Some see this as opportunity; others view EU institutional design as self-inflicted paralysis.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, external_adversaries, observer,
    powerful, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__diplomatic_capital_reading, consensus_builders).
narrative_ontology:fixing_cost_class(eu_council_unanimity__diplomatic_capital_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that every member state's core interests are considered before collective EU action, requiring iterative negotiation to build packages that all can live with. This forces exploration of alternative framings, side-payments, and sequencing that increases the durability and inclusivity of the resulting policy. Consensus-building is itself the coordination function — it produces legitimate, collectively-owned decisions less prone to subsequent defection or legal challenge.
% TRANSFER_FUNCTION: Transfers negotiating power and agenda-setting capacity to the diplomats and institutional actors skilled at consensus-building. Moves time-cost from the coordination process to deciders in urgent situations. Shifts legitimacy from majoritarian victory to inclusive agreement — a form of symbolic capital accumulation for consensus-builders and small states whose agreement is necessary.
% ABSENT_VOICES: Citizens and constituencies who would benefit from faster policy action (climate advocates, migrant protection groups, labor movements) are structurally excluded from the negotiation process. They perceive unanimity as the cause of EU paralysis but have no seat in the consensus-building table. Policy beneficiaries affected by delay have no formal voice in determining whether to require unanimity.
% DISAPPEARANCE_RATIONALE: If unanimity vanished and QMV became the standard, EU decision-making would accelerate substantially; the character of EU decisions would shift from consensus-legitimated to majority-legitimated; small-state leverage would evaporate; and the institutional role of diplomacy in iterative consensus-building would shrink. Member states would reorganize their strategies around voting blocs and majoritarian coalition-building rather than exhaustive negotiation.
% FOUNDING_PROBLEM: Early European integration required legitimacy that transcended national legislatures — no single member could be overridden by others on existential sovereignty questions. Unanimity was adopted to guarantee that integration proceeded only where every government could defend the decision to its own parliament and citizens.
% FOUNDING_PROBLEM_CORROBORATION: The EU institutionalist body (Commission, diplomatic corps) attests the founding problem remains live: legitimacy through inclusion is still necessary for controversial policies. Member state governments in foreign policy and security affairs attest the founding protection is still essential. However, policy-advocacy organizations and external observers attest the founding problem has largely been solved — the EU is now an established polity with strong constitutional legitimacy, and the continued requirement of unanimity on substantive matters reflects institutional inertia rather than necessary protection. Independent scholarly consensus: the founding problem (securing legitimacy against majoritarian override) was real; contemporary unanimity requirements on non-existential matters are debated as either proportional protection or institutional pathology.
narrative_ontology:disappearance_verdict(eu_council_unanimity__diplomatic_capital_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__diplomatic_capital_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__diplomatic_capital_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(eu_council_unanimity__diplomatic_capital_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__diplomatic_capital_reading_tests).
:- end_tests(eu_council_unanimity__diplomatic_capital_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because while unanimous consent imposes real negotiation costs and can delay urgent decisions, the mechanism produces durable policies that reduce downstream defection and reversal. The coordination payoff (legitimacy through inclusion) is substantial enough to offset the extraction of negotiating time and power from urgency-driven actors. Theater ratio is low (0.22): the consensus-building negotiation is real and central to EU governance, not performative. The ratio rises modestly over time as crisis situations increase pressure to justify slow decisions through elaborate consensus-theater, but the core function remains genuine. Suppression is very low (0.12): unanimity does not rest on coercion — it rests on the agreement that all member states must consent. The minimal suppression reflects the structural nature of the rule, not enforcement through force. The measurement series track one shared time grid: extractiveness rises and plateaus as the EU faces more urgent external challenges (migration, security, climate); theater rises as diplomats elaborate justifications for delay; suppression stays minimal because the constraint relies on consensus-building, not coercion.
 *
 * PERSPECTIVAL GAP:
 *   From the diplomatic corps seat, unanimity is legitimate consensus-building that strengthens EU decisions. From the time-constrained national government seat (especially during external crises), unanimity is an extraction mechanism that converts urgent decision-making power into negotiating leverage for slower actors. The engine will compute different classifications from each seat because the structural asymmetry is real — the constraint does benefit consensus-builders and smaller states while imposing costs on urgency-driven majorities. This reading does NOT adjudicate the dispute; it names the mechanism by which the asymmetry operates.
 *
 * DIRECTIONALITY LOGIC:
 *   Consensus-builders (institutional beneficiaries) have high directionality toward the beneficiary end: the constraint makes them indispensable and accumulates their capital. Time-constrained deciders (national governments in crises) are targets: their urgency is extracted as negotiating leverage. Small member states occupy the middle ground — they benefit from veto leverage but pay in forced compromise. The majoritarian coalition faces asymmetry: they have power globally but are constrained within the EU by the unanimity rule. This reading emphasizes that the constraint's beneficiaries and victims depend on the temporal context (urgency level) — a given government is a payer in crisis but a beneficiary in deliberative scenarios.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (securing legitimacy for integration against majoritarian override) was live at the EU's origin. The diplomatic_capital_reading asserts that unanimity still solves that problem — it keeps decisions legitimate through inclusion. However, the founding-problem_status is contested: if the founding problem has substantially been solved (the EU now has constitutional legitimacy independent of unanimity), then continuing unanimity on all matters reflects institutional inertia, and the constraint has shifted from rope (solving a real coordination problem) toward tangled_rope or piton (persisting by administrative habit and beneficiary defense). The reading survives this contestation because the legitimacy payoff is ongoing — every major decision that requires unanimous consent builds buy-in from the slowest actor, which pays dividends in durability. But if a future test shows that unanimity-generated buy-in does not produce durability (unanimous policies are just as prone to reversal as QMV policies), then the founding problem would have genuinely died and mandatrophy would apply.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_payoff_empirical,
    'Do unanimously adopted EU decisions actually exhibit greater durability, lower defection rates, and higher citizen support than QMV-adopted decisions, or is the claimed legitimacy payoff a post-hoc narrative?',
    'Comparative analysis of decision reversal rates, implementation compliance, and public legitimacy measures across unanimity vs. QMV decisions in comparable policy domains; longitudinal tracking of defection and legal challenges.',
    'If legitimacy payoff is real (low reversal, high durability), this reading is well-grounded and extractiveness is correctly measured as moderate. If unanimity decisions are reversed or defected at similar rates to QMV decisions, the legitimacy claim becomes narrative cover and extractiveness should be re-measured as higher (pure coordination cost with no payoff).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_payoff_empirical, empirical, 'Whether consensus-building produces durable policy legitimacy or is performative justification for delay.').

omega_variable(
    founding_problem_live_or_dead,
    'Does the founding problem (securing legitimacy for integration against majoritarian override) remain live and unsolved, or has the EU achieved sufficient constitutional legitimacy that unanimity requirements are now institutional inertia rather than necessary protection?',
    'Comparison of legitimacy sources across EU decisions: do unanimity-based decisions draw legitimacy from the unanimous consent itself, or from independent constitutional authority? Do member state parliaments require unanimous decision-making to ratify, or has this requirement been relaxed?',
    'If the founding problem is dead but unanimity persists, the constraint has crossed into mandatrophy — it persists despite its founding justification having evaporated. The reading would shift toward piton (inertial) or tangled_rope (beneficiary-defended) rather than rope (solving a live coordination problem).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_live_or_dead, conceptual, 'Whether unanimity''s founding justification (sovereignty protection/legitimacy insurance) remains valid or has been superseded by EU constitutional development.').

omega_variable(
    consensus_builders_as_structural_beneficiary,
    'Are diplomats and EU institutionalists identified as consensus_builders a real structural beneficiary group, or is naming them as beneficiaries a description of their role rather than evidence of extraction?',
    'Track career advancement, resource allocation, and institutional power accumulation among diplomats under unanimity vs. QMV regimes. Compare compensation, status, and decision-making authority of diplomatic corps in EU systems with unanimity vs. majority-rule systems.',
    'If consensus-builders'' institutional power genuinely expands under unanimity (relative to QMV), they are a real beneficiary and the constraint has extraction (transfer of power to diplomatic middlemen). If diplomatic corps power is stable across rule types, naming them as beneficiaries conflates functional role with structural benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consensus_builders_as_structural_beneficiary, empirical, 'Whether the identified beneficiary (consensus-builders/diplomatic corps) actually captures gain from the constraint or merely executes its function.').

omega_variable(
    negotiation_cost_allocation,
    'Who actually bears the negotiation cost of unanimity, and is it distributed equally across member states or concentrated on specific seats?',
    'Time-accounting of negotiation burden by member state (diplomatic corps hours, political attention, opportunity cost); analysis of who most frequently makes concessions vs. extracting them in unanimity-driven negotiations.',
    'If negotiation cost is concentrated on smaller states or slower actors (those with constrained alternatives), then the constraint has a hidden victim structure and higher extractiveness than measured. If costs are distributed, the measured extractiveness holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negotiation_cost_allocation, empirical, 'The actual distribution of negotiation-burden cost, which drives real extractiveness.').

omega_variable(
    reading_foreclosure_sovereignty_vs_process,
    'Does the diplomatic_capital_reading (legitimacy through process) foreclose the sovereignty_guarantor_reading (legitimacy through protection), or can both coexist as different justifications for the same rule?',
    'Examine institutional practice: do member states invoke unanimity as process-legitimacy (consensus-building) or as protection-from-coercion (sovereignty guarantee)? Can a single decision be justified on both grounds simultaneously?',
    'If the readings can coexist (both justifications operate simultaneously), they are distinct framings of the same rule and neither forecloses the other. If only one justification is operative in practice, the other is narrative cover and the false-summit mechanism may apply.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_sovereignty_vs_process, conceptual, 'The relationship between this reading''s legitimacy-through-process framing and the sovereignty_guarantor_reading''s legitimacy-through-protection framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__diplomatic_capital_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(eu_c_tr_t0, observed).
narrative_ontology:measurement(eu_c_tr_t5, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement_basis(eu_c_tr_t5, observed).
narrative_ontology:measurement(eu_c_tr_t10, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(eu_c_tr_t10, observed).
narrative_ontology:measurement(eu_c_tr_t15, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(eu_c_tr_t15, observed).
narrative_ontology:measurement(eu_c_tr_t20, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(eu_c_tr_t20, observed).
narrative_ontology:measurement(eu_c_tr_t25, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(eu_c_tr_t25, observed).
narrative_ontology:measurement(eu_c_tr_t30, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(eu_c_tr_t30, observed).
narrative_ontology:measurement(eu_c_tr_t35, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 35, 0.22).
narrative_ontology:measurement_basis(eu_c_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(eu_c_be_t0, observed).
narrative_ontology:measurement(eu_c_be_t5, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement_basis(eu_c_be_t5, observed).
narrative_ontology:measurement(eu_c_be_t10, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement_basis(eu_c_be_t10, observed).
narrative_ontology:measurement(eu_c_be_t15, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement_basis(eu_c_be_t15, observed).
narrative_ontology:measurement(eu_c_be_t20, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(eu_c_be_t20, observed).
narrative_ontology:measurement(eu_c_be_t25, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(eu_c_be_t25, observed).
narrative_ontology:measurement(eu_c_be_t30, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(eu_c_be_t30, observed).
narrative_ontology:measurement(eu_c_be_t35, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 35, 0.38).
narrative_ontology:measurement_basis(eu_c_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t0, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(eu_c_su_t0, observed).
narrative_ontology:measurement(eu_c_su_t5, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 5, 0.07).
narrative_ontology:measurement_basis(eu_c_su_t5, observed).
narrative_ontology:measurement(eu_c_su_t10, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 10, 0.09).
narrative_ontology:measurement_basis(eu_c_su_t10, observed).
narrative_ontology:measurement(eu_c_su_t15, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 15, 0.1).
narrative_ontology:measurement_basis(eu_c_su_t15, observed).
narrative_ontology:measurement(eu_c_su_t20, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 20, 0.11).
narrative_ontology:measurement_basis(eu_c_su_t20, observed).
narrative_ontology:measurement(eu_c_su_t25, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 25, 0.12).
narrative_ontology:measurement_basis(eu_c_su_t25, observed).
narrative_ontology:measurement(eu_c_su_t30, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 30, 0.12).
narrative_ontology:measurement_basis(eu_c_su_t30, observed).
narrative_ontology:measurement(eu_c_su_t35, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 35, 0.12).
narrative_ontology:measurement_basis(eu_c_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__diplomatic_capital_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__diplomatic_capital_reading, 0.12).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__veto_trap_reading).

% DUAL FORMULATION NOTE:
% The eu_council_unanimity kernel decomposes into three structurally distinct constraint stories because the rule (unanimous consent) is justified by different readings that carry different ε values. This reading (diplomatic_capital_reading) emphasizes legitimacy through iterative process and produces low-to-moderate ε because the coordination payoff (durable consensus-based policy) offsets the process cost. The sovereignty_guarantor_reading emphasizes consent as a protection right and produces even lower ε (pure coordination). The veto_trap_reading emphasizes minoritarian leverage and produces substantially higher ε (extraction). All three readings apply the same formal rule; the ε variation reflects their different structural understandings of how unanimity operates. Each story must be authored independently with its own ε, beneficiary/victim structure, and temporal metrics. The three form a family linked by network.affects_constraints: the diplomatic_capital reading influences (and is influenced by) both siblings because the legitimacy justification this reading offers is contested by the other readings' alternative framings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_council_unanimity__diplomatic_capital_reading, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
