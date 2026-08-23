% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__performative_tool_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__performative_tool_reading, []).

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
 *   constraint_id: doomsday_clock_metric__performative_tool_reading
 *   human_readable: Strategically Set Doomsday Clock (Performative Tool Reading)
 *   domain: science communication/normative epistemology/risk governance
 *
 * SUMMARY:
 *   The Bulletin of the Atomic Scientists' Science and Security Board resets
 *   the Doomsday Clock annually. Under the performative_tool_reading — the
 *   reading instantiated here — the setting is chosen strategically to
 *   maximize policy impact and mobilize collective action, not to track
 *   measured existential risk. The standing arrangement under contest (the ε
 *   referent, fixed per the kernel-reading rule) is that strategic-setting
 *   practice itself: advocacy coalitions and mobilization movements consume
 *   the urgency signal the setting manufactures, the Bulletin converts the
 *   Clock's news value into institutional resources, and the cost lands as a
 *   drawdown on epistemic credibility — a commons held by the scientific
 *   community at large and inherited by future audiences who had no seat in
 *   the setting. This file is one member of a three-story constraint family
 *   decomposing the colloquial label 'the Doomsday Clock'; the
 *   objective_index_reading and hybrid_legitimacy_reading instantiate
 *   different constraints with different ε over the same symbol. Claim and
 *   metrics are authored independently: I claim tangled_rope because the
 *   arrangement possesses both a real coordination function (attention
 *   mobilization for genuinely hard collective-action problems) and an
 *   asymmetric, actively enforced extraction (credibility spent by those who
 *   do not own it); the metrics describe the arrangement's actual operation
 *   without being tuned to that claim.
 *
 * KEY AGENTS:
 *   - science_security_board: agenda setter (institutional/identity_locked) — chooses the minutes-to-midnight with explicit regard for communicative force and defends the strategic framing
 *   - bulletin_of_atomic_scientists_institution: primary institutional beneficiary (institutional/constrained) — converts the Clock's salience into funding, subscriptions, and media partnerships
 *   - disarmament_advocacy_coalitions: beneficiary (organized/mobile) — consumes each setting as a ready-made campaign hook
 *   - climate_mobilization_movements: beneficiary (organized/mobile) — borrows the Clock's urgency grammar and moral authority
 *   - future_policy_audiences: primary target (powerless/trapped, civilizational horizon) — inherits a devalued signal they never voted on and cannot exit
 *   - independent_risk_scientists: target (moderate/constrained) — their testimony is discounted by association when the setting diverges from their indicators
 *   - competing_risk_indices: target (organized/constrained) — crowded out of the attention pool the Clock dominates
 *   - quantitative_risk_modelers: excluded voice (moderate/constrained) — would demand published criteria and uncertainty bounds; outside the invitation set
 *   - science_journalists: analytical observer (organized/analytical) — amplify each setting and supply the main post-hoc scrutiny of the strategic framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, 0.58).
domain_priors:suppression_score(doomsday_clock_metric__performative_tool_reading, 0.52).
domain_priors:theater_ratio(doomsday_clock_metric__performative_tool_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(doomsday_clock_metric__performative_tool_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__performative_tool_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__performative_tool_reading, "Strategically Set Doomsday Clock (Performative Tool Reading)").
narrative_ontology:topic_domain(doomsday_clock_metric__performative_tool_reading, "science communication/normative epistemology/risk governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__performative_tool_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__performative_tool_reading, 'b4fbda70-f384-4fe7-af24-a4318d60fb71').
narrative_ontology:cs_kernel_codification('b4fbda70-f384-4fe7-af24-a4318d60fb71', formalized).
narrative_ontology:cs_authority_grounding('b4fbda70-f384-4fe7-af24-a4318d60fb71', expertise).
narrative_ontology:cs_interpretation_layer_present('b4fbda70-f384-4fe7-af24-a4318d60fb71').
narrative_ontology:cs_reading_relation('b4fbda70-f384-4fe7-af24-a4318d60fb71', doomsday_clock_metric__objective_index_reading, influences).
narrative_ontology:cs_reading_relation('b4fbda70-f384-4fe7-af24-a4318d60fb71', doomsday_clock_metric__hybrid_legitimacy_reading, coexists_with).
narrative_ontology:cs_axiom('b4fbda70-f384-4fe7-af24-a4318d60fb71', foundational, impact_justifies_setting_discretion).
narrative_ontology:cs_axiom_status(impact_justifies_setting_discretion, holdable).
narrative_ontology:cs_axiom_grounding('b4fbda70-f384-4fe7-af24-a4318d60fb71', impact_justifies_setting_discretion, instrumental).
narrative_ontology:cs_axiom('b4fbda70-f384-4fe7-af24-a4318d60fb71', foundational, credibility_spend_is_mobilization_price).
narrative_ontology:cs_axiom_status(credibility_spend_is_mobilization_price, holdable).
narrative_ontology:cs_axiom_grounding('b4fbda70-f384-4fe7-af24-a4318d60fb71', credibility_spend_is_mobilization_price, instrumental).
narrative_ontology:cs_reference_frame('b4fbda70-f384-4fe7-af24-a4318d60fb71', persuasion_instrument_stewardship).
narrative_ontology:cs_drift_state('b4fbda70-f384-4fe7-af24-a4318d60fb71', contemporary_attention_economy, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b4fbda70-f384-4fe7-af24-a4318d60fb71', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, disarmament_advocacy_coalitions).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, climate_mobilization_movements).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, bulletin_of_atomic_scientists_institution).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, future_policy_audiences).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, independent_risk_scientists).
narrative_ontology:constraint_victim(doomsday_clock_metric__performative_tool_reading, competing_risk_indices).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__performative_tool_reading, science_security_board).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__performative_tool_reading, symbolic_urgency_superiority_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes twice yearly, weighs developments, and selects the minutes-to-midnight with explicit regard for communicative force, then announces the result with framing built to provoke coverage and policy debate. Members lend their personal scientific eminence to the symbol and receive amplified platforms in return. Stepping off the annual-setting treadmill would dissolve the body's reason to exist; the stewardship role and the members' public identities have fused.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, science_security_board, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__performative_tool_reading, science_security_board, beneficiary).

% Publishes the journal, hosts the announcement, and converts the Clock's news value into subscriptions, donations, licensing, and media partnerships. The Clock is the institution's flagship asset; diversifying away would forfeit the instant recognizability nothing else in its portfolio commands, so the institution stays bound to the practice that funds it.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, bulletin_of_atomic_scientists_institution, beneficiary,
    institutional, biographical, constrained, global).

% Use each new setting as a ready-made hook for campaigns, legislative testimony, and fundraising; the annual reset hands them a predictable news moment they would otherwise have to manufacture. Other urgency devices exist and they could switch, but none carries equivalent off-the-shelf recognizability, so they stay and draw.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, disarmament_advocacy_coalitions, beneficiary,
    organized, generational, mobile, global).

% Cite the Clock's treatment of climate disruption to bind their cause to the nuclear-warning tradition's moral authority, and borrow its urgency grammar for their own timelines. They contribute nothing to the setting and bear none of the credibility cost, which falls on the scientific community at large.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, climate_mobilization_movements, beneficiary,
    organized, generational, mobile, global).

% Will inherit an information environment in which the most famous risk signal moves for reasons that cannot be checked against anything. Every strategic overshoot today discounts the weight tomorrow's genuine warnings can carry. They cannot vote in the setting, cannot leave the environment, and cannot yet organize — the bill arrives before they have a seat.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, future_policy_audiences, payer,
    powerless, civilizational, trapped, global).

% Produce the hazard assessments the Clock rhetorically draws on. When the announced setting visibly diverges from their published indicators, their testimony is discounted as alarmism by association. Distancing themselves carries professional cost — press access and policy relevance flow through the Clock's orbit — so most stay inside the ecosystem their association with it devalues.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, independent_risk_scientists, payer,
    moderate, biographical, constrained, global).

% Offer transparent methodologies, published criteria, and reproducible weights, but compete for the same finite attention pool. The Clock's seven-decade brand crowds them out of coverage regardless of methodological merit. Nothing formally bars them; the attention economics the Clock dominates do the excluding.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, competing_risk_indices, payer,
    organized, biographical, constrained, global).

% Would demand published setting criteria, uncertainty bounds, and indicator weights, and would treat unverifiable movement as disqualifying. They sit outside the Board's invitation set; their objections surface as op-ed rejoinders after each announcement rather than as input to the deliberation they criticize.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, quantitative_risk_modelers, excluded,
    moderate, biographical, constrained, global).

% Amplify each setting worldwide and increasingly build coverage around the gap between the number and any checkable measure. Their scrutiny is the principal external check on the strategic framing, though it always arrives after the setting is fixed and rarely alters the next one.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__performative_tool_reading, science_journalists, observer,
    organized, immediate, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__performative_tool_reading, bulletin_of_atomic_scientists_institution).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__performative_tool_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the attention-allocation problem for existential risk: converts diffuse, abstract, probabilistic hazards into a single shared urgency signal that publics, policymakers, and coalitions can orient around, and supplies a recurring focal event (the annual reset) that sustains coalition coordination across decades.
% TRANSFER_FUNCTION: Moves public attention and mobilizational energy toward existential-risk causes, and media value and funding toward the Bulletin, paying for both with a drawdown of epistemic credibility held by the scientific community collectively and inherited by future audiences.
% ABSENT_VOICES: Quantitative risk modelers who would demand published criteria are outside the Board's invitation set; future audiences cannot object because they do not yet exist as a constituency; rival indices have no seat in the deliberation that crowds them out. Unanimity around each setting arises in part because the seats that would contest it were never in the room.
% DISAPPEARANCE_RATIONALE: If the strategic-setting practice vanished overnight, advocacy coalitions would lose their anchor event and rebuild calendars around rival signals, the Bulletin would lose its flagship revenue and relevance engine, coverage share would migrate to transparent indices, and the credibility commons would stop being drawn down — the risk-communication landscape would reorganize around whoever supplied the next usable urgency device.
% FOUNDING_PROBLEM: After Hiroshima, the danger of nuclear catastrophe was scientifically evident but publicly invisible; the Bulletin's founders needed a way to translate expert judgment of proximity to disaster into a form lay audiences could grasp, remember, and act on.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the atomic scientists' movement attest the original invisibility problem and its post-war urgency; contemporary risk-communication researchers independently attest that attention allocation for catastrophic risk remains an unsolved coordination problem. Neither group belongs to the benefiting parties, so the live status is corroborated from outside the beneficiary set.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__performative_tool_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__performative_tool_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__performative_tool_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(doomsday_clock_metric__performative_tool_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__performative_tool_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__performative_tool_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__performative_tool_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__performative_tool_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.58: even by this reading's own tolerant lights, the strategic premium is a real drawdown on a credibility stock the setters do not own — the reading accepts the price, it does not deny the cost. Suppression is 0.52 and is authored as a raw structural property (unscaled by power or scope): the mechanism is editorial gatekeeping — unilateral setting authority, unpublished criteria, no dissenting minority report — not coercion; roughly three-quarters of it is structural (control of the setting channel) and one-quarter internalized (the Board's self-conception as stewards makes transparency feel like betrayal of the mission). Theater_ratio is 0.60: the annual announcement ceremony, the staged minute adjustment, and the press cycle are performative by design, sitting atop a real but subordinate deliberative core. Accessibility_collapse is 0.42 — alternatives (transparent indices, quantified risk registers) persist and are legible once the strategic character is understood; the Clock marginalizes them through attention dominance rather than foreclosing them. Resistance is 0.48: every major setting now draws politicization critiques, dissenting op-eds, and refusals to participate — real friction, insufficient to move the practice. The measurement series run on one shared time grid (t = 0, 12, 25, 37, 50, 62, 75; 1947–2022) so every metric is authored at every examined point; trajectories are monotonic rather than cyclical — the annual announcement cycle oscillates within years, but the secular trend (rising strategic premium, rising staging share, rising enforcement burden) is the dynamic this story traces.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the Board's seat the arrangement is responsible stewardship of a necessary provocation — the identity-locked exit means the members cannot conceive of the practice as extraction without dissolving their own role, so their computed type will sit rope-ward. From the payer seats the same structure operates as credibility mining: independent scientists watch their warnings discounted by association, and rival indices watch superior methodologies starved of coverage. The future-audience seat is the sharpest divergence: powerless, trapped, on a civilizational horizon, it bears the compounding cost yet has no perception of the constraint at all during the interval — its classification is computed entirely from structure, never from testimony. Journalists occupy the analytical seat and see both layers, which is why their coverage increasingly frames the gap between the number and any checkable measure.
 *
 * DIRECTIONALITY LOGIC:
 *   Disarmament coalitions and climate movements are declared beneficiaries with mobile exits — they collect the urgency signal without bearing its cost and can substitute other framings, so their derived directionality sits near the subsidy end. The Bulletin is a beneficiary with constrained exit: it cannot drop the Clock without gutting its own brand, which pins it closer to the structure than a mobile beneficiary would sit. Future audiences, independent risk scientists, and competing risk indices are declared victims; the first is fully trapped (d near the full-target end), the latter two constrained (high but not maximal d). The Board is dual-positioned — agenda_setter with a beneficiary secondary role — and its identity_locked exit amplifies effective extraction on the payer side while insulating it from its own costs. No directionality overrides are used: the beneficiary/victim declarations plus exit options already produce the correct structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making invisible catastrophic risk graspable enough to act on — is still live, so this is not a mandatrophy case and mandatrophy_resolved is not declared. The classification work is boundary-drawing: the coordination function is genuine (existential-risk attention allocation is a real collective-action failure, and the Clock solves it more cheaply than any rival), which blocks a snare reading; the extraction is asymmetric and enforced (credibility is spent by setters, borne by non-setters, defended by editorial control), which blocks a rope reading. The R5 interview corroborates this: the founding problem is attested live by historians of the atomic scientists' movement and by contemporary risk-communication researchers — sources outside the benefiting parties — and the founding-problem-status-by-disappearance combination (live x world_rearranges) raises no zombie flag. The trap this analysis guards against is the reverse mislabel: reading the Board's identity-fused sincerity as proof of benignity. Sincerity of the setter is not absence of extraction; the victims are precisely those absent from the sincere deliberation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    governing_reading_ambiguity,
    'Which reading of the doomsday_clock_metric kernel actually governs the setting procedure in practice — pure strategic choice (this reading), objective indicator tracking, or irreducible hybrid entanglement?',
    'Disclosure of Science and Security Board deliberation records, or systematic comparison of announced settings against contemporaneous published risk indicators to detect whether divergence correlates with communicative opportunity rather than measured change.',
    'If the objective reading governs, this story''s epsilon collapses toward measurement-noise levels and the classification shifts rope-ward; if the hybrid reading governs, epsilon sits intermediate and the victim structure narrows to contested cases only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governing_reading_ambiguity, conceptual, 'Committer-frame omega: this constraint is one reading of the doomsday_clock_metric kernel; sibling readings would change the beneficiary/victim structure and epsilon materially.').

omega_variable(
    credibility_stock_replenishment,
    'Is the epistemic credibility drawn down by each strategic setting net-depleting, or does demonstrated mobilization success replenish public trust in expert risk assessment enough to offset the spend?',
    'Longitudinal survey series on trust in scientific risk assessment, correlated with Clock announcement events and subsequent policy outcomes attributed to Clock-driven mobilization.',
    'Net depletion confirms the victim structure and drives drift toward snare; credible replenishment would downgrade epsilon substantially and strengthen the rope half of the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_stock_replenishment, empirical, 'Whether the credibility commons is being mined faster than it regenerates.').

omega_variable(
    counterfactual_mobilization_yield,
    'Does strategic amplification of the setting generate mobilization beyond what transparent, indicator-faithful reporting would achieve, or does it merely relabel attention that would occur anyway?',
    'Comparative campaign studies contrasting transparent-index risk communication with symbolic-threshold communication on matched policy domains.',
    'Low marginal yield means the extraction purchases little coordination benefit, pushing the classification toward snare; high yield substantiates the genuine coordination half of the tangled_rope structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_mobilization_yield, empirical, 'Whether the strategic premium actually buys additional collective action.').

omega_variable(
    board_identity_lock_depth,
    'How deep is the Science and Security Board''s identity lock — is stewardship of the Clock constitutive of the members'' professional selves, or a revisable institutional habit?',
    'Observe Board behavior under a graceful-transition offer: adoption of published criteria with a ceremonial retirement path for the symbolic setting would indicate shallow lock.',
    'Shallow lock means the authored fixing_cost is overstated and reform is cheaper than the receipt surface suggests; deep lock confirms the prohibitive cost class and stabilizes the enforcement requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(board_identity_lock_depth, empirical, 'Depth of the identity fusion binding the agenda setter to the strategic-setting practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__performative_tool_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doomsday_clock_perf_tr_t0, doomsday_clock_metric__performative_tool_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(doomsday_clock_perf_tr_t0, observed).
narrative_ontology:measurement(doomsday_clock_perf_tr_t12, doomsday_clock_metric__performative_tool_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(doomsday_clock_perf_tr_t12, observed).
narrative_ontology:measurement(doomsday_clock_perf_tr_t25, doomsday_clock_metric__performative_tool_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(doomsday_clock_perf_tr_t25, observed).
narrative_ontology:measurement(doomsday_clock_perf_tr_t37, doomsday_clock_metric__performative_tool_reading, theater_ratio, 37, 0.47).
narrative_ontology:measurement_basis(doomsday_clock_perf_tr_t37, observed).
narrative_ontology:measurement(doomsday_clock_perf_tr_t50, doomsday_clock_metric__performative_tool_reading, theater_ratio, 50, 0.52).
narrative_ontology:measurement_basis(doomsday_clock_perf_tr_t50, observed).
narrative_ontology:measurement(doomsday_clock_perf_tr_t62, doomsday_clock_metric__performative_tool_reading, theater_ratio, 62, 0.57).
narrative_ontology:measurement_basis(doomsday_clock_perf_tr_t62, observed).
narrative_ontology:measurement(doomsday_clock_perf_tr_t75, doomsday_clock_metric__performative_tool_reading, theater_ratio, 75, 0.6).
narrative_ontology:measurement_basis(doomsday_clock_perf_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(doomsday_clock_perf_be_t0, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(doomsday_clock_perf_be_t0, observed).
narrative_ontology:measurement(doomsday_clock_perf_be_t12, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 12, 0.34).
narrative_ontology:measurement_basis(doomsday_clock_perf_be_t12, observed).
narrative_ontology:measurement(doomsday_clock_perf_be_t25, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 25, 0.4).
narrative_ontology:measurement_basis(doomsday_clock_perf_be_t25, observed).
narrative_ontology:measurement(doomsday_clock_perf_be_t37, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 37, 0.46).
narrative_ontology:measurement_basis(doomsday_clock_perf_be_t37, observed).
narrative_ontology:measurement(doomsday_clock_perf_be_t50, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 50, 0.51).
narrative_ontology:measurement_basis(doomsday_clock_perf_be_t50, observed).
narrative_ontology:measurement(doomsday_clock_perf_be_t62, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 62, 0.55).
narrative_ontology:measurement_basis(doomsday_clock_perf_be_t62, observed).
narrative_ontology:measurement(doomsday_clock_perf_be_t75, doomsday_clock_metric__performative_tool_reading, base_extractiveness, 75, 0.58).
narrative_ontology:measurement_basis(doomsday_clock_perf_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(doomsday_clock_perf_su_t0, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(doomsday_clock_perf_su_t0, observed).
narrative_ontology:measurement(doomsday_clock_perf_su_t12, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement_basis(doomsday_clock_perf_su_t12, observed).
narrative_ontology:measurement(doomsday_clock_perf_su_t25, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 25, 0.36).
narrative_ontology:measurement_basis(doomsday_clock_perf_su_t25, observed).
narrative_ontology:measurement(doomsday_clock_perf_su_t37, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 37, 0.41).
narrative_ontology:measurement_basis(doomsday_clock_perf_su_t37, observed).
narrative_ontology:measurement(doomsday_clock_perf_su_t50, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 50, 0.45).
narrative_ontology:measurement_basis(doomsday_clock_perf_su_t50, observed).
narrative_ontology:measurement(doomsday_clock_perf_su_t62, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 62, 0.49).
narrative_ontology:measurement_basis(doomsday_clock_perf_su_t62, observed).
narrative_ontology:measurement(doomsday_clock_perf_su_t75, doomsday_clock_metric__performative_tool_reading, suppression_requirement, 75, 0.52).
narrative_ontology:measurement_basis(doomsday_clock_perf_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__performative_tool_reading, identity_coordination).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__performative_tool_reading, doomsday_clock_metric__hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel doomsday_clock_metric. The colloquial label 'the Doomsday Clock' conflates three structurally distinct claims: that the setting tracks measured existential risk (objective_index_reading — upstream, higher empirical confidence, negligible extraction if true), that the setting embodies irreducible science-values entanglement (hybrid_legitimacy_reading), and that the setting is strategically chosen for impact (this file — downstream, substantially extractive). Each story carries its own ε, beneficiaries, and victims; they are linked here because the performative reading's operation structurally erodes the credibility base the objective reading depends on, and because the siblings are cited as cover or as critique in the same public disputes. Per the ε-invariance principle, no single story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
