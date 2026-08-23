% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__categorical_prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__categorical_prohibition_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: ihl_distinction_proportionality__categorical_prohibition_reading
 *   human_readable: Categorical Prohibition of Lethal Autonomous Weapons (Martens Clause Reading)
 *   domain: international_humanitarian_law / military ethics / technology governance
 *
 * SUMMARY:
 *   A categorical prohibition reading of the IHL distinction/proportionality
 *   kernel: the claim that principles of humanity and the dictates of public
 *   conscience — the Martens Clause reservoir — render lethal autonomous
 *   weapons systems unlawful in all cases, independent of how well they
 *   perform, because a machine's selection of a human target violates dignity
 *   per se. This story instantiates one reading only; the human-agency
 *   reading (irreducible human judgment at the moment of force) and the
 *   outcomes-based reading (technology-neutral performance standards) are
 *   separate constraints in the same family, linked by network edges.
 *   Epsilon's referent is the standing arrangement this reading institutes
 *   and defends — the categorical ban regime over autonomous-weapons
 *   development, possession, and use — not the permissive status quo it
 *   contests and not any softer alternative it might tolerate. The
 *   claimed_type records the reading's own self-presentation: it argues from
 *   per se principles, that is, as natural law rather than constructed
 *   policy, and therefore claims mountain. The metrics are authored
 *   independently and describe the ban's actual structural operation: total
 *   foreclosure of a technology class with no performance exit, concentrated
 *   on advanced-capability actors, held up by enforcement machinery against
 *   great-power resistance. Where the engine computes a constructed,
 *   extractive arrangement under a mountain claim, that divergence is the
 *   false-summit measurement this story exists to take. KEY AGENTS (by
 *   structural relationship): - advanced_autonomy_military_powers: Primary
 *   target (powerful/constrained) — bears the categorical foreclosure of an
 *   entire capability class - autonomous_systems_defense_industry: Secondary
 *   target (organized/constrained) — bears market foreclosure on a product
 *   class - capability_lagging_states: Primary beneficiary
 *   (organized/constrained) — collects security-by-norm at zero capability
 *   cost - anti_militarist_civil_society: Mobilization beneficiary
 *   (organized/mobile) — collects normative victory and agenda access -
 *   humanitarian_diplomacy_establishment: Authority beneficiary
 *   (institutional/identity_locked) — collects interpretive authority over
 *   weapons law - nonparty_development_states: Arbitrage beneficiary
 *   (powerful/arbitrage) — profits from others' restraint -
 *   civilian_populations_at_risk: Intended protectee (powerless/trapped) —
 *   the claimed constituency, holding no seat - frontline_service_members:
 *   Dual-positioned (powerless/trapped) — keeps human answerability, forgoes
 *   autonomous force protection - ccw_high_contracting_parties: Agenda setter
 *   (institutional/constrained) — administers the process under consensus
 *   rules - international_humanitarian_law_jurists: Analytical observer
 *   (analytical/analytical) — sees the full structure and splits
 *
 * KEY AGENTS:
 *   - advanced_autonomy_military_powers: Primary target (powerful/constrained) — bears the categorical foreclosure of an entire capability class
 *   - autonomous_systems_defense_industry: Secondary target (organized/constrained) — bears market foreclosure on a product class
 *   - capability_lagging_states: Primary beneficiary (organized/constrained) — collects security-by-norm at zero capability cost
 *   - anti_militarist_civil_society: Mobilization beneficiary (organized/mobile) — collects normative victory and agenda access
 *   - humanitarian_diplomacy_establishment: Authority beneficiary (institutional/identity_locked) — collects interpretive authority over weapons law
 *   - nonparty_development_states: Arbitrage beneficiary (powerful/arbitrage) — profits from others' restraint
 *   - civilian_populations_at_risk: Intended protectee (powerless/trapped) — the claimed constituency, holding no seat
 *   - frontline_service_members: Dual-positioned (powerless/trapped) — keeps human answerability, forgoes autonomous force protection
 *   - ccw_high_contracting_parties: Agenda setter (institutional/constrained) — administers the process under consensus rules
 *   - international_humanitarian_law_jurists: Analytical observer (analytical/analytical) — sees the full structure and splits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, 0.84).
domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, 0.7).
domain_priors:theater_ratio(ihl_distinction_proportionality__categorical_prohibition_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__categorical_prohibition_reading, mountain).
narrative_ontology:human_readable(ihl_distinction_proportionality__categorical_prohibition_reading, "Categorical Prohibition of Lethal Autonomous Weapons (Martens Clause Reading)").
narrative_ontology:topic_domain(ihl_distinction_proportionality__categorical_prohibition_reading, "international_humanitarian_law / military ethics / technology governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__categorical_prohibition_reading).
domain_priors:emerges_naturally(ihl_distinction_proportionality__categorical_prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__categorical_prohibition_reading, '75306956-c9c3-420a-bd5b-4c2f2ba80dc3').
narrative_ontology:cs_kernel_codification('75306956-c9c3-420a-bd5b-4c2f2ba80dc3', fixed_text).
narrative_ontology:cs_authority_grounding('75306956-c9c3-420a-bd5b-4c2f2ba80dc3', lineage).
narrative_ontology:cs_interpretation_layer_present('75306956-c9c3-420a-bd5b-4c2f2ba80dc3').
narrative_ontology:cs_reading_relation('75306956-c9c3-420a-bd5b-4c2f2ba80dc3', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_reading_relation('75306956-c9c3-420a-bd5b-4c2f2ba80dc3', ihl_distinction_proportionality__human_agency_reading, influences).
narrative_ontology:cs_axiom('75306956-c9c3-420a-bd5b-4c2f2ba80dc3', foundational, machine_killing_dignity_violation_per_se).
narrative_ontology:cs_axiom_status(machine_killing_dignity_violation_per_se, holdable).
narrative_ontology:cs_axiom_grounding('75306956-c9c3-420a-bd5b-4c2f2ba80dc3', machine_killing_dignity_violation_per_se, deontological).
narrative_ontology:cs_axiom('75306956-c9c3-420a-bd5b-4c2f2ba80dc3', foundational, lawfulness_invariant_to_system_performance).
narrative_ontology:cs_axiom_status(lawfulness_invariant_to_system_performance, holdable).
narrative_ontology:cs_axiom_grounding('75306956-c9c3-420a-bd5b-4c2f2ba80dc3', lawfulness_invariant_to_system_performance, deontological).
narrative_ontology:cs_reference_frame('75306956-c9c3-420a-bd5b-4c2f2ba80dc3', principles_of_humanity_constitutive_supremacy).
narrative_ontology:cs_drift_state('75306956-c9c3-420a-bd5b-4c2f2ba80dc3', contemporary_ccw_negotiation_phase, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('75306956-c9c3-420a-bd5b-4c2f2ba80dc3', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, capability_lagging_states).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, humanitarian_diplomacy_establishment).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, advanced_autonomy_military_powers).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, autonomous_systems_defense_industry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, civilian_populations_at_risk).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, frontline_service_members).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, nonparty_development_states).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, frontline_service_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the transnational campaign for a preemptive treaty ban on fully autonomous weapons — research reports, parliamentary lobbying, mass petitions, and sustained street-level mobilization. The categorical framing supplies its brightest line: a demand that cannot be negotiated down to safeguards. What flows to it is mobilization capital and agenda access; what flows away is flexibility — if states settle for human-control standards instead of a ban, the campaign's organizing claim collapses. Exit looks like pivoting to adjacent causes at the cost of a decade of issue-specific momentum.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society, beneficiary,
    organized, generational, mobile, global).

% Small and mid-sized states without advanced military-AI programs lead the General Assembly resolutions calling for a binding instrument. They acquire, at zero capability cost, a rule that freezes the battlefield at their current disadvantage and shuts down rivals' acquisition of a decisive edge. Their leverage exists only inside the multilateral process; outside it they are ordinary small powers. Exit would mean conceding the field to whichever great powers keep building.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, capability_lagging_states, beneficiary,
    organized, generational, constrained, global).

% The ICRC and allied humanitarian bodies champion the Martens-Clause route: principles of humanity and the dictates of public conscience as a source of obligation beyond treaty text. Each categorical success expands their interpretive authority over weapons law generally. Their institutional identity is fused with stewardship of those principles; retreating to a merely technical, performance-based position would repudiate a century of institutional self-understanding. Exit is unthinkable from inside the role.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, humanitarian_diplomacy_establishment, beneficiary,
    institutional, generational, identity_locked, global).

% Residents of contemporary and future conflict zones in whose name the prohibition is argued. They would receive the norm's protection wherever it binds — no machine selects them as targets without a human answerable for the decision. They hold no seat in the CCW room; their interests arrive filtered through advocacy organizations and delegations. They cannot exit the wars fought over them.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, civilian_populations_at_risk, beneficiary,
    powerless, biographical, trapped, global).

% Soldiers and airmen who would operate alongside or against autonomous systems. On one side they keep what the categorical reading preserves: a human being answerable for every lethal decision in their chain of command. On the other they forgo whatever force-protection and attrition-resistance fully autonomous systems might have provided — sentry duty, convoy defense, operations in contaminated or denied environments where a human crew is the liability. They can neither exit their services nor choose their equipment.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, frontline_service_members, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__categorical_prohibition_reading, frontline_service_members, payer).

% Great powers with mature autonomous-weapons programs — the United States, China, Russia, Israel, and peers — who would surrender an entire capability class under the categorical rule. The rule grants no relief for excellence: a system that distinguishes combatants better than a tired human sergeant is unlawful all the same. Their realistic choices are resisting a binding instrument, signing and quietly hedging, or staying outside and absorbing stigma. Every path taxes either capability or reputation.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, advanced_autonomy_military_powers, payer,
    powerful, generational, constrained, global).

% Contractors and research houses with sunk investment in autonomous targeting, swarming, and loitering-munition technology. A categorical ban closes off their product class in every adherent jurisdiction and chills export markets everywhere the stigma reaches. They lobby against the instrument and pivot product lines toward human-supervised autonomy; the pivot preserves revenue but strands a decade of specialized capital.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, autonomous_systems_defense_industry, payer,
    organized, biographical, constrained, global).

% The state parties administering the Convention on Certain Conventional Weapons and its Group of Governmental Experts, where the autonomous-weapons file has lived since 2014 under a consensus rule that lets any party block an outcome. They set the agenda, commission the drafts, and own the decision of whether the categorical reading becomes treaty text. The consensus rule traps them: no party can impose closure, and none can walk away without collapsing the only venue where the question is formally alive.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, ccw_high_contracting_parties, agenda_setter,
    institutional, generational, constrained, global).

% States positioned to decline whatever instrument emerges — keeping the full option space while rivals bind themselves. If the categorical norm consolidates among signatories, every hour of restraint purchased by others is advantage banked for themselves. They bear no cost of the norm and collect its strategic dividend; their abstention is the standing arbitrage the enforcement debate keeps failing to close.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, nonparty_development_states, beneficiary,
    powerful, generational, arbitrage, global).

% Academic and institutional lawyers who adjudicate the interpretive question: whether the Martens Clause actually compels a categorical prohibition or merely licenses it as one reading among several. They publish the competing grammars — decision-moment tests, performance thresholds, per se dignity claims — and their split is the scholarly shadow of the diplomatic stalemate. Their seat observes the whole structure; it commands no votes.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, international_humanitarian_law_jurists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__categorical_prohibition_reading, capability_lagging_states).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__categorical_prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem three ways at once: it arrests an arms race before a machine-killing threshold normalizes; a class-level bright line eliminates the verification burden that performance-based rules would create (no need to certify how good a banned system is); and it preserves an unbroken chain of human answerability for every lethal decision, closing the accountability gap that machine-decided killing opens. Stated without evaluation: these are the problems the arrangement coordinates around.
% TRANSFER_FUNCTION: Moves decision authority over lethal force away from machine systems and back to human operators; moves capability-option space out of the hands of advanced military powers and their industrial bases; moves interpretive authority over weapons law toward humanitarian institutions; and moves relative security toward states that never had the capability being banned.
% ABSENT_VOICES: Missing from the CCW consensus room and from the 'public conscience' determination: the engineers and defense technologists whose professional judgment on feasibility and risk is treated as interested rather than conscientious; the publics of advanced military powers whose security rests on technological edge; service members who might rationally prefer machine overwatch in attritional or contaminated environments; and populations under bombardment today who might be defended more effectively by systems the reading would outlaw. Civil-society observer accreditation skews toward prohibition advocacy, so the room hears conscience filtered through one coalition.
% DISAPPEARANCE_RATIONALE: If the categorical norm vanished overnight, the governance space would refill immediately: the human-agency and outcomes-based readings would compete for the vacuum, unconstrained development would resume openly in capable states, the stigmatization architecture (campaigns, export controls, draft protocols) would dissolve into ordinary arms-trade politics, and the decade of agenda investment at the CCW would redirect to performance-certification debates. Arrangements visibly depend on it.
% FOUNDING_PROBLEM: Built to solve a threshold problem before it arrives: once machines routinely select and kill human targets, the delegation becomes normalized and irreversible, the accountability chain breaks (no one answers for a machine's kill), and an arms race in autonomous killing acquires its own momentum. The founders' aim was a preemptive constitutional moment for warfare — draw the line while crossing it is still a choice.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary coalition: the advanced military states that resist the categorical remedy nonetheless attest the underlying problem in their own directives — e.g., United States doctrine requiring appropriate levels of human judgment over lethal force — and independent jurists across the interpretive camps affirm the accountability gap is real even while disputing that a class ban follows from it. What no outsider attests is the categorical remedy itself; corroboration covers the problem, not this reading's solution.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__categorical_prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__categorical_prohibition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__categorical_prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 0.84, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, ExtMetricName, E),
    domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ihl_distinction_proportionality__categorical_prohibition_reading),
    narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.84) because the categorical form maximizes foreclosure: it bans the class, not the failure, so compliance purchases nothing and the only exit is surrendering the capability outright; costs concentrate on actors with advanced programs while laggards and nonparties gain. Suppression (0.70) is a raw structural input, unscaled by power or scope: the norm survives only through ratification pressure, stigmatization, export-control harmonization, and prospective criminalization, aimed at a defined class while leaving supervised autonomy lawful — hence substantial but not total. Theater_ratio (0.50) reflects a decade in which declaratory output (resolutions, pledges, panel conclusions) has outrun binding effect; roughly half the observable activity around the norm is positional performance. Accessibility_collapse (0.55): within adherent jurisdictions the banned class closes completely, but the underlying engineering knowledge persists and nonparty programs continue, so alternatives collapse politically rather than physically. Resistance (0.72): major military powers have blocked consensus at the CCW for the whole interval while continuing programs. All three tracked series share one seven-point grid (2014-2026, biennial); the 2026 column is projected, the rest observed. Extraction and enforcement rise monotonically — no oscillation — so no cyclical commentary applies; the rising base_extractiveness series under a mountain claim is the T17 accumulation signal, offered as hypothesis for investigation, not as a verdict. Coordination type is declared enforcement_mechanism: the arrangement's primary coordination function is a legal restraint framework whose failure mode is resumed arms competition.
 *
 * PERSPECTIVAL GAP:
 *   Seats should classify differently. From the advanced-powers seat the norm is an evidence-proof ideology: their best engineering is unlawful by stipulation, so the arrangement computes as enforced extraction with no relief path. From the lagging-states seat the same structure is cheap security — a rule that freezes the field — computing as coordination they would defend. The humanitarian-establishment seat fuses the norm with institutional identity, so its exit costs are internal rather than strategic. The agenda-setter seat experiences deadlock rather than either. The jurist seat sees all of it and splits. The engine derives these per-seat classifications from power, exit, and role data; the story does not adjudicate which perception is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to d as follows. Beneficiaries: capability_lagging_states (organized, constrained exit) sit near the full-beneficiary end — the norm subsidizes them with rivals' restraint; anti_militarist_civil_society (mobile) collects mobilization capital and can redirect; humanitarian_diplomacy_establishment (identity_locked) collects authority and cannot leave without repudiating institutional self-understanding; civilian_populations_at_risk (trapped, powerless) are the intended protectees; nonparty_development_states (arbitrage) approach the extreme beneficiary pole — they pay nothing and collect the strategic dividend of others' compliance. Payers: advanced_autonomy_military_powers (constrained) and autonomous_systems_defense_industry (constrained) sit near the full-target end — the ban takes their option space wholesale and offers no performance exit. frontline_service_members straddle: protected by the human-answerability guarantee, taxed by lost autonomous force protection. The agenda-setter and jurist seats are administrative and analytical respectively. Scope is global throughout, which amplifies effective extraction modestly at the target seats, since verifying class-wide abstention is hardest at planetary scale.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — who answers for a lethal decision, and can a machine-killing threshold be crossed reversibly — is live by the testimony of all sides, including the states that resist the remedy; mandatrophy is therefore not resolved and no sunset applies. The classification work this story performs is preventing two symmetrical mislabels: reading the norm as pure moral rope (ignoring that its categorical form transfers strategic position from advanced to lagging actors and banks arbitrage for nonparties) or as pure snare (ignoring the genuine collective-action core — arms-race suppression, the verification economy of a bright line, accountability preservation — that even resisting states acknowledge in their own directives). The mountain claim plus the rising extraction series routes the naturalization question to false-summit evaluation and T17 investigation rather than settling it by assertion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    martens_clause_naturalness_ambiguity,
    'Is the categorical prohibition a discovery of pre-political natural law — principles of humanity operating independently of anyone''s enforcement — or a constructed norm advanced by identifiable parties for identifiable position?',
    'Comparative genealogy and counterfactual test: trace whether the ''dictates of public conscience'' determination tracks principled argument or coalition composition, and test the reading''s response to adverse performance evidence — a genuine per se principle is evidence-insensitive by construction, whereas a constructed norm shows motivated reasoning under pressure.',
    'If constructed, the mountain claim fails and the false-summit chain reclassifies toward a coordinated-but-extractive type; if natural, the measured burden on advanced-capability actors is the price of a moral floor rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martens_clause_naturalness_ambiguity, conceptual, 'Natural law versus constructed norm in the Martens Clause grounding.').

omega_variable(
    evidence_insensitivity_diagnostic,
    'Is the reading''s invariance to technical performance a principled deontological commitment, or an unfalsifiable structure that insulates the norm from exactly the evidence that would discipline rival readings?',
    'Observe the categorical camp''s updating behavior when fielded systems demonstrate super-human distinction or proportionality performance: principled invariance predicts no change in the claim; insulation predicts goalpost migration (new auxiliary harms, slippery-slope arguments).',
    'Determines whether epsilon is stable across observables (one clean constraint) or whether the constraint decomposes into a dignity core plus an evidentiary shield with separable extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evidence_insensitivity_diagnostic, empirical, 'Whether performance-invariance is principle or shield.').

omega_variable(
    lagging_state_motivation_mix,
    'Do capability-lagging states support the categorical reading from convictions of humanity or from leveling self-interest — locking in rivals'' disadvantage at zero cost to themselves?',
    'Revealed-preference analysis: compare their enthusiasm for the ban against their support for costly verification and compliance machinery (conviction welcomes verification; pure leveling prefers cheap declaratory norms), cross-checked against autonomous-capability indices.',
    'If leveling interest dominates, the coordination function thins and the arrangement drifts toward enforced extraction with a moral cover story; if conviction dominates, coordination content strengthens and the extraction reading softens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lagging_state_motivation_mix, empirical, 'Conviction versus leveling interest in the beneficiary coalition.').

omega_variable(
    nonparty_arbitrage_hollowing,
    'Will nonparty development hollow the norm — signatories paying the full opportunity cost while outsiders bank the strategic dividend — as with earlier weapons conventions?',
    'Track nonparty autonomous-weapons programs and doctrine after any instrument''s adoption; measure whether adherence correlates inversely with capability level (capable states abstaining, incapable states joining).',
    'If arbitrage materializes, effective extraction on compliant advanced states amplifies sharply (they pay while rivals do not), pushing the arrangement toward a snare profile for its own adherents; robust universalization would dampen it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nonparty_arbitrage_hollowing, empirical, 'Whether the enforcement gap converts the ban into unilateral disarmament.').

omega_variable(
    sibling_framework_compatibility,
    'Can the categorical reading and the outcomes-based reading coexist in any single state''s legal framework, or does adopting one logically commit a party to rejecting the other''s core premise?',
    'Doctrinal analysis: test whether ''unlawful in all cases'' admits performance-conditioned exceptions without dissolving the category, and survey whether any state''s stated position genuinely holds both.',
    'Sets the foreclosure structure of the kernel family: mutual foreclosure forces a winner-take-all codification fight; compatibility permits hybrid practice (categorical domestically, performance-based in coalition operations) that changes which seats bear the burden.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_framework_compatibility, conceptual, 'Logical compatibility of categorical and performance-based frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__categorical_prohibition_reading, 2014, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t2014, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2014, 0.2).
narrative_ontology:measurement_basis(ihl__tr_t2014, observed).
narrative_ontology:measurement(ihl__tr_t2016, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2016, 0.25).
narrative_ontology:measurement_basis(ihl__tr_t2016, observed).
narrative_ontology:measurement(ihl__tr_t2018, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2018, 0.3).
narrative_ontology:measurement_basis(ihl__tr_t2018, observed).
narrative_ontology:measurement(ihl__tr_t2020, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2020, 0.36).
narrative_ontology:measurement_basis(ihl__tr_t2020, observed).
narrative_ontology:measurement(ihl__tr_t2022, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2022, 0.42).
narrative_ontology:measurement_basis(ihl__tr_t2022, observed).
narrative_ontology:measurement(ihl__tr_t2024, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2024, 0.47).
narrative_ontology:measurement_basis(ihl__tr_t2024, observed).
narrative_ontology:measurement(ihl__tr_t2026, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2026, 0.5).
narrative_ontology:measurement_basis(ihl__tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(ihl__be_t2014, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2014, 0.55).
narrative_ontology:measurement_basis(ihl__be_t2014, observed).
narrative_ontology:measurement(ihl__be_t2016, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2016, 0.6).
narrative_ontology:measurement_basis(ihl__be_t2016, observed).
narrative_ontology:measurement(ihl__be_t2018, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2018, 0.65).
narrative_ontology:measurement_basis(ihl__be_t2018, observed).
narrative_ontology:measurement(ihl__be_t2020, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2020, 0.7).
narrative_ontology:measurement_basis(ihl__be_t2020, observed).
narrative_ontology:measurement(ihl__be_t2022, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2022, 0.75).
narrative_ontology:measurement_basis(ihl__be_t2022, observed).
narrative_ontology:measurement(ihl__be_t2024, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2024, 0.8).
narrative_ontology:measurement_basis(ihl__be_t2024, observed).
narrative_ontology:measurement(ihl__be_t2026, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2026, 0.84).
narrative_ontology:measurement_basis(ihl__be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t2014, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2014, 0.35).
narrative_ontology:measurement_basis(ihl__su_t2014, observed).
narrative_ontology:measurement(ihl__su_t2016, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2016, 0.42).
narrative_ontology:measurement_basis(ihl__su_t2016, observed).
narrative_ontology:measurement(ihl__su_t2018, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2018, 0.48).
narrative_ontology:measurement_basis(ihl__su_t2018, observed).
narrative_ontology:measurement(ihl__su_t2020, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2020, 0.54).
narrative_ontology:measurement_basis(ihl__su_t2020, observed).
narrative_ontology:measurement(ihl__su_t2022, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2022, 0.6).
narrative_ontology:measurement_basis(ihl__su_t2022, observed).
narrative_ontology:measurement(ihl__su_t2024, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2024, 0.66).
narrative_ontology:measurement_basis(ihl__su_t2024, observed).
narrative_ontology:measurement(ihl__su_t2026, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2026, 0.7).
narrative_ontology:measurement_basis(ihl__su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__categorical_prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__outcomes_based_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'IHL and autonomous weapons' decomposes into three epsilon-distinct constraints sharing one kernel. This file is the categorical reading (class-level ban, highest epsilon, performance-invariant). The human_agency_reading conditions lawfulness on the decision moment (moderate epsilon); the outcomes_based_reading conditions it on demonstrated performance (lowest epsilon, technology-neutral). Upstream/downstream: the categorical reading is cited as the moral ceiling that pressures the human-agency reading's floor, and both are resisted through the outcomes-based reading's evidentiary grammar. Family members link mutually through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
