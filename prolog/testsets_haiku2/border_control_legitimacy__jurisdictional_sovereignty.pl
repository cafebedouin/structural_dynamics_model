% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__jurisdictional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__jurisdictional_sovereignty, []).

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
 *   constraint_id: border_control_legitimacy__jurisdictional_sovereignty
 *   human_readable: Border Control Legitimacy (Jurisdictional Sovereignty Reading)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   Border control legitimacy in the jurisdictional sovereignty reading
 *   treats state authority to regulate entry as grounded in Westphalian
 *   sovereignty (territorial jurisdictional power) but constrained by human
 *   rights proportionality obligations and a quid pro quo: the state's
 *   authority to exclude is legitimate only if coupled with demonstrable
 *   protection of domestic labor conditions and public consent from existing
 *   members. This reading sits between the sovereignty_primary reading
 *   (absolute discretion to exclude) and the freedom_of_movement_primary
 *   reading (human rights override state closure authority). The constraint
 *   identifies two victim sets: excluded migrants who bear the cost of
 *   enforcement, and displaced citizens whose labor conditions may be
 *   undermined by admission or threatened by high enforcement costs.
 *   Legitimacy crisis occurs when enforcement violates human rights OR when
 *   admission undermines public consent to the state's authority itself.
 *
 * KEY AGENTS:
 *   - State apparatus: exercises jurisdictional authority, administers and enforces border policy, claims Westphalian sovereignty
 *   - Excluded migrants: bear the cost of enforcement (denial of entry, family separation, forced return); are regulated by state authority but not parties to the legitimacy-granting citizenry
 *   - Displaced citizens: bear labor-market costs but consent-source for legitimacy; demand protection obligations as quid pro quo for enforcement authority
 *   - Domestic labor coalitions: benefit from border enforcement when coupled with labor protections; coordinate around the proportionality requirement
 *   - International human rights bodies: monitor proportionality compliance, hold state apparatus accountable for human rights violations, shape legitimacy standards
 *   - Origin state governments: structurally excluded from receiving state's jurisdiction but claim interest in nationals abroad
 *   - Receiving state courts: constrain executive discretion through proportionality review, mediate between sovereignty and human rights
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, 0.62).
domain_priors:suppression_score(border_control_legitimacy__jurisdictional_sovereignty, 0.71).
domain_priors:theater_ratio(border_control_legitimacy__jurisdictional_sovereignty, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, extractiveness, 0.62).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__jurisdictional_sovereignty, "Border Control Legitimacy (Jurisdictional Sovereignty Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__jurisdictional_sovereignty, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__jurisdictional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__jurisdictional_sovereignty, 'b2ac60c9-c1b9-48b3-9456-40ad2cf1bbf1').
narrative_ontology:cs_kernel_codification('b2ac60c9-c1b9-48b3-9456-40ad2cf1bbf1', formalized).
narrative_ontology:cs_authority_grounding('b2ac60c9-c1b9-48b3-9456-40ad2cf1bbf1', extraction).
narrative_ontology:cs_interpretation_layer_present('b2ac60c9-c1b9-48b3-9456-40ad2cf1bbf1').
narrative_ontology:cs_reading_relation('b2ac60c9-c1b9-48b3-9456-40ad2cf1bbf1', border_control_legitimacy__sovereignty_primary, influences).
narrative_ontology:cs_reading_relation('b2ac60c9-c1b9-48b3-9456-40ad2cf1bbf1', border_control_legitimacy__freedom_of_movement_primary, influences).
narrative_ontology:cs_axiom('b2ac60c9-c1b9-48b3-9456-40ad2cf1bbf1', foundational, jurisdictional_authority_separable_from_closure_authority).
narrative_ontology:cs_axiom_status(jurisdictional_authority_separable_from_closure_authority, holdable).
narrative_ontology:cs_axiom_grounding('b2ac60c9-c1b9-48b3-9456-40ad2cf1bbf1', jurisdictional_authority_separable_from_closure_authority, deontological).
narrative_ontology:cs_axiom('b2ac60c9-c1b9-48b3-9456-40ad2cf1bbf1', foundational, legitimacy_requires_proportionality_and_reciprocity).
narrative_ontology:cs_axiom_status(legitimacy_requires_proportionality_and_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('b2ac60c9-c1b9-48b3-9456-40ad2cf1bbf1', legitimacy_requires_proportionality_and_reciprocity, deontological).
narrative_ontology:cs_reference_frame('b2ac60c9-c1b9-48b3-9456-40ad2cf1bbf1', post_wwii_international_law_reconciliation).
narrative_ontology:cs_drift_state('b2ac60c9-c1b9-48b3-9456-40ad2cf1bbf1', contemporary_migration_crisis_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b2ac60c9-c1b9-48b3-9456-40ad2cf1bbf1', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, state_apparatus).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, domestic_labor_protection_coalitions).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, business_sectors_dependent_on_migration).
narrative_ontology:constraint_vindicates(border_control_legitimacy__jurisdictional_sovereignty, westphalian_statehood).
narrative_ontology:constraint_vindicates(border_control_legitimacy__jurisdictional_sovereignty, proportionality_doctrine).
narrative_ontology:constraint_vindicates(border_control_legitimacy__jurisdictional_sovereignty, public_consent_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces border control policy. Possesses jurisdictional authority to regulate rights and obligations within territory. Claims legitimacy grounded in Westphalian sovereignty and public consent. Enforces both admission standards (proportionality-constrained) and exclusion (necessity-justified). Bears the enforcement cost and the accountability for violating human rights; simultaneously pressured to protect domestic labor conditions.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Bear the primary cost of border control enforcement: denial of entry, family separation, forced return to origin. Are subject to the state's jurisdictional regulatory power regarding entry and residence but are not parties to the state's governance structure that legitimizes that power. Face proportionality constraints in theory (humanitarian exceptions, family unity protections) but enforcement discretion often overrides them.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Bear costs of labor-market competition and public-service competition when admission is high; benefit from public consent legitimacy that requires the state to protect their labor conditions as a condition of border enforcement. Are parties to the state's governance structure but their voice in border policy is mediated through elections and advocacy, not direct stake in border outcomes. Seek credible protection of labor standards as the quid pro quo for accepting border control authority.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens, beneficiary).

% Benefit from border control when it moderates labor supply and supports wage/condition protections. Coordinate around the claim that state authority to regulate borders is legitimate only if coupled with protection obligations to existing residents. Mobilize political pressure to link border enforcement to labor standards enforcement.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, domestic_labor_protection_coalitions, beneficiary,
    organized, biographical, mobile, national).

% Monitor whether border enforcement complies with human rights law (non-refoulement, family unity, freedom from torture). Hold the state apparatus accountable for proportionality violations and necessity overreach. Their verdicts shape legitimacy claims and can trigger remediation or sanctions.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, international_human_rights_bodies, observer,
    institutional, generational, analytical, universal).

% Claim sovereign interest in the welfare of their nationals abroad and in return migration. Are structurally excluded from the receiving state's jurisdictional authority but would argue for voice in the mutual recognition of migration governance. Their absence from legitimacy deliberation is a structural feature of the Westphalian frame.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, origin_state_governments, excluded,
    moderate, generational, constrained, national).

% Interpret and enforce border control law. Constrain executive discretion through proportionality review and necessity scrutiny. Mediate between sovereign authority (jurisdictional power to exclude) and human rights obligations (proportionality constraints). Their rulings shape what 'legitimate' border enforcement looks like in practice.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, receiving_state_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__jurisdictional_sovereignty, receiving_state_courts, observer).

% Benefit from border control regimes that permit controlled labor migration while excluding non-authorized workers. Lobby for admission of specific skill categories while supporting enforcement against undocumented migration. Leverage border control apparatus to manage labor supply.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, business_sectors_dependent_on_migration, beneficiary,
    powerful, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__jurisdictional_sovereignty, state_apparatus).
narrative_ontology:fixing_cost_class(border_control_legitimacy__jurisdictional_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single authoritative framework (jurisdictional sovereignty) for regulating entry, residence, and rights-bearing status within territory. Solves the coordination problem of 'who decides access to a bounded political community and under what legitimacy conditions?' by locating that authority in the state apparatus, constrained by proportionality obligations and accountability to existing members via public consent.
% TRANSFER_FUNCTION: Moves the cost of exclusion and labor-market regulation from those who benefit (domestic labor coalitions, state apparatus revenue from border enforcement, business sectors practicing discriminatory hiring) to those who bear it (excluded migrants denied entry, displaced citizens in competition with admitted workers, origin states losing nationals). Legitimacy turns on whether the state reciprocally protects the labor conditions of its own citizens as part of the quid pro quo for enforcement authority.
% ABSENT_VOICES: Origin state governments are structurally excluded from the receiving state's jurisdictional authority framework; their nationals are the population most directly regulated by border policy but they have no seat in legitimacy deliberation. Excluded migrants themselves have no direct voice (though international human rights bodies speak for some of their interests). The framing assumes recipients (domestic citizens) are the legitimacy-granting constituency, but this brackets the question of whether non-residents should participate in decisions that bind their mobility.
% DISAPPEARANCE_RATIONALE: If this particular reading of border legitimacy vanished and was replaced by another (e.g. sovereignty_primary with no proportionality constraints, or freedom_of_movement_primary with no border closure authority), the institutional arrangements would shift dramatically: either enforcement would become unconstrained and human-rights accountable, or border authority would dissolve and movement would be substantially deregulated. The labor protections tied to this reading's quid pro quo would collapse if the reading disappeared.
% FOUNDING_PROBLEM: After WWII and the establishment of the UN, states needed a framework to reconcile two conflicting demands: (1) the Westphalian right to territorial control and self-determination, and (2) emerging international human rights law that constrained absolute sovereignty. The founding problem is: how can a state exercise jurisdictional authority over its territory and population without reducing border control to naked power, and how can it justify excluding people from access to its labor market and citizenship while remaining legitimate in the eyes of its own citizens?
% FOUNDING_PROBLEM_CORROBORATION: Post-war international law texts (Universal Declaration of Human Rights, Geneva Conventions, regional human rights frameworks) attest that the founding problem remains live: states continue to claim sovereignty while international law constrains it. NGOs, courts, and scholars outside beneficiary circles testify that the tension between sovereignty and human rights is unresolved. The EU's attempted balance (free movement within, proportionality-constrained external borders) and its crises (2015 migration surge, rule-of-law conflicts) confirm the founding problem persists and the proposed solution is contested.
narrative_ontology:disappearance_verdict(border_control_legitimacy__jurisdictional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__jurisdictional_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__jurisdictional_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_control_legitimacy__jurisdictional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__jurisdictional_sovereignty, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the constraint concentrates the benefit of border enforcement (labor protection, revenue, state control) in the hands of the state apparatus and domestic coalitions, while concentrating the cost in excluded migrants and displaced citizens. The temporal series shows extractiveness rising from 0.48 to 0.62 over 15 observed years, then plateauing — this reflects increasing political mobilization around border control (beneficiaries pressing for enforcement) and increasing human rights contestation (victims pressing for proportionality), which creates a temporary extraction ratchet until either legitimacy crisis erupts or the quid pro quo is credibly renewed. Suppression is high (0.71) because the constraint requires active enforcement machinery to exclude (deportations, border militarization, visa systems) and because excluded migrants have trapped exit (nowhere else to go if denied entry). Theater rises from 0.35 to 0.48 because proportionality review and human rights monitoring are real (actors must justify enforcement) but enforcement discretion is wide; the increasing theater ratio reflects the state's growing reliance on legitimacy performance (humanitarian rhetoric) as actual protections falter. Accessibility collapse is moderate (0.58): excluded migrants see alternatives (not entering, fleeing to other states, litigation) but they are costly and often unavailable; displaced citizens see constrained alternatives (labor organizing, political mobilization) but the Westphalian frame makes them seem natural. Resistance is high (0.73): excluded migrants resist through asylum claims, courts, international advocacy; displaced citizens resist through political pressure for labor protections; origin states resist through diplomatic pressure. This is tangled_rope because: (a) genuine coordination function (solving 'who decides access to a bounded political community'), (b) asymmetric extraction (excluded migrants and displaced citizens pay, state apparatus and labor coalitions benefit), (c) active enforcement required to maintain exclusion and proportionality constraints.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus and domestic labor coalitions see this constraint as a balanced framework grounded in legitimate sovereignty and proportionality. From the excluded migrant seat, the same structure is pure extraction dressed in legitimacy language — Westphalian sovereignty is invoked to justify the right to exclude, proportionality review is performed as theater (humanitarian rhetoric without real teeth), and the extraction accrues to insiders while costs fall on powerless outsiders. From the displaced citizen seat, legitimacy is conditional: the state's authority to exclude is legitimate only if it actually protects labor conditions; when this quid pro quo fails, the seat shifts from beneficiary to payer. The engine computes per-seat types from the structural data; the authored claim (tangled_rope at the story level) reflects the reading's own frame, while the metrics (moderate-high extraction, high suppression, rising theater) capture the structural tensions and asymmetries the reading instantiates.
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus: near-beneficiary end (d ≈ 0.2). Controls the rules, performs enforcement, claims legitimacy, collects political benefit. Has arbitrage-grade exit (can change the rules unilaterally, though at political cost). Institutional power. Excluded migrants: at the full-target end (d ≈ 0.95). Bear the primary cost of enforcement, have no voice in legitimacy deliberation, have trapped exit (cannot enter if denied). Powerless. Displaced citizens: near-symmetric (d ≈ 0.45-0.55). Benefit from labor protection (coordination function) but bear labor-market competition costs (asymmetric extraction). Have constrained exit (can organize, vote, migrate within the state) and moderate power. Domestic labor coalitions: near-beneficiary end (d ≈ 0.25). Benefit directly from border enforcement coupled with labor protections. Have mobile exit (can shift coalitions or labor markets) and organized power. This directionality profile is derived from beneficiary/victim declarations plus power/exit atoms; no override needed because the structure is internally coherent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to reconcile sovereignty with human rights) remains live, as confirmed by post-war international law and ongoing court cases. The constraint does NOT exhibit mandatrophy: the state apparatus and labor coalitions still perceive and claim to be solving the founding problem (jurisdictional authority coupled with proportionality and public consent). However, the measurements show rising theater ratio (0.35→0.48) and rising extraction (0.48→0.62) over 15 observed years, which suggests creeping Goodhart drift: the proportionality function is being performed (courts, humanitarian reviews) while actual protections (human rights compliance, labor standards enforcement) may be failing. This is the precursor to mandatrophy: if extraction continues to rise while proportionality theater remains constant, eventually the reading's own legitimacy conditions will fail and the mandate will be perceived as dead (either replaced by sovereignty_primary pure extraction, or by freedom_of_movement_primary override). Current state (t=0-15 observed): tangled_rope with functioning quid pro quo but visible strain. Future state (t=20-30 projected): if theater and extraction trajectories diverge further, legitimacy crisis; if they re-converge (enforcement genuinely couples with protection), the reading persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_constraint_enforceability,
    'Can proportionality obligations be enforced against state border apparatus, or do courts defer to executive discretion in ways that hollow out the constraint?',
    'Audit of court rulings over a decade: measure the rate at which courts overturn border enforcement decisions on proportionality grounds. If courts overturn <5% of cases, proportionality is performative; if >30%, it is a functioning constraint.',
    'If proportionality is performative, theater_ratio is fabricated legitimacy and extraction is underreported (the state claims constraint it does not actually observe). If proportionality is enforced, the reading functions as authored — tangled_rope with real constraint, not snare with constraint theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_constraint_enforceability, empirical, 'Whether proportionality review actually constrains state border enforcement or is procedural theater.').

omega_variable(
    quid_pro_quo_labor_protection_credibility,
    'When border enforcement is high, do displaced citizens perceive credible protection of their labor conditions (wages, working hours, union rights)? Or is the quid pro quo rhetoric without corresponding enforcement?',
    'Survey displaced citizens and labor organizations on whether they see border enforcement as coupled to labor protection. If >60% perceive coupling, the quid pro quo is credible; if <30%, the reading''s legitimacy foundation is undermined.',
    'If quid pro quo is credible, displaced citizens remain beneficiaries and the reading sustains legitimacy. If it is not credible, displaced citizens shift toward payer and the reading becomes a snare (extractive rhetoric without reciprocal protection).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quid_pro_quo_labor_protection_credibility, empirical, 'Whether the state''s border enforcement is perceived as coupled to labor protection by citizens whose consent grounds legitimacy.').

omega_variable(
    jurisdictional_authority_vs_border_closure_separability,
    'Are jurisdictional authority (power to regulate rights and obligations within territory) and border closure authority (power to exclude non-citizens from entry) structurally separable, or does one necessarily entail the other?',
    'Comparative constitutional analysis: do any legitimate states exercise jurisdictional authority while not exercising border closure authority (e.g., EU member states with Schengen freedom of movement)? If yes, the authorities are separable; if no, they are entangled.',
    'If separable, this reading''s core claim (sovereignty does not automatically entail closure authority) is structurally plausible. If entangled, the reading''s authority distinction collapses and the constraint must be reclassified as either sovereignty_primary or freedom_of_movement_primary (i.e., one of the readings forecloses this one).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(jurisdictional_authority_vs_border_closure_separability, conceptual, 'Whether jurisdictional authority and border closure authority are logically distinct or inseparably coupled.').

omega_variable(
    origin_state_exclusion_legitimacy,
    'Is the structural exclusion of origin states from receiving state border legitimacy deliberation itself a fundamental illegitimacy that undermines the entire reading?',
    'Normative analysis: if origin states have a legitimate claim to voice in decisions that affect their nationals'' mobility, then a framework that excludes them is incomplete. If origin states'' interests are captured through international human rights bodies, the exclusion is justified.',
    'If origin state exclusion is fundamental illegitimacy, the reading''s legitimacy foundation cracks at a deeper level — not just proportionality or quid pro quo, but the basic constituency for legitimacy (who gets a say) is contested. The reading''s claimed type would need revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(origin_state_exclusion_legitimacy, conceptual, 'Whether the structural exclusion of origin states from border legitimacy deliberation is itself a fatal flaw in the reading.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.71) primarily structural (legal barriers, border walls, visa systems) or internalized (excluded migrants internalize the message that they are not entitled to enter, belong elsewhere, or are naturally excluded)?',
    'Post-exclusion trajectory: if migrants expelled from a state continue to see entry as illegitimate and do not re-attempt crossing after being denied, suppression is partly internalized. If they immediately re-attempt entry or litigate, suppression is primarily structural.',
    'If suppression is internalized, the constraint''s hold is deeper and more stable (victims carry the suppression with them even after the structural barrier is removed). If structural, remedying the barrier would weaken the constraint. The distinction informs whether the constraint persists through consensus or coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether border suppression operates primarily through structural barriers or internalized legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__jurisdictional_sovereignty, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(bord_tr_t0, observed).
narrative_ontology:measurement(bord_tr_t5, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 5, 0.38).
narrative_ontology:measurement_basis(bord_tr_t5, observed).
narrative_ontology:measurement(bord_tr_t10, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(bord_tr_t10, observed).
narrative_ontology:measurement(bord_tr_t15, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 15, 0.46).
narrative_ontology:measurement_basis(bord_tr_t15, observed).
narrative_ontology:measurement(bord_tr_t20, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 20, 0.47).
narrative_ontology:measurement_basis(bord_tr_t20, projected).
narrative_ontology:measurement(bord_tr_t25, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(bord_tr_t25, projected).
narrative_ontology:measurement(bord_tr_t30, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(bord_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(bord_be_t0, observed).
narrative_ontology:measurement(bord_be_t5, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(bord_be_t5, observed).
narrative_ontology:measurement(bord_be_t10, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(bord_be_t10, observed).
narrative_ontology:measurement(bord_be_t15, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(bord_be_t15, observed).
narrative_ontology:measurement(bord_be_t20, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(bord_be_t20, projected).
narrative_ontology:measurement(bord_be_t25, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(bord_be_t25, projected).
narrative_ontology:measurement(bord_be_t30, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(bord_be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(bord_su_t0, observed).
narrative_ontology:measurement(bord_su_t5, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(bord_su_t5, observed).
narrative_ontology:measurement(bord_su_t10, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(bord_su_t10, observed).
narrative_ontology:measurement(bord_su_t15, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(bord_su_t15, observed).
narrative_ontology:measurement(bord_su_t20, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(bord_su_t20, projected).
narrative_ontology:measurement(bord_su_t25, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(bord_su_t25, projected).
narrative_ontology:measurement(bord_su_t30, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(bord_su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__jurisdictional_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_control_legitimacy__jurisdictional_sovereignty, 0.12).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy__sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy__freedom_of_movement_primary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel border_control_legitimacy. The kernel contests how to ground state authority over borders. The three readings (jurisdictional_sovereignty [this one], sovereignty_primary, freedom_of_movement_primary) share a common institutional domain (state border authority) but instantiate different constraints because they anchor legitimacy in different sources: jurisdictional authority coupled with proportionality obligations (this reading), absolute statehood entitlement to closure (sovereignty_primary), and human rights override of state exclusion (freedom_of_movement_primary). These are not different measurements of one constraint; they are structurally distinct constraints with different beneficiary sets, different extraction mechanisms, and different failure modes. The three stories must be authored separately and linked via network.affects_constraints. Each reading's ε is stable within its own frame; the readings do not average or blur. The three readings coexist as live positions in contemporary international law and political philosophy — they are not foreclosed against each other, but they do influence each other (this reading's proportionality constraints influence sovereignty_primary's enforcement costs; freedom_of_movement_primary's human rights framing influences the legitimacy conditions this reading claims).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
