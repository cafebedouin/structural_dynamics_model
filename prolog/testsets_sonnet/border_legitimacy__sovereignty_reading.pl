% ============================================================================
% CONSTRAINT STORY: border_legitimacy__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__sovereignty_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: border_legitimacy__sovereignty_reading
 *   human_readable: Territorial Sovereignty as Ground of Border Exclusion Authority
 *   domain: political_philosophy/migration_law/international_law
 *
 * SUMMARY:
 *   This story instantiates the sovereignty reading of the border-legitimacy
 *   kernel: the claim that a state's authority to exclude non-members derives
 *   directly from territorial sovereignty, and that this authority is a
 *   legitimate feature of statehood rather than a contingent policy requiring
 *   independent justification against each excluded person's interests. Under
 *   this reading, the excluded migrant is structurally a payer within a
 *   legitimate constraint, not a victim of an illegitimate one in the
 *   doctrine's own terms — but the authored metrics measure the constraint's
 *   actual operation, which shows substantial and rising extraction
 *   concentrated on a powerless, trapped population, alongside a genuine
 *   underlying coordination function for the citizen polity. This is the
 *   tangled_rope signature: real coordination (self-governance, resource
 *   allocation, democratic boundary-setting) bundled with asymmetric
 *   extraction (exclusion cost borne overwhelmingly by those with no voice in
 *   the deciding polity), sustained by active enforcement (border policing,
 *   detention, deportation machinery).
 *
 * KEY AGENTS:
 *   - state_administrative_apparatus: agenda_setter (institutional/analytical) — defines and enforces exclusion, grounds authority in sovereignty doctrine
 *   - citizen_polity: beneficiary (organized/arbitrage) — receives exclusive membership goods, bears none of the exclusion's direct cost
 *   - excluded_would_be_migrants: payer (powerless/trapped) — bears the full weight of exclusion with no standing to contest the sovereignty premise
 *   - undocumented_residents_already_present: payer (powerless/trapped) — lives inside the excluding territory subject to the same doctrine's removal power
 *   - international_refugee_and_migration_bodies: excluded (organized/constrained) — can criticize but cannot bind sovereign admission decisions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, 0.68).
domain_priors:suppression_score(border_legitimacy__sovereignty_reading, 0.79).
domain_priors:theater_ratio(border_legitimacy__sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__sovereignty_reading, "Territorial Sovereignty as Ground of Border Exclusion Authority").
narrative_ontology:topic_domain(border_legitimacy__sovereignty_reading, "political_philosophy/migration_law/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__sovereignty_reading, '3c824e0c-b79f-44a8-89ff-45d96a5e7bde').
narrative_ontology:cs_kernel_codification('3c824e0c-b79f-44a8-89ff-45d96a5e7bde', formalized).
narrative_ontology:cs_authority_grounding('3c824e0c-b79f-44a8-89ff-45d96a5e7bde', lineage).
narrative_ontology:cs_interpretation_layer_present('3c824e0c-b79f-44a8-89ff-45d96a5e7bde').
narrative_ontology:cs_reading_relation('3c824e0c-b79f-44a8-89ff-45d96a5e7bde', border_legitimacy__freedom_of_movement_reading, forecloses).
narrative_ontology:cs_reading_relation('3c824e0c-b79f-44a8-89ff-45d96a5e7bde', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('3c824e0c-b79f-44a8-89ff-45d96a5e7bde', foundational, territorial_sovereignty_grounds_exclusion).
narrative_ontology:cs_axiom_status(territorial_sovereignty_grounds_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('3c824e0c-b79f-44a8-89ff-45d96a5e7bde', territorial_sovereignty_grounds_exclusion, conventional).
narrative_ontology:cs_axiom('3c824e0c-b79f-44a8-89ff-45d96a5e7bde', secondary, membership_is_state_consent_not_individual_entitlement).
narrative_ontology:cs_axiom_status(membership_is_state_consent_not_individual_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('3c824e0c-b79f-44a8-89ff-45d96a5e7bde', membership_is_state_consent_not_individual_entitlement, conventional).
narrative_ontology:cs_reference_frame('3c824e0c-b79f-44a8-89ff-45d96a5e7bde', westphalian_territorial_sovereignty).
narrative_ontology:cs_drift_state('3c824e0c-b79f-44a8-89ff-45d96a5e7bde', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3c824e0c-b79f-44a8-89ff-45d96a5e7bde', '').
narrative_ontology:cs_kernel_id(border_legitimacy__sovereignty_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, citizen_polity).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, incumbent_labor_market_participants).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, state_administrative_apparatus).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, excluded_would_be_migrants).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, undocumented_residents_already_present).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, transnational_family_networks).
narrative_ontology:constraint_vindicates(border_legitimacy__sovereignty_reading, territorial_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_legitimacy__sovereignty_reading, state_consent_basis_of_membership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and enforces entry criteria, operates border checkpoints, detention and removal systems, and visa regimes. Derives its authority claim from the doctrine that a sovereign territorial state has an inherent right to control admission, and justifies enforcement machinery (walls, patrols, detention, deportation) as the exercise of that right rather than as a policy choice open to renegotiation.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, state_administrative_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Receives the exclusive claim on public goods, labor market position, and political voice that bordered membership confers. Citizens can travel, exit, and re-enter freely; the constraint operates almost entirely as a shield around what they already possess, not as a burden they bear.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, citizen_polity, beneficiary,
    organized, generational, arbitrage, national).

% Benefits from reduced labor supply competition in sectors where migrant entry is restricted. Some segments (agriculture, care work, construction) simultaneously depend on migrant labor supplied through irregular or precarious channels the same border regime creates, but the net structural position is protective of incumbent wages and standing.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, incumbent_labor_market_participants, beneficiary,
    moderate, biographical, mobile, national).

% Bears the full weight of the exclusion: denied entry, denied labor market access, denied family reunification, often facing risk of violence or destitution in the country of departure or transit. Has no standing to contest the sovereignty claim from within the excluding state's legal order; the only channels available are asylum categories that do not fit general economic or survival-driven movement.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, excluded_would_be_migrants, payer,
    powerless, biographical, trapped, global).

% Lives inside the excluding state's territory without recognized status, unable to access protections available to citizens, and subject to removal at any time under the same sovereignty claim used to justify the border. Exit to the country of origin is often more dangerous than remaining; exit to a third country is generally unavailable without documentation.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, undocumented_residents_already_present, payer,
    powerless, biographical, trapped, national).

% Family members are split across the border line; reunification depends on discretionary visa categories the sovereignty doctrine treats as gifts of the state rather than entitlements. The cost is borne in separated households, missed care obligations, and remittance-dependent economies structured around the border's persistence.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, transnational_family_networks, payer,
    powerless, biographical, constrained, global).

% Adjudicates specific claims of unlawful detention, asylum denial, or due-process failure without generally questioning the sovereignty premise itself; in doing so, courts both observe the constraint's operation and periodically re-legitimate it by ruling within, rather than against, the sovereignty frame.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, receiving_state_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__sovereignty_reading, receiving_state_courts, agenda_setter).

% UNHCR and comparable bodies can advise, monitor, and criticize but have no enforcement authority over a state's territorial exclusion decisions; sovereignty doctrine is precisely what limits their standing to compel admission, so their voice registers as commentary rather than binding constraint.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, international_refugee_and_migration_bodies, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__sovereignty_reading, citizen_polity).
narrative_ontology:fixing_cost_class(border_legitimacy__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Territorial sovereignty allows a bounded political community to coordinate on who shares in collectively produced goods (welfare systems, labor protections, democratic voice, security guarantees) without those goods being unboundedly claimable by anyone able to physically arrive — a genuine coordination problem for any finite-resource polity.
% TRANSFER_FUNCTION: Moves the costs of global inequality, conflict, and labor demand away from the excluding state's population and onto excluded migrants, transit-country populations, and split families, while channeling the benefits of controlled admission (protected wages, fiscal capacity, political cohesion) to the citizen polity.
% ABSENT_VOICES: Excluded would-be migrants have no standing in the excluding state's legal or political process by construction — the sovereignty claim itself is what denies them a seat. International refugee bodies can speak but not bind. Sending-state governments, who bear the fiscal and social cost of blocked emigration and remittance loss, are rarely party to the receiving state's admission decisions at all.
% DISAPPEARANCE_RATIONALE: If the sovereignty-based exclusion authority vanished, labor markets, welfare eligibility rules, political membership, and security screening in every bordered state would have to be rebuilt from different premises; millions of currently excluded people would gain standing to enter, and citizen-polity beneficiaries would lose the exclusivity of goods currently reserved to them. This is not a natural fact whose disappearance would leave arrangements untouched.
% FOUNDING_PROBLEM: Sovereignty-grounded exclusion was built to solve real problems of self-governance: enabling a bounded political community to set its own laws, allocate finite public resources, and control who participates in its democratic processes, against a historical backdrop of empire, conquest, and externally imposed rule.
% FOUNDING_PROBLEM_CORROBORATION: States and their courts attest the self-governance problem remains fully live. Migration scholars, sending-state governments, and international humanitarian bodies — parties outside the excluding state's direct beneficiary set — attest that the doctrine has been extended well past self-governance into a general license for exclusion untethered from any resource-scarcity or democratic-integrity justification, particularly where labor demand for migrant work coexists with formal exclusion. No source entirely outside some benefiting party (the state itself, or citizens who gain from exclusivity) attests the doctrine's current scope is fully justified by the founding problem alone.
narrative_ontology:disappearance_verdict(border_legitimacy__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_legitimacy__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__sovereignty_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) is high because the cost of exclusion is concentrated on people with zero voice in setting the rule, while gains (labor market protection, fiscal exclusivity, political cohesion) accrue to a polity that never bears the direct cost. Suppression (0.79) is high and structural: border enforcement, detention, and deportation infrastructure exist specifically to make the exclusion binding rather than advisory. Theater ratio is comparatively low (0.28) because most enforcement activity is functionally real (physical barriers, actual removals) rather than performative, though the ratio has risen over the interval as some enforcement (announced crackdowns, symbolic wall construction) increasingly substitutes visible action for administratively effective throughput control. Accessibility collapse (0.62) and resistance (0.71) both reflect that this is a constructed, contested arrangement, not a natural fact: alternatives (open admission regimes, regional free-movement zones) exist and are actively defended against by the state, and organized resistance (advocacy litigation, sanctuary movements, transnational solidarity networks) is substantial and growing.
 *
 * PERSPECTIVAL GAP:
 *   From the state administrative apparatus's seat, this constraint is a rope or even mountain-adjacent structural feature of statehood — sovereignty is treated as prior to and independent of any particular exclusion decision's effects. From the excluded migrant's seat, the identical structure is experienced as an enforced extraction with no coordination benefit reaching them at all. The engine's per-seat computation should reproduce this divergence directly from the declared power/exit/beneficiary-victim structure, without the story needing to assert which seat is 'correct' — that divergence is the data.
 *
 * DIRECTIONALITY LOGIC:
 *   The state administrative apparatus sets the rule and bears no cost from its own operation — it sits at the agenda-setter/analytical pole. The citizen polity is a clean structural beneficiary: full exit mobility, full benefit capture, near-zero direct cost, so directionality sits near the beneficiary end even without an override. Excluded would-be migrants and undocumented residents are full targets: trapped exit options, zero standing in the deciding process, and the entire cost of the arrangement lands on them — directionality sits near the full-target end, consistent with the derivation chain from victim declaration plus trapped exit. Incumbent labor market participants are a genuine but partial beneficiary class — mobile exit, moderate power, benefiting from reduced competition but with segments dependent on the same excluded labor, which is why they are declared beneficiary rather than payer despite mixed interests.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (self-governance, protection from externally imposed rule, resource allocation among a bounded community) remains partly live — small polities and fragile states plausibly still need exclusion authority to prevent overwhelming demographic or fiscal shock. But the doctrine's contemporary scope in wealthy receiving states — used to justify near-total exclusion of economic migrants and asylum seekers whose claims fall outside narrow categories, while labor markets simultaneously depend on irregular migrant labor — has drifted well past the scarcity and self-governance justification that originally grounded it. The tangled_rope classification (rather than snare) preserves the genuine coordination residue: unlike a pure snare, the sovereignty doctrine does solve a real self-governance problem for the citizen polity, which is exactly why blanket 'snare' framing would mislabel functioning democratic self-determination as pure extraction. But the asymmetric victim set, the active enforcement requirement, and the rising extraction trend across the interval block treating it as a clean rope either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_scope_boundary,
    'Does territorial sovereignty''s self-governance justification extend to justify exclusion of all non-members regardless of claim type, or only to exclusion decisions that protect a genuine scarcity or democratic-integrity interest?',
    'Comparative analysis of migration flows against actual fiscal, labor-market, and democratic-participation capacity constraints in receiving states — does exclusion scope track measurable scarcity, or exceed it categorically regardless of admission capacity?',
    'If exclusion scope tracks genuine scarcity, the sovereignty reading''s coordination function is doing most of the classificatory work and tangled_rope may understate coordination relative to extraction. If exclusion scope exceeds any measurable scarcity constraint, the coordination story is closer to cover, and the constraint would sit nearer snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_scope_boundary, conceptual, 'Whether sovereignty-grounded exclusion tracks genuine scarcity or exceeds it as a general exclusion license.').

omega_variable(
    committer_kernel_disagreement_location,
    'Where exactly do the three readings of the border_legitimacy kernel disagree — on the underlying moral fact (whether movement is a right), on the scope of a shared right (universal vs. persecution-limited), or only on enforcement mechanism given agreement on scope?',
    'Structural comparison of the three sibling constraint files'' beneficiary/victim declarations and axiom sets: freedom_of_movement_reading''s foundational axiom directly negates this reading''s territorial_sovereignty_grounds_exclusion axiom, while humanitarian_obligation_reading narrows rather than negates it.',
    'If the disagreement is located at the moral-fact level (this reading vs. freedom_of_movement_reading), the readings forecloses each other within a single legal framework. If located only at scope (this reading vs. humanitarian_obligation_reading), the readings can coexist as a spectrum within overlapping frameworks and the relation should be influences or coexists_with rather than forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_disagreement_location, conceptual, 'Locating exactly where the three sibling readings of the border_legitimacy kernel diverge.').

omega_variable(
    state_versus_citizen_beneficiary_asymmetry,
    'Is the state administrative apparatus itself a beneficiary of the sovereignty doctrine (bureaucratic budget, enforcement-agency headcount, political capital) independent of whether the citizen polity benefits, such that the apparatus has an interest in maintaining or expanding exclusion beyond what citizen welfare would require?',
    'Budgetary and institutional-growth analysis of border and immigration enforcement agencies relative to measured migration pressure and citizen-polity welfare indicators over the same interval.',
    'If the enforcement apparatus''s institutional interests diverge from and exceed citizen welfare, the state_administrative_apparatus stakeholder may need a secondary beneficiary role rather than pure agenda_setter, which would shift gain_flow away from a purely diffuse citizen-polity account toward partial institutional capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_versus_citizen_beneficiary_asymmetry, empirical, 'Whether the enforcing institution has independent beneficiary interests beyond the citizen polity it claims to serve.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__sovereignty_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1950, border_legitimacy__sovereignty_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(bord_tr_t1965, border_legitimacy__sovereignty_reading, theater_ratio, 1965, 0.14).
narrative_ontology:measurement(bord_tr_t1980, border_legitimacy__sovereignty_reading, theater_ratio, 1980, 0.17).
narrative_ontology:measurement(bord_tr_t1995, border_legitimacy__sovereignty_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(bord_tr_t2010, border_legitimacy__sovereignty_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(bord_tr_t2025, border_legitimacy__sovereignty_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(bord_be_t1950, border_legitimacy__sovereignty_reading, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement(bord_be_t1965, border_legitimacy__sovereignty_reading, base_extractiveness, 1965, 0.48).
narrative_ontology:measurement(bord_be_t1980, border_legitimacy__sovereignty_reading, base_extractiveness, 1980, 0.53).
narrative_ontology:measurement(bord_be_t1995, border_legitimacy__sovereignty_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(bord_be_t2010, border_legitimacy__sovereignty_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(bord_be_t2025, border_legitimacy__sovereignty_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1950, border_legitimacy__sovereignty_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(bord_su_t1965, border_legitimacy__sovereignty_reading, suppression_requirement, 1965, 0.55).
narrative_ontology:measurement(bord_su_t1980, border_legitimacy__sovereignty_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(bord_su_t1995, border_legitimacy__sovereignty_reading, suppression_requirement, 1995, 0.67).
narrative_ontology:measurement(bord_su_t2010, border_legitimacy__sovereignty_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(bord_su_t2025, border_legitimacy__sovereignty_reading, suppression_requirement, 2025, 0.79).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1950, tn=2025
narrative_ontology:measurement(bord_grid_01, border_legitimacy__sovereignty_reading, accessibility_collapse(class), 1950, 0.45).
narrative_ontology:measurement(bord_grid_02, border_legitimacy__sovereignty_reading, accessibility_collapse(class), 2025, 0.72).
narrative_ontology:measurement(bord_grid_03, border_legitimacy__sovereignty_reading, accessibility_collapse(individual), 1950, 0.4).
narrative_ontology:measurement(bord_grid_04, border_legitimacy__sovereignty_reading, accessibility_collapse(individual), 2025, 0.7).
narrative_ontology:measurement(bord_grid_05, border_legitimacy__sovereignty_reading, accessibility_collapse(organizational), 1950, 0.35).
narrative_ontology:measurement(bord_grid_06, border_legitimacy__sovereignty_reading, accessibility_collapse(organizational), 2025, 0.6).
narrative_ontology:measurement(bord_grid_07, border_legitimacy__sovereignty_reading, accessibility_collapse(structural), 1950, 0.5).
narrative_ontology:measurement(bord_grid_08, border_legitimacy__sovereignty_reading, accessibility_collapse(structural), 2025, 0.78).
narrative_ontology:measurement(bord_grid_09, border_legitimacy__sovereignty_reading, resistance(class), 1950, 0.2).
narrative_ontology:measurement(bord_grid_10, border_legitimacy__sovereignty_reading, resistance(class), 2025, 0.45).
narrative_ontology:measurement(bord_grid_11, border_legitimacy__sovereignty_reading, resistance(individual), 1950, 0.15).
narrative_ontology:measurement(bord_grid_12, border_legitimacy__sovereignty_reading, resistance(individual), 2025, 0.35).
narrative_ontology:measurement(bord_grid_13, border_legitimacy__sovereignty_reading, resistance(organizational), 1950, 0.25).
narrative_ontology:measurement(bord_grid_14, border_legitimacy__sovereignty_reading, resistance(organizational), 2025, 0.55).
narrative_ontology:measurement(bord_grid_15, border_legitimacy__sovereignty_reading, resistance(structural), 1950, 0.1).
narrative_ontology:measurement(bord_grid_16, border_legitimacy__sovereignty_reading, resistance(structural), 2025, 0.3).
narrative_ontology:measurement(bord_grid_17, border_legitimacy__sovereignty_reading, stakes_inflation(class), 1950, 0.42).
narrative_ontology:measurement(bord_grid_18, border_legitimacy__sovereignty_reading, stakes_inflation(class), 2025, 0.68).
narrative_ontology:measurement(bord_grid_19, border_legitimacy__sovereignty_reading, stakes_inflation(individual), 1950, 0.38).
narrative_ontology:measurement(bord_grid_20, border_legitimacy__sovereignty_reading, stakes_inflation(individual), 2025, 0.75).
narrative_ontology:measurement(bord_grid_21, border_legitimacy__sovereignty_reading, stakes_inflation(organizational), 1950, 0.3).
narrative_ontology:measurement(bord_grid_22, border_legitimacy__sovereignty_reading, stakes_inflation(organizational), 2025, 0.55).
narrative_ontology:measurement(bord_grid_23, border_legitimacy__sovereignty_reading, stakes_inflation(structural), 1950, 0.4).
narrative_ontology:measurement(bord_grid_24, border_legitimacy__sovereignty_reading, stakes_inflation(structural), 2025, 0.6).
narrative_ontology:measurement(bord_grid_25, border_legitimacy__sovereignty_reading, suppression(class), 1950, 0.5).
narrative_ontology:measurement(bord_grid_26, border_legitimacy__sovereignty_reading, suppression(class), 2025, 0.77).
narrative_ontology:measurement(bord_grid_27, border_legitimacy__sovereignty_reading, suppression(individual), 1950, 0.45).
narrative_ontology:measurement(bord_grid_28, border_legitimacy__sovereignty_reading, suppression(individual), 2025, 0.82).
narrative_ontology:measurement(bord_grid_29, border_legitimacy__sovereignty_reading, suppression(organizational), 1950, 0.35).
narrative_ontology:measurement(bord_grid_30, border_legitimacy__sovereignty_reading, suppression(organizational), 2025, 0.6).
narrative_ontology:measurement(bord_grid_31, border_legitimacy__sovereignty_reading, suppression(structural), 1950, 0.55).
narrative_ontology:measurement(bord_grid_32, border_legitimacy__sovereignty_reading, suppression(structural), 2025, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_legitimacy__sovereignty_reading, 0.1).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__freedom_of_movement_reading).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% This file is one of three sibling readings of the border_legitimacy kernel. sovereignty_reading (this file) treats exclusion as a legitimate exercise of territorial sovereignty and computes as tangled_rope with a high, rising ε on exclusion. freedom_of_movement_reading treats border exclusion as presumptively illegitimate and would show a different, more extraction-dominant beneficiary/victim structure. humanitarian_obligation_reading narrows the legitimate-exclusion scope to non-protection claims only, shrinking the victim set to exclude those fleeing persecution or disaster. Each reading is ε-invariant on its own terms; the three do not average into one border-legitimacy ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_legitimacy__sovereignty_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
