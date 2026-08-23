% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__broad_effects_test
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__broad_effects_test, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: commerce_clause_scope__broad_effects_test
 *   human_readable: Commerce Clause — Substantial Effects / Aggregation Doctrine (Broad Effects Test)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   The broad effects test reads the Commerce Clause as authorizing federal
 *   regulation of any activity that, in the aggregate, substantially affects
 *   interstate commerce. This reading emerged from the 1937 constitutional
 *   revolution (NLRB v. Jones & Laughlin Steel, Wickard v. Filburn) and was
 *   extended to civil rights (Heart of Atlanta Motel, Katzenbach v. McClung)
 *   and beyond. It is claimed as a Rope — a genuine coordination mechanism
 *   for national problems — but operates with high extractiveness from state
 *   sovereignty and local autonomy, active enforcement through judicial
 *   precedent, and identifiable victims (states, local businesses, federalism
 *   as constraint). The engine will compute per-seat types from the
 *   structural data; the claimed_type (tangled_rope) reflects this author's
 *   judgment that both coordination and extraction are structurally present.
 *
 * KEY AGENTS:
 *   - federal_government: Primary agenda_setter (institutional/arbitrage) — sets and enforces the doctrine
 *   - federal_agencies: Primary beneficiary (institutional/constrained) — gains regulatory jurisdiction
 *   - civil_rights_enforcement: Beneficiary (institutional/constrained) — depends on broad commerce power
 *   - national_interest_groups: Beneficiary (organized/mobile) — seeks uniform federal standards
 *   - state_governments: Primary payer (organized/constrained) — loses regulatory autonomy
 *   - local_businesses: Payer (moderate/constrained) — bears compliance costs for local activity
 *   - state_sovereignty_advocates: Excluded (moderate/trapped) — constitutional vision excluded since 1937
 *   - originalist_scholars: Excluded (moderate/identity_locked) — professional commitment to excluded reading
 *   - constitutional_scholars: Observer (analytical/analytical) — no material stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, 0.72).
domain_priors:suppression_score(commerce_clause_scope__broad_effects_test, 0.68).
domain_priors:theater_ratio(commerce_clause_scope__broad_effects_test, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, extractiveness, 0.72).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__broad_effects_test, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__broad_effects_test, "Commerce Clause — Substantial Effects / Aggregation Doctrine (Broad Effects Test)").
narrative_ontology:topic_domain(commerce_clause_scope__broad_effects_test, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__broad_effects_test).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__broad_effects_test, '282dd1ee-adbf-404e-8a26-72cb83306f78').
narrative_ontology:cs_kernel_codification('282dd1ee-adbf-404e-8a26-72cb83306f78', formalized).
narrative_ontology:cs_authority_grounding('282dd1ee-adbf-404e-8a26-72cb83306f78', lineage).
narrative_ontology:cs_interpretation_layer_present('282dd1ee-adbf-404e-8a26-72cb83306f78').
narrative_ontology:cs_reading_relation('282dd1ee-adbf-404e-8a26-72cb83306f78', commerce_clause_scope__narrow_originalist, forecloses).
narrative_ontology:cs_reading_relation('282dd1ee-adbf-404e-8a26-72cb83306f78', commerce_clause_scope__intermediate_channels, coexists_with).
narrative_ontology:cs_axiom('282dd1ee-adbf-404e-8a26-72cb83306f78', foundational, aggregation_doctrine_valid).
narrative_ontology:cs_axiom_status(aggregation_doctrine_valid, holdable).
narrative_ontology:cs_axiom_grounding('282dd1ee-adbf-404e-8a26-72cb83306f78', aggregation_doctrine_valid, empirically_contingent).
narrative_ontology:cs_axiom('282dd1ee-adbf-404e-8a26-72cb83306f78', foundational, federal_power_includes_prohibition).
narrative_ontology:cs_axiom_status(federal_power_includes_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('282dd1ee-adbf-404e-8a26-72cb83306f78', federal_power_includes_prohibition, conventional).
narrative_ontology:cs_reference_frame('282dd1ee-adbf-404e-8a26-72cb83306f78', new_deal_commerce_expansion).
narrative_ontology:cs_drift_state('282dd1ee-adbf-404e-8a26-72cb83306f78', post_lopez_morrison_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('282dd1ee-adbf-404e-8a26-72cb83306f78', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__broad_effects_test, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, federal_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_interest_groups).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, civil_rights_enforcement).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_governments).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, local_economic_autonomy).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, federalism_as_constraint).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, local_businesses).
narrative_ontology:constraint_vindicates(commerce_clause_scope__broad_effects_test, national_market_unity).
narrative_ontology:constraint_vindicates(commerce_clause_scope__broad_effects_test, federal_civil_rights_authority).
narrative_ontology:constraint_vindicates(commerce_clause_scope__broad_effects_test, collective_action_problem_solution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces federal statutes under the Commerce Clause; administers the regulatory state through agencies; appoints judges who interpret the scope. Collects regulatory authority and policy-making power. Can shift doctrine through appointments and legislation but is constrained by Court precedent.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain expansive regulatory jurisdiction over economic and non-economic activity with cumulative national impact. Their budgets, staffing, and mission scope depend on broad Commerce Clause authority. Exit means accepting narrower jurisdiction or seeking new statutory bases.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_agencies, beneficiary,
    institutional, biographical, constrained, national).

% Lobby for uniform federal standards that preempt state variation (labor, environmental, consumer protection). Benefit from single national forum rather than 50 state battles. Can redirect advocacy to states if federal power contracts, but lose efficiency of centralized rulemaking.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_interest_groups, beneficiary,
    organized, biographical, mobile, national).

% Relies on Commerce Clause to reach private discrimination (Heart of Atlanta, Katzenbach). Gains federal enforcement tools against state and local resistance. If Commerce power narrows, must fall back on Section 5 of 14th Amendment — narrower and more contested.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, civil_rights_enforcement, beneficiary,
    institutional, generational, constrained, national).

% Lose regulatory autonomy over intrastate economic activity when federal government asserts cumulative impact. Must comply with federal mandates, often without full funding. Can resist through litigation, interposition, or constitutional amendment — all high-cost, low-probability exits.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_governments, payer,
    organized, biographical, constrained, regional).

% Subject to federal regulation of purely local activity (wheat for home consumption, hotel serving interstate travelers, medical marijuana). Compliance costs fall disproportionately on small operators. Exit means ceasing the regulated activity or relocating — often economically infeasible.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, local_businesses, payer,
    moderate, biographical, constrained, local).

% Argue for enumerated powers limits and state police power reservation. Their constitutional vision is structurally excluded from operative doctrine since 1937; they litigate at margins (Lopez, Morrison, NFIB) but cannot access the agenda-setting seat.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_sovereignty_advocates, excluded,
    moderate, generational, trapped, national).

% Produce intellectual architecture for narrow reading but remain outside governing coalition. Their exit is identity-locked — professional reputation and theoretical commitment bind them to the position even when it has no operative force.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, originalist_scholars, excluded,
    moderate, generational, identity_locked, national).

% Analyze doctrine from outside the power structure; no material stake in expansion or contraction. Provide the analytical surface the engine computes seat types from.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves collective action problems requiring uniform national rules: prevents state race-to-the-bottom in regulation, enables federal civil rights enforcement against state resistance, creates single national market by preempting balkanized state barriers.
% TRANSFER_FUNCTION: Transfers regulatory authority from state legislatures to Congress; transfers compliance costs from diffuse national majorities to targeted regulated entities (often local); transfers policy discretion from local democratic choice to centralized bureaucratic-adjudicative process.
% ABSENT_VOICES: State sovereignty advocates and originalist scholars are structurally excluded from the doctrinal conversation since the 1937 switch; local communities preferring distinct policy regimes (e.g., on land use, labor standards, drug policy) have no institutional voice in the federal aggregation calculus.
% DISAPPEARANCE_RATIONALE: If the substantial effects / aggregation doctrine vanished overnight, the modern federal regulatory state (environmental, labor, civil rights, drug control, firearms) would lose its primary constitutional foundation. Congress would need new bases (taxing, spending, treaty, 14th Amendment) — each narrower and more contested. State regulatory autonomy would expand dramatically.
% FOUNDING_PROBLEM: The Articles of Confederation failed because states could obstruct national commerce and evade collective obligations. The Commerce Clause was meant to empower Congress to keep interstate trade regular and free from state interference. The New Deal Court reinterpreted this as authority to regulate any activity with substantial aggregate effect on interstate commerce, to address national economic crisis and later civil rights violations.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the beneficiary set (e.g., originalist scholars, federalism scholars like Raoul Berger, Randy Barnett) document that the founding-era understanding was far narrower — 'commerce' meant trade/navigation, 'regulate' meant make regular, not prohibit. The New Deal Court itself (Jones & Laughlin Steel, Wickard) acknowledged it was expanding beyond original understanding to meet crisis conditions.
narrative_ontology:disappearance_verdict(commerce_clause_scope__broad_effects_test, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__broad_effects_test, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__broad_effects_test, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_scope__broad_effects_test, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__broad_effects_test, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__broad_effects_test_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__broad_effects_test_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.72: The doctrine transfers vast regulatory authority from states to federal government; the aggregation principle means virtually no economic activity is categorically beyond reach. Suppression 0.68: States and local actors cannot exit the federal regulatory regime; Lopez/Morrison/NFIB created marginal limits but the core aggregation doctrine stands. Theater 0.38: The 'substantial effects' test performs a limiting function but in practice operates as near-plenary power; the coordination story (national market, civil rights) is real but the extraction from state sovereignty is substantial. Accessibility_collapse 0.62: Alternatives (state regulation, interstate compacts, common law) exist but are practically foreclosed by federal preemption and dormant commerce clause. Resistance 0.55: Significant scholarly, judicial, and state-level resistance (Lopez, Morrison, NFIB, state nullification movements) but has not reversed the doctrine.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government and agencies are structural beneficiaries (d ~ 0.15-0.25): they collect regulatory authority and face minimal exit costs. Civil rights enforcement is a beneficiary with constrained exit (d ~ 0.3): gains enforcement tools but would lose them if doctrine narrowed. National interest groups are beneficiaries with mobile exit (d ~ 0.25): can shift to state lobbying. State governments are payers with constrained exit (d ~ 0.8): lose autonomy, high-cost resistance. Local businesses are payers with constrained exit (d ~ 0.75): compliance costs, no realistic exit. Excluded agents (state sovereignty advocates, originalists) have trapped/identity_locked exit (d ~ 0.9) — they bear the doctrinal exclusion but cannot change it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (national market unity, collective action) was live in 1787 and 1937. The current doctrine extends far beyond that problem — regulating non-commercial intrastate activity (Gonzales v. Raich: home-grown medical marijuana) via aggregation. The mandate has atrophied: the coordination function (preventing state barriers to trade) is achieved; the extraction function (federal regulatory state reaching local non-economic activity) persists. This is mandatrophy — the constraint's original justification is dead but the arrangement expands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregation_doctrine_naturalness,
    'Does the aggregation doctrine reflect genuine economic integration that makes state-level regulation impossible, or is it a judicial construction that expands federal power beyond the coordination need?',
    'Empirical study of whether state-level regulation of the aggregated activities actually produces the collective action problems the doctrine claims to solve; counterfactual analysis of regulatory outcomes in domains where Lopez/Morrison restored state authority.',
    'If the doctrine is a judicial construction, the high extractiveness is not justified by coordination necessity — classification shifts toward snare. If genuine integration exists, the coordination function is real and tangled_rope is sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_doctrine_naturalness, empirical, 'Whether aggregation reflects real economic necessity or judicial power expansion').

omega_variable(
    civil_rights_coordination_vs_extraction,
    'Is civil rights enforcement via Commerce Clause a genuine coordination function (solving state resistance to desegregation) or an extraction from state sovereignty that happened to serve a moral cause?',
    'Counterfactual: if civil rights had been enforced solely through 14th Amendment Section 5, would the Commerce Clause expansion have been narrower? Compare doctrinal trajectory with and without civil rights cases.',
    'If civil rights was the driver that locked in broad doctrine, the coordination function is historically contingent — the current extraction may be path-dependent rather than structurally necessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civil_rights_coordination_vs_extraction, conceptual, 'Whether civil rights enforcement is genuine coordination or contingent path-dependency').

omega_variable(
    lopez_morrison_doctrinal_significance,
    'Do Lopez (1995) and Morrison (2000) represent genuine doctrinal limits on the aggregation principle, or temporary retreats that left the broad effects test intact?',
    'Track subsequent lower court applications: if courts routinely distinguish Lopez/Morrison to uphold federal statutes, the limits are performative. If they genuinely constrain new legislation, the doctrine has structural boundaries.',
    'If limits are performative, suppression and extractiveness remain high and theater_ratio rises. If limits are real, the constraint may be transitioning toward intermediate_channels or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lopez_morrison_doctrinal_significance, empirical, 'Whether Lopez/Morrison created real limits or theatrical boundaries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__broad_effects_test, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cc_bet_tr_t1937, commerce_clause_scope__broad_effects_test, theater_ratio, 1937, 0.15).
narrative_ontology:measurement(cc_bet_tr_t1942, commerce_clause_scope__broad_effects_test, theater_ratio, 1942, 0.22).
narrative_ontology:measurement(cc_bet_tr_t1964, commerce_clause_scope__broad_effects_test, theater_ratio, 1964, 0.28).
narrative_ontology:measurement(cc_bet_tr_t1976, commerce_clause_scope__broad_effects_test, theater_ratio, 1976, 0.35).
narrative_ontology:measurement(cc_bet_tr_t1995, commerce_clause_scope__broad_effects_test, theater_ratio, 1995, 0.32).
narrative_ontology:measurement(cc_bet_tr_t2000, commerce_clause_scope__broad_effects_test, theater_ratio, 2000, 0.34).
narrative_ontology:measurement(cc_bet_tr_t2012, commerce_clause_scope__broad_effects_test, theater_ratio, 2012, 0.36).
narrative_ontology:measurement(cc_bet_tr_t2024, commerce_clause_scope__broad_effects_test, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(cc_bet_be_t1937, commerce_clause_scope__broad_effects_test, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement(cc_bet_be_t1942, commerce_clause_scope__broad_effects_test, base_extractiveness, 1942, 0.58).
narrative_ontology:measurement(cc_bet_be_t1964, commerce_clause_scope__broad_effects_test, base_extractiveness, 1964, 0.65).
narrative_ontology:measurement(cc_bet_be_t1976, commerce_clause_scope__broad_effects_test, base_extractiveness, 1976, 0.72).
narrative_ontology:measurement(cc_bet_be_t1995, commerce_clause_scope__broad_effects_test, base_extractiveness, 1995, 0.68).
narrative_ontology:measurement(cc_bet_be_t2000, commerce_clause_scope__broad_effects_test, base_extractiveness, 2000, 0.66).
narrative_ontology:measurement(cc_bet_be_t2012, commerce_clause_scope__broad_effects_test, base_extractiveness, 2012, 0.7).
narrative_ontology:measurement(cc_bet_be_t2024, commerce_clause_scope__broad_effects_test, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(cc_bet_su_t1937, commerce_clause_scope__broad_effects_test, suppression_requirement, 1937, 0.4).
narrative_ontology:measurement(cc_bet_su_t1942, commerce_clause_scope__broad_effects_test, suppression_requirement, 1942, 0.55).
narrative_ontology:measurement(cc_bet_su_t1964, commerce_clause_scope__broad_effects_test, suppression_requirement, 1964, 0.65).
narrative_ontology:measurement(cc_bet_su_t1976, commerce_clause_scope__broad_effects_test, suppression_requirement, 1976, 0.7).
narrative_ontology:measurement(cc_bet_su_t1995, commerce_clause_scope__broad_effects_test, suppression_requirement, 1995, 0.62).
narrative_ontology:measurement(cc_bet_su_t2000, commerce_clause_scope__broad_effects_test, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(cc_bet_su_t2012, commerce_clause_scope__broad_effects_test, suppression_requirement, 2012, 0.66).
narrative_ontology:measurement(cc_bet_su_t2024, commerce_clause_scope__broad_effects_test, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__broad_effects_test, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__intermediate_channels).

% DUAL FORMULATION NOTE:
% This constraint (broad_effects_test) is one of three readings of the commerce_clause_scope kernel. The narrow_originalist reading forecloses this one (mutually exclusive premises in a single framework). The intermediate_channels reading coexists_with this one (competing live doctrines). The ε values differ substantially: narrow_originalist has near-zero extractiveness (mountain); intermediate_channels has moderate extractiveness (rope/tangled_rope boundary); this reading has high extractiveness (tangled_rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_scope__broad_effects_test, institutional, 0.2).
constraint_indexing:directionality_override(commerce_clause_scope__broad_effects_test, organized, 0.75).
constraint_indexing:directionality_override(commerce_clause_scope__broad_effects_test, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
