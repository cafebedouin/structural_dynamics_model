% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__intermediate_channels
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__intermediate_channels, []).

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
 *   constraint_id: commerce_clause_scope__intermediate_channels
 *   human_readable: Commerce Clause — Intermediate Channels & Substantial Effects with Limiting Principles
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint story captures the intermediate_channels reading of the
 *   Commerce Clause — the current Supreme Court framework from Lopez (1995)
 *   through Morrison (2000), Raich (2005), and NFIB (2012). Federal power
 *   reaches three categories: channels of interstate commerce,
 *   instrumentalities/persons/things in interstate commerce, and activities
 *   substantially affecting interstate commerce. But limiting principles
 *   cabin the reach: non-economic activity requires a jurisdictional element
 *   (Lopez), aggregation applies only to economic activity (Morrison/Raich),
 *   and attenuated causal chains cannot support regulation (NFIB). The
 *   constraint is claimed as tangled_rope: genuine coordination of national
 *   economic regulation coexisting with asymmetric extraction of state
 *   regulatory authority in the economic sphere. The metrics describe a
 *   constraint that has grown more extractive and more theatrical over 30
 *   years as the economic/non-economic distinction proves manipulable and the
 *   substantial effects test swallows more local activity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, 0.62).
domain_priors:suppression_score(commerce_clause_scope__intermediate_channels, 0.48).
domain_priors:theater_ratio(commerce_clause_scope__intermediate_channels, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, extractiveness, 0.62).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__intermediate_channels, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__intermediate_channels, "Commerce Clause — Intermediate Channels & Substantial Effects with Limiting Principles").
narrative_ontology:topic_domain(commerce_clause_scope__intermediate_channels, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__intermediate_channels).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__intermediate_channels, '28d185f1-9d19-4356-91f2-d3de36712068').
narrative_ontology:cs_kernel_codification('28d185f1-9d19-4356-91f2-d3de36712068', formalized).
narrative_ontology:cs_authority_grounding('28d185f1-9d19-4356-91f2-d3de36712068', lineage).
narrative_ontology:cs_interpretation_layer_present('28d185f1-9d19-4356-91f2-d3de36712068').
narrative_ontology:cs_reading_relation('28d185f1-9d19-4356-91f2-d3de36712068', commerce_clause_scope__broad_effects_test, coexists_with).
narrative_ontology:cs_reading_relation('28d185f1-9d19-4356-91f2-d3de36712068', commerce_clause_scope__narrow_originalist, coexists_with).
narrative_ontology:cs_axiom('28d185f1-9d19-4356-91f2-d3de36712068', foundational, economic_activity_categorically_reachable).
narrative_ontology:cs_axiom_status(economic_activity_categorically_reachable, holdable).
narrative_ontology:cs_axiom_grounding('28d185f1-9d19-4356-91f2-d3de36712068', economic_activity_categorically_reachable, conventional).
narrative_ontology:cs_axiom('28d185f1-9d19-4356-91f2-d3de36712068', foundational, non_economic_activity_requires_jurisdictional_hook).
narrative_ontology:cs_axiom_status(non_economic_activity_requires_jurisdictional_hook, holdable).
narrative_ontology:cs_axiom_grounding('28d185f1-9d19-4356-91f2-d3de36712068', non_economic_activity_requires_jurisdictional_hook, conventional).
narrative_ontology:cs_axiom('28d185f1-9d19-4356-91f2-d3de36712068', foundational, aggregation_limited_to_economic_class).
narrative_ontology:cs_axiom_status(aggregation_limited_to_economic_class, holdable).
narrative_ontology:cs_axiom_grounding('28d185f1-9d19-4356-91f2-d3de36712068', aggregation_limited_to_economic_class, conventional).
narrative_ontology:cs_axiom('28d185f1-9d19-4356-91f2-d3de36712068', secondary, attenuated_causal_chains_insufficient).
narrative_ontology:cs_axiom_status(attenuated_causal_chains_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('28d185f1-9d19-4356-91f2-d3de36712068', attenuated_causal_chains_insufficient, instrumental).
narrative_ontology:cs_reference_frame('28d185f1-9d19-4356-91f2-d3de36712068', lopez_morrison_raich_framework).
narrative_ontology:cs_drift_state('28d185f1-9d19-4356-91f2-d3de36712068', post_dobbs_contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('28d185f1-9d19-4356-91f2-d3de36712068', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(commerce_clause_scope__intermediate_channels, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, federal_government).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, state_governments_in_non_economic_sphere).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, state_governments_in_economic_sphere).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, local_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, regulated_economic_entities).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, state_governments).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, regulated_economic_entities).
narrative_ontology:constraint_vindicates(commerce_clause_scope__intermediate_channels, federalism_balance_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__intermediate_channels, enumerated_powers_principle).
narrative_ontology:constraint_vindicates(commerce_clause_scope__intermediate_channels, state_police_powers_reserved).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the doctrinal framework through Congress and the Solicitor General. Gains regulatory authority over all economic activity substantially affecting interstate commerce. Uses the limiting principles to defend against overreach challenges while maintaining broad reach. Can choose litigation strategy and statutory design to maximize authority.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__intermediate_channels, federal_government, beneficiary).

% Lose regulatory authority over economic activity deemed to substantially affect interstate commerce (payer role). Retain exclusive authority over family law, criminal law, education, and other non-economic local matters (beneficiary role). Exit from federal preemption is constrained — can litigate, seek political safeguards, or experiment at margins but cannot opt out of Supremacy Clause.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, state_governments, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__intermediate_channels, state_governments, beneficiary).

% Subject to both state and federal preemption in economic regulation. Have no independent constitutional status — their powers derive from states. Bear compliance costs from layered federal-state regulation. Exit options limited to seeking state protection or federal lobbying; no direct constitutional voice.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, local_governments, payer,
    moderate, biographical, constrained, regional).

% Gain uniform national rules reducing compliance complexity across states (beneficiary). Bear federal compliance costs and lose ability to forum-shop for favorable state regulation (payer). Exit options include regulatory arbitrage, lobbying, litigation, or restructuring operations — more mobile than governments but constrained by market forces.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, regulated_economic_entities, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__intermediate_channels, regulated_economic_entities, payer).

% Adjudicate the boundaries through case-by-case litigation. Apply the three-category framework (channels, instrumentalities, substantial effects) and the limiting principles (jurisdictional element for non-economic, economic-only aggregation, no attenuated chains). Their decisions ratify or adjust the constraint's operation. No direct stake in the federalism balance beyond institutional legitimacy.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Advocate for the narrow_originalist reading — commerce as trade crossing state lines, regulate as make-regular. Would object to the substantial effects test and broad economic definition. Excluded from controlling the doctrine but influence through dissents, concurrences, and academic discourse. Identity-locked because professional identity and judicial philosophy are fused with this position.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, originalist_scholars_and_judges, excluded,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a workable division of regulatory authority for a modern integrated economy: federal power over genuinely national economic activity (channels, instrumentalities, substantial effects) while preserving state autonomy over traditionally local non-economic matters (family, crime, education). Solves the collective-action problem of state-by-state regulation fragmenting national markets.
% TRANSFER_FUNCTION: Moves regulatory authority over economic activity from states to federal government; moves compliance costs to regulated entities who gain uniform national standards; moves legal certainty to all parties through doctrinal categories. The limiting principles (jurisdictional element, economic-only aggregation, no attenuated chains) act as valves modulating the transfer.
% ABSENT_VOICES: States seeking broader regulatory authority over economic activity (e.g., progressive states wanting stronger labor/environmental rules than federal floor). Local governments fully preempted in economic zones with no compensatory authority. Originalist scholars and judges who reject the substantial effects test entirely — their preferred reading (narrow_originalist) is excluded from controlling doctrine but persists in dissent.
% DISAPPEARANCE_RATIONALE: If the intermediate_channels doctrine vanished overnight, federal power would either expand to the broad_effects_test (near-plenary economic authority) or contract to the narrow_originalist reading (trade-crossing-state-lines only). Either shift would radically restructure federal-state relations, the regulatory state, and the national economy. The world does not stay the same.
% FOUNDING_PROBLEM: The Articles of Confederation failed because states could balkanize the national economy. The Commerce Clause was meant to create a free-trade zone among states. By the New Deal, the economy was nationally integrated — agriculture, manufacturing, labor, finance all crossed state lines. The founding problem of the *current* doctrine (Lopez/Morrison/Raich) is: how to permit federal regulation of modern national economic activity without eliminating the constitutional distinction between national and local authority?
% FOUNDING_PROBLEM_CORROBORATION: The Lopez/Morrison majority (Rehnquist, O'Connor, Scalia, Kennedy, Thomas) attested the problem is live — limiting principles necessary to preserve state sovereignty. The Raich majority (Stevens, Kennedy, Souter, Ginsburg, Breyer) attested the problem is substantially solved — aggregation and substantial effects suffice for national markets. NFIB v. Sebelius (Roberts, Ginsburg, Breyer, Sotomayor, Kagan on Commerce Clause) attested the problem persists but the mandate exceeded even intermediate channels. Academic federalism scholars (e.g., Erwin Chemerinsky, Randy Barnett, Heather Gerken, Gillian Metzger) provide outside-the-beneficiary corroboration across the spectrum.
narrative_ontology:disappearance_verdict(commerce_clause_scope__intermediate_channels, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__intermediate_channels, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__intermediate_channels, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_scope__intermediate_channels, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__intermediate_channels, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__intermediate_channels_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__intermediate_channels_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects that federal authority now covers virtually all economic activity — the 'substantially affects' test operates as a near-plenary grant within the economic sphere. Suppression (0.48) is moderate: states retain genuine authority over non-economic local matters, but cannot regulate economic activity that Congress occupies. Theater ratio (0.32) has risen from 0.15 (Lopez era) as the limiting principles become more performative — the jurisdictional element requirement is easily satisfied, the economic/non-economic line is gerrymandered, and 'attenuated chains' is a post-hoc limitation. Accessibility collapse (0.52) is moderate: alternatives exist (state non-economic regulation, federal economic regulation) but the middle ground is squeezed. Resistance (0.54) is sustained: states litigate, scholars contest, but the doctrinal framework holds.
 *
 * PERSPECTIVAL GAP:
 *   The federal seat experiences this as coordination (rope-like): a workable framework for national economic governance. State seats in the economic sphere experience extraction (snare-like): their regulatory authority is displaced by a test they cannot satisfy. State seats in the non-economic sphere experience coordination (rope-like): their reserved domain is genuinely protected. The engine computes this divergence from the dual-role stakeholder structure and the beneficiary/victim declarations — the same constraint is different types from different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government is agenda_setter/beneficiary (d ~0.15) — it designs the framework and collects regulatory authority. State governments are dual-role: payer in economic sphere (d ~0.7), beneficiary in non-economic sphere (d ~0.3). Local governments are pure payers (d ~0.8) — no constitutional voice, layered preemption. Regulated entities are dual-role: beneficiaries of uniformity (d ~0.4) but payers of compliance costs (d ~0.6). Courts are analytical observers (d = 0.5). Originalist scholars are excluded and identity-locked — their exit would require abandoning professional identity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (national economic integration vs. state local autonomy) remains contested — not dead, not settled. The intermediate_channels doctrine was built to solve this, but the economic/non-economic distinction has become a manipulable boundary rather than a stable limit. The constraint persists not because the founding problem is solved, but because no coalition can agree on a replacement. This is mandatrophy: the arrangement's mandate (balance federal economic power and state local autonomy) has outlived its functional coherence, but the constraint remains because the cost of replacement exceeds any single actor's benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_non_economic_boundary_stability,
    'Is the economic/non-economic distinction a stable constitutional boundary or a manipulable line that Congress and courts can gerrymander to expand or contract federal power?',
    'Empirical analysis of post-Lopez cases: track how often the distinction does genuine limiting work vs. how often it is sidestepped by recharacterization (e.g., Raich treating homegrown marijuana as economic; NFIB refusing to treat inactivity as economic). Measure congressional drafting behavior — do statutes include jurisdictional elements as pro forma compliance?',
    'If the boundary is stable, the constraint is genuine coordination (rope/tangled_rope with real limiting principles). If manipulable, the limiting principles are theater and the constraint drifts toward snare (extraction without genuine coordination check).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_non_economic_boundary_stability, empirical, 'Whether the core limiting principle has structural integrity or is a performative gate.').

omega_variable(
    substantial_effects_test_genuine_limit,
    'Does the ''substantially affects'' test have independent limiting force, or does it collapse into a rational-basis review for any economic regulation?',
    'Case-level coding: identify every post-Raich Commerce Clause challenge. Code whether the Court struck down the regulation, and if so, on what grounds (attenuated chain? non-economic? jurisdictional element missing?). Compare strike rate to rational-basis review in other contexts.',
    'If the test has independent force, the constraint maintains tangled_rope character (coordination + extraction with real check). If it collapses to rational basis, the coordination function is illusory and the constraint is snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substantial_effects_test_genuine_limit, empirical, 'Whether the substantial effects category is a genuine category or a residual plenary grant.').

omega_variable(
    kernel_framing_ambiguity,
    'Does the intermediate_channels reading represent a stable doctrinal synthesis, or is it an unstable equilibrium between the broad_effects_test and narrow_originalist readings that will collapse into one or the other?',
    'Track Supreme Court personnel changes and opinion alignment. Code whether intermediate_channels opinions (Roberts, Kavanaugh, Barrett, Gorsuch partial) hold together as a coherent middle, or whether justices migrate toward broad or narrow poles. Monitor lower court applications — do they treat limiting principles as binding or advisory?',
    'If unstable equilibrium, the constraint''s current classification is temporally contingent — the engine''s snapshot captures a transition state. This affects whether temporal drift measurements (rising theater, rising extraction) indicate degradation or cycles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether this reading is a stable doctrinal position or a transitional compromise.').

omega_variable(
    state_autonomy_real_vs_symbolic,
    'Is the state autonomy preserved in non-economic spheres (family law, criminal law, education) genuine regulatory freedom, or is it a symbolic reservation that shrinks as the definition of ''economic'' expands?',
    'Inventory federal statutes encroaching on traditional state domains since Lopez. Code whether they use Commerce Clause authority with jurisdictional elements, or other powers (Spending, Taxing, Necessary and Proper). Measure state legislative output in reserved domains — is it constrained by federal shadow regulation?',
    'If state autonomy is genuinely preserved, the constraint''s coordination function is real (tangled_rope). If it shrinks with economic definition expansion, the constraint is a ratchet (snare drift).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_autonomy_real_vs_symbolic, empirical, 'Whether the beneficiary side of the dual-role (state governments) receives real value or symbolic concession.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__intermediate_channels, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccs_intermediate_tr_t0, commerce_clause_scope__intermediate_channels, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ccs_intermediate_tr_t5, commerce_clause_scope__intermediate_channels, theater_ratio, 5, 0.2).
narrative_ontology:measurement(ccs_intermediate_tr_t10, commerce_clause_scope__intermediate_channels, theater_ratio, 10, 0.25).
narrative_ontology:measurement(ccs_intermediate_tr_t17, commerce_clause_scope__intermediate_channels, theater_ratio, 17, 0.28).
narrative_ontology:measurement(ccs_intermediate_tr_t25, commerce_clause_scope__intermediate_channels, theater_ratio, 25, 0.3).
narrative_ontology:measurement(ccs_intermediate_tr_t30, commerce_clause_scope__intermediate_channels, theater_ratio, 30, 0.32).

% Extraction over time
narrative_ontology:measurement(ccs_intermediate_be_t0, commerce_clause_scope__intermediate_channels, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ccs_intermediate_be_t5, commerce_clause_scope__intermediate_channels, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(ccs_intermediate_be_t10, commerce_clause_scope__intermediate_channels, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(ccs_intermediate_be_t17, commerce_clause_scope__intermediate_channels, base_extractiveness, 17, 0.6).
narrative_ontology:measurement(ccs_intermediate_be_t25, commerce_clause_scope__intermediate_channels, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(ccs_intermediate_be_t30, commerce_clause_scope__intermediate_channels, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ccs_intermediate_su_t0, commerce_clause_scope__intermediate_channels, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ccs_intermediate_su_t5, commerce_clause_scope__intermediate_channels, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(ccs_intermediate_su_t10, commerce_clause_scope__intermediate_channels, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(ccs_intermediate_su_t17, commerce_clause_scope__intermediate_channels, suppression_requirement, 17, 0.48).
narrative_ontology:measurement(ccs_intermediate_su_t25, commerce_clause_scope__intermediate_channels, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(ccs_intermediate_su_t30, commerce_clause_scope__intermediate_channels, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__intermediate_channels, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__intermediate_channels, 0.12).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, federal_preemption_doctrine).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, state_police_powers_reservation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the commerce_clause_scope kernel. The broad_effects_test reading treats aggregation as sufficient for virtually all economic regulation (higher extractiveness, lower theater). The narrow_originalist reading restricts federal power to removing state trade barriers (lower extractiveness, higher suppression of federal authority). This intermediate_channels reading occupies the contested middle — the current doctrinal settlement. The three stories form a constraint family linked by affects_constraints; their epsilon values differ because they instantiate structurally distinct constraints from the same constitutional text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_scope__intermediate_channels, organized, 0.35).
constraint_indexing:directionality_override(commerce_clause_scope__intermediate_channels, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
