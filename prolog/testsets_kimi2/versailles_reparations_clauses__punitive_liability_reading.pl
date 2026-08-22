% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__punitive_liability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__punitive_liability_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: versailles_reparations_clauses__punitive_liability_reading
 *   human_readable: Versailles Reparations Clauses â Punitive Liability Reading
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   The Treaty of Versailles (1919) imposed on Germany the obligation to make
 *   reparations for the total costs of the war, grounded in Article 231's
 *   'war guilt' clause. The punitive liability reading treats this clause as
 *   establishing Germany's unique moral and financial responsibility for all
 *   war damages, authorizing quasi-unlimited claims administered by the
 *   Allied Reparations Commission. This reading was championed by France and
 *   the United Kingdom to fund reconstruction and service inter-Allied war
 *   debts. German governments, workers, and industrialists bore the
 *   extraction through taxes, inflation, and direct resource transfers. The
 *   constraint operated as a legally formalized transfer mechanism that
 *   coordinated creditor-state claims while asymmetrically extracting from
 *   the defeated party. This story instantiates the punitive liability
 *   reading of the versailles_reparations_clauses kernel; sibling readings
 *   (limited responsibility, repudiation) are modeled as separate
 *   constraints.
 *
 * KEY AGENTS:
 *   - allied_reparations_commission: Agenda-setter (institutional/generational) â sets claims, enforces schedules, interprets Article 231 expansively
 *   - allied_creditor_states: Primary beneficiary (powerful/generational) â receives reparations flows and war-debt relief
 *   - german_reich: Payer/executor (institutional/biographical) â compelled to levy taxes and transfer resources under foreign oversight
 *   - german_workers_taxpayers: Primary target (powerless/biographical) â bears extraction through wage suppression, taxation, and currency collapse
 *   - german_industrialists: Secondary target (powerful/biographical) â subjected to direct coal and output seizures, Ruhr occupation
 *   - neutral_economic_observers: Analytical seat â evaluates capacity and documents extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, 0.72).
domain_priors:suppression_score(versailles_reparations_clauses__punitive_liability_reading, 0.65).
domain_priors:theater_ratio(versailles_reparations_clauses__punitive_liability_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__punitive_liability_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__punitive_liability_reading, "Versailles Reparations Clauses â Punitive Liability Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__punitive_liability_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__punitive_liability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__punitive_liability_reading, '170bc331-8a84-4251-95fa-6a4451e3d1e5').
narrative_ontology:cs_kernel_codification('170bc331-8a84-4251-95fa-6a4451e3d1e5', formalized).
narrative_ontology:cs_authority_grounding('170bc331-8a84-4251-95fa-6a4451e3d1e5', lineage).
narrative_ontology:cs_interpretation_layer_present('170bc331-8a84-4251-95fa-6a4451e3d1e5').
narrative_ontology:cs_reading_relation('170bc331-8a84-4251-95fa-6a4451e3d1e5', versailles_reparations_clauses__limited_responsibility_reading, forecloses).
narrative_ontology:cs_reading_relation('170bc331-8a84-4251-95fa-6a4451e3d1e5', versailles_reparations_clauses__repudiation_reading, forecloses).
narrative_ontology:cs_axiom('170bc331-8a84-4251-95fa-6a4451e3d1e5', foundational, unlimited_moral_liability_for_war_costs).
narrative_ontology:cs_axiom_status(unlimited_moral_liability_for_war_costs, holdable).
narrative_ontology:cs_axiom_grounding('170bc331-8a84-4251-95fa-6a4451e3d1e5', unlimited_moral_liability_for_war_costs, deontological).
narrative_ontology:cs_axiom('170bc331-8a84-4251-95fa-6a4451e3d1e5', foundational, article_231_total_claims_authority).
narrative_ontology:cs_axiom_status(article_231_total_claims_authority, holdable).
narrative_ontology:cs_axiom_grounding('170bc331-8a84-4251-95fa-6a4451e3d1e5', article_231_total_claims_authority, conventional).
narrative_ontology:cs_reference_frame('170bc331-8a84-4251-95fa-6a4451e3d1e5', punitive_peace_framework).
narrative_ontology:cs_drift_state('170bc331-8a84-4251-95fa-6a4451e3d1e5', late_weimar_crisis, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('170bc331-8a84-4251-95fa-6a4451e3d1e5', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_reich).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_workers_taxpayers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_industrialists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and interprets Article 231 to set reparation schedules, oversees German fiscal machinery, and enforces compliance through sanctions and occupation threats. It translates the treaty text into concrete extraction demands and adjusts modalities while preserving the principle of unlimited liability.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, allied_reparations_commission, agenda_setter,
    institutional, generational, mobile, continental).

% Receive reparations flows used to fund reconstruction and service inter-Allied war debts to the United States. They have mobile exit options because they can reschedule claims, securitize them, or shift to other revenue sources, but they benefit directly from maintaining the punitive liability framework.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states, beneficiary,
    powerful, generational, mobile, national).

% Compelled to levy internal taxes, float foreign loans, and transfer gold and commodities to the Reparations Commission. Its fiscal sovereignty is subordinated to external oversight; default triggers occupation or loss of industrial territory.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_reich, payer,
    institutional, biographical, constrained, national).

% Bear the extraction through wage suppression, heavy taxation, and hyperinflation eroding savings. They are trapped within the German fiscal jurisdiction by citizenship, poverty, and post-war border restrictions that prevent mass emigration.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_workers_taxpayers, payer,
    powerless, biographical, trapped, national).

% Subject to direct seizures of coal and industrial output, especially during the Ruhr occupation. Despite domestic economic power, their physical assets are location-bound and targeted by enforcement, constraining exit to partial capital flight.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_industrialists, payer,
    powerful, biographical, constrained, national).

% Assess the macroeconomic sustainability of transfer payments and document the gap between claimed liability and German capacity. Their analyses inform diplomatic negotiations but do not control enforcement.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, neutral_economic_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__punitive_liability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the distribution of war costs among Allied powers by establishing a centralized legal mechanism to extract reconstruction resources from the defeated enemy, replacing ad hoc seizures with a formalized claims process.
% TRANSFER_FUNCTION: Moves wealthâgold, coal, industrial output, foreign exchange, and tax revenueâfrom German workers, taxpayers, and industrial enterprises to Allied creditor states via the German government's fiscal apparatus and international commission oversight.
% ABSENT_VOICES: German civil society, labor unions, and democratic socialist parties were excluded from the treaty negotiations at Versailles; anti-reparations economists who argued for capacity-based limits were heard but overruled by creditor-state delegations.
% DISAPPEARANCE_RATIONALE: The interwar European economic order, French reconstruction financing, and the Allied war-debt chain to the United States all depended on the reparations flow. Absent the clauses, the entire post-war settlement would have required immediate renegotiation and alternative funding sources.
% FOUNDING_PROBLEM: How to finance reconstruction of devastated Allied territories and settle inter-Allied war debts without bankrupting the victorious states' own treasuries.
% FOUNDING_PROBLEM_CORROBORATION: Allied government delegations attested to the need for reconstruction finance. Independent observers, notably John Maynard Keynes in The Economic Consequences of the Peace, corroborated the scale of devastation but argued from outside the beneficiary camp that the punitive solution exceeded the founding problem and destroyed the capacity to solve it.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__punitive_liability_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__punitive_liability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__punitive_liability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(versailles_reparations_clauses__punitive_liability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__punitive_liability_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end, higher at peak) because the punitive reading authorized claims deliberately decoupled from Germany's contemporaneous payment capacity, subordinating German fiscal sovereignty to external creditors. Suppression is substantial (0.65) because enforcement relied on ultimata, foreign customs receivership, and military occupation of the Ruhr to block default and alternative settlement. Theater ratio is moderate-high (0.60) because by the early 1930s a growing share of diplomatic activity sustained the legal fiction of unlimited liability while actual payments were rescheduled and commercially rationalized. Accessibility collapse (0.65) reflects that unilateral default or negotiation of a clean slate were structurally blocked by the threat of occupation and treaty sanctions. Resistance (0.75) is high due to German passive resistance in the Ruhr, hyperinflation as implicit default, and continuous diplomatic efforts to reduce the schedule.
 *
 * PERSPECTIVAL GAP:
 *   From the creditor-state seat, the constraint appears as necessary coordination to solve post-war reconstruction financing and prevent free-riding among victorious powers. From the German payer seats, the same structure reads as coercive extraction dressed in legal formality. The engine resolves this divergence through the structural data: the presence of both beneficiaries and victims, active enforcement, and the decoupling of claims from capacity. The perspectival gap is not an error to reconcile but the core phenomenon the classification measures.
 *
 * DIRECTIONALITY LOGIC:
 *   The Allied creditor states and the Reparations Commission sit at the beneficiary end of the directionality axis: they collect the transfer, set the rules, and have mobile exit options (can reschedule, securitize, or substitute with other revenue). German workers and taxpayers sit at the full-target end: they are trapped within the German fiscal jurisdiction, bearing costs they cannot evade. The German Reich sits near the target end but is institutional rather than powerless; its directionality is high because it is constrained to execute the extraction against its own population. German industrialists, though powerful in domestic terms, are constrained by asset specificity and territorial jurisdiction, placing them in the mid-high target range. The divergence between creditor-state seats and German seats is the structural axis the engine computes.
 *
 * MANDATROPHY ANALYSIS:
 *   The punitive liability reading resists classification as pure coordination (Rope) because it declares identifiable victims (German workers, taxpayers, industrialists) and requires active enforcement against their resistance. It resists classification as pure extraction (Snare) because it did solve a genuine coordination problem among Allied creditors (who gets how much, in what order). The Tangled Rope classification captures the hybrid: a real coordination function (inter-creditor allocation) fused with asymmetric extraction (Germany pays regardless of capacity). Mandatrophy is declared via founding_problem_status: dead â the reconstruction problem that justified the mechanism was largely solved by the late 1920s, yet the constraint persisted, producing the rising theater_ratio that signals drift toward performative maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_231_unlimited_scope,
    'Does Article 231''s text structurally admit the quasi-unlimited reparations claims of the punitive reading, or does the treaty language inherently limit liability to restorable damages?',
    'Textual and diplomatic historiography of the drafting of Article 231, comparing English and French versions.',
    'If the text is inherently limited, the punitive reading is a constructed extraction superstructure; if open-ended, the kernel itself enables the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_231_unlimited_scope, conceptual, 'Whether the treaty text supports unlimited liability or caps it.').

omega_variable(
    german_capacity_payment_gap,
    'Did the reparations schedules demanded under the punitive reading systematically exceed Germany''s actual current-account and fiscal capacity to transfer?',
    'Comparative economic historiography comparing scheduled annuities to contemporaneous German trade balances and budget surpluses.',
    'A persistent gap would confirm the constraint as extraction decoupled from coordination; alignment would support the bounded-settlement framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(german_capacity_payment_gap, empirical, 'Whether claims were economically sustainable or extractive overreach.').

omega_variable(
    sibling_reading_divergence,
    'How would the structural classification change if the limited_responsibility_reading or repudiation_reading were adopted instead of this punitive reading?',
    'Generate sibling constraint stories and compare epsilon, beneficiary/victim sets, and directionality maps.',
    'The punitive reading''s high epsilon and pronounced victim set may collapse or invert under alternative readings, revealing the kernel''s contested nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_divergence, conceptual, 'Structural delta across sibling readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__punitive_liability_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t0, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(vers_tr_t2, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 2, 0.4).
narrative_ontology:measurement(vers_tr_t4, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 4, 0.48).
narrative_ontology:measurement(vers_tr_t6, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 6, 0.52).
narrative_ontology:measurement(vers_tr_t8, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 8, 0.56).
narrative_ontology:measurement(vers_tr_t10, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(vers_tr_t12, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 12, 0.6).

% Extraction over time
narrative_ontology:measurement(vers_be_t0, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 0, 0.82).
narrative_ontology:measurement(vers_be_t2, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 2, 0.86).
narrative_ontology:measurement(vers_be_t4, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 4, 0.88).
narrative_ontology:measurement(vers_be_t6, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 6, 0.82).
narrative_ontology:measurement(vers_be_t8, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 8, 0.78).
narrative_ontology:measurement(vers_be_t10, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(vers_be_t12, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 12, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t0, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(vers_su_t2, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 2, 0.7).
narrative_ontology:measurement(vers_su_t4, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 4, 0.85).
narrative_ontology:measurement(vers_su_t6, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 6, 0.75).
narrative_ontology:measurement(vers_su_t8, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(vers_su_t10, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(vers_su_t12, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 12, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__punitive_liability_reading, resource_allocation).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, repudiation_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Versailles reparations clauses' conflates three structurally distinct readings: punitive liability (high extraction, unlimited moral liability), limited responsibility (capacity-bounded extraction), and repudiation (zero legitimate extraction). Each reading has a different epsilon, beneficiary/victim structure, and directionality map. They are modeled as separate constraints in a family linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
