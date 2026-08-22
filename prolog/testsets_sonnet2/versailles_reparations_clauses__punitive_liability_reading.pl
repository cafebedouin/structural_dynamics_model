% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__punitive_liability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: versailles_reparations_clauses__punitive_liability_reading
 *   human_readable: Versailles War-Guilt Reparations Regime (Punitive Liability Reading)
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   This story instantiates the punitive liability reading of the Versailles
 *   reparations kernel: Article 231 is read as establishing Germany's unique
 *   moral and causal responsibility for the war, grounding a quasi-unlimited
 *   claim scaled to Allied reconstruction needs and war costs rather than to
 *   German fiscal capacity. Under this reading the treaty is a tangled rope —
 *   it genuinely coordinates Allied reconstruction financing (a real
 *   collective-action problem among war-damaged creditor states) while
 *   simultaneously running an asymmetric extraction from German taxpayers and
 *   workers who had no voice in the liability finding and no legal forum to
 *   contest it. The 1923 Ruhr occupation crisis marks the enforcement peak;
 *   the Dawes (1924) and Young (1929) Plans represent partial de-escalation
 *   without touching the underlying liability doctrine.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, 0.81).
domain_priors:suppression_score(versailles_reparations_clauses__punitive_liability_reading, 0.72).
domain_priors:theater_ratio(versailles_reparations_clauses__punitive_liability_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__punitive_liability_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__punitive_liability_reading, "Versailles War-Guilt Reparations Regime (Punitive Liability Reading)").
narrative_ontology:topic_domain(versailles_reparations_clauses__punitive_liability_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__punitive_liability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__punitive_liability_reading, '9cb8401d-1683-4de8-ba44-3421373d65ec').
narrative_ontology:cs_kernel_codification('9cb8401d-1683-4de8-ba44-3421373d65ec', fixed_text).
narrative_ontology:cs_authority_grounding('9cb8401d-1683-4de8-ba44-3421373d65ec', extraction).
narrative_ontology:cs_interpretation_layer_present('9cb8401d-1683-4de8-ba44-3421373d65ec').
narrative_ontology:cs_reading_relation('9cb8401d-1683-4de8-ba44-3421373d65ec', versailles_reparations_clauses__limited_responsibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('9cb8401d-1683-4de8-ba44-3421373d65ec', versailles_reparations_clauses__repudiation_reading, coexists_with).
narrative_ontology:cs_axiom('9cb8401d-1683-4de8-ba44-3421373d65ec', foundational, germany_bears_unique_causal_moral_responsibility).
narrative_ontology:cs_axiom_status(germany_bears_unique_causal_moral_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('9cb8401d-1683-4de8-ba44-3421373d65ec', germany_bears_unique_causal_moral_responsibility, conventional).
narrative_ontology:cs_axiom('9cb8401d-1683-4de8-ba44-3421373d65ec', secondary, liability_scaled_to_total_war_cost_not_capacity).
narrative_ontology:cs_axiom_status(liability_scaled_to_total_war_cost_not_capacity, holdable).
narrative_ontology:cs_axiom_grounding('9cb8401d-1683-4de8-ba44-3421373d65ec', liability_scaled_to_total_war_cost_not_capacity, instrumental).
narrative_ontology:cs_reference_frame('9cb8401d-1683-4de8-ba44-3421373d65ec', treaty_as_binding_moral_and_legal_judgment).
narrative_ontology:cs_drift_state('9cb8401d-1683-4de8-ba44-3421373d65ec', post_dawes_young_plan_recalibration, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9cb8401d-1683-4de8-ba44-3421373d65ec', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, french_reconstruction_authority).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, reparations_commission_administrators).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_taxpayers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_industrial_workers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, weimar_fiscal_administration).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__punitive_liability_reading, war_guilt_doctrine).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__punitive_liability_reading, unconditional_belligerent_liability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% France, Britain, Belgium and others hold reparations claims fixed by the treaty's war-guilt clause and enforce collection through the Reparations Commission, occupation threats (e.g. the Ruhr), and control of German customs revenue. They set the schedule and can revise or enforce it unilaterally; their own war debts to the United States create pressure to maximize German transfers.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states, agenda_setter).

% The inter-Allied commission fixes German payment obligations, audits compliance, and can authorize sanctions (occupation, customs seizure) for default. It administers Article 231's liability finding as settled fact and treats renegotiation requests as compliance failures rather than legitimate grievance.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, reparations_commission_administrators, agenda_setter,
    institutional, generational, arbitrage, continental).

% Bear reparations obligations through taxation, currency depreciation, and the fiscal consequences of transfer payments (including the hyperinflation of 1921-1923 tied partly to reparations-driven monetary policy). They have no seat at the commission and no legal mechanism to contest the war-guilt finding underlying the obligation; emigration is the only individual exit, and it does not remove the obligation from those who stay.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_taxpayers, payer,
    powerless, biographical, trapped, national).

% Labor in reparations-in-kind industries (coal, timber, manufactured goods shipped directly to Allied states) and absorb wage suppression and unemployment when occupation (e.g. the Ruhr in 1923) disrupts production. Their labor output is directly requisitioned as part of the transfer mechanism; they cannot bargain around the treaty obligation.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_industrial_workers, payer,
    powerless, biographical, trapped, national).

% The German state must find revenue to meet scheduled payments while maintaining domestic political legitimacy. It can negotiate schedules (Dawes Plan, Young Plan) but cannot exit the underlying liability finding without Allied consent; default risks renewed occupation. It administers domestic collection but did not set the obligation.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, weimar_fiscal_administration, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__punitive_liability_reading, weimar_fiscal_administration, agenda_setter).

% Argue the war-guilt clause is a moral fiction imposed under duress and that the entire liability finding should be void. Their objection is treated by the Commission as bad-faith revisionism rather than a legitimate legal challenge; they have no forum within the treaty's own institutions to contest Article 231 itself, only its payment schedules.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_nationalist_political_movements, excluded,
    organized, generational, constrained, national).

% Assess whether Article 231's liability language was intended and functioned as a moral war-guilt judgment or as a legal predicate for capping claims. They do not participate in enforcement but their readings shape how the constraint is remembered and cited in later treaty design (e.g. post-WWII reparations frameworks explicitly avoiding war-guilt language).
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, international_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__punitive_liability_reading, allied_creditor_states).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__punitive_liability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, enforceable mechanism for allocating the material costs of the war among the belligerents, avoiding a chaotic multiplicity of bilateral claims and creating a schedule Allied treasuries can plan around.
% TRANSFER_FUNCTION: Moves wealth (currency, industrial goods, coal, intellectual property, colonial territory) from the German state and its taxed population to Allied creditor states, justified by attributing sole causal and moral responsibility for the war to Germany under Article 231.
% ABSENT_VOICES: German nationalist and mainstream political factions who argued the war-guilt attribution was itself contestable had no forum within the treaty's own institutions to challenge Article 231; they could contest payment schedules but not the underlying liability finding. Independent international arbitration on the guilt question itself was never offered.
% DISAPPEARANCE_RATIONALE: If the punitive liability reading were repudiated overnight, Germany's fiscal obligations would collapse to whatever residual claims survived under a limited-responsibility or repudiation framework; Allied reconstruction budgets built on anticipated transfers would face immediate shortfalls, and the domestic German political narrative used to organize resistance and later revisionist politics would lose its central grievance.
% FOUNDING_PROBLEM: The Allied powers needed to finance war-damaged reconstruction (chiefly in France and Belgium) and needed a legal and moral basis to compel transfers from the defeated power rather than absorbing the costs domestically or through new borrowing.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary British economists (notably Keynes, writing from outside the French and Belgian beneficiary bloc) attested the reconstruction-financing problem was real but argued the punitive liability framing vastly overstated what German capacity could sustainably transfer, corroborating that the founding problem was genuine while the liability reading's scale was not calibrated to it. No corroboration for the specific 'unique moral responsibility' framing has been offered by any party outside the Allied creditor bloc itself.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__punitive_liability_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__punitive_liability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__punitive_liability_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(versailles_reparations_clauses__punitive_liability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__punitive_liability_reading, 0.81, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises sharply around the 1923 Ruhr occupation (0.85) when France and Belgium physically seize the Ruhr industrial region over a payment default, then partially recedes under the Dawes Plan's rescheduling before rising again toward 1932 as depression-era German fiscal capacity collapses against a fixed nominal schedule. Suppression tracks the same arc — highest during active occupation enforcement, lower during negotiated rescheduling periods, because suppression here is the raw enforcement apparatus (occupation, customs control, commission oversight) rather than a scope- or power-scaled quantity. Theater ratio rises modestly over the interval as commission proceedings increasingly perform technical fiscal analysis (capacity studies feeding the Dawes and Young Plans) while the underlying liability finding remains untouched — a growing gap between the appearance of technocratic recalibration and the fixed moral-liability premise beneath it.
 *
 * DIRECTIONALITY LOGIC:
 *   Allied creditor states and the Reparations Commission sit at the beneficiary/agenda-setter end: they collect transfers and administer the schedule under a liability finding they authored and can revise but never abandon. German taxpayers and industrial workers sit at the full-target end: trapped exit (national scope, no individual emigration removes the collective obligation), powerless in the relevant institutional sense (no seat on the Commission), and their labor and tax base are the transfer's direct object. The Weimar fiscal administration occupies an intermediate structural position — it administers collection domestically (a thin agenda-setter role) while itself bearing payer status vis-à-vis the Allied schedule, which is why it carries a secondary_role rather than a pure payer designation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — financing reconstruction in war-damaged France and Belgium — was real and, under this reading, remained partially live through the 1920s as reconstruction continued. But the SCALE and MORAL FRAMING of the liability (unique total-war-cost responsibility under Article 231) outran what the founding problem required, which is why this reading computes as tangled_rope rather than pure snare: there is a genuine coordination function (Allied reconstruction financing) riding alongside an extraction whose magnitude was set by moral attribution rather than calibrated need. Contemporary corroboration (Keynes, writing from outside the Allied creditor bloc) supports treating the founding problem as partly live but the liability reading's scale as uncalibrated — exactly the asymmetry that prevents both mislabeling this as pure coordination (which the punitive scale contradicts) and as pure extraction with no coordination function at all (which the real reconstruction-financing problem contradicts).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_versus_legal_reading_of_article_231,
    'Was Article 231 drafted and intended as a moral war-guilt judgment justifying unlimited liability, or as a narrow legal predicate for the Allied powers'' compensation claims under the laws of war, later inflated in political rhetoric beyond its drafters'' intent?',
    'Close textual and drafting-history analysis of the Versailles negotiations (comparing US, British, and French delegation records) against subsequent Commission practice and German legal challenges.',
    'If the narrow legal reading is correct, the punitive_liability_reading modeled here overstates the kernel''s own textual commitments, and the limited_responsibility_reading would be the more textually grounded sibling; if the moral reading is correct, this story''s high ε and tangled_rope classification are the textually accurate account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_versus_legal_reading_of_article_231, conceptual, 'Whether Article 231 was intended as moral judgment or narrow legal predicate — the central interpretive fork between this reading and the limited_responsibility sibling.').

omega_variable(
    duress_and_treaty_validity,
    'Does the treaty''s negotiation under military and economic coercion (the Allied blockade continued into 1919, no German counter-negotiation permitted) affect the legitimacy of the liability finding under contemporary or period international law?',
    'Comparative analysis against period doctrines of treaty validity under duress, and against how the international legal community (League of Nations jurists, subsequent scholarship) treated the question.',
    'If duress is found to void or substantially weaken the obligation''s legitimacy, this reading''s premise of a genuinely binding unique liability is undermined in favor of the repudiation_reading; if duress does not affect validity under the applicable legal framework, this reading''s binding-liability premise stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duress_and_treaty_validity, conceptual, 'Whether coercive negotiating conditions void the liability finding this reading depends on — the fork against the repudiation sibling.').

omega_variable(
    reparations_capacity_versus_schedule_gap,
    'How large was the actual gap between German fiscal capacity and the scheduled reparations burden at each renegotiation point (1921 London Schedule, 1924 Dawes Plan, 1929 Young Plan)?',
    'Historical fiscal capacity estimates (national income, tax base, trade balance) compared against nominal and real scheduled payments at each point.',
    'A large and persistent gap supports the tangled_rope classification''s claim of extraction beyond coordination need; a narrow or closing gap would push the story toward the limited_responsibility_reading''s lower-ε profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reparations_capacity_versus_schedule_gap, empirical, 'The empirical magnitude of over-extraction relative to sustainable capacity across renegotiation points.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__punitive_liability_reading, 1919, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1919, 0.18).
narrative_ontology:measurement_basis(vers_tr_t1919, observed).
narrative_ontology:measurement(vers_tr_t1921, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1921, 0.2).
narrative_ontology:measurement_basis(vers_tr_t1921, observed).
narrative_ontology:measurement(vers_tr_t1923, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1923, 0.22).
narrative_ontology:measurement_basis(vers_tr_t1923, observed).
narrative_ontology:measurement(vers_tr_t1925, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1925, 0.26).
narrative_ontology:measurement_basis(vers_tr_t1925, observed).
narrative_ontology:measurement(vers_tr_t1929, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1929, 0.3).
narrative_ontology:measurement_basis(vers_tr_t1929, observed).
narrative_ontology:measurement(vers_tr_t1932, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1932, 0.28).
narrative_ontology:measurement_basis(vers_tr_t1932, observed).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1919, 0.68).
narrative_ontology:measurement_basis(vers_be_t1919, observed).
narrative_ontology:measurement(vers_be_t1921, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1921, 0.74).
narrative_ontology:measurement_basis(vers_be_t1921, observed).
narrative_ontology:measurement(vers_be_t1923, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1923, 0.85).
narrative_ontology:measurement_basis(vers_be_t1923, observed).
narrative_ontology:measurement(vers_be_t1925, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1925, 0.79).
narrative_ontology:measurement_basis(vers_be_t1925, observed).
narrative_ontology:measurement(vers_be_t1929, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1929, 0.7).
narrative_ontology:measurement_basis(vers_be_t1929, observed).
narrative_ontology:measurement(vers_be_t1932, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1932, 0.81).
narrative_ontology:measurement_basis(vers_be_t1932, observed).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1919, 0.6).
narrative_ontology:measurement_basis(vers_su_t1919, observed).
narrative_ontology:measurement(vers_su_t1921, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1921, 0.65).
narrative_ontology:measurement_basis(vers_su_t1921, observed).
narrative_ontology:measurement(vers_su_t1923, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1923, 0.88).
narrative_ontology:measurement_basis(vers_su_t1923, observed).
narrative_ontology:measurement(vers_su_t1925, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1925, 0.7).
narrative_ontology:measurement_basis(vers_su_t1925, observed).
narrative_ontology:measurement(vers_su_t1929, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1929, 0.6).
narrative_ontology:measurement_basis(vers_su_t1929, observed).
narrative_ontology:measurement(vers_su_t1932, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1932, 0.72).
narrative_ontology:measurement_basis(vers_su_t1932, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__punitive_liability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__repudiation_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, weimar_hyperinflation_monetary_policy).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, ruhr_occupation_1923).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the versailles_reparations_clauses kernel, decomposed per the ε-invariance principle rather than modeled as a single constraint with a measurement parameter. punitive_liability_reading (this story) carries the highest ε and computes as tangled_rope; limited_responsibility_reading carries a lower ε bounded by capacity and computes closer to rope/scaffold; repudiation_reading treats the obligation as void and computes near-zero ε. Each reading shares the Article 231 textual kernel but authors independent beneficiary/victim structures, metrics, and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
