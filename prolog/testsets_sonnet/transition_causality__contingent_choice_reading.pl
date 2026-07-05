% ============================================================================
% CONSTRAINT STORY: transition_causality__contingent_choice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__contingent_choice_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: transition_causality__contingent_choice_reading
 *   human_readable: Bretton Woods Suspension as Contingent Nixon Policy Choice
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This story instantiates the contingent-choice reading of the Bretton
 *   Woods transition kernel: the August 1971 suspension of dollar-gold
 *   convertibility was a discretionary policy act taken by a small group at
 *   Camp David, made in response to real but not uniquely resolving
 *   pressures, with viable multilateral alternatives on the table that were
 *   bypassed for domestic political timing reasons. Sibling constraints
 *   (overdetermined_collapse_reading, hybrid_trigger_reading — not this file)
 *   instantiate the competing claims that the transition was structurally
 *   inevitable or required a contingent trigger against an already-collapsing
 *   structure. Each reading has its own epsilon, beneficiary structure, and
 *   classification; this file does not average or hedge across them. The
 *   extractiveness series shows a step increase concentrated at 1971
 *   (unilateral action) rather than smooth structural accumulation,
 *   consistent with a discretionary-decision reading rather than gradual
 *   inevitability.
 *
 * KEY AGENTS:
 *   - nixon_administration_political_coalition: Primary causal agent, institutional/arbitrage exit — made the discretionary choice and captured policy autonomy
 *   - us_domestic_manufacturers: Secondary beneficiary, organized/mobile — gained competitive relief without bearing reserve costs
 *   - foreign_dollar_reserve_holders: Primary target, powerful/trapped — absorbed value loss on dollar claims with no advance consultation
 *   - bretton_woods_treaty_partners: Institutional payer, institutional/constrained — forced to renegotiate the monetary order on a compressed timeline
 *   - economic_historians: Analytical observer — evaluates the counterfactual viability claim central to this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, 0.58).
domain_priors:suppression_score(transition_causality__contingent_choice_reading, 0.42).
domain_priors:theater_ratio(transition_causality__contingent_choice_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, accessibility_collapse, 0.47).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__contingent_choice_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__contingent_choice_reading, "Bretton Woods Suspension as Contingent Nixon Policy Choice").
narrative_ontology:topic_domain(transition_causality__contingent_choice_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__contingent_choice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__contingent_choice_reading, '3f95a846-b169-4e85-8a5b-b3fbbb45f22e').
narrative_ontology:cs_kernel_codification('3f95a846-b169-4e85-8a5b-b3fbbb45f22e', distributed).
narrative_ontology:cs_authority_grounding('3f95a846-b169-4e85-8a5b-b3fbbb45f22e', distributed).
narrative_ontology:cs_reading_relation('3f95a846-b169-4e85-8a5b-b3fbbb45f22e', transition_causality__overdetermined_collapse_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f95a846-b169-4e85-8a5b-b3fbbb45f22e', transition_causality__hybrid_trigger_reading, influences).
narrative_ontology:cs_axiom('3f95a846-b169-4e85-8a5b-b3fbbb45f22e', foundational, policy_discretion_was_decisive).
narrative_ontology:cs_axiom_status(policy_discretion_was_decisive, holdable).
narrative_ontology:cs_axiom_grounding('3f95a846-b169-4e85-8a5b-b3fbbb45f22e', policy_discretion_was_decisive, empirically_contingent).
narrative_ontology:cs_axiom('3f95a846-b169-4e85-8a5b-b3fbbb45f22e', secondary, negotiated_multilateral_alternative_was_executable).
narrative_ontology:cs_axiom_status(negotiated_multilateral_alternative_was_executable, holdable).
narrative_ontology:cs_axiom_grounding('3f95a846-b169-4e85-8a5b-b3fbbb45f22e', negotiated_multilateral_alternative_was_executable, empirically_contingent).
narrative_ontology:cs_reference_frame('3f95a846-b169-4e85-8a5b-b3fbbb45f22e', bretton_woods_fixed_convertibility_regime).
narrative_ontology:cs_drift_state('3f95a846-b169-4e85-8a5b-b3fbbb45f22e', camp_david_august_1971, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('3f95a846-b169-4e85-8a5b-b3fbbb45f22e', '').
narrative_ontology:cs_kernel_id(transition_causality__contingent_choice_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_executive_policy_autonomy).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_domestic_manufacturers).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, nixon_administration_political_coalition).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, foreign_dollar_reserve_holders).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, bretton_woods_treaty_partners).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, fixed_exchange_dependent_exporters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Faced a domestic reelection calendar, rising unemployment, and a dollar under speculative pressure. Chose to suspend gold convertibility unilaterally on August 15, 1971 rather than pursue available alternatives — coordinated devaluation, capital controls, or IMF-mediated adjustment. Controlled the timing, framing, and announcement; retained full discretion over which policy tool to deploy and when.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, nixon_administration_political_coalition, agenda_setter,
    institutional, immediate, arbitrage, global).

% Gained competitive relief as the dollar's de facto devaluation made US exports cheaper and import competition costlier. Lobbied actively for the wage-price freeze and import surcharge package that accompanied the gold suspension. Did not bear the reserve-currency costs that foreign holders absorbed.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_domestic_manufacturers, beneficiary,
    organized, biographical, mobile, national).

% The abstract policy capacity gained: freed from the gold-convertibility constraint, the executive branch acquired durable discretion over monetary and exchange-rate policy that persisted long after 1971, unconstrained by the treaty commitments that previously bound it.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_executive_policy_autonomy, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_non_agent(transition_causality__contingent_choice_reading, us_executive_policy_autonomy).

% Central banks (notably in Europe and Japan) held dollar reserves accumulated under the Bretton Woods promise of gold convertibility at $35/oz. The unilateral suspension left them holding depreciating claims with no advance consultation and no negotiated compensation; they could not have exited the dollar reserve system quickly enough to avoid the loss.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, foreign_dollar_reserve_holders, payer,
    powerful, biographical, trapped, global).

% Governments that had built fixed-exchange-rate policy architectures around the Bretton Woods system had those architectures unilaterally invalidated. Had to renegotiate the entire postwar monetary order (Smithsonian Agreement, eventual floating rates) on a compressed timeline set by Washington, not by treaty process.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, bretton_woods_treaty_partners, payer,
    institutional, generational, constrained, global).

% Exporters in countries whose competitiveness depended on the fixed-rate peg to the dollar faced sudden currency appreciation risk and the costs of adjusting supply chains and pricing to a floating-rate world they had not chosen to enter.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, fixed_exchange_dependent_exporters, payer,
    moderate, biographical, constrained, national).

% Coordinated multilateral devaluation, temporary capital controls, or a negotiated gold-price adjustment through IMF channels were available and had been proposed by Treasury staff and foreign counterparts. These paths had no seat in the room where the August 1971 decision was made at Camp David; their exclusion is central to the contingent-choice claim.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, alternative_policy_paths_not_taken, excluded,
    analytical, immediate, analytical, global).
narrative_ontology:stakeholder_non_agent(transition_causality__contingent_choice_reading, alternative_policy_paths_not_taken).

% Assess declassified Camp David records, Treasury memos, and comparative counterfactuals to evaluate whether the suspension was a discretionary policy act or a forced response to structural pressure. This story takes the position that the decision was substantially discretionary.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolving acute dollar-gold convertibility pressure required some coordinated policy response among the US and its major trading partners to prevent a disorderly run on US gold reserves.
% TRANSFER_FUNCTION: Moves adjustment costs from the US domestic economy (which gains competitiveness and policy discretion) to foreign reserve holders and treaty partners (who absorb the value loss on dollar holdings and the disruption of renegotiating the monetary order) — accomplished by unilateral executive action rather than negotiated multilateral adjustment.
% ABSENT_VOICES: Foreign finance ministries and central banks were not consulted before the August 15, 1971 announcement; IMF technical staff who had modeled multilateral adjustment paths were bypassed. Treasury's own internal advocates for a negotiated approach were overruled by the small Camp David circle.
% DISAPPEARANCE_RATIONALE: Proponents of this reading hold that had different individuals or different domestic political incentives been in place in mid-1971, a negotiated multilateral adjustment was genuinely available and the world would have arrived at a materially different monetary architecture — slower, more consultative, less US-centric in its discretionary power. Sibling readings dispute this counterfactual viability; that dispute is exactly what the kernel decomposition is for.
% FOUNDING_PROBLEM: By 1971 the US could not simultaneously maintain gold convertibility at $35/oz, fund domestic and Vietnam War spending, and prevent a speculative run on gold reserves triggered by persistent balance-of-payments deficits.
% FOUNDING_PROBLEM_CORROBORATION: Declassified Camp David memoranda and later admissions by participants (including Paul Volcker, present at the meeting) attest that alternative paths were actively discussed and rejected for political-timing reasons rather than being structurally foreclosed; this corroboration comes from a participant who later expressed reservations about the path chosen, not solely from the administration's own retrospective framing.
narrative_ontology:disappearance_verdict(transition_causality__contingent_choice_reading, contested).
narrative_ontology:founding_problem_status(transition_causality__contingent_choice_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__contingent_choice_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(transition_causality__contingent_choice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__contingent_choice_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__contingent_choice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(transition_causality__contingent_choice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58, moderate-high: the suspension transferred real value from foreign reserve holders to US domestic policy space, but the transfer was framed as (and partly was) a response to a genuine payments crisis, not pure predation — hence tangled_rope rather than snare. Suppression is moderate (0.42): the unilateral announcement foreclosed negotiation, but it did not require ongoing coercive enforcement against foreign governments beyond the fait accompli itself, which is why suppression sits below extraction. Theater ratio rose at the 1971 step (0.20 to 0.35) reflecting the New Economic Program's accompanying wage-price freeze theater layered onto the substantive gold suspension. Accessibility collapse is moderate (0.47): once announced, reversal became politically and practically difficult, but the pre-1971 window genuinely had open alternative paths, which is the crux of this reading's counterfactual-viability claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The Nixon administration and US domestic manufacturers sit near the beneficiary end: the policy choice was made by and substantially for their benefit, with low-cost exit (arbitrage/mobile). Foreign reserve holders and treaty partners sit near the target end: trapped or constrained exit, bearing costs from a decision made without their participation. This directional asymmetry is exactly what makes tangled_rope rather than rope the correct claim — there was a real coordination problem (payments crisis resolution) but the resolution mechanism chosen extracted disproportionately from non-consenting parties who had no seat in the decision.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unsustainable gold-dollar peg under fiscal and payments pressure) is genuinely dead — no one argues for restoring $35/oz convertibility. What remains contested is not whether the old system needed to end, but whether THIS specific way of ending it (unilateral, uncompensated, discretionary) was necessary or merely convenient for the parties who controlled the decision. This reading holds it was the latter, which is why founding_problem_status is 'dead' rather than 'live' — the mandate the decision claimed to serve is gone, but that does not retroactively validate the discretionary path taken over the negotiated alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_viability_of_negotiated_path,
    'Was a coordinated multilateral devaluation or IMF-mediated gold-price adjustment genuinely available in mid-1971, or had structural pressures already foreclosed that path by the time of the Camp David decision?',
    'Comparative analysis of declassified Treasury and IMF planning documents from 1969-1971 against the timeline of speculative pressure on gold reserves; interviews and memoirs of Treasury staff who advocated for the negotiated alternative (e.g., Paul Volcker''s own later ambivalence about the path chosen).',
    'If the negotiated path was genuinely viable, this contingent_choice_reading is the structurally accurate one and the transition should classify as tangled_rope reflecting discretionary extraction layered on a real but non-uniquely-resolved crisis. If the negotiated path was already foreclosed by 1971, the overdetermined_collapse_reading or hybrid_trigger_reading better captures the structure, and this reading''s beneficiary framing (Nixon administration as primary causal agent) overstates agency that had already been structurally exhausted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_viability_of_negotiated_path, empirical, 'Whether the multilateral alternative to unilateral suspension was a live option or already foreclosed.').

omega_variable(
    kernel_disagreement_location,
    'Where exactly do the three sibling readings of the transition_causality kernel disagree — is it about the antecedent conditions (degree of structural pressure), the decision process (who had real discretion at Camp David), or the counterfactual space (what alternatives were practically executable)?',
    'Structural comparison of the three constraint files'' beneficiary/victim declarations and their treatment of the same historical evidence base (Camp David transcripts, BOP data, gold reserve trajectories) to locate the precise point of divergence.',
    'Locating the disagreement clarifies whether the three readings are genuinely incompatible (forecloses) or represent different emphases on a shared causal structure (coexists_with/influences) — this determines the reading_relations declared in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_disagreement_location, conceptual, 'The specific structural element on which the three kernel readings differ.').

omega_variable(
    beneficiary_structure_asymmetry,
    'Is the concentration of benefit in US policy autonomy and domestic manufacturers a genuine structural feature of the decision, or does it reflect this reading''s selection of which downstream effects to count as ''benefits'' versus ''coordination outcomes''?',
    'Cross-check against economic-historical consensus on distributional effects of the 1971-1973 transition to floating rates across US and non-US economies.',
    'If the beneficiary concentration is an artifact of this reading''s framing rather than a robust historical finding, the tangled_rope classification (which requires genuine asymmetric extraction) would be weaker than authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_structure_asymmetry, empirical, 'Whether the authored beneficiary/victim asymmetry reflects genuine distributional history or reading-selection bias.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__contingent_choice_reading, 1958, 1976).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1958, transition_causality__contingent_choice_reading, theater_ratio, 1958, 0.1).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__contingent_choice_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(tran_tr_t1969, transition_causality__contingent_choice_reading, theater_ratio, 1969, 0.2).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__contingent_choice_reading, theater_ratio, 1971, 0.35).
narrative_ontology:measurement(tran_tr_t1973, transition_causality__contingent_choice_reading, theater_ratio, 1973, 0.32).
narrative_ontology:measurement(tran_tr_t1976, transition_causality__contingent_choice_reading, theater_ratio, 1976, 0.31).

% Extraction over time
narrative_ontology:measurement(tran_be_t1958, transition_causality__contingent_choice_reading, base_extractiveness, 1958, 0.22).
narrative_ontology:measurement(tran_be_t1965, transition_causality__contingent_choice_reading, base_extractiveness, 1965, 0.34).
narrative_ontology:measurement(tran_be_t1969, transition_causality__contingent_choice_reading, base_extractiveness, 1969, 0.45).
narrative_ontology:measurement(tran_be_t1971, transition_causality__contingent_choice_reading, base_extractiveness, 1971, 0.6).
narrative_ontology:measurement(tran_be_t1973, transition_causality__contingent_choice_reading, base_extractiveness, 1973, 0.58).
narrative_ontology:measurement(tran_be_t1976, transition_causality__contingent_choice_reading, base_extractiveness, 1976, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1958, transition_causality__contingent_choice_reading, suppression_requirement, 1958, 0.15).
narrative_ontology:measurement(tran_su_t1965, transition_causality__contingent_choice_reading, suppression_requirement, 1965, 0.22).
narrative_ontology:measurement(tran_su_t1969, transition_causality__contingent_choice_reading, suppression_requirement, 1969, 0.3).
narrative_ontology:measurement(tran_su_t1971, transition_causality__contingent_choice_reading, suppression_requirement, 1971, 0.48).
narrative_ontology:measurement(tran_su_t1973, transition_causality__contingent_choice_reading, suppression_requirement, 1973, 0.44).
narrative_ontology:measurement(tran_su_t1976, transition_causality__contingent_choice_reading, suppression_requirement, 1976, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, overdetermined_collapse_reading).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, hybrid_trigger_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the transition_causality kernel (contested claim: what caused the 1971 Bretton Woods suspension and was it avoidable). contingent_choice_reading places the Nixon administration's Camp David decision as the primary causal node with high counterfactual viability for alternative paths, and centers beneficiary structure on US policy autonomy gain. overdetermined_collapse_reading (sibling, separate file) holds the transition was structurally inevitable from reinforcing contradictions (Triffin dilemma, persistent BOP deficits, Vietnam spending, European dollar overhang) with low counterfactual viability. hybrid_trigger_reading (sibling, separate file) holds structural contradictions had accumulated to a critical threshold but still required the contingent Camp David decision to actualize the collapse at that specific moment and in that specific form. All three share the same underlying historical event but assign different epsilon values, different beneficiary/victim structures, and different classifications because they make different structural claims about causal necessity and available alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
