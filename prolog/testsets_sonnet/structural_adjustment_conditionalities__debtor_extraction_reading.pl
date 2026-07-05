% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__debtor_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__debtor_extraction_reading, []).

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
 *   constraint_id: structural_adjustment_conditionalities__debtor_extraction_reading
 *   human_readable: Structural Adjustment Conditionalities — Debtor Extraction Reading
 *   domain: international_political_economy/development_finance
 *
 * SUMMARY:
 *   Structural adjustment conditionality links disbursement of emergency
 *   sovereign financing to debtor-state adoption of fiscal austerity, subsidy
 *   removal, currency devaluation, trade liberalization, and privatization of
 *   state assets. Under this reading, the arrangement functions as a
 *   mechanism for transferring resources from debtor populations to
 *   transnational creditors and investors: domestic wage freezes and subsidy
 *   cuts fund continued debt service to banks and bondholders, while forced
 *   privatization transfers state assets to multinational investors at
 *   depressed valuations. The debtor state's finance ministry administers the
 *   terms domestically but did not set them and cannot meaningfully
 *   renegotiate them without triggering default consequences that are, in
 *   practice, catastrophic and immediate.
 *
 * KEY AGENTS:
 *   - transnational_creditor_banks: primary beneficiary (institutional/arbitrage) — receives continued debt service secured by austerity
 *   - creditor_state_treasuries: agenda_setter and beneficiary (institutional/arbitrage) — controls voting shares and program design at lending institutions
 *   - debtor_state_finance_ministry: administers under duress (moderate/constrained) — nominal signatory, no comparable bargaining power
 *   - domestic_public_sector_workers, rural_smallholder_farmers, urban_poor, public_health_service_users: primary targets (powerless/trapped) — bear the adjustment cost directly
 *   - development_economists_dependency_school: analytical observer — documents the extraction pattern across cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, 0.87).
domain_priors:suppression_score(structural_adjustment_conditionalities__debtor_extraction_reading, 0.79).
domain_priors:theater_ratio(structural_adjustment_conditionalities__debtor_extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__debtor_extraction_reading, snare).
narrative_ontology:human_readable(structural_adjustment_conditionalities__debtor_extraction_reading, "Structural Adjustment Conditionalities — Debtor Extraction Reading").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__debtor_extraction_reading, "international_political_economy/development_finance").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__debtor_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__debtor_extraction_reading, '9661e058-ca0c-4f3a-bfab-6efbb6d4ac2b').
narrative_ontology:cs_kernel_codification('9661e058-ca0c-4f3a-bfab-6efbb6d4ac2b', formalized).
narrative_ontology:cs_authority_grounding('9661e058-ca0c-4f3a-bfab-6efbb6d4ac2b', extraction).
narrative_ontology:cs_interpretation_layer_present('9661e058-ca0c-4f3a-bfab-6efbb6d4ac2b').
narrative_ontology:cs_reading_relation('9661e058-ca0c-4f3a-bfab-6efbb6d4ac2b', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('9661e058-ca0c-4f3a-bfab-6efbb6d4ac2b', structural_adjustment_conditionalities__hybrid_selectivity_reading, influences).
narrative_ontology:cs_axiom('9661e058-ca0c-4f3a-bfab-6efbb6d4ac2b', foundational, conditionality_severity_reflects_creditor_power_not_fiscal_necessity).
narrative_ontology:cs_axiom_status(conditionality_severity_reflects_creditor_power_not_fiscal_necessity, holdable).
narrative_ontology:cs_axiom_grounding('9661e058-ca0c-4f3a-bfab-6efbb6d4ac2b', conditionality_severity_reflects_creditor_power_not_fiscal_necessity, empirically_contingent).
narrative_ontology:cs_axiom('9661e058-ca0c-4f3a-bfab-6efbb6d4ac2b', foundational, debtor_sovereignty_is_substantively_suspended_under_program_administration).
narrative_ontology:cs_axiom_status(debtor_sovereignty_is_substantively_suspended_under_program_administration, holdable).
narrative_ontology:cs_axiom_grounding('9661e058-ca0c-4f3a-bfab-6efbb6d4ac2b', debtor_sovereignty_is_substantively_suspended_under_program_administration, deontological).
narrative_ontology:cs_reference_frame('9661e058-ca0c-4f3a-bfab-6efbb6d4ac2b', bretton_woods_emergency_lender_of_last_resort).
narrative_ontology:cs_drift_state('9661e058-ca0c-4f3a-bfab-6efbb6d4ac2b', post_washington_consensus_critique_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9661e058-ca0c-4f3a-bfab-6efbb6d4ac2b', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, bondholder_asset_managers).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_state_treasuries).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, multinational_investors_in_privatized_assets).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_public_sector_workers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, rural_smallholder_farmers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, urban_poor_reliant_on_subsidies).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, public_health_service_users).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, future_generations_of_debtor_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_finance_ministry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold sovereign debt instruments and receive continued debt service because conditionality-linked lending programs are structured to prioritize repayment over domestic spending. Can exit any single debtor relationship, diversify across sovereigns, and price risk into new lending. Bear essentially none of the adjustment cost themselves.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks, beneficiary,
    institutional, generational, arbitrage, global).

% Dominate voting shares and staff leadership at the multilateral lending institutions that design and enforce conditionality packages. Set the fiscal targets, privatization schedules, and subsidy-removal timelines debtor governments must meet to receive disbursements, and can suspend tranches unilaterally if targets are missed.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_state_treasuries, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_state_treasuries, beneficiary).

% Trade sovereign bonds whose value is protected by conditionality-enforced fiscal discipline; benefit indirectly from austerity that prioritizes debt service, without ever appearing in the negotiation room. Free to sell exposure to any given country at will.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, bondholder_asset_managers, beneficiary,
    organized, biographical, arbitrage, global).

% Acquire state enterprises, utilities, and land at depressed valuations once privatization conditions force asset sales. Structural adjustment programs open sectors that were previously protected, converting public assets into private revenue streams.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, multinational_investors_in_privatized_assets, beneficiary,
    organized, biographical, arbitrage, global).

% Signs and administers the conditionality agreement domestically, implementing subsidy cuts, currency devaluation, and public sector layoffs under threat of disbursement suspension or credit rating collapse. Nominally an agenda-setter within its borders, but its menu of choices is set upstream; exit would mean default, capital flight, and exclusion from international capital markets.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_finance_ministry, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_finance_ministry, agenda_setter).

% Face mandated wage freezes, mass layoffs, and pension cuts as fiscal consolidation targets under the program. Cannot relocate their labor to another jurisdiction easily and have no seat in the negotiations that determine their employment terms.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_public_sector_workers, payer,
    powerless, biographical, trapped, national).

% Lose input subsidies and price supports mandated for removal under trade liberalization conditions, while facing import competition from subsidized foreign agriculture. Land and livelihood immobility mean exit from the arrangement means destitution or forced migration.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, rural_smallholder_farmers, payer,
    powerless, biographical, trapped, regional).

% Absorb the removal of fuel, food, and utility subsidies required to hit fiscal targets, often triggering sudden price spikes on basic goods. Have no organized voice in program design and limited capacity to relocate or substitute away from affected goods.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, urban_poor_reliant_on_subsidies, payer,
    powerless, immediate, trapped, national).

% Experience reduced public health spending as conditionality-mandated expenditure ceilings compress the health budget, increasing user fees and reducing service availability. Cannot exit the national health system they depend on.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, public_health_service_users, payer,
    powerless, generational, trapped, national).

% Inherit degraded public infrastructure, weakened state capacity, and a privatized asset base whose returns flow largely abroad — a structural legacy that constrains their own future fiscal and developmental choices without their having consented to the original agreement.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, future_generations_of_debtor_citizens, payer,
    powerless, civilizational, trapped, national).

% Organize protests and advocacy against austerity measures but have no formal role in conditionality design or renegotiation, which occurs between finance ministry technocrats and lending institution staff. Their objections are registered in the street, not the negotiating room.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, civil_society_and_labor_movements, excluded,
    powerless, biographical, trapped, national).

% Document the historical pattern of conditionality programs producing sustained current-account transfers from debtor to creditor economies, drawing continuity with earlier colonial extraction structures. Their analysis informs the extraction reading of this kernel but does not itself alter program design.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, development_economists_dependency_school, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__debtor_extraction_reading, diffuse).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__debtor_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is a residual coordination claim — sovereign default is genuinely disruptive to both parties, and conditionality nominally coordinates fiscal behavior with continued credit access — but under this reading that coordination function is a cover story: the terms are set unilaterally by creditor-dominated institutions, and the debtor state has no comparable power to shape the bargain.
% TRANSFER_FUNCTION: Moves real resources — foregone wages, cut subsidies, sold public assets, reduced public services — from domestic populations in debtor states to creditor banks (via continued debt service), bondholders (via protected asset value), and multinational investors (via discounted privatized assets).
% ABSENT_VOICES: Public sector workers, subsistence farmers, health service users, and future generations bear the adjustment cost but are not parties to conditionality negotiations, which occur exclusively between finance ministry technocrats and creditor-institution staff. Civil society and labor movements object through protest, not through any formal channel in program design.
% DISAPPEARANCE_RATIONALE: If conditionality enforcement vanished, debtor governments would regain domestic fiscal discretion — able to shield subsidies, public wages, and state assets from creditor-mandated cuts — while creditor banks and bondholders would face materially higher default risk on outstanding sovereign debt and would need to reprice or renegotiate exposure. Privatization pipelines feeding multinational investors would stall. The current-account transfer structure that development economists document would end.
% FOUNDING_PROBLEM: Debtor states faced balance-of-payments crises and needed emergency external financing to avoid disorderly default; conditionality was framed as ensuring that financing wasn't simply spent without addressing the underlying fiscal imbalance.
% FOUNDING_PROBLEM_CORROBORATION: Creditor institutions and their board-representing treasuries attest the founding problem remains live (ongoing fiscal risk in borrowing states). Independent development economists, UN human rights rapporteurs on debt and poverty, and post-program historical audits from outside the lending institutions attest that in many cases the fiscal targets exceeded what crisis resolution required and that the sustained pattern of austerity-linked asset transfer outlived any plausible emergency justification — corroboration exists on both sides of the status question, which is why it is authored as contested rather than resolved.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__debtor_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__debtor_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__debtor_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, 0.87, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__debtor_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(structural_adjustment_conditionalities__debtor_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.87 at interval end) because the resource transfer under this reading is structural and recurring: continued debt service is prioritized over domestic spending as a condition of ongoing disbursement, and privatization terms transfer public assets to external buyers below market value. Suppression is authored high (0.79) because the mechanism that keeps the arrangement in place is not persuasion but structural coercion — exclusion from international capital markets, credit rating collapse, and capital flight are the enforced consequences of noncompliance, and no comparable alternative financing source exists for a state already in crisis. Theater ratio rises over the interval (0.22 to 0.42) reflecting a documented pattern in which conditionality packages increasingly retain performative 'poverty reduction strategy' and 'social safety net' components alongside the core fiscal targets, without those components altering the underlying extraction structure — a Goodhart-style substitution where visible mitigation measures substitute for actual burden reduction.
 *
 * DIRECTIONALITY LOGIC:
 *   Creditor banks, bondholder asset managers, and creditor state treasuries sit near the full-beneficiary end: they set terms, can exit any single relationship via diversification or sale, and collect the transfer. Multinational investors in privatized assets are similarly positioned as beneficiaries who enter only once conditions force asset sales. The debtor state finance ministry occupies an intermediate but constrained position — nominally an agenda-setter domestically, but its actual bargaining position is closer to a payer forced to administer terms it did not set, which is why exit_options is authored as constrained rather than mobile. Domestic populations (public sector workers, farmers, urban poor, health service users) sit at the full-target end: trapped exit options, no seat in negotiation, and direct absorption of the adjustment cost. Future generations are authored as trapped and civilizational-horizon because privatized assets and degraded state capacity persist as inherited structural conditions beyond any single program's duration.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — balance-of-payments crisis requiring emergency financing without moral hazard — was genuinely live in early program instances. Under this reading, the arrangement's mandate has been extended well past resolving individual crises into a durable extraction architecture: programs recur, targets tighten, and privatization conditions expand into sectors with no plausible connection to fiscal sustainability (utilities, land, health infrastructure). The status is authored as contested rather than dead because creditor institutions maintain the founding problem is still live in each specific case (a claim that is locally true for any given crisis) while the extraction reading identifies the pattern across cases as evidence the mandate has become self-perpetuating. This is precisely the mislabeling risk mandatrophy classification exists to catch: treating a recurring, self-justifying crisis-response architecture as if each instance were a fresh, neutral coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_cover_story,
    'Is the fiscal-discipline coordination function genuinely necessary to prevent disorderly default, or is it a cover story for a bargaining structure in which creditors set terms unilaterally regardless of actual fiscal necessity?',
    'Compare program-mandated fiscal targets against independent post-hoc analysis of what would have been minimally sufficient to restore debt sustainability in each case; a persistent gap between mandated austerity and minimally sufficient austerity would support the extraction reading over the coordination reading.',
    'If targets consistently exceed what crisis resolution required, this reading''s snare classification is strengthened; if targets track independently-verified sustainability thresholds closely, the coordination reading gains support and this reading''s extraction premise weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_cover_story, empirical, 'Whether conditionality severity tracks genuine fiscal necessity or creditor bargaining power.').

omega_variable(
    debtor_state_agency_question,
    'Does the debtor state''s finance ministry retain meaningful agency as a co-negotiator, or is its signatory role effectively coerced by the absence of any comparable alternative financing source?',
    'Examine instances where debtor states successfully renegotiated or rejected conditionality terms and what alternative financing, if any, was available to them at the time.',
    'Frequent successful pushback with viable alternatives would suggest the finance ministry has more genuine bargaining power than this reading assumes, pulling the classification toward tangled_rope; near-uniform compliance under threat of exclusion supports the snare classification authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debtor_state_agency_question, empirical, 'Whether domestic administering authorities are genuine co-negotiators or coerced implementers.').

omega_variable(
    kernel_reading_selection_basis,
    'Among the three declared readings of the conditionality kernel (coordination, extraction, hybrid selectivity), what evidence determines which reading best fits a given historical program instance, versus which reading is simply the analyst''s prior?',
    'Case-level historical audit comparing program design, enforcement intensity, and outcome distribution against each reading''s predictions — the hybrid_selectivity_reading in particular predicts systematic variance by debtor geopolitical importance that the extraction reading (authored here as structurally uniform) does not predict.',
    'If enforcement intensity varies systematically with debtor geopolitical alignment rather than applying uniformly, the hybrid_selectivity_reading may better explain the corpus of cases than the uniform-extraction premise this story authors, though the two readings are not mutually exclusive across different debtor states.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether uniform extraction or selective severity better characterizes the empirical pattern across debtor states.').

omega_variable(
    natural_disaster_vs_constructed_crisis,
    'Are the balance-of-payments crises that trigger conditionality programs themselves partly downstream of prior conditionality-linked liberalization (capital account opening, currency convertibility mandates) — making the ''crisis'' the arrangement claims to solve partly self-generated?',
    'Trace the policy history of debtor states prior to crisis onset for prior program-mandated liberalization measures that increased capital flight vulnerability.',
    'If crises are frequently downstream of prior program conditions, the founding-problem framing itself is compromised — the arrangement would be solving a problem it substantially created, which strengthens the extraction reading considerably.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_disaster_vs_constructed_crisis, conceptual, 'Whether the crises justifying conditionality are exogenous or partly endogenous to prior program conditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__debtor_extraction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(stru_tr_t8, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(stru_tr_t16, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(stru_tr_t24, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(stru_tr_t32, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(stru_tr_t40, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(stru_be_t8, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(stru_be_t16, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 16, 0.73).
narrative_ontology:measurement(stru_be_t24, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 24, 0.79).
narrative_ontology:measurement(stru_be_t32, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 32, 0.84).
narrative_ontology:measurement(stru_be_t40, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 40, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stru_su_t8, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(stru_su_t16, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(stru_su_t24, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(stru_su_t32, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(stru_su_t40, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__debtor_extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(structural_adjustment_conditionalities__debtor_extraction_reading, 0.1).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__hybrid_selectivity_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, sovereign_debt_restructuring_holdout_litigation).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, privatization_mandates_utility_sector).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the structural_adjustment_conditionalities kernel. creditor_coordination_reading claims the same underlying policy-conditional-lending arrangement as a rope (necessary coordination avoiding moral hazard, debtor as rational co-signatory). hybrid_selectivity_reading claims severity itself varies by debtor geopolitical alignment, making the arrangement a tangled_rope whose extraction intensity is selectively applied rather than uniform. This story (debtor_extraction_reading) claims the arrangement is uniformly a snare: coercive, victim-bearing, with the coordination function as cover. All three share the same underlying policy mechanism but diverge in claimed type, beneficiary/victim structure, and metrics — per the ε-invariance principle, they are authored as three separate constraints rather than one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
