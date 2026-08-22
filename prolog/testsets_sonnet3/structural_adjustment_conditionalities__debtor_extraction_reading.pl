% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__debtor_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   This story reads a single, standing arrangement — creditor-imposed
 *   conditionality attached to sovereign lending programs — through the lens
 *   that treats it as coercive extraction rather than mutual coordination. A
 *   debtor state facing balance-of-payments crisis is offered financing
 *   contingent on domestic policy changes: fiscal consolidation, subsidy
 *   removal, privatization, currency liberalization. Under this reading, the
 *   arrangement is not a negotiated bargain between equals but a coercive
 *   instrument: the debtor state's 'consent' is manufactured by the absence
 *   of any real alternative to default or collapse, and the resulting
 *   austerity is transferred directly onto populations who never sat at the
 *   negotiating table. The coordination story — that conditionality restores
 *   fiscal discipline and market confidence for everyone's benefit —
 *   functions as legitimating cover for a transfer of real resources to
 *   creditor institutions and to the domestic elites positioned to profit
 *   from liberalization. This is one of three linked readings of the same
 *   kernel (structural_adjustment_conditionalities): the
 *   creditor_coordination_reading treats the identical arrangement as
 *   necessary fiscal discipline with low extraction; the
 *   hybrid_selectivity_reading treats enforcement as geopolitically
 *   selective. ε in THIS reading (0.87 at interval end) is authored from this
 *   reading's own lights and is not averaged against or reconciled to the
 *   siblings' ε values — each reading has its own referent and its own stable
 *   ε per the ε-invariance principle.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, 0.87).
domain_priors:suppression_score(structural_adjustment_conditionalities__debtor_extraction_reading, 0.81).
domain_priors:theater_ratio(structural_adjustment_conditionalities__debtor_extraction_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__debtor_extraction_reading, snare).
narrative_ontology:human_readable(structural_adjustment_conditionalities__debtor_extraction_reading, "Structural Adjustment Conditionalities — Debtor Extraction Reading").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__debtor_extraction_reading, "international_political_economy/development_finance").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__debtor_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__debtor_extraction_reading, '039357ad-cf78-4d45-a7ad-b861d5212956').
narrative_ontology:cs_kernel_codification('039357ad-cf78-4d45-a7ad-b861d5212956', formalized).
narrative_ontology:cs_authority_grounding('039357ad-cf78-4d45-a7ad-b861d5212956', extraction).
narrative_ontology:cs_interpretation_layer_present('039357ad-cf78-4d45-a7ad-b861d5212956').
narrative_ontology:cs_reading_relation('039357ad-cf78-4d45-a7ad-b861d5212956', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('039357ad-cf78-4d45-a7ad-b861d5212956', structural_adjustment_conditionalities__hybrid_selectivity_reading, influences).
narrative_ontology:cs_axiom('039357ad-cf78-4d45-a7ad-b861d5212956', foundational, conditionality_consent_is_structurally_coerced).
narrative_ontology:cs_axiom_status(conditionality_consent_is_structurally_coerced, holdable).
narrative_ontology:cs_axiom_grounding('039357ad-cf78-4d45-a7ad-b861d5212956', conditionality_consent_is_structurally_coerced, empirically_contingent).
narrative_ontology:cs_axiom('039357ad-cf78-4d45-a7ad-b861d5212956', foundational, creditor_protection_is_the_arrangements_true_founding_function).
narrative_ontology:cs_axiom_status(creditor_protection_is_the_arrangements_true_founding_function, holdable).
narrative_ontology:cs_axiom_grounding('039357ad-cf78-4d45-a7ad-b861d5212956', creditor_protection_is_the_arrangements_true_founding_function, empirically_contingent).
narrative_ontology:cs_reference_frame('039357ad-cf78-4d45-a7ad-b861d5212956', post_bretton_woods_conditional_lending_order).
narrative_ontology:cs_drift_state('039357ad-cf78-4d45-a7ad-b861d5212956', post_2008_debt_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('039357ad-cf78-4d45-a7ad-b861d5212956', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, bondholder_capital).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_state_treasuries).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_public_sector_workers).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, rural_smallholders).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, urban_poor_service_users).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, national_health_and_education_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_export_oligarchs).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_finance_ministry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold sovereign debt instruments and receive continued debt service extracted from the borrowing state's budget as conditionality-driven austerity frees fiscal space for repayment. They face no direct exposure to the domestic social costs and can trade or hedge their exposure across borrowers at will.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks, beneficiary,
    institutional, generational, arbitrage, global).

% Dominate voting shares on the lending institution's board, shape the conditionality template, and benefit from the debtor state's continued integration into a trade and capital regime favorable to their own exporters and banks.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_state_treasuries, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_state_treasuries, agenda_setter).

% Design and monitor the loan conditionality package — fiscal targets, privatization schedules, subsidy removal timetables — and can withhold disbursement tranches if targets are missed. Their careers advance on program completion metrics, not on domestic welfare outcomes.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, lending_institution_program_staff, agenda_setter,
    institutional, biographical, mobile, global).

% Negotiates and signs the conditionality agreement under acute balance-of-payments pressure, then administers the cuts domestically. Formally consents to each round, but the alternative is default, currency collapse, or exclusion from capital markets — a choice structured to appear voluntary while foreclosing real alternatives.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_finance_ministry, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_finance_ministry, agenda_setter).

% Face mass layoffs, wage freezes, and pension cuts mandated by fiscal consolidation targets they had no voice in setting. Emigration is the only meaningful exit, available mainly to the more skilled among them.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_public_sector_workers, payer,
    powerless, biographical, trapped, national).

% Lose input subsidies and price supports mandated by market-liberalization conditions, and absorb currency devaluation costs on imported inputs while export crop prices are set in markets they cannot influence. Land and livelihood tie them to place; exit means abandoning both.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, rural_smallholders, payer,
    powerless, biographical, trapped, regional).

% Absorb the introduction or steep increase of user fees for healthcare and education mandated by cost-recovery conditions. Many are priced out of services previously provided at low or no cost, with no alternative provider available.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, urban_poor_service_users, payer,
    powerless, immediate, trapped, national).

% Institutional capacity is degraded by budget ceilings and staffing freezes mandated as fiscal targets, with facility closures and service quality collapse compounding over successive program cycles.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, national_health_and_education_systems, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(structural_adjustment_conditionalities__debtor_extraction_reading, national_health_and_education_systems).

% Benefit from currency devaluation and trade liberalization conditions that cheapen their exports and lift import competition on inputs, gaining domestic market share and export revenue while the broader population absorbs the devaluation's cost-of-living effects.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_export_oligarchs, beneficiary,
    powerful, biographical, mobile, national).

% Organize strikes and protests against austerity measures but are not party to the conditionality negotiation itself, which occurs between finance ministry technocrats and lending institution staff behind closed doors. Their objections register only as post-hoc 'implementation risk' in program documents.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_social_movements, excluded,
    organized, biographical, constrained, national).

% Study program outcomes across dozens of debtor countries, documenting growth collapse, poverty spikes, and service degradation following conditionality rounds, publishing findings independent of both lender and borrower institutions.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, independent_development_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_creditor_banks).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__debtor_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In this reading, the coordination story — that conditionality disciplines fiscal behavior to restore market confidence and enable future lending — is cover. The actual function is extraction: conditionality converts a solvency crisis into a mechanism for guaranteeing continued debt service by transferring the adjustment cost onto populations with no voice in the terms.
% TRANSFER_FUNCTION: Moves real resources — foregone wages, degraded public services, lost subsidies, devalued currency purchasing power — from domestic populations, especially the poorest and least mobile, to creditor institutions as continued debt service, and secondarily to domestic export elites positioned to benefit from devaluation and liberalization.
% ABSENT_VOICES: Domestic social movements, public sector unions, and the rural and urban poor who bear the adjustment are not parties to the negotiation, which occurs between finance ministry technocrats (acting under duress) and lending institution program staff. Legislatures in many debtor states are presented conditionality packages as faits accomplis.
% DISAPPEARANCE_RATIONALE: If conditionality enforcement vanished, debtor states would very likely restructure or partially default rather than complete fiscal consolidation on the mandated schedule; creditor recovery rates would fall sharply, domestic public services would be restored or expanded, and the entire architecture of program-linked lending would require renegotiation from a position of far greater debtor leverage.
% FOUNDING_PROBLEM: Sovereign debt crises threatened creditor losses and international financial contagion; conditionality was built to ensure orderly (i.e., creditor-favorable) resolution by making continued lending contingent on borrower policy changes that preserved debt service capacity.
% FOUNDING_PROBLEM_CORROBORATION: Independent development economists and post-program empirical reviews (including retrospective assessments commissioned by academic and UN-system bodies outside the lending institutions themselves) corroborate that the debt-service-preservation function remains live and has, if anything, intensified across successive program generations; this corroboration comes from outside the creditor and lending-institution seats that benefit from the arrangement.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__debtor_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__debtor_extraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high and rising (0.58 to 0.87) because successive program generations have layered additional conditions (structural benchmarks, governance triggers) onto the original fiscal targets, widening the domain of domestic policy subordinated to creditor terms. Suppression is authored high (0.81) because exit from the arrangement — restructuring, default, capital controls — is met with credit rating downgrades, capital flight, and exclusion from future market access, actively enforced by the interlocking incentives of rating agencies, bondholders, and the lending institution's own disbursement leverage. Theater ratio is moderate (0.40): some conditionality genuinely tracks fiscal sustainability, but a rising share of program design (structural benchmarks on labor law, state-owned enterprise governance, etc.) extends well beyond what solvency requires and functions as leverage extension rather than crisis resolution.
 *
 * PERSPECTIVAL GAP:
 *   From the creditor and lending-institution seats, the arrangement looks like prudent, temporary discipline in exchange for continued access to capital. From the domestic payer seats, the identical structure delivers coerced austerity with no meaningful voice in its terms and no meaningful exit. The engine computes these as structurally different experiences of the same positional data (power, exit, scope) — this reading does not average the two experiences into one number; it asserts that the extraction reading correctly describes the arrangement's actual operation, while acknowledging (via the sibling readings) that other parties hold the coordination framing in good faith.
 *
 * DIRECTIONALITY LOGIC:
 *   Transnational creditor banks and bondholders sit at the full-beneficiary end: they collect continued debt service without absorbing any of the domestic adjustment cost, and their exit options (portfolio diversification, credit default swaps) are unconstrained. The debtor finance ministry occupies an ambiguous dual position — formally an agenda-setter (it signs the agreement) but structurally a payer, since its 'choice' is constrained to the point of near-coercion; this is exactly the seat divergence the framework is built to surface. Domestic public sector workers, rural smallholders, and urban service users sit at the full-target end: trapped exit, powerless bargaining position, and the arrangement's costs land on them directly and disproportionately relative to any benefit received.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — creditor exposure to disorderly sovereign default — is authored as still live, which forecloses a simple mandatrophy-resolved verdict (the arrangement is not merely an inertial holdover; it continues to actively serve its founding purpose for creditors). But this reading holds that the founding purpose itself was never symmetric coordination — it was creditor protection from the outset, and the escalating conditionality scope (governance and structural benchmarks added over successive program generations, per the measurements series) shows the arrangement's mandate expanding well past what debt sustainability requires, which is the extraction-accumulation signature this reading is built to register.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_coercion_boundary,
    'Is the debtor state''s formal agreement to conditionality terms genuine consent (however constrained) or is it structurally equivalent to coercion given the absence of a viable alternative?',
    'Comparative analysis of debtor states that have credibly threatened or executed unilateral default/restructuring outside the conditionality framework: if such states achieve comparable or better outcomes without catastrophic capital-market exclusion, the ''no alternative'' premise weakens and the coercion framing strengthens.',
    'If genuine alternatives existed and were merely unattractive rather than foreclosed, this reading''s snare classification weakens toward the hybrid or coordination readings; if alternatives are shown to be effectively foreclosed by coordinated creditor and rating-agency behavior, the snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_coercion_boundary, conceptual, 'Whether debtor consent to conditionality is meaningfully voluntary or structurally coerced.').

omega_variable(
    conditionality_scope_creep_causation,
    'Is the expansion of conditionality scope over successive program generations (from narrow fiscal targets to broad structural and governance benchmarks) driven by genuine learning about what sustains fiscal solvency, or by creditor leverage extension once the initial crisis-response justification was established?',
    'Archival and comparative analysis of program design documents across decades, cross-referenced against independent assessments of which conditions actually correlated with improved debt sustainability versus which correlated with expanded creditor-favorable policy environments (privatization terms, capital account liberalization) unrelated to solvency.',
    'If scope creep tracks genuine solvency-relevant learning, part of the rising extractiveness measurement should be attributed to legitimate coordination refinement rather than extraction; if it tracks leverage extension, the rising trend corroborates the extraction-accumulation reading directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_scope_creep_causation, empirical, 'Whether widening conditionality scope reflects genuine fiscal learning or extraction ratchet.').

omega_variable(
    cs_framing_which_actor_is_kernel,
    'Is the kernel more accurately framed as the specific loan-conditionality text negotiated per program, or as the deeper legitimacy claim that market discipline via conditionality is the only responsible response to sovereign insolvency?',
    'Trace whether disputes over specific programs (framing 1) resolve independently of disputes over the underlying legitimacy claim (framing 2) — if program-level disputes are settled by renegotiation while the legitimacy claim remains untouched across decades, framing 2 is the more structurally load-bearing kernel.',
    'Under framing 1 (the specific text), this reading''s classification rests on program-specific enforcement mechanics. Under framing 2 (the legitimacy claim), the classification would need to account for a much longer-running, more deeply institutionalized authority structure (the entire post-Bretton-Woods architecture of conditional lending), potentially raising suppression and lowering resistance further since the legitimacy claim itself faces less direct contestation than any single program.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_which_actor_is_kernel, conceptual, 'Whether the operative kernel is the per-program conditionality text or the deeper market-discipline legitimacy claim it instantiates.').


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
narrative_ontology:measurement(stru_tr_t24, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(stru_tr_t32, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement(stru_tr_t40, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(stru_be_t8, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(stru_be_t16, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 16, 0.73).
narrative_ontology:measurement(stru_be_t24, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 24, 0.79).
narrative_ontology:measurement(stru_be_t32, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 32, 0.84).
narrative_ontology:measurement(stru_be_t40, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 40, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stru_su_t8, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(stru_su_t16, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(stru_su_t24, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(stru_su_t32, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 32, 0.78).
narrative_ontology:measurement(stru_su_t40, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 40, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__debtor_extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities__hybrid_selectivity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the structural_adjustment_conditionalities kernel, each a separate constraint file per the ε-invariance principle: this file (debtor_extraction_reading, snare, ε=0.87) treats the arrangement as coercive extraction with domestic populations as victims and creditor capital as beneficiary; structural_adjustment_conditionalities__creditor_coordination_reading treats the identical standing arrangement as necessary, low-extraction fiscal coordination; structural_adjustment_conditionalities__hybrid_selectivity_reading treats it as selectively enforced discipline, harsh on weak debtors and waived for strategic ones. The three files share no averaged ε — each is authored from its own reading's lights per the referent rule for kernel-reading stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
