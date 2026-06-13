% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__hybrid_selectivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__hybrid_selectivity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: structural_adjustment_conditionalities__hybrid_selectivity_reading
 *   human_readable: Selective Structural Adjustment Conditionalities Regime
 *   domain: international_political_economy/development_finance
 *
 * SUMMARY:
 *   The International Monetary Fund and World Bank impose 'conditionalities'
 *   on debtor countries: austerity, privatization, trade liberalization, and
 *   public-sector contraction tied to loan disbursement. This reading
 *   instantiates the hybrid-selectivity reading of the
 *   structural-adjustment-conditionalities kernel: the regime is PRESENTED as
 *   universal discipline coordinating on fiscal sustainability, but is
 *   FUNCTIONALLY selective discipline—harshly enforced against geopolitically
 *   non-aligned states and waived or minimized for strategic partners. The
 *   reading asserts that selectivity proves the coordination story is cover;
 *   the true function is extraction from vulnerable states and geopolitical
 *   insurance for aligned ones. This constraint's extraction ε-value (~0.71)
 *   reflects the fact that for non-strategic debtors, conditionalities
 *   produce net transfer of resources and autonomy, while the coordination
 *   rationale does not hold under the selectivity evidence. The engine
 *   computes per-seat classifications from this structural data; the
 *   reading's narrative explains why different creditor and debtor seats
 *   experience it as either coordination or extraction.
 *
 * KEY AGENTS:
 *   - IMF/World Bank: agenda-setter, enforcer, beneficiary (collects authority and legitimacy from coordination frame; profits from selective enforcement)
 *   - Core creditor institutions: beneficiary (loan fees, debt service collection, reduced default risk from discipline)
 *   - Hegemon-aligned states: beneficiary (waivers preserve fiscal autonomy and political stability; immunity from discipline proves conditionality is not technically necessary)
 *   - Non-strategic debtor states: victim (bears full discipline; geopolitical non-alignment makes them ineligible for waivers; loses fiscal autonomy and resources)
 *   - Vulnerable populations in subjected states: victim (concrete costs: user fees, wage suppression, unemployment, price spikes from privatization)
 *   - Regional competitors (China, regional banks): excluded (locked out of conditionality design; cannot offer competing terms; waiver system protects hegemon from losing strategic debtors to alternatives)
 *   - Domestic reformers and civil society: excluded (no seat in creditor-debtor negotiations; would object but are not heard)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.71).
domain_priors:suppression_score(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.78).
domain_priors:theater_ratio(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__hybrid_selectivity_reading, tangled_rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__hybrid_selectivity_reading, "Selective Structural Adjustment Conditionalities Regime").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__hybrid_selectivity_reading, "international_political_economy/development_finance").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__hybrid_selectivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'a690c475-f6c2-4113-878d-def51b2f7909').
narrative_ontology:cs_kernel_codification('a690c475-f6c2-4113-878d-def51b2f7909', formalized).
narrative_ontology:cs_authority_grounding('a690c475-f6c2-4113-878d-def51b2f7909', extraction).
narrative_ontology:cs_interpretation_layer_present('a690c475-f6c2-4113-878d-def51b2f7909').
narrative_ontology:cs_reading_relation('a690c475-f6c2-4113-878d-def51b2f7909', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('a690c475-f6c2-4113-878d-def51b2f7909', structural_adjustment_conditionalities__debtor_extraction_reading, influences).
narrative_ontology:cs_axiom('a690c475-f6c2-4113-878d-def51b2f7909', foundational, conditionality_universality_false).
narrative_ontology:cs_axiom_status(conditionality_universality_false, holdable).
narrative_ontology:cs_axiom_grounding('a690c475-f6c2-4113-878d-def51b2f7909', conditionality_universality_false, empirically_contingent).
narrative_ontology:cs_axiom('a690c475-f6c2-4113-878d-def51b2f7909', foundational, selective_enforcement_proves_extraction_over_coordination).
narrative_ontology:cs_axiom_status(selective_enforcement_proves_extraction_over_coordination, holdable).
narrative_ontology:cs_axiom_grounding('a690c475-f6c2-4113-878d-def51b2f7909', selective_enforcement_proves_extraction_over_coordination, deontological).
narrative_ontology:cs_reference_frame('a690c475-f6c2-4113-878d-def51b2f7909', debtor_fiscal_discipline_gate).
narrative_ontology:cs_drift_state('a690c475-f6c2-4113-878d-def51b2f7909', contemporary_post_selective_waiver_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a690c475-f6c2-4113-878d-def51b2f7909', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_creditor_institutions).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_strategic_debtor_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, vulnerable_populations_in_subjected_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, regional_competitors_to_hegemons).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__hybrid_selectivity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(structural_adjustment_conditionalities__hybrid_selectivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 at the regime's start (1982) to 0.71 by interval end. This trajectory reflects the historical shift: initially, conditionalities had a genuine coordination rationale (restore debtor fiscal capacity so debt could be serviced without default). As capital markets normalized and many debtors repaid or refinanced, the founding problem lost urgency. But conditionalities persisted and became harder, converting from coordination to extraction. The selective-waiver pattern (observable from the 1990s onward, accelerating post-2000) proves the extraction reading: conditions are dropped for strategic states without any demonstrated fiscal deterioration or loss of market confidence. This proves conditions were never technically necessary for stability. Theater_ratio rises from 0.22 to 0.44 because the regime's public justification (universal discipline for stability) increasingly contradicts the private practice (selective waivers based on geopolitics). Suppression rises from 0.65 to 0.78 because the enforcement machinery hardens: debt crisis triggers are orchestrated (ratings downgrades, capital flight) to force compliance, and alternative lenders (China, regional banks) are actively excluded from negotiations to prevent debtors from shopping for softer terms. The shared time grid (40 points) allows measurement on every examined time point for every tracked metric.
 *
 * PERSPECTIVAL GAP:
 *   The IMF/World Bank and creditor-state seats compute the regime as rope or coordination: they see a genuine problem (fiscal discipline essential for stability), they provide real value (market confidence signals, debt restructuring expertise), and they enforce impartially (the rules apply to all). From non-strategic debtor seats, the same structure computes as snare or tangled_rope with asymmetric extraction: the coordination function is real but separated from the enforcement; strategic states get coordination benefits without extraction, while non-strategic states get extraction without coordination. From the geopolitically-favored-state seat, the regime is a hybrid: real coordination benefits (austerity is optional) plus asymmetric capture (rivals are locked out). The engine's per-seat classification captures this perspectival divergence from the structural data alone.
 *
 * DIRECTIONALITY LOGIC:
 *   IMF/World Bank sit as institutional agenda-setters with arbitrage exit (they set the terms and can waive them selectively); their directionality is near the beneficiary end (d ~ 0.15) because they control the coordination frame and collect enforcement authority. Core creditors sit as institutional beneficiaries with arbitrage exit (they profit from debt service and origination fees); their d is also low (~0.15), they collect without running operations. Hegemon-aligned states sit as institutional power with arbitrage exit (they can negotiate waivers because they matter geopolitically); their d is moderate (~0.45) because they get coordination benefits (market access, IMF seal of approval) without extraction costs. Non-strategic debtor states sit as moderate power with constrained exit (they need the loans, cannot walk away without capital-flight risk); their d is high (~0.78) because they bear the extraction costs (austerity, privatization, tariff shock) with minimal coordination benefit (the discipline imposed on them does not produce better outcomes than waived conditions produce for strategic states, proving the discipline is not coordination-necessary). Vulnerable populations sit as powerless with trapped exit (no borrowing alternative, nowhere to emigrate to without leaving the jurisdiction); their d is at the target end (~0.95) because they carry concrete deprivation costs and have zero alternative. Regional competitors sit as powerful but trapped (they want to lend but are excluded); their d is inverted (~0.20) because the constraint benefits them by locking out debtor alternatives, even though they want in. The directionality overrides section is omitted because the derivation from beneficiary/victim + exit options produces accurate d values without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading frames the mandatrophy question precisely: did conditionalities outlive their founding purpose? The founding problem (1982 debt crisis) was acute—debtor countries could not repay, creditors faced insolvency, capital markets froze. By the 1990s, the acute crisis passed: debtor-country debt-to-GDP ratios normalized, many graduated to voluntary borrowing at market rates, and capital markets recovered. The founding problem moved from live to dead. But conditionalities persisted and were actually intensified in new agreements, proving mandate drift. The selective-waiver evidence clinches the mandatrophy diagnosis: if the founding problem were live (fiscal discipline necessary for stability), waiving conditions for Russia (1998), Egypt (post-2013), or Pakistan (post-2008) would cause measurable market deterioration—capital flight, ratings collapse, debt spiral. No such deterioration occurred. This proves the regime persists not because it solves the founding problem, but because it extracts rents and provides geopolitical insurance. The theater_ratio rise (0.22 to 0.44) is the smoking gun of mandatrophy: the regime maintains the coordination narrative (stable fiscal conditions) while the actual enforcement delivers extraction (selective discipline) and geopolitical sorting (waivers for allies). A piton reading is not appropriate here because beneficiaries exist and benefit substantially (IMF/World Bank collect authority, creditors collect debt service, strategic states collect waivers); a piton would lack concentrated beneficiaries. Instead, this is a tangled_rope with mandatrophy—coordinating on the cover story while extracting from non-strategic debtors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_resolution_date,
    'When did the founding problem (debtor-state fiscal unsustainability requiring external discipline) factually resolve? Is it 1989 (Brady Plan debt reduction), mid-1990s (capital-market recovery), 2000s (emerging-market growth), or ongoing?',
    'Historical analysis of debtor-country debt-to-GDP ratios, voluntary vs. conditionalized borrowing rates, and market risk premiums. If debtors graduated to unconditionalized borrowing while showing no fiscal deterioration, the founding problem is historically resolved.',
    'If the founding problem is dead but conditionalities persist, the constraint certifies as mandatrophy or snare (extraction decoupled from stated purpose). If the founding problem is contested or ongoing, the constraint remains tangled_rope (coordination function live but extraction-biased).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_resolution_date, empirical, 'Whether the founding problem conditionalities were designed to solve has resolved or persists.').

omega_variable(
    selectivity_mechanism_intentionality,
    'Is the selective-waiver pattern (strict conditions for non-strategic states, waived for strategic states) an intentional enforcement design by creditors, or an incidental effect of debtor bargaining power differentials?',
    'Documentary evidence (IMF board minutes, internal memos) showing explicit discussion of condition-setting based on geopolitical importance. Absence of such evidence does not prove non-intentionality (institutional actors routinely avoid explicit written records of selective application), but presence proves intentionality.',
    'If intentional, the constraint is a Snare: the coordination frame is deliberately false cover for extractive selectivity. If incidental, the constraint is Tangled Rope: coordination and extraction coexist, selectivity is emergent institutional behavior, not designed deception.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_mechanism_intentionality, empirical, 'Whether selective enforcement is an intentional institutional design or an emergent artifact of bargaining asymmetry.').

omega_variable(
    alternative_creditor_substitutability,
    'Are alternative creditors (China, regional development banks, bilateral lenders) genuinely available as substitutes for IMF/World Bank lending to non-strategic debtors, or is their capacity/coverage insufficient to provide real alternative terms?',
    'Comparative analysis of lending terms (conditions, interest rates, grace periods) offered by IMF/World Bank vs. alternatives to the same debtor cohort over time. If alternatives offer measurably softer terms for the same fiscal position, credibility-test: do debtors defect? If not, the constraint is more snare (exclusion is active); if yes, alternative genuinely available (constraint is less suppressive).',
    'If alternatives are genuinely available, suppression is lower (~0.50), exit_options for non-strategic debtors shifts from ''constrained'' to ''mobile'', and directionality d drops. If alternatives are locked out or blocked (via creditor-IMF coordination), suppression stays high and the constraint is closer to snare-with-exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_creditor_substitutability, empirical, 'Whether alternative creditors provide a credible exit option for debtor states seeking to avoid IMF conditionalities.').

omega_variable(
    kernel_reading_boundary,
    'Is this hybrid_selectivity_reading structurally distinct from the debtor_extraction_reading, or does selective enforcement collapse the distinction into pure extraction?',
    'Proof by contradiction: if conditionalities produced no coordination benefit even for strategic states (i.e., even waived conditions did not improve market access or reduce borrowing costs), then selectivity proves the entire regime is extraction, and this reading is empirically identical to the extraction reading. If strategic states measurably benefit (lower rates, faster approvals, market certification), the hybrid reading holds distinct from pure extraction.',
    'If this reading collapses into pure extraction, the constraint certifies as Snare for all seats, and the kernel has only two substantive readings (coordination vs. extraction), not three. If the reading holds distinct, it demonstrates how institutional selectivity can create heterogeneous constraint types per seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether selective enforcement constitutes a structurally distinct reading or collapses into pure extraction.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression maintaining non-strategic debtor compliance structural (external barriers: capital-flight risk, ratings downgrades, inability to access capital markets without IMF certification) or internalized (non-strategic debtor governments internalize the discipline narrative and self-censor reformist policies)?',
    'Post-IMF-exit evidence: if a non-strategic debtor withdraws from the conditionality regime and suppression persists (self-censorship of spending, continued austerity, voluntary privatization), suppression is partially internalized. If suppression drops immediately (spending rises, reforms implemented), suppression is primarily structural.',
    'If structural, suppression can be reduced by improving alternative access or reducing creditor coordination. If internalized, suppression persists even if the structural constraint loosens; debtors carry the discipline with them. High internalization makes the constraint''s effective suppression higher than the 0.78 baseline suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the measured suppression is maintained by external barriers or internalized compliance narratives.').

omega_variable(
    knowledge_asymmetry_and_false_summit_risk,
    'The coordination-narrative framing (conditionalities ensure stability, apply universally) is false for non-strategic debtors but true-enough for strategic debtors and creditors. Is this a false summit (a constructed constraint disguised as natural law) or a genuine hybrid coordination/extraction mechanism?',
    'Debtor-government testimony: do non-strategic debtor officials believe the coordination narrative ex-ante (before they discover selectivity), or do they enter negotiations already suspecting extraction? If ex-ante belief (true false summit), conditioning narratives are doing work; if ex-ante skepticism, the false-summit metaphor fails because the target never believed it.',
    'If false summit: the constraint benefits from coordination legitimacy that is factually undeserved; certification as Snare is appropriate. If genuine hybrid: the constraint has real coordination components for some seats and real extraction for others; Tangled Rope certification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_asymmetry_and_false_summit_risk, conceptual, 'Whether the coordination narrative constitutes false-summit cover or a genuinely hybrid coordination/extraction mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(stru_tr_t5, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement(stru_tr_t10, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(stru_tr_t15, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(stru_tr_t20, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(stru_tr_t25, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(stru_tr_t30, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement(stru_tr_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(stru_be_t5, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(stru_be_t10, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(stru_be_t15, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(stru_be_t20, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(stru_be_t25, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(stru_be_t30, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement(stru_be_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(stru_su_t5, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(stru_su_t10, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement(stru_su_t15, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(stru_su_t20, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(stru_su_t25, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 25, 0.77).
narrative_ontology:measurement(stru_su_t30, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(stru_su_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__hybrid_selectivity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.12).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities__creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities__debtor_extraction_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, capital_account_liberalization_gate).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, debt_restructuring_selectivity).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, trade_agreement_asymmetry).

% DUAL FORMULATION NOTE:
% This is one of three readings of the structural_adjustment_conditionalities kernel. The hybrid_selectivity_reading asserts that conditionalities coordinate on paper but functionally extract from non-strategic debtors while benefiting strategic states through selective waivers. The creditor_coordination_reading (sibling) emphasizes the genuine coordination and risk-pooling function; the debtor_extraction_reading (sibling) asserts pure extraction with no real coordination. All three readings share the same underlying institutional arrangements but have different ε-values and structural interpretations. The three readings' empirical interaction tests whether coordination or extraction is the primary function and whether selectivity proves extraction (this reading) or rational differentiation (sibling creditor reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
