% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__orthodox_price_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__orthodox_price_stability, []).

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
 *   constraint_id: ecb_mandate_article_127__orthodox_price_stability
 *   human_readable: ECB Mandate Article 127 — Orthodox Price Stability Reading
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   The ECB's mandate under Article 127 TFEU establishes price stability as
 *   the primary objective, with secondary objectives (supporting general
 *   economic policies) pursued 'without prejudice to price stability.' The
 *   orthodox reading treats this as an exclusive, lexicographic hierarchy:
 *   the 2% inflation target is the sole operational objective; secondary
 *   objectives are rhetorical, not operational. This reading has been the
 *   ECB's self-declared framework since 1999. However, the ECB's actual
 *   practice — especially since the eurozone crisis — has repeatedly operated
 *   beyond this narrow frame (OMT, asset purchases, pandemic emergency
 *   programmes, climate-oriented operations). The constraint story captures
 *   the tension between the declared orthodox frame and the lived
 *   institutional practice, which extracts distributive consequences
 *   (creditor-favoring, climate-externalizing) while suppressing
 *   mandate-expansion alternatives through legal-institutional enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, 0.68).
domain_priors:suppression_score(ecb_mandate_article_127__orthodox_price_stability, 0.82).
domain_priors:theater_ratio(ecb_mandate_article_127__orthodox_price_stability, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, extractiveness, 0.68).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__orthodox_price_stability, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__orthodox_price_stability, "ECB Mandate Article 127 — Orthodox Price Stability Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__orthodox_price_stability, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__orthodox_price_stability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__orthodox_price_stability, '97f41ed5-a1e8-47c1-a3d0-7dd93aaf92f0').
narrative_ontology:cs_kernel_codification('97f41ed5-a1e8-47c1-a3d0-7dd93aaf92f0', fixed_text).
narrative_ontology:cs_authority_grounding('97f41ed5-a1e8-47c1-a3d0-7dd93aaf92f0', lineage).
narrative_ontology:cs_interpretation_layer_present('97f41ed5-a1e8-47c1-a3d0-7dd93aaf92f0').
narrative_ontology:cs_reading_relation('97f41ed5-a1e8-47c1-a3d0-7dd93aaf92f0', ecb_mandate_article_127__expansive_secondary_objectives, forecloses).
narrative_ontology:cs_reading_relation('97f41ed5-a1e8-47c1-a3d0-7dd93aaf92f0', ecb_mandate_article_127__climate_incorporation, forecloses).
narrative_ontology:cs_axiom('97f41ed5-a1e8-47c1-a3d0-7dd93aaf92f0', foundational, price_stability_exclusive_mandate).
narrative_ontology:cs_axiom_status(price_stability_exclusive_mandate, holdable).
narrative_ontology:cs_axiom_grounding('97f41ed5-a1e8-47c1-a3d0-7dd93aaf92f0', price_stability_exclusive_mandate, conventional).
narrative_ontology:cs_axiom('97f41ed5-a1e8-47c1-a3d0-7dd93aaf92f0', secondary, secondary_objectives_non_operational).
narrative_ontology:cs_axiom_status(secondary_objectives_non_operational, holdable).
narrative_ontology:cs_axiom_grounding('97f41ed5-a1e8-47c1-a3d0-7dd93aaf92f0', secondary_objectives_non_operational, conventional).
narrative_ontology:cs_axiom('97f41ed5-a1e8-47c1-a3d0-7dd93aaf92f0', secondary, inflation_target_symmetric_two_percent).
narrative_ontology:cs_axiom_status(inflation_target_symmetric_two_percent, holdable).
narrative_ontology:cs_axiom_grounding('97f41ed5-a1e8-47c1-a3d0-7dd93aaf92f0', inflation_target_symmetric_two_percent, instrumental).
narrative_ontology:cs_reference_frame('97f41ed5-a1e8-47c1-a3d0-7dd93aaf92f0', treaty_text_price_stability_primacy).
narrative_ontology:cs_drift_state('97f41ed5-a1e8-47c1-a3d0-7dd93aaf92f0', post_pandemic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('97f41ed5-a1e8-47c1-a3d0-7dd93aaf92f0', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, savers_creditors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, ecb_institutional).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, debtors_borrowers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, climate_vulnerable_populations).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, eurozone_governments).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__orthodox_price_stability, price_stability_as_sufficient_condition_for_growth).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__orthodox_price_stability, central_bank_independence_as_credibility_anchor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the monetary policy mandate with operational independence guaranteed by treaty. Sets the 2% inflation target and defines what counts as 'price stability.' Controls the analytical framework (models, projections, communication) that legitimizes policy choices. Collects institutional capital and legitimacy from maintaining the narrow mandate interpretation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_institutional, agenda_setter,
    institutional, generational, arbitrage, continental).

% Hold nominal financial claims (bonds, deposits, pensions) whose real value is protected by low inflation. Benefit from the mandate's exclusive focus on price stability without bearing its adjustment costs. Can diversify across jurisdictions and asset classes; exit is relatively low-cost.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, savers_creditors, beneficiary,
    organized, biographical, mobile, continental).

% Bear higher real debt burdens when inflation runs below target or when monetary tightening raises rates. Include households with mortgages, firms with variable-rate debt, and peripheral sovereigns. Exit options limited: cannot easily leave the eurozone, refinancing depends on ECB policy stance, fiscal constraints bind.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, debtors_borrowers, payer,
    moderate, biographical, constrained, continental).

% Experience climate risks (physical and transition) that the orthodox reading treats as outside the mandate. No voice in ECB governance; climate impacts fall disproportionately on lower-income regions and generations not represented in current policy calculus. Cannot exit the monetary union or the climate exposure.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, climate_vulnerable_populations, payer,
    powerless, generational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__orthodox_price_stability, climate_vulnerable_populations, excluded).

% Face fiscal discipline pressures from monetary policy that prioritizes inflation over growth/employment. Peripheral governments especially constrained by spread dynamics and market discipline amplified by ECB's narrow mandate. Treaty exit is politically prohibitive; fiscal policy space compressed by monetary dominance.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, eurozone_governments, payer,
    institutional, biographical, constrained, national).

% Produce the analytical literature on mandate interpretation, optimal monetary policy, and institutional design. Not directly subject to the constraint's extraction but shape the legitimizing discourse. See the full structural field across readings.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, academic_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Anchors inflation expectations across the eurozone, providing a stable nominal anchor for contracts, wage-setting, and investment decisions. Solves the time-inconsistency problem of discretionary monetary policy by committing to a credible low-inflation regime.
% TRANSFER_FUNCTION: Transfers purchasing power from debtors (households, firms, peripheral sovereigns) to creditors (bondholders, savers, core-country financial sectors) via systematically tighter-than-necessary monetary conditions. Externalizes climate transition costs onto vulnerable populations and future generations by excluding them from the policy calculus.
% ABSENT_VOICES: Climate-vulnerable populations (especially in Global South and future generations) are structurally excluded from ECB governance. Youth and future generations who bear long-term climate costs have no representation. Peripheral eurozone citizens experience the constraint's distributive effects without democratic input into mandate interpretation.
% DISAPPEARANCE_RATIONALE: If the orthodox reading vanished overnight, the ECB would immediately operationalize secondary objectives (employment, climate, financial stability) as co-equal mandates. Asset purchase programs would incorporate green taxonomy; collateral frameworks would price climate risk; rate-setting would explicitly balance inflation against unemployment. The eurozone's macroeconomic policy framework would reorganize around a multi-objective mandate.
% FOUNDING_PROBLEM: The euro's creation required a credible commitment to price stability to overcome German inflation aversion and secure political agreement for monetary union. The orthodox reading was the constitutional bargain: a narrow, rules-like mandate in exchange for surrendering national monetary sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: The founding bargain is attested by the Maastricht Treaty negotiators and the Bundesbank's historical position. However, the 'contested' status reflects that the ECB's own practice since 2010 (OMT, APP, PEPP, TLTROs, climate action) has operationally expanded the mandate beyond the orthodox reading, a shift documented by the European Parliament's monetary dialogue reports, academic literature (e.g., De Grauwe, Wyplosz), and the ECB's own strategy review (2021) — none of which are beneficiaries of the orthodox reading.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__orthodox_price_stability, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__orthodox_price_stability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__orthodox_price_stability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ecb_mandate_article_127__orthodox_price_stability, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__orthodox_price_stability, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the systematic transfer from debtors to creditors via a monetary stance that consistently errs on the side of undershooting rather than overshooting the symmetric target, and the externalization of climate costs. Suppression (0.82) is high because the constraint's persistence depends on the ECB's treaty-guaranteed independence, the Court of Justice's deference to ECB mandate interpretation, and the political impossibility of treaty change — not on participant preference. Theater (0.42) has risen as the ECB's actual operations (green tilting, climate stress tests, strategic reviews) diverge from the orthodox frame while the public communication maintains it. Accessibility collapse (0.71) is high: the treaty text and institutional structure make alternative mandate interpretations structurally difficult to operationalize. Resistance (0.58) is moderate: political contestation exists (European Parliament, national courts, climate litigation) but has not shifted the operational framework.
 *
 * PERSPECTIVAL GAP:
 *   From the ECB institutional seat, the constraint is genuine coordination (credible anchor, solved time-inconsistency). From debtor and climate-vulnerable seats, it is extraction (systematic transfer, externalized costs). The engine computes this divergence from the structural data: the same constraint produces different effective extraction (χ) across seats because directionality (d) differs — near 0 for ECB/savers, near 1 for debtors/climate-vulnerable. The claimed_type (tangled_rope) reflects the author's structural judgment that both coordination and extraction are real; the engine will verify per-seat.
 *
 * DIRECTIONALITY LOGIC:
 *   ECB institutional is the agenda setter with arbitrage-grade exit (it controls the interpretation and faces no credible threat to its independence). Savers/creditors are beneficiaries with mobile exit (diversifiable portfolios, cross-border mobility). Debtors/borrowers are payers with constrained exit (eurozone lock-in, debt contracts denominated in euro). Climate-vulnerable populations are payers and excluded with trapped exit (no voice, no exit, generational impact). Eurozone governments are payers with constrained exit (treaty lock-in, market discipline). Academic observers sit at the analytical seat with analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (credible anti-inflation commitment for euro adoption) was live in 1999. By the 2010s, the disinflationary bias and secular stagnation made the exclusive focus maladaptive; the ECB's own practice (unconventional policies) implicitly recognized this. Yet the orthodox frame persists because it protects the ECB's independence (legitimacy shield) and creditor interests (material benefit). The mandate has not been formally revised — treaty change is blocked — so the arrangement persists as a zombie coordination structure: the original problem is dead/contested, but the constraint extracts via institutional inertia and legal enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_interpretation_naturalness,
    'Is the exclusive price stability reading a genuine textual necessity of Article 127 TFEU, or a constructed interpretation that benefits identifiable agents (ECB institution, creditors)?',
    'Comparative legal analysis of treaty drafting history (travaux préparatoires), CJEU jurisprudence on mandate interpretation, and counterfactual assessment of whether alternative readings were foreclosed by text or by political choice.',
    'If the orthodox reading is a constructed interpretation rather than textual necessity, the constraint is a false summit candidate (mountain claim with beneficiaries) and the extraction is political choice, not legal mandate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_interpretation_naturalness, conceptual, 'Natural-law vs. constructed interpretation of the treaty mandate.').

omega_variable(
    climate_externalization_mechanism,
    'Is the exclusion of climate risks from the mandate a structural feature of the orthodox reading (logically entailed) or a contingent policy choice within the reading''s discretion?',
    'Analyze whether the orthodox reading''s axioms logically require climate exclusion, or whether the ECB''s climate actions (2021 strategy review, green tilting, climate stress tests) demonstrate the reading permits climate incorporation.',
    'If climate exclusion is contingent, the extraction on climate-vulnerable populations is a policy choice, not a mandate necessity — strengthening the tangled_rope classification. If logically entailed, the reading itself generates the extraction structurally.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(climate_externalization_mechanism, conceptual, 'Whether climate exclusion is logically necessary or discretionary under the orthodox reading.').

omega_variable(
    creditor_coalition_stability,
    'How stable is the creditor/saver beneficiary coalition supporting the orthodox reading, given demographic shifts (aging populations, pension fund pressures) and political realignments?',
    'Track political economy coalitions: German ordoliberal consensus, northern eurozone fiscal councils, financial sector lobbying, versus rising debtor-country political pressure and green transition politics.',
    'If the beneficiary coalition fractures, the mandate''s political sustainability weakens, potentially triggering mandate revision or reading shift — affecting the constraint''s persistence mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_coalition_stability, empirical, 'Political stability of the beneficiary coalition maintaining the orthodox reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__orthodox_price_stability, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb_mandate_orthodox_tr_t0, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ecb_mandate_orthodox_tr_t5, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 5, 0.22).
narrative_ontology:measurement(ecb_mandate_orthodox_tr_t10, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 10, 0.31).
narrative_ontology:measurement(ecb_mandate_orthodox_tr_t15, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 15, 0.38).
narrative_ontology:measurement(ecb_mandate_orthodox_tr_t20, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 20, 0.4).
narrative_ontology:measurement(ecb_mandate_orthodox_tr_t25, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 25, 0.41).
narrative_ontology:measurement(ecb_mandate_orthodox_tr_t30, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(ecb_mandate_orthodox_be_t0, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ecb_mandate_orthodox_be_t5, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(ecb_mandate_orthodox_be_t10, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(ecb_mandate_orthodox_be_t15, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(ecb_mandate_orthodox_be_t20, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(ecb_mandate_orthodox_be_t25, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(ecb_mandate_orthodox_be_t30, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ecb_mandate_orthodox_su_t0, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(ecb_mandate_orthodox_su_t5, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 5, 0.78).
narrative_ontology:measurement(ecb_mandate_orthodox_su_t10, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(ecb_mandate_orthodox_su_t15, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 15, 0.81).
narrative_ontology:measurement(ecb_mandate_orthodox_su_t20, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 20, 0.82).
narrative_ontology:measurement(ecb_mandate_orthodox_su_t25, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 25, 0.82).
narrative_ontology:measurement(ecb_mandate_orthodox_su_t30, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 30, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__orthodox_price_stability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__orthodox_price_stability, 0.12).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__climate_incorporation).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, eurozone_fiscal_rules_stability_pact).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, eu_climate_law_2021).

% DUAL FORMULATION NOTE:
% This constraint is one reading in the ecb_mandate_article_127 kernel family. The orthodox reading (this story) claims exclusive price stability focus; the expansive_secondary_objectives reading claims operational discretion for secondary objectives; the climate_incorporation reading claims mandatory climate integration under Article 11 TFEU. The three readings have divergent ε values (this reading: 0.68; expansive: ~0.35; climate: ~0.45) because they structure beneficiaries, victims, and suppression differently. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ecb_mandate_article_127__orthodox_price_stability, institutional, 0.15).
constraint_indexing:directionality_override(ecb_mandate_article_127__orthodox_price_stability, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
