% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__climate_incorporation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-18
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__climate_incorporation, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: ecb_mandate_article_127__climate_incorporation
 *   human_readable: ECB Mandate Climate Incorporation Reading (Article 127 + 11 TFEU)
 *   domain: monetary_policy/constitutional_law/eu_governance
 *
 * SUMMARY:
 *   This constraint instantiates the climate_incorporation reading of the
 *   contested ecb_mandate_article_127 kernel. Under this reading, Article 127
 *   TFEU (price stability mandate) must be interpreted in conjunction with
 *   Article 11 TFEU (environmental integration), obliging the ECB to
 *   incorporate climate risk into asset purchases and collateral frameworks.
 *   The result is a structurally asymmetric arrangement: green transition
 *   sectors receive coordinated financing advantages while fossil fuel
 *   sectors face extraction via collateral haircuts and portfolio tilting.
 *   The constraint is actively enforced through ECB operational rules and
 *   benchmark design, contested by orthodox member states, and pending
 *   judicial review.
 *
 * KEY AGENTS:
 *   - ECB Executive Board (agenda_setter): Operationalizes climate integration within treaty bounds; controls purchase and collateral eligibility.
 *   - Green transition sectors (beneficiary): Receive preferential financing through ECB demand and eligibility.
 *   - EU climate governance (beneficiary): Gains institutional reinforcement from central bank alignment.
 *   - Fossil fuel sector (payer): Bears higher haircuts and reduced programme demand.
 *   - Carbon-intensive borrowers (payer): Face tighter collateral terms and higher refinancing costs.
 *   - Orthodox member states (excluded): Contest competence but are minority voices in operational decisions.
 *   - ECJ (observer): Ultimate arbiter of mandate proportionality and competence boundaries.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, 0.68).
domain_priors:suppression_score(ecb_mandate_article_127__climate_incorporation, 0.72).
domain_priors:theater_ratio(ecb_mandate_article_127__climate_incorporation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, extractiveness, 0.68).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ecb_mandate_article_127__climate_incorporation, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__climate_incorporation, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__climate_incorporation, "ECB Mandate Climate Incorporation Reading (Article 127 + 11 TFEU)").
narrative_ontology:topic_domain(ecb_mandate_article_127__climate_incorporation, "monetary_policy/constitutional_law/eu_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__climate_incorporation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__climate_incorporation, '49755096-e6f6-4b48-80e9-f7824a1c7e9a').
narrative_ontology:cs_kernel_codification('49755096-e6f6-4b48-80e9-f7824a1c7e9a', formalized).
narrative_ontology:cs_authority_grounding('49755096-e6f6-4b48-80e9-f7824a1c7e9a', lineage).
narrative_ontology:cs_interpretation_layer_present('49755096-e6f6-4b48-80e9-f7824a1c7e9a').
narrative_ontology:cs_reading_relation('49755096-e6f6-4b48-80e9-f7824a1c7e9a', ecb_mandate_article_127__orthodox_price_stability, forecloses).
narrative_ontology:cs_reading_relation('49755096-e6f6-4b48-80e9-f7824a1c7e9a', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_axiom('49755096-e6f6-4b48-80e9-f7824a1c7e9a', foundational, environmental_integration_obligatory).
narrative_ontology:cs_axiom_status(environmental_integration_obligatory, holdable).
narrative_ontology:cs_axiom_grounding('49755096-e6f6-4b48-80e9-f7824a1c7e9a', environmental_integration_obligatory, conventional).
narrative_ontology:cs_axiom('49755096-e6f6-4b48-80e9-f7824a1c7e9a', foundational, climate_risk_material_to_price_stability).
narrative_ontology:cs_axiom_status(climate_risk_material_to_price_stability, holdable).
narrative_ontology:cs_axiom_grounding('49755096-e6f6-4b48-80e9-f7824a1c7e9a', climate_risk_material_to_price_stability, empirically_contingent).
narrative_ontology:cs_reference_frame('49755096-e6f6-4b48-80e9-f7824a1c7e9a', environmentally_integrated_monetary_policy).
narrative_ontology:cs_drift_state('49755096-e6f6-4b48-80e9-f7824a1c7e9a', post_climate_strategy_2021, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('49755096-e6f6-4b48-80e9-f7824a1c7e9a', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__climate_incorporation, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, green_transition_sectors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__climate_incorporation, eu_climate_governance).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, fossil_fuel_sector).
narrative_ontology:constraint_victim(ecb_mandate_article_127__climate_incorporation, carbon_intensive_borrowers).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__climate_incorporation, article_11_tfeu_integration).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__climate_incorporation, climate_risk_materiality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates Eurosystem monetary policy and sets asset purchase and collateral frameworks. Since the 2021 strategy review, it integrates climate criteria into corporate bond purchases and collateral haircuts, citing Article 127 TFEU in conjunction with Article 11 TFEU environmental integration. It cannot exit the treaty framework but exercises broad operational discretion within it.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, ecb_executive_board, agenda_setter,
    institutional, generational, constrained, continental).

% Green bond issuers and renewable energy firms benefit from preferential treatment in ECB purchase programmes and collateral frameworks, which compresses their financing costs relative to carbon-intensive peers. Their market access partly depends on continued ECB demand and eligibility criteria.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, green_transition_sectors, beneficiary,
    organized, biographical, constrained, continental).

% The EU climate policy architecture gains institutional reinforcement when the ECB balances sheet policy aligns with environmental objectives. Treaty goals on decarbonization are substantiated by central bank operations, reducing the reliance on fiscal instruments alone.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, eu_climate_governance, beneficiary,
    institutional, generational, constrained, continental).

% Carbon-intensive energy corporations face higher collateral haircuts and reduced or excluded demand from ECB purchase programmes. Portfolio tilting raises their relative refinancing costs within the euro area and strands existing debt held as collateral, even though their global operations provide partial exit.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, fossil_fuel_sector, payer,
    powerful, biographical, constrained, global).

% SMEs and mid-cap firms in carbon-intensive industries face tighter collateral eligibility and higher refinancing costs as ECB climate criteria tighten. They lack the capital market access to bypass ECB-dependent banking channels easily.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, carbon_intensive_borrowers, payer,
    moderate, biographical, constrained, continental).

% Certain member state finance ministries and central banks argue that climate incorporation exceeds the ECB's monetary policy competence and violates the principle of market neutrality. They are structurally in the minority on the Governing Council and excluded from operational decision-making, though they can challenge the constraint via litigation and treaty politics.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, orthodox_member_states, excluded,
    institutional, generational, constrained, continental).

% The European Court of Justice adjudicates competence disputes over ECB mandate interpretation. Its eventual proportionality review will determine whether climate incorporation under Article 127 TFEU is legally valid or constitutes competence overreach, thereby ratifying or invalidating the constraint.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__climate_incorporation, ecj, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__climate_incorporation, green_transition_sectors).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates environmental externalities and transition risk into monetary policy operations to prevent climate-related financial instability and align central bank balance sheet management with EU environmental objectives under Article 11 TFEU.
% TRANSFER_FUNCTION: Moves relative financing cost advantages to green bonds and climate-aligned issuers while shifting refinancing disadvantages and collateral haircuts to carbon-intensive sovereign and corporate debt, via purchase programme eligibility and collateral framework adjustments.
% ABSENT_VOICES: Fossil fuel industry associations and orthodox member state finance ministries argue the integration exceeds monetary policy competence and distorts market neutrality; they are present in litigation and public debate but structurally excluded from ECB Governing Council operational decision-making.
% DISAPPEARANCE_RATIONALE: If the climate incorporation requirement vanished overnight, ECB portfolio composition would revert to climate-neutral benchmarks, green bond spreads would widen as central bank demand receded, carbon-intensive collateral would regain par treatment, and the relative price structure of EU capital markets would shift away from transition financing.
% FOUNDING_PROBLEM: Carbon externalities were systematically mispriced in sovereign and corporate debt markets financed by the ECB; the central bank's balance sheet was accumulating transition-risk exposure and indirectly subsidizing high-carbon issuance through neutral collateral and purchase rules.
% FOUNDING_PROBLEM_CORROBORATION: Climate finance researchers and the EU Commission attest the mispricing problem is live and requires central bank action. Orthodox central bankers, some national constitutional courts, and fossil fuel sector economists attest the problem is real but belongs to fiscal and regulatory policy, not monetary policy; independent academic literature documents stranded asset risk but disputes whether monetary policy integration is the appropriate remedy.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__climate_incorporation, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__climate_incorporation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__climate_incorporation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ecb_mandate_article_127__climate_incorporation, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__climate_incorporation, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__climate_incorporation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__climate_incorporation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__climate_incorporation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint scores as tangled_rope because it carries a genuine coordination functionâaddressing climate risk mispricing and stranded asset exposure in ECB portfoliosâwhile simultaneously producing asymmetric extraction that falls disproportionately on carbon-intensive issuers. Extractiveness (0.68) reflects the material financing cost divergence between green and brown assets; suppression (0.72) captures the active enforcement through portfolio tilting and eligibility exclusion; theater_ratio (0.42) acknowledges that some green bond purchases serve signalling functions beyond pure risk adjustment. The temporal series trace the post-2021 strategy review ratchet, with extraction and suppression intensifying as criteria tighten.
 *
 * PERSPECTIVAL GAP:
 *   From the ECB and green sectors, the constraint is lawful treaty implementation and prudential risk management. From fossil fuel sectors and orthodox states, it is competence overreach and politically motivated extraction. The engine computes this divergence from the same structural data: the agenda_setter and beneficiaries derive low directionality (subsidy/coordination), while the payer seats derive high directionality (extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   The ECB Executive Board sits near the beneficiary end of the directionality spectrumânot because it collects rents, but because the constraint expands its operational mandate and discretion. Green sectors are clear beneficiaries (low d). Fossil fuel sectors and carbon-intensive borrowers are structural targets (high d), amplified by their constrained exit options (stranded assets, dependence on bank intermediation). Orthodox member states are excluded from operational gains but are not direct payers; their directionality falls toward the fallback. The ECJ occupies the analytical seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents misidentifying the constraint as a pure rope (which would ignore the asymmetric collateral haircuts and portfolio tilting that actively disadvantage fossil fuels) or as a pure snare (which would deny the genuine coordination function of pricing climate risk into financial stability frameworks). The mandate has not atrophied into a piton because the coordination function remains operationally active and contested, with real resources at stake rather than merely theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does Article 127 TFEU structurally permit the climate incorporation reading, or does the orthodox_price_stability reading foreclose it as a matter of treaty interpretation?',
    'ECJ proportionality and competence ruling on ECB climate integration; treaty travaux and linguistic analysis of ''without prejudice'' in Article 127(1).',
    'If the orthodox reading is juridically correct, this constraint collapses into a snare or piton (unauthorized extraction under the guise of mandate interpretation); if this reading is correct, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The kernel reading contest determines whether the constraint''s authority is legitimate or usurped.').

omega_variable(
    portfolio_tilting_suppression_ambiguity,
    'Is portfolio tilting a neutral prudential risk-management tool, or an active suppression mechanism that structurally disadvantages carbon-intensive financing independent of risk?',
    'Comparative financial analysis of ECB haircut schedules against market-implied climate risk metrics; divergence indicates suppression beyond risk pricing.',
    'If tilting exceeds risk-neutral pricing, effective extraction is higher than the base metric suggests and the coordination story weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(portfolio_tilting_suppression_ambiguity, empirical, 'Whether portfolio tilting tracks risk or enforces suppression.').

omega_variable(
    climate_risk_materiality,
    'Is climate risk genuinely material to the ECB''s price stability and financial stability objectives, or is the risk materiality claim a constructed justification for policy preferences?',
    'Ex-post analysis of financial losses in carbon-intensive ECB portfolios during transition shocks; comparison with green portfolio performance.',
    'If climate risk is not material to price stability, the coordination function dissolves and the constraint reads as pure extraction via mandate expansion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_risk_materiality, empirical, 'Empirical foundation of the climate-risk coordination claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__climate_incorporation, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__climate_incorporation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ecb__tr_t12, ecb_mandate_article_127__climate_incorporation, theater_ratio, 12, 0.28).
narrative_ontology:measurement(ecb__tr_t24, ecb_mandate_article_127__climate_incorporation, theater_ratio, 24, 0.33).
narrative_ontology:measurement(ecb__tr_t36, ecb_mandate_article_127__climate_incorporation, theater_ratio, 36, 0.38).
narrative_ontology:measurement(ecb__tr_t48, ecb_mandate_article_127__climate_incorporation, theater_ratio, 48, 0.4).
narrative_ontology:measurement(ecb__tr_t60, ecb_mandate_article_127__climate_incorporation, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ecb__be_t12, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(ecb__be_t24, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(ecb__be_t36, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 36, 0.6).
narrative_ontology:measurement(ecb__be_t48, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 48, 0.65).
narrative_ontology:measurement(ecb__be_t60, ecb_mandate_article_127__climate_incorporation, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ecb__su_t12, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(ecb__su_t24, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(ecb__su_t36, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 36, 0.65).
narrative_ontology:measurement(ecb__su_t48, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 48, 0.7).
narrative_ontology:measurement(ecb__su_t60, ecb_mandate_article_127__climate_incorporation, suppression_requirement, 60, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__climate_incorporation, resource_allocation).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__climate_incorporation, expansive_secondary_objectives).

% DUAL FORMULATION NOTE:
% The ecb_mandate_article_127 kernel decomposes into three structurally distinct constraints because the same treaty text emits different beneficiary/victim structures, Îµ values, and enforcement mechanisms under each reading. Climate incorporation treats Art 11 TFEU as obligatory; orthodox treats secondary objectives as inoperable; expansive treats them as discretionary. These are not observables of the same constraint but different constraints linked by shared source text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
