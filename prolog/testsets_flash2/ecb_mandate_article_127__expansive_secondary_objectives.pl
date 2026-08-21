% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__expansive_secondary_objectives
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__expansive_secondary_objectives, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: ecb_mandate_article_127__expansive_secondary_objectives
 *   human_readable: ECB Mandate Article 127: Expansive Secondary Objectives Reading
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   This constraint represents the 'expansive secondary objectives' reading
 *   of Article 127 of the Treaty on the Functioning of the European Union
 *   (TFEU), which defines the ECB's mandate. This reading interprets the
 *   'without prejudice' clause as granting the ECB operational discretion to
 *   consider employment and growth, provided price stability is not
 *   threatened. It is one reading of the broader 'ecb_mandate_article_127'
 *   kernel, which also includes 'orthodox_price_stability' and
 *   'climate_incorporation' readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, 0.35).
domain_priors:suppression_score(ecb_mandate_article_127__expansive_secondary_objectives, 0.45).
domain_priors:theater_ratio(ecb_mandate_article_127__expansive_secondary_objectives, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, extractiveness, 0.35).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__expansive_secondary_objectives, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__expansive_secondary_objectives, "ECB Mandate Article 127: Expansive Secondary Objectives Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__expansive_secondary_objectives, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__expansive_secondary_objectives).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__expansive_secondary_objectives, 'd96da73a-5bc8-4b69-99ef-130c7b92e783').
narrative_ontology:cs_kernel_codification('d96da73a-5bc8-4b69-99ef-130c7b92e783', fixed_text).
narrative_ontology:cs_authority_grounding('d96da73a-5bc8-4b69-99ef-130c7b92e783', lineage).
narrative_ontology:cs_interpretation_layer_present('d96da73a-5bc8-4b69-99ef-130c7b92e783').
narrative_ontology:cs_reading_relation('d96da73a-5bc8-4b69-99ef-130c7b92e783', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('d96da73a-5bc8-4b69-99ef-130c7b92e783', ecb_mandate_article_127__climate_incorporation, coexists_with).
narrative_ontology:cs_axiom('d96da73a-5bc8-4b69-99ef-130c7b92e783', foundational, discretionary_balancing_permitted).
narrative_ontology:cs_axiom_status(discretionary_balancing_permitted, holdable).
narrative_ontology:cs_axiom_grounding('d96da73a-5bc8-4b69-99ef-130c7b92e783', discretionary_balancing_permitted, conventional).
narrative_ontology:cs_axiom('d96da73a-5bc8-4b69-99ef-130c7b92e783', foundational, secondary_objectives_operational).
narrative_ontology:cs_axiom_status(secondary_objectives_operational, holdable).
narrative_ontology:cs_axiom_grounding('d96da73a-5bc8-4b69-99ef-130c7b92e783', secondary_objectives_operational, conventional).
narrative_ontology:cs_reference_frame('d96da73a-5bc8-4b69-99ef-130c7b92e783', broad_economic_support_mandate).
narrative_ontology:cs_drift_state('d96da73a-5bc8-4b69-99ef-130c7b92e783', contemporary_eurozone_crises, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d96da73a-5bc8-4b69-99ef-130c7b92e783', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, eu_citizens_employment).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, eu_member_states_growth).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, inflation_hawks).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, fiscal_conservatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and implements the ECB mandate, balancing price stability with secondary objectives. This reading grants them discretion to consider employment and growth, allowing for more flexible policy responses within the treaty framework.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council, agenda_setter,
    institutional, generational, constrained, continental).

% Benefit from monetary policies that explicitly consider employment levels, potentially leading to lower unemployment and better labor market conditions. Their influence is indirect, through political representation and public discourse.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eu_citizens_employment, beneficiary,
    organized, biographical, constrained, continental).

% Benefit from policies that support economic growth, especially during downturns, by allowing the ECB to consider broader economic conditions beyond just inflation. This provides fiscal space and reduces pressure on national budgets.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eu_member_states_growth, beneficiary,
    institutional, generational, constrained, continental).

% Advocate for a strict, singular focus on price stability, viewing any consideration of secondary objectives as a dilution of the ECB's primary mandate. They bear the 'cost' of perceived policy deviation and potential future inflation risk.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, inflation_hawks, payer,
    powerful, biographical, constrained, national).

% Prefer strict monetary policy to avoid moral hazard and maintain fiscal discipline among member states. They see expansive secondary objectives as potentially enabling looser fiscal policy and increased public debt.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, fiscal_conservatives, payer,
    powerful, biographical, constrained, national).

% Provide theoretical and empirical support for the view that central banks should consider broader macroeconomic objectives, especially when inflation is low. They influence policy debates through research and public commentary.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, academic_economists_doves, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates monetary policy across the Eurozone to achieve both price stability and support for general economic policies, particularly employment and growth, when the primary objective is not threatened. This allows for a more holistic response to economic challenges.
% TRANSFER_FUNCTION: Transfers policy discretion and influence from a narrow price stability focus to a broader set of macroeconomic objectives, potentially shifting benefits towards labor markets and economic growth, and costs to those prioritizing strict inflation control.
% ABSENT_VOICES: Citizens and businesses in member states facing high unemployment or low growth would strongly advocate for this expansive reading, but their direct voice in ECB decision-making is limited to indirect political channels.
% DISAPPEARANCE_RATIONALE: If this expansive reading of the secondary objectives vanished, the ECB would likely revert to a more rigid, singular focus on price stability. This would significantly alter policy responses to economic downturns, potentially leading to higher unemployment and slower growth, forcing national governments to bear more of the stabilization burden.
% FOUNDING_PROBLEM: The original mandate sought to balance the need for an independent central bank focused on price stability with the broader economic goals of the European Union, recognizing that monetary policy has implications beyond inflation.
% FOUNDING_PROBLEM_CORROBORATION: The ECB Governing Council and many EU member states attest that balancing price stability with employment and growth remains a live and critical challenge, especially in diverse economic conditions across the Eurozone. Academic economists (doves) also corroborate the ongoing relevance of this balancing act.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__expansive_secondary_objectives, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__expansive_secondary_objectives, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__expansive_secondary_objectives, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ecb_mandate_article_127__expansive_secondary_objectives, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).
:- end_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates monetary policy for broader economic welfare (beneficiaries: EU citizens, member states) but also involves asymmetric extraction (victims: inflation hawks, fiscal conservatives) who perceive a dilution of the primary mandate. Extractiveness is moderate (0.35) as it shifts policy focus, but not a direct financial transfer. Suppression (0.45) is moderate, reflecting the ongoing political and legal debates that challenge this interpretation, but the ECB's institutional power allows it to enforce this reading. Theater ratio is low (0.20) as the consideration of secondary objectives is genuinely operational, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   The ECB Governing Council and beneficiaries of this reading perceive it as a necessary and legitimate interpretation for effective monetary policy in a complex economy. Conversely, those who prioritize strict price stability view it as an overreach or a dangerous precedent, leading to a divergence in how the constraint's legitimacy and effects are perceived.
 *
 * DIRECTIONALITY LOGIC:
 *   The ECB Governing Council (agenda_setter) benefits from increased policy flexibility. EU citizens (employment) and member states (growth) are beneficiaries as their concerns are explicitly considered. Inflation hawks and fiscal conservatives are 'victims' in the sense that their preferred, narrower interpretation of the mandate is suppressed, leading to policy outcomes they oppose. Academic economists (doves) are observers who provide analytical support for this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling coordination as pure extraction by acknowledging the genuine coordination function of supporting broader EU economic policy. However, the 'without prejudice' clause's ambiguity means its persistence is subject to ongoing contestation, preventing it from being a pure Rope. It's a Tangled Rope because the coordination comes with a cost for those who prefer a narrower mandate, requiring active enforcement of this interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretionary_scope_ambiguity,
    'What is the precise operational boundary of ''without prejudice to the objective of price stability'' when considering employment and growth?',
    'Further legal clarification from the European Court of Justice or explicit legislative amendment to the TFEU, or a sustained period of ECB policy where secondary objectives are pursued without any perceived threat to price stability.',
    'A narrower interpretation would increase extractiveness for beneficiaries of this reading and push the constraint towards ''orthodox_price_stability''. A broader interpretation would solidify this reading and potentially reduce perceived extraction from ''inflation_hawks''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretionary_scope_ambiguity, conceptual, 'Ambiguity regarding the extent of ECB''s discretion in pursuing secondary objectives.').

omega_variable(
    price_stability_threat_definition,
    'How is ''threat to price stability'' defined and measured in practice, and who adjudicates this definition?',
    'Formalized, quantitative criteria for ''threat'' agreed upon by the Governing Council and publicly communicated, or a clear ruling from an external legal body on the interpretation of ''threat''.',
    'A strict, easily triggered definition of ''threat'' would limit the operational space for secondary objectives, increasing extraction for employment/growth beneficiaries. A flexible definition would empower this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(price_stability_threat_definition, empirical, 'The operational definition of when price stability is ''threatened'' is contested.').

omega_variable(
    reading_legitimacy_contest,
    'Is this ''expansive secondary objectives'' reading a legitimate interpretation of the ECB mandate, or an overreach of its powers?',
    'A definitive ruling from the European Court of Justice on the scope of Article 127, or a broad political consensus among EU member states and institutions endorsing this interpretation.',
    'If deemed an overreach, the constraint would shift towards a Snare for the beneficiaries of this reading, as its legitimacy would be undermined. If fully legitimized, it would move closer to a Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_legitimacy_contest, conceptual, 'The fundamental legitimacy of this expansive reading is contested by other interpretations of the mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__expansive_secondary_objectives, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ecb__tr_t5, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 5, 0.15).
narrative_ontology:measurement(ecb__tr_t10, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 10, 0.18).
narrative_ontology:measurement(ecb__tr_t15, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 15, 0.19).
narrative_ontology:measurement(ecb__tr_t20, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(ecb__be_t5, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(ecb__be_t10, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(ecb__be_t15, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 15, 0.34).
narrative_ontology:measurement(ecb__be_t20, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ecb__su_t5, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(ecb__su_t10, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 10, 0.43).
narrative_ontology:measurement(ecb__su_t15, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(ecb__su_t20, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 20, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
