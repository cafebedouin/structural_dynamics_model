% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__orthodox_price_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: ecb_mandate_article_127__orthodox_price_stability
 *   human_readable: ECB Mandate: Orthodox Price Stability Focus
 *   domain: monetary_policy/institutional_governance/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the orthodox reading of the European Central
 *   Bank's (ECB) mandate under Article 127 of the Treaty on the Functioning
 *   of the European Union (TFEU), which requires an exclusive focus on
 *   maintaining price stability (defined as 2% inflation). Under this
 *   reading, any secondary objectives, such as supporting general economic
 *   policies of the Union (e.g., employment, growth, climate action), are
 *   strictly subordinate and non-operational, to be pursued only 'without
 *   prejudice' to the primary price stability objective. This interpretation
 *   actively suppresses alternative readings that would grant more
 *   operational weight to secondary objectives or integrate new concerns like
 *   climate risk.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, 0.68).
domain_priors:suppression_score(ecb_mandate_article_127__orthodox_price_stability, 0.75).
domain_priors:theater_ratio(ecb_mandate_article_127__orthodox_price_stability, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, extractiveness, 0.68).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__orthodox_price_stability, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__orthodox_price_stability, "ECB Mandate: Orthodox Price Stability Focus").
narrative_ontology:topic_domain(ecb_mandate_article_127__orthodox_price_stability, "monetary_policy/institutional_governance/constitutional_law").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__orthodox_price_stability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__orthodox_price_stability, 'dd69b015-4c62-4d2c-b57f-9d18ef1d6b34').
narrative_ontology:cs_kernel_codification('dd69b015-4c62-4d2c-b57f-9d18ef1d6b34', fixed_text).
narrative_ontology:cs_authority_grounding('dd69b015-4c62-4d2c-b57f-9d18ef1d6b34', lineage).
narrative_ontology:cs_interpretation_layer_present('dd69b015-4c62-4d2c-b57f-9d18ef1d6b34').
narrative_ontology:cs_reading_relation('dd69b015-4c62-4d2c-b57f-9d18ef1d6b34', ecb_mandate_article_127__expansive_secondary_objectives, forecloses).
narrative_ontology:cs_reading_relation('dd69b015-4c62-4d2c-b57f-9d18ef1d6b34', ecb_mandate_article_127__climate_incorporation, forecloses).
narrative_ontology:cs_axiom('dd69b015-4c62-4d2c-b57f-9d18ef1d6b34', foundational, price_stability_singular_mandate).
narrative_ontology:cs_axiom_status(price_stability_singular_mandate, holdable).
narrative_ontology:cs_axiom_grounding('dd69b015-4c62-4d2c-b57f-9d18ef1d6b34', price_stability_singular_mandate, conventional).
narrative_ontology:cs_axiom('dd69b015-4c62-4d2c-b57f-9d18ef1d6b34', foundational, secondary_objectives_non_operational).
narrative_ontology:cs_axiom_status(secondary_objectives_non_operational, holdable).
narrative_ontology:cs_axiom_grounding('dd69b015-4c62-4d2c-b57f-9d18ef1d6b34', secondary_objectives_non_operational, conventional).
narrative_ontology:cs_reference_frame('dd69b015-4c62-4d2c-b57f-9d18ef1d6b34', maastricht_treaty_original_intent).
narrative_ontology:cs_drift_state('dd69b015-4c62-4d2c-b57f-9d18ef1d6b34', contemporary_mandate_challenges, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('dd69b015-4c62-4d2c-b57f-9d18ef1d6b34', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, savers_and_creditors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, financial_markets).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, unemployed_workers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, climate_vulnerable_sectors).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, member_states_with_fiscal_stress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the ECB's mandate, prioritizing price stability as the primary objective. Actively defends this orthodox interpretation against calls for mandate expansion, viewing it as essential for credibility and legal consistency. Benefits from the clarity and focus this interpretation provides.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from low and stable inflation, which preserves the real value of their assets and income. They are a key constituency for the orthodox interpretation, as inflation erodes their purchasing power and investment returns.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, savers_and_creditors, beneficiary,
    organized, biographical, mobile, national).

% Benefit from the predictability and stability that a clear, singular price stability mandate provides. This reduces uncertainty and allows for more efficient pricing of assets and risk, even if it means less accommodation for other objectives.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, financial_markets, beneficiary,
    institutional, immediate, arbitrage, global).

% Bear the costs of an exclusive focus on price stability if it leads to tighter monetary policy that dampens economic growth and job creation. They have limited direct influence on ECB policy and few exit options from its effects.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, unemployed_workers, payer,
    powerless, immediate, trapped, national).

% Bear the costs of the ECB's non-integration of climate risks into its operational framework (e.g., asset purchases, collateral rules). They advocate for the ECB to actively support climate transition, but this reading of the mandate suppresses such actions.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, climate_vulnerable_sectors, payer,
    organized, generational, constrained, regional).

% Experience constraints on their fiscal policy space when monetary policy is exclusively focused on price stability, especially during economic downturns. They often advocate for a more expansive interpretation of secondary objectives to support growth and employment.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, member_states_with_fiscal_stress, payer,
    institutional, biographical, constrained, national).

% Advocate for the ECB to actively incorporate climate change into its mandate and operations, citing treaty obligations and systemic financial risks. Their arguments are largely excluded from the operational decision-making of the ECB under this orthodox interpretation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, climate_activists_and_academics, excluded,
    organized, generational, constrained, global).

% Provides legal interpretations that reinforce the orthodox view of price stability as the primary and overriding objective. Their professional identity is tied to defending the legal integrity and independence of the ECB's mandate as currently understood.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_legal_department, agenda_setter,
    institutional, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, singular objective for monetary policy across the Eurozone, coordinating expectations around price stability and ensuring a consistent framework for economic actors.
% TRANSFER_FUNCTION: Transfers the burden of economic stabilization (e.g., employment, growth support) away from monetary policy and onto fiscal policy or other actors, while transferring benefits of stable prices to savers and creditors.
% ABSENT_VOICES: Advocates for a more expansive interpretation of the ECB's secondary objectives, including those focused on employment, growth, and climate action, are structurally marginalized or excluded from operational influence. They would argue for a more balanced approach to the mandate.
% DISAPPEARANCE_RATIONALE: If the orthodox interpretation of the ECB's mandate vanished overnight, the ECB would face immediate pressure to operationalize secondary objectives, leading to a fundamental shift in monetary policy strategy, asset purchase programs, and communication. Financial markets would react to increased uncertainty regarding the ECB's priorities, and political pressure from member states would intensify.
% FOUNDING_PROBLEM: The founding problem was to establish a credible, independent central bank for the Eurozone with a clear mandate to maintain price stability, preventing a return to high inflation experienced in some member states' past.
% FOUNDING_PROBLEM_CORROBORATION: The ECB Governing Council and financial markets attest that maintaining price stability remains a live and critical problem. However, academics and civil society groups argue that while inflation control is important, the singular focus has become disproportionate to other pressing economic and social challenges, making the 'live' status contested in its exclusivity.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__orthodox_price_stability, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__orthodox_price_stability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__orthodox_price_stability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ecb_mandate_article_127__orthodox_price_stability, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__orthodox_price_stability, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.68) because the singular focus on price stability imposes significant costs on other policy objectives and their constituencies, such as higher unemployment or delayed climate action, without direct compensation. Suppression is also high (0.75) due to the active legal and institutional defense of this orthodox interpretation, which limits the ability of other actors to push for mandate expansion or alternative operationalization. The theater ratio is low (0.10) because the ECB genuinely pursues price stability; the constraint is not primarily performative, but rather a deeply held institutional commitment. Accessibility collapse is high (0.70) as alternative policy approaches are difficult to pursue within this mandate, and resistance is moderate (0.55) from those advocating for broader objectives.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the ECB Governing Council, this constraint is a necessary Rope, coordinating expectations and ensuring the credibility of the Euro. From the perspective of unemployed workers or climate activists, it operates as a Snare, extracting from them by prioritizing a narrow objective over broader societal welfare, and actively suppressing alternative policy paths. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The ECB Governing Council and its legal department are agenda-setters and beneficiaries, as this interpretation provides clarity and reinforces their institutional independence. Savers, creditors, and financial markets are direct beneficiaries of stable prices. Unemployed workers, climate-vulnerable sectors, and member states with fiscal stress are victims, bearing the costs of foregone policy options. Climate activists and academics are excluded voices, whose arguments for mandate expansion are suppressed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_127_without_prejudice_ambiguity,
    'Is the ''without prejudice'' clause in Article 127(1) TFEU a genuine authorization for discretionary balancing of secondary objectives, or merely a formal acknowledgement of their existence without operational implication?',
    'Legal rulings from the European Court of Justice or a formal amendment to the TFEU clarifying the operational scope of secondary objectives.',
    'If it authorizes balancing, this reading''s suppression of secondary objectives is less justified, potentially reclassifying it closer to a Tangled Rope or even a Rope if extraction is reduced. If it is merely an acknowledgement, the orthodox reading is legally reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_127_without_prejudice_ambiguity, conceptual, 'Ambiguity of the ''without prejudice'' clause regarding secondary objectives.').

omega_variable(
    article_11_tfeu_climate_integration_scope,
    'Does Article 11 TFEU (environmental integration clause) legally require the ECB to integrate climate considerations into its monetary policy operations, even under a strict interpretation of price stability?',
    'A definitive legal interpretation by the European Court of Justice or a consensus among legal scholars on the binding nature and scope of Article 11 for the ECB.',
    'If Article 11 is found to impose a binding obligation, this reading''s exclusion of climate risks is legally vulnerable, potentially forcing a re-evaluation of its operational framework and reducing its suppressive force on climate-related initiatives. If not, the orthodox reading''s position is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_11_tfeu_climate_integration_scope, conceptual, 'Legal scope of environmental integration clause for ECB mandate.').

omega_variable(
    long_term_costs_of_exclusive_focus,
    'What are the long-term economic and social costs of an exclusive focus on price stability, particularly concerning employment, growth, and climate transition, compared to a more balanced mandate?',
    'Comprehensive, long-term empirical studies and counterfactual analyses comparing economic outcomes under different central bank mandate structures.',
    'If significant long-term costs are empirically demonstrated, the perceived legitimacy and coordination function of this orthodox reading would erode, increasing resistance and potentially shifting its classification towards a Snare due to unacknowledged extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_costs_of_exclusive_focus, empirical, 'Empirical costs of singular price stability focus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__orthodox_price_stability, 1999, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t1999, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 1999, 0.05).
narrative_ontology:measurement(ecb__tr_t2004, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2004, 0.06).
narrative_ontology:measurement(ecb__tr_t2009, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2009, 0.07).
narrative_ontology:measurement(ecb__tr_t2014, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2014, 0.08).
narrative_ontology:measurement(ecb__tr_t2019, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2019, 0.09).
narrative_ontology:measurement(ecb__tr_t2024, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(ecb__be_t1999, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 1999, 0.55).
narrative_ontology:measurement(ecb__be_t2004, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2004, 0.58).
narrative_ontology:measurement(ecb__be_t2009, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2009, 0.6).
narrative_ontology:measurement(ecb__be_t2014, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2014, 0.63).
narrative_ontology:measurement(ecb__be_t2019, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2019, 0.66).
narrative_ontology:measurement(ecb__be_t2024, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t1999, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 1999, 0.6).
narrative_ontology:measurement(ecb__su_t2004, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2004, 0.65).
narrative_ontology:measurement(ecb__su_t2009, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2009, 0.7).
narrative_ontology:measurement(ecb__su_t2014, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2014, 0.72).
narrative_ontology:measurement(ecb__su_t2019, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2019, 0.74).
narrative_ontology:measurement(ecb__su_t2024, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__orthodox_price_stability, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__climate_incorporation).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, eu_fiscal_rules_stability_and_growth_pact).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the ECB's Article 127 mandate, each with different structural properties and implications. This 'orthodox_price_stability' reading emphasizes the singular focus on price stability, contrasting with 'expansive_secondary_objectives' and 'climate_incorporation'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
