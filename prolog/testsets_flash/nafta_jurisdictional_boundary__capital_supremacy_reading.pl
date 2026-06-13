% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__capital_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__capital_supremacy_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nafta_jurisdictional_boundary__capital_supremacy_reading
 *   human_readable: NAFTA Jurisdictional Boundary: Capital Supremacy Reading
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This constraint represents the 'capital supremacy' reading of the NAFTA
 *   jurisdictional boundary, where the trade agreement text is interpreted as
 *   supreme law overriding domestic regulatory standards, and capital
 *   mobility and regulatory harmonization are mandatory treaty obligations.
 *   This reading prioritizes investor rights and free trade over national
 *   sovereignty and domestic public policy, leading to significant extraction
 *   from labor and environmental sectors. The constraint is claimed as a
 *   Snare due to its high extractiveness and suppression, and the
 *   identifiable victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.85).
domain_priors:suppression_score(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.9).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__capital_supremacy_reading, snare).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__capital_supremacy_reading, "NAFTA Jurisdictional Boundary: Capital Supremacy Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__capital_supremacy_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__capital_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__capital_supremacy_reading, 'a4b36010-f1b7-4ba0-a228-5b4d824f5589').
narrative_ontology:cs_kernel_codification('a4b36010-f1b7-4ba0-a228-5b4d824f5589', fixed_text).
narrative_ontology:cs_authority_grounding('a4b36010-f1b7-4ba0-a228-5b4d824f5589', lineage).
narrative_ontology:cs_interpretation_layer_present('a4b36010-f1b7-4ba0-a228-5b4d824f5589').
narrative_ontology:cs_reading_relation('a4b36010-f1b7-4ba0-a228-5b4d824f5589', nafta_jurisdictional_boundary__embedded_liberalism_reading, influences).
narrative_ontology:cs_reading_relation('a4b36010-f1b7-4ba0-a228-5b4d824f5589', nafta_jurisdictional_boundary__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('a4b36010-f1b7-4ba0-a228-5b4d824f5589', foundational, capital_mobility_as_fundamental_right).
narrative_ontology:cs_axiom_status(capital_mobility_as_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('a4b36010-f1b7-4ba0-a228-5b4d824f5589', capital_mobility_as_fundamental_right, conventional).
narrative_ontology:cs_axiom('a4b36010-f1b7-4ba0-a228-5b4d824f5589', foundational, trade_agreement_as_supreme_law).
narrative_ontology:cs_axiom_status(trade_agreement_as_supreme_law, holdable).
narrative_ontology:cs_axiom_grounding('a4b36010-f1b7-4ba0-a228-5b4d824f5589', trade_agreement_as_supreme_law, conventional).
narrative_ontology:cs_reference_frame('a4b36010-f1b7-4ba0-a228-5b4d824f5589', unfettered_capital_movement).
narrative_ontology:cs_drift_state('a4b36010-f1b7-4ba0-a228-5b4d824f5589', contemporary_nationalist_backlash, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('a4b36010-f1b7-4ba0-a228-5b4d824f5589', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, international_investors).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_unions).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, environmental_advocacy_groups).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, national_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, local_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from reduced regulatory hurdles and enhanced capital mobility across borders, allowing them to optimize production and investment without significant domestic interference. They actively lobby for interpretations that prioritize trade and investment over national regulations.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Gain from the protection of their investments against expropriation or regulatory changes that might diminish their value, often through investor-state dispute settlement (ISDS) mechanisms. They can easily shift capital to more favorable regulatory environments.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, international_investors, beneficiary,
    organized, biographical, arbitrage, global).

% Experience their domestic regulatory authority constrained by the supremacy of trade agreement provisions. They must harmonize standards or face challenges, leading to a 'race to the bottom' in environmental and labor protections. Their ability to protect public goods is diminished.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, national_regulatory_agencies, payer,
    institutional, biographical, constrained, national).

% Face downward pressure on wages and working conditions as capital mobility allows corporations to seek out lower-cost labor markets. Their ability to advocate for stronger domestic labor standards is undermined by the threat of capital flight.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_unions, payer,
    organized, generational, trapped, national).

% Struggle to implement and enforce robust environmental protections when these are deemed 'barriers to trade' or 'indirect expropriation' under the agreement. Their efforts are often challenged by corporations using ISDS mechanisms.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, environmental_advocacy_groups, payer,
    moderate, generational, constrained, national).

% Bear the direct consequences of relaxed environmental and labor standards, including pollution, resource depletion, and job insecurity, with little recourse against powerful international actors. Their ability to self-govern and protect local interests is severely curtailed.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, local_communities, payer,
    powerless, generational, trapped, local).

% Interpret and enforce the trade agreement text, often prioritizing investor rights and free trade principles over domestic regulatory autonomy. Their rulings set precedents that further entrench the capital supremacy reading.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, trade_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common legal framework for international trade and investment, reducing transaction costs and legal uncertainty for cross-border economic activity.
% TRANSFER_FUNCTION: Transfers regulatory authority and policy space from national governments and their citizens to international trade tribunals and multinational corporations, facilitating the free movement of capital and goods.
% ABSENT_VOICES: Sovereign citizens and unorganized labor/environmental interests are largely absent from the negotiation and interpretation of these agreements; they would argue for the primacy of democratic control over economic policy and the protection of public goods.
% DISAPPEARANCE_RATIONALE: If this reading of NAFTA vanished, national governments would immediately regain significant policy space, potentially leading to a re-regulation of labor and environmental standards. Multinational corporations would face increased regulatory fragmentation, and capital flows might become more localized, fundamentally altering the global economic landscape.
% FOUNDING_PROBLEM: The founding problem was to reduce barriers to trade and investment between member countries, promoting economic integration and growth by harmonizing regulations and protecting foreign investments.
% FOUNDING_PROBLEM_CORROBORATION: Multinational corporations and international investors attest that the problem of trade barriers and investment risk is still live. Domestic labor unions, environmental groups, and some national governments argue that the original problem has been largely solved, and the agreement's current interpretation primarily serves to extract rents and undermine democratic sovereignty; this is corroborated by academic studies on regulatory chill and ISDS outcomes.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__capital_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__capital_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__capital_supremacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the interpretation of NAFTA consistently favors investor protections and trade liberalization, leading to a transfer of wealth and regulatory power from domestic actors to multinational capital. Suppression is also high (0.90) due to the binding nature of treaty obligations and the enforcement power of international tribunals, which effectively suppress domestic regulatory alternatives and resistance. The theater ratio is low (0.10) because the mechanisms are actively and effectively used to achieve their extractive goals, with little performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   Multinational corporations and international investors experience this constraint as a beneficial Rope or even a Mountain, providing stable and predictable conditions for their operations. In contrast, domestic labor unions, environmental groups, and national regulatory agencies experience it as a Snare, actively undermining their ability to protect local interests and set independent policy. Trade tribunals, as agenda-setters, operate within a framework that reinforces the capital supremacy reading, seeing their role as upholding the 'rule of law' of the agreement.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational corporations and international investors are clear beneficiaries (d near 0.0) as the constraint directly facilitates their operations and protects their assets. Domestic labor unions, environmental groups, national regulatory agencies, and local communities are victims (d near 1.0) as they bear the costs of regulatory chill, weakened protections, and loss of sovereignty. Trade tribunals, while appearing neutral, act as agenda-setters whose interpretations consistently reinforce the extractive aspects of this reading, placing them closer to the beneficiary end.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling what is presented as a coordination mechanism (reducing trade barriers) as a pure Rope. By identifying the high extractiveness, suppression, and clear victims, it highlights how the 'capital supremacy' reading has transformed a trade agreement into a Snare that actively undermines domestic regulatory autonomy for the benefit of international capital. The persistence of the founding problem as 'contested' further indicates that the constraint's function has drifted from its original mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretation_vs_text,
    'To what extent is the ''capital supremacy'' reading inherent in the NAFTA text itself, versus being a product of specific interpretive practices by trade tribunals and legal scholars?',
    'Comparative legal analysis of similar trade agreements with different interpretive histories, or a re-negotiation of the text with explicit clauses limiting investor-state dispute settlement (ISDS) scope.',
    'If inherent, the constraint is a more robust Snare, requiring fundamental renegotiation. If primarily interpretive, a shift in judicial philosophy or a re-framing of legal arguments could reduce its extractiveness without altering the core text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_vs_text, conceptual, 'Ambiguity between textual determinism and interpretive agency in shaping the constraint.').

omega_variable(
    regulatory_chill_quantification,
    'What is the quantifiable impact of ''regulatory chill'' (governments refraining from enacting regulations due to fear of ISDS claims) on domestic labor and environmental standards?',
    'Empirical studies tracking proposed vs. enacted regulations in member states, correlated with ISDS claim frequency and outcomes, and expert surveys of regulatory agencies.',
    'Higher quantifiable chill would increase the measured suppression and extractiveness, solidifying the Snare classification. Lower chill would suggest more policy space remains than currently perceived, potentially shifting the classification towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_chill_quantification, empirical, 'Quantifying the chilling effect of ISDS on domestic regulation.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reading of the ''nafta_jurisdictional_boundary'' kernel, or an overreach that fundamentally distorts the original intent?',
    'Historical analysis of negotiating documents, original intent arguments from treaty drafters, and a comparison with the ''embedded_liberalism_reading'' and ''sovereignty_primacy_reading'' to identify the point of divergence.',
    'If it''s an overreach, the constraint''s legitimacy is severely undermined, potentially leading to its repudiation or re-interpretation. If it''s a defensible reading, the contest is over fundamental values, not interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''nafta_jurisdictional_boundary'' kernel, where trade agreement text is supreme law overriding domestic regulatory standards, and capital mobility and regulatory harmonization are mandatory treaty obligations. Sibling readings (''embedded_liberalism_reading'', ''sovereignty_primacy_reading'') would shift the balance towards domestic policy space or national sovereignty, respectively, altering the beneficiary/victim structure and extractiveness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__capital_supremacy_reading, 1994, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 1994, 0.2).
narrative_ontology:measurement(naft_tr_t2000, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(naft_tr_t2006, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2006, 0.15).
narrative_ontology:measurement(naft_tr_t2012, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2012, 0.12).
narrative_ontology:measurement(naft_tr_t2020, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 1994, 0.7).
narrative_ontology:measurement(naft_be_t2000, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(naft_be_t2006, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2006, 0.8).
narrative_ontology:measurement(naft_be_t2012, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2012, 0.83).
narrative_ontology:measurement(naft_be_t2020, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t1994, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 1994, 0.75).
narrative_ontology:measurement(naft_su_t2000, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(naft_su_t2006, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2006, 0.85).
narrative_ontology:measurement(naft_su_t2012, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2012, 0.88).
narrative_ontology:measurement(naft_su_t2020, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2020, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__capital_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, global_supply_chain_optimization).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_environmental_regulations).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_laws).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'nafta_jurisdictional_boundary' kernel. This 'capital supremacy' reading emphasizes the binding nature of trade obligations over domestic law, leading to higher extraction and suppression compared to the 'embedded liberalism' or 'sovereignty primacy' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
