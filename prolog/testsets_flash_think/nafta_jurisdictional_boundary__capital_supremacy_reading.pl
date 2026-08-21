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
 *   constraint_id: nafta_jurisdictional_boundary__capital_supremacy_reading
 *   human_readable: NAFTA Jurisdictional Boundary: Capital Supremacy Reading
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This constraint represents the 'capital supremacy' reading of the NAFTA
 *   jurisdictional boundary, where the trade agreement text is interpreted as
 *   supreme law overriding domestic regulatory standards, and capital
 *   mobility and regulatory harmonization are mandatory treaty obligations.
 *   This reading prioritizes the interests of multinational capital and
 *   international investors, often at the expense of domestic regulatory
 *   autonomy, labor rights, and environmental protection. The high
 *   extractiveness and suppression reflect the structural power imbalance
 *   embedded in this interpretation.
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
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__capital_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__capital_supremacy_reading, "NAFTA Jurisdictional Boundary: Capital Supremacy Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__capital_supremacy_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__capital_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__capital_supremacy_reading, 'bb77fd01-e377-4d97-a9c4-2bcd7df5726a').
narrative_ontology:cs_kernel_codification('bb77fd01-e377-4d97-a9c4-2bcd7df5726a', fixed_text).
narrative_ontology:cs_authority_grounding('bb77fd01-e377-4d97-a9c4-2bcd7df5726a', extraction).
narrative_ontology:cs_interpretation_layer_present('bb77fd01-e377-4d97-a9c4-2bcd7df5726a').
narrative_ontology:cs_reading_relation('bb77fd01-e377-4d97-a9c4-2bcd7df5726a', nafta_jurisdictional_boundary__embedded_liberalism_reading, forecloses).
narrative_ontology:cs_reading_relation('bb77fd01-e377-4d97-a9c4-2bcd7df5726a', nafta_jurisdictional_boundary__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('bb77fd01-e377-4d97-a9c4-2bcd7df5726a', foundational, trade_law_supremacy_over_domestic_law).
narrative_ontology:cs_axiom_status(trade_law_supremacy_over_domestic_law, holdable).
narrative_ontology:cs_axiom_grounding('bb77fd01-e377-4d97-a9c4-2bcd7df5726a', trade_law_supremacy_over_domestic_law, conventional).
narrative_ontology:cs_axiom('bb77fd01-e377-4d97-a9c4-2bcd7df5726a', foundational, unrestricted_capital_mobility_as_economic_imperative).
narrative_ontology:cs_axiom_status(unrestricted_capital_mobility_as_economic_imperative, holdable).
narrative_ontology:cs_axiom_grounding('bb77fd01-e377-4d97-a9c4-2bcd7df5726a', unrestricted_capital_mobility_as_economic_imperative, instrumental).
narrative_ontology:cs_reference_frame('bb77fd01-e377-4d97-a9c4-2bcd7df5726a', unfettered_capital_mobility_framework).
narrative_ontology:cs_drift_state('bb77fd01-e377-4d97-a9c4-2bcd7df5726a', contemporary_global_resistance, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('bb77fd01-e377-4d97-a9c4-2bcd7df5726a', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, international_investors).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, labor_unions).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, environmental_advocates).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, national_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from reduced regulatory hurdles and guaranteed capital mobility across signatory nations. They can leverage treaty provisions, particularly investor-state dispute settlement (ISDS), to challenge domestic laws that might impede their operations or profits, effectively overriding national sovereignty in specific areas.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Benefit from strong protections for foreign investment and the ability to move capital freely across borders, reducing political and regulatory risk. They are empowered to seek compensation from states for policies that diminish the value of their investments, even if those policies serve public interest goals.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, international_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Experience a loss of jurisdictional authority to set and enforce standards (e.g., labor, environmental, health) that might be deemed to impede trade or capital flows. They face the constant threat of legal challenges under treaty provisions, leading to regulatory chill or self-censorship.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_regulatory_agencies, payer,
    institutional, biographical, constrained, national).

% See domestic labor protections and standards undermined by downward pressure from trade liberalization and capital mobility, as companies seek lower-cost production environments. Their ability to advocate for stronger worker rights is constrained by the perceived need to remain 'competitive' within the trade regime.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, labor_unions, payer,
    organized, biographical, constrained, national).

% Witness domestic environmental standards challenged as non-tariff barriers to trade or as expropriations of foreign investment, leading to a 'race to the bottom' in environmental regulation. Their efforts to promote stronger environmental protections are often subordinated to trade imperatives.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, environmental_advocates, payer,
    organized, biographical, constrained, national).

% Cede significant sovereign regulatory space to international trade tribunals and treaty obligations, limiting their ability to enact domestic policy in areas like public health, environmental protection, and social welfare without risking costly international disputes.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, national_governments, payer,
    institutional, generational, constrained, national).

% Interpret and enforce the trade agreement text, often prioritizing trade and investment liberalization over domestic regulatory autonomy. Their rulings establish precedents that further entrench the supremacy of trade law and capital mobility.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, trade_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To facilitate cross-border trade and investment by establishing a common legal framework, harmonizing regulatory environments, and ensuring predictable legal protections for capital mobility across signatory nations.
% TRANSFER_FUNCTION: Transfers regulatory authority and policy space from domestic governments and civil society to international trade tribunals and multinational capital, in exchange for guaranteed market access and investment protections for specific economic actors.
% ABSENT_VOICES: Local communities, indigenous groups, and small businesses whose interests are often not directly represented in international trade negotiations and whose concerns about environmental degradation, labor exploitation, or cultural impacts are subordinated to trade liberalization goals.
% DISAPPEARANCE_RATIONALE: If the supremacy of trade agreement text and mandatory capital mobility vanished overnight, domestic regulatory standards would immediately regain primacy, national governments would reassert policy space, and capital flows would face new friction and regulatory diversity. The global economic governance landscape would reorganize significantly around re-empowered national and sub-national regulatory bodies.
% FOUNDING_PROBLEM: Fragmented national regulations, unpredictable investment environments, and protectionist trade barriers created inefficiencies and hindered economic growth across North America.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (multinational corporations, international investors) argue the problem of regulatory friction and trade barriers remains live, necessitating the agreement's strong provisions. Critics (labor unions, environmental advocates, some national governments) attest that the original problem has been largely solved, and the current arrangement primarily serves to entrench corporate power and extract rents, as evidenced by numerous investor-state dispute settlement cases and the 'race to the bottom' in standards. Legislative-hearing testimony and independent economic analysis from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__capital_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__capital_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__capital_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because this reading enables significant transfers of wealth and regulatory power from domestic public spheres to private capital, often through mechanisms like ISDS. Suppression is also high (0.90) as it actively limits the policy space of national governments and suppresses alternative regulatory approaches that might impede trade or investment. The theater ratio is low (0.10) because the constraint is highly functional in its extractive purpose, with little performative maintenance. The increasing extractiveness and suppression over the interval reflect the maturation and hardening of the enforcement mechanisms and legal precedents that entrench this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of multinational corporations and international investors, this arrangement is a necessary 'rope' for efficient global commerce, providing stability and predictability. However, from the perspective of domestic regulatory agencies, labor unions, and environmental advocates, it operates as a 'snare' or 'tangled rope,' extracting sovereignty and undermining public interest regulations. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational corporations and international investors are clear beneficiaries (d near 0.0), as the constraint directly subsidizes their operations by reducing regulatory costs and risks. Domestic regulatory agencies, labor unions, environmental advocates, and national governments are targets (d near 1.0), bearing the costs of lost autonomy and policy space. Trade tribunals act as agenda-setters, enforcing the rules that produce this asymmetric flow.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_interpretation_ambiguity,
    'Is the trade agreement text truly intended as supreme law overriding domestic standards, or as a framework for coordination that respects national policy space?',
    'Analysis of original negotiating records, subsequent amendments, and judicial interpretations that explicitly address the balance between trade liberalization and domestic regulatory autonomy.',
    'If interpreted as a coordination framework, the constraint''s extractiveness and suppression would be lower, potentially reclassifying it as a ''rope'' or ''scaffold'' rather than a ''tangled_rope'' or ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_interpretation_ambiguity, conceptual, 'Ambiguity in the foundational interpretation of the trade agreement''s legal status.').

omega_variable(
    economic_impact_ambiguity,
    'Do the aggregate economic benefits of capital mobility and regulatory harmonization for signatory nations outweigh the costs of lost domestic policy space, environmental degradation, and labor market pressures?',
    'Comprehensive, independent economic and social impact assessments that disaggregate benefits and costs across different sectors and populations, rather than relying solely on aggregate GDP figures.',
    'If the costs are found to significantly outweigh the benefits for the majority, it would strengthen the ''snare'' classification and justify calls for renegotiation or withdrawal. If benefits are broadly distributed, it would support a ''rope'' or ''tangled_rope'' framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_ambiguity, empirical, 'Uncertainty regarding the net economic and social welfare effects of the constraint''s operation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of domestic regulatory autonomy primarily structural (direct treaty obligations and ISDS threats) or internalized (governments self-censoring to avoid disputes and maintain investor confidence)?',
    'Comparative analysis of regulatory policy changes in jurisdictions with and without ISDS provisions, and qualitative studies of policymaker decision-making processes under trade agreement pressures.',
    'If internalized suppression is a significant factor, the constraint''s effective suppression is higher than the structural measure suggests, as the ''chill effect'' extends beyond explicit legal challenges. This would reinforce the ''tangled_rope'' or ''snare'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for domestic regulatory autonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__capital_supremacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(naft_tr_t4, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 4, 0.13).
narrative_ontology:measurement(naft_tr_t8, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(naft_tr_t12, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 12, 0.11).
narrative_ontology:measurement(naft_tr_t16, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(naft_tr_t20, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(naft_be_t4, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 4, 0.75).
narrative_ontology:measurement(naft_be_t8, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 8, 0.8).
narrative_ontology:measurement(naft_be_t12, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 12, 0.82).
narrative_ontology:measurement(naft_be_t16, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 16, 0.84).
narrative_ontology:measurement(naft_be_t20, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(naft_su_t4, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 4, 0.8).
narrative_ontology:measurement(naft_su_t8, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 8, 0.85).
narrative_ontology:measurement(naft_su_t12, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 12, 0.87).
narrative_ontology:measurement(naft_su_t16, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 16, 0.89).
narrative_ontology:measurement(naft_su_t20, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 20, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
