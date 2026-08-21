% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__supranational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__supranational_reading, []).

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
 *   constraint_id: paris_article_4_ndc__supranational_reading
 *   human_readable: Paris Agreement Article 4 NDCs: Supranational Binding Commitments
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   This constraint story represents the 'supranational reading' of the Paris
 *   Agreement's Article 4, which interprets Nationally Determined
 *   Contributions (NDCs) as binding commitments on a ratcheting trajectory
 *   towards net-zero, enforced by international accountability mechanisms.
 *   This reading implies significant extraction from carbon-intensive sectors
 *   and fossil-fuel-dependent nations, with institutionalized wealth
 *   transfers to climate-vulnerable nations. The metrics reflect this
 *   high-extraction, actively enforced interpretation, even though the
 *   'claimed_type' is 'tangled_rope' to acknowledge the coordination
 *   function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, 0.85).
domain_priors:suppression_score(paris_article_4_ndc__supranational_reading, 0.78).
domain_priors:theater_ratio(paris_article_4_ndc__supranational_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__supranational_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__supranational_reading, "Paris Agreement Article 4 NDCs: Supranational Binding Commitments").
narrative_ontology:topic_domain(paris_article_4_ndc__supranational_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__supranational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__supranational_reading, '16e69252-59c4-4b8c-a330-8d02d15ddce3').
narrative_ontology:cs_kernel_codification('16e69252-59c4-4b8c-a330-8d02d15ddce3', formalized).
narrative_ontology:cs_authority_grounding('16e69252-59c4-4b8c-a330-8d02d15ddce3', lineage).
narrative_ontology:cs_interpretation_layer_present('16e69252-59c4-4b8c-a330-8d02d15ddce3').
narrative_ontology:cs_reading_relation('16e69252-59c4-4b8c-a330-8d02d15ddce3', paris_article_4_ndc__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('16e69252-59c4-4b8c-a330-8d02d15ddce3', paris_article_4_ndc__equity_reading, influences).
narrative_ontology:cs_axiom('16e69252-59c4-4b8c-a330-8d02d15ddce3', foundational, ndcs_are_legally_binding_under_international_law).
narrative_ontology:cs_axiom_status(ndcs_are_legally_binding_under_international_law, holdable).
narrative_ontology:cs_axiom_grounding('16e69252-59c4-4b8c-a330-8d02d15ddce3', ndcs_are_legally_binding_under_international_law, conventional).
narrative_ontology:cs_axiom('16e69252-59c4-4b8c-a330-8d02d15ddce3', foundational, international_accountability_mechanisms_are_effective).
narrative_ontology:cs_axiom_status(international_accountability_mechanisms_are_effective, holdable).
narrative_ontology:cs_axiom_grounding('16e69252-59c4-4b8c-a330-8d02d15ddce3', international_accountability_mechanisms_are_effective, empirically_contingent).
narrative_ontology:cs_reference_frame('16e69252-59c4-4b8c-a330-8d02d15ddce3', ratcheting_ambition_framework).
narrative_ontology:cs_drift_state('16e69252-59c4-4b8c-a330-8d02d15ddce3', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('16e69252-59c4-4b8c-a330-8d02d15ddce3', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__supranational_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, international_climate_regime).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, climate_vulnerable_nations).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, renewable_energy_sector).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, carbon_intensive_industries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, fossil_fuel_exporting_nations).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, developing_nations_with_high_emissions_growth).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, developing_nations_with_high_emissions_growth).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, developed_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The UNFCCC secretariat and associated bodies that interpret and enforce the Paris Agreement. They push for stronger commitments, monitor compliance, and facilitate international accountability mechanisms, including potential sanctions for non-compliance. They benefit from the expansion of their mandate and the legitimacy of a functioning global climate governance system.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, international_climate_regime, agenda_setter,
    institutional, generational, constrained, global).

% Nations most at risk from climate change impacts. They benefit from the binding nature and ratcheting ambition of NDCs, as this reading implies a stronger global effort to mitigate climate change and institutionalized financial transfers for adaptation and loss and damage. Their 'exit' is existential, making them highly dependent on the constraint's enforcement.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, climate_vulnerable_nations, beneficiary,
    organized, immediate, trapped, global).

% Industries and investors in renewable energy technologies. They benefit from policies and regulations driven by binding NDCs, which accelerate the transition away from fossil fuels and create massive market opportunities for their products and services. They can shift investment to regions with stronger climate policies.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, renewable_energy_sector, beneficiary,
    powerful, biographical, mobile, global).

% Industries heavily reliant on fossil fuels (e.g., coal, oil, gas, heavy manufacturing). They face increasing regulatory burdens, carbon pricing, and potential 'carbon border adjustments' under this reading, leading to significant costs, stranded assets, and pressure to decarbonize or face extinction. Their exit options are limited by their existing infrastructure and business models.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, carbon_intensive_industries, payer,
    powerful, biographical, constrained, global).

% Nations whose economies are heavily dependent on the extraction and export of fossil fuels. They face severe economic disruption and a fundamental challenge to their national development models as global demand for fossil fuels declines under this binding regime. Their national identity and economic structure are deeply tied to fossil fuels, making exit extremely difficult.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, fossil_fuel_exporting_nations, payer,
    institutional, generational, identity_locked, global).

% Nations undergoing rapid industrialization with growing energy demands, often met by fossil fuels. While they benefit from global climate action, this reading imposes stringent emissions reduction targets and potentially limits their development pathways, requiring costly transitions to green technologies and potentially accepting wealth transfers from developed nations. Their development trajectory is constrained by international climate obligations.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, developing_nations_with_high_emissions_growth, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, developing_nations_with_high_emissions_growth, beneficiary).

% Historically high emitters who are expected to lead in emissions reductions and provide financial and technological support to developing nations. Under this reading, they are key architects and enforcers of the supranational regime, but also bear significant costs for decarbonization and international climate finance. Their exit is constrained by reputational costs and the global nature of the climate problem.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, developed_nations, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, developed_nations, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global efforts to limit global warming to well below 2°C, preferably to 1.5°C, by establishing a universal framework for national climate action (NDCs) and a mechanism for their regular review and enhancement, preventing free-riding on climate mitigation.
% TRANSFER_FUNCTION: Transfers regulatory burden and financial costs from the global climate system (and climate-vulnerable nations) to carbon-intensive industries and fossil-fuel-dependent economies. It also institutionalizes wealth transfers from developed to developing nations for climate finance.
% ABSENT_VOICES: Future generations, who are the ultimate beneficiaries of effective climate action, are structurally absent from the negotiation table but are represented by the long-term goals of the agreement. Their interests are articulated by climate scientists and advocacy groups, but they have no direct voice in the design or enforcement of NDCs.
% DISAPPEARANCE_RATIONALE: If this supranational interpretation of NDCs vanished, the global climate regime would collapse into voluntary, uncoordinated national actions. Emissions would likely surge, international climate finance would dry up, and the world would rapidly accelerate towards catastrophic warming, fundamentally altering geopolitical and economic landscapes.
% FOUNDING_PROBLEM: The failure of previous international climate agreements (like Kyoto Protocol) to achieve universal participation and sufficient ambition, leading to an accelerating climate crisis and the threat of irreversible environmental damage.
% FOUNDING_PROBLEM_CORROBORATION: The IPCC reports and scientific consensus consistently corroborate the ongoing and worsening climate crisis, validating the founding problem. Climate-vulnerable nations and international organizations also attest to the urgency and live status of the problem, independent of the fossil fuel industry's narrative.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__supranational_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__supranational_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__supranational_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(paris_article_4_ndc__supranational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__supranational_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__supranational_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__supranational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) and suppression (0.78) reflect the significant economic and political costs imposed on non-compliant states and industries, and the active enforcement required to maintain the ratcheting ambition. The decreasing theater ratio (0.20) suggests that as the climate crisis intensifies, the performative aspects of climate diplomacy give way to more substantive, binding action. The accessibility collapse (0.65) indicates that while alternatives to the global climate regime exist (e.g., bilateral agreements, unilateral action), they are increasingly constrained by the perceived necessity of a universal framework. Resistance (0.70) is high, reflecting the ongoing political and economic pushback from those who bear the costs of decarbonization.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of climate-vulnerable nations and the international climate regime, this interpretation is a necessary 'rope' to coordinate global survival. From the perspective of fossil-fuel-exporting nations and carbon-intensive industries, it is a 'snare' designed to extract wealth and dismantle their economic base. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The international climate regime and climate-vulnerable nations are clear beneficiaries, as this reading strengthens their position and ensures global action. The renewable energy sector also benefits from the market shift. Carbon-intensive industries and fossil-fuel-exporting nations are primary targets, facing significant extraction. Developing nations with high emissions growth are in a dual position, benefiting from global action and climate finance but also bearing costs for their own decarbonization. Developed nations act as agenda-setters but also bear significant financial and decarbonization costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling genuine coordination as pure extraction by acknowledging the foundational problem of climate change and the need for collective action. However, it also guards against mislabeling extraction as coordination by emphasizing the binding nature, enforcement, and significant transfers of wealth and regulatory burden, which are characteristic of a Tangled Rope rather than a pure Rope. The 'live' status of the founding problem, coupled with the 'world_rearranges' disappearance verdict, indicates that the constraint's mandate is very much active and critical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism_strength,
    'Are the international accountability mechanisms truly binding and enforceable, or do they primarily rely on reputational pressure and ''name and shame'' tactics?',
    'Observation of actual sanctions or trade measures imposed on non-compliant states, or the establishment of a formal dispute resolution mechanism with coercive power.',
    'If enforcement is primarily reputational, the effective suppression and extractiveness are lower, pushing the classification closer to a Piton or even a Rope (if the coordination function is strong). If truly coercive, the Snare-like aspects are amplified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_strength, empirical, 'The actual coercive power of international accountability mechanisms.').

omega_variable(
    wealth_transfer_implementation,
    'Are the institutionalized wealth transfers from North to South for climate finance (adaptation, loss and damage) actually materializing at the scale implied by this reading, or are they largely aspirational?',
    'Tracking of actual financial flows against pledged amounts and assessed needs, and analysis of the mechanisms for disbursement and accountability.',
    'If transfers are largely aspirational, the ''beneficiary'' status of climate-vulnerable nations is diminished, and the overall extractiveness from developing nations with high emissions growth is effectively higher, pushing the constraint closer to a pure Snare for them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wealth_transfer_implementation, empirical, 'The reality vs. aspiration of climate finance transfers.').

omega_variable(
    sovereignty_vs_supranationality,
    'To what extent does this supranational reading genuinely override national sovereignty in climate policy, versus being a rhetorical framing that masks continued national self-interest?',
    'Analysis of national legislative and policy responses to international pressure, particularly in cases where domestic economic interests conflict with NDC targets, and the outcomes of international disputes.',
    'If national sovereignty consistently prevails, the ''binding'' nature of NDCs is weakened, reducing effective suppression and extractiveness, potentially shifting the classification towards a Rope or even a Piton. If supranational authority consistently prevails, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_vs_supranationality, conceptual, 'The actual balance of power between national sovereignty and supranational climate governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__supranational_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t0, paris_article_4_ndc__supranational_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(pari_tr_t6, paris_article_4_ndc__supranational_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(pari_tr_t12, paris_article_4_ndc__supranational_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(pari_tr_t18, paris_article_4_ndc__supranational_reading, theater_ratio, 18, 0.23).
narrative_ontology:measurement(pari_tr_t24, paris_article_4_ndc__supranational_reading, theater_ratio, 24, 0.21).
narrative_ontology:measurement(pari_tr_t30, paris_article_4_ndc__supranational_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(pari_be_t0, paris_article_4_ndc__supranational_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(pari_be_t6, paris_article_4_ndc__supranational_reading, base_extractiveness, 6, 0.75).
narrative_ontology:measurement(pari_be_t12, paris_article_4_ndc__supranational_reading, base_extractiveness, 12, 0.8).
narrative_ontology:measurement(pari_be_t18, paris_article_4_ndc__supranational_reading, base_extractiveness, 18, 0.83).
narrative_ontology:measurement(pari_be_t24, paris_article_4_ndc__supranational_reading, base_extractiveness, 24, 0.84).
narrative_ontology:measurement(pari_be_t30, paris_article_4_ndc__supranational_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t0, paris_article_4_ndc__supranational_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(pari_su_t6, paris_article_4_ndc__supranational_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(pari_su_t12, paris_article_4_ndc__supranational_reading, suppression_requirement, 12, 0.74).
narrative_ontology:measurement(pari_su_t18, paris_article_4_ndc__supranational_reading, suppression_requirement, 18, 0.76).
narrative_ontology:measurement(pari_su_t24, paris_article_4_ndc__supranational_reading, suppression_requirement, 24, 0.77).
narrative_ontology:measurement(pari_su_t30, paris_article_4_ndc__supranational_reading, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__supranational_reading, global_infrastructure).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, global_carbon_market_mechanisms).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, national_climate_legislation).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, international_trade_regulations).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'paris_article_4_ndc' kernel. This 'supranational_reading' emphasizes binding commitments and international accountability, contrasting with the 'sovereigntist_reading' (voluntary pledges) and the 'equity_reading' (differentiated responsibilities).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
