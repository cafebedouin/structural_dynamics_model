% ============================================================================
% CONSTRAINT STORY: cbdr_principle__historical_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cbdr_principle__historical_responsibility_reading, []).

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
 *   constraint_id: cbdr_principle__historical_responsibility_reading
 *   human_readable: CBDR Principle: Historical Responsibility Reading
 *   domain: international_climate_governance/treaty_law/development_economics
 *
 * SUMMARY:
 *   This constraint represents the 'historical responsibility' reading of the
 *   Common But Differentiated Responsibilities (CBDR) principle in
 *   international climate governance. It mandates binding emissions
 *   reductions and financial transfers from developed nations, proportional
 *   to their cumulative historical emissions, to support developing nations
 *   in mitigation, adaptation, and loss/damage. This reading emphasizes
 *   climate justice and equity, placing the burden on those who contributed
 *   most to the problem. The constraint is claimed as a Tangled Rope due to
 *   its genuine coordination function (addressing a global commons problem)
 *   coupled with asymmetric extraction (from developed to developing nations)
 *   and the need for active enforcement through treaty mechanisms.
 *
 * KEY AGENTS:
 *   - developed_nations: Primary payer (institutional/constrained)
 *   - developing_nations: Primary beneficiary (organized/constrained)
 *   - climate_vulnerable_communities: Ultimate beneficiary (powerless/trapped)
 *   - fossil_fuel_industries: Indirect payer (powerful/constrained)
 *   - international_climate_negotiators: Agenda setter (institutional/constrained)
 *   - global_civil_society_organizations: Observer (organized/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, 0.65).
domain_priors:suppression_score(cbdr_principle__historical_responsibility_reading, 0.4).
domain_priors:theater_ratio(cbdr_principle__historical_responsibility_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__historical_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__historical_responsibility_reading, "CBDR Principle: Historical Responsibility Reading").
narrative_ontology:topic_domain(cbdr_principle__historical_responsibility_reading, "international_climate_governance/treaty_law/development_economics").

domain_priors:requires_active_enforcement(cbdr_principle__historical_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__historical_responsibility_reading, '8069cb06-7ab0-42a8-87cd-7079a607c830').
narrative_ontology:cs_kernel_codification('8069cb06-7ab0-42a8-87cd-7079a607c830', formalized).
narrative_ontology:cs_authority_grounding('8069cb06-7ab0-42a8-87cd-7079a607c830', lineage).
narrative_ontology:cs_interpretation_layer_present('8069cb06-7ab0-42a8-87cd-7079a607c830').
narrative_ontology:cs_reading_relation('8069cb06-7ab0-42a8-87cd-7079a607c830', cbdr_principle__voluntary_commitment_reading, coexists_with).
narrative_ontology:cs_axiom('8069cb06-7ab0-42a8-87cd-7079a607c830', foundational, historical_emissions_create_binding_debt).
narrative_ontology:cs_axiom_status(historical_emissions_create_binding_debt, holdable).
narrative_ontology:cs_axiom_grounding('8069cb06-7ab0-42a8-87cd-7079a607c830', historical_emissions_create_binding_debt, deontological).
narrative_ontology:cs_axiom('8069cb06-7ab0-42a8-87cd-7079a607c830', secondary, climate_justice_requires_reparations).
narrative_ontology:cs_axiom_status(climate_justice_requires_reparations, holdable).
narrative_ontology:cs_axiom_grounding('8069cb06-7ab0-42a8-87cd-7079a607c830', climate_justice_requires_reparations, deontological).
narrative_ontology:cs_reference_frame('8069cb06-7ab0-42a8-87cd-7079a607c830', rio_declaration_equity_framework).
narrative_ontology:cs_drift_state('8069cb06-7ab0-42a8-87cd-7079a607c830', contemporary_climate_negotiations, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8069cb06-7ab0-42a8-87cd-7079a607c830', '').
narrative_ontology:cs_kernel_id(cbdr_principle__historical_responsibility_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, developing_nations).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, climate_vulnerable_communities).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, developed_nations).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, fossil_fuel_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Expected to bear the primary burden of emissions reductions and provide significant financial transfers for loss and damage, proportional to their historical emissions. They face domestic political resistance to these obligations and seek to dilute them.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developed_nations, payer,
    institutional, generational, constrained, global).

% Advocate for the historical responsibility principle, expecting financial and technological support from developed nations to address climate change impacts and pursue sustainable development without hindering their economic growth. Their leverage comes from collective action and moral claims.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developing_nations, beneficiary,
    organized, generational, constrained, global).

% Suffer disproportionately from climate change impacts (sea-level rise, extreme weather) despite minimal historical contribution to emissions. They are the ultimate beneficiaries of loss and damage financing and adaptation support, but lack direct negotiating power.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, climate_vulnerable_communities, beneficiary,
    powerless, immediate, trapped, local).

% Face direct and indirect costs from emissions reduction mandates and potential carbon pricing. They actively lobby against stringent climate policies and seek to delay the transition away from fossil fuels.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, fossil_fuel_industries, payer,
    powerful, biographical, constrained, global).

% Tasked with drafting and implementing international climate agreements. They mediate between the conflicting demands of developed and developing nations, attempting to forge consensus on emissions targets and financial mechanisms. Their power is derived from the treaty framework itself.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, international_climate_negotiators, agenda_setter,
    institutional, biographical, constrained, global).

% Monitor climate negotiations, advocate for ambitious targets, and highlight the plight of vulnerable communities. They exert pressure on both developed and developing nations to uphold climate justice principles.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, global_civil_society_organizations, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate global efforts to address climate change by assigning differentiated responsibilities based on historical contributions, thereby fostering trust and collective action between nations with vastly different development trajectories and historical impacts.
% TRANSFER_FUNCTION: Transfers financial resources and technology from developed nations (who bear historical responsibility for emissions) to developing nations (who are disproportionately vulnerable to climate impacts and have contributed less to historical emissions) for mitigation, adaptation, and loss and damage.
% ABSENT_VOICES: Future generations, who will bear the full brunt of climate change if current actions are insufficient, are structurally absent from negotiations. Indigenous communities, often on the front lines of climate impacts, are frequently marginalized in decision-making processes despite their deep ecological knowledge.
% DISAPPEARANCE_RATIONALE: If the historical responsibility principle vanished, the moral and legal basis for differentiated climate action would collapse. Developed nations would likely reduce their financial commitments and emissions targets, leading to a severe breakdown in international climate cooperation and exacerbating climate injustice, with developing nations bearing an even greater burden.
% FOUNDING_PROBLEM: The problem of global climate change, where a small number of industrialized nations historically contributed the most to greenhouse gas emissions, while the impacts are felt globally, disproportionately affecting nations with minimal historical contribution.
% FOUNDING_PROBLEM_CORROBORATION: The scientific consensus on anthropogenic climate change and the historical emissions data, corroborated by the IPCC and numerous independent research institutions, attests to the live status of the problem. Developing nations and climate scientists consistently highlight the ongoing and escalating impacts.
narrative_ontology:disappearance_verdict(cbdr_principle__historical_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(cbdr_principle__historical_responsibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cbdr_principle__historical_responsibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(cbdr_principle__historical_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cbdr_principle__historical_responsibility_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cbdr_principle__historical_responsibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cbdr_principle__historical_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because this reading demands substantial, non-voluntary transfers and emissions cuts from developed nations, which they resist. Suppression (0.40) is moderate, reflecting the difficulty of enforcing international treaty obligations without a global sovereign, but also the moral and political pressure that limits developed nations' exit options. Theater ratio (0.20) is relatively low, as the principle's advocates genuinely seek to implement its core tenets, though some 'greenwashing' and symbolic gestures exist. Resistance is high (0.75) from developed nations and fossil fuel industries, who actively push back against these obligations.
 *
 * PERSPECTIVAL GAP:
 *   Developed nations perceive this as a highly extractive constraint, imposing significant costs on their economies and citizens. Developing nations, conversely, see it as a necessary coordination mechanism for global climate action and a matter of justice, where the 'extraction' is a rightful transfer. The engine's per-seat classification will reflect this divergence based on their declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed nations are targets (high d) due to the financial and emissions obligations. Developing nations and climate-vulnerable communities are beneficiaries (low d) as they receive transfers and are exempt from stringent early emissions cuts. Fossil fuel industries are indirect targets as their business model is directly impacted by emissions reductions. International climate negotiators are agenda setters, mediating the constraint's implementation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of CBDR prevents mislabeling genuine coordination (global climate action) as pure extraction by acknowledging the shared problem, but also prevents mislabeling asymmetric transfers as symmetric coordination by highlighting the historical responsibility and the active enforcement required to overcome resistance from developed nations. The mandate is live, as the climate crisis continues to worsen, and the problem of historical responsibility remains central to equity discussions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism_efficacy,
    'How effective are existing international legal and political mechanisms in enforcing binding emissions reductions and financial transfers from developed nations?',
    'Analysis of compliance rates with treaty obligations, effectiveness of dispute resolution mechanisms, and the impact of sanctions or diplomatic pressure on non-compliant nations.',
    'If enforcement is weak, the constraint''s effective suppression and extractiveness are lower than stated, potentially reclassifying it closer to a Piton (if the coordination function also atrophies) or a Rope (if compliance is mostly voluntary). If enforcement is robust, the Tangled Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_efficacy, empirical, 'Uncertainty regarding the actual coercive power of international climate governance.').

omega_variable(
    historical_responsibility_framing_legitimacy,
    'Is the ''historical responsibility'' framing of CBDR a legitimate basis for binding obligations, or is it a conceptual tool for rent-seeking by developing nations?',
    'Analysis of international legal precedents, philosophical arguments for intergenerational equity, and the evolving discourse on climate justice. This is a conceptual debate with no single empirical resolution.',
    'If the framing is widely accepted as legitimate, the constraint''s coordination function is strengthened, and resistance from developed nations is seen as illegitimate. If it''s widely rejected, the constraint''s legitimacy erodes, increasing resistance and potentially shifting it towards a Snare (if coercion is maintained without a coordination story).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_responsibility_framing_legitimacy, conceptual, 'The conceptual validity of historical responsibility as a basis for current obligations.').

omega_variable(
    developed_nation_exit_arbitrage,
    'To what extent can developed nations effectively arbitrage their obligations by outsourcing emissions-intensive industries or by using carbon offsets that do not represent genuine reductions?',
    'Empirical tracking of carbon leakage, analysis of the integrity of carbon markets, and assessment of the ''additionality'' of offset projects.',
    'If arbitrage is widespread and effective, developed nations'' effective directionality shifts towards beneficiary, reducing their perceived extraction and weakening the constraint''s overall force. This would push the classification towards a Piton or even a Rope, as the ''extraction'' becomes easily avoidable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developed_nation_exit_arbitrage, empirical, 'The ability of developed nations to circumvent obligations through various forms of arbitrage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__historical_responsibility_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_tr_t1992, cbdr_principle__historical_responsibility_reading, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(cbdr_tr_t2000, cbdr_principle__historical_responsibility_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(cbdr_tr_t2008, cbdr_principle__historical_responsibility_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(cbdr_tr_t2016, cbdr_principle__historical_responsibility_reading, theater_ratio, 2016, 0.2).
narrative_ontology:measurement(cbdr_tr_t2024, cbdr_principle__historical_responsibility_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(cbdr_be_t1992, cbdr_principle__historical_responsibility_reading, base_extractiveness, 1992, 0.5).
narrative_ontology:measurement(cbdr_be_t2000, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(cbdr_be_t2008, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2008, 0.6).
narrative_ontology:measurement(cbdr_be_t2016, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2016, 0.63).
narrative_ontology:measurement(cbdr_be_t2024, cbdr_principle__historical_responsibility_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_su_t1992, cbdr_principle__historical_responsibility_reading, suppression_requirement, 1992, 0.3).
narrative_ontology:measurement(cbdr_su_t2000, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(cbdr_su_t2008, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2008, 0.38).
narrative_ontology:measurement(cbdr_su_t2016, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2016, 0.4).
narrative_ontology:measurement(cbdr_su_t2024, cbdr_principle__historical_responsibility_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__historical_responsibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, paris_agreement_ndc_framework).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, loss_and_damage_fund).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the CBDR principle. The 'voluntary_commitment_reading' is a sibling constraint that emphasizes nationally determined contributions and technology transfer over binding historical responsibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
