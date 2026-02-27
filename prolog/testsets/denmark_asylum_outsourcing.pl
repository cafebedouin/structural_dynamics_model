% ============================================================================
% CONSTRAINT STORY: denmark_asylum_outsourcing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_denmark_asylum_outsourcing, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: denmark_asylum_outsourcing
 *   human_readable: Denmark's Asylum Processing Outsourcing to Third Countries
 *   domain: political/migration/international_relations
 *
 * SUMMARY:
 *   Denmark's policy of processing asylum claims in third countries outside
 *   the EU represents a structural constraint combining coordination function
 *   (responding to domestic political demand for restrictive migration) with
 *   systematic extraction from asylum seekers and erosion of international
 *   refugee protection norms. The constraint exhibits dual nature: it
 *   coordinates Danish domestic politics with restrictive migration outcomes,
 *   while simultaneously extracting costs from powerless asylum seekers who
 *   cannot contest processing in non-signatory jurisdictions and eroding the
 *   international refugee regime that has no enforcement mechanism against
 *   state circumvention. The policy creates a coordination equilibrium
 *   between Denmark and third-country hosts (financial compensation in
 *   exchange for processing services) while distributing extraction costs to
 *   asylum seekers and the abstract collective good of refugee protection
 *   norms. Theater ratio (0.65) reflects significant performative elements:
 *   Denmark maintains formal compliance with EU and international law through
 *   contractual arrangements while achieving restrictive outcomes that would
 *   violate those same norms if applied domestically. The measurement
 *   trajectory shows extractiveness increasing over time as the policy
 *   institutionalizes and processing standards diminish, while theater ratio
 *   rises as the legal architecture becomes more elaborate to justify the
 *   extraction mechanism.
 *
 * KEY AGENTS:
 *   - Asylum Seekers: Primary victims (powerless/trapped) — denied onshore processing; no exit mechanism; subjected to external processing in jurisdictions without refugee protection obligations
 *   - Danish Government: Primary beneficiary (institutional/arbitrage) — captures domestic political benefits of restrictive asylum policy while maintaining formal international law compliance
 *   - International Refugee Protection Regime: Secondary victim (powerless/trapped) — abstract collective good bearing costs of norm erosion and legitimacy damage; no mechanism for self-defense or exit
 *   - EU and Member State Coalition: Organized beneficiary (organized/constrained) — coordinate on reducing onshore asylum flows but constrained by EU legal requirements and peer pressure; some states resist (Hungary, Poland) while others participate
 *   - Third Country Host Governments: Secondary beneficiary (moderate/constrained) — receive financial compensation but bear administrative costs and reputational risk; constrained by economic dependency
 *   - NGO and Rights Advocacy Coalition: Organized victim (organized/constrained) — conduct performative advocacy and litigation with limited structural impact; constrained by institutional capture
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing state sovereignty as immutable law rather than contingent political choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(denmark_asylum_outsourcing, 0.58).
domain_priors:suppression_score(denmark_asylum_outsourcing, 0.72).
domain_priors:theater_ratio(denmark_asylum_outsourcing, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(denmark_asylum_outsourcing, extractiveness, 0.58).
narrative_ontology:constraint_metric(denmark_asylum_outsourcing, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(denmark_asylum_outsourcing, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(denmark_asylum_outsourcing, tangled_rope).
narrative_ontology:human_readable(denmark_asylum_outsourcing, "Denmark's Asylum Processing Outsourcing to Third Countries").
narrative_ontology:topic_domain(denmark_asylum_outsourcing, "political/migration/international_relations").

domain_priors:requires_active_enforcement(denmark_asylum_outsourcing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(denmark_asylum_outsourcing, danish_government).
narrative_ontology:constraint_beneficiary(denmark_asylum_outsourcing, danish_domestic_politics).
narrative_ontology:constraint_victim(denmark_asylum_outsourcing, asylum_seekers).
narrative_ontology:constraint_victim(denmark_asylum_outsourcing, international_refugee_protection_regime).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASYLUM SEEKER (SNARE) — Individuals fleeing persecution have no exit from the constraint. Physically present in Denmark, they are denied processing on home territory and must navigate external processing centers in countries with no obligation to protect them. Zero meaningful alternatives. Maximum experienced extraction.
constraint_indexing:constraint_classification(denmark_asylum_outsourcing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REFUGEE PROTECTION REGIME (SNARE) — The 1951 Refugee Convention framework and its norms are systematically circumvented. States cannot exit the regime without formal withdrawal, yet outsourcing undermines the regime's functional capacity. The regime bears costs (legitimacy erosion, norm cascade effects) with no mechanism for self-defense.
constraint_indexing:constraint_classification(denmark_asylum_outsourcing, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: DANISH GOVERNMENT (ROPE) — Experiences the constraint as coordination: outsourcing enables domestic political management of migration (responding to voter demand for restrictive asylum policy) while maintaining formal EU and international law compliance. Net beneficiary through arbitrage — can claim adherence to international norms while implementing restrictive outcomes.
constraint_indexing:constraint_classification(denmark_asylum_outsourcing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EU AND MEMBER STATE COALITION (TANGLED ROPE) — EU states collectively benefit from reduced onshore asylum flows (coordination function) while facing coordination costs: legal challenges, norm erosion, and migration destabilization in third countries. Exit is constrained by EU law requirements and peer pressure, but coalitions (Hungary, Poland) have agency to resist. Mixed extraction and benefit.
constraint_indexing:constraint_classification(denmark_asylum_outsourcing, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: THIRD COUNTRY HOST GOVERNMENTS (TANGLED ROPE) — Countries hosting external processing centers (Rwanda, Uganda, others) receive financial compensation (coordination benefit) but must bear administrative and security costs, and face reputational risk from refugee rights organizations. Exit is constrained by economic dependency on Danish/EU payments, yet they retain agency to set processing standards.
constraint_indexing:constraint_classification(denmark_asylum_outsourcing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NGO AND RIGHTS ADVOCACY COALITION (PITON) — Maintains performative monitoring, litigation, and public advocacy despite structural constraints on effectiveness. Resources are devoted to legal challenges and documentation, but the underlying extraction mechanism (outsourcing to non-signatory jurisdictions) persists through political will. Theater ratio reflects that oversight activity is largely symbolic — constrained exit due to institutional capture and political asymmetry.
constraint_indexing:constraint_classification(denmark_asylum_outsourcing, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, state sovereignty over border control appears as an irreducible feature of international law. The Westphalian system grants states authority to determine entry and processing mechanisms. This perspective risks naturalizing what is actually a contingent legal/political choice — the outsourcing model is not inevitable from sovereignty, but a strategic interpretation of it.
constraint_indexing:constraint_classification(denmark_asylum_outsourcing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(denmark_asylum_outsourcing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(denmark_asylum_outsourcing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(denmark_asylum_outsourcing, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(denmark_asylum_outsourcing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(denmark_asylum_outsourcing, TR),
    TR >= 0.70.

:- end_tests(denmark_asylum_outsourcing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): High-moderate. The constraint extracts significantly from asylum seekers through denial of accessible processing and due process, and from the international regime through norm erosion. However, extraction is not total (0.7+) because some coordination value is genuine — Denmark does address real coordination problems around asylum pressure, even if the solution is extractive. Suppression (0.72): High. Asylum seekers face extreme suppression: physical barriers (external processing), legal barriers (non-signatory jurisdiction), economic barriers (cost of international travel to process claims), and informational barriers (lack of transparency in external centers). The suppression mechanism relies on spatial distance and jurisdictional opacity. Theater ratio (0.65): Moderate-high. The policy requires substantial legal and diplomatic theater to maintain: contractual agreements with third countries, legal opinions claiming compatibility with refugee law, public narratives framing outsourcing as 'responsibility-sharing' with third countries. The theater has increased over time as legal challenges mount and the framework requires more elaborate justification. Claimed type (Tangled Rope): The constraint exhibits both coordination function (managing domestic migration politics) and asymmetric extraction (from asylum seekers and the protection regime). Active enforcement is required both to maintain the extraction (deportations, processing denials) and to sustain the international legal theater (contract negotiation, diplomatic framing).
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates stark perspectival disagreement. The asylum seeker sees pure extraction (Snare) — no coordination benefit, only costs. The refugee protection regime sees extraction and erosion (Snare) — the regime cannot exit and bears delegitimization costs. The Danish government sees coordination (Rope) — solving the legitimate problem of managing asylum pressure while satisfying voter demand. The EU coalition sees mixed extraction and benefit (Tangled Rope) — coordinating to reduce onshore flows but constrained by legal and normative costs. Third country hosts see compensation (Tangled Rope) — financial benefit offsetting administrative burden, though constrained by economic dependency. NGOs see their own degraded function (Piton) — doing advocacy work that is largely symbolic because the underlying extraction mechanism persists. The analytical observer risks naturalizing as law (Mountain) what is actually a strategic political choice about how to interpret state sovereignty. The core gap is between the beneficiary's experience (coordination) and the powerless agent's experience (extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position. Asylum seekers have d ≈ 0.95 (trapped victim) → maximum f(d) → maximum experienced extraction. The refugee regime has d ≈ 0.90 (powerless, trapped) → near-maximum extraction. The Danish government has d ≈ 0.15 (institutional, arbitrage beneficiary) → minimal or negative f(d) → they experience the constraint as enabling, not extractive. The EU coalition has d ≈ 0.55 (organized, constrained) → moderate f(d) ≈ 0.75 → moderate experienced extraction. Third country hosts have d ≈ 0.50 (moderate power, economic constraint) → symmetric f(d) ≈ 0.65 → costs and benefits roughly balanced. NGOs have d ≈ 0.75 (organized advocates, limited structural power) → elevated f(d) ≈ 1.15 → they experience the constraint as limiting despite advocacy efforts. The analytical observer has d ≈ 0.72 (analytical position) → f(d) ≈ 1.15 → they see the full structural complexity but risk naturalizing it as inevitable. Spatial scope (global for asylum seekers, national for Danish government) affects verification difficulty and hidden extraction — global scope amplifies χ for those tracking regime-level impacts.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that it is genuinely a Tangled Rope, not a false snare or false rope. The genuine coordination function (managing domestic demand for restrictive asylum policy, EU burden-sharing) is real and benefits real actors (Danish government, some third-country hosts). The genuine extraction (from asylum seekers, from protection regime) is equally real and harms real actors (asylum seekers, regime legitimacy). This is not 'extraction masquerading as coordination' (a snare), and not 'coordination masquerading as extraction' (a false snare). The mandatrophy is resolved by accepting that both functions coexist in the same constraint. The perspectival disagreement is not an error to be corrected but a structural feature: different agents genuinely experience different mixes of coordination and extraction depending on their position. The risk is confusing the beneficiary's experience (coordination) with the victim's experience (extraction) and declaring one 'the truth.' The framework shows both are true from their respective structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    processing_fairness_standard,
    'What standard determines whether external processing achieves equivalent procedural fairness to onshore processing?',
    'Comparison of asylum approval rates, appeal success rates, and procedural transparency between Denmark''s external centers and domestic processing; independent audits of due process compliance',
    'If external processing meets fairness threshold: constraint may downgrade to Rope (coordination). If systematic disparities exist: snare classification confirmed (extraction via due process denial).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(processing_fairness_standard, empirical, 'Whether external processing meets fairness standards equivalent to onshore processing').

omega_variable(
    third_country_legitimacy,
    'Do third countries hosting external processing centers have genuine capacity to provide asylum protection, or are they primarily extracting rents while delivering minimal protection?',
    'Analysis of refugee outcomes post-processing: resettlement rates, return rates, integration outcomes; assessment of third country legal frameworks for protection guarantees',
    'If third countries deliver genuine protection: constraint shifts toward legitimate coordination. If rent extraction dominates: constraint remains tangled rope or snare at host government level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_country_legitimacy, empirical, 'Whether third country hosts genuinely provide asylum protection or primarily extract rents').

omega_variable(
    norm_cascade_failure_mechanism,
    'Does Danish outsourcing trigger norm cascade failure in the international refugee protection regime, or do states maintain functional refugee conventions despite varied implementation?',
    'Longitudinal analysis of asylum processing trends across EU states post-Danish policy; measurement of convention adherence rates and withdrawal/reinterpretation patterns',
    'If norm cascade occurs: outsourcing accelerates regime erosion and snare classification is vindicated. If norms persist: constraint may be Tangled Rope rather than snare at regime level.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(norm_cascade_failure_mechanism, empirical, 'Whether outsourcing triggers cascading erosion of refugee protection norms').

omega_variable(
    political_demand_authenticity,
    'Is Danish voter demand for restrictive asylum policy genuine and durable, or does it reflect manufactured preference through media framing?',
    'Longitudinal survey analysis of asylum policy preferences; comparison with media framing narratives; analysis of preference formation mechanisms',
    'If demand is authentic: Danish government benefits from genuine coordination (rope perspective valid). If manufactured: government and media are co-extracting from both asylum seekers and public opinion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_demand_authenticity, conceptual, 'Whether restrictive asylum preferences reflect authentic or manufactured political demand').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(denmark_asylum_outsourcing, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dao_tr_t0, denmark_asylum_outsourcing, theater_ratio, 0, 0.5).
narrative_ontology:measurement(dao_tr_t3, denmark_asylum_outsourcing, theater_ratio, 3, 0.58).
narrative_ontology:measurement(dao_tr_t6, denmark_asylum_outsourcing, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(dao_be_t0, denmark_asylum_outsourcing, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dao_be_t3, denmark_asylum_outsourcing, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(dao_be_t6, denmark_asylum_outsourcing, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(denmark_asylum_outsourcing, enforcement_mechanism).
narrative_ontology:affects_constraint(denmark_asylum_outsourcing, eu_migration_burden_sharing).
narrative_ontology:affects_constraint(denmark_asylum_outsourcing, refugee_convention_state_interpretation).
narrative_ontology:affects_constraint(denmark_asylum_outsourcing, third_country_partnership_extraction).

% DUAL FORMULATION NOTE:
% Denmark's outsourcing represents a downstream implementation of broader EU migration policy coordination. It is structurally distinct from the abstract burden-sharing agreements (coordination-level constraint) but implements those agreements through a specific extraction mechanism. The constraint family includes the EU-level coordination problem (Rope or Tangled Rope depending on perspective) and the implementation-level extraction mechanism (this story). Network links establish causal and institutional coupling between EU policy coordination and national outsourcing implementation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(denmark_asylum_outsourcing, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
