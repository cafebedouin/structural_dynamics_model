% ============================================================================
% CONSTRAINT STORY: primate_welfare_policy_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_primate_welfare_policy_framework, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: primate_welfare_policy_framework
 *   human_readable: Primate Welfare Policy Framework
 *   domain: animal_welfare/biomedical_ethics/regulatory_governance
 *
 * SUMMARY:
 *   Primate welfare policy frameworks globally create a structural system
 *   where the moral recognition of primate sentience and capacity for
 *   suffering simultaneously legitimates their confinement and use in
 *   research. The constraint operates across institutional, ethical, and
 *   economic domains: research institutions require primate access to
 *   maintain funding and prestige; regulatory agencies require frameworks to
 *   manage public legitimacy and scientific credibility; welfare advocates
 *   require standards to reduce suffering; captive primates have no
 *   negotiating position. The framework exemplifies a Tangled Rope constraint
 *   because it performs genuine coordination (establishing baseline
 *   standards, reducing unnecessary suffering) while extracting value
 *   (enabling continued research use, legitimating confinement, suppressing
 *   calls for abolition). The theater ratio has increased over the 45-year
 *   interval as welfare standards have become more elaborate and
 *   performative, while actual enforcement has remained inconsistent and
 *   self-policing. Alternative methods (computational models, organ-on-chip,
 *   human tissue banking) are developing but have not yet achieved sufficient
 *   institutional acceptance to displace primate-dependent research. The
 *   constraint resolves mandatrophy by showing that all perspectives are
 *   structurally coherent: it genuinely is a coordination mechanism (from
 *   institutional perspective), genuinely is extraction (from captive primate
 *   perspective), and genuinely is theatrical legitimation (from
 *   civilizational perspective).
 *
 * KEY AGENTS:
 *   - Captive Primate Populations: Primary victim (powerless/trapped) — biologically dependent, physically confined, no exit mechanism; bear full cost of research extraction masked by welfare improvements
 *   - Research Institutions: Primary beneficiary (institutional/arbitrage) — capture research value, maintain prestige and funding through continued primate access; can offshore if domestic regulation becomes onerous
 *   - Pharmaceutical Industry: Primary beneficiary (institutional/arbitrage) — extract drug development efficiency and regulatory legitimacy from primate-tested compounds; externalize welfare costs to facilities
 *   - Regulatory Agencies: Secondary actor (powerful/mobile) — manage competing mandates of enabling research and managing public legitimacy; benefit from welfare framework (enables continued research while appearing ethical)
 *   - Ethics Committees: Secondary victim (moderate/constrained) — face institutional pressure to approve protocols while enforcing welfare standards; constrained by career incentives and resource limitations
 *   - Animal Welfare Advocacy Coalition: Organized beneficiary (organized/constrained) — perceive welfare framework as temporary structure with sunset pathway; pressuring for standards increases and alternative development
 *   - Funding Agencies: Beneficiary (institutional/arbitrage) — justify funding through welfare framework's legitimacy; maintain portfolio concentrated in primate-dependent research
 *   - Conservation Populations: Passive victim (powerless/trapped) — potential extraction through capture and removal of individuals from wild populations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(primate_welfare_policy_framework, 0.58).
domain_priors:suppression_score(primate_welfare_policy_framework, 0.65).
domain_priors:theater_ratio(primate_welfare_policy_framework, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(primate_welfare_policy_framework, extractiveness, 0.58).
narrative_ontology:constraint_metric(primate_welfare_policy_framework, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(primate_welfare_policy_framework, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(primate_welfare_policy_framework, tangled_rope).
narrative_ontology:human_readable(primate_welfare_policy_framework, "Primate Welfare Policy Framework").
narrative_ontology:topic_domain(primate_welfare_policy_framework, "animal_welfare/biomedical_ethics/regulatory_governance").

domain_priors:requires_active_enforcement(primate_welfare_policy_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(primate_welfare_policy_framework, research_institutions).
narrative_ontology:constraint_beneficiary(primate_welfare_policy_framework, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(primate_welfare_policy_framework, funding_agencies).
narrative_ontology:constraint_victim(primate_welfare_policy_framework, captive_primate_populations).
narrative_ontology:constraint_victim(primate_welfare_policy_framework, conservation_genetics).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CAPTIVE PRIMATE POPULATION (SNARE) — Primates in research facilities have no exit mechanism. They are physically confined, biologically dependent on institutional care, and lack ability to negotiate terms. The welfare framework ostensibly protects them but simultaneously legitimates their confinement and extraction. Welfare regulations permit ongoing invasive procedures, resource extraction (blood, tissue, organs), and behavioral suppression in exchange for minimized suffering rather than elimination of harm. The framework's existence suppresses calls for complete abolition by creating the appearance of moral resolution. Maximum extraction experienced from powerless perspective.
constraint_indexing:constraint_classification(primate_welfare_policy_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSTITUTIONAL ETHICS COMMITTEES (TANGLED ROPE) — Committees face competing mandates: enforce welfare standards (genuine coordination function for reducing unnecessary suffering) while maintaining research pipeline approval rates. Constrained by institutional pressure to approve proposals, career incentives favoring approval over obstruction, and resource limitations for thorough review. The welfare framework both enables their gatekeeping function (coordination benefit) and constrains their independence (extraction). Moderate experienced extraction with genuine coordination function.
constraint_indexing:constraint_classification(primate_welfare_policy_framework, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RESEARCH INSTITUTIONS & PHARMACEUTICAL INDUSTRY (ROPE) — Primary beneficiaries experiencing the welfare framework as coordination mechanism. The framework enables continued research access by legitimating primate use through demonstrating 'ethical oversight.' Benefits from welfare standards by reducing regulatory uncertainty, attracting funding from ethics-conscious sources, and preempting calls for abolition. The coordination function (standardized welfare assessment, transparent reporting) creates administrative overhead that benefits large institutions more than smaller ones. Low experienced extraction; net beneficiary position with arbitrage options (can offshore to less regulated jurisdictions if needed).
constraint_indexing:constraint_classification(primate_welfare_policy_framework, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANIMAL WELFARE ADVOCACY COALITION (SCAFFOLD) — Organized agents (NGOs, regulatory bodies, some scientific reformers) perceive the welfare framework as a temporary coordination structure with a sunset clause embedded in its own logic. The framework permits transition pathways: alternative methods development, in vitro models, computational approaches. If these alternatives mature, the welfare framework's justification (primate testing is necessary) becomes false. The coalition has constrained but real agency — can pressure for standards increases, fund alternative development, and accelerate the sunset timeline. Low extractiveness from this perspective because exit path is structurally visible and timeline is generational.
constraint_indexing:constraint_classification(primate_welfare_policy_framework, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: REGULATORY COMPLIANCE THEATER (PITON) — Welfare assessments and reporting requirements have become substantially performative. Many protocols receive rubber-stamp approvals; welfare monitoring relies on self-reporting by the same institutions that benefit from research approval; enforcement is inconsistent. The compliance infrastructure persists through institutional inertia and bureaucratic standardization rather than because it effectively prevents suffering. Theater_ratio is high because the process is maintenance of legitimacy rather than substantive welfare improvement. Piton classification reflects degraded function maintained by institutional momentum.
constraint_indexing:constraint_classification(primate_welfare_policy_framework, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY AGENCIES & GOVERNMENTS (TANGLED ROPE) — Powerful agents with mobile exit options face genuine coordination problem (managing research safety, public legitimacy, scientific credibility) alongside asymmetric extraction benefit (primate research generates economic value, employment, medical claims). Agencies benefit from centralized welfare framework (enables research continuation) while bearing costs of enforcement and public accountability. High mobility (can shift regulatory standards, allow exemptions, reclassify welfare requirements) combined with genuine coordination function and extraction benefit. Moderate-high experienced extraction from this perspective due to mobile exit options and powerful position.
constraint_indexing:constraint_classification(primate_welfare_policy_framework, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE SUMMIT) — Civilizational-scale analysis may frame primate welfare constraints as inherent to biomedical progress: primate cognition and physiology are sufficiently human-like that valid medical models require primate subjects; the constraint is immutable because the research necessity is immutable. This perspective risks naturalizing what is actually a contingent institutional choice (continued reliance on primate models despite developing alternatives). Engine false summit detection identifies this as misplaced mountain classification — the 'necessity' is institutional and economic, not natural law.
constraint_indexing:constraint_classification(primate_welfare_policy_framework, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(primate_welfare_policy_framework_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(primate_welfare_policy_framework, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(primate_welfare_policy_framework, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(primate_welfare_policy_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(primate_welfare_policy_framework, TR),
    TR >= 0.70.

:- end_tests(primate_welfare_policy_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The welfare framework coordinates baseline standards (genuine reduction in unnecessary suffering) but simultaneously enables extraction: research institutions extract productivity and prestige from continued primate access; pharmaceutical industry extracts drug-development efficiency; regulatory agencies extract legitimacy. The extraction is not maximal because welfare standards genuinely reduce suffering and create some administrative burden on beneficiaries. The measurement shows increasing extractiveness from 0.38 to 0.58 over 45 years, reflecting accumulation of extraction mechanisms and evolution of beneficiary sophistication in using the framework to suppress alternatives. Suppression (0.65): High. Suppression operates at multiple levels: (1) Physical: primates are confined and dependent; (2) Regulatory: welfare framework legitimates use by addressing moral concerns; (3) Epistemic: alternative methods are underfunded and undervalidated relative to established primate models; (4) Institutional: career incentives favor research continuation over abolition; (5) Narrative: welfare standards create appearance of moral resolution while maintaining extraction. Theater ratio (0.68): High and increasing. Welfare assessments are substantially performative: rubber-stamp approvals, self-reporting compliance, inconsistent enforcement, public relations function. The theater increased from 0.42 to 0.68 over the interval as the framework became more elaborate while actual enforcement remained weak. Claimed type (Tangled Rope): Justified by the presence of genuine coordination function (welfare standard-setting, suffering reduction) combined with asymmetric extraction (research institutions and industry benefit; primates and conservation bear costs) and active enforcement requirement (regulatory oversight, ethics committees).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the framework's structural function: it creates agreement among powerful actors (institutions, industry, regulators all see Rope or low extraction) while producing maximum disagreement with powerless agents (primates see Snare). This is not a disagreement about facts but about structural position — all perspectives are internally coherent. The gap is diagnostic: if all agents perceived the same constraint type, no extraction would be occurring (all would agree on classification). The gap itself is evidence of asymmetric extraction. The analytical observer's potential false summit (viewing primate research as natural law) represents the highest-level expression of this gap — naturalizing the contingent institutional arrangement that benefits some agents at the cost of others.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by each agent's structural position relative to extraction flow. Captive primates (victims + trapped) derive maximum d ≈ 0.95, producing maximum experienced extraction. Research institutions (beneficiaries + arbitrage) derive low d ≈ 0.05, producing negative effective extraction (they are subsidized by the constraint). Ethics committees (victims and beneficiaries simultaneously, constrained exit) derive d ≈ 0.50, producing moderate experienced extraction. Regulatory agencies (powerful + mobile, beneficiaries) derive d ≈ 0.35, producing low-moderate experienced extraction due to mobility. The derived directionality values reflect that extraction is directional: flowing from powerless/trapped agents toward institutional/arbitrage agents. No overrides are required because the structural relationships are clear.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that all six classification types apply to different agents observing the same constraint. This is not inconsistency — it is structural asymmetry made visible through perspectival analysis. Captive primates (Snare) and research institutions (Rope) are observing the same constraint from positions where one extracts and one benefits. The welfare framework achieves its structural function precisely by producing this perspectival gap: it legitimates the constraint to beneficiaries (who see coordination) while appearing to address concerns of victims (who are offered welfare improvements in lieu of exit). The mandatrophy is not 'which type is really correct?' but 'why does the same constraint appear so different depending on who is paying the cost?' The answer is: because extraction requires legitimation, and the welfare framework provides that legitimation by constructing primate moral status in a way that permits continued research use.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_model_sufficiency,
    'Do non-primate alternatives (in vitro, computational, human tissue models, organ-on-chip) provide equivalent predictive validity for human pharmacology and toxicology as primate models?',
    'Comparative validation studies: track FDA approval success rates for drugs developed with primate-tested vs primate-free protocols; clinical trial outcomes correlation; mechanism-of-action prediction accuracy across methods',
    'If alternatives are equivalent or superior: the welfare framework''s ''necessity'' justification fails, and the classification shifts from Snare (unavoidable extraction) to pure institutional choice. If primate models provide irreplaceable data: Snare classification is validated, but the framework remains more extraction than coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_model_sufficiency, empirical, 'Whether alternative models provide sufficient validity to replace primate testing').

omega_variable(
    welfare_standard_enforcement_reality,
    'How much of the difference between regulatory welfare standards and actual conditions in research facilities reflects enforcement gaps vs deliberate institutional non-compliance?',
    'Unannounced facility inspections; third-party welfare audits; cross-facility comparison of approved protocols vs observed conditions; whistleblower documentation; statistical correlation between facility size/prestige and approval rates vs observed welfare outcomes',
    'If enforcement gaps dominate: the framework is a legitimate coordination structure degraded by resource constraints (Tangled Rope remains valid). If non-compliance is systemic and deliberate: the framework is primarily theatrical legitimation (Piton classification elevated; Snare from primate perspective confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_standard_enforcement_reality, empirical, 'Whether welfare standard non-compliance is enforcement failure or systemic').

omega_variable(
    moral_status_and_extraction,
    'Does the welfare framework''s construction of primates as sentient beings deserving protection simultaneously construct them as usable research resources, creating a logical contradiction that the framework itself cannot resolve?',
    'Textual analysis of regulatory documents; comparison of moral status claims with permitted procedures; examination of jurisdictional differences in moral status vs permitted use; interviews with ethics committees on the contradiction between sentience claims and resource extraction permission',
    'If contradiction is foundational and unresolvable: the framework is intrinsically extractive, using moral recognition as legitimation for continued extraction (Snare classification; welfare framework as sophisticated suppression mechanism). If contradiction is solvable through refinement: the framework is a genuine Tangled Rope with potential for increasing coordination relative to extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_status_and_extraction, conceptual, 'Whether welfare framework contains unresolvable moral contradiction').

omega_variable(
    primates_as_conservation_proxy,
    'Does research capture and confinement reduce genetic diversity and breeding opportunities in wild primate populations, effectively causing conservation harm?',
    'Population genetics analysis of wild vs research-colony primate populations; tracking of individuals captured for research vs wild population trajectories; conservation impact modeling for species where research demand is significant',
    'If capture causes measurable conservation damage: the framework has a second victim group (conservation genetics) in addition to captive primates, elevating suppression and extraction metrics. The constraint becomes a dual-extraction mechanism — simultaneously extracting research value and conservation cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(primates_as_conservation_proxy, empirical, 'Whether research capture causes conservation harm to wild populations').

omega_variable(
    institutional_identity_lock,
    'Have research institutions and funding bodies developed identity structures around ''we are primate research institutions'' such that exiting primate-dependent research programs would threaten institutional self-concept and funding narratives?',
    'Institutional history analysis; tracking of mission statements and funding justifications; interviews with leadership about alternative research portfolios; correlation between institutional age and primate research integration; case studies of institutions that successfully transitioned away from primate research',
    'If institutions are identity-locked into primate research: even powerful actors (governmental, institutional) experience constrained exit options due to cognitive/identity barriers rather than material barriers. This could elevate the beneficiary''s experienced extraction classification from Rope (arbitrage available) to Tangled Rope or Snare (identity-locked, constrained). The constraint would be binding institutional identity, not just economic incentives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_identity_lock, empirical, 'Whether institutions are identity-locked into primate research dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(primate_welfare_policy_framework, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(primate_welfare_tr_t0, primate_welfare_policy_framework, theater_ratio, 0, 0.42).
narrative_ontology:measurement(primate_welfare_tr_t15, primate_welfare_policy_framework, theater_ratio, 15, 0.58).
narrative_ontology:measurement(primate_welfare_tr_t30, primate_welfare_policy_framework, theater_ratio, 30, 0.68).
narrative_ontology:measurement(primate_welfare_tr_t45, primate_welfare_policy_framework, theater_ratio, 45, 0.72).

% Extraction over time
narrative_ontology:measurement(primate_welfare_be_t0, primate_welfare_policy_framework, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(primate_welfare_be_t15, primate_welfare_policy_framework, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(primate_welfare_be_t30, primate_welfare_policy_framework, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(primate_welfare_be_t45, primate_welfare_policy_framework, base_extractiveness, 45, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(primate_welfare_policy_framework, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(primate_welfare_policy_framework, 0.18).
narrative_ontology:affects_constraint(primate_welfare_policy_framework, pharmaceutical_development_timeline_compression).
narrative_ontology:affects_constraint(primate_welfare_policy_framework, wild_primate_population_genetics).
narrative_ontology:affects_constraint(primate_welfare_policy_framework, research_ethics_legitimacy).

% DUAL FORMULATION NOTE:
% The primate welfare framework decomposes into at least two structurally distinct constraints: (1) captive primate welfare within facilities (extraction mechanism, Snare from primate perspective); (2) institutional use of welfare standards as legitimacy mechanism (coordination + extraction, Tangled Rope from institutional perspective). These are linked: the legitimacy function depends on the welfare mechanism existing, but the welfare mechanism's effectiveness is independent of its legitimacy function. Both stories share the same constraint_id in the network but could be decomposed if necessary to distinguish welfare-delivery function from welfare-legitimacy function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(primate_welfare_policy_framework, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
