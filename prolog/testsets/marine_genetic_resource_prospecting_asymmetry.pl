% ============================================================================
% CONSTRAINT STORY: marine_genetic_resource_prospecting_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marine_genetic_resource_prospecting_asymmetry, []).

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
 *   constraint_id: marine_genetic_resource_prospecting_asymmetry
 *   human_readable: Marine Genetic Resource Prospecting Asymmetry
 *   domain: bioeconomics/environmental_governance
 *
 * SUMMARY:
 *   Marine genetic resource prospecting creates a structural asymmetry
 *   between organizations with capital, technological infrastructure, and IP
 *   system participation (primarily developed-nation pharmaceutical firms and
 *   research institutions) and source communities and developing nations
 *   whose marine territories harbor the genetic diversity being prospected.
 *   The Nagoya Protocol on Access and Benefit Sharing (2014) established a
 *   governance framework requiring Prior Informed Consent (PIC) and mandatory
 *   benefit-sharing agreements, creating the appearance of reciprocal
 *   coordination. However, structural asymmetries in technical capacity,
 *   capital availability, and enforcement mechanisms mean that benefits
 *   accrue overwhelmingly to prospecting firms and developed-nation research
 *   institutions. The constraint exhibits characteristics of both pure
 *   extraction (Snare) for powerless agents and coordinated asymmetric
 *   extraction (Tangled Rope) at the regulatory level. The theater_ratio has
 *   increased over the decade as institutional compliance apparatus has grown
 *   (PIC documentation, benefit-sharing agreements) while enforcement
 *   capacity and actual repatriation of benefits have remained flat. This
 *   pattern—increasing institutional theater without corresponding functional
 *   verification—indicates Piton-like degradation of the Nagoya framework.
 *
 * KEY AGENTS:
 *   - Pharmaceutical Biotechnology Firm: Primary beneficiary (institutional/arbitrage) — captures IP value from discovered bioactive compounds with minimal royalty obligations; can arbitrage between multiple EEZs and research partnerships
 *   - Coastal Developing Nation: Primary victim (powerless/trapped) — territorial sovereign over marine genetic resources but lacks prospecting capacity; trapped by capital and technical barriers; benefits flow upstream despite legal resource ownership
 *   - Indigenous Maritime Community: Primary victim (powerless/trapped) — traditional knowledge of marine organisms extracted without compensation; trapped by absence of legal protection for indigenous IP rights; epistemic marginalization prevents knowledge valorization
 *   - Developing Nation Regulatory Authority: Secondary actor (organized/constrained) — implements Nagoya Protocol access controls and PIC requirements; experiences asymmetric information and weak offshore enforcement capacity; constrained by monitoring gaps and ABNJ jurisdiction void
 *   - Developed Nation Research Institution: Secondary beneficiary (powerful/mobile) — accesses novel marine organisms through collaborative frameworks; converts shared discoveries into proprietary knowledge through IP systems; mobile across multiple partnership options
 *   - Nagoya Protocol Implementation Framework: Institutional system (institutional/arbitrage) — maintains performative compliance apparatus; sees own verification mechanisms as degraded; theater persists through institutional inertia and aspirational goals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marine_genetic_resource_prospecting_asymmetry, 0.58).
domain_priors:suppression_score(marine_genetic_resource_prospecting_asymmetry, 0.65).
domain_priors:theater_ratio(marine_genetic_resource_prospecting_asymmetry, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marine_genetic_resource_prospecting_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(marine_genetic_resource_prospecting_asymmetry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(marine_genetic_resource_prospecting_asymmetry, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marine_genetic_resource_prospecting_asymmetry, tangled_rope).
narrative_ontology:human_readable(marine_genetic_resource_prospecting_asymmetry, "Marine Genetic Resource Prospecting Asymmetry").
narrative_ontology:topic_domain(marine_genetic_resource_prospecting_asymmetry, "bioeconomics/environmental_governance").

domain_priors:requires_active_enforcement(marine_genetic_resource_prospecting_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marine_genetic_resource_prospecting_asymmetry, pharmaceutical_biotechnology_firms).
narrative_ontology:constraint_beneficiary(marine_genetic_resource_prospecting_asymmetry, developed_nation_research_institutions).
narrative_ontology:constraint_victim(marine_genetic_resource_prospecting_asymmetry, coastal_developing_nations).
narrative_ontology:constraint_victim(marine_genetic_resource_prospecting_asymmetry, indigenous_maritime_communities).
narrative_ontology:constraint_victim(marine_genetic_resource_prospecting_asymmetry, marine_biodiversity_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COASTAL DEVELOPING NATION (SNARE) — Territorial sovereignty over marine genetic resources within EEZ, but lacks capacity to conduct prospecting independently. Trapped by resource constraints (sequencing technology, bioactive compound screening infrastructure, regulatory expertise). Benefits flow upstream to developed nation biotech firms regardless of nation's institutional capacity. High suppression: capital barriers, technical expertise concentration, and the regulatory requirement to demonstrate prior informed consent without reciprocal benefit-sharing enforcement.
constraint_indexing:constraint_classification(marine_genetic_resource_prospecting_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INDIGENOUS MARITIME COMMUNITY (SNARE) — Traditional knowledge of bioactive marine organisms is extracted without compensation. Trapped by legal systems that do not recognize indigenous property rights over knowledge, lack of formal documentation of traditional practices, and structural dependency on marine resources. Prospecting firms can access knowledge through ethnobotanical databases, published literature, or community interaction without triggering benefit-sharing obligations. Suppression is particularly high: epistemic marginalization, language barriers, and the framing of 'traditional knowledge' as pre-scientific and therefore unpatentable.
constraint_indexing:constraint_classification(marine_genetic_resource_prospecting_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: DEVELOPING NATION REGULATORY AUTHORITY (TANGLED ROPE) — Genuinely coordinates benefit-sharing and access control through permitting and PIC (Prior Informed Consent) frameworks required by the Nagoya Protocol. Also coordinates marine resource management and biodiversity conservation. BUT experiences asymmetric extraction: limited capacity to monitor offshore prospecting activities, weak enforcement against biopiracy, and asymmetric information (prospectors know which organisms are commercially valuable; authorities do not). Organized but constrained by technical and institutional capacity gaps. Benefits from coordination framework (legitimacy, access fees) offset by enforcement asymmetry.
constraint_indexing:constraint_classification(marine_genetic_resource_prospecting_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PHARMACEUTICAL BIOTECHNOLOGY FIRM (ROPE) — Experiences the constraint as pure coordination: access rules clarify which marine organisms can be screened, PIC requirements structure negotiations, benefit-sharing agreements enable risk management. The firm can arbitrage between multiple EEZs and research partnerships. Extraction runs toward the firm: they capture disproportionate value from marine genetic resources relative to the source communities' compensation. Net beneficiary — coordination function (access clarity, risk framework) is genuine, but asymmetric.
constraint_indexing:constraint_classification(marine_genetic_resource_prospecting_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DEVELOPED NATION RESEARCH INSTITUTION (TANGLED ROPE) — Coordinates scientific exchange and collaborative research frameworks with developing nation partners (genuine benefit-sharing in some cases). Also experiences extraction: access to genetic libraries developed by centuries of biodiversity evolution, converted into proprietary knowledge through patent systems. Powerful and mobile (can conduct research on diverse organisms), but embedded in global intellectual property asymmetries that privilege developed-nation patent holders. Benefits from coordination (collaborative legitimacy, access to novel organisms) alongside structural advantage in converting shared discoveries into private IP.
constraint_indexing:constraint_classification(marine_genetic_resource_prospecting_asymmetry, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: NAGOYA PROTOCOL IMPLEMENTATION FRAMEWORK (PITON) — The treaty framework establishes benefit-sharing and access control principles, but enforcement is largely performative. Theater ratio is high: countries declare compliance through paperwork (PIC documentation, benefit-sharing agreements), but actual monitoring of marine prospecting at sea is minimal. The framework persists through institutional inertia and legitimate aspirational goals, but the verification mechanisms are weak. High-seas prospecting largely bypasses the framework entirely (genetic resources in areas beyond national jurisdiction—ABNJ—remain largely unregulated). Theater ratio reflects the gap between the institutional apparatus and actual enforcement capacity.
constraint_indexing:constraint_classification(marine_genetic_resource_prospecting_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, genetic advantage for developed nations with capital-intensive prospecting infrastructure is structurally inherent to industrial biology. The constraint appears as a natural law: disparities in technological capacity, capital access, and IP system participation inevitably concentrate benefits. However, this perspective risks naturalizing a contingent institutional arrangement (IP system design, capital allocation, research infrastructure concentration) as an immutable feature of biology.
constraint_indexing:constraint_classification(marine_genetic_resource_prospecting_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marine_genetic_resource_prospecting_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marine_genetic_resource_prospecting_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marine_genetic_resource_prospecting_asymmetry, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(marine_genetic_resource_prospecting_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(marine_genetic_resource_prospecting_asymmetry, TR),
    TR >= 0.70.

:- end_tests(marine_genetic_resource_prospecting_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, trending upward. The constraint extracts genetic resources (collective evolutionary heritage) and traditional knowledge (cumulative community knowledge) and converts them into private IP with asymmetric compensation. The extractiveness is not maximum (0.8+) because some benefit-sharing does occur under Nagoya, and developing-nation regulatory capacity is gradually improving. The upward trend reflects increasing sophistication in prospecting technology (genomic screening, bioactivity prediction) without corresponding increases in benefit-sharing rates—extraction efficiency is improving. Suppression (0.65): High. Multiple barriers prevent developing nations and indigenous communities from capturing proportional value: capital barriers to conducting prospecting independently (sequencing, bioactivity screening infrastructure costs $millions), technical expertise concentration (advanced biotech skills concentrated in developed nations), legal barriers (indigenous knowledge not recognized in IP systems), information asymmetry (prospectors know which organisms are commercially valuable before PIC negotiations), and regulatory monitoring gaps (offshore prospecting largely undetectable). High-seas prospecting bypasses Nagoya entirely, creating a regulatory void. Theater ratio (0.48): Moderate and increasing. The Nagoya Protocol framework has established extensive institutional apparatus (PIC documentation, benefit-sharing agreements, national focal points), but actual benefit flows remain minimal and enforcement is weak. The constraint shows the characteristic piton pattern: institutional theater increasing while functional verification decreases.
 *
 * PERSPECTIVAL GAP:
 *   The gap reveals the core structural claim: the same Nagoya framework appears as legitimate coordination (Rope) to the beneficiary, asymmetric coordination (Tangled Rope) to the constrained regulator, pure extraction (Snare) to the powerless victim, and degraded theater (Piton) to the institutional framework itself. This presheaf over the observation site is the complete description: no single type captures all perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect structural position in the extraction flow. Powerless developing nations and indigenous communities are full targets of extraction (d ≈ 0.95): they have no exit options and all benefits flow away from them, producing maximum f(d) ≈ 1.42. The developing-nation regulatory authority is partial target with some agency (d ≈ 0.55): constrained exit options (must implement Nagoya to maintain legitimacy) and mixed costs/benefits (enforcement asymmetry vs. coordination function), producing moderate f(d) ≈ 0.75. Pharmaceutical beneficiaries are low-d agents (d ≈ 0.10): arbitrage exit options and net benefit flow, producing negative f(d) ≈ -0.01. Developed-nation research institutions are mixed (d ≈ 0.45): powerful and mobile but embedded in asymmetric IP systems, producing intermediate f(d) ≈ 0.55. The directionality profile explains why the primary beneficiaries (lowest d) experience the constraint as Rope while primary targets (highest d) experience it as Snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy by accurately distinguishing coordination from extraction. Developing-nation regulatory authorities genuinely coordinate access control and benefit-sharing (Tangled Rope is correct). Pharmaceutical firms genuinely coordinate scientific exchange frameworks (Rope is correct). But this coordination is wrapped in asymmetric extraction: the structure coordinates who has access and how benefits are nominally shared, but the actual benefit flow is overwhelmingly asymmetric. The mandatrophy would arise if claiming the constraint is 'just coordination' (ignoring the snare perspective of powerless victims) or 'pure extraction' (ignoring the genuine coordination of the Nagoya framework). The tangled_rope classification captures both: the coordination is real, and the extraction is real, and they operate simultaneously in the same institutional mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    traditional_knowledge_valuation_ambiguity,
    'Is traditional knowledge about marine bioactivity a distinct intellectual contribution warranting independent compensation, or part of the marine genetic resource itself?',
    'Legal precedent analysis of how indigenous knowledge claims have been adjudicated in benefit-sharing disputes; empirical tracing of compound discovery attributions (which discoveries trace to documented traditional use vs. independent prospecting)',
    'If distinct intellectual contribution: indigenous communities should receive separate compensation tier from genetic resource extraction. If part of the resource: compensation structure already addresses it. This determines whether snare classification for indigenous communities is accurate or understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditional_knowledge_valuation_ambiguity, conceptual, 'Whether traditional marine knowledge is a distinct IP category or embedded in genetic resource extraction').

omega_variable(
    biopiracy_detection_asymmetry,
    'How much marine prospecting occurs without documented PIC or benefit-sharing agreements, and what portion represents undetected biopiracy vs. ABNJ (high-seas) prospecting that falls outside Nagoya scope?',
    'Institutional audit of prospecting firm compliance records; patent literature analysis tracking organism sourcing; interviews with regulatory authorities on detection and enforcement capacity',
    'If detection rate is <30%: suppression is higher than measured (0.65), constraint approaches Snare threshold for regulatory authority. If >70%: framework is more effective than piton classification suggests. This determines whether theater_ratio (0.48) understates performativity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(biopiracy_detection_asymmetry, empirical, 'Scale of undetected biopiracy and ABNJ prospecting outside regulatory scope').

omega_variable(
    high_seas_genetic_prospecting_governance_gap,
    'Should genetic resources in areas beyond national jurisdiction (ABNJ) be brought under Nagoya Protocol or a successor regime with stronger enforcement?',
    'International negotiations on BBNJ (Biodiversity Beyond National Jurisdiction) agreement implementation; tracking which nations ratify high-seas genetic resource provisions',
    'If ABNJ resources are brought under global benefit-sharing: suppression decreases, constraint becomes more balanced (Tangled Rope across more perspectives). If ABNJ remains a regulatory void: suppression persists, constraint remains Snare for high-seas prospecting. This determines the constraint''s long-term classification trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(high_seas_genetic_prospecting_governance_gap, preference, 'Governance architecture for high-seas genetic prospecting').

omega_variable(
    synthetic_biology_substitution_rate,
    'As synthetic biology, computational protein design, and cell-free synthesis advance, what fraction of ''marine genetic resource prospecting'' shifts from sampling organisms to synthesizing candidate compounds computationally?',
    'Patent literature analysis of compound discovery methods in marine biotech 2020-2030; tracking of synthetic vs. natural-derived lead compound ratios in pharma pipelines',
    'If synthetic substitution accelerates: extractiveness decreases (genetic prospecting becomes less valuable), constraint may shift toward Rope or Scaffold. If substitution stalls: extractiveness persists or increases as scarcity value rises. This determines whether the constraint has an inherent sunset horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthetic_biology_substitution_rate, empirical, 'Substitution of marine genetic prospecting by synthetic biology methods').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marine_genetic_resource_prospecting_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mgr_tr_t0, marine_genetic_resource_prospecting_asymmetry, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mgr_tr_t5, marine_genetic_resource_prospecting_asymmetry, theater_ratio, 5, 0.42).
narrative_ontology:measurement(mgr_tr_t10, marine_genetic_resource_prospecting_asymmetry, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(mgr_be_t0, marine_genetic_resource_prospecting_asymmetry, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mgr_be_t5, marine_genetic_resource_prospecting_asymmetry, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(mgr_be_t10, marine_genetic_resource_prospecting_asymmetry, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marine_genetic_resource_prospecting_asymmetry, resource_allocation).
narrative_ontology:affects_constraint(marine_genetic_resource_prospecting_asymmetry, pharmaceutical_ip_asymmetry).
narrative_ontology:affects_constraint(marine_genetic_resource_prospecting_asymmetry, indigenous_knowledge_appropriation).
narrative_ontology:affects_constraint(marine_genetic_resource_prospecting_asymmetry, marine_biodiversity_commons_tragedy).

% DUAL FORMULATION NOTE:
% Marine genetic resource prospecting asymmetry is downstream of multiple structural constraints: IP system design (pharmaceutical patents concentrate developed-nation advantage), traditional knowledge marginalization (indigenous knowledge is epistemically devalued), and marine biodiversity as a commons (collective resources, dispersed appropriation). Each upstream constraint has its own extractiveness value. This constraint represents the coordination mechanism that operationalizes the asymmetries created by upstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marine_genetic_resource_prospecting_asymmetry, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
