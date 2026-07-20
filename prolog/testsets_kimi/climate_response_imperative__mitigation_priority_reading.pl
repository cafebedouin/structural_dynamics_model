% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__mitigation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__mitigation_priority_reading, []).

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
 *   constraint_id: climate_response_imperative__mitigation_priority_reading
 *   human_readable: Mitigation-Priority Climate Response Constraint
 *   domain: climate_policy/political_economy/intergenerational
 *
 * SUMMARY:
 *   This constraint is the mitigation_priority_reading of the contested
 *   kernel climate_response_imperative. It encodes the claim that climate
 *   response should be organized primarily around emissions reduction via
 *   technological innovation and market mechanisms, treating adaptation as
 *   residual. Structurally, it operates as a tangled rope: it coordinates
 *   genuine collective action on a planetary commons (the atmosphere) while
 *   asymmetrically extracting from future generations and climate-vulnerable
 *   regions by deferring adaptation finance and betting on unproven
 *   carbon-dioxide removal. Global North innovation sectors and carbon-market
 *   intermediaries are the concentrated beneficiaries. The claim and metrics
 *   are authored independently: the structural claim is tangled_rope because
 *   both coordination and extraction are present, while the metrics describe
 *   an increasingly extractive and theatrical trajectory as the gap between
 *   modeled mitigation and observed warming widens.
 *
 * KEY AGENTS:
 *   - Global North innovation sectors: Primary beneficiary (powerful/arbitrage) â capture public and private investment flows directed at green technology and CDR.
 *   - Carbon market intermediaries: Secondary beneficiary (organized/arbitrage) â extract fees from trading and verification volumes.
 *   - Future generations: Primary target (powerless/trapped) â inherit locked-in warming and unproven CDR debt with no seat at the table.
 *   - Climate vulnerable regions: Primary target (powerless/trapped) â bear deferred adaptation costs and climate impacts now.
 *   - Mitigation policy architects: Agenda-setter (institutional/mobile) â enforce the hierarchy that treats adaptation as residual and controls the UNFCCC process.
 *   - Adaptation advocates: Excluded voice (moderate/constrained) â structurally marginalized in finance and binding targets.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, 0.72).
domain_priors:suppression_score(climate_response_imperative__mitigation_priority_reading, 0.75).
domain_priors:theater_ratio(climate_response_imperative__mitigation_priority_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__mitigation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__mitigation_priority_reading, "Mitigation-Priority Climate Response Constraint").
narrative_ontology:topic_domain(climate_response_imperative__mitigation_priority_reading, "climate_policy/political_economy/intergenerational").

domain_priors:requires_active_enforcement(climate_response_imperative__mitigation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__mitigation_priority_reading, '75a873eb-4956-49f4-bf07-5e0be2548526').
narrative_ontology:cs_kernel_codification('75a873eb-4956-49f4-bf07-5e0be2548526', formalized).
narrative_ontology:cs_authority_grounding('75a873eb-4956-49f4-bf07-5e0be2548526', lineage).
narrative_ontology:cs_interpretation_layer_present('75a873eb-4956-49f4-bf07-5e0be2548526').
narrative_ontology:cs_reading_relation('75a873eb-4956-49f4-bf07-5e0be2548526', climate_response_imperative__adaptation_priority_reading, influences).
narrative_ontology:cs_reading_relation('75a873eb-4956-49f4-bf07-5e0be2548526', climate_response_imperative__degrowth_reading, forecloses).
narrative_ontology:cs_axiom('75a873eb-4956-49f4-bf07-5e0be2548526', foundational, technological_sufficiency).
narrative_ontology:cs_axiom_status(technological_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('75a873eb-4956-49f4-bf07-5e0be2548526', technological_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('75a873eb-4956-49f4-bf07-5e0be2548526', foundational, mitigation_hierarchy).
narrative_ontology:cs_axiom_status(mitigation_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('75a873eb-4956-49f4-bf07-5e0be2548526', mitigation_hierarchy, conventional).
narrative_ontology:cs_reference_frame('75a873eb-4956-49f4-bf07-5e0be2548526', atmospheric_stabilization_framework).
narrative_ontology:cs_drift_state('75a873eb-4956-49f4-bf07-5e0be2548526', contemporary_climate_policy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('75a873eb-4956-49f4-bf07-5e0be2548526', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__mitigation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, carbon_market_intermediaries).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, future_generations).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, climate_vulnerable_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% They design and deploy green technologies, carbon capture systems, and market mechanisms. They capture public and private investment flows directed toward mitigation innovations, with financial returns and intellectual property rents concentrated in OECD economies.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors, beneficiary,
    powerful, biographical, arbitrage, global).

% They trade offsets, verify credits, and structure compliance instruments. Their revenue depends directly on the volume of carbon traded under cap-and-trade or offset mechanisms legitimated by the mitigation-priority framework.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, carbon_market_intermediaries, beneficiary,
    organized, biographical, arbitrage, global).

% They face climate impacts now but receive residual adaptation funding. Their survival depends on mitigation success they did not cause and cannot control, while bearing the costs of deferred adaptation and loss-and-damage.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, climate_vulnerable_regions, payer,
    powerless, generational, trapped, global).

% They inherit the climate locked in by present emissions pathways and the unproven CDR bets made by current policy. They have no seat at negotiating tables and cannot exit the temporal commitment.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, future_generations, payer,
    powerless, civilizational, trapped, universal).

% They design NDCs, carbon pricing regimes, and technology roadmaps. They enforce the institutional hierarchy that treats adaptation as residual and relies on innovation and markets to deliver temperature targets, with professional legitimacy tied to this framing.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, mitigation_policy_architects, agenda_setter,
    institutional, generational, mobile, global).

% They argue for resilience, loss-and-damage finance, and local adaptation but are marginalized in resource allocation by the mitigation-first architecture. Their voices appear in assessment reports but not in binding targets or primary finance flows.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, adaptation_advocates, excluded,
    moderate, generational, constrained, global).

% They model pathways and assess technologies. Many operate within mitigation-framed funding priorities; some observe the structural bias toward technological solutions but must publish within institutions that reward integrated assessment modeling.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, climate_research_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global emissions reductions through shared metrics (tons CO2e), tradable instruments, nationally determined contributions, and technology diffusion frameworks to address the free-rider dynamics of the atmospheric commons.
% TRANSFER_FUNCTION: Moves present capital, policy attention, and moral priority toward mitigation technologies and carbon markets concentrated in the Global North, while transferring climate risk and deferred adaptation costs to vulnerable regions and future generations.
% ABSENT_VOICES: Future generations who cannot negotiate; climate-vulnerable regions with tokenistic representation; degrowth advocates arguing for consumption limits; indigenous land stewards whose practices fall outside carbon accounting frameworks.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority imperative vanished overnight, capital flows would shift from CDR and offset markets toward immediate adaptation and resilience; vulnerable regions would reorganize planning around direct protection rather than atmospheric stabilization bets; the global policy architecture would lose its organizing principle and competing framings would gain institutional purchase.
% FOUNDING_PROBLEM: The atmospheric commons is a global public good subject to free-rider dynamics; uncoordinated greenhouse-gas emissions lead to cumulative heating that harms all parties, requiring coordinated restraint.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists attest the problem is live because emissions continue to rise. Climate justice advocates and adaptation economists from the Global South attest the founding problem has been partially captured by wealth-preserving framings that defer real sacrifice and underfund adaptation. Independent development economists corroborate that the commons framing obscures differentiated responsibility and historical emissions.
narrative_ontology:disappearance_verdict(climate_response_imperative__mitigation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__mitigation_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__mitigation_priority_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_imperative__mitigation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__mitigation_priority_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__mitigation_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__mitigation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness rises from 0.30 to 0.72 over the interval because early UNFCCC coordination (1990s) genuinely reduced free-riding but increasingly shifted costs temporally and spatially onto those outside the negotiation. Suppression (0.75) is high because the framework actively marginalizes adaptation-first and degrowth alternatives through funding structures and scenario selection. Theater ratio (0.55) reflects the growing performative component: net-zero pledges, offset markets with questionable integrity, and reliance on speculative CDR that maintains the appearance of action while deferring hard choices. Accessibility collapse (0.58) is moderate because alternatives are intellectually visible but institutionally blocked â the IPCC produces adaptation chapters, but NDC architecture and finance flows lock in mitigation priority. Resistance (0.52) reflects climate justice movements and vulnerable-nation negotiating blocs that contest the framing but remain under-resourced relative to agenda-setters.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the constraint as necessary planetary coordination: without shared metrics and market mechanisms, the atmospheric commons collapses into uncoordinated overuse. The payer seats experience the same structure as deferred extraction: future generations see a bet on their future made with their inheritance; vulnerable regions see finance diverted to technologies that may never protect them. The engine should compute divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (global_north_innovation_sectors, carbon_market_intermediaries) hold mobile or arbitrage exit options and sit at powerful/organized power levels, driving their directionality toward the beneficiary pole. Victims (future_generations, climate_vulnerable_regions) are trapped â temporally locked or geographically exposed with no meaningful exit â driving directionality toward the full-target pole. The agenda-setter (mitigation_policy_architects) has mobile exit (can rotate across institutions) and derives professional legitimacy from the constraint, placing them near the beneficiary end but not at the extreme.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â atmospheric commons free-riding â remains live, which prevents pure snare classification. However, the mandate has drifted: the apparatus now preserves present economic structures and Global North consumption patterns by deferring real sacrifice onto future and vulnerable agents. This is not mandatrophy resolved; it is mandate drift. The classification as tangled_rope captures that the coordination function is real but has been captured by actors who benefit from its asymmetric operation. A pure snare reading would ignore the genuine collective-action structure; a pure rope reading would ignore the residualization of adaptation and the CDR deferral mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cdr_delivery_uncertainty,
    'Are the negative emissions technologies relied upon in mitigation pathways deliverable at scale, or are they a deferral device that increases eventual extraction if they fail?',
    'Observational tracking of CDR deployment against integrated assessment model assumptions, including direct air capture, BECCS, and natural sinks at gigatonne scale.',
    'If CDR fails to deliver at the assumed scale, future generations bear locked-in warming with no offset, raising effective extraction and potentially shifting the constraint type toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cdr_delivery_uncertainty, empirical, 'Whether speculative CDR is genuine mitigation or temporal extraction.').

omega_variable(
    adaptation_residualization,
    'Is adaptation treated as residual because mitigation is genuinely sufficient, or because mitigation framing protects present economic structures and Global North consumption patterns?',
    'Comparative budget analysis of mitigation versus adaptation finance; accounting of loss and damage against modeled adaptation gaps; examination of NDC resource allocations.',
    'If adaptation is systematically underfunded relative to modeled damages and mitigation costs, the constraint functions as extraction from vulnerable regions and the type asymmetry is validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_residualization, conceptual, 'Whether adaptation residualization is physical necessity or structural extraction.').

omega_variable(
    kernel_reading_position,
    'Is the mitigation-priority reading the only coherent operationalization of climate response, or does it represent one contested reading among structurally viable siblings?',
    'Comparative policy outcome analysis across jurisdictions and modeling frameworks adopting adaptation-priority or degrowth framings versus mitigation-priority.',
    'If sibling readings are operationally viable, the constraint''s high extraction is partially an artifact of framing monopoly rather than physical necessity, supporting the contested-kernel classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Contested kernel status of the climate response imperative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__mitigation_priority_reading, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__mitigation_priority_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clim_tr_t6, climate_response_imperative__mitigation_priority_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(clim_tr_t12, climate_response_imperative__mitigation_priority_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(clim_tr_t18, climate_response_imperative__mitigation_priority_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement(clim_tr_t24, climate_response_imperative__mitigation_priority_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(clim_tr_t30, climate_response_imperative__mitigation_priority_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(clim_tr_t34, climate_response_imperative__mitigation_priority_reading, theater_ratio, 34, 0.55).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(clim_be_t6, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(clim_be_t12, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(clim_be_t18, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 18, 0.5).
narrative_ontology:measurement(clim_be_t24, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(clim_be_t30, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(clim_be_t34, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 34, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(clim_su_t6, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement(clim_su_t12, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(clim_su_t18, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement(clim_su_t24, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement(clim_su_t30, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(clim_su_t34, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 34, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__mitigation_priority_reading, global_infrastructure).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, climate_response_imperative__adaptation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, climate_response_imperative__degrowth_reading).

% DUAL FORMULATION NOTE:
% This constraint decomposes the colloquial label 'climate response imperative' into three structurally distinct claims. The mitigation-priority reading (this file), adaptation-priority reading, and degrowth reading have different epsilon values, different beneficiary/victim structures, and different coordination functions. They form a constraint family linked by shared kernel origin but divergent structural commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
