% ============================================================================
% CONSTRAINT STORY: preparedness_retention__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__hybrid_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: preparedness_retention__hybrid_reading
 *   human_readable: Stratified Disaster Preparedness: Technical Core and Ceremonial Periphery
 *   domain: governance/institutional_memory/disaster_preparedness
 *
 * SUMMARY:
 *   This constraint story instantiates the hybrid_reading of the
 *   preparedness_retention kernel. In the Dutch disaster-preparedness regime,
 *   permanent technical institutions such as Rijkswaterstaat and regional
 *   water boards retain live operational competence in flood defense and
 *   water management. Meanwhile, municipalities and the broader public
 *   perform preparedness through drills, protocols, and awareness campaigns
 *   that have become largely ceremonial. The arrangement solves a genuine
 *   coordination problemâmaintaining hydraulic expertise across
 *   generationsâwhile asymmetrically extracting distributed resilience by
 *   making local self-reliance atrophy. The reading does not adjudicate
 *   between its siblings inside the constraint; it authors one clean
 *   Îµ-invariant claim.
 *
 * KEY AGENTS:
 *   - specialized_preparation_institutions: Primary agenda-setter (institutional/constrained) â retains live technical competence and sets preparedness standards.
 *   - municipal_governments: Primary payer (moderate/constrained) â performs ceremonial compliance and has lost operational capacity.
 *   - at_risk_communities: Secondary payer (powerless/trapped) â geographically fixed and dependent on centralized technical response.
 *   - peripheral_emergency_personnel: Tertiary payer (moderate/constrained) â staffs the ceremonial layer with neither authority nor deep expertise.
 *   - resilience_researchers: Analytical observer (analytical/analytical) â documents the core-periphery competence gap.
 *   - local_indigenous_knowledge_holders: Excluded voice (powerless/trapped) â holds delegitimized place-based memory.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, 0.62).
domain_priors:suppression_score(preparedness_retention__hybrid_reading, 0.55).
domain_priors:theater_ratio(preparedness_retention__hybrid_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_retention__hybrid_reading, "Stratified Disaster Preparedness: Technical Core and Ceremonial Periphery").
narrative_ontology:topic_domain(preparedness_retention__hybrid_reading, "governance/institutional_memory/disaster_preparedness").

domain_priors:requires_active_enforcement(preparedness_retention__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__hybrid_reading, '40992a32-42f8-4266-a7c3-cb5e12982308').
narrative_ontology:cs_kernel_codification('40992a32-42f8-4266-a7c3-cb5e12982308', formalized).
narrative_ontology:cs_authority_grounding('40992a32-42f8-4266-a7c3-cb5e12982308', expertise).
narrative_ontology:cs_interpretation_layer_present('40992a32-42f8-4266-a7c3-cb5e12982308').
narrative_ontology:cs_reading_relation('40992a32-42f8-4266-a7c3-cb5e12982308', preparedness_retention__competence_reading, influences).
narrative_ontology:cs_reading_relation('40992a32-42f8-4266-a7c3-cb5e12982308', preparedness_retention__husk_reading, forecloses).
narrative_ontology:cs_axiom('40992a32-42f8-4266-a7c3-cb5e12982308', foundational, institutional_specialization_preserves_operational_competence).
narrative_ontology:cs_axiom_status(institutional_specialization_preserves_operational_competence, holdable).
narrative_ontology:cs_axiom_grounding('40992a32-42f8-4266-a7c3-cb5e12982308', institutional_specialization_preserves_operational_competence, instrumental).
narrative_ontology:cs_axiom('40992a32-42f8-4266-a7c3-cb5e12982308', foundational, distributed_preparedness_inevitably_decays).
narrative_ontology:cs_axiom_status(distributed_preparedness_inevitably_decays, holdable).
narrative_ontology:cs_axiom_grounding('40992a32-42f8-4266-a7c3-cb5e12982308', distributed_preparedness_inevitably_decays, empirically_contingent).
narrative_ontology:cs_reference_frame('40992a32-42f8-4266-a7c3-cb5e12982308', functional_dual_track_preparation).
narrative_ontology:cs_drift_state('40992a32-42f8-4266-a7c3-cb5e12982308', contemporary_climate_stress_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('40992a32-42f8-4266-a7c3-cb5e12982308', '').
narrative_ontology:cs_kernel_id(preparedness_retention__hybrid_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, specialized_preparation_institutions).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, municipal_governments).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, at_risk_communities).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, peripheral_emergency_personnel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain live technical competence in flood defense and integrated water management through permanent engineering staffs, intergenerational training, and continuous infrastructure maintenance. Set national and regional preparedness standards and protocols. Their institutional continuity, budgets, and professional status depend on the monopoly over technical legitimacy.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, specialized_preparation_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Legally required to conduct civil defense drills, maintain emergency plans, and coordinate with specialized agencies. Actual operational capacity has atrophied over decades; they now perform scripts written by the core institutions and depend entirely on those institutions for any meaningful technical response. Compliance consumes budget without building local competence.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, municipal_governments, payer,
    moderate, biographical, constrained, regional).

% Reside in protected floodplains behind centralized hydraulic infrastructure. Participate in public awareness campaigns and ceremonial drills. Lack meaningful autonomous preparedness capacity; survival during crisis depends on the specialized institutions functioning correctly. Cannot geographically or economically exit the risk zone.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, at_risk_communities, payer,
    powerless, biographical, trapped, local).

% Staff municipal and regional emergency offices, executing preparedness protocols designed by specialized agencies. Their work maintains the appearance of a distributed response capacity but lacks decision-making authority or deep technical expertise. Employment and career paths are bound to the ceremonial apparatus.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, peripheral_emergency_personnel, payer,
    moderate, biographical, constrained, regional).

% Study preparedness systems and publish empirical findings on the growing gap between centralized institutional competence and distributed societal resilience. Their analysis is cited by reformers and parliamentary inquiries but does not directly alter resource flows or institutional mandates.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, resilience_researchers, observer,
    analytical, generational, analytical, national).

% Hold place-based flood memory, informal response traditions, and historical adaptation knowledge that have been delegitimized by professionalized preparedness frameworks. Not invited to planning tables or protocol-design processes; their expertise is treated as obsolete folk knowledge rather than operational memory.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, local_indigenous_knowledge_holders, excluded,
    powerless, generational, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__hybrid_reading, specialized_preparation_institutions).
narrative_ontology:fixing_cost_class(preparedness_retention__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains complex hydraulic and emergency-response expertise across generational turnover by concentrating resources, training, and operational authority in permanent specialized institutions that would otherwise fragment.
% TRANSFER_FUNCTION: Moves financial and human resources from general governance and local communities to specialized technical agencies, while transferring risk liability and dependency back to the periphery.
% ABSENT_VOICES: Local communities with place-based flood memory, small-scale civic defense associations, critical infrastructure operators outside the water sector, and future generations who bear concentrated single-point-of-failure risk are structurally excluded from preparedness design.
% DISAPPEARANCE_RATIONALE: If the specialized institutions vanished, the technical coordination of flood defense would collapse because the peripheral system cannot perform core hydraulic functions. If the stratified constraint itself vanished and competence were suddenly required to be distributed, the current social equilibrium would rearrange as municipalities rebuilt capacity or the hollow periphery was exposed.
% FOUNDING_PROBLEM: How to preserve complex technical water-management and civil-defense competence across decades without relying on constant crisis activation.
% FOUNDING_PROBLEM_CORROBORATION: Disaster historians and engineers outside the benefiting agencies attest that technical complexity requires institutional specialization. Resilience researchers and critical infrastructure scholars attest that the resulting centralized dependency creates a brittle single-point-of-failure that undermines the original protective purpose.
narrative_ontology:disappearance_verdict(preparedness_retention__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_retention__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__hybrid_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high because the stratified system extracts distributed resilience by professionalizing preparedness and rendering local alternatives structurally unnecessary. Suppression (0.55) is moderate: local self-reliance is not banned but is defunded, delegitimized, and displaced by central protocols. Theater_ratio (0.50) reflects that roughly half the systemâthe peripheral municipal and public layerâoperates as performance rather than function. Accessibility_collapse (0.65) captures how difficult reverting to distributed competence would be after decades of centralized specialization. Resistance (0.30) is low because the arrangement is widely accepted as modern, rational governance.
 *
 * PERSPECTIVAL GAP:
 *   From the specialized institutions, the constraint is a successful expert system preserving irreplaceable technical capacity. From municipal governments, it is a compliance treadmill that consumes resources without building real local capacity. From at-risk communities, it is an opaque dependency on distant agencies. The engine computes these divergent seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   specialized_preparation_institutions are the structural beneficiary: they receive budgetary flows, legal mandate, and professional status from the arrangement (low directionality). Municipal governments, peripheral emergency personnel, and at-risk communities are the payers: they bear compliance costs, atrophy of autonomous capacity, and concentrated risk exposure (high directionality). The divergence is driven by power and exit asymmetryâthe core is institutionally entrenched while the periphery is legally and geographically trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreserving complex technical competenceâremains live, so the constraint has not undergone full mandatrophy. However, the solution has developed a ceremonial appendage that no longer serves the founding purpose. Classifying the whole arrangement as tangled_rope prevents the error of calling it a rope (ignoring the peripheral extraction) or a snare (ignoring the real technical coordination in the core). The theater_ratio metric captures the appendage's growth without denying the core function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    core_periphery_boundary_ambiguity,
    'Is the boundary between technical core and ceremonial periphery a sharp institutional divide or a continuous gradient of competence?',
    'Ethnographic and operational audit of decision-making capacity across tiers of government and agencies during simulated and actual events.',
    'A gradient would reduce the effective extraction by showing distributed competence still exists; a sharp boundary confirms the dual-track structure and supports the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(core_periphery_boundary_ambiguity, empirical, 'Whether competence decay is continuous or dichotomous across institutional tiers.').

omega_variable(
    ceremony_coordination_function,
    'Does the ceremonial periphery serve any genuine coordination function (social solidarity, signaling, latent network activation) or is it purely performative?',
    'Comparative outcome analysis of disaster response in jurisdictions with more versus less emphasis on broad ceremonial preparedness.',
    'If ceremony has latent function, the base extractiveness is overstated and the constraint moves toward rope; if purely performative, the metric is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremony_coordination_function, conceptual, 'Whether peripheral ceremony is functional or pure theater.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of local alternatives structural (centralized budgets and legal mandates) or internalized (belief that only accredited experts can manage risk)?',
    'Behavioral analysis of local governments when granted devolved emergency budgets and legal autonomy.',
    'If internalized, effective suppression exceeds structural measures because local actors self-limit even when formal barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of distributed preparedness.').

omega_variable(
    kernel_reading_validity,
    'Which reading of the preparedness_retention kernelâcompetence, husk, or hybridâbest matches the empirical distribution of operational capacity?',
    'Standardized competence testing across core agencies, municipal offices, and community organizations coupled with post-incident after-action reviews.',
    'Would reclassify the constraint to rope (competence), snare or piton (husk), or confirm tangled_rope (hybrid).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_validity, empirical, 'Empirical adjudication among the three readings of the preparedness kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__hybrid_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(prep_tr_t8, preparedness_retention__hybrid_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(prep_tr_t16, preparedness_retention__hybrid_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(prep_tr_t24, preparedness_retention__hybrid_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(prep_tr_t32, preparedness_retention__hybrid_reading, theater_ratio, 32, 0.47).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__hybrid_reading, theater_ratio, 40, 0.5).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(prep_be_t8, preparedness_retention__hybrid_reading, base_extractiveness, 8, 0.3).
narrative_ontology:measurement(prep_be_t16, preparedness_retention__hybrid_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(prep_be_t24, preparedness_retention__hybrid_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(prep_be_t32, preparedness_retention__hybrid_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__hybrid_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__hybrid_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(prep_su_t8, preparedness_retention__hybrid_reading, suppression_requirement, 8, 0.25).
narrative_ontology:measurement(prep_su_t16, preparedness_retention__hybrid_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(prep_su_t24, preparedness_retention__hybrid_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement(prep_su_t32, preparedness_retention__hybrid_reading, suppression_requirement, 32, 0.5).
narrative_ontology:measurement(prep_su_t40, preparedness_retention__hybrid_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__competence_reading).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__husk_reading).

% DUAL FORMULATION NOTE:
% The preparedness_retention kernel decomposes into three structurally distinct constraints because the natural-language label 'preparedness' conflates universal competence, universal husk, and stratified hybrid arrangements. Each reading carries a different Îµ, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
