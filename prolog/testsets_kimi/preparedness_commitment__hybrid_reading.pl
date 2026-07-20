% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__hybrid_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: preparedness_commitment__hybrid_reading
 *   human_readable: Preparedness as Layered Memorial-Competence System
 *   domain: institutional/disaster_preparedness
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid_reading of the
 *   preparedness_commitment kernel: preparedness is understood as a layered
 *   system in which memorial elements stabilize long-term commitment while
 *   competence elements maintain operational function. The constraint governs
 *   how institutions allocate resources and attention across inter-crisis
 *   periods. It is actively enforced through intergovernmental grant
 *   requirements, training mandates, and audit regimes that demand both
 *   exercised capability and commemorative participation. The claim is
 *   tangled_rope because the arrangement genuinely coordinates disaster
 *   readiness but also extracts maintenance costs from frontline operators
 *   and taxpayers through its insistence on fused institutional performance.
 *
 * KEY AGENTS:
 *   - emergency_management_institutions: Primary agenda-setter (institutional/national) â administers standards, funding, and audits that enforce the dual-layer mandate.
 *   - memorial_institutions: Secondary beneficiary (moderate/regional) â receives public funding to preserve memory and conduct commemorative rituals.
 *   - frontline_operators: Primary target (moderate/constrained) â bears the tension between competence maintenance and memorial duties.
 *   - municipal_taxpayers: Secondary target (organized/constrained) â funds both layers through taxation without opt-out.
 *   - disaster_exposed_public: Diffuse beneficiary (organized/constrained) â receives risk reduction and commitment stability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, 0.6).
domain_priors:suppression_score(preparedness_commitment__hybrid_reading, 0.5).
domain_priors:theater_ratio(preparedness_commitment__hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_commitment__hybrid_reading, "Preparedness as Layered Memorial-Competence System").
narrative_ontology:topic_domain(preparedness_commitment__hybrid_reading, "institutional/disaster_preparedness").

domain_priors:requires_active_enforcement(preparedness_commitment__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__hybrid_reading, 'b0f98ff6-dee8-4f73-a1aa-eceff8b128d1').
narrative_ontology:cs_kernel_codification('b0f98ff6-dee8-4f73-a1aa-eceff8b128d1', distributed).
narrative_ontology:cs_authority_grounding('b0f98ff6-dee8-4f73-a1aa-eceff8b128d1', practice).
narrative_ontology:cs_interpretation_layer_present('b0f98ff6-dee8-4f73-a1aa-eceff8b128d1').
narrative_ontology:cs_reading_relation('b0f98ff6-dee8-4f73-a1aa-eceff8b128d1', preparedness_commitment__competence_reading, influences).
narrative_ontology:cs_reading_relation('b0f98ff6-dee8-4f73-a1aa-eceff8b128d1', preparedness_commitment__husk_reading, forecloses).
narrative_ontology:cs_axiom('b0f98ff6-dee8-4f73-a1aa-eceff8b128d1', foundational, competence_requires_memorial_commitment).
narrative_ontology:cs_axiom_status(competence_requires_memorial_commitment, holdable).
narrative_ontology:cs_axiom_grounding('b0f98ff6-dee8-4f73-a1aa-eceff8b128d1', competence_requires_memorial_commitment, empirically_contingent).
narrative_ontology:cs_axiom('b0f98ff6-dee8-4f73-a1aa-eceff8b128d1', foundational, memorial_without_competence_fails).
narrative_ontology:cs_axiom_status(memorial_without_competence_fails, holdable).
narrative_ontology:cs_axiom_grounding('b0f98ff6-dee8-4f73-a1aa-eceff8b128d1', memorial_without_competence_fails, empirically_contingent).
narrative_ontology:cs_reference_frame('b0f98ff6-dee8-4f73-a1aa-eceff8b128d1', layered_commitment_system).
narrative_ontology:cs_drift_state('b0f98ff6-dee8-4f73-a1aa-eceff8b128d1', contemporary_austerity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b0f98ff6-dee8-4f73-a1aa-eceff8b128d1', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__hybrid_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, emergency_management_institutions).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, memorial_institutions).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, disaster_exposed_public).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, frontline_operators).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, municipal_taxpayers).
narrative_ontology:constraint_vindicates(preparedness_commitment__hybrid_reading, institutional_memory_preserves_commitment).
narrative_ontology:constraint_vindicates(preparedness_commitment__hybrid_reading, dual_layer_preparedness_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers preparedness grants, sets training standards, and audits local agencies. Their authority and budget depend on maintaining a visible preparedness system that satisfies political principals. They enforce the dual-layer mandate: competence metrics and memorial participation requirements.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, emergency_management_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Museums, memorial sites, and commemorative programs that receive public funding to preserve disaster memory and conduct annual ceremonies. Their institutional survival is tied to the preparedness system's commitment to memorial stabilization; they do not deliver operational response capacity.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, memorial_institutions, beneficiary,
    moderate, biographical, constrained, regional).

% Firefighters, emergency medical services, and local response personnel who must maintain equipment readiness, certifications, and exercised drills while also participating in memorial ceremonies, public commemorations, and documentation rituals that stabilize political commitment. The tension between the two layers consumes time and operational budget.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, frontline_operators, payer,
    moderate, biographical, constrained, regional).

% Fund both emergency response capabilities and memorial infrastructure through local taxation. They cannot opt out of funding the memorial layer even when they would prefer resources directed entirely toward competence; the bundled funding mechanism is enforced by intergovernmental grant requirements.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, municipal_taxpayers, payer,
    organized, biographical, constrained, local).

% Receive reduced disaster risk from maintained operational competence and psychological reassurance from visible memorial commitment. They have limited ability to audit whether the layered system is genuinely functional or performative, and cannot selectively opt out of the institutional arrangement.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, disaster_exposed_public, beneficiary,
    organized, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains societal capacity to respond to disasters across long inter-crisis periods by combining operational readiness with institutionalized memory that prevents policy abandonment and funding attrition.
% TRANSFER_FUNCTION: Moves financial resources from municipal taxpayers and labor resources from frontline operators to emergency management and memorial institutions, in exchange for maintained disaster readiness and stabilized public commitment.
% ABSENT_VOICES: Communities permanently displaced by past disasters, and frontline operators who would prefer to defund memorial ceremonies in favor of equipment maintenance, are underrepresented in preparedness planning and standard-setting.
% DISAPPEARANCE_RATIONALE: If the layered system vanished, emergency management would collapse into either pure competence (risking abandonment during quiet periods) or pure memorial (risking catastrophic failure when competence is needed); the institutional form of preparedness would reorganize around whichever reading dominated locally.
% FOUNDING_PROBLEM: Disaster preparedness suffers from a cycle of investment after crises and abandonment during quiet periods, leading to catastrophic surprise when the next event occurs.
% FOUNDING_PROBLEM_CORROBORATION: Disaster sociologists and public administration scholars outside the emergency management profession attest to the boom-bust funding cycle; independent audits of post-disaster responses routinely identify competence decay during inter-crisis periods as a contributing factor to failure.
narrative_ontology:disappearance_verdict(preparedness_commitment__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_commitment__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__hybrid_reading, 0.6, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_commitment__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60) is moderate-high because the system bundles genuine coordination with memorial overhead that is not directly risk-reducing. Suppression (0.50) reflects moderate coercion: grant conditions and audit mandates enforce participation but do not fully close alternatives. Theater ratio (0.40) captures the growing performative load of the memorial layer over the 25-year interval. Accessibility collapse (0.50) indicates that alternatives (pure competence or pure memorial approaches) are visible but institutionally devalued. Resistance (0.30) is low because the public goods framing muffles opposition.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the constraint as necessary coordination against political abandonment; the payer seats experience it as an enforced bundle that diverts resources from purely operational readiness. The engine computes this divergence from the structural data: same constraint, opposed directionality, different computed seat types.
 *
 * DIRECTIONALITY LOGIC:
 *   Emergency management institutions and memorial institutions are structural beneficiaries: they receive funding and authority from the constraint's operation (d near beneficiary end). Disaster_exposed_public sits near symmetric: they benefit from competence but cannot disaggregate the memorial cost. Frontline_operators and municipal_taxpayers are structural targets: they bear the maintenance cost and the tension between layers (d near target end).
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading prevents mandatrophy mislabeling by acknowledging that the founding problem (boom-bust preparedness cycles) is still live. If the competence layer were to atrophy while the memorial layer persisted, the constraint would drift toward the husk_reading (piton/snare territory); if the memorial layer were abandoned while competence remained, it would drift toward the competence_reading (rope territory). The hybrid classification holds only while both layers are structurally required and the tension between them is actively maintained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    memorial_competence_separability,
    'Can the memorial layer and competence layer be funded and maintained separately, or does the constraint require their institutional fusion?',
    'Natural experiments from jurisdictions that fund response agencies and memorial institutions through separate budget lines.',
    'If separable, the hybrid constraint dissolves into two independent constraints (one rope-like, one potentially piton-like); if inseparable, the tension is structural and the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_competence_separability, conceptual, 'Whether memorial and competence layers are structurally separable').

omega_variable(
    husk_drift_inevitability,
    'Does the memorial layer inevitably drift toward pure performance (husk) when the competence layer is stressed by resource constraints?',
    'Longitudinal comparison of preparedness systems under fiscal austerity; measure whether memorial budgets survive deeper cuts than competence budgets.',
    'If inevitable, the hybrid reading describes an unstable equilibrium and the constraint family collapses toward husk over time; if contingent, the layered system can be maintained with adequate resources.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_drift_inevitability, empirical, 'Whether hybrid systems inevitably decay toward pure memorial performance').

omega_variable(
    kernel_reading_contest,
    'Is preparedness commitment best understood as a single layered constraint, or as a family of distinct constraints (competence, memorial, hybrid) that compete for institutional resources?',
    'Comparative institutional analysis of how agencies define and measure preparedness.',
    'If the kernel is irreducibly contested, the hybrid reading is one legitimate constraint among many; if the readings are merely facets of one system, decomposition into separate stories overstates the disjunction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the kernel decomposes into distinct structurally separate constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__hybrid_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_hyb_tr_t0, preparedness_commitment__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(prep_hyb_tr_t5, preparedness_commitment__hybrid_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(prep_hyb_tr_t10, preparedness_commitment__hybrid_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(prep_hyb_tr_t15, preparedness_commitment__hybrid_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(prep_hyb_tr_t20, preparedness_commitment__hybrid_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(prep_hyb_tr_t25, preparedness_commitment__hybrid_reading, theater_ratio, 25, 0.4).

% Extraction over time
narrative_ontology:measurement(prep_hyb_be_t0, preparedness_commitment__hybrid_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(prep_hyb_be_t5, preparedness_commitment__hybrid_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(prep_hyb_be_t10, preparedness_commitment__hybrid_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(prep_hyb_be_t15, preparedness_commitment__hybrid_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(prep_hyb_be_t20, preparedness_commitment__hybrid_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(prep_hyb_be_t25, preparedness_commitment__hybrid_reading, base_extractiveness, 25, 0.6).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_commitment__hybrid_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__hybrid_reading, resource_allocation).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__husk_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the preparedness_commitment kernel. The hybrid reading posits that preparedness requires both memorial and competence layers; the competence reading isolates operational knowledge; the husk reading isolates memorial performance. Each reading has distinct epsilon and stakeholder structures, linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
