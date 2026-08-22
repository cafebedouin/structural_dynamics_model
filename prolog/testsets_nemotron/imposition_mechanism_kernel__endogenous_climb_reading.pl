% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__endogenous_climb_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: imposition_mechanism_kernel__endogenous_climb_reading
 *   human_readable: Endogenous Norm Adoption with Post-Hoc State Ratification
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This reading models the endogenous climb mechanism: new norms emerge from
 *   civil society, diffuse through voluntary adoption, and only then are
 *   ratified by the state. The state acts as a coordinator that reduces
 *   transaction costs of an already-consensus practice, not as a coercer
 *   imposing alien norms. The constraint's low extractiveness and suppression
 *   reflect the voluntary nature of the adoption phase. The theater ratio
 *   captures the performative aspect of state ratification ceremonies that
 *   add little functional value. This is one reading of the
 *   imposition_mechanism_kernel; sibling readings model state-led imposition
 *   and hybrid pathways.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__endogenous_climb_reading, 0.12).
domain_priors:suppression_score(imposition_mechanism_kernel__endogenous_climb_reading, 0.08).
domain_priors:theater_ratio(imposition_mechanism_kernel__endogenous_climb_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__endogenous_climb_reading, rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__endogenous_climb_reading, "Endogenous Norm Adoption with Post-Hoc State Ratification").
narrative_ontology:topic_domain(imposition_mechanism_kernel__endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__endogenous_climb_reading, '5685b098-4322-4715-b5f7-b04651c66b1b').
narrative_ontology:cs_kernel_codification('5685b098-4322-4715-b5f7-b04651c66b1b', distributed).
narrative_ontology:cs_authority_grounding('5685b098-4322-4715-b5f7-b04651c66b1b', practice).
narrative_ontology:cs_interpretation_layer_present('5685b098-4322-4715-b5f7-b04651c66b1b').
narrative_ontology:cs_reading_relation('5685b098-4322-4715-b5f7-b04651c66b1b', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('5685b098-4322-4715-b5f7-b04651c66b1b', imposition_mechanism_kernel__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('5685b098-4322-4715-b5f7-b04651c66b1b', foundational, legitimacy_requires_cultural_consensus).
narrative_ontology:cs_axiom_status(legitimacy_requires_cultural_consensus, holdable).
narrative_ontology:cs_axiom_grounding('5685b098-4322-4715-b5f7-b04651c66b1b', legitimacy_requires_cultural_consensus, empirically_contingent).
narrative_ontology:cs_axiom('5685b098-4322-4715-b5f7-b04651c66b1b', foundational, state_ratification_follows_social_fact).
narrative_ontology:cs_axiom_status(state_ratification_follows_social_fact, holdable).
narrative_ontology:cs_axiom_grounding('5685b098-4322-4715-b5f7-b04651c66b1b', state_ratification_follows_social_fact, conventional).
narrative_ontology:cs_reference_frame('5685b098-4322-4715-b5f7-b04651c66b1b', cultural_consensus_as_legitimacy_source).
narrative_ontology:cs_drift_state('5685b098-4322-4715-b5f7-b04651c66b1b', contemporary_institutional_analysis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5685b098-4322-4715-b5f7-b04651c66b1b', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, civil_society_norm_entrepreneurs).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, state_coordinators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, general_population).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__endogenous_climb_reading, normative_legitimacy_requires_cultural_rootedness).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__endogenous_climb_reading, state_authority_is_derivative_of_social_consensus).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__endogenous_climb_reading, bottom_up_adoption_precedes_legal_codification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiate and diffuse new normative practices through voluntary associations, religious movements, or intellectual networks. Their norms gain traction because they solve coordination problems or express emerging moral intuitions. They face no coercion to adopt; their influence grows through persuasion and demonstration. Exit means abandoning the normative project, but the constraint itself does not trap them.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, civil_society_norm_entrepreneurs, agenda_setter,
    organized, generational, mobile, national).

% Formalize already-widespread norms into law, gaining legitimacy from ratifying what society has already accepted. They avoid the costs of coercive enforcement because compliance is already near-universal. Their role is to standardize, record, and provide a focal point for the norm's application. They could choose not to ratify, but doing so would forfeit the coordination benefits of legal clarity.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, state_coordinators, beneficiary,
    institutional, generational, arbitrage, national).

% Adopt the new norms because they resonate with lived experience or solve practical problems. They gain predictable social coordination and reduced transaction costs. Resistance is minimal because the norms emerged from their own practices. Legal ratification later merely confirms what they already do. Exit would mean rejecting a norm they already find useful, which carries social costs but not legal penalties at the adoption stage.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, general_population, beneficiary,
    organized, biographical, constrained, national).

% Hold beliefs or practices incompatible with the new norm. They are not consulted in the bottom-up process and their objections carry no weight in the cultural consensus. Once the state ratifies the norm, they face legal penalties for non-compliance. Their exit options are extremely limited: conform, hide, or migrate. They are the structural victims of the endogenous climb, though the reading's framing renders them invisible.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, dissenting_minorities, excluded,
    powerless, biographical, trapped, local).

% Analyze the sequence of norm emergence, diffusion, and legal codification. They observe that state action follows cultural change rather than preceding it. They note the low enforcement costs and rapid adoption as evidence of genuine coordination. They also document the post-hoc exclusion of dissenters once the norm becomes law. Their seat is outside the constraint's operation but their analysis shapes how the constraint is understood.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of achieving widespread behavioral coordination around new norms without relying on state coercion. The norm spreads because it genuinely coordinates — it reduces uncertainty, lowers transaction costs, or expresses a shared moral intuition that people independently find compelling.
% TRANSFER_FUNCTION: Moves normative authority from civil society (where it is generated and validated) to the state (where it is codified and standardized). The state does not extract resources; it receives the coordination surplus — a pre-aligned population that makes governance cheaper. The transfer is legitimacy, not wealth.
% ABSENT_VOICES: Dissenting minorities whose practices conflict with the emergent norm. They are absent from the bottom-up adoption process because the norm's legitimacy derives from majority cultural resonance, not universal consent. They become visible only after state ratification criminalizes their non-compliance. Their exclusion is structural: the endogenous climb mechanism has no procedural place for them.
% DISAPPEARANCE_RATIONALE: If this constraint vanished — meaning new norms could not achieve legitimacy through bottom-up adoption followed by state ratification — then either norms would fail to coordinate at scale (remaining fragmented local practices) or the state would be forced to impose norms coercively (exogenous override). The historical record shows that endogenous climb is a primary pathway for large-scale normative change; removing it would alter the dynamics of state-society relations and the legitimacy of legal systems.
% FOUNDING_PROBLEM: How to achieve legitimate, large-scale behavioral coordination around new norms in a way that minimizes enforcement costs and maximizes voluntary compliance. The endogenous climb reading identifies a historical solution: let norms emerge and prove themselves in civil society, then have the state ratify the winner.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociologists (Eisenstadt, Tilly, Mann) document multiple cases where state law followed cultural change (e.g., abolition of slavery in Britain, women's suffrage in Scandinavia, religious tolerance in the Dutch Republic). The corroboration comes from comparative historical analysis, not from the state or norm entrepreneurs themselves.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__endogenous_climb_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(imposition_mechanism_kernel__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__endogenous_climb_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).
:- end_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the state extracts minimal resources — it mostly captures coordination surplus. Suppression is minimal (0.08) during the adoption phase; it rises slightly at ratification when dissenters face legal penalties, but the bulk of compliance is voluntary. Theater ratio (0.18) reflects the symbolic gap between the state's claim to be the source of normative authority and its actual role as a late-stage coordinator. Accessibility collapse (0.72) is moderately high because once a norm achieves cultural saturation, alternatives become socially invisible — but not legally forbidden until ratification. Resistance (0.15) is low because the norm solves real coordination problems for most adopters.
 *
 * PERSPECTIVAL GAP:
 *   From the norm entrepreneur's seat, the constraint is a pure rope — a coordination mechanism that works because it is genuinely useful. From the state coordinator's seat, it is a rope that delivers governance efficiency. From the dissenter's seat, it is a snare that criminalizes their way of life after the fact. The engine computes this divergence from the structural data: same constraint, different directionalities. The claimed_type (rope) reflects the dominant coordination function, not the dissenter's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil society norm entrepreneurs are agenda_setters with mobile exit — they initiate the norm and can abandon it if it fails to gain traction. State coordinators are beneficiaries with arbitrage exit — they gain legitimacy and coordination benefits at near-zero cost, and can choose which norms to ratify. The general population are beneficiaries with constrained exit — they gain coordination but face social costs for non-adoption. Dissenting minorities are excluded with trapped exit — they bear the costs of the norm's eventual legal enforcement without having participated in its cultural validation. Historical sociologists are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows no mandatrophy: its founding problem (legitimate large-scale coordination) remains live, and the mechanism continues to operate in contemporary norm diffusion (e.g., same-sex marriage, digital privacy norms). The state's role as post-hoc coordinator remains functionally justified — it is not maintaining a dead arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dissenter_exclusion_in_endogenous_climb,
    'Does the endogenous climb mechanism structurally require the exclusion of dissenting minorities, or is their exclusion a contingent historical artifact of state ratification?',
    'Comparative case analysis: identify endogenous climb episodes where dissenters were accommodated vs. suppressed post-ratification. Test whether accommodation correlates with norm stability or fragmentation.',
    'If exclusion is structurally necessary, the endogenous climb reading conceals a snare component for dissenters — the constraint would be a tangled rope at the class level. If contingent, the rope classification holds for the adopting population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissenter_exclusion_in_endogenous_climb, empirical, 'Whether dissenters'' exclusion is inherent to the endogenous climb mechanism or added at ratification').

omega_variable(
    state_agency_in_ratification_timing,
    'How much agency does the state have in choosing WHEN to ratify an emergent norm — is ratification automatic once a threshold is crossed, or does the state strategically delay/accelerate for its own interests?',
    'Process-tracing of state ratification decisions across multiple endogenous climb episodes. Measure time lag between cultural saturation and legal codification; correlate with state capacity, competing norms, and elite interests.',
    'If ratification timing is strategic, the state is an active player extracting political capital from the norm''s momentum — raising extractiveness. If automatic, the state is a passive coordinator.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_agency_in_ratification_timing, empirical, 'Degree of state strategic agency in the ratification decision').

omega_variable(
    kernel_reading_relations_endogenous_climb,
    'What is the structural relationship between the endogenous climb reading and its sibling readings of the imposition mechanism kernel?',
    'Analyze whether the readings foreclose each other (mutually exclusive causal claims), coexist as competing historical explanations for different cases, or influence each other (e.g., endogenous climb creates conditions that enable exogenous override).',
    'Determines the cs_structure.reading_relations classification. Affects whether the kernel is modeled as a genuine multiplicity or a single mechanism with variable parameters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relations_endogenous_climb, conceptual, 'Structural relationship between this reading and its kernel siblings').

omega_variable(
    cultural_saturation_threshold_measurement,
    'What constitutes ''cultural saturation'' triggering state ratification — a measurable threshold (e.g., % adoption) or a qualitative shift in elite discourse?',
    'Operationalize saturation metrics across historical cases. Test whether ratification correlates with quantitative adoption rates or qualitative markers (elite endorsement, media framing, institutional isomorphism).',
    'If quantitative, the mechanism has a measurable coordination threshold. If qualitative, the state''s interpretive discretion is larger, potentially increasing extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_saturation_threshold_measurement, empirical, 'Nature of the cultural saturation trigger for state ratification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__endogenous_climb_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imposition_mechanism_endogenous_climb_tr_t0, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(imposition_mechanism_endogenous_climb_tr_t20, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(imposition_mechanism_endogenous_climb_tr_t40, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(imposition_mechanism_endogenous_climb_tr_t60, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 60, 0.17).
narrative_ontology:measurement(imposition_mechanism_endogenous_climb_tr_t80, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement(imposition_mechanism_endogenous_climb_tr_t100, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 100, 0.18).

% Extraction over time
narrative_ontology:measurement(imposition_mechanism_endogenous_climb_be_t0, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(imposition_mechanism_endogenous_climb_be_t20, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(imposition_mechanism_endogenous_climb_be_t40, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 40, 0.1).
narrative_ontology:measurement(imposition_mechanism_endogenous_climb_be_t60, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 60, 0.11).
narrative_ontology:measurement(imposition_mechanism_endogenous_climb_be_t80, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 80, 0.12).
narrative_ontology:measurement(imposition_mechanism_endogenous_climb_be_t100, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 100, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(imposition_mechanism_endogenous_climb_su_t0, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(imposition_mechanism_endogenous_climb_su_t20, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 20, 0.04).
narrative_ontology:measurement(imposition_mechanism_endogenous_climb_su_t40, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 40, 0.06).
narrative_ontology:measurement(imposition_mechanism_endogenous_climb_su_t60, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 60, 0.07).
narrative_ontology:measurement(imposition_mechanism_endogenous_climb_su_t80, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 80, 0.08).
narrative_ontology:measurement(imposition_mechanism_endogenous_climb_su_t100, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__endogenous_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imposition_mechanism_kernel__endogenous_climb_reading, 0.06).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% Part of the imposition_mechanism_kernel constraint family. This reading (endogenous climb) models bottom-up legitimation; exogenous_override_reading models top-down imposition; hybrid_legitimation_reading models authority-transfer plus incentives. The three readings share the kernel's core question — how do new norms achieve legitimate authority? — but differ on the causal primacy of state vs. civil society, the role of coercion, and the sequencing of cultural and legal change. Their ε values differ substantially: endogenous climb has low ε (voluntary coordination), exogenous override has high ε (coercive imposition), hybrid has intermediate ε with distinct enforcement profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
