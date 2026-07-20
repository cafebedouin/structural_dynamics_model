% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__constitutional_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__constitutional_hybrid_reading, []).

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
 *   constraint_id: sovereign_legitimacy__constitutional_hybrid_reading
 *   human_readable: Constitutional Hybrid Sovereign Legitimacy
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   Legitimate authority in this reading is dual-sourced: ceremonial
 *   authority inheres in the monarch through lineage, while political
 *   authority is delegated to elected officials, with constitutional law and
 *   interpretive precedent mediating the boundary. This constraint story
 *   instantiates the constitutional_hybrid_reading of the
 *   sovereign_legitimacy kernel, distinct from the monarchical_reading (pure
 *   inherited divine right) and the republican_reading (pure popular
 *   sovereignty). The hybrid is a contested compromise: it coordinates
 *   stability by separating symbolic continuity from political
 *   accountability, but asymmetrically extracts from absolutists and
 *   republican purists by foreclosing their preferred pure forms and imposing
 *   ambiguity costs. The claim is tangled_rope â the metrics independently
 *   describe moderate extraction and suppression, with rising theater as
 *   ceremonial authority becomes increasingly performative over the
 *   constitutional lifecycle.
 *
 * KEY AGENTS:
 *   - hereditary_monarch: Primary beneficiary (institutional/identity_locked) â retains status and income through inherited ceremonial authority.
 *   - elected_officials: Primary beneficiary (institutional/mobile) â holds delegated policy power under constitutional limits.
 *   - absolutists: Primary target (moderate/constrained) â seeks pure monarchical form, constrained by constitutional delegation.
 *   - republican_purists: Primary target (moderate/constrained) â seeks pure popular sovereignty, constrained by inherited ceremonial authority.
 *   - constitutional_courts: Agenda setter (institutional/analytical) â mediates boundary disputes through interpretation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__constitutional_hybrid_reading, 0.45).
domain_priors:suppression_score(sovereign_legitimacy__constitutional_hybrid_reading, 0.55).
domain_priors:theater_ratio(sovereign_legitimacy__constitutional_hybrid_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__constitutional_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__constitutional_hybrid_reading, "Constitutional Hybrid Sovereign Legitimacy").
narrative_ontology:topic_domain(sovereign_legitimacy__constitutional_hybrid_reading, "political/constitutional").

domain_priors:requires_active_enforcement(sovereign_legitimacy__constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__constitutional_hybrid_reading, '940f85a2-2c05-4be8-bc47-82a9aaf92e17').
narrative_ontology:cs_kernel_codification('940f85a2-2c05-4be8-bc47-82a9aaf92e17', formalized).
narrative_ontology:cs_authority_grounding('940f85a2-2c05-4be8-bc47-82a9aaf92e17', lineage).
narrative_ontology:cs_interpretation_layer_present('940f85a2-2c05-4be8-bc47-82a9aaf92e17').
narrative_ontology:cs_reading_relation('940f85a2-2c05-4be8-bc47-82a9aaf92e17', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('940f85a2-2c05-4be8-bc47-82a9aaf92e17', sovereign_legitimacy__republican_reading, influences).
narrative_ontology:cs_axiom('940f85a2-2c05-4be8-bc47-82a9aaf92e17', foundational, dual_source_legitimacy_principle).
narrative_ontology:cs_axiom_status(dual_source_legitimacy_principle, holdable).
narrative_ontology:cs_axiom_grounding('940f85a2-2c05-4be8-bc47-82a9aaf92e17', dual_source_legitimacy_principle, conventional).
narrative_ontology:cs_axiom('940f85a2-2c05-4be8-bc47-82a9aaf92e17', foundational, ceremonial_political_separation_mandate).
narrative_ontology:cs_axiom_status(ceremonial_political_separation_mandate, holdable).
narrative_ontology:cs_axiom_grounding('940f85a2-2c05-4be8-bc47-82a9aaf92e17', ceremonial_political_separation_mandate, conventional).
narrative_ontology:cs_reference_frame('940f85a2-2c05-4be8-bc47-82a9aaf92e17', constitutional_dual_source_equilibrium).
narrative_ontology:cs_drift_state('940f85a2-2c05-4be8-bc47-82a9aaf92e17', contemporary_constitutional_dispute_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('940f85a2-2c05-4be8-bc47-82a9aaf92e17', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, absolutists).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, republican_purists).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__constitutional_hybrid_reading, separation_of_ceremonial_and_political_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains ceremonial and symbolic authority, status, and income through inherited right; political authority is constitutionally delegated to elected officials. Bound by conventions that prevent direct governance; exit means abdication or revolutionary overthrow, which would dissolve the identity-fused role.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch, beneficiary,
    institutional, generational, identity_locked, national).

% Hold delegated political authority and policy power within constitutional limits. Derive legitimacy from electoral consent but must operate within the shadow of ceremonial authority and constitutional precedent; they can exit through electoral defeat or resignation.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, beneficiary,
    institutional, biographical, mobile, national).

% Seek unified sovereign authority in the monarch without constitutional limitation. Constrained by the dual-source framework that strips the crown of governing power; their preferred form is rendered illegitimate and unattainable within the current arrangement.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, absolutists, payer,
    moderate, generational, constrained, national).

% Seek pure popular sovereignty without inherited ceremonial authority. Constrained by constitutional recognition of monarchical lineage and symbolic power; their preferred form is blocked by the dual-source framework, though they may agitate for abolition.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, republican_purists, payer,
    moderate, biographical, constrained, national).

% Adjudicate boundary disputes between ceremonial and political authority. Their interpretive precedent is the enforcement mechanism that stabilizes the dual-source arrangement and determines the effective scope of each domain.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__constitutional_hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__constitutional_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes political order after revolutionary or post-dynastic crisis by separating symbolic continuity (inherited ceremonial authority) from political accountability (delegated elected authority), preventing the violence of contested sovereignty while preserving both tradition and popular representation.
% TRANSFER_FUNCTION: Moves status, income, and symbolic capital to the hereditary monarch; moves policy power and governing authority to elected officials; moves ambiguity costs and blocked aspiration to absolutists and republican purists who are foreclosed from realizing their preferred pure forms.
% ABSENT_VOICES: Advocates of direct democratic assemblies without constitutional mediation, and theocratic or divine-right absolutists who reject any delegated popular authority. Both are structurally outside the constitutional interpretive framework and its legitimacy conditions.
% DISAPPEARANCE_RATIONALE: If the dual-source mediation vanished overnight, the boundary between crown and parliament would collapse into either absolute monarchy or pure republicanism; constitutional precedent would lose its organizing force; the monarch's retained position and the officials' delegated authority would face simultaneous legitimation crises.
% FOUNDING_PROBLEM: How to stabilize political order after revolutionary or post-dynastic crisis without annihilating the symbolic capital of the inherited crown or suppressing the demand for popular representation.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative political scientists outside the benefiting parties attest that hybrid systems typically originate in specific post-crisis settlements. Republican critics dispute that aristocratic privilege remains necessary; monarchist traditionalists dispute that popular delegation is legitimate; both attest from outside the beneficiary set that the founding problem is contested.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__constitutional_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__constitutional_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__constitutional_hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sovereign_legitimacy__constitutional_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).
:- end_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.45) because the compromise reduces the extractiveness of both pure forms but introduces ambiguity costs and forecloses pure alternatives. Suppression is moderate (0.55) because the constraint actively blocks absolutist and republican projects through constitutional bars and precedent. Theater ratio rises to 0.50 because ceremonial authority becomes increasingly performative as political power shifts to elected officials over the lifecycle. Accessibility collapse is 0.65 because once the hybrid is entrenched, pure monarchy and pure republicanism become cognitively and institutionally distant. Resistance is 0.45 because both absolutist and republican movements mount real but bounded opposition. The measurement series share a single time grid to prevent misaligned temporal inference.
 *
 * PERSPECTIVAL GAP:
 *   The monarch experiences the constraint as protective of rightful status and dynastic continuity; elected officials experience it as a stable framework for legitimate delegation; absolutists and republican purists experience the identical structure as an illegitimate foreclosure of their preferred sovereign form. Constitutional courts experience it as a productive interpretive field that constitutes their own authority. The engine computes these divergent seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary monarch and elected officials are structural beneficiaries (low d): they collect status, income, and policy power from the arrangement. Their exit options differ â the monarch is identity_locked while officials are mobile â but both sit near the beneficiary end. Absolutists and republican purists are structural victims (high d): their preferred forms are constitutionally barred, and their exit is constrained by the accessibility collapse of alternatives. Constitutional courts sit near symmetric (d ~ 0.5) because their authority is constituted by the constraint itself; they are neither its target nor its passive collector.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â post-crisis stabilization â is contested rather than dead, because ongoing boundary disputes and periodic republican challenges keep the coordination function under pressure. Were the founding problem clearly dead and the constraint maintained purely by inertia, it would drift toward piton. The rising theater_ratio (0.50) signals performative maintenance but not yet full atrophy; the persistent boundary disputes indicate that active enforcement and interpretation still operate. Thus the classification as tangled_rope captures the live coordination function alongside the asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_of_hybrid,
    'Is the dual-source constitutional hybrid a discovered equilibrium of political order or merely a contingent post-crisis compromise?',
    'Comparative historical analysis: if hybrid systems consistently emerge across independent crises and persist long after the crisis, the equilibrium reading gains support; if they dissolve once the founding crisis fades, the contingency reading is stronger.',
    'If natural equilibrium, the constraint''s extractiveness is the necessary cost of coordination; if contingent compromise, the extraction is a rent imposed by historical accident and the classification tilts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_hybrid, conceptual, 'Whether the hybrid is natural or contingent').

omega_variable(
    boundary_dispute_stability,
    'Do recurring boundary disputes between ceremonial and political authority strengthen the framework through adaptive interpretation, or do they signal structural instability leading to eventual collapse?',
    'Longitudinal constitutional stability metrics: if boundary disputes are routinely resolved by courts without regime change, the framework is adaptive; if disputes escalate into constitutional crises or erosion of compliance, instability is indicated.',
    'If adaptive, the coordination function remains live and the tangled_rope classification holds; if leading to collapse, the constraint is a transitional scaffold or a decaying piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_dispute_stability, empirical, 'Whether boundary disputes indicate health or decay').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the constraint on pure-form seekers primarily structural (constitutional bars and electoral minority status) or internalized (broad acceptance of constitutional legitimacy that makes pure alternatives unthinkable)?',
    'Post-exit trajectory analysis: if absolutists and republicans who emigrate to polities with their preferred form rapidly abandon the hybrid legitimacy frame, suppression is primarily structural; if they continue to affirm hybrid legitimacy even where it no longer binds them, internalization is substantial.',
    'If internalized, effective suppression exceeds the structural measure and the victim seats experience higher extraction than the base metric suggests; if structural, the constraint''s hold is thinner and more contestable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__constitutional_hybrid_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sov_leg_hyb_tr_t0, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sov_leg_hyb_tr_t20, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(sov_leg_hyb_tr_t40, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(sov_leg_hyb_tr_t60, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(sov_leg_hyb_tr_t80, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 80, 0.47).
narrative_ontology:measurement(sov_leg_hyb_tr_t100, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 100, 0.5).

% Extraction over time
narrative_ontology:measurement(sov_leg_hyb_be_t0, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(sov_leg_hyb_be_t20, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(sov_leg_hyb_be_t40, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(sov_leg_hyb_be_t60, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(sov_leg_hyb_be_t80, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 80, 0.43).
narrative_ontology:measurement(sov_leg_hyb_be_t100, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sov_leg_hyb_su_t0, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sov_leg_hyb_su_t20, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(sov_leg_hyb_su_t40, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(sov_leg_hyb_su_t60, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(sov_leg_hyb_su_t80, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 80, 0.56).
narrative_ontology:measurement(sov_leg_hyb_su_t100, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__constitutional_hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, republican_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the sovereign_legitimacy kernel, decomposed from the colloquial label into three structurally distinct claims per the epsilon-invariance principle: monarchical_reading (high extraction from popular sovereignty seekers), republican_reading (high extraction from inherited authority seekers), and constitutional_hybrid_reading (moderate extraction from both pure-form seekers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
