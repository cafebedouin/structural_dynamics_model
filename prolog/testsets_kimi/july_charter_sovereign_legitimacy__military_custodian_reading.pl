% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__military_custodian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__military_custodian_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__military_custodian_reading
 *   human_readable: July Charter Military Custodian Reading
 *   domain: constitutional_law/political_transitions
 *
 * SUMMARY:
 *   A post-revolutionary constitutional charter is interpreted by the
 *   military establishment as ratifying the armed forces as the permanent
 *   institutional guardian of state stability. This reading subordinates
 *   civilian institutions to military veto authority and bounds political
 *   contestation through the security apparatus. The kernel is contested: the
 *   same charter text supports a secular democratic reading (civilian
 *   supremacy) and a guided nationalism reading (religious identity as
 *   sovereign ground). This file instantiates only the military custodian
 *   reading as a clean, epsilon-invariant constraint.
 *
 * KEY AGENTS:
 *   - Military establishment (agenda_setter, institutional, identity_locked) â sets constitutional boundaries and collects political and economic rents from guardianship.
 *   - Autonomous political parties (payer, moderate, trapped) â operate under red lines enforced by security apparatus.
 *   - Student movement (payer, powerless, trapped) â primary dissenters subject to containment and surveillance.
 *   - Civilian judiciary (payer, moderate, constrained) â procedurally active but subordinated on military prerogative.
 *   - Military-aligned elites (beneficiary, powerful, constrained) â secondary beneficiaries of the enforced order.
 *   - International observers (observer, institutional, analytical) â external analytical seat documenting democratic backsliding.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, 0.82).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__military_custodian_reading, 0.88).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__military_custodian_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__military_custodian_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__military_custodian_reading, "July Charter Military Custodian Reading").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__military_custodian_reading, "constitutional_law/political_transitions").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__military_custodian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__military_custodian_reading, '3dbcd984-9102-43b6-b755-2c6cabb14499').
narrative_ontology:cs_kernel_codification('3dbcd984-9102-43b6-b755-2c6cabb14499', fixed_text).
narrative_ontology:cs_authority_grounding('3dbcd984-9102-43b6-b755-2c6cabb14499', extraction).
narrative_ontology:cs_interpretation_layer_present('3dbcd984-9102-43b6-b755-2c6cabb14499').
narrative_ontology:cs_reading_relation('3dbcd984-9102-43b6-b755-2c6cabb14499', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('3dbcd984-9102-43b6-b755-2c6cabb14499', july_charter_sovereign_legitimacy__guided_nationalism_reading, coexists_with).
narrative_ontology:cs_axiom('3dbcd984-9102-43b6-b755-2c6cabb14499', foundational, military_guardianship_permanent).
narrative_ontology:cs_axiom_status(military_guardianship_permanent, holdable).
narrative_ontology:cs_axiom_grounding('3dbcd984-9102-43b6-b755-2c6cabb14499', military_guardianship_permanent, conventional).
narrative_ontology:cs_axiom('3dbcd984-9102-43b6-b755-2c6cabb14499', foundational, civilian_supremacy_subordinate_to_security).
narrative_ontology:cs_axiom_status(civilian_supremacy_subordinate_to_security, holdable).
narrative_ontology:cs_axiom_grounding('3dbcd984-9102-43b6-b755-2c6cabb14499', civilian_supremacy_subordinate_to_security, conventional).
narrative_ontology:cs_reference_frame('3dbcd984-9102-43b6-b755-2c6cabb14499', permanent_military_guardianship).
narrative_ontology:cs_drift_state('3dbcd984-9102-43b6-b755-2c6cabb14499', contemporary_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3dbcd984-9102-43b6-b755-2c6cabb14499', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_establishment).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_aligned_elites).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_judiciary).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, military_supremacy_doctrine).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, stability_over_contestation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds constitutional veto authority over political decisions and administers the security apparatus. Justifies its role as the permanent guardian of state stability, controlling appointments, budgets, and the boundaries of permissible political contestation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_establishment, agenda_setter,
    institutional, generational, identity_locked, national).

% Civilian and economic elites whose status and commercial security depend on the military-led order. They do not set constitutional rules but benefit from predictable suppression of labor and political demands that would threaten their position.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_aligned_elites, beneficiary,
    powerful, biographical, constrained, national).

% Formally permitted to contest elections but operate within red lines enforced by the security apparatus. Campaigning on military subordination or constitutional revision triggers legal dissolution, arrest of leadership, or extra-legal intimidation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties, payer,
    moderate, biographical, trapped, national).

% Primary mobilizing force for democratic contestation and the most visible source of street-level opposition. Subject to systematic containment through surveillance, campus closures, preventive detention, and judicial harassment.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement, payer,
    powerless, immediate, trapped, national).

% Retains procedural form and handles routine civil and criminal matters but lacks independence on questions touching military prerogative. Rulings against military interests are overridden by constitutional clauses or trigger institutional restructuring.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_judiciary, payer,
    moderate, biographical, constrained, national).

% Monitor and document restrictions on political contestation and judicial independence. Their reports frame the charter reading as democratic backsliding but they hold no enforcement authority within the domestic constitutional order.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, international_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__military_custodian_reading, military_establishment).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__military_custodian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preventing state collapse and maintaining public order during and after revolutionary transition by centralizing coercive capacity under a unified military command that claims to stand above factional politics.
% TRANSFER_FUNCTION: Transfers sovereignty from elected civilian institutions to permanent military veto authority; transfers political agency from autonomous parties, social movements, and the judiciary to channels bounded by the security apparatus.
% ABSENT_VOICES: Secular democratic constitutionalists who advocate full military subordination to civilian authority; grassroots movements who reject military guardianship in favor of popular sovereignty; regional autonomy movements who experience the military as an occupying force rather than a guardian.
% DISAPPEARANCE_RATIONALE: If the military custodianship and its enforcement apparatus disappeared, civilian institutions would either consolidate genuine autonomous authority or fragment into competing centers of power. The bounded party system would open, judicial review would gain independence on military matters, and the security apparatus would lose its constitutional mandate to intervene in politics.
% FOUNDING_PROBLEM: Post-revolutionary collapse of state authority, proliferation of armed non-state actors, and the immediate risk of civil war or state fragmentation following the overthrow of the prior regime.
% FOUNDING_PROBLEM_CORROBORATION: Military leadership and state media attest the problem remains live, citing terrorism and foreign interference. Independent historians, opposition parties, and international election monitors attest the revolutionary crisis has passed and the arrangement now serves authoritarian consolidation; human rights organizations outside the beneficiary set corroborate the shifted-function reading.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__military_custodian_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__military_custodian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__military_custodian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the military veto extracts sovereignty from civilian institutions and channels political agency into bounded forms. Suppression is higher (0.88) because the constraint persists only through active enforcement by the security apparatus against parties, students, and judicial independence. Theater ratio is substantial (0.62) and rising: over the interval the performance of guardianship has increasingly displaced genuine stability provision, with constitutional rituals masking direct military prerogative. Accessibility collapse is high (0.78) because civilian supremacy is structurally closed off by the charter's custodian clauses. Resistance is significant (0.72) due to persistent student and party opposition despite containment.
 *
 * PERSPECTIVAL GAP:
 *   The military establishment experiences the constraint as necessary coordination it built and maintains to prevent state collapse; autonomous political parties and the student movement experience the identical structure as enforced extraction that forecloses democratic sovereignty. The civilian judiciary experiences a hybrid of professional survival and subordination. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   The military establishment is the structural beneficiary and agenda setter (low d, subsidy from the constraint). Military-aligned elites are secondary beneficiaries (low d). Autonomous political parties, the student movement, and the civilian judiciary are the targets (high d, amplified effective extraction). The student movement's powerlessness and trapped exit place them nearest the full-target end. International observers sit at the analytical pole with neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by distinguishing genuine transitional guardianship (a scaffold with a sunset and declining enforcement) from permanent custodianship. Here the founding problem is contested rather than dead, and there is no sunset clause. The coordination function (stability) is structurally coupled with asymmetric extraction (veto over politics), producing tangled_rope rather than snare because the stability claim is historically rooted and institutionally codified, not merely cover. Were the stability function entirely fictive, the metrics would compute as snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stability_claim_empirical_validity,
    'Does the military custodian framework actually reduce state fragility and violence relative to civilian-led alternatives, or is the stability claim a post-hoc justification for entrenchment?',
    'Comparative analysis of post-revolutionary states with and without permanent military guardianship; counterfactual stability modeling and longitudinal security data.',
    'If the stability claim is empirically unsupported, the coordination function is cover and the constraint shifts toward snare classification; if supported, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_claim_empirical_validity, empirical, 'Whether the military''s stability claim is empirically grounded or rhetorical cover.').

omega_variable(
    political_containment_mechanism,
    'Is the bounding of political contestation achieved primarily through structural coercion (constitutional bars and security enforcement) or through internalized self-censorship by civilian actors?',
    'Post-exit behavior analysis during temporary liberalization windows: do political parties and students resume full contestation when structural barriers are lifted?',
    'If internalized, effective suppression exceeds structural measures and victim exit is more constrained than authored; if purely structural, resistance potential is higher than observed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_containment_mechanism, empirical, 'Structural versus internalized suppression mechanism in political containment.').

omega_variable(
    custodian_vs_usurper_framing,
    'Does the charter text structurally encode military custody as temporary/transitional or permanent, and does the current reading depend on interpretive drift beyond the fixed text?',
    'Philological and jurisprudential analysis of drafting records, constituent assembly debates, and subsequent constitutional court interpretations.',
    'If the text originally encoded a transitional scaffold, the current permanent reading represents a drift-state that would reclassify the constraint as snare or piton; if originally permanent, the extraction was constitutionalized at origin.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(custodian_vs_usurper_framing, conceptual, 'Whether the permanent custodian reading is textually grounded or product of interpretive drift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__military_custodian_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(july_tr_t8, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(july_tr_t16, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(july_tr_t24, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 24, 0.5).
narrative_ontology:measurement(july_tr_t32, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 32, 0.6).
narrative_ontology:measurement(july_tr_t40, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(july_be_t8, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(july_be_t16, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(july_be_t24, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 24, 0.76).
narrative_ontology:measurement(july_be_t32, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 32, 0.8).
narrative_ontology:measurement(july_be_t40, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 40, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(july_su_t8, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(july_su_t16, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(july_su_t24, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 24, 0.82).
narrative_ontology:measurement(july_su_t32, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 32, 0.87).
narrative_ontology:measurement(july_su_t40, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 40, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__military_custodian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, guided_nationalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the july_charter_sovereign_legitimacy kernel. The kernel decomposes into three structurally distinct constraints because the charter text underdetermines sovereignty: secular democratic reading (civilian supremacy), military custodian reading (military veto authority), and guided nationalism reading (religious-nationalist identity). Each reading has distinct beneficiary/victim structures, epsilon values, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
