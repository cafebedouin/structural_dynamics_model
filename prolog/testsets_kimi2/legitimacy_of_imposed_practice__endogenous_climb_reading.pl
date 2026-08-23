% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__endogenous_climb_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: legitimacy_of_imposed_practice__endogenous_climb_reading
 *   human_readable: Endogenous Climb Reading of Imposed Practice Legitimacy
 *   domain: political history / state formation / cultural imposition
 *
 * SUMMARY:
 *   This constraint story instantiates the endogenous_climb_reading of the
 *   legitimacy_of_imposed_practice kernel. The standing arrangement under
 *   contest is state-mandated cultural practice displacement (calendar and
 *   dress reform) imposed by decree. From this reading, the arrangement fails
 *   to achieve durable legitimacy because it lacks bottom-up internalization:
 *   lunar observance persists for decades, urban dress adoption is
 *   performative with private retention, and the primary beneficiaries are
 *   communities that preserve autonomy. The state modernization project bears
 *   the costs of enforcement, legitimacy erosion, and timeline delay. The
 *   constraint is claimed as tangled_rope because it contains a genuine
 *   state-building coordination function (administrative and symbolic
 *   unification) coupled with asymmetric extraction that falls on the state
 *   and compliant urban classes while benefiting resistant communities.
 *
 * KEY AGENTS:
 *   - state_reform_elite: Primary agenda-setter and payer (institutional/constrained) â imposes reforms and bears the extraction costs of failure and legitimacy erosion
 *   - communities_preserving_autonomy: Primary beneficiary (moderate/identity_locked) â preserve traditional practices because imposition lacks internalized legitimacy
 *   - urban_merchant_class: Secondary payer (moderate/constrained) â performs partial public adoption while retaining private practice, bearing dual-practice costs
 *   - traditional_ritual_specialists: Excluded voice (moderate/trapped) â displaced authority, absent from state discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.62).
domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.52).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__endogenous_climb_reading, "Endogenous Climb Reading of Imposed Practice Legitimacy").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__endogenous_climb_reading, "political history / state formation / cultural imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__endogenous_climb_reading, 'cbad879b-4128-4af6-a920-79cca5782478').
narrative_ontology:cs_kernel_codification('cbad879b-4128-4af6-a920-79cca5782478', implicit).
narrative_ontology:cs_authority_grounding('cbad879b-4128-4af6-a920-79cca5782478', extraction).
narrative_ontology:cs_interpretation_layer_present('cbad879b-4128-4af6-a920-79cca5782478').
narrative_ontology:cs_reading_relation('cbad879b-4128-4af6-a920-79cca5782478', legitimacy_of_imposed_practice__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('cbad879b-4128-4af6-a920-79cca5782478', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('cbad879b-4128-4af6-a920-79cca5782478', foundational, legitimacy_requires_bottom_up_internalization).
narrative_ontology:cs_axiom_status(legitimacy_requires_bottom_up_internalization, holdable).
narrative_ontology:cs_axiom_grounding('cbad879b-4128-4af6-a920-79cca5782478', legitimacy_requires_bottom_up_internalization, empirically_contingent).
narrative_ontology:cs_reference_frame('cbad879b-4128-4af6-a920-79cca5782478', endogenous_legitimacy_framework).
narrative_ontology:cs_drift_state('cbad879b-4128-4af6-a920-79cca5782478', post_reform_implementation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cbad879b-4128-4af6-a920-79cca5782478', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_autonomy).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, state_reform_elite).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_merchant_class).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__endogenous_climb_reading, endogenous_legitimacy_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Impose national calendars and dress codes by decree, investing administrative and coercive resources in cultural displacement. Bear the costs of incomplete adoption, legitimacy erosion, and persistent non-compliance that delays the modernization timeline.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, state_reform_elite, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, state_reform_elite, payer).

% Maintain traditional lunar observance and dress in private, domestic, and local ritual contexts despite state mandates. Their autonomy is preserved because the imposed alternatives lack internalized legitimacy and fail to displace endogenous cultural repertoires.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_autonomy, beneficiary,
    moderate, generational, identity_locked, regional).

% Adopt state-mandated dress and calendar observance in public and commercial life to maintain market access and social standing, while retaining traditional practices in domestic contexts. Bear the cognitive and cultural costs of performative dual practice without full internalization.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_merchant_class, payer,
    moderate, biographical, constrained, regional).

% Would assert the continuing legitimacy of traditional calendrical and sartorial practice but are excluded from the state modernization discourse. Their authority is eroded by official narratives that frame traditional practice as backward, yet they have no channel to contest the imposed framework.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, traditional_ritual_specialists, excluded,
    moderate, generational, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_autonomy).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: State-building coordination: creating administrative uniformity (taxation, record-keeping, legal standardization) and visible national identity across a diverse population that lacks shared civic symbols.
% TRANSFER_FUNCTION: Moves the burden of cultural adaptation and performative compliance from state reform institutions onto subject populations; moves the autonomy dividend of incomplete displacement back to traditional communities that retain private practice despite public mandates.
% ABSENT_VOICES: Rural peasant communities maintaining lunar observance and traditional ritual specialists are excluded from state legitimacy discourse; their private retention is invisible in official compliance metrics and their objections are treated as backwardness rather than evidence of failed internalization.
% DISAPPEARANCE_RATIONALE: Without the imposition mechanism, the state would lose its primary instrument for cultural standardization and would need to rely on persuasion and endogenous adoption; communities would resume traditional practices openly, and the partial urban compliance would likely revert as the social pressure of public performance lifted.
% FOUNDING_PROBLEM: The administrative and symbolic fragmentation of the polity: multiple calendars preventing coordinated taxation and governance, regional dress signaling local rather than national loyalty, and the absence of a shared civic culture to underpin a modern state.
% FOUNDING_PROBLEM_CORROBORATION: State nationalist historians and reform bureaucrats attest the problem from within the benefiting and coordinating parties. Revisionist historians and ethnographers from outside the state elite argue that administrative unity was achievable through lighter standardization and that the depth of cultural displacement was unnecessary for state capacity â this external corroboration exists but is itself contested.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater_ratio (0.58) is high because public compliance masks extensive private retention; the state performs modernization while actual practice remains bifurcated. Extractiveness (0.62) reflects the substantial resource and legitimacy costs borne by the state project combined with the autonomy dividend captured by resistant communities. Resistance (0.65) captures persistent passive and active non-compliance across the interval. Accessibility_collapse (0.30) is low because traditional alternatives remain viable in private and local contexts. The measurement series shows extraction rising as enforcement intensifies, then stabilizing as the state accepts theatrical compliance; theater rises monotonically as private retention becomes entrenched behind public performance.
 *
 * PERSPECTIVAL GAP:
 *   The state_reform_elite experiences the constraint as a coordination necessity that has backfired (high directionality as victim: they pay for its failure in wasted resources and timeline delay). Communities experience it as a failed imposition that protects their autonomy (low directionality as beneficiary: the constraint effectively subsidizes their cultural continuity). The engine computes this divergence from the structural declarations â the agenda_setter role does not override the payer role in directionality derivation because the victim declaration takes precedence for the state elite's seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Communities_preserving_autonomy are declared beneficiaries with identity_locked exit, producing low directionality (near the beneficiary end): the constraint effectively subsidizes their autonomy by failing to displace their practices. State_reform_elite are declared victims (payer) with constrained exit, producing high directionality (near the target end): the constraint extracts state capacity and legitimacy. Urban_merchant_class are payers with constrained exit, also mapping to high directionality. No overrides are needed because the beneficiary/victim declarations plus exit atoms already capture the true structural relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling as a pure snare because the state-building coordination function is genuine: standardizing calendars does solve real administrative coordination problems, and dress codes do serve symbolic integration. It prevents mislabeling as a pure rope because the cost asymmetry is severe: the state bears enforcement costs and legitimacy erosion while communities capture an autonomy benefit, and the arrangement requires active enforcement precisely because internalization is absent. If the founding problem (administrative fragmentation) were dead but the imposition persisted purely by inertia, the constraint would drift toward piton; here the founding problem is contested, keeping the coordination claim live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_inversion,
    'If the exogenous_override reading were adopted, the same state imposition would be read as legitimate and effective, reversing the beneficiary/victim structure. What empirical marker distinguishes these readings?',
    'Long-term practice persistence post-enforcement withdrawal: if practices revert to traditional forms after enforcement lapses, the endogenous_climb reading is supported; if imposed practices persist, the exogenous_override reading is supported.',
    'Would flip the directionality vector and reclassify the constraint from tangled_rope toward rope or mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_inversion, empirical, 'Empirical test to discriminate endogenous from exogenous readings of the same kernel.').

omega_variable(
    urban_rural_internalization_gap,
    'Does urban partial adoption represent genuine internalization or performative compliance retained by state surveillance and social pressure?',
    'Ethnographic depth studies of private ritual and domestic practice in urban adopting households over the full interval.',
    'If private retention is near-universal even among urban adopters, the constraint''s theater_ratio is higher than surface compliance suggests and extraction falls more heavily on the urban class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(urban_rural_internalization_gap, empirical, 'Whether urban adoption is genuine internalization or public performance.').

omega_variable(
    identity_lock_vs_structural_resistance,
    'Is community autonomy preservation driven by identity-locked cultural attachment or by structural isolation from state enforcement capacity?',
    'Comparison of enforcement-accessible urban communities with isolated rural ones; if both retain practices at similar rates, identity_lock dominates.',
    'Would raise effective extraction for communities if exit is identity_locked rather than merely constrained, and would shift suppression interpretation from structural to internalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_resistance, conceptual, 'Structural versus internalized mechanism of community resistance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__endogenous_climb_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(legi_tr_t5, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(legi_tr_t15, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(legi_tr_t25, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 25, 0.62).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 30, 0.65).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 40, 0.63).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(legi_be_t5, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(legi_be_t15, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(legi_be_t25, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(legi_su_t5, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(legi_su_t15, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(legi_su_t25, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__endogenous_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This constraint is the endogenous_climb_reading of the legitimacy_of_imposed_practice kernel. It decomposes the kernel into a structurally distinct claim: practice displacement is durable only through bottom-up internalization. Sibling readings produce different epsilon profiles, different beneficiary/victim orientations, and different predicted empirical trajectories. All three form a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
