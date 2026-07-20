% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__practice_decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__practice_decline_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__practice_decline_reading
 *   human_readable: Honor Satisfaction Substrate â Practice Decline Reading
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   The honor satisfaction substrate is the normative complex that scripts
 *   how elite males respond to insult and recover social standing through
 *   ritualized violence. This readingâthe practice_decline readingâholds
 *   that the substrate persisted largely unchanged into the modern era, but
 *   the terminal practice of dueling became impractical due to exogenous
 *   legal prohibition, institutional barriers (professional advancement
 *   contingent on clean legal record), and rising opportunity costs. The
 *   constraint therefore operates as a rope: a genuine coordination mechanism
 *   for status disputes that has been disabled by external enforcement
 *   pressure, leaving participants in a coordination failure where the script
 *   is thinkable but enactable only at prohibitive cost. It is NOT a mountain
 *   erosion (the code did not naturally dissipate) nor a cultural
 *   transformation (the code did not become dignity-based). The kernel
 *   'honor_satisfaction_substrate' admits multiple readings; this file
 *   instantiates the practice_decline reading only, per Îµ-invariance.
 *
 * KEY AGENTS:
 *   - honor_bound_gentry: Primary beneficiary (powerful/identity_locked) â coordinated by the honor script but legally barred from enacting it.
 *   - military_officers: Secondary beneficiary (institutional/constrained) â preserves attenuated honor mechanisms within formal military structures.
 *   - southern_honor_carrying_communities: Regional beneficiary (moderate/identity_locked) â modern carriers of the substrate in altered ritual form.
 *   - state_legal_apparatus: External observer/enforcer (institutional/analytical) â suppresses dueling through criminal law.
 *   - women_and_subaltern_groups: Excluded seat (powerless/trapped) â bear costs but have no voice in the normative framework.
 *   - modernizing_reformers: Observer (powerful/mobile) â promote abolition and bureaucratic rationality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__practice_decline_reading, 0.2).
domain_priors:suppression_score(honor_satisfaction_substrate__practice_decline_reading, 0.4).
domain_priors:theater_ratio(honor_satisfaction_substrate__practice_decline_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__practice_decline_reading, rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__practice_decline_reading, "Honor Satisfaction Substrate â Practice Decline Reading").
narrative_ontology:topic_domain(honor_satisfaction_substrate__practice_decline_reading, "historical_sociology/cultural_anthropology/legal_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__practice_decline_reading, '16dd2632-d364-4cd3-8f84-ad7cbb9e1995').
narrative_ontology:cs_kernel_codification('16dd2632-d364-4cd3-8f84-ad7cbb9e1995', distributed).
narrative_ontology:cs_authority_grounding('16dd2632-d364-4cd3-8f84-ad7cbb9e1995', practice).
narrative_ontology:cs_interpretation_layer_present('16dd2632-d364-4cd3-8f84-ad7cbb9e1995').
narrative_ontology:cs_reading_relation('16dd2632-d364-4cd3-8f84-ad7cbb9e1995', honor_satisfaction_substrate__cultural_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('16dd2632-d364-4cd3-8f84-ad7cbb9e1995', honor_satisfaction_substrate__composite_overdetermined_reading, influences).
narrative_ontology:cs_axiom('16dd2632-d364-4cd3-8f84-ad7cbb9e1995', foundational, honor_code_substrate_persistence).
narrative_ontology:cs_axiom_status(honor_code_substrate_persistence, holdable).
narrative_ontology:cs_axiom_grounding('16dd2632-d364-4cd3-8f84-ad7cbb9e1995', honor_code_substrate_persistence, empirically_contingent).
narrative_ontology:cs_axiom('16dd2632-d364-4cd3-8f84-ad7cbb9e1995', foundational, exogenous_enforcement_sufficiency).
narrative_ontology:cs_axiom_status(exogenous_enforcement_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('16dd2632-d364-4cd3-8f84-ad7cbb9e1995', exogenous_enforcement_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('16dd2632-d364-4cd3-8f84-ad7cbb9e1995', gentlemanly_reciprocal_honor).
narrative_ontology:cs_drift_state('16dd2632-d364-4cd3-8f84-ad7cbb9e1995', modern_nation_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('16dd2632-d364-4cd3-8f84-ad7cbb9e1995', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, honor_bound_gentry).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, military_officers).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, southern_honor_carrying_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their social identity and status standing are constituted through the honor code. An unresolved insult threatens their position in a closed elite hierarchy. Dueling remains the thinkable but legally barred path to restore reputation; they bear the psychological cost of unsatisfied claims and the legal risk of clandestine satisfaction.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, honor_bound_gentry, beneficiary,
    powerful, biographical, identity_locked, national).

% The officer corps preserves honor-based status hierarchies through formal courts of honor and disciplinary codes that mutated from dueling substrates. They benefit from a scripted mechanism to resolve internal status challenges without civilian legal exposure.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, military_officers, beneficiary,
    institutional, generational, constrained, national).

% Modern regional carriers of attenuated honor culture where personal reputation remains a publicly defended asset. The substrate persists in altered ritual formsâsocial ostracism, public apology demandsâwithout the terminal duel.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, southern_honor_carrying_communities, beneficiary,
    moderate, generational, identity_locked, regional).

% Criminalizes and suppresses dueling through homicide and assault statutes, treating private honor violence as a challenge to the state's monopoly on legitimate force. The legal apparatus observes and intervenes in the honor-bound community's conflict-resolution practices.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, state_legal_apparatus, observer,
    institutional, generational, analytical, national).

% Structurally excluded from the field of honor and its protections, yet bear the material and emotional costs of male honor violenceâwidowhood, destabilization, and instrumentalization as objects of status competition. Their absence from the normative conversation is constitutive of the constraint.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, women_and_subaltern_groups, excluded,
    powerless, biographical, trapped, local).

% Advocated for the legal abolition of dueling as part of a broader civilizing process; they view the honor code as incompatible with bureaucratic rationality and contract-based social order, supporting state suppression.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, modernizing_reformers, observer,
    powerful, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a scripted, reciprocal mechanism for resolving interpersonal status challenges among elite males, preventing unregulated vendettas and social chaos in the absence of a centralized justice monopoly.
% TRANSFER_FUNCTION: Moves social standing and reputational credit from the insulting party to the challenged party through ritualized risk-taking and public acknowledgment; the duel is the terminal transfer mechanism, while the substrate coordinates deference and status repair.
% ABSENT_VOICES: Women, dependents, and non-elite males are structurally excluded from the field of honor; they bear the material costs of honor violence but have no voice in defining the code. Legal modernizers are present in public discourse but absent from the normative community that still interprets affronts through the honor substrate.
% DISAPPEARANCE_RATIONALE: If the honor substrate vanished entirely, elite status hierarchies would lose a key script for conflict resolution; military discipline would require re-grounding outside honor logic; Southern culture-of-honor regions would lose a constitutive normative framework. The arrangements that still depend on it would reorganize.
% FOUNDING_PROBLEM: In the absence of a centralized state monopoly on legitimate violence, elite males required a self-regulating mechanism to resolve interpersonal conflicts over reputation without triggering family vendettas or sustained social disorder.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (e.g., James Kelly, Ute Frevert) attest that the state justice monopoly supplanted private honor enforcement; military historians corroborate that formal courts of honor replaced dueling within the officer corps. Cultural anthropologists (e.g., Nisbett and Cohen) note persistent honor scripts in attenuated forms, contesting the 'dead' verdict and supporting the persistence claim asserted here.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__practice_decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__practice_decline_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__practice_decline_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__practice_decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__practice_decline_reading, 0.2, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__practice_decline_reading_tests).
:- end_tests(honor_satisfaction_substrate__practice_decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.20) because the substrate no longer extracts life-or-death compliance through the duel; its residual extraction operates through status anxiety and informal social penalties. Suppression is moderate (0.40) because the honor code still suppresses non-honor-based responses to insult (e.g., legal recourse, ignoring), even though the state suppresses the duel itself. Theater_ratio rises to 0.32 because attenuated forms (formal military courts of honor, social 'cutting') become increasingly performative substitutes for the functional duel. Accessibility_collapse is moderate (0.45): within the honor-bound community, alternatives to the script remain cognitively collapsed, but the legal system provides an external alternative. Resistance is elevated (0.60) because the state and modernizing elites actively resist the practice, while the honor-bound community resists the loss of the substrate. Metrics are authored independently of the rope claim; the engine will compute per-seat types.
 *
 * PERSPECTIVAL GAP:
 *   From the honor-bound gentry seat, the constraint is a lifeworld necessity that external power has illegitimately suppressed; from the state legal apparatus seat, the same constraint is a violent anachronism that legitimate law has rightly disabled. The military officer seat experiences a hybrid: the substrate is functionally preserved in attenuated institutional form. These divergences are structural, not perspectival illusions.
 *
 * DIRECTIONALITY LOGIC:
 *   The gentry, military officers, and Southern honor communities are beneficiaries of the coordination script (low d, low Ï). The state and modernizing reformers are not targets of the constraint but external resistors; their directionality is not derived from the constraint's extraction but from their opposition to it. Women and subalterns are excluded from the normative framework but bear diffuse costs; their exclusion gives them no directional seat within the constraint's logic. Because the constraint is a rope with no concentrated extraction, effective extraction is damped across all beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâabsence of a centralized justice mechanism leading to feudingâis dead in the sense that modern states now monopolize legitimate violence. However, the constraint persists not as a zombie piton but as a rope whose coordination function is suppressed by a competing constraint (state law). The R5 mismatch consumer may flag a zombie reading, but the temporal measurements show declining extractiveness and rising theater, consistent with a coordination mechanism under external pressure rather than an atrophied extraction structure. The classification as rope prevents misreading the persistence of the substrate as inertial debris (piton) or as natural law (mountain).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exogenous_legal_sufficiency,
    'Is exogenous legal suppression sufficient to explain the decline of dueling, or did endogenous normative delegitimation occur within the honor-bound community itself?',
    'Comparative historical analysis of jurisdictions with weak versus strong anti-dueling enforcement; if dueling persists where law is weak and collapses where law is strong, exogenous sufficiency is supported.',
    'If endogenous delegitimation is shown, this reading''s rope classification weakens and the composite_overdetermined or cultural_contraction readings become more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exogenous_legal_sufficiency, empirical, 'Whether legal suppression alone caused dueling decline.').

omega_variable(
    substrate_functional_status,
    'Do modern attenuated forms of the honor code (military courts of honor, Southern culture of honor) still perform genuine coordination, or are they primarily theatrical residue?',
    'Ethnographic and institutional analysis of conflict resolution outcomes in honor-carrying communities: if the scripts still determinately resolve status disputes, coordination is genuine; if they are post-hoc rationalizations, the constraint drifts toward piton.',
    'If theatrical, theater_ratio and base_extractiveness would support reclassification toward piton; if functional, the rope claim is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_functional_status, empirical, 'Functional vs theatrical status of attenuated honor forms.').

omega_variable(
    kernel_reading_scope,
    'Does the practice_decline reading''s claim of substrate persistence foreclose the cultural_contraction reading''s claim of foundational normative transformation?',
    'Historical semantic analysis of ''honor'' across centuries: if the term''s normative content is stable, practice_decline is viable; if it shifted from external reputation to internal dignity, cultural_contraction is required.',
    'If semantic stability holds, this reading''s axioms are coherent; if transformation is proven, this reading mischaracterizes the kernel and should be reclassified or deprecated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_scope, conceptual, 'Logical relationship between persistence and transformation readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__practice_decline_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hss_pdr_tr_t0, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(hss_pdr_tr_t20, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(hss_pdr_tr_t40, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(hss_pdr_tr_t60, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(hss_pdr_tr_t80, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement(hss_pdr_tr_t100, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 100, 0.32).

% Extraction over time
narrative_ontology:measurement(hss_pdr_be_t0, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(hss_pdr_be_t20, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(hss_pdr_be_t40, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 40, 0.27).
narrative_ontology:measurement(hss_pdr_be_t60, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 60, 0.24).
narrative_ontology:measurement(hss_pdr_be_t80, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 80, 0.22).
narrative_ontology:measurement(hss_pdr_be_t100, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 100, 0.2).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(honor_satisfaction_substrate__practice_decline_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__practice_decline_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, cultural_contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% The kernel honor_satisfaction_substrate decomposes into three readings because the label 'honor code' conflates structurally distinct claims about persistence, transformation, and causal overdetermination. This reading isolates the exogenous-suppression/practice-persistence claim with its own Îµ and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
