% ============================================================================
% CONSTRAINT STORY: permissive_license_text__copyleft_counterfactual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__copyleft_counterfactual_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: permissive_license_text__copyleft_counterfactual_reading
 *   human_readable: Permissive License Exploitation Without Reciprocity (Copyleft Counterfactual Reading)
 *   domain: technology/intellectual_property/governance
 *
 * SUMMARY:
 *   This constraint story instantiates the COPYLEFT COUNTERFACTUAL READING of
 *   permissive licensing: the view that permissive licenses (MIT, Apache,
 *   BSD) without viral reciprocity requirements structurally enable
 *   proprietary builders to extract value from open-source labor without
 *   reciprocal contribution or commons reinvestment. The reading emphasizes
 *   that copyleft (GPL) is the necessary alternative that would enforce
 *   reciprocity and preserve commons integrity. This is one reading of a
 *   contested kernel (permissive_license_text); other readings
 *   (commons_coordination, corporate_moat) instantiate different constraints
 *   with different ε values and beneficiary/victim structures. The reading's
 *   core premise: permissive licensing without reciprocity is a TANGLED ROPE
 *   where technical coordination is genuine but asymmetric extraction is
 *   endemic and requires active enforcement (proprietary-builder preference
 *   aggregation + social norm maintenance).
 *
 * KEY AGENTS:
 *   - copyleft_advocates: organized beneficiaries of reciprocity norms; set the counterfactual framing
 *   - open_source_maintainers: payer, identity-locked; bear opportunity cost and burnout from unreciprocated use
 *   - proprietary_builders: agenda-setter, institutional; aggregate behavior enforces permissive acceptance
 *   - end_users: dual-position (beneficiary of features, payer via reduced upstream innovation)
 *   - FSF/observer: articulates the copyleft counterfactual as structural remedy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, 0.78).
domain_priors:suppression_score(permissive_license_text__copyleft_counterfactual_reading, 0.62).
domain_priors:theater_ratio(permissive_license_text__copyleft_counterfactual_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__copyleft_counterfactual_reading, tangled_rope).
narrative_ontology:human_readable(permissive_license_text__copyleft_counterfactual_reading, "Permissive License Exploitation Without Reciprocity (Copyleft Counterfactual Reading)").
narrative_ontology:topic_domain(permissive_license_text__copyleft_counterfactual_reading, "technology/intellectual_property/governance").

domain_priors:requires_active_enforcement(permissive_license_text__copyleft_counterfactual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__copyleft_counterfactual_reading, '4e165d96-1fd5-4fb4-b239-faf541582137').
narrative_ontology:cs_kernel_codification('4e165d96-1fd5-4fb4-b239-faf541582137', distributed).
narrative_ontology:cs_authority_grounding('4e165d96-1fd5-4fb4-b239-faf541582137', practice).
narrative_ontology:cs_interpretation_layer_present('4e165d96-1fd5-4fb4-b239-faf541582137').
narrative_ontology:cs_reading_relation('4e165d96-1fd5-4fb4-b239-faf541582137', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('4e165d96-1fd5-4fb4-b239-faf541582137', permissive_license_text__corporate_moat_reading, influences).
narrative_ontology:cs_axiom('4e165d96-1fd5-4fb4-b239-faf541582137', foundational, reciprocal_contribution_necessary).
narrative_ontology:cs_axiom_status(reciprocal_contribution_necessary, holdable).
narrative_ontology:cs_axiom_grounding('4e165d96-1fd5-4fb4-b239-faf541582137', reciprocal_contribution_necessary, deontological).
narrative_ontology:cs_axiom('4e165d96-1fd5-4fb4-b239-faf541582137', secondary, proprietary_extraction_requires_commons_violation).
narrative_ontology:cs_axiom_status(proprietary_extraction_requires_commons_violation, holdable).
narrative_ontology:cs_axiom_grounding('4e165d96-1fd5-4fb4-b239-faf541582137', proprietary_extraction_requires_commons_violation, empirically_contingent).
narrative_ontology:cs_reference_frame('4e165d96-1fd5-4fb4-b239-faf541582137', permissive_licensing_as_extraction_enabler).
narrative_ontology:cs_drift_state('4e165d96-1fd5-4fb4-b239-faf541582137', contemporary_burnout_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4e165d96-1fd5-4fb4-b239-faf541582137', '').
narrative_ontology:cs_kernel_id(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, commons_beneficiaries).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, open_source_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, upstream_contributors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, end_users).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promotes viral reciprocity norms and articulates the counterfactual (copyleft as necessary remedy). Benefits from the constraint's visibility and contestation, which validates their framing. They choose which projects to advocate for; they can shift allegiance across organizations and movements; they are not trapped by the arrangement.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates, beneficiary,
    organized, generational, mobile, global).

% Contribute code under permissive licenses; experience silent forking and proprietary enclosure. Their exit is constrained because maintainer identity is fused with their professional role, relational identity (communities depend on them), and ideological identity (opening code means accepting any distribution). The constraint operates by making their exit conditional on abandoning these identities.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, open_source_maintainers, payer,
    moderate, biographical, identity_locked, global).

% Use permissive-licensed components in proprietary products without reciprocal obligation. They set the de-facto licensing norm by aggregated license acceptance decisions. They can arbitrage between permissive and proprietary, using open-source where it fits and building proprietary where it doesn't. Their institutional power and arbitrage exit give them the structural position to enforce the permissive norm.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, proprietary_builders, agenda_setter,
    institutional, biographical, arbitrage, global).

% Benefit from fast feature integration (proprietary builders ship features faster because they use open-source components without reciprocal obligation). They bear the cost indirectly: upstream innovation incentives decline as maintainers burn out, and they lose access to improvements the proprietary builder makes (proprietary derivatives are closed to inspection/modification).
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, end_users, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__copyleft_counterfactual_reading, end_users, payer).

% Articulates the counterfactual norm and the copyleft remedy. They are not trapped in the constraint but observe it analytically, advocating for GPL adoption and reciprocity enforcement. Their position is independent of the constraint's persistence; they can shift strategy without losing their organizational integrity.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, free_software_fsf, observer,
    organized, generational, analytical, global).

% Often sponsor open-source development under permissive licenses (to maximize adoption) but are excluded from the license-norm conversation. They would benefit from viral reciprocity (ensuring funded research stays open and contributes back to the commons) but the license choice is not under their control once project independence begins. They are structurally outside the decision-making about permissive vs. reciprocal norms.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, academic_institutions, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__copyleft_counterfactual_reading, proprietary_builders).
narrative_ontology:fixing_cost_class(permissive_license_text__copyleft_counterfactual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permissive licensing creates frictionless technical coordination: code can flow freely between proprietary and open-source contexts, reducing legal overhead and enabling faster ecosystem integration.
% TRANSFER_FUNCTION: Moves open-source contributors' unpaid labor and innovation into proprietary derivative products without reciprocal contribution, improvement sharing, or commons reinvestment.
% ABSENT_VOICES: Upstream contributors who have exited the commons (burned out, proprietary-captured, or deprioritized) are not speaking; their absence creates selection bias. Research communities measuring innovation incentives are excluded from the license-norm conversation.
% DISAPPEARANCE_RATIONALE: If the permissive-license norm disappeared and copyleft reciprocity became mandatory, proprietary builders would reorganize around GPL adoption, in-house development, or licensing fees. The open-source commons would experience increased reinvestment and maintainer retention. The software ecosystem's power distribution would fundamentally rearrange.
% FOUNDING_PROBLEM: Early open-source faced legal uncertainty and license fragmentation. Permissive licensing (MIT, Apache, BSD) emerged to minimize legal friction and maximize adoption of open-source components in any context.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and copyleft advocates attest the founding problem is SOLVED and the permissive-license persistence is extractive. Proprietary builders attest the founding problem is still live. Independent research on maintainer burnout and long-tail innovation supports the copyleft reading.
narrative_ontology:disappearance_verdict(permissive_license_text__copyleft_counterfactual_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__copyleft_counterfactual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__copyleft_counterfactual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(permissive_license_text__copyleft_counterfactual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__copyleft_counterfactual_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is HIGH (0.78 at interval end) because the constraint's operation moves open-source contributions into proprietary derivative products without reciprocal obligation, and the beneficiary (proprietary builder) captures the value. Suppression is MODERATE-HIGH (0.62) because maintainers' exit is constrained by identity fusion (role-as-maintainer is inseparable from their professional identity); the constraint persists by making exit conditional on accepting the exploitation outcome. Theater is LOW-MODERATE (0.28): the framing of permissive licensing as 'maximizing freedom' is real but increasingly theatrical as the extraction pattern becomes visible. The measurement series track extractiveness rising as proprietary enclosure accumulates and suppression stabilizes as maintainer identities lock. Theater remains below 0.5 because the coordination (technical integration) is genuine, not performed.
 *
 * PERSPECTIVAL GAP:
 *   The proprietary-builder seat and the maintainer seat compute divergent types from the same structural data. From the builder's position, permissive licensing is genuine coordination (legal friction removed, faster innovation) with manageable extraction (they paid for talent, infrastructure). From the maintainer's position, the same structure is enforced asymmetry: they chose to open-source believing in reciprocal improvement cycles; the proprietary builder's refusal to reciprocate is experienced as a broken norm, not a neutral choice. The engine computes this divergence from the stakeholder-specific exit options (builder: arbitrage; maintainer: identity-locked) and power distribution (institutional vs. moderate). The analytical seat (FSF) sees the structure as intentionally designed to favor proprietary extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyleft advocates are beneficiaries because the constraint's visibility and contestation validates their counterfactual framing: permissive licenses ARE extractive, and copyleft reciprocity IS the necessary alternative. Their benefit is norm-setting authority and confirmation that the commons requires protection. Open-source maintainers are victims because the constraint extracts their labor (technical contribution), their innovation (improvements go into proprietary products), and their time (maintenance burden unpaid). The constraint's persistence depends on their identity-lock: they cannot exit without abandoning the professional/ideological identity that defines them. Proprietary builders are the structural beneficiaries (they collect uncompensated improvements) but are named as agenda-setter because their aggregated license preferences enforce the permissive norm. From their position, the constraint is experienced as beneficial coordination, not extraction — that divergence is the measurement the engine takes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (license fragmentation, legal friction) is CONTESTED in status: proprietary builders attest it is LIVE (permissive licensing still necessary for ecosystem adoption); copyleft advocates and burnout researchers attest it is DEAD (the friction is solved; persistence is extractive rent-seeking). The DISAPPEARANCE VERDICT is world_rearranges because the permissive-license norm shapes downstream proprietary investment, maintainer career trajectories, and commons reinvestment patterns. If the constraint disappeared (reciprocity became mandatory), proprietary builders would reorganize around GPL compliance or licensing fees; the commons would experience increased reinvestment. This mismatch (founding_problem_status=contested, disappearance_verdict=world_rearranges) flags the constraint as a mandatrophy candidate: a persistent arrangement whose original justification is contentious, sustained by beneficiary preference aggregation and norm enforcement rather than by ongoing problem-solving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_necessity,
    'Is viral reciprocity (GPL-style copyleft) NECESSARY to preserve the open-source commons, or is it one viable norm among several?',
    'Long-term empirical comparison: do permissive-license projects experience higher maintainer burnout, lower commons reinvestment, and faster proprietary capture than GPL projects controlling for age, domain, and scale? Do GPL projects show higher sustained contribution rates?',
    'If reciprocity is NECESSARY, the copyleft reading''s framing is validated and permissive licensing''s persistence becomes a genuine mandate violation (founding problem solved, extraction sustained). If it is one viable norm, the copyleft reading is one contestable position among multiple valid arrangements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_necessity, empirical, 'Whether copyleft reciprocity is structurally necessary for commons health or one contingent norm among options').

omega_variable(
    identity_lock_internalization,
    'Is maintainer identity-lock (the inability/unwillingness to exit the contributor role) structural (external barriers like career incentives, reputation) or internalized (the maintainer has fused their self-concept with the role)?',
    'Post-burnout trajectory data: when maintainers exit open-source (leave projects), do they carry the identity-fusion with them (feeling guilty, inadequate, untethered) or do they cleanly exit the role? Do they return to open-source after recovery?',
    'If structural, the suppression is external and removable (change career incentives, reputation systems). If internalized, the suppression is carried with the agent and the constraint''s effective suppression is higher than the structural measure suggests — the target internalizes the lock.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_internalization, empirical, 'Whether maintainer identity-lock is structural or internalized suppression').

omega_variable(
    commons_coordination_vs_extraction_boundary,
    'At what point does the technical coordination function (permissive licensing''s genuine benefit: frictionless integration) become separable from the extraction function (proprietary builders'' uncompensated leverage)?',
    'Thought experiment + policy design: could a hybrid licensing scheme (permissive for academic/non-commercial, reciprocal for commercial) preserve the coordination function while removing extraction? Or is the extraction inseparable from the frictionless technical design?',
    'If separable, the constraint is FIXABLE without abandoning permissive licensing entirely — split the license terms. If inseparable, then the copyleft reading is correct: viral reciprocity is the only way to preserve commons coordination while removing extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_coordination_vs_extraction_boundary, conceptual, 'Whether technical coordination and extraction functions are structurally separable under permissive licensing').

omega_variable(
    reading_foreclosure_or_coexistence,
    'Does the copyleft counterfactual reading logically FORECLOSE the commons_coordination reading (that permissive licensing maximizes freedom), or do both readings remain COEXISTENT in different institutional frameworks?',
    'Analyze the core premises: copyleft reads permissive licensing as enabling extraction (core: reciprocity is necessary); commons_coordination reads it as maximizing implementation freedom (core: legal friction is the constraint). The premises are not logically contradictory — they emphasize different outcomes of the same arrangement. The readings coexist when held by different parties in an ongoing dispute.',
    'If readings FORECLOSE each other, only one is holdable and the constraint classification is singular. If they COEXIST, the readings are siblings in a constraint family and the copyleft reading is one valid perspective among live alternatives. Preliminary assessment: the readings COEXIST; the foreclosure reading claims to resolve the dispute but the empirical question (reciprocity necessary?) is not yet settled.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_or_coexistence, conceptual, 'Whether the copyleft and commons-coordination readings logically foreclose each other or coexist as live alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__copyleft_counterfactual_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(perm_tr_t3, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 3, 0.15).
narrative_ontology:measurement(perm_tr_t7, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 7, 0.19).
narrative_ontology:measurement(perm_tr_t12, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(perm_tr_t18, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 18, 0.26).
narrative_ontology:measurement(perm_tr_t25, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 0, 0.51).
narrative_ontology:measurement(perm_be_t3, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(perm_be_t7, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 7, 0.65).
narrative_ontology:measurement(perm_be_t12, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 12, 0.71).
narrative_ontology:measurement(perm_be_t18, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 18, 0.76).
narrative_ontology:measurement(perm_be_t25, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 25, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(perm_su_t3, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 3, 0.52).
narrative_ontology:measurement(perm_su_t7, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 7, 0.56).
narrative_ontology:measurement(perm_su_t12, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 12, 0.59).
narrative_ontology:measurement(perm_su_t18, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 18, 0.61).
narrative_ontology:measurement(perm_su_t25, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__copyleft_counterfactual_reading, information_standard).
narrative_ontology:boltzmann_floor_override(permissive_license_text__copyleft_counterfactual_reading, 0.08).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, corporate_moat_reading).

% DUAL FORMULATION NOTE:
% The permissive_license_text kernel decomposes into three constraint stories: (1) commons_coordination_reading — permissive licensing maximizes freedom and enables universal ecosystem participation; (2) corporate_moat_reading — permissive licensing enables proprietary builders to establish uncompensated extraction moats; (3) copyleft_counterfactual_reading (this story) — permissive licensing without reciprocity is extractive and copyleft reciprocity is the necessary alternative. Each reading instantiates a different constraint with a different ε, beneficiary/victim structure, and claimed type. They are linked via network.affects_constraints because they contest the same kernel and their classifications depend on which reading is adopted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
