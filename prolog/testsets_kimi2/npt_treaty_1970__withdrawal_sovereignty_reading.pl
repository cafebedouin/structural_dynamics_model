% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__withdrawal_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__withdrawal_sovereignty_reading, []).

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
 *   constraint_id: npt_treaty_1970__withdrawal_sovereignty_reading
 *   human_readable: NPT Article X Withdrawal Sovereignty Reading
 *   domain: international_law/nuclear_nonproliferation
 *
 * SUMMARY:
 *   This constraint story instantiates the withdrawal_sovereignty_reading of
 *   the NPT treaty kernel: the interpretation that Article X constitutes a
 *   legitimate exercise of state sovereignty and that NPT obligations are
 *   contingent on the security environment rather than permanently binding.
 *   The kernel is contested by the oligopoly_enforcement_reading (Articles
 *   I-II as primary binding horizontal obligations) and the
 *   reciprocal_disarmament_reading (Article VI as a binding reciprocal
 *   bargain with temporal urgency). Under this reading, regime stability is
 *   extracted from compliant states and converted into option value for
 *   threshold states. The structural data are authored independently of the
 *   claimed type; the engine measures the divergence.
 *
 * KEY AGENTS:
 *   - threshold_states (powerful/mobile): beneficiaries that gain strategic option value from a legitimate withdrawal pathway
 *   - non_nuclear_weapon_states (organized/constrained): primary payers whose security assurance erodes with revocable obligations
 *   - nuclear_weapon_states (institutional/constrained): secondary payers through regime decay and renewed horizontal proliferation risk
 *   - npt_regime_institutions (institutional/analytical): observers that administer the opposing regime-stability reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, 0.58).
domain_priors:suppression_score(npt_treaty_1970__withdrawal_sovereignty_reading, 0.52).
domain_priors:theater_ratio(npt_treaty_1970__withdrawal_sovereignty_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__withdrawal_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__withdrawal_sovereignty_reading, "NPT Article X Withdrawal Sovereignty Reading").
narrative_ontology:topic_domain(npt_treaty_1970__withdrawal_sovereignty_reading, "international_law/nuclear_nonproliferation").

domain_priors:requires_active_enforcement(npt_treaty_1970__withdrawal_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__withdrawal_sovereignty_reading, '3afcb93d-5cc2-46c4-bebe-e151e86ee43f').
narrative_ontology:cs_kernel_codification('3afcb93d-5cc2-46c4-bebe-e151e86ee43f', fixed_text).
narrative_ontology:cs_authority_grounding('3afcb93d-5cc2-46c4-bebe-e151e86ee43f', lineage).
narrative_ontology:cs_interpretation_layer_present('3afcb93d-5cc2-46c4-bebe-e151e86ee43f').
narrative_ontology:cs_reading_relation('3afcb93d-5cc2-46c4-bebe-e151e86ee43f', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('3afcb93d-5cc2-46c4-bebe-e151e86ee43f', npt_treaty_1970__reciprocal_disarmament_reading, coexists_with).
narrative_ontology:cs_axiom('3afcb93d-5cc2-46c4-bebe-e151e86ee43f', foundational, treaty_obligations_security_contingent).
narrative_ontology:cs_axiom_status(treaty_obligations_security_contingent, holdable).
narrative_ontology:cs_axiom_grounding('3afcb93d-5cc2-46c4-bebe-e151e86ee43f', treaty_obligations_security_contingent, conventional).
narrative_ontology:cs_axiom('3afcb93d-5cc2-46c4-bebe-e151e86ee43f', foundational, withdrawal_as_sovereign_prerogative).
narrative_ontology:cs_axiom_status(withdrawal_as_sovereign_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('3afcb93d-5cc2-46c4-bebe-e151e86ee43f', withdrawal_as_sovereign_prerogative, conventional).
narrative_ontology:cs_reference_frame('3afcb93d-5cc2-46c4-bebe-e151e86ee43f', sovereign_conditional_commitment).
narrative_ontology:cs_drift_state('3afcb93d-5cc2-46c4-bebe-e151e86ee43f', contemporary_multipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3afcb93d-5cc2-46c4-bebe-e151e86ee43f', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain advanced nuclear fuel-cycle infrastructure and the latent capacity to weaponize. They cite Article X in review conferences to preserve the legal possibility that a radical deterioration in security could justify withdrawal and nuclear hedging. Their exit from the treaty is textually available and politically rehearsed, giving them leverage over regime negotiations.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states, beneficiary,
    powerful, generational, mobile, global).

% Relinquished nuclear weapons pursuit in exchange for a permanent nonproliferation assurance and promised progress on disarmament. They experience the sovereignty reading as a structural erosion of that permanence: if major states can exit when security shifts, the assurance they purchased becomes conditional. Their own nuclear forbearance is locked in by safeguards and geopolitical position, making exit costly and dangerous.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).

% The five recognized nuclear powers rely on the treaty to prevent horizontal proliferation among allies and adversaries. While the sovereignty reading reduces pressure on their Article VI disarmament obligations, it simultaneously undermines the regime stability that secures their nonproliferation gains, forcing them into diplomatic expenditure to keep threshold states from invoking withdrawal.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states, payer,
    institutional, generational, constrained, global).

% The IAEA safeguards department, NPT review conference bureaus, and treaty depositaries administer verification and negotiation procedures. They operate under a mandate to treat the treaty as a permanent normative structure and are structurally hostile to the sovereignty reading, which they regard as a loophole threatening regime integrity.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, npt_regime_institutions, observer,
    institutional, generational, analytical, global).

% A network of NGOs and diplomatic coalitions that treat the NPT as an irreversible disarmament compact. They are present at review conferences but are rhetorically sidelined when security-contingency framing dominates, because their appeals to permanent legal obligation do not engage the sovereignty vocabulary.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, disarmament_advocacy_coalition, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__withdrawal_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates sovereign state consent to nonproliferation by preserving an explicit exit pathway, allowing states to commit to renunciation conditional on enduring security assessments rather than demanding an irrevocable surrender of the nuclear option.
% TRANSFER_FUNCTION: Transfers regime stability and security assurance from compliant non-nuclear weapon states and nuclear weapon states to threshold states, converting permanent nonproliferation norms into conditional, revocable obligations that generate strategic option value.
% ABSENT_VOICES: States that have already withdrawn or been expelled (e.g., North Korea) are excluded from review-conference discourse; disarmament advocates who treat Article X as a loophole are formally present but rhetorically marginalized when the debate is framed around sovereignty and security contingency.
% DISAPPEARANCE_RATIONALE: If the sovereignty reading vanished and Article X were delegitimized as a mere loophole, threshold states would face higher costs of hedging and potential preventive counterproliferation responses. Some might refuse to join or remain in the treaty, forcing a rearrangement of alliance structures, safeguard architectures, and the basic bargain between nuclear and non-nuclear states.
% FOUNDING_PROBLEM: How to secure nonproliferation commitments from sovereign states in an anarchic international system without a world government capable of enforcing permanent bans, while accommodating legitimate security concerns that might make irrevocable renunciation politically impossible for some states.
% FOUNDING_PROBLEM_CORROBORATION: Declassified negotiating history (1965-1968) records that several non-aligned states demanded an explicit withdrawal clause as a condition of consent, corroborated by diplomatic cables from depositary states. However, the vast majority of current non-nuclear weapon states and regime institutions attest that the founding security-contingency problem has been superseded by a permanent nonproliferation norm, and that modern security assurances should be addressed through ancillary instruments rather than treaty exit.
narrative_ontology:disappearance_verdict(npt_treaty_1970__withdrawal_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__withdrawal_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_1970__withdrawal_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the asymmetric transfer of regime stability from compliant states to threshold states. Suppression (0.52) captures the active suppression of the 'permanently binding treaty' norm. Theater ratio (0.38) reflects the growing performative quality of Article X invocations at review conferences. Accessibility collapse (0.48) indicates that once the security-contingency frame is accepted, alternatives like irrevocable renunciation become politically inaccessible. Resistance (0.62) is high because regime institutions and most NNWS actively contest this reading. The claim is tangled_rope because the constraint carries a genuine coordination function (sovereign consent to nonproliferation) alongside asymmetric extraction (option value for threshold states) and requires active enforcement (legal argumentation and political rehearsal of withdrawal).
 *
 * PERSPECTIVAL GAP:
 *   From the threshold-state seat, the constraint is a necessary sovereignty safeguard that makes the treaty politically survivable. From the non-nuclear weapon state seat, it is a unilateral escape clause that hollows out the grand bargain. The engine computes this divergence from the structural data: same treaty text, opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   threshold_states are beneficiaries (low d) because the constraint subsidizes their strategic flexibility; their mobile exit options confirm this. non_nuclear_weapon_states and nuclear_weapon_states are payers (high d) because the constraint extracts permanence from their commitment. npt_regime_institutions and disarmament_advocacy_coalition are observers with analytical exit, placing them near symmetric d.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the mandatrophy guard, the sovereignty reading could be misclassified as a rope if one only looked at the coordination function (preserving sovereign consent). Declaring the victims (compliant states) and the active enforcement requirement prevents this: the constraint would collapse if the regime stopped contesting it, and the option-value transfer is asymmetric. The founding problem (securing consent without world government) is contested because some parties claim it is still live while others treat it as superseded; this contested status flags the tangled rope classification against scaffold or rope misreadings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    withdrawal_legitimacy_scope,
    'Does the Article X sovereignty reading apply legitimately to all parties, or has it been structurally captured by threshold states as an option-value extraction device?',
    'Compare invocation patterns: if small non-nuclear states without latent capacity cite Article X symmetrically, the reading is general; if invocation correlates exclusively with threshold-state hedging behavior, it is captured.',
    'General application supports the coordination framing; threshold-state capture supports reclassification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_legitimacy_scope, empirical, 'Whether the sovereignty reading is captured by threshold states').

omega_variable(
    security_environment_indeterminacy,
    'What constitutes a ''supreme interest'' or radical security-environment change sufficient to legitimate Article X withdrawal, and who decides?',
    'Legal analysis of state practice and ICJ advisory capacity; empirical tracking of post-withdrawal security justifications.',
    'If the threshold is intrinsically indeterminate, the reading functions as a revocability clause without constraint, amplifying extraction; if determinable, it remains bounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_environment_indeterminacy, conceptual, 'Indeterminacy of the security-environment trigger').

omega_variable(
    committer_reading_boundary,
    'Is this constraint a sincere legal interpretation of Article X or a strategic instrumentalization of treaty text to preserve nuclear option value?',
    'Examine state practice and opinio juris: if invoking states submit to post-withdrawal safeguards and cite genuine security necessity, the reading is interpretive; if invocation correlates with breakout procurement, it is instrumental.',
    'If instrumental, the constraint is a snare using legal cover; if interpretive, it is a tangled rope with genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_boundary, conceptual, 'Whether the sovereignty reading is interpretive or instrumental').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__withdrawal_sovereignty_reading, 0, 54).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(npt__tr_t9, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 9, 0.18).
narrative_ontology:measurement(npt__tr_t18, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement(npt__tr_t27, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 27, 0.26).
narrative_ontology:measurement(npt__tr_t36, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 36, 0.3).
narrative_ontology:measurement(npt__tr_t45, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 45, 0.34).
narrative_ontology:measurement(npt__tr_t54, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 54, 0.38).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(npt__be_t9, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 9, 0.35).
narrative_ontology:measurement(npt__be_t18, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 18, 0.4).
narrative_ontology:measurement(npt__be_t27, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 27, 0.46).
narrative_ontology:measurement(npt__be_t36, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 36, 0.51).
narrative_ontology:measurement(npt__be_t45, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 45, 0.55).
narrative_ontology:measurement(npt__be_t54, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 54, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(npt__su_t9, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 9, 0.3).
narrative_ontology:measurement(npt__su_t18, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 18, 0.35).
narrative_ontology:measurement(npt__su_t27, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 27, 0.4).
narrative_ontology:measurement(npt__su_t36, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 36, 0.45).
narrative_ontology:measurement(npt__su_t45, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 45, 0.49).
narrative_ontology:measurement(npt__su_t54, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 54, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__withdrawal_sovereignty_reading, global_infrastructure).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, reciprocal_disarmament_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT treaty kernel (npt_treaty_1970). The kernel decomposes into three structurally distinct constraints because the natural-language label 'NPT regime' conflates competing normative claims: horizontal oligopoly enforcement, reciprocal vertical disarmament, and sovereign conditional commitment. Each reading has a distinct epsilon, beneficiary structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
