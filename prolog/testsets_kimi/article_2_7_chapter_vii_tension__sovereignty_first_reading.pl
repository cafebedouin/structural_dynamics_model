% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__sovereignty_first_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__sovereignty_first_reading, []).

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
 *   constraint_id: article_2_7_chapter_vii_tension__sovereignty_first_reading
 *   human_readable: Sovereignty-First Reading of UN Charter Non-Intervention
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty-first reading of the UN
 *   Charter framework, centered on Article 2(7) and a narrow interpretation
 *   of Chapter VII. It treats state sovereignty as absolute, domestic
 *   jurisdiction as non-derogable, and lawful intervention as requiring
 *   either explicit territorial consent or Security Council authorization
 *   limited to inter-state aggression. The reading functions within a
 *   commitment system grounded in the UN Charter text and administered by the
 *   P5 veto structure. It provides genuine coordination against unilateral
 *   war and colonial intervention, while simultaneously extracting from
 *   populations under domestic atrocity by blocking protective enforcement.
 *
 * KEY AGENTS:
 *   - post_colonial_authoritarian_states: Primary beneficiary (institutional/constrained) â collects sovereignty protection and domestic autonomy
 *   - populations_under_domestic_atrocity: Primary target (powerless/trapped) â bears the cost of blocked humanitarian intervention
 *   - security_council_p5: Agenda setter (institutional/mobile) â administers veto and interprets Charter limits
 *   - humanitarian_advocacy_networks: Excluded voice (organized/constrained) â argues for broader protection but lacks legal standing
 *   - international_legal_scholars: Analytical observer (analytical/analytical) â documents doctrinal contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.8).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.82).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__sovereignty_first_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__sovereignty_first_reading, "Sovereignty-First Reading of UN Charter Non-Intervention").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__sovereignty_first_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__sovereignty_first_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'cf547ab0-5721-4135-a8f7-b20e018b72b4').
narrative_ontology:cs_kernel_codification('cf547ab0-5721-4135-a8f7-b20e018b72b4', formalized).
narrative_ontology:cs_authority_grounding('cf547ab0-5721-4135-a8f7-b20e018b72b4', lineage).
narrative_ontology:cs_interpretation_layer_present('cf547ab0-5721-4135-a8f7-b20e018b72b4').
narrative_ontology:cs_reading_relation('cf547ab0-5721-4135-a8f7-b20e018b72b4', article_2_7_chapter_vii_tension__r2p_reading, coexists_with).
narrative_ontology:cs_axiom('cf547ab0-5721-4135-a8f7-b20e018b72b4', foundational, absolute_sovereignty_charter_derived).
narrative_ontology:cs_axiom_status(absolute_sovereignty_charter_derived, holdable).
narrative_ontology:cs_axiom_grounding('cf547ab0-5721-4135-a8f7-b20e018b72b4', absolute_sovereignty_charter_derived, conventional).
narrative_ontology:cs_axiom('cf547ab0-5721-4135-a8f7-b20e018b72b4', foundational, chapter_vii_limited_to_interstate_aggression).
narrative_ontology:cs_axiom_status(chapter_vii_limited_to_interstate_aggression, holdable).
narrative_ontology:cs_axiom_grounding('cf547ab0-5721-4135-a8f7-b20e018b72b4', chapter_vii_limited_to_interstate_aggression, conventional).
narrative_ontology:cs_reference_frame('cf547ab0-5721-4135-a8f7-b20e018b72b4', westphalian_sovereignty_supremacy).
narrative_ontology:cs_drift_state('cf547ab0-5721-4135-a8f7-b20e018b72b4', post_cold_war_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cf547ab0-5721-4135-a8f7-b20e018b72b4', '2026-06-20T00:00:00Z').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_authoritarian_states).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive legal insulation from external military intervention and coercive interference under Article 2(7) and a narrow reading of Chapter VII; invoke state sovereignty to resist sanctions, humanitarian corridors, and regime-change operations; their domestic authority is protected by the UN Charter framework as interpreted through this reading.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_authoritarian_states, beneficiary,
    institutional, generational, constrained, global).

% Inhabit states where their own government carries out systematic atrocities; are blocked from receiving protective external intervention because the legal framework prioritizes state consent and restricts Chapter VII to inter-state aggression; their appeals for rescue are routed through a Security Council where veto powers often shield the perpetrator state.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity, payer,
    powerless, immediate, trapped, local).

% Exercise veto authority over Chapter VII resolutions and thereby control whether collective enforcement bypasses state consent; interpret threats to peace narrowly to exclude most domestic atrocities; maintain strategic flexibility to protect allies and themselves from intervention while preserving the formal legal order they helped design.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, security_council_p5, agenda_setter,
    institutional, civilizational, mobile, global).

% Document atrocities and lobby for international protective action; operate under a human rights legal framework that is structurally subordinate to the UN Charter's sovereignty and non-intervention principles in this reading; lack standing to compel Security Council action and are routinely overruled by state-consent objections.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, humanitarian_advocacy_networks, excluded,
    organized, biographical, constrained, global).

% Analyze the doctrinal tension between Article 2(7) and emerging human security norms; publish on the historical evolution of sovereignty; do not themselves benefit from the constraint or bear its direct costs, but observe the interpretive contest between absolute sovereignty and conditional responsibility.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__sovereignty_first_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_authoritarian_states).
narrative_ontology:fixing_cost_class(article_2_7_chapter_vii_tension__sovereignty_first_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents unilateral military intervention and great-power war by establishing state sovereignty as the foundational principle of international order; channels enforcement through collective consent (UN Security Council) and limits it to inter-state aggression, thereby coordinating expectations among states about the boundaries of lawful force.
% TRANSFER_FUNCTION: Moves the cost of domestic atrocity from the international community (which is blocked from acting) onto the population suffering the atrocity; transfers security and autonomy to the territorial state at the expense of internal populations.
% ABSENT_VOICES: Populations under domestic atrocity are not represented in the Charter authorization framework; humanitarian advocacy networks are present in discourse but excluded from the legal decision structure; potential intervening states that would act on moral rather than strategic grounds are legally sidelined.
% DISAPPEARANCE_RATIONALE: If the sovereignty-first reading vanished, the legal barrier to humanitarian intervention would drop; unilateral and collective intervention norms would proliferate; the UNSC's monopoly on enforcement authorization would erode; post-colonial and authoritarian states would lose their legal shield and likely face more frequent cross-border military action or coercive interference.
% FOUNDING_PROBLEM: Preventing great-power war and colonial or imperial intervention by banning unilateral use of force and entrenching self-determination after World War II.
% FOUNDING_PROBLEM_CORROBORATION: Historians and mid-century legal scholars attest the post-WWII founding problem of great-power war and colonial intervention. However, human rights advocates and legal scholars from outside the beneficiary bloc contest that the founding problem persists in a form justifying current shielding of domestic atrocity; they argue the reading has outlived its founding purpose and now serves regime security rather than collective peace.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__sovereignty_first_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__sovereignty_first_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.8, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.80) because the constraint systematically blocks protective intervention for populations under atrocity, transferring the full cost of domestic violence onto them. Suppression is slightly higher (0.82) because the reading actively disqualifies rival norms such as unilateral humanitarian intervention and R2P through Charter-based legal argument and diplomatic pressure. Theater ratio is moderate (0.45): the coordination function (preventing great-power war) is real, but an increasing share of enforcement effort consists of performative legal shielding for atrocity perpetrators. Resistance (0.58) reflects persistent but structurally unsuccessful advocacy from human rights networks and some middle-power states. Accessibility collapse (0.72) captures the near-total formal closure of lawful alternatives once the Charter framework is accepted, even if powerful actors occasionally bypass it.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience this constraint as a necessary bulwark of international order and self-determination. The payer seat experiences it as a lethal legal barrier to rescue. The engine computes this divergence from the structural data: agenda-setters and beneficiaries have institutional power and constrained or mobile exit, while payers are powerless and trapped. The authored claim (tangled_rope) reflects the coexistence of genuine coordination and asymmetric extraction; the metrics are authored independently to describe the constraint's actual operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Post-colonial and authoritarian states are declared beneficiaries, placing their directionality near the full-beneficiary end; their sovereignty is subsidized by the constraint. Populations under domestic atrocity are declared victims, placing their directionality near the full-target end; they bear the effective extraction. The P5 agenda-setters are not declared in either base array, but their institutional power and mobile exit options structurally damp their effective extraction relative to their formal administrative role. Humanitarian networks are excluded rather than coordinated, meaning their directionality is not derived from a beneficiary role but from their constrained exit and organized power, leaving them in a middle-to-high extraction zone where they expend resources against a locked structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents mislabeling this constraint as either pure coordination (rope) or pure extraction (snare). The genuine founding coordination problemâpreventing great-power war and colonial interventionâhas not fully disappeared, which blocks a clean piton or snare classification. However, the reading's current operation shields domestic atrocity systematically enough that it cannot be classified as benign coordination. Mandatrophy would arise if the founding problem (inter-state aggression) were dead while the arrangement persisted solely to protect regimes from accountability; the contested status of the founding problem, combined with rising extractiveness and theater over the measurement interval, keeps the constraint in tangled rope territory rather than resolving toward scaffold or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_naturalness_ambiguity,
    'Is state sovereignty in this reading a functional coordination necessity for international order, or a constructed legal fiction that serves incumbent power holders?',
    'Historical counterfactual analysis of international stability without the sovereignty norm; empirical study of intervention outcomes in cases where the norm was breached.',
    'If purely constructed, the constraint''s coordination function is cover for extraction and the classification should shift toward snare; if functional, the extraction is the price of order and tangled rope remains appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_naturalness_ambiguity, conceptual, 'Whether the sovereignty norm is a functional necessity or a constructed shield.').

omega_variable(
    atrocity_threshold_selectivity,
    'What threshold of domestic atrocity would justify overriding sovereignty under this reading, and is the threshold itself a mechanism for selective enforcement driven by P5 interests?',
    'Comparative case law of UNSC Chapter VII resolutions; statistical analysis of which atrocities trigger authorization and which do not, correlated with P5 strategic interests.',
    'If threshold application correlates with P5 interests rather than atrocity severity, the constraint is more extractive than its coordination function suggests and the theater ratio may be higher than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrocity_threshold_selectivity, empirical, 'Whether atrocity thresholds are applied selectively based on power politics.').

omega_variable(
    kernel_reading_stability,
    'Can the sovereignty-first reading and the R2P reading be logically reconciled within a single international legal framework, or do they represent mutually exclusive authority groundings that will eventually force foreclosure?',
    'Jurisprudential analysis of whether R2P has achieved opinio juris status or remains a political commitment; observation of state practice when the two readings collide in Council deliberations.',
    'If mutually exclusive, the kernel is unstable and one reading will eventually foreclose the other through practice drift or codification; if reconcilable, the constraint may evolve into a more complex conditional form.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Whether the two kernel readings are structurally reconcilable or heading toward foreclosure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(arti_tr_t6, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement(arti_tr_t12, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(arti_tr_t18, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement(arti_tr_t24, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 24, 0.43).
narrative_ontology:measurement(arti_tr_t30, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(arti_be_t6, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(arti_be_t12, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(arti_be_t18, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 18, 0.71).
narrative_ontology:measurement(arti_be_t24, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 24, 0.75).
narrative_ontology:measurement(arti_be_t30, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 30, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(arti_su_t6, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 6, 0.66).
narrative_ontology:measurement(arti_su_t12, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(arti_su_t18, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 18, 0.77).
narrative_ontology:measurement(arti_su_t24, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 24, 0.8).
narrative_ontology:measurement(arti_su_t30, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 30, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__sovereignty_first_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
