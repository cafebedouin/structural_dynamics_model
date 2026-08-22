% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__constitutional_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Constitutional Hybrid Sovereignty (Dual-Source Legitimacy)
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   This constraint instantiates the constitutional_hybrid_reading of the
 *   sovereign_legitimacy kernel. It posits that legitimate authority is
 *   dual-sourced: ceremonial/symbolic authority is inherited through
 *   bloodline, while political authority is delegated through electoral
 *   consent, with constitutional law and judicial interpretation mediating
 *   the boundary. This reading is structurally distinct from its sibling
 *   readings: the monarchical_reading (pure downward divine-right
 *   sovereignty) and the republican_reading (pure upward popular
 *   sovereignty). The hybrid reading forecloses both pure forms within a
 *   single framework, creating a compromise that reduces the extractiveness
 *   of absolutism and republicanism alike while imposing ambiguity costs on
 *   those seeking pure regime forms.
 *
 * KEY AGENTS:
 *   - hereditary_monarch (institutional/identity_locked): Retained ceremonial beneficiary shielded by the constitutional settlement.
 *   - elected_officials (institutional/mobile): Delegated political power beneficiary.
 *   - absolutists (moderate/constrained): Payer constrained by limits on royal prerogative.
 *   - republicans (moderate/constrained): Payer constrained by entrenched hereditary ceremonial authority.
 *   - constitutional_courts (institutional/constrained): Agenda-setter administering the boundary through interpretation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__constitutional_hybrid_reading, 0.42).
domain_priors:suppression_score(sovereign_legitimacy__constitutional_hybrid_reading, 0.5).
domain_priors:theater_ratio(sovereign_legitimacy__constitutional_hybrid_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__constitutional_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__constitutional_hybrid_reading, "Constitutional Hybrid Sovereignty (Dual-Source Legitimacy)").
narrative_ontology:topic_domain(sovereign_legitimacy__constitutional_hybrid_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(sovereign_legitimacy__constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__constitutional_hybrid_reading, 'f4b022f3-601f-48e4-be54-1cbcd9fcda3e').
narrative_ontology:cs_kernel_codification('f4b022f3-601f-48e4-be54-1cbcd9fcda3e', formalized).
narrative_ontology:cs_authority_grounding('f4b022f3-601f-48e4-be54-1cbcd9fcda3e', lineage).
narrative_ontology:cs_interpretation_layer_present('f4b022f3-601f-48e4-be54-1cbcd9fcda3e').
narrative_ontology:cs_reading_relation('f4b022f3-601f-48e4-be54-1cbcd9fcda3e', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('f4b022f3-601f-48e4-be54-1cbcd9fcda3e', sovereign_legitimacy__republican_reading, forecloses).
narrative_ontology:cs_axiom('f4b022f3-601f-48e4-be54-1cbcd9fcda3e', foundational, legitimacy_dual_origin).
narrative_ontology:cs_axiom_status(legitimacy_dual_origin, holdable).
narrative_ontology:cs_axiom_grounding('f4b022f3-601f-48e4-be54-1cbcd9fcda3e', legitimacy_dual_origin, deontological).
narrative_ontology:cs_axiom('f4b022f3-601f-48e4-be54-1cbcd9fcda3e', foundational, constitutional_mediation_imperative).
narrative_ontology:cs_axiom_status(constitutional_mediation_imperative, holdable).
narrative_ontology:cs_axiom_grounding('f4b022f3-601f-48e4-be54-1cbcd9fcda3e', constitutional_mediation_imperative, instrumental).
narrative_ontology:cs_reference_frame('f4b022f3-601f-48e4-be54-1cbcd9fcda3e', dual_source_constitutional_settlement).
narrative_ontology:cs_drift_state('f4b022f3-601f-48e4-be54-1cbcd9fcda3e', contemporary_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('f4b022f3-601f-48e4-be54-1cbcd9fcda3e', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, absolutists).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, republicans).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_boundary_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains ceremonial status, symbolic authority, and constitutional protection of the crown under the settlement. Cannot reclaim full political power without constitutional rupture, but is shielded from republican abolition by the same arrangement. Income and public role are guaranteed by the hybrid order.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch, beneficiary,
    institutional, civilizational, identity_locked, national).

% Exercise delegated political authority within the constitutional framework. Benefit from the legitimacy and stability conferred by the symbolic monarchical anchor, which distances them from direct claims of usurpation while they govern.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, beneficiary,
    institutional, biographical, mobile, national).

% Advocate for concentration of sovereignty in the monarch and rejection of popular delegation. Are constrained by constitutional limits on royal prerogative and by the legal prohibition of absolutist restoration.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, absolutists, payer,
    moderate, generational, constrained, national).

% Advocate for abolition of the hereditary principle and full popular sovereignty. Are constrained by the constitutional entrenchment of the monarchy and by the settlement that reserves ceremonial authority to the crown.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, republicans, payer,
    moderate, generational, constrained, national).

% Adjudicate disputes over the boundary between ceremonial and political authority through constitutional interpretation and precedent. Their rulings actively maintain the hybrid by enforcing the separation and allocation of powers.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__constitutional_hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__constitutional_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents civil conflict over sovereignty by splitting authority between an inherited ceremonial source and a delegated political source, providing stable succession without absolute rule and democratic governance without revolutionary rupture.
% TRANSFER_FUNCTION: Moves symbolic capital and constitutional protection to the hereditary monarch; moves policy-making power and governing legitimacy to elected officials; moves compliance and loyalty from the population to both sources through constitutional mediation.
% ABSENT_VOICES: Radical republicans who reject any hereditary element and ultra-monarchists who reject popular delegation are formally excluded from the constitutional bargaining table; their exclusion is the condition of the hybrid's stability.
% DISAPPEARANCE_RATIONALE: If the constitutional boundary vanished overnight, the dual source would collapse into a contest between hereditary claim and popular mandate; the polity would rearrange toward either republican abolition of the crown, monarchical seizure of full power, or civil conflict over sovereignty.
% FOUNDING_PROBLEM: How to secure stable legitimate government after rejecting absolute monarchy without resorting to revolutionary republicanism that might destabilize property, tradition, and social order.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative political scientists outside the benefiting institutions attest the hybrid was designed to manage post-absolutist transition. Republican critics and monarchist traditionalists corroborate that the original problem has morphed, while liberal constitutionalists argue it remains live.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__constitutional_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__constitutional_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sovereign_legitimacy__constitutional_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 0.42, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low-to-moderate (0.42) because the hybrid is explicitly a compromise: neither pure monarchical extraction nor pure majoritarian domination prevails; the cost is ambiguity and the suppression of pure-form advocates. Suppression (0.50) reflects the constitutional and legal mechanisms required to bar absolutist restoration and republican abolition. Theater_ratio (0.48) is moderately high because ceremonial authority is inherently performative, and the ritual of monarchy increases as political power shifts to elected bodies. Accessibility_collapse (0.60) indicates that pure alternatives become constitutionally illegitimate, though they persist as live ideological positions. Resistance (0.40) captures ongoing republican and absolutist opposition. Temporal measurements show declining suppression_requirement as the settlement normalizes, and rising theater_ratio as the ceremonial function becomes more symbolic over time.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (monarch, elected officials) experience the constraint as a stabilizing equilibrium that secures their respective authorities. The payer seats (absolutists, republicans) experience the same structure as an enforced blockage of their preferred regime. The engine computes this divergence from the structural data: identical national scope but opposite beneficiary/victim declarations and constrained exit for the payers produce divergent directionality and effective extraction per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary_monarch and elected_officials are declared beneficiaries with institutional power; the monarch is identity_locked to the role, while officials are mobile. Both receive low directionality, dampening effective extraction into subsidy or low cost. The absolutists and republicans are declared victims with moderate power and constrained exit, yielding high directionality that amplifies effective extraction. Constitutional_courts are agenda_setters, not beneficiaries or victims; their directionality is structurally intermediate but enforcement role is captured in suppression metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by exhibiting both genuine coordination (it solves the succession/legitimacy problem without civil war) and asymmetric extraction (it suppresses pure-form advocates). The presence of declared beneficiaries on both sides of the authority split, plus declared victims among those excluded from the compromise, plus active enforcement through constitutional interpretation, satisfies the Tangled Rope gate. It is not a Snare because the coordination function is structurally real; it is not a Rope because identifiable parties bear the costs of exclusion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereign_legitimacy_kernel_position,
    'Is the constitutional hybrid reading of sovereign legitimacy a genuine coordination equilibrium or a transitional scaffold that has ossified?',
    'Comparative historical analysis of constitutional monarchies that transitioned to republics versus those that retained hybrid form; measure whether the hybrid persists because it solves a live coordination problem or because incumbents block reform.',
    'If transitional, the constraint should be reclassified as scaffold or piton; if equilibrium, tangled_rope remains appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereign_legitimacy_kernel_position, conceptual, 'Whether the hybrid is a permanent equilibrium or transitional scaffold').

omega_variable(
    boundary_dispute_ambiguity,
    'Does the constitutional mediation of the ceremonial/political boundary inherently favor one source over time, eroding the dual-source claim?',
    'Track constitutional evolution: does political authority gradually subsume ceremonial functions (republican drift) or does ceremonial authority reclaim political influence (monarchical drift)?',
    'If drift is systematic, the hybrid is unstable and the reading may need reclassification; if stable oscillation, the ambiguity cost is the price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_dispute_ambiguity, empirical, 'Whether the dual-source boundary drifts toward one pole').

omega_variable(
    pure_form_exclusion_nature,
    'Are absolutists and republicans genuinely victimized by the hybrid constraint, or are they merely political minorities in a legitimate democratic order?',
    'Examine whether the constitutional order suppresses their speech and organization through structural legal barriers, or simply outvotes them through ordinary democratic procedures.',
    'If mere minority status, epsilon may be overestimated; if structurally suppressed, the victim designation holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pure_form_exclusion_nature, conceptual, 'Whether exclusion of pure-form advocates is suppression or democratic outcome').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__constitutional_hybrid_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(sove_tr_t20, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(sove_tr_t60, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement(sove_tr_t80, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 80, 0.45).
narrative_ontology:measurement(sove_tr_t100, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sove_be_t20, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 40, 0.39).
narrative_ontology:measurement(sove_be_t60, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 60, 0.41).
narrative_ontology:measurement(sove_be_t80, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 80, 0.42).
narrative_ontology:measurement(sove_be_t100, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(sove_su_t20, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(sove_su_t60, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 60, 0.53).
narrative_ontology:measurement(sove_su_t80, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 80, 0.51).
narrative_ontology:measurement(sove_su_t100, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 100, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__constitutional_hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, republican_reading).

% DUAL FORMULATION NOTE:
% The sovereign_legitimacy kernel decomposes into three structurally distinct readings because the label 'legitimate authority' conflates exclusive downward, exclusive upward, and dual-sourced claims. Each reading has a different epsilon, beneficiary/victim structure, and classification. The hybrid reading sits between the two pure forms, linking to both as the compromise position that forecloses each exclusivity claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
