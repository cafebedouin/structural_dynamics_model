% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__sovereignty_first_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_2_7_chapter_vii_tension__sovereignty_first_reading
 *   human_readable: Article 2(7) / Chapter VII Tension: Sovereignty-First Reading
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint embodies the sovereignty-first reading of the Article
 *   2(7) / Chapter VII tension. It holds that state sovereignty is
 *   foundational and that external intervention in a state's internal affairs
 *   is permissible only under two conditions: (1) explicit consent from the
 *   target state, or (2) Security Council authorization under Chapter VII,
 *   which is formally limited to threats to international peace and security
 *   (traditionally interpreted as inter-state aggression, not internal
 *   atrocity). This reading prioritizes the principle that states are free
 *   from external coercion in their domestic governance. It benefits
 *   post-colonial and authoritarian states by providing a legal shield
 *   against humanitarian intervention justifications. It extracts a severe
 *   cost from populations suffering systematic violence at the hands of their
 *   own governments, who cannot claim international protection as a right
 *   under this reading. The constraint is actively enforced through
 *   diplomatic resistance to R2P norm-setting, vetoes of humanitarian
 *   intervention authorizations, and rhetorical defense of Article 2(7) in
 *   international forums.
 *
 * KEY AGENTS:
 *   - post_colonial_states: Primary beneficiary; jointly defend Article 2(7) to maintain sovereignty shield against stronger states.
 *   - authoritarian_regimes: Primary beneficiary; use sovereignty shield to conduct internal repression with reduced intervention risk.
 *   - populations_under_domestic_atrocity: Primary victim; lack legal pathway to external protection under this reading.
 *   - security_council_permanent_members: Agenda-setters and enforcers; administer Chapter VII gate and use veto power to block humanitarian authorizations.
 *   - western_powers: Payers; constrained in their humanitarian intervention objectives by the sovereignty-first reading's legal force.
 *   - international_court_system: Analytical observer; interprets Article 2(7) and adjudicates boundary cases between sovereignty and humanitarian intervention.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.78).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.71).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__sovereignty_first_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__sovereignty_first_reading, "Article 2(7) / Chapter VII Tension: Sovereignty-First Reading").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__sovereignty_first_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__sovereignty_first_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__sovereignty_first_reading, '0ea5da2d-622e-4b6e-859b-64491783b303').
narrative_ontology:cs_kernel_codification('0ea5da2d-622e-4b6e-859b-64491783b303', fixed_text).
narrative_ontology:cs_authority_grounding('0ea5da2d-622e-4b6e-859b-64491783b303', extraction).
narrative_ontology:cs_interpretation_layer_present('0ea5da2d-622e-4b6e-859b-64491783b303').
narrative_ontology:cs_reading_relation('0ea5da2d-622e-4b6e-859b-64491783b303', article_2_7_chapter_vii_tension__r2p_reading, forecloses).
narrative_ontology:cs_axiom('0ea5da2d-622e-4b6e-859b-64491783b303', foundational, state_sovereignty_is_foundational).
narrative_ontology:cs_axiom_status(state_sovereignty_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('0ea5da2d-622e-4b6e-859b-64491783b303', state_sovereignty_is_foundational, deontological).
narrative_ontology:cs_axiom('0ea5da2d-622e-4b6e-859b-64491783b303', foundational, intervention_requires_consent_or_chapter_vii).
narrative_ontology:cs_axiom_status(intervention_requires_consent_or_chapter_vii, holdable).
narrative_ontology:cs_axiom_grounding('0ea5da2d-622e-4b6e-859b-64491783b303', intervention_requires_consent_or_chapter_vii, conventional).
narrative_ontology:cs_reference_frame('0ea5da2d-622e-4b6e-859b-64491783b303', charter_non_intervention_principle).
narrative_ontology:cs_drift_state('0ea5da2d-622e-4b6e-859b-64491783b303', contemporary_atrocity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0ea5da2d-622e-4b6e-859b-64491783b303', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_states).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, authoritarian_regimes).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_domestic_atrocity).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__sovereignty_first_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__sovereignty_first_reading, 'none', 1).

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
 *   Extractiveness is high (0.78 at interval end) because the constraint blocks access to international protection for populations at risk, creating a binary choice: state consent (unlikely when the state is the violator) or Security Council authorization (blockable by veto from allies of the perpetrating state). Suppression requirement is substantial (0.71) because the constraint's persistence depends on actively suppressing R2P norm-building, blocking humanitarian intervention authorizations, and rhetorically defending sovereignty against moral critiques of inaction. Theater ratio is moderate (0.41): some of the enforcement energy goes to genuine coordination (preventing imperialist intervention under humanitarian cover), but a growing share defends the sovereignty principle against humanitarian criticism — performing adherence to non-intervention while atrocities proceed unimpeded. The measurement series show rising extractiveness over the 79-year interval: as the founding problem (preventing humanitarian imperialism) receded and R2P advocacy grew, the constraint's function shifted from mutual protection (all states benefit from non-intervention) to asymmetric extraction (powerful states with repressive allies benefit; victimized populations lose). Theater ratio also rose as humanitarian criticism intensified and the constraint's defenders invested more energy in rhetorical maintenance. Suppression requirement rose steadily as the R2P challenge to the sovereignty-first reading became more organizationally coherent (Kofi Annan, doctrine development, regional precedents).
 *
 * PERSPECTIVAL GAP:
 *   The post-colonial and veto-power seats perceive the sovereignty-first reading as genuine coordination that protects them from imperialist intervention and preserves their independence. The atrocity-victim seats and humanitarian advocates perceive the same constraint as enforced extraction that shields perpetrators and denies protection. The engine computes this divergence from power, exit options, and beneficiary/victim structure: beneficiaries with exit to coalitional defense and powerful veto seats compute low or negative effective extraction; powerless victims with trapped exit compute high effective extraction. The structural asymmetry is irreducible.
 *
 * DIRECTIONALITY LOGIC:
 *   Post-colonial states and authoritarian regimes are structural beneficiaries (d near 0.0): the constraint shields them from unilateral intervention; they control its defense through diplomacy and veto power; their exit is mobile within coalitions of allies. Populations under atrocity are structural victims (d near 1.0): the constraint extracts from them by withholding protection; they have no exit; they carry no power over the constraint's maintenance. Security Council permanent members are beneficiaries (d ≈ 0.1): they control the Chapter VII gate and use it to shield allies; they have high power and exit mobility. Western powers are partial payers (d ≈ 0.6): they advocate for intervention but are blocked by the legal force of Article 2(7) and the veto gate; they have high power but constrained exit from the framework. No directionality overrides are warranted: the structural derivation captures the seats' true relationships to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereignty-first reading satisfies the tangled_rope gates: it has coordination function (prevents imperialist intervention), active enforcement (defense of Article 2(7), blocking R2P authorizations), beneficiaries (post-colonial and authoritarian states), and victims (populations under atrocity). It is not a snare masquerading as coordination (it has genuine coordination content) and not a piton (it is actively defended and extracts asymmetrically, not diffusely). The claim/metric independence is maintained: the reading is CLAIMED as tangled_rope by those who believe the coordination benefit outweighs the extraction cost, and the authored metrics describe substantially extractive, actively enforced operation. The engine's per-seat classification will diverge: agenda-setter and beneficiary seats will compute coordination-dominant types; victim seats will compute snare or extraction-dominant types. This divergence is the measurement the story exists to take.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_atrocity_prevention_foreclosure,
    'Does the sovereignty-first reading''s core premise (state sovereignty is foundational and intervention requires consent or Chapter VII) logically foreclose the R2P reading (sovereignty is conditional on protecting populations, atrocity triggers responsibility), or can both readings coexist as competing doctrines?',
    'Examine whether a single legal framework could hold both principles simultaneously without contradiction. If yes, both readings coexist; if no, one forecloses the other. The test: can a state be simultaneously held to have unquestionable sovereignty over its internal affairs AND be subject to international responsibility when its internal conduct produces atrocity?',
    'If foreclosure is true, the kernel admits only one reading and the other is logically defeated rather than merely disputed. If coexistence is true, both readings remain live options held by different parties. This affects how the constraint''s stability is interpreted: foreclosure suggests one reading will eventually win; coexistence suggests indefinite rivalry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_atrocity_prevention_foreclosure, conceptual, 'Logical compatibility of the two readings'' core premises within a single framework.').

omega_variable(
    authority_grounding_shift,
    'Has the authority grounding for the sovereignty-first reading shifted over the 1945-2024 interval from lineage (continuity with the Charter''s founding intention) to extraction (post-colonial and authoritarian states using it to shield themselves), and if so, does this shift affect the reading''s legitimacy?',
    'Historical analysis of who defends the sovereignty-first reading and why: (1) early period (1945-1965): widespread defense based on Charter interpretation and anti-imperialism; (2) middle period (1965-2005): increasing defense by states with documented human rights abuses; (3) late period (2005-2024): rhetorical dominance of sovereignty defense by authoritarian coalitions using it explicitly to shield against R2P. If the beneficiary coalition has become primarily extractive-motivated rather than defending a principle, the authority grounding has shifted.',
    'If authority grounding has shifted from lineage to extraction, the reading''s legitimacy is undermined: it appears less like a principled interpretation of the Charter and more like a rhetorical shield for abuse. This could support reclassification from tangled_rope (with coordination content) to snare (pure extraction). If authority grounding has held stable, the reading retains its legitimacy despite the high extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_shift, empirical, 'Whether the authority grounding for the sovereignty-first reading has shifted from principled interpretation to extractive defense.').

omega_variable(
    r2p_norm_emergence_as_competing_kernel_reading,
    'Is R2P (Responsibility to Protect) an emerging alternative reading of the same Charter kernel (Article 2(7) and Chapter VII), or is it a new doctrine that overrides rather than reinterprets the original kernel?',
    'Examine R2P proponents'' arguments: do they claim to reinterpret Article 2(7) and Chapter VII, or do they claim the Charter is obsolete and a new international norm supersedes it? If reinterpretation, R2P is a sibling reading of the same kernel. If supersession, R2P is a new commitment system, not a reading of the old one.',
    'If R2P is a sibling reading, it competes with the sovereignty-first reading within the framework of the UN Charter. If R2P is a new doctrine, the kernel itself (Article 2(7) / Chapter VII) is being replaced by a new authority structure. This affects whether the constraint story should be a single kernel with multiple readings or a narrative of one kernel being superseded by another.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(r2p_norm_emergence_as_competing_kernel_reading, conceptual, 'Whether R2P is an alternative reading of the Charter or a new doctrine superseding it.').

omega_variable(
    beneficiary_coalition_stability,
    'Is the beneficiary coalition defending the sovereignty-first reading (post-colonial states + authoritarian regimes + veto-power holders) internally stable, or are there structural tensions within it that could cause it to splinter?',
    'Monitor voting patterns in UN forums, coalition formation on specific interventions, and public statements. If post-colonial democracies begin breaking ranks and supporting R2P interventions, or if veto-power holders diverge on humanitarian cases, the coalition has weakened. If the coalition holds solid despite R2P advocacy, it is stable.',
    'If the coalition splinters, the constraint''s enforcement will weaken and extraction will decline. If it holds stable, extraction will persist and may rise as defenders invest more energy in suppression. A splintered beneficiary coalition suggests the constraint may transition from tangled_rope to piton (maintained by inertia rather than active defense).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_coalition_stability, empirical, 'Whether the post-colonial/authoritarian/veto-power beneficiary coalition defending sovereignty-first will remain cohesive or fragment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__sovereignty_first_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(arti_tr_t1965, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1965, 0.28).
narrative_ontology:measurement(arti_tr_t1985, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 1985, 0.32).
narrative_ontology:measurement(arti_tr_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(arti_tr_t2015, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(arti_tr_t2024, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1945, 0.55).
narrative_ontology:measurement(arti_be_t1965, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1965, 0.62).
narrative_ontology:measurement(arti_be_t1985, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 1985, 0.68).
narrative_ontology:measurement(arti_be_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2005, 0.74).
narrative_ontology:measurement(arti_be_t2015, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2015, 0.76).
narrative_ontology:measurement(arti_be_t2024, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1945, 0.58).
narrative_ontology:measurement(arti_su_t1965, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1965, 0.63).
narrative_ontology:measurement(arti_su_t1985, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 1985, 0.66).
narrative_ontology:measurement(arti_su_t2005, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2005, 0.69).
narrative_ontology:measurement(arti_su_t2015, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(arti_su_t2024, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__sovereignty_first_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.22).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension__r2p_reading).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, security_council_veto_power).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, humanitarian_intervention_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the article_2_7_chapter_vii_tension kernel. The sibling reading (article_2_7_chapter_vii_tension__r2p_reading) interprets the same Charter articles differently: sovereignty as conditional on protecting populations; atrocity as a Chapter VII threat. The two readings are structurally incompatible — a single state cannot simultaneously have unquestionable sovereignty over its internal affairs and be subject to international responsibility when its internal conduct produces systematic atrocity. The kernel contest is live and unresolved in international law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
