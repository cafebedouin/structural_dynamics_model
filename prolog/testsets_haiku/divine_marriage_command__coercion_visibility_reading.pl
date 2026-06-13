% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__coercion_visibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__coercion_visibility_reading, []).

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
 *   constraint_id: divine_marriage_command__coercion_visibility_reading
 *   human_readable: Divine Marriage Command (Coercion Visibility Reading)
 *   domain: religious_authority/political_theology
 *
 * SUMMARY:
 *   The institutional leadership issues the Manifesto suspending the practice
 *   of polygamy while maintaining theological ambiguity about whether this
 *   represents prudential response to federal coercion, new revelation, or
 *   doctrinal rescission. This reading—the coercion_visibility
 *   reading—explicitly acknowledges federal pressure as a ground for the
 *   doctrinal shift and treats the Manifesto as an institutional survival
 *   mechanism rather than as a pure theological discovery. The constraint
 *   that emerges from this reading is a tangled_rope: it coordinates
 *   institutional survival (the real problem the Manifesto solves: keeping
 *   the institution legal and intact) while simultaneously extracting from
 *   practitioners and doctrinal fidelity adherents, who must abandon a
 *   practice they believe remains divinely commanded. The admission of
 *   coercive grounds—that exogenous federal pressure is a valid input to
 *   doctrinal change—creates a potential legitimacy crisis: if coercion can
 *   shape doctrine, on what grounds does the authority structure claim divine
 *   authority rather than political expedience?
 *
 * KEY AGENTS:
 *   - institutional_leadership: Sets/enforces the constraint; collects institutional survival benefit; maintains authority over doctrine while acknowledging coercive pressure
 *   - plural_practitioners: Bears the cost of practice abandonment; identity-locked to the institution and the prior doctrine; no exit without severe cost
 *   - doctrinal_fidelity_adherents: Maintains private belief in the prior command's validity while complying publicly; suppression is both structural (institutional enforcement) and internalized (identity-fusion, shame cycles)
 *   - federal_coercive_apparatus: Excluded from theological conversation but determines the constraint through enforcement capacity; their law is the exogenous pressure that drives the doctrinal shift
 *   - doctrinal_theorists: Analytical seat; measures the coherence of the authority structure and the logical status of the reading relative to siblings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, 0.68).
domain_priors:suppression_score(divine_marriage_command__coercion_visibility_reading, 0.71).
domain_priors:theater_ratio(divine_marriage_command__coercion_visibility_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__coercion_visibility_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__coercion_visibility_reading, "Divine Marriage Command (Coercion Visibility Reading)").
narrative_ontology:topic_domain(divine_marriage_command__coercion_visibility_reading, "religious_authority/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__coercion_visibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__coercion_visibility_reading, '325ab62b-5a98-4c70-ae23-e0f5338f2b9e').
narrative_ontology:cs_kernel_codification('325ab62b-5a98-4c70-ae23-e0f5338f2b9e', fixed_text).
narrative_ontology:cs_authority_grounding('325ab62b-5a98-4c70-ae23-e0f5338f2b9e', extraction).
narrative_ontology:cs_interpretation_layer_present('325ab62b-5a98-4c70-ae23-e0f5338f2b9e').
narrative_ontology:cs_reading_relation('325ab62b-5a98-4c70-ae23-e0f5338f2b9e', divine_marriage_command__continuationist_reading, coexists_with).
narrative_ontology:cs_reading_relation('325ab62b-5a98-4c70-ae23-e0f5338f2b9e', divine_marriage_command__substitutionist_reading, influences).
narrative_ontology:cs_axiom('325ab62b-5a98-4c70-ae23-e0f5338f2b9e', foundational, coercion_as_valid_doctrinal_input).
narrative_ontology:cs_axiom_status(coercion_as_valid_doctrinal_input, holdable).
narrative_ontology:cs_axiom_grounding('325ab62b-5a98-4c70-ae23-e0f5338f2b9e', coercion_as_valid_doctrinal_input, conventional).
narrative_ontology:cs_axiom('325ab62b-5a98-4c70-ae23-e0f5338f2b9e', foundational, institutional_survival_legitimates_doctrinal_shift).
narrative_ontology:cs_axiom_status(institutional_survival_legitimates_doctrinal_shift, holdable).
narrative_ontology:cs_axiom_grounding('325ab62b-5a98-4c70-ae23-e0f5338f2b9e', institutional_survival_legitimates_doctrinal_shift, instrumental).
narrative_ontology:cs_reference_frame('325ab62b-5a98-4c70-ae23-e0f5338f2b9e', original_polygamy_doctrine_authority).
narrative_ontology:cs_drift_state('325ab62b-5a98-4c70-ae23-e0f5338f2b9e', post_federal_coercion_period, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('325ab62b-5a98-4c70-ae23-e0f5338f2b9e', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__coercion_visibility_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, doctrinal_authority_preservation).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, plural_practitioners).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, doctrinal_fidelity_adherents).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__coercion_visibility_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(divine_marriage_command__coercion_visibility_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__coercion_visibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__coercion_visibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at end) is elevated but not maximal because a real coordination function is present: the Manifesto does solve the institutional survival problem and maintains legal standing. Suppression (0.71) is high because the constraint's persistence depends on active enforcement: practitioners must abandon the practice, adherents must suppress private belief, and the authority structure must suppress acknowledgment of the coercive basis for its legitimacy. Theater_ratio (0.58 at end) plateaus at moderate-high: initial rise (0.35→0.58) reflects an increasing gap between the stated coordination function (doctrinal discovery) and the actual function (institutional survival under federal pressure); the plateau suggests the theater has stabilized—the Manifesto's framing is now routine institutional performance. The measurements show extractiveness and suppression both rising steeply in the first 15 time units (the immediate post-Manifesto period, when enforcement is most active and practitioners most resistant) and then plateauing by t=25, suggesting the constraint has achieved stable institutional embedding. Accessibility_collapse (0.62) is moderate: practitioners have not lost all alternatives (schism, migration to non-institutional polygamy communities, exit to other faiths) but those alternatives carry such high identity cost that they are effectively collapsed for those identity-locked to the institution. Resistance (0.74) is high and persistent: doctrinal adherents continue to resist through private belief and theological argument, even under suppression.
 *
 * PERSPECTIVAL GAP:
 *   The institutional_leadership seat computes the constraint very differently from the plural_practitioners and doctrinal_fidelity_adherents seats. From the leadership seat: this is a coordinated solution to an external threat; the Manifesto represents responsible institutional stewardship under duress; the coercive grounds are regrettable but non-delegitimizing (coercion from outside does not negate divine authority). From the adherents' seats: this is enforced contradiction; the leadership has abandoned the doctrine under pressure and is using authority to suppress dissent; the admission of coercive grounds exposes the authority structure as contingent on political expedience rather than revelation. The engine computes these divergences from the structural data—beneficiaries collide with victims on the meaning of the constraint itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional_leadership sits at the beneficiary end (d~0.15) because the constraint subsidizes their institutional survival—they collect the primary benefit (legal standing, continued authority, averted dissolution risk) and control the constraint's operation. They have moderate power (institutional, able to negotiate) and moderate exit options (constrained but not trapped—they can negotiate with federal authorities or adjust the doctrine's framing). The plural_practitioners sit at the target end (d~0.85) because they are fully extracted from: they must abandon a practice they believe divinely commanded, they have identity-lock exit (the cost of leaving exceeds the cost of staying and complying), and they have no seat in the negotiation. The doctrinal_fidelity_adherents sit at a second target end (d~0.80) because they experience enforced contradiction: they must publicly comply with a doctrine they privately disbelieve, they are identity-locked to the institutional tradition, and they bear the suppression cost (both structural enforcement and internalized guilt/shame). The federal_coercive_apparatus is excluded from the stakeholder table but appears implicitly as the exogenous pressure that drives the directionality: their enforcement capacity determines whether the constraint persists. No override is needed; the derivation chain (beneficiaries→low d, victims→high d, identity-lock→amplified target d) produces accurate directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy confusion by explicitly acknowledging that the founding problem (federal coercion) remains live and that institutional survival—not doctrinal discovery—is the actual coordination function the Manifesto solves. The theater_ratio plateau at 0.58 (rather than rising toward 1.0) indicates that the constraint is not pure performance: the Manifesto genuinely changes institutional practice (monogamy is now legally and institutionally enforced) and genuinely solves the federal coercion problem. The constraint persists not from institutional inertia but from ongoing enforcement (suppression_requirement stays at 0.71) and genuine institutional benefit (institutional_leadership remains beneficiary). What the theater_ratio captures is the increasing theatrical quality of the authority structure's claim to non-contingency: the more the Manifesto is framed as divine discovery, the more performative that framing becomes, given the acknowledged coercive grounds. This is not mandatrophy (a function that has atrophied and now persists only through theater); it is a constraint that coordinates real institutional survival while simultaneously extracting from practitioners through suppression and doctrinal incoherence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_validity_as_doctrine,
    'Does this reading''s core admission—that federal coercion is a valid, acknowledged input to doctrinal change—constitute a legitimacy crisis for the theological authority structure, or can coercion be integrated as part of divine pedagogy?',
    'Examine theological commentary and official exegesis: whether institutional interpreters treat coercion as compatible with divine authority or as a gap in authority''s claim to non-contingency.',
    'If coercion is deemed incompatible with divine authority, this reading destabilizes the authority structure itself and shifts toward a substitutionist reading (pure new revelation) or toward schism. If coercion can be integrated, the constraint persists as a stable tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_validity_as_doctrine, conceptual, 'Whether acknowledged coercion undermines theological legitimacy or is theologically absorbable.').

omega_variable(
    private_belief_suppression_mechanism,
    'Is the doctrinal fidelity adherents'' suppression structural (institutional enforcement preventing practice and speech) or internalized (cognitive patterns of guilt, shame, identity-fusion preventing belief-expression even absent enforcement)?',
    'Post-enforcement relaxation trajectory: if suppression persists after institutional enforcement capacity erodes, infer internalization; if it dissolves, infer structural mediation.',
    'If internalized, the constraint''s effective suppression is higher than the measured scalar suggests, and adherents carry the suppression with them through exits. If structural, remedies focused on enforcement removal would be more effective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(private_belief_suppression_mechanism, empirical, 'Structural vs. internalized suppression of doctrinal fidelity belief.').

omega_variable(
    reading_temporal_stability,
    'Can this reading (coercion_visibility) remain theologically dominant indefinitely, or does the acknowledgment of coercive grounds create pressure toward convergence on either the continuationist (coercion is prudential suspension only) or substitutionist (coercion is path to new revelation) reading?',
    'Track theological commentary and institutional framing over generational time scales: does the coercion frame stabilize, slide toward one sibling, or splinter into schism?',
    'If unstable, the constraint is temporary (scaffold-like) and the measured theater_ratio plateau at 0.58 masks an underlying drift toward one of the sibling readings. If stable, the tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_temporal_stability, empirical, 'Temporal stability of the coercion_visibility reading versus pressure toward sibling readings.').

omega_variable(
    institutional_legitimacy_crisis_timing,
    'At what point does the acknowledged gap between original command (polygamy) and enforced practice (monogamy) trigger a formal schism or a forced theological resolution?',
    'Monitor institutional coherence statements and splinter movements: the crisis fires when institutional authority can no longer contain the contradiction between coercion and divine command.',
    'A crisis would shift the constraint from tangled_rope (active enforcement holding extraction stable) to snare (pure extraction with weakening authority) or toward a substitutionist reading (new revelation resolving the gap).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_legitimacy_crisis_timing, empirical, 'Institutional legitimacy crisis point triggered by sustained acknowledgment of coercion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__coercion_visibility_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__coercion_visibility_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(divi_tr_t5, divine_marriage_command__coercion_visibility_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(divi_tr_t10, divine_marriage_command__coercion_visibility_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(divi_tr_t15, divine_marriage_command__coercion_visibility_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement(divi_tr_t20, divine_marriage_command__coercion_visibility_reading, theater_ratio, 20, 0.56).
narrative_ontology:measurement(divi_tr_t25, divine_marriage_command__coercion_visibility_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement(divi_tr_t30, divine_marriage_command__coercion_visibility_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement(divi_tr_t40, divine_marriage_command__coercion_visibility_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(divi_be_t5, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(divi_be_t10, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(divi_be_t15, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(divi_be_t20, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(divi_be_t25, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(divi_be_t30, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(divi_be_t40, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(divi_su_t5, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(divi_su_t10, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(divi_su_t15, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(divi_su_t20, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(divi_su_t25, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(divi_su_t30, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(divi_su_t40, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__coercion_visibility_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__coercion_visibility_reading, 0.12).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__substitutionist_reading).

% DUAL FORMULATION NOTE:
% The divine_marriage_command kernel decomposes into three structurally distinct constraint readings, each with different ε values and beneficiary/victim structures. This constraint (coercion_visibility_reading) acknowledges federal coercion as grounds for doctrinal shift and treats institutional survival as the primary coordination function. The continuationist_reading maintains the original command's validity and reads the Manifesto as prudential suspension only; it has higher ε (pure extraction, no coordination function). The substitutionist_reading treats the Manifesto as new revelation and monogamy as doctrinally required; it has lower theater_ratio (no gap between framing and function). All three readings compete for institutional legitimacy and theological dominance; they affect one another through the authority structure's need to adjudicate the kernel's meaning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
