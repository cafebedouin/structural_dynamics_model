% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__positivist_reading, []).

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
 *   constraint_id: us_constitution_1787__positivist_reading
 *   human_readable: US Constitution 1787 — Positivist Reading (Text + Democratic Amendment)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The positivist reading of the US Constitution holds that constitutional
 *   meaning is what the text says, plus any formal amendments. Judicial
 *   interpretation is constrained to the text itself; judges cannot
 *   reinterpret meaning based on contemporary values or framers' private
 *   intent. This reading positions the amendment process as the legitimate
 *   mechanism for changing constitutional meaning, not judicial
 *   reinterpretation. It is one of three competing readings of the same
 *   constitutional kernel — originalist (meaning fixed at ratification via
 *   framers' intent), living (meaning evolves with society), and positivist
 *   (meaning is the text plus amendments). The positivist reading emerged in
 *   late-20th-century jurisprudence as an attempt to provide a stable,
 *   rule-bound interpretive anchor that neither judges nor social movements
 *   can easily shift. It coordinates judicial behavior around a fixed text
 *   while preserving the electorate's formal power to amend. The extraction
 *   measured here is not money or resources, but interpretive authority: the
 *   reading reallocates power from judges (who under the living reading can
 *   evolve meaning) to the electorate (who under the positivist reading is
 *   the only source of new meaning beyond the text). This is coordination —
 *   all actors reference the same text — but it comes with a cost for those
 *   who believe constitutional rights should respond to contemporary
 *   standards without waiting for amendment.
 *
 * KEY AGENTS:
 *   - judiciary: the institutional actor whose interpretive authority is constrained by this reading
 *   - electorate (via amendment process): the institutional actor whose formal power is heightened relative to living-reading frame
 *   - amendment advocates: organized movements that benefit from the amendment process being the sole mechanism for constitutional change
 *   - living-constitution advocates: scholars and judges who lose interpretive flexibility under this reading
 *   - originalist judges: institutional allies against the living reading, but methodological competitors (they ground meaning in intent, not text)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__positivist_reading, 0.42).
domain_priors:suppression_score(us_constitution_1787__positivist_reading, 0.28).
domain_priors:theater_ratio(us_constitution_1787__positivist_reading, 0.19).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, theater_ratio, 0.19).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__positivist_reading, rope).
narrative_ontology:human_readable(us_constitution_1787__positivist_reading, "US Constitution 1787 — Positivist Reading (Text + Democratic Amendment)").
narrative_ontology:topic_domain(us_constitution_1787__positivist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__positivist_reading, 'fe552574-7c72-4960-b10c-73bed1a59e5b').
narrative_ontology:cs_kernel_codification('fe552574-7c72-4960-b10c-73bed1a59e5b', fixed_text).
narrative_ontology:cs_authority_grounding('fe552574-7c72-4960-b10c-73bed1a59e5b', lineage).
narrative_ontology:cs_interpretation_layer_present('fe552574-7c72-4960-b10c-73bed1a59e5b').
narrative_ontology:cs_reading_relation('fe552574-7c72-4960-b10c-73bed1a59e5b', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe552574-7c72-4960-b10c-73bed1a59e5b', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_axiom('fe552574-7c72-4960-b10c-73bed1a59e5b', foundational, constitutional_meaning_text_only).
narrative_ontology:cs_axiom_status(constitutional_meaning_text_only, holdable).
narrative_ontology:cs_axiom_grounding('fe552574-7c72-4960-b10c-73bed1a59e5b', constitutional_meaning_text_only, conventional).
narrative_ontology:cs_axiom('fe552574-7c72-4960-b10c-73bed1a59e5b', foundational, amendment_exclusive_change_mechanism).
narrative_ontology:cs_axiom_status(amendment_exclusive_change_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('fe552574-7c72-4960-b10c-73bed1a59e5b', amendment_exclusive_change_mechanism, conventional).
narrative_ontology:cs_reference_frame('fe552574-7c72-4960-b10c-73bed1a59e5b', fixed_textual_meaning_with_formal_amendment).
narrative_ontology:cs_drift_state('fe552574-7c72-4960-b10c-73bed1a59e5b', contemporary_jurisprudential_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fe552574-7c72-4960-b10c-73bed1a59e5b', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__positivist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, textualist_judicial_tradition).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, amendment_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, electorate_via_amendment_power).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__positivist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(us_constitution_1787__positivist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__positivist_reading_tests).
:- end_tests(us_constitution_1787__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the reading does redistribute interpretive authority — judges lose discretion, the electorate gains the formal amendment path as the exclusive mechanism for constitutional change. This is a real redistribution, not a fake one. Suppression is low (0.28) because the text itself is publicly available and the amendment process, while difficult, is not secret or coercive — it is a formal democratic procedure. Theater is low-moderate (0.19) because the positivist reading is defended on principled grounds (rule of law, fixed meaning, popular sovereignty), not theatrically maintained. The constraint's core claim — that judges should interpret text, not rewrite it — is genuinely believed by textualist judges and scholars, not performed. The measurements show slight increases from t0 to t30, plateauing near t50, reflecting the gradual entrenchment of textualism in judicial appointments and legal academia over the interval, with a minor decay near t50 as living-reading pressure mounted in response.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and the electorate experience this constraint differently. From the judicial seat, the constraint is a rule that binds interpretive methodology — judges accept the constraint as legitimate because it embeds the rule of law principle (meaning comes from law, not from judges' values). From the electorate's seat, the constraint is a tool that gives them exclusive formal power to change meaning, but it also burdens them with the need to achieve supermajority consensus for any constitutional change. Living-constitution advocates see the constraint as illegitimately rigid; originalists see it as incomplete (it should include framers' intent). The engine should compute different per-seat types from this structural gap.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is the payer seat: this reading constrains their interpretive power. The electorate (especially amendment advocates and democratically organized movements) are the beneficiaries: their formal constitutional power is heightened. The originalist tradition is a competitor payer: positivism displaces originalism as the authoritative interpretive framework, even though both reject evolutionary interpretation. The living-constitution tradition is the primary payer: they lose the institutional mechanism (courts as evolutionary interpreters) that the living reading provides. Directionality: the judiciary sits near d=0.7 (constrained, loses interpretive discretion); the electorate sits near d=0.2 (benefits from formal amendment power, bears the cost of rigidity); amendment advocates sit near d=0.1 (primary beneficiaries); living advocates sit near d=0.9 (targets, lose their mechanism).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (when a constraint's founding mandate has outlived its function) is contested for this reading. The founding problem was to anchor judicial authority in something stable and publicly knowable — the text itself — so that meaning does not shift with judges' personal values. That founding problem remains live: courts do continue to interpret the text, and textualism vs. living-constitutionalism remains an active methodological dispute. However, critics argue that the founding mandate (restraining judicial overreach) has been achieved to the point where the cost of rigidity now exceeds the benefit — that once the Court has been populated with textualist justices, the constraint serves mainly to prevent legislative amendment and prevent courts from responding to genuine injustice revealed after ratification. This is a genuine dispute about whether the constraint still solves its founding problem or has become an obstacle. No omega is needed; the debate is visible in the six_questions answers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    text_indeterminacy,
    'When the constitutional text is genuinely ambiguous or incomplete (e.g., ''due process,'' ''cruel and unusual punishment,'' ''regulate interstate commerce''), how much does a positivist judge have to read into the text to settle real cases?',
    'Sustained examination of positivist judges'' actual opinions to measure how much textual supplement (canons of interpretation, structural reasoning, historical usage of words) is applied when the text itself is indeterminate.',
    'If positivist judges regularly supplement the text with extensive interpretive machinery to settle real cases, the reading''s claim to be ''text-bound'' is partially incoherent — judges would be disguising evolutionary interpretation as textual interpretation. If judges leave many questions unsettled (declaring the text silent and deferring to the legislature), the reading is coherent but produces governance gaps.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(text_indeterminacy, empirical, 'Whether positivist methodology can actually stay within the text for indeterminate clauses or requires hidden interpretive moves.').

omega_variable(
    amendment_pathway_accessibility,
    'Is the amendment process a genuinely accessible mechanism for constitutional change, or is the supermajority threshold so high that it functions as a veto on change for any coalition without consensus?',
    'Historical analysis of amendment success rates and comparative institutional study of democratic constitutions with different amendment thresholds. Track which constitutional changes (civil rights, voting rights, etc.) succeeded through amendment vs. which were achieved through living-reading interpretation.',
    'If the amendment threshold is too high to be practically accessible, the positivist reading concentrates constitutional power in whichever faction controls the judiciary (by appointment) for that era — paradoxically defeating its own goal of democratizing constitutional change. If amendment is accessible, the reading succeeds in shifting power to the electorate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_pathway_accessibility, empirical, 'Whether amendment is a viable mechanism for constitutional change under supermajority requirement.').

omega_variable(
    originalism_vs_positivism_differentiation,
    'Is the methodological difference between originalism and positivism substantive enough to produce different outcomes on real cases, or do both frameworks arrive at similar readings of the text?',
    'Systematic comparison of originalist and positivist judicial opinions on the same constitutional issues (e.g., Second Amendment, commerce clause, due process) to measure divergence in outcomes.',
    'If originalism and positivism produce meaningfully different results, they are distinct constraints with different structural implications. If they arrive at the same place most of the time, they are functionally equivalent and the constraint''s true competitor is the living reading, not originalism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_vs_positivism_differentiation, empirical, 'Whether positivism and originalism are substantively different methodologies or pragmatically equivalent.').

omega_variable(
    living_reading_foreclosure_strength,
    'Does the positivist reading truly foreclose the living reading, or can a judge hold both — accepting that the text is binding while also believing that constitutional meaning should evolve with society?',
    'Examine whether any prominent legal figures or judicial opinions articulate a hybrid position (text-bound for baseline constraints, but with room for evolutionary interpretation within the textual boundaries).',
    'If living and positivist readings are logically separable (both can be held by the same person in different contexts), the foreclosure relation should be downgraded to ''influences.'' If they are genuinely contradictory, the foreclosure relation is correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(living_reading_foreclosure_strength, conceptual, 'Whether positivism and living-constitutionalism are logically incompatible or pragmatically distinguishable in some cases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__positivist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_1787__positivist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_1787__positivist_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_1787__positivist_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_1787__positivist_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_1787__positivist_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_1787__positivist_reading, theater_ratio, 50, 0.19).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_1787__positivist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(us_c_be_t10, us_constitution_1787__positivist_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(us_c_be_t20, us_constitution_1787__positivist_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(us_c_be_t30, us_constitution_1787__positivist_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(us_c_be_t40, us_constitution_1787__positivist_reading, base_extractiveness, 40, 0.43).
narrative_ontology:measurement(us_c_be_t50, us_constitution_1787__positivist_reading, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_1787__positivist_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(us_c_su_t10, us_constitution_1787__positivist_reading, suppression_requirement, 10, 0.24).
narrative_ontology:measurement(us_c_su_t20, us_constitution_1787__positivist_reading, suppression_requirement, 20, 0.26).
narrative_ontology:measurement(us_c_su_t30, us_constitution_1787__positivist_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement(us_c_su_t40, us_constitution_1787__positivist_reading, suppression_requirement, 40, 0.29).
narrative_ontology:measurement(us_c_su_t50, us_constitution_1787__positivist_reading, suppression_requirement, 50, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__positivist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__living_reading).

% DUAL FORMULATION NOTE:
% The positivist reading is one of three structurally distinct constraints derived from the contested kernel us_constitution_1787. The originalist reading grounds meaning in framers' intent (different ε, different beneficiary structure, different victims). The living reading grounds meaning in contemporary evolution (lowest ε for constraint, highest flexibility, different competitive dynamics). All three share the artifact (the 1787 text) but assign different meaning-rules to it, producing different constraint types per the engine's per-seat computation. This story models the positivist reading; the others are separate stories linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
