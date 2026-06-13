% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__artifact_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__artifact_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: gelassenheit_separation__artifact_reading
 *   human_readable: Artifact-Based Separation Constraint (Gelassenheit Artifact Reading)
 *   domain: religious_studies/technology_governance
 *
 * SUMMARY:
 *   The artifact-reading of Gelassenheit separation defines separation as
 *   visible distinction from English society, enforced by prohibiting any
 *   technology whose physical form resembles English secular infrastructure,
 *   regardless of actual function or isolation. A solar panel is forbidden
 *   not because it uses worldly energy but because it looks like a worldly
 *   artifact; modern insulation is forbidden not because it connects to
 *   worldly systems but because synthetic materials resemble what English
 *   houses use; water pumping is forbidden not for functional entanglement
 *   but for form-resemblance. This reading prioritizes visible markers over
 *   practical consequence or functional isolation. The constraint operates as
 *   highly extractive and suppressive: it transfers interpretive authority to
 *   the church structure, imposes practical hardship on off-grid households
 *   and those with acute needs, and elevates enforcement (determining
 *   form-resemblance, monitoring for violations, disciplining members) above
 *   community consent. The measurement series show extraction and
 *   theater-ratio rising over the interval as the doctrine becomes more
 *   stringently applied, while suppression remains consistently high.
 *
 * KEY AGENTS:
 *   - Church authority structure: sets and enforces the artifact-form standard; derives power from monopolizing interpretation
 *   - Community members with practical needs: live under the constraint; identity-locked to the community; bear direct costs of artifact prohibition
 *   - Off-grid households: structurally most constrained; cannot access the practical technologies they depend on because those technologies' forms are prohibited
 *   - Principle-reading and consequence-reading adherents: contest the artifact-reading but are excluded from its frame; represent internal alternatives within Gelassenheit theology
 *   - Younger generation pragmatists: question whether form-distinction serves the actual founding commitment; suppressed by authority but growing in voice
 *   - English secular society: the reference contrast; separation is defined as visible distinction FROM English norms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, 0.82).
domain_priors:suppression_score(gelassenheit_separation__artifact_reading, 0.91).
domain_priors:theater_ratio(gelassenheit_separation__artifact_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(gelassenheit_separation__artifact_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__artifact_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__artifact_reading, "Artifact-Based Separation Constraint (Gelassenheit Artifact Reading)").
narrative_ontology:topic_domain(gelassenheit_separation__artifact_reading, "religious_studies/technology_governance").

domain_priors:requires_active_enforcement(gelassenheit_separation__artifact_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__artifact_reading, '444551b9-c431-4712-af5b-8a0c95eb8498').
narrative_ontology:cs_kernel_codification('444551b9-c431-4712-af5b-8a0c95eb8498', distributed).
narrative_ontology:cs_authority_grounding('444551b9-c431-4712-af5b-8a0c95eb8498', extraction).
narrative_ontology:cs_interpretation_layer_present('444551b9-c431-4712-af5b-8a0c95eb8498').
narrative_ontology:cs_reading_relation('444551b9-c431-4712-af5b-8a0c95eb8498', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_reading_relation('444551b9-c431-4712-af5b-8a0c95eb8498', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('444551b9-c431-4712-af5b-8a0c95eb8498', foundational, form_as_moral_property).
narrative_ontology:cs_axiom_status(form_as_moral_property, holdable).
narrative_ontology:cs_axiom_grounding('444551b9-c431-4712-af5b-8a0c95eb8498', form_as_moral_property, deontological).
narrative_ontology:cs_axiom('444551b9-c431-4712-af5b-8a0c95eb8498', secondary, artifact_resemblance_as_worldly_entanglement).
narrative_ontology:cs_axiom_status(artifact_resemblance_as_worldly_entanglement, holdable).
narrative_ontology:cs_axiom_grounding('444551b9-c431-4712-af5b-8a0c95eb8498', artifact_resemblance_as_worldly_entanglement, conventional).
narrative_ontology:cs_reference_frame('444551b9-c431-4712-af5b-8a0c95eb8498', visible_aesthetic_separation_from_english_norms).
narrative_ontology:cs_drift_state('444551b9-c431-4712-af5b-8a0c95eb8498', contemporary_pluralistic_context, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('444551b9-c431-4712-af5b-8a0c95eb8498', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__artifact_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__artifact_reading, church_authority_structure).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, community_members_with_practical_needs).
narrative_ontology:constraint_victim(gelassenheit_separation__artifact_reading, off_grid_households).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__artifact_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gelassenheit_separation__artifact_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__artifact_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gelassenheit_separation__artifact_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gelassenheit_separation__artifact_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.82) reflects the high transfer of practical autonomy to the authority structure and the substantial cost borne by members (labor, energy access, agricultural efficiency, weather protection). Suppression is even higher (0.91) because the constraint depends on active enforcement: form-resemblance is not objectively verifiable (a modern fabric might resemble a worldly equivalent in degree; a solar panel's similarity to English infrastructure is interpretive); the authority must continuously adjudicate borderline cases, discipline violations, and suppress alternative readings. Theater-ratio rises to 0.58 over the interval because increasing amounts of enforcement activity defends the artifact-form distinction itself rather than the underlying separation goal (younger members would achieve separation via consequence or principle readings without the enforcement burden; the theater is the maintenance of the distinction doctrine, not the separation it claims to serve). Accessibility collapse is high (0.88) because once the artifact-reading is the official doctrine, alternatives (practical technology, functional-isolation reasoning, consequence-based evaluation) become cognitively and socially unavailable to members without breaking with the community. Resistance is moderate (0.74) because the younger generation and practitioners with technical knowledge mount real objections, but those objections are suppressed through authority rather than resolved. The suppression_requirement measurements track rising enforcement intensity: as pragmatic pressure increases, the authority must work harder to maintain the distinction.
 *
 * PERSPECTIVAL GAP:
 *   From the church authority's seat, the artifact-reading is a straightforward application of separation doctrine: visible distinctness is the proper form of separation, and form-based prohibition is a clear, enforceable standard. From the community members' seat (especially off-grid and practical-needs seats), the constraint is experienced as arbitrary extraction: a solar panel functions identically whether it's visible or hidden, whether off-grid or grid-connected, yet visibility is the only thing that matters under this reading. The principle-reading seat views this as overextension: functional isolation is the actual safeguard against worldly entanglement, and form-resemblance is a proxy that imports worldly concerns (what English society looks like) into a separation that should be about functional autonomy. The consequence-reading seat views this as category error: separation should be measured by community effect (visiting, mutual aid, rootedness), and artifact form is a performative marker that substitutes for actual community preservation. These perspectival gaps arise directly from the different axioms the readings hold (form-as-moral-property under artifact-reading vs. function-as-moral-property under principle-reading vs. consequence-as-moral-property under consequence-reading). The engine computes per-seat types from the structural data; the artifact-reading's claim-type and the computed types across seats will likely diverge, which is exactly the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The church authority structure is the structural beneficiary (controls interpretation, monopolizes adjudication, derives authority from the doctrine): d near 0.0–0.2. Community members with practical needs are targets (bear costs, constrained by identity-lock): d near 0.75–0.85. Off-grid households are fully targeted (trapped exit, highest burden): d near 0.95–1.0. Principle-reading and consequence-reading adherents have mixed directionality: they are payers (lose authority, treated as doctrinal error) but partially beneficiaries (remain in community, maintain spiritual home): d near 0.5–0.65. Younger pragmatists are similar but with rising d as suppression intensifies. English secular society has zero directionality within the constraint (it is the reference frame, not a seat).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and real (assimilation risk persists), but the artifact-reading increasingly appears to be solving a different problem (maintaining a visible aesthetic of separation) than the founding problem itself (resisting cultural absorption). This is the classic mandatrophy pattern: a constraint built to solve one problem persists in solving a proxy problem when the original problem changes or shifts. The consequence-reading and principle-reading both solve the founding problem (preserve separation) without the theater cost of artifact prohibition. The measurement of rising theater-ratio and stable-then-rising suppression_requirement is exactly where mandatrophy appears: the constraint must work harder to maintain the distinction itself, not because separation is harder to achieve, but because the real separation is being achieved through other means (consequence-preservation, principle-isolation) and the artifact-form distinction has become decorative. A mandatrophy declaration would state: the founding problem of assimilation resistance remains live, but the artifact-reading has become a theater of separation rather than its functional mechanism; the consequence-reading and principle-reading preserve separation at lower cost and higher consensus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    form_vs_function_boundary,
    'Is form-resemblance a meaningful moral property, or is it a proxy for function and worldly entanglement that mistakes appearance for substance?',
    'Examine cases where form and function diverge sharply (e.g., a solar panel in a cave where it is invisible, a synthetic fabric used in isolation). If the artifact-reading permits these in practice while prohibiting visible equivalents, the reading treats form as primary; if actual usage patterns show community members treating function as decisive when form is hidden, the reading''s moral purchase is weaker than claimed.',
    'If form is a genuine moral property (as the artifact-reading claims), then the reading''s logic is sound and the constraint is a valid application of separation doctrine. If form is a proxy that breaks down under scrutiny, then the reading is performative theater: it maintains visible markers of separation without preserving actual separation from worldly systems. This shifts classification from tangled_rope (genuine coordination + asymmetric extraction) toward snare (pure extraction disguised as coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(form_vs_function_boundary, empirical, 'Whether form-resemblance is a primary moral property or a performative proxy.').

omega_variable(
    suppression_internalization,
    'Is the measured suppression structural (enforced by authority discipline) or internalized (community members have adopted the form-based standard as intrinsically correct)?',
    'Post-exit tracking: interview or observe members who have left the community. If suppression persists after external enforcement is removed (members continue to avoid artifact-forms even when geographically and socially isolated from the authority), the suppression is partially internalized. If suppression collapses immediately upon exit, it is primarily structural.',
    'Internalized suppression increases effective suppression beyond the measured 0.91 because the target carries the constraint with them after leaving. Structural suppression alone (0.91) is already high, but internalization would suggest deeper identity fusion and a snare-like operation. Mixed internalization/structural would indicate the artifact-reading has created cognitive patterns that persist independent of enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression is externally enforced or internalized as identity.').

omega_variable(
    founding_problem_shift,
    'Has the founding problem (assimilation risk) genuinely declined, or has it merely transformed into different channels, making the artifact-reading less necessary but more costly?',
    'Historical analysis of cultural assimilation pressures: compare the early period (when the artifact-reading was developed) to the contemporary period. Assess whether English secular culture''s social coercion toward assimilation remains as strong. If assimilation risk has declined (pluralism, reduced social pressure, increased institutional recognition of minority communities), then the artifact-reading persists as mandatrophy.',
    'If the founding problem has substantially declined, the artifact-reading becomes a constraint whose original justification has evaporated; it persists through institutional inertia and authority structure interests. This would support a mandatrophy_resolved declaration and signal that the consequence-reading or principle-reading would solve actual contemporary separation needs at lower cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_shift, empirical, 'Whether the founding problem has declined or transformed.').

omega_variable(
    reading_gatekeeping,
    'Is the artifact-reading enforced as the exclusive official interpretation, or do principle-reading and consequence-reading positions retain legitimate standing within the authority structure?',
    'Review authority structure documents, sermons, disciplinary records. Count how many leadership positions are held by principle-reading and consequence-reading adherents vs. artifact-reading adherents. Assess whether dissenting readings are treated as legitimate theological alternatives or as doctrinal errors subject to discipline.',
    'If the artifact-reading is gatekept as the exclusive interpretation (principle and consequence readings are treated as errors), the constraint operates with higher suppression and lower accessibility to alternatives than if the readings are genuinely pluralized within the authority structure. Exclusive gatekeeping increases the reading''s suppression profile and its snare-like characteristics; pluralized readings would lower effective suppression and reduce mandatrophy risk.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_gatekeeping, empirical, 'Whether the artifact-reading is enforced exclusively or coexists with principle and consequence readings.').

omega_variable(
    younger_generation_exit_risk,
    'What proportion of the younger generation will exit the community due to the practical costs of artifact-form prohibition, and at what rate is that proportion growing?',
    'Track demographic data on youth retention across 10-year intervals. Conduct exit interviews with members who leave, coding for mention of artifact-form prohibition vs. other reasons. Monitor rising resistance measurements (already climbing in the coercion grid from 0.68 to 0.74 at the class level).',
    'High youth exit (>15% per generation) due to artifact costs would eventually erode community viability and force doctrine recalibration. This would be observable as rising resistance measurements and potential future type transitions (from tangled_rope toward snare as suppression becomes more coercive to hold younger members). It is also a sign that the authority structure is maintaining the artifact-reading against actual community preference, which is a snare indicator.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(younger_generation_exit_risk, empirical, 'Trajectory of youth retention under artifact-form prohibition costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__artifact_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__artifact_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(gela_tr_t5, gelassenheit_separation__artifact_reading, theater_ratio, 5, 0.45).
narrative_ontology:measurement(gela_tr_t10, gelassenheit_separation__artifact_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(gela_tr_t15, gelassenheit_separation__artifact_reading, theater_ratio, 15, 0.51).
narrative_ontology:measurement(gela_tr_t20, gelassenheit_separation__artifact_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(gela_tr_t25, gelassenheit_separation__artifact_reading, theater_ratio, 25, 0.57).
narrative_ontology:measurement(gela_tr_t30, gelassenheit_separation__artifact_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__artifact_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__artifact_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(gela_be_t5, gelassenheit_separation__artifact_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement(gela_be_t10, gelassenheit_separation__artifact_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(gela_be_t15, gelassenheit_separation__artifact_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(gela_be_t20, gelassenheit_separation__artifact_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(gela_be_t25, gelassenheit_separation__artifact_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement(gela_be_t30, gelassenheit_separation__artifact_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__artifact_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__artifact_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(gela_su_t5, gelassenheit_separation__artifact_reading, suppression_requirement, 5, 0.87).
narrative_ontology:measurement(gela_su_t10, gelassenheit_separation__artifact_reading, suppression_requirement, 10, 0.88).
narrative_ontology:measurement(gela_su_t15, gelassenheit_separation__artifact_reading, suppression_requirement, 15, 0.89).
narrative_ontology:measurement(gela_su_t20, gelassenheit_separation__artifact_reading, suppression_requirement, 20, 0.9).
narrative_ontology:measurement(gela_su_t25, gelassenheit_separation__artifact_reading, suppression_requirement, 25, 0.91).
narrative_ontology:measurement(gela_su_t30, gelassenheit_separation__artifact_reading, suppression_requirement, 30, 0.91).
narrative_ontology:measurement(gela_su_t40, gelassenheit_separation__artifact_reading, suppression_requirement, 40, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__artifact_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__artifact_reading, 0.12).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__principle_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__artifact_reading, gelassenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% The gelassenheit_separation kernel has three distinct constraint readings: (1) artifact-reading (THIS CONSTRAINT) — separation as visible distinction from English society; technology forbidden if form resembles worldly equivalents; high ε ≈ 0.82 due to form-ambiguity and enforcement overhead; (2) principle-reading — separation as functional isolation from worldly systems; technology acceptable if operationally independent; lower ε due to objective verifiability of function; (3) consequence-reading — separation as preservation of community practices (visiting, mutual aid, rootedness); technology evaluated by effect on practices; lowest ε due to actual community function. All three readings share the founding problem (assimilation resistance) but differ in their core moral property claims (form vs. function vs. consequence) and produce divergent classification outcomes. The artifact-reading is the most suppressive and extractive; the consequence-reading and principle-reading preserve separation at lower cost and higher community consensus. The artifact-reading influences both sibling readings by gatekeeping the official doctrine and treating alternative readings as doctrinal error; the consequence-reading and principle-reading coexist within the community as internal alternatives, neither foreclosing the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gelassenheit_separation__artifact_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
