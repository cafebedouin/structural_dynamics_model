% ============================================================================
% CONSTRAINT STORY: personhood_boundary__birth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__birth_reading, []).

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
 *   constraint_id: personhood_boundary__birth_reading
 *   human_readable: Personhood Boundary at Birth: Absolute Maternal Bodily Autonomy
 *   domain: normative_ethics/political_philosophy/bioethics
 *
 * SUMMARY:
 *   The birth-reading of the personhood boundary represents one institutional
 *   commitment for resolving the contested claim 'when does a person begin?'
 *   This reading establishes that moral and legal personhood commence at live
 *   birth, with full accompanying rights. Critically, this reading implies
 *   that the fetus has no independent moral status prior to birth and cannot
 *   constrain the pregnant person's bodily autonomy. The constraint operates
 *   as a tangled rope from the analytical perspective: it provides genuine
 *   coordination benefits (medical practice clarity, legal certainty for
 *   reproductive rights) while simultaneously extracting from those who hold
 *   competing personhood theories (fetal autonomy advocates,
 *   conception-reading adherents). The extractiveness arises from the
 *   institutionalization of one boundary claim as legitimate while others are
 *   suppressed — not through violence but through structural exclusion from
 *   the victim set and from moral consideration. The theater_ratio has risen
 *   over time (0.35 → 0.48 across the measurement interval), indicating
 *   increasing performativity: advocates appeal to 'natural' personhood while
 *   actually defending an institutional choice. The suppression_requirement
 *   has also risen, suggesting that maintaining the birth-reading's
 *   legitimacy requires increasing active enforcement — legal prohibition of
 *   alternatives, institutional gatekeeping, and doctrinal defense.
 *
 * KEY AGENTS:
 *   - Pregnant Persons: Primary beneficiaries (institutional/arbitrage in reproductive-rights institutions; moderate/constrained in restrictive jurisdictions) — benefit from bodily autonomy principle and legal clarity
 *   - Bodily Autonomy Claimants: Overlapping beneficiary group (powerful/mobile in liberal democracies; powerless/trapped in restrictive jurisdictions) — benefit from the principle's institutional codification
 *   - Fetal Autonomy Advocates: Primary victims (powerless/trapped) — structurally excluded from moral consideration; cannot exit or negotiate within the framework
 *   - Potential Personhood Advocates: Overlapping victim group (moderate/constrained) — their alternative boundary claims are excluded from institutionalized legitimacy
 *   - Reproductive Rights Institutions: Institutional beneficiaries (institutional/arbitrage) — clinics, medical licensing, advocacy organizations benefit from legal clarity and coordinated practice
 *   - Religious Authority Structures: Degraded institutional position (institutional/constrained) — once adjudicated personhood questions; now subordinate to secular boundary claims
 *   - Analytical Observer: Cross-position analyst (analytical/analytical) — sees both coordination function and asymmetric exclusion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__birth_reading, 0.28).
domain_priors:suppression_score(personhood_boundary__birth_reading, 0.65).
domain_priors:theater_ratio(personhood_boundary__birth_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(personhood_boundary__birth_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(personhood_boundary__birth_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__birth_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__birth_reading, "Personhood Boundary at Birth: Absolute Maternal Bodily Autonomy").
narrative_ontology:topic_domain(personhood_boundary__birth_reading, "normative_ethics/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(personhood_boundary__birth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__birth_reading, 'd1693383-1bfc-4383-81ba-6ea36c9e968d').
narrative_ontology:cs_kernel_codification('d1693383-1bfc-4383-81ba-6ea36c9e968d', fixed_text).
narrative_ontology:cs_authority_grounding('d1693383-1bfc-4383-81ba-6ea36c9e968d', lineage).
narrative_ontology:cs_interpretation_layer_present('d1693383-1bfc-4383-81ba-6ea36c9e968d').
narrative_ontology:cs_reading_relation('d1693383-1bfc-4383-81ba-6ea36c9e968d', personhood_boundary__conception_reading, coexists_with).
narrative_ontology:cs_reading_relation('d1693383-1bfc-4383-81ba-6ea36c9e968d', personhood_boundary__viability_reading, influences).
narrative_ontology:cs_axiom('d1693383-1bfc-4383-81ba-6ea36c9e968d', foundational, bodily_autonomy_absolutism).
narrative_ontology:cs_axiom_status(bodily_autonomy_absolutism, holdable).
narrative_ontology:cs_axiom_grounding('d1693383-1bfc-4383-81ba-6ea36c9e968d', bodily_autonomy_absolutism, deontological).
narrative_ontology:cs_axiom('d1693383-1bfc-4383-81ba-6ea36c9e968d', foundational, personhood_status_at_birth).
narrative_ontology:cs_axiom_status(personhood_status_at_birth, holdable).
narrative_ontology:cs_axiom_grounding('d1693383-1bfc-4383-81ba-6ea36c9e968d', personhood_status_at_birth, deontological).
narrative_ontology:cs_reference_frame('d1693383-1bfc-4383-81ba-6ea36c9e968d', liberal_bodily_autonomy_framework).
narrative_ontology:cs_drift_state('d1693383-1bfc-4383-81ba-6ea36c9e968d', post_reproductive_justice_movement, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('d1693383-1bfc-4383-81ba-6ea36c9e968d', '').
narrative_ontology:cs_kernel_id(personhood_boundary__birth_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_reading, pregnant_persons).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_reading, bodily_autonomy_claimants).
narrative_ontology:constraint_victim(personhood_boundary__birth_reading, fetal_autonomy_claimants).
narrative_ontology:constraint_victim(personhood_boundary__birth_reading, potential_personhood_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PREGNANT PERSON (MOUNTAIN) — From the position of a pregnant person in a jurisdiction that denies abortion, the constraint appears as an immutable natural/legal boundary: the fetus is declared a rights-holder and her autonomy is overridden by law. No exit available; the constraint is experienced as absolute and unchangeable within the biographical horizon. The mountain classification reflects experienced immutability from structural entrapment.
constraint_indexing:constraint_classification(personhood_boundary__birth_reading, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: PREGNANT PERSON WITH ACCESS (TANGLED ROPE) — In jurisdictions recognizing the birth boundary, the pregnant person experiences a mixed constraint: coordination function (medical support, legal certainty, bodily autonomy principle) coexists with asymmetric extraction (state manages her medical decisions, social pressure, resource scarcity in reproductive care). The constraint is experienced as changeable through legal/political action, but exit costs remain high (travel, stigma, medical disruption). Moderate power, constrained exit.
constraint_indexing:constraint_classification(personhood_boundary__birth_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FETAL AUTONOMY CLAIMANTS (SNARE) — From the position of advocates for fetal personhood and autonomy, the birth-reading constraint is a pure extraction mechanism. The fetus is entirely excluded from the victim set and from moral consideration; the constraint benefits pregnant persons at the expense of fetal interests. This perspective experiences high extraction with no coordination function — they cannot exit the framework through negotiation or appeal. The snare gate fires: high suppression (their alternative framings are excluded from law), high extraction (fetal interests overridden), minimal coordination benefit for them.
constraint_indexing:constraint_classification(personhood_boundary__birth_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: REPRODUCTIVE RIGHTS INSTITUTIONALISTS (ROPE) — Institutions (clinics, medical licensing boards, abortion-rights organizations) that operate under the birth-reading framework experience it as pure coordination: communicating the principle enables medical practice, legal certainty, and organizational mission alignment. No asymmetric extraction from their vantage — they benefit from the clarity and can exit through institutional choices (e.g., relocation, scope changes). This is the institutional beneficiary position.
constraint_indexing:constraint_classification(personhood_boundary__birth_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: RELIGIOUS AUTHORITY STRUCTURES (PITON) — Religious institutions that historically grounded personhood claims in conception or ensoulment now operate under the birth-reading constraint in many jurisdictions. They see their own authority as degraded: they once adjudicated the personhood question through doctrinal authority; now they appeal to the same state law they once rejected. The theater_ratio is high (performative moral authority without doctrinal power); extractiveness is low (they have exited through other means: education, lobbying, parallel institutional structures). Piton: degraded but persistent through institutional inertia.
constraint_indexing:constraint_classification(personhood_boundary__birth_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational/analytical level, the birth-reading constraint is recognized as a constructed institutional commitment grounding legitimate authority in a specific boundary claim. The analytical observer sees both the coordination function (legal certainty, medical practice enabled) and the extraction mechanism (fetal interests excluded, potential personhood advocates structurally powerless). The constraint is neither natural law nor pure extraction; it is a deliberate institutional choice with coherent justification and structural costs.
constraint_indexing:constraint_classification(personhood_boundary__birth_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__birth_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(personhood_boundary__birth_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(personhood_boundary__birth_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(personhood_boundary__birth_reading, TR),
    TR >= 0.70.

:- end_tests(personhood_boundary__birth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate-low. The birth-reading does provide genuine coordination benefits for medical practice, legal certainty, and institutional clarity. However, extractiveness is non-zero because the boundary claim is not justified as an empirical discovery or logical necessity but as a policy choice that excludes viable alternative frameworks. The fetal autonomy claimants experience this as pure extraction (snare perspective, high d), while reproductive-rights beneficiaries experience it as coordination (rope perspective, low d). The base_extractiveness of 0.28 reflects the averaged structural position before applying the sigmoid f(d) modifier. Suppression (0.65): Moderate-high. The constraint requires active suppression of competing boundary readings. This is enforced through: legal prohibition of alternative personhood claims in institutional medical settings, educational gatekeeping (alternative readings excluded from medical school curricula), doctrinal authority (dominant ethical frameworks adopt the birth boundary as settled), and resource allocation (funding for reproductive rights advocacy vastly exceeds funding for fetal personhood research in many jurisdictions). The measurement trajectory (0.58 → 0.65) indicates rising suppression requirement — as the reading faces increasing philosophical challenge, enforcement machinery intensifies. Theater ratio (0.48): Moderate. The constraint exhibits performative elements: appeals to 'natural' personhood, invocation of bodily autonomy as axiomatic (rather than acknowledged as a policy choice), and institutional theater around 'settled science' when the boundary is actually contested. However, the reading also has genuine substantive content — bodily autonomy principles do coherently ground the birth boundary, and the medical/legal coordination benefits are real. Theater_ratio = 0.48 reflects a constraint that is neither purely performative nor purely functional.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is the core diagnostic feature of this constraint. The pregnant person in a restrictive jurisdiction experiences mountain-level immutability, while the pregnant person in a permissive jurisdiction experiences tangled-rope with achievable exit (through legal appeal or geographic mobility). The fetal autonomy advocate experiences snare-level extraction with no exit. The analytical observer sees both the coordination function and the exclusion mechanism, recognizing the constraint as a constructed institutional choice rather than a natural law or inevitable outcome. This gap reveals that the same structural claim — 'personhood begins at birth' — operates as an immutable law, a workable coordination mechanism, and a pure extraction mechanism depending on the observer's position. The gap is not a measurement error; it is the constraint's diagnostic signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural relationship to the birth boundary and the victim/beneficiary declaration. Pregnant persons in beneficiary roles (institutional/arbitrage) have d ≈ 0.05-0.15, producing low or slightly negative f(d), experiencing the constraint as coordination (rope). Pregnant persons in victim-adjacent positions (moderate/constrained) have d ≈ 0.50-0.65, producing moderate f(d), experiencing mixed extraction and coordination (tangled rope). Fetal autonomy claimants in strict victim roles (powerless/trapped) have d ≈ 0.90+, producing high f(d) ≈ 1.40, experiencing maximum extraction (snare). The analytical observer at civilizational scope (d ≈ 0.72) experiences effective extraction at χ ≈ ε × 1.15 × 1.0, recognizing both the genuine coordination and the structural exclusion. The directionality overrides are not needed — the structural derivation from beneficiary/victim declarations and exit options produces accurate d values across all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The birth-reading does not generate mandatrophy in the classical sense (mislabeling coordination as extraction or vice versa) because the extractiveness is genuinely moderate — the constraint is neither pure coordination nor pure extraction but an honest hybrid. The mandatrophy resolution comes through explicit recognition that the boundary claim is an institutional choice with justified beneficiaries and genuine victims. The key insight is that the fetal autonomy advocate's snare experience is not a misclassification or measurement error; it is the actual structural consequence of the birth-reading. From their position, the constraint is extractive. From the reproductive-rights beneficiary's position, it is coordinative. The mandatrophy is resolved by acknowledging both as structurally true — the constraint is a tangled rope that appears as snare to the excluded party and as rope to the beneficiary. This is not a failure of the framework but a demonstration of how indexical classification reveals structural asymmetry that unitary classification (claiming the constraint 'is' either rope or snare) obscures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    personhood_kernel_identity,
    'Does the birth boundary represent a genuine kernel of contested personhood, or is the ''contest'' itself a constructed artifact of institutional framing?',
    'Cross-cultural and historical analysis of personhood boundary claims; identification of whether the conception/viability/birth triad emerges naturally from philosophical traditions or is an artifact of Western medical/legal systematization',
    'If contested kernel: all readings are live options. If constructed artifact: birth-reading advocates are naturalizing an institutional choice and the sibling readings are competing articulations of the same underlying claim. Classification would shift from tangled_rope toward snare for non-beneficiary perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(personhood_kernel_identity, conceptual, 'Whether personhood boundary dispute is a genuine kernel or constructed institutional artifact').

omega_variable(
    bodily_autonomy_absolutism,
    'Is bodily autonomy an absolute right, or does it admit proportional constraints when another rights-holder''s interests are sufficiently weighty?',
    'Philosophical analysis of autonomy rights in other medical contexts (organ donation, forced blood transfusion, immunization mandates); whether the birth-reading''s absolutism is coherent with other bodily autonomy principles',
    'If absolute: birth-reading stands. If proportional: fetal interests could constrain maternal autonomy (shift toward viability_reading or conception_reading). Theater ratio would rise (the constraint becomes more performative as it appeals to absolutism while other practices admit proportionality).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bodily_autonomy_absolutism, conceptual, 'Whether bodily autonomy is absolute or admits proportional constraints').

omega_variable(
    fetal_interests_epistemic_status,
    'Can fetal interests be meaningfully ascribed independent of maternal interests, or are they inherently entangled such that the distinction is conceptual only?',
    'Philosophical analysis of interest attribution to pre-autonomous beings; empirical investigation of fetal sensory capacity and neural correlates of suffering; whether interest-independence is metaphysically coherent',
    'If genuinely independent: birth-reading''s exclusion of fetal interests is a structural choice (constraint is real). If entangled: the birth-reading is correct but the snare perspective is misidentifying its exclusion — fetal interests aren''t being excluded, they''re being properly attributed as inseparable from maternal interests. This would lower extractiveness because the suppression of the snare perspective becomes justified exclusion rather than illegitimate extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fetal_interests_epistemic_status, empirical, 'Whether fetal interests are metaphysically independent or entangled with maternal interests').

omega_variable(
    reading_incommensurability,
    'Are the birth-reading, conception-reading, and viability-reading fundamentally incommensurable (no shared evaluative framework), or do they inhabit a common moral space where one could be demonstrated superior?',
    'Meta-ethical analysis; identification of shared moral vocabulary across readings; test whether any reading can accommodate the others'' core concerns without abandoning its own foundation',
    'If incommensurable: coexists_with relation holds (birth-reading neither forecloses nor influences siblings). If commensurable: one reading could foreclose another through stronger argument. Currently classified as coexisting; commensurability would require reconsideration of reading_relations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_incommensurability, conceptual, 'Whether personhood boundary readings are incommensurable or occupying a shared moral space').

omega_variable(
    institutional_boundary_stability,
    'Has the birth boundary achieved institutional stability across diverse legal/medical systems, or does it remain contested in practice despite legal codification?',
    'Documentation of jurisdictional variation; tracking of legal challenges and boundary shifts; analysis of medical practice deviation from stated legal boundaries (e.g., late-term abortion circumstances, neonatal resuscitation decisions)',
    'If stable: extractiveness remains moderate. If contested: the constraint is performing stability (theater_ratio rises) while lacking genuine institutional consensus — classification would shift toward piton (degraded enforcement, high theater). Current theater_ratio (0.48) suggests moderate performativity; rising theater would indicate increasing institutional degradation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_boundary_stability, empirical, 'Whether birth boundary has achieved genuine institutional stability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__birth_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pb_birth_theater_t0, personhood_boundary__birth_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pb_birth_theater_t30, personhood_boundary__birth_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(pb_birth_theater_t60, personhood_boundary__birth_reading, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(pb_birth_extract_t0, personhood_boundary__birth_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(pb_birth_extract_t30, personhood_boundary__birth_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement(pb_birth_extract_t60, personhood_boundary__birth_reading, base_extractiveness, 60, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(pb_birth_suppress_t0, personhood_boundary__birth_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(pb_birth_suppress_t30, personhood_boundary__birth_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(pb_birth_suppress_t60, personhood_boundary__birth_reading, suppression_requirement, 60, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__birth_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__birth_reading, personhood_boundary__conception_reading).
narrative_ontology:affects_constraint(personhood_boundary__birth_reading, personhood_boundary__viability_reading).
narrative_ontology:affects_constraint(personhood_boundary__birth_reading, bodily_autonomy_institutional_enforcement).
narrative_ontology:affects_constraint(personhood_boundary__birth_reading, reproductive_restriction_regimes).

% DUAL FORMULATION NOTE:
% The personhood_boundary kernel decomposes into three distinct constraints corresponding to the three major boundary readings (conception, viability, birth). Each reading instantiates its own constraint with distinct ε values, beneficiary/victim structures, and suppression mechanisms. The birth-reading (this constraint) has ε=0.28 (moderate extraction due to exclusion of alternative readings); the conception-reading has ε≈0.15 (lower extraction, less institutionally entrenched in liberal democracies, higher resistance); the viability-reading has ε≈0.35-0.42 (moderate-high extraction due to the practical difficulty of precisely identifying viability and the need for active judicial/medical gatekeeping). These are not measurements of the same constraint under different observables; they are structurally distinct constraints with different core claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
