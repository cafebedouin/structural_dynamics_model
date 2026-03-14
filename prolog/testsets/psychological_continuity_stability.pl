% ============================================================================
% CONSTRAINT STORY: psychological_continuity_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_psychological_continuity_stability, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: psychological_continuity_stability
 *   human_readable: Psychological Continuity Stability
 *   domain: cognitive_psychology/identity_philosophy
 *
 * SUMMARY:
 *   The requirement for psychological continuity — the assumption that a
 *   person must maintain a unified, coherent narrative identity across time
 *   to be a legal person, moral agent, and responsible actor — operates as a
 *   constraint that coordinates social institutions while simultaneously
 *   extracting from agents whose identity is fragmented by trauma,
 *   neurological variation, or phenomenological discontinuity. This
 *   constraint exhibits structural tension between its genuine coordination
 *   function (without some degree of identity continuity, contract law and
 *   criminal responsibility become incoherent) and its extractive overlay
 *   (the continuity requirement is typically enforced at a level far
 *   exceeding what coordination would require, creating suppression and
 *   psychological harm for vulnerable populations). The theater ratio (0.58)
 *   reflects that institutional enforcement increasingly relies on
 *   performative narrative coherence rather than on actual continuity of
 *   conscious experience — trauma survivors master the art of appearing
 *   continuous while experiencing profound fragmentation. The constraint's
 *   extractiveness has increased over the measurement interval as
 *   neuroscience reveals the constructed and revisable nature of identity
 *   while institutions double down on continuity requirements to maintain
 *   social order.
 *
 * KEY AGENTS:
 *   - Trauma Survivors: Primary victims (powerless/trapped) — structurally unable to maintain the enforced continuity narrative; cannot exit without losing legal recognition
 *   - Neurodivergent Persons: Secondary victims (moderate/constrained) — face high costs to maintain continuity masking; also benefit from some degree of narrative scaffolding
 *   - Legal and Social Institutions: Primary beneficiaries (institutional/arbitrage) — depend on psychological continuity as coordination mechanism for contract law, criminal liability, property rights
 *   - Disability Rights Coalition: Organized agents (organized/constrained) — building alternative identity frameworks that decouple personhood from narrative continuity
 *   - Cartesian Philosophy Framework: Institutional actor (institutional/arbitrage) — maintains performative commitment to unified self; sees own framework as degraded but necessary
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional requirement as cognitive law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(psychological_continuity_stability, 0.52).
domain_priors:suppression_score(psychological_continuity_stability, 0.65).
domain_priors:theater_ratio(psychological_continuity_stability, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(psychological_continuity_stability, extractiveness, 0.52).
narrative_ontology:constraint_metric(psychological_continuity_stability, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(psychological_continuity_stability, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(psychological_continuity_stability, tangled_rope).
narrative_ontology:human_readable(psychological_continuity_stability, "Psychological Continuity Stability").
narrative_ontology:topic_domain(psychological_continuity_stability, "cognitive_psychology/identity_philosophy").

domain_priors:requires_active_enforcement(psychological_continuity_stability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(psychological_continuity_stability, social_institutions).
narrative_ontology:constraint_beneficiary(psychological_continuity_stability, legal_systems).
narrative_ontology:constraint_beneficiary(psychological_continuity_stability, narrative_identity_frameworks).
narrative_ontology:constraint_victim(psychological_continuity_stability, agents_experiencing_identity_discontinuity).
narrative_ontology:constraint_victim(psychological_continuity_stability, trauma_survivors).
narrative_ontology:constraint_victim(psychological_continuity_stability, neurodivergent_persons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAUMA SURVIVOR (SNARE) — Structurally trapped within the requirement to maintain narrative continuity despite fractured memory and dissociated identity. Cannot exit the constraint without losing legal personhood recognition. Bears full psychological cost of enforced coherence narrative.
constraint_indexing:constraint_classification(psychological_continuity_stability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NEURODIVERGENT PERSON (TANGLED ROPE) — Faces high costs to exit the continuity requirement (masking exhaustion, identity fragmentation, cognitive overload) but also benefits from neurotypical coordination mechanisms that assume stable continuous identity. Extraction is significant but not total — some degree of identity scaffolding is genuinely useful.
constraint_indexing:constraint_classification(psychological_continuity_stability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGAL AND SOCIAL INSTITUTIONS (ROPE) — Benefit from psychological continuity requirement as coordination mechanism. Contract law, criminal liability, property inheritance, and social trust all depend on the assumption of a continuous responsible agent. Net beneficiary with institutional capacity to arbitrage — can modify the continuity standard when it becomes dysfunctional.
constraint_indexing:constraint_classification(psychological_continuity_stability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DISABILITY RIGHTS COALITION (SCAFFOLD) — Organized agents (neurodiversity advocates, trauma-informed psychology, legal accommodations movements) are building alternative identity frameworks that decouple legal/social personhood from narrative continuity. Sunset mechanism: as identity plurality becomes normalized and legally recognized, the continuity requirement becomes optional rather than enforced.
constraint_indexing:constraint_classification(psychological_continuity_stability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CARTESIAN PHILOSOPHY FRAMEWORK (PITON) — The assumption that a continuous unified self is a prerequisite for agency, responsibility, and personhood persists through institutional inertia in legal and philosophical discourse. The framework is largely performative — neuroscience reveals identity as constructed and revisable, yet institutions maintain the theater of essential continuity. Maintained through academic tradition and legal precedent rather than functional necessity.
constraint_indexing:constraint_classification(psychological_continuity_stability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some continuity binding is inherent to agency itself: to act in the world, an agent must have minimal temporal integration between intention and execution. Without some degree of self-continuity, action itself becomes incoherent. This perspective risks naturalizing a contingent institutional requirement as a law of cognition — a false summit that the engine's detector should flag.
constraint_indexing:constraint_classification(psychological_continuity_stability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(psychological_continuity_stability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(psychological_continuity_stability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(psychological_continuity_stability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(psychological_continuity_stability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(psychological_continuity_stability, TR),
    TR >= 0.70.

:- end_tests(psychological_continuity_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts psychological labor from agents whose identity is naturally discontinuous. Trauma survivors must perform continuity masking; neurodivergent persons must suppress plural self-states; dissociative persons must suppress conscious discontinuity. The extraction is enforced through legal penalties (loss of personhood, commitment, guardianship) and social stigma (diagnosis of disorder, moral failure). However, extractiveness is not as extreme as pure snare (0.70+) because some degree of minimal continuity genuinely serves coordination — agents do need some temporal integration to act intentionally. The measured value reflects the surplus extraction above the coordination minimum. Suppression (0.65): High. Agents face multiple barriers to rejecting the continuity requirement: legal consequences (commitment, loss of contracts), social consequences (stigmatization as disordered), cognitive consequences (internalized shame, identity denial). The suppression is strong because the constraint operates on identity itself — it attempts to shape how agents experience their own consciousness. Theater ratio (0.58): Moderate-high. Institutions increasingly engage in performative continuity verification — institutions cannot actually assess whether an agent experiences continuous phenomenological consciousness, so they rely on narrative coherence tests (can you tell a coherent life story?), behavioral consistency checks, and memory tests. These are theater: they correlate with perceived continuity without measuring actual consciousness continuity. Trauma survivors become expert at performing continuity despite fragmentation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp disagreement between structural positions on whether continuity is genuine coordination or extractive enforcement. Institutions perceive coordination — continuity is necessary for law to function. Trauma survivors perceive snare — the continuity requirement is unachievable and extracts psychological cost regardless of effort. Disability advocates perceive scaffold — alternative identity frameworks (legal pluralism, neurodiversity acceptance) are building new coordination mechanisms that don't require continuity. The piton view recognizes that philosophical commitment to unified selfhood persists through inertia — neuroscience has undermined Cartesian identity, yet institutions maintain the theater. The mountain view risks naturalizing contingency — the assumption that continuous identity is a prerequisite for agency is an institutional artifact, not a law of cognition. The perspectival gap is sharpest between institutional beneficiaries (who see coordination) and trapped victims (who experience extraction despite compliance effort).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position relative to the continuity extraction flow. Trauma survivors are pure targets (d ≈ 0.95): they are fully identified with the victim group, trapped by legal consequences of discontinuity, powerless to change institutional requirements. Neurodivergent persons are partially targets (d ≈ 0.70): they face significant extraction through masking costs but also benefit from identity scaffolding and have some agency to negotiate accommodations. Institutions are beneficiaries (d ≈ 0.05): continuity requirement flows value toward institutional function; they can exit or arbitrage when continuity becomes dysfunctional (e.g., neurodiversity accommodations). Disability advocates are organized agents with constrained exit (d ≈ 0.45): they are partly victims (serving marginalized populations) and partly leveraging institutional capacity to redefine standards. The Cartesian framework is an institutional beneficiary (d ≈ 0.10): maintains philosophical commitment that preserves institutional order. The analytical observer risks d ≈ 0.72 if they naturalize contingency as law — their analysis becomes captured by the false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVING MANDATROPHY: The constraint resolves as tangled_rope because it combines genuine coordination function (institutions need some degree of identity continuity to function) with asymmetric extraction (continuity requirement is enforced far beyond coordination minimum, creating suppression for discontinuous agents). The mandatrophy is resolved by recognizing that identity-based constraints naturally contain both elements: identity coordination is always hybrid because identity itself is simultaneously a genuine coordination mechanism (we coordinate through shared assumptions about who agents are) and an extraction mechanism (we can use identity requirements to extract psychological labor). The false summit (mountain classification at the analytical level) is correctly identified as a false summit because the apparent natural law (continuity is inherent to agency) dissolves when examined through disability rights perspectives that show agency is possible without continuity. The institutional piton is correctly identified as degraded — philosophical commitment to unified self persists despite neuroscience revealing plurality, maintained through institutional inertia rather than functional necessity. The scaffold is structurally real — disability rights frameworks are building genuine alternative coordination mechanisms that decouple personhood from continuity, with a realistic sunset trajectory as legal precedent accumulates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_threshold_ambiguity,
    'What degree of psychological discontinuity is compatible with legal personhood and moral responsibility?',
    'Case law analysis from dissociative identity disorder litigation; comparison of institutional treatment across legal jurisdictions; longitudinal tracking of identity discontinuity thresholds in accommodation law',
    'If threshold is set conservatively (requiring high continuity): trauma survivors and neurodivergent persons are systematically denied accommodations and legal standing. If threshold is permissive (allowing high discontinuity): institutions lose coordination mechanisms and cannot enforce contracts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_threshold_ambiguity, empirical, 'Threshold for psychological discontinuity compatible with legal personhood').

omega_variable(
    narrative_vs_phenomenological_continuity,
    'Are continuity requirements based on narrative-autobiographical coherence or on moment-to-moment phenomenological continuity? Are these the same thing?',
    'Philosophical analysis of neuroscientific evidence on self-representation; comparison of institutions that require narrative vs lived continuity; examination of dissociative disorders where narrative is fragmented but phenomenological consciousness is integrated',
    'If narrative continuity is the actual requirement: trauma survivors experiencing memory gaps can maintain legal personhood if they construct coherent post-hoc narratives (theater mechanism). If phenomenological continuity is required: the constraint is genuinely about continuous experience, not story construction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(narrative_vs_phenomenological_continuity, conceptual, 'Whether continuity requirement is narrative or phenomenological').

omega_variable(
    institutional_arbitrage_scope,
    'Can institutions that depend on psychological continuity actually function with meaningful pluralization of identity standards, or does the requirement collapse back to enforced continuity under pressure?',
    'Analysis of institutional adaptation in jurisdictions that have legally recognized identity plurality; stress-testing of alternatives under conditions of high transaction complexity or systemic risk',
    'If institutions can arbitrage: the scaffold perspective is correct and continuity requirement can genuinely sunset. If institutions collapse under identity plurality: the rope perspective is correct and continuity is a necessary enforcement mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_arbitrage_scope, empirical, 'Whether institutions can function with pluralized identity standards').

omega_variable(
    trauma_recovery_suppression_mechanism,
    'Is suppression of identity discontinuity in trauma survivors structural (institutional barriers to alternative identity frameworks) or internalized (cognitive patterns of denial and fragmentation that persist after institutional barriers are removed)?',
    'Longitudinal comparison of trauma survivors in jurisdictions with strong vs weak continuity requirements; post-institutional analysis of identity integration in communities that normalize discontinuity',
    'If structural: removing institutional continuity requirements enables recovery. If internalized: the suppression persists and the constraint''s effective suppression is higher than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trauma_recovery_suppression_mechanism, empirical, 'Whether suppression is structural or internalized in trauma recovery').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(psychological_continuity_stability, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(psyc_tr_t0, psychological_continuity_stability, theater_ratio, 0, 0.35).
narrative_ontology:measurement(psyc_tr_t25, psychological_continuity_stability, theater_ratio, 25, 0.48).
narrative_ontology:measurement(psyc_tr_t50, psychological_continuity_stability, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(psyc_be_t0, psychological_continuity_stability, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(psyc_be_t25, psychological_continuity_stability, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(psyc_be_t50, psychological_continuity_stability, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(psychological_continuity_stability, identity_coordination).
narrative_ontology:boltzmann_floor_override(psychological_continuity_stability, 0.12).
narrative_ontology:affects_constraint(psychological_continuity_stability, criminal_responsibility_framework).
narrative_ontology:affects_constraint(psychological_continuity_stability, legal_personhood_status).
narrative_ontology:affects_constraint(psychological_continuity_stability, autobiographical_memory_requirement).

% DUAL FORMULATION NOTE:
% Psychological continuity stability is upstream of legal personhood and criminal responsibility constraints. Those downstream constraints inherit the continuity requirement and amplify its extractive effects through institutional coupling. The upstream constraint's degradation (recognition of identity plurality) cascades to transform downstream constraints' classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(psychological_continuity_stability, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
