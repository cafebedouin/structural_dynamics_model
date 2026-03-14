% ============================================================================
% CONSTRAINT STORY: performer_likeness_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performer_likeness_rights, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: performer_likeness_rights
 *   human_readable: Performer Likeness Rights and Commercial Exploitation
 *   domain: intellectual_property/entertainment/commercial_law
 *
 * SUMMARY:
 *   Performer likeness rights create a structural tension between protecting
 *   performers' commercial interests in their image and enabling derivative
 *   creation, commentary, and cultural participation. The constraint exhibits
 *   tangled coordination-extraction: established performers benefit from
 *   rights frameworks that protect their brand and enable licensing revenue,
 *   while emerging performers and derivative creators face suppressive
 *   barriers to image reproduction. Licensing agencies maintain performative
 *   governance infrastructure (rights registration, clearance coordination)
 *   that was essential when reproduction required distribution but persists
 *   through institutional inertia. The constraint's extractiveness has
 *   increased over the interval (0.32 to 0.58) as digital reproduction made
 *   image copying technically frictionless, driving rights-enforcement
 *   expansion and licensing cost accumulation. Theater ratio has remained
 *   moderate (0.35 to 0.48) because actual enforcement does require genuine
 *   administrative effort, unlike pure performative constraints — but the
 *   ratio is rising as synthetic likeness generation forces rights frameworks
 *   to become increasingly theoretical (defining infringement boundaries for
 *   images that don't exist).
 *
 * KEY AGENTS:
 *   - Established Performers: Primary beneficiaries (institutional/arbitrage) — capture licensing revenue and brand protection through rights frameworks
 *   - Emerging Performers: Primary victims (powerless/trapped) — cannot afford licensing fees for own promotional use in some jurisdictions, face barriers to unauthorized-but-transformative self-presentation
 *   - Derivative Creators (Fan Artists, Satirists, Educators): Secondary victims (moderate/constrained) — face licensing friction and legal uncertainty for transformative uses
 *   - Platform Intermediaries (TikTok, YouTube, Instagram): Institutional actors (institutional/constrained) — liable for user content; must enforce rights but profit from enabling derivative creation; experience the constraint as mixed enforcement burden and revenue opportunity
 *   - Licensing Agencies (Performing Rights Organizations, Rights Registries): Institutional beneficiaries (institutional/arbitrage) — extract rent from rights administration; maintain infrastructure that persists despite digital-native alternatives
 *   - Public Domain and Cultural Commons: Primary victim (powerless/trapped) — systematic reduction in freely accessible performer images and publicly usable likenesses; academic and cultural accessibility declines
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performer_likeness_rights, 0.58).
domain_priors:suppression_score(performer_likeness_rights, 0.62).
domain_priors:theater_ratio(performer_likeness_rights, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performer_likeness_rights, extractiveness, 0.58).
narrative_ontology:constraint_metric(performer_likeness_rights, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(performer_likeness_rights, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performer_likeness_rights, tangled_rope).
narrative_ontology:human_readable(performer_likeness_rights, "Performer Likeness Rights and Commercial Exploitation").
narrative_ontology:topic_domain(performer_likeness_rights, "intellectual_property/entertainment/commercial_law").

domain_priors:requires_active_enforcement(performer_likeness_rights).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performer_likeness_rights, established_performers).
narrative_ontology:constraint_beneficiary(performer_likeness_rights, rights_holders).
narrative_ontology:constraint_beneficiary(performer_likeness_rights, licensing_agencies).
narrative_ontology:constraint_victim(performer_likeness_rights, emerging_performers).
narrative_ontology:constraint_victim(performer_likeness_rights, public_domain_accessibility).
narrative_ontology:constraint_victim(performer_likeness_rights, derivative_creators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING PERFORMER (SNARE) — Faces regulatory barriers to image reproduction, cannot afford licensing fees for fan-created content or derivative works, and has no negotiating power with established rights-management infrastructure. Exit would require abandoning digital presence and commercial viability. High suppression through legal threat and economic dependency on platforms that enforce rights regimes.
constraint_indexing:constraint_classification(performer_likeness_rights, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT CREATOR (TANGLED ROPE) — Benefits from rights frameworks that protect their own creations but bears costs when using performer likenesses in derivative works, transformative art, or commentary. Significant friction in negotiating clearances; career path dependent on either licensing agreements or fair-use interpretations. Constrained by transaction costs and legal uncertainty.
constraint_indexing:constraint_classification(performer_likeness_rights, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED PERFORMER (ROPE) — Experiences likeness rights as coordination infrastructure protecting their brand and enabling licensing revenue. Rights frameworks enable profitable partnerships, prevent unauthorized exploitation, and establish stable pricing. Net beneficiary — the constraint coordinates their commercial interests and generates extractive capacity they control.
constraint_indexing:constraint_classification(performer_likeness_rights, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM INTERMEDIARY (TANGLED ROPE) — Operates under legal liability for user-generated content featuring performer likenesses; must enforce rights regimes to avoid takedown notices but also profits from enabling derivative content creation. Caught between rights enforcement (imposed cost) and user engagement (revenue source). Requires active enforcement but also extracts platform rent from the coordination.
constraint_indexing:constraint_classification(performer_likeness_rights, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LICENSING AGENCY (PITON) — Maintains performative governance infrastructure (rights registration, clearance coordination, fee collection) that was essential when reproduction required distribution infrastructure but persists through institutional inertia despite digital-native alternatives. Theater ratio high: much regulatory effort goes to enforcement theater rather than genuine coordination, as digital copying is technically uncontrollable. Beneficiary through rent extraction; sees own role as increasingly degraded.
constraint_indexing:constraint_classification(performer_likeness_rights, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk of naturalizing likeness rights as inherent property entitlements: 'people own their image, therefore...' This perspective treats the rights regime as a natural law rather than a contingent institutional arrangement. However, the structural data reveals this as a false summit — likeness rights are historically contingent (absent in most pre-modern societies), technologically dependent (arise from reproducibility), and jurisdictionally variable (differ across countries). The 'mountain' framing risks naturalizing what is actually a tangled institutional arrangement.
constraint_indexing:constraint_classification(performer_likeness_rights, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performer_likeness_rights_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(performer_likeness_rights, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(performer_likeness_rights, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(performer_likeness_rights, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(performer_likeness_rights, TR),
    TR >= 0.70.

:- end_tests(performer_likeness_rights_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extracts value from emerging performers and derivative creators through licensing friction, legal threat, and gatekeeping by established performers and agencies. However, extraction is not maximal because (1) some performers willingly license images at reasonable cost, creating genuine coordination benefit; (2) fair-use exceptions provide partial escape routes for transformative use; (3) synthetic generation is beginning to decouple image use from likeness rights (omega variable). The upward trend (0.32→0.58) reflects increasing enforcement costs and licensing expansion driven by digital reproduction ease. Suppression (0.62): Moderately high. Barriers include legal threat (copyright/publicity right enforcement), economic cost (licensing fees), technical obscurity (rights holders difficult to identify), and institutional gatekeeping (platforms enforce on behalf of absent rights holders). However, suppression is not maximal (would be 0.85+) because (1) fair use provides legitimate escape routes; (2) platforms vary in enforcement rigor; (3) cross-border enforcement is fragmented; (4) emerging performers can sometimes negotiate directly. Theater ratio (0.48): Moderate. Licensing coordination requires genuine administrative effort (rights registration, fee collection, clearance tracking), so the theater is not purely performative. But the ratio is rising (0.35→0.48) because enforcement increasingly focuses on boundary cases (synthetic likeness, transformative fair use) where the theatrical aspects dominate — debates about what constitutes 'infringement' when the reproduction is synthetic or transformative reveal the governance theater rather than functional protection.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates sharp perspectival divergence between established performers and emerging creators. Established performers see rope (coordination protecting their brand), while emerging performers see snare (legal trap with no exit). Platform intermediaries experience the constraint as tangled rope (both enabling and enforcing), while independent creators experience it as tangled rope with higher extraction (constrained exit due to platform liability). The licensing agency sees piton (their infrastructure is increasingly theater as digital reproduction makes enforcement theoretical). The analytical observer risks the mountain perspective (naturalizing likeness rights as inherent property), but the structural data reveals this as a false summit — likeness rights are contingent historical institutions, not natural laws.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from beneficiary/victim status and exit options. Established performers with institutional power and arbitrage exit (they can license directly or refuse licensing) experience low d → negative χ (they benefit). Emerging performers with powerless status and trapped exit (they cannot afford licensing, cannot exit commercial platforms) experience high d → high χ (they are extracted from). Independent creators with moderate power and constrained exit (they can create but face friction and legal risk) experience moderate d → moderate χ. Platform intermediaries with institutional power but constrained exit (they must enforce for liability reasons; cannot exit enforcement) experience moderate d due to the split between beneficiary position and enforcement burden. Licensing agencies with institutional power and arbitrage exit (they profit from rights administration) experience low d → negative χ in the pipeline, but they are classified as beneficiaries in the constraint structure, so actual χ reflects their extraction of licensing rent.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: This constraint is genuinely tangled_rope, not mislabeled snare disguised as coordination. The coordination function is real: performers do benefit from formalized protection against unauthorized commercial exploitation, and licensing frameworks do enable profitable partnerships that wouldn't exist without clear ownership. The extraction is also real: emerging performers and derivative creators bear genuine costs (licensing fees, legal uncertainty, gatekeeping) that don't contribute to their own benefit. The constraint succeeds as tangled_rope because both functions persist simultaneously — you cannot remove the extraction without destroying coordination capacity, and you cannot increase coordination without expanding extraction. The rising extractiveness (0.32→0.58) reflects increasing costs of enforcement theater as synthetic generation and transformative use push the boundary of what 'likeness infringement' means. The false mountain perspective (treating likeness rights as natural law) is diagnostically important: it reveals the naturalization pattern by which institutional arrangements become invisible as property entitlements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    artificial_likeness_generation,
    'What is the boundary between a performer''s protectable likeness and synthetic generated images that approximate but do not reproduce the original likeness?',
    'Court cases establishing standards for synthetic likeness infringement; emergence of technical metrics (facial recognition scores, perceptual thresholds); legislative definitions of ''likeness'' vs ''inspired by'' vs ''homage''',
    'If boundary is strict (synthesis without permission is infringement): extraction expands dramatically and rights enforcement becomes technologically impossible. If boundary is permissive (synthesis is fair use if transformative): extraction declines and emerging creators gain arbitrage exit. Current ambiguity expands suppression through legal uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artificial_likeness_generation, empirical, 'Boundary between protectable likeness and synthetic approximation').

omega_variable(
    cross_border_enforcement_viability,
    'Can likeness rights be effectively enforced across jurisdictions with incompatible legal regimes (strong IP in US, moral rights in EU, weaker enforcement in Asia)?',
    'Analysis of platform enforcement patterns across regions; examination of successful vs failed cross-border licensing agreements; tracking of rights violations that evade jurisdiction',
    'If enforcement is viable: suppression remains high and constraint functions as global snare. If enforcement is fragmented: suppression declines, emerging performers gain regional arbitrage, and constraint degrades to piton (theater without substance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cross_border_enforcement_viability, empirical, 'Cross-border enforceability of likeness rights').

omega_variable(
    fair_use_scope_expansion,
    'Should transformative works (commentary, parody, critique, fan art, educational use) be exempt from likeness rights licensing requirements?',
    'Jurisprudential development through case law; legislative clarification; academic analysis of fair-use precedent in copyright applied to likeness',
    'If fair use expands: creator exit improves, suppression declines, constraint shifts toward rope/scaffold. If fair use contracts: suppression tightens, snare classification strengthens, independent creator mobility declines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_use_scope_expansion, conceptual, 'Scope of fair use exemptions for transformative likeness uses').

omega_variable(
    collective_rights_pool_adequacy,
    'Do collective licensing agencies (performing rights organizations) adequately compensate individual performers and ensure competitive clearance pricing?',
    'Analysis of licensing fee trends over time; comparison of performer compensation to platform revenue; examination of collective agency governance and dispute rates; alternative licensing models (direct licensing, automated licensing).',
    'If collective pools are extractive: victims accumulate (emerging performers) and suppression is enforced by institutional gatekeeping. If collective pools are adequate and transparent: beneficiary-victim asymmetry decreases and constraint approaches rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_rights_pool_adequacy, empirical, 'Whether collective licensing adequately compensates performers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performer_likeness_rights, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plr_tr_t0, performer_likeness_rights, theater_ratio, 0, 0.35).
narrative_ontology:measurement(plr_tr_t10, performer_likeness_rights, theater_ratio, 10, 0.42).
narrative_ontology:measurement(plr_tr_t20, performer_likeness_rights, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(plr_be_t0, performer_likeness_rights, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(plr_be_t10, performer_likeness_rights, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(plr_be_t20, performer_likeness_rights, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performer_likeness_rights, resource_allocation).
narrative_ontology:affects_constraint(performer_likeness_rights, copyright_fair_use_boundary).
narrative_ontology:affects_constraint(performer_likeness_rights, synthetic_media_governance).
narrative_ontology:affects_constraint(performer_likeness_rights, platform_intermediary_liability).

% DUAL FORMULATION NOTE:
% Performer likeness rights decompose into distinct structural constraints: (1) image ownership and licensing (this story), with extractiveness 0.58; (2) synthetic likeness generation boundaries (separate story, higher extractiveness 0.72 due to enforcement impossibility); (3) transformative fair use scope (separate story, extractiveness 0.35 as genuine coordination). These are linked through network affects_constraints but have distinct ε values reflecting different observable dimensions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performer_likeness_rights, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
