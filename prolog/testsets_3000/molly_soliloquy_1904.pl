% ============================================================================
% CONSTRAINT STORY: molly_soliloquy_1904
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_molly_soliloquy_1904, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: molly_soliloquy_1904
 *   human_readable: Molly Soliloquy: Epistemic Containment and Narrative Authority in 1904 Literature
 *   domain: literary_studies/narrative_theory
 *
 * SUMMARY:
 *   The Molly soliloquy represents a structural constraint on narrative
 *   representation operating in early 20th-century literary discourse. The
 *   constraint manifests as critical and market gatekeeping that suppresses
 *   certain forms of interiority representation (particularly unconstrained
 *   female consciousness) while simultaneously extracting value from their
 *   eventual legitimation. The constraint exhibits a dual structure: it
 *   coordinates readers around recognizable narrative forms (coordination
 *   function) while simultaneously extracting epistemic and commercial value
 *   from practitioners who innovate beyond those forms (extraction function).
 *   The suppression is high (0.65) because alternative narrative forms are
 *   actively delegitimized, not merely difficult. The theater ratio is high
 *   (0.80) because the critical establishment performs legitimacy judgments
 *   through elaborate justifications (incoherence, unmotivated form,
 *   technical failure) that obscure what is actually a gatekeeping mechanism.
 *   The extractiveness is moderate (0.35) because the constraint operates
 *   through reputational and market mechanisms rather than through direct
 *   coercive force.
 *
 * KEY AGENTS:
 *   - Literary Establishment: Institutional beneficiary (institutional/arbitrage) — publishers, critics, canonical gatekeepers who benefit from constraining narrative form to recognizable patterns and maintaining interpretive authority
 *   - Female Interiority Discourse: Primary victim (powerless/trapped) — the form of consciousness being suppressed; cannot advocate for its own representability or organize exit from the constraint
 *   - Literary Practitioners: Secondary victims (moderate/constrained) — writers constrained by market conventions and critical apparatus; bear career and reputational risk for formal innovation
 *   - Modernist Coalition: Organized actors (organized/mobile) — avant-garde movements and alternative publication networks building exit pathways through form-breaking and audience expansion
 *   - Critical Canon: Institutional maintenance agent (institutional/constrained) — performs gatekeeping function; increasingly operates through inertia as explanatory power decreases
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent literary-historical arrangements as features of language itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(molly_soliloquy_1904, 0.35).
domain_priors:suppression_score(molly_soliloquy_1904, 0.65).
domain_priors:theater_ratio(molly_soliloquy_1904, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(molly_soliloquy_1904, extractiveness, 0.35).
narrative_ontology:constraint_metric(molly_soliloquy_1904, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(molly_soliloquy_1904, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(molly_soliloquy_1904, tangled_rope).
narrative_ontology:human_readable(molly_soliloquy_1904, "Molly Soliloquy: Epistemic Containment and Narrative Authority in 1904 Literature").
narrative_ontology:topic_domain(molly_soliloquy_1904, "literary_studies/narrative_theory").

domain_priors:requires_active_enforcement(molly_soliloquy_1904).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(molly_soliloquy_1904, literary_establishment).
narrative_ontology:constraint_beneficiary(molly_soliloquy_1904, narrative_gatekeepers).
narrative_ontology:constraint_victim(molly_soliloquy_1904, interiority_representation).
narrative_ontology:constraint_victim(molly_soliloquy_1904, female_subjectivity_discourse).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNRECORDED INTERIOR (SNARE) — Female interiority trapped in a representational bottleneck. The constraint prevents authentic rendering of female consciousness from being recognized as such within dominant literary discourse. Full extraction from the perspective of what cannot be said or credited.
constraint_indexing:constraint_classification(molly_soliloquy_1904, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LITERARY PRACTITIONER (TANGLED ROPE) — Writer constrained by market conventions and critical apparatus, yet also coordinating with readers through shared narrative structures. Genuine coordination function (readers understand the form) combined with asymmetric extraction (practitioners bear career risk for formal innovation).
constraint_indexing:constraint_classification(molly_soliloquy_1904, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LITERARY ESTABLISHMENT (ROPE) — Publishers, critics, and canonical gatekeepers benefit from constraining narrative form to recognizable patterns. Experience this as pure coordination: maintaining standards and coherence. Net beneficiary through preservation of interpretive authority.
constraint_indexing:constraint_classification(molly_soliloquy_1904, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CRITICAL CANON (PITON) — Once-functional gatekeeping mechanism (distinguishing serious literature from pulp) now operates primarily as theatrical maintenance of cultural authority. Canon selection persists through institutional inertia despite reduced explanatory power.
constraint_indexing:constraint_classification(molly_soliloquy_1904, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MODERNIST COALITION (SCAFFOLD) — Organized agents (avant-garde movements, small presses, alternative publication networks) recognize the constraint as a temporary coordination failure with built-in sunset. Sees path through form-breaking and audience expansion. High agency, clear exit trajectory via parallel literary institutions.
constraint_indexing:constraint_classification(molly_soliloquy_1904, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From civilizational distance, the constraint appears inherent to language itself: some forms of consciousness are structurally difficult to represent in conventional narrative. The limit appears as a feature of linguistic capacity, not of institutional gatekeeping. This perspective risks false-summit classification by naturalizing contingent editorial and market constraints as immutable linguistic limits.
constraint_indexing:constraint_classification(molly_soliloquy_1904, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(molly_soliloquy_1904_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(molly_soliloquy_1904, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(molly_soliloquy_1904, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(molly_soliloquy_1904, TR),
    TR >= 0.70.

:- end_tests(molly_soliloquy_1904_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint extracts epistemic authority (interpretation belongs to gatekeepers) and commercial value (initial suppression followed by prestige capture when forms are later legitimated). However, the extraction is not absolute — alternative markets exist (small presses, serialization, manuscript circulation), and the constraint weakens over time as audiences expand. Suppression (0.65): High. Alternative narrative forms are not merely difficult but actively delegitimized. Critical vocabulary frames unmarked interior monologue as incoherent, unmotivated, or technically failed rather than as innovative. Publication markets reject such forms. This is active suppression, not passive difficulty. Theater ratio (0.80): High and increasing. The critical establishment justifies its gatekeeping through elaborate aesthetic arguments that perform legitimacy testing while obscuring the underlying reputational gatekeeping. As the constraint ages, theater increases: critics maintain the form even as its explanatory power decreases, keeping the machinery running through inertia. Measurements show theater rising from 0.65 to 0.80 over the interval, indicating increasing performativity relative to function.
 *
 * PERSPECTIVAL GAP:
 *   The literary establishment experiences this as pure coordination (Rope) — they are maintaining standards and enabling readers to understand complex narratives. Female interiority experiences it as pure extraction (Snare) — the form cannot represent itself or advocate for its own validity. Literary practitioners experience mixed coordination and extraction (Tangled Rope) — the constraint both enables communication with readers (readers share the form-understanding) and punishes innovation. The modernist coalition experiences it as temporary and solvable (Scaffold) — alternative institutions and audience expansion create an exit path. The critical canon increasingly experiences itself as degraded (Piton) — gatekeeping persists through institutional inertia despite reduced explanatory power. The civilizational analytical observer risks naturalizing this as inherent to language (Mountain) — assuming some interiority is structurally difficult to represent rather than recognizing it as socially gatekept.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (literary establishment) experience low directionality: they have arbitrage options, institutional power, and the constraint runs toward them. Practitioners experience moderate directionality: constrained exit (market dependency, reputation risk) but also some mobility (alternative forms, niche audiences). Female interiority experiences maximum directionality: fully trapped, no exit options, extraction runs away from it. The analytical observer derives d from their structural position as observer rather than beneficiary or victim — they see the full system but risk naturalizing it. Overrides would be needed only if an institutional actor claimed to be trapped within the constraint despite having arbitrage options, or if a victim claimed arbitrage mobility despite actual market barriers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    representational_authenticity_threshold,
    'What constitutes authentic rendering of interior consciousness versus stylistic performance that mimics authenticity?',
    'Historical analysis of reader reception; comparison of works rejected as incoherent vs later works using similar techniques that were accepted; genealogical study of which formal innovations were deemed illegible when published vs recognizable retroactively',
    'If threshold is low (readers readily recognize authentic interiority in any form): constraint is primarily extractive gatekeeping. If threshold is high (some interiority is genuinely difficult to represent): constraint has legitimate coordination function (audience coordination on meaning-making)',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(representational_authenticity_threshold, conceptual, 'Whether representational difficulty is linguistic or institutional').

omega_variable(
    female_subjectivity_discourse_boundary,
    'Is the constraint specifically targeting female interiority or does it suppress all unmarked interior monologue equally?',
    'Comparative analysis of critical reception for male vs female interior monologue across the same period; tracking of which authors are deemed experimental vs illegible; analysis of gendered language in rejection letters and critical responses',
    'If equally suppressed: constraint is primarily linguistic (scaffolding for a new narrative technique). If gendered suppression: constraint includes identity-based extraction targeting female subjectivity specifically',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(female_subjectivity_discourse_boundary, empirical, 'Gender-specificity of narrative suppression').

omega_variable(
    market_versus_aesthetic_extraction,
    'Is the suppression driven by market forces (unrecognizable = unsellable) or by aesthetic gatekeeping (unrecognizable = invalid)?',
    'Analysis of which agents benefit from suppression: publishers profit from safe forms; critics profit from interpretive authority over canonical works. Distinguish whether extraction concentrates in commercial tier or in prestige tier',
    'If market-driven: scaffold sunset is real (authors can publish outside markets). If prestige-gated: suppression persists even when alternative markets exist (prestige concentration remains extractive)',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_versus_aesthetic_extraction, empirical, 'Whether extraction driver is economic or epistemic authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(molly_soliloquy_1904, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(moll_tr_t0, molly_soliloquy_1904, theater_ratio, 0, 0.65).
narrative_ontology:measurement(moll_tr_t5, molly_soliloquy_1904, theater_ratio, 5, 0.73).
narrative_ontology:measurement(moll_tr_t10, molly_soliloquy_1904, theater_ratio, 10, 0.8).

% Extraction over time
narrative_ontology:measurement(moll_be_t0, molly_soliloquy_1904, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(moll_be_t5, molly_soliloquy_1904, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(moll_be_t10, molly_soliloquy_1904, base_extractiveness, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(molly_soliloquy_1904, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
