% ============================================================================
% CONSTRAINT STORY: ulysses_chp13
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp13, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ulysses_chp13
 *   human_readable: The Sentimental Snare (Sandymount Shore)
 *   domain: social/artistic/psychological
 *
 * SUMMARY:
 *   Chapter 13 of Joyce's *Ulysses* presents an encounter between Gerty
 *   MacDowell (a young woman of Dublin's lower-middle class) and Leopold
 *   Bloom (the novel's protagonist, a wandering Jewish advertising canvasser)
 *   on Sandymount Strand. The chapter operates under a dual narrative regime:
 *   the first half is narrated in the sentimental, clichéd voice of women's
 *   popular literature; the second half shifts to Bloom's interior monologue.
 *   The constraint is not primarily the characters' interaction but the
 *   sentimental literary form itself — a form that both characters inhabit,
 *   that shapes their desires and self-perceptions, and that extracts their
 *   authentic feelings in exchange for narratively prescribed emotional
 *   experiences. Gerty is trapped in the archetype of the romantic heroine;
 *   Bloom is trapped in the archetype of the romantic hero. Neither can exit
 *   without violating the form's rules, yet the form itself is the source of
 *   their entrapment. The chapter is Joyce's modernist interrogation of
 *   sentimentality as a social technology for managing desire — a technology
 *   that appears consensual but operates through suppression and
 *   manipulation.
 *
 * KEY AGENTS:
 *   - Gerty MacDowell: Primary victim (powerless/trapped) — ensnared by sentimental literary conventions that shape her self-perception and desire; cannot exit without social/narrative transgression
 *   - Leopold Bloom: Secondary victim (moderate/constrained) — seduced and used as mirror/validation object within Gerty's sentimental fantasy; constrained by his own desire and social isolation
 *   - The Sentimental Literary Form: Enforcer (institutional/implicit) — not an agent but a structural mechanism that suppresses authentic desire and substitutes narrative formula; maintenance through both characters' complicity
 *   - The Analytical Reader: Observer (analytical/analytical) — positioned to see the constraint structure; Joyce's ironic narrative voice implicates the reader in the extraction process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp13, 0.58).
domain_priors:suppression_score(ulysses_chp13, 0.68).
domain_priors:theater_ratio(ulysses_chp13, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp13, extractiveness, 0.58).
narrative_ontology:constraint_metric(ulysses_chp13, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ulysses_chp13, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp13, snare).
narrative_ontology:human_readable(ulysses_chp13, "The Sentimental Snare (Sandymount Shore)").
narrative_ontology:topic_domain(ulysses_chp13, "social/artistic/psychological").

% --- Structural relationships ---
narrative_ontology:constraint_victim(ulysses_chp13, gerty_macdowell).
narrative_ontology:constraint_victim(ulysses_chp13, leopold_bloom).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GERTY MACDOWELL (SNARE) — Trapped by sentimental literary conventions, feminine propriety, and the narrative voice's appropriation of her desire. She experiences no genuine exit: abandonment of propriety means social ruin; acceptance means self-erasure into romance narrative. d≈0.96, f(d)≈1.41, σ=0.8 → χ≈0.65.
constraint_indexing:constraint_classification(ulysses_chp13, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: LEOPOLD BLOOM (SNARE) — Constrained by solitude, lust, and the narrative's use of his desire to drive the scene. The constraint extracts his attention and erotic energy but offers no genuine reciprocity; Gerty uses him as a mirror for her own sentimental fantasy. d≈0.88, f(d)≈1.32, σ=0.8 → χ≈0.62.
constraint_indexing:constraint_classification(ulysses_chp13, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: ANALYTICAL READER (SNARE) — From the perspective of textual/aesthetic analysis, Chapter 13 traps both characters and reader within a sentimental literary form that obscures the actual extraction occurring. The narrative voice (which mimics women's popular literature) is itself the snare mechanism: it enforces suppression of genuine desire and self-knowledge. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.67.
constraint_indexing:constraint_classification(ulysses_chp13, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp13_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp13, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ulysses_chp13_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The sentimental form extracts authentic emotional experience from both characters and substitutes narratively prescribed affect. Gerty's genuine loneliness and physical desire are channeled into sentimental fantasy archetypes (the wounded heroine, the mysterious stranger, the redemptive romance). Bloom's genuine isolation and sexual need are channeled into role-play as the understanding gentleman caller. The extraction is not totalizing — both characters retain some self-awareness — but it is systematic and enforced. The value reflects that sentimentality is a partial constraint: there are moments of authenticity breaking through, but the form reasserts itself. Suppression (0.68): High. Multiple suppression mechanisms: (1) Social propriety — both characters face severe consequences for transgression. (2) Narrative convention — the sentimental form actively suppresses non-sentimental modes of expression. (3) Psychological — both characters are partly unaware of being trapped; they have internalized the form's values. (4) Linguistic — the vocabulary available to Gerty in particular is the clichéd vocabulary of sentimental literature; more authentic expression lacks culturally available words. Theater ratio (0.81): Very high, and increasing. The encounter is substantially performative. Both characters are enacting roles from sentimental literature. Gerty performs wounded heroine (mysterious malady in her gait, the enigmatic smile). Bloom performs mysterious gentleman (the knowing nod, the symbolic gesture). Even their internal monologues are infiltrated by sentimental clichés. The theater increases over the encounter as the narrative voice's sentimental tone intensifies (Goodhart drift: the form reinforces itself). By the end, authentic feeling is almost entirely displaced by narrative performance.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives classify the constraint as snare, but with different grounds and implications. Gerty's perspective (powerless/trapped) experiences maximum coercion: she has internalized the sentimental form so thoroughly that transgression is psychologically unavailable to her. Bloom's perspective (moderate/constrained) experiences the snare as seduction: he chooses to participate, but the choice is made under conditions of isolation and desperation that render it less than free. The analytical reader's perspective (civilizational) experiences the snare as a historical form: sentimental literature is a mechanism of social control that appears natural and inevitable from within but is exposed as contingent and manipulative from outside. No gap toward lighter classifications (rope, scaffold) emerges because the constraint has no genuine coordination function — sentimentality is pure extraction of authentic affect, performed as though it were reciprocal.
 *
 * DIRECTIONALITY LOGIC:
 *   Gerty MacDowell: Victim + trapped → d≈0.96. Near-maximum directionality toward target. She is deeply embedded in the sentimental form and has no cultural resources to exit. Her desire has been colonized by the form. Bloom: Victim + constrained → d≈0.88. High directionality toward target but not maximum — he has some capacity to step outside the fantasy (which he does when the tone shifts to his monologue), but he is constrained by his loneliness and desire. The constraint extracts his attention and sexual energy. Analytical Reader: Observer position → d≈0.72. The reader is complicit: Joyce's ironic narrative voice seduces the reader into the sentimental form precisely so the reader can experience being trapped. The reader becomes a third victim of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY: This constraint tests whether a form/narrative mechanism can be classified as snare even when the victims appear to participate willingly. Classical mandatrophy would ask: Is this coordination (rope) with dramatic irony, or extraction (snare) masquerading as coordination? The answer: it is pure extraction precisely because it appears and feels like coordination. Neither Gerty nor Bloom experiences the encounter as extraction — they experience it as romantic connection, as being understood, as mutual validation. Yet the narrative irony (which Joyce enforces through stylistic pastiche) reveals that they are enacting roles, that their authenticity is being displaced by formula, that the apparent connection is illusory. The mandatrophy is resolved by distinguishing between the subjective experience (coordination-like) and the structural function (extraction). The sentimental form extracts by making the extraction invisible — by making it feel voluntary and reciprocal. This is the essence of a snare: a trap that the victim does not recognize as such, that may feel pleasant or desirable, but that systematically prevents exit and suppresses alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gerty_agency_boundary,
    'To what degree is Gerty a victim of the sentimental literary form versus an active agent using the form to manipulate Bloom?',
    'Stylistic analysis of narrative voice shifts; textual markers of Gerty''s self-awareness vs. her immersion in sentimental cliché; biographical evidence from Joyce''s later reflections on the chapter',
    'If Gerty is primarily victim: snare classification holds. If Gerty is partly agent/manipulator: classification shifts toward tangled_rope (mixed extraction/coordination). If Gerty is primarily agent: constraint relocates to Bloom''s perspective (making it less symmetric).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gerty_agency_boundary, conceptual, 'Gerty''s agency boundary within sentimental form').

omega_variable(
    bloom_complicity_threshold,
    'Is Bloom''s participation in the scene a seduction (victim) or a willing indulgence (beneficiary) that he later regrets?',
    'Textual markers of Bloom''s self-deception; his internal monologue before and after the encounter; narrative irony density in the Bloom sections',
    'If Bloom is seduced victim: snare from his perspective holds. If Bloom is willing beneficiary who later rationalizes: classification becomes rope (coordination with moral shadow). If Bloom is both simultaneously: tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bloom_complicity_threshold, conceptual, 'Whether Bloom is seduced victim or willing participant').

omega_variable(
    naturalization_of_sentiment,
    'Is sentimental literary form an immutable constraint (mountain-like) on how desire can be expressed, or is it a historical contingency (snare) that could be otherwise?',
    'Historical-philological: comparison with non-sentimental narrative modes available in 1904 literature; analysis of Joyce''s deliberate choice of sentimental pastiche as critique vs. inevitability',
    'If form is natural/inevitable: some aspects of snare should reclassify as mountain (immutable communication limit). If form is chosen/contingent: snare classification is correct; Joyce exposes the form as trap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturalization_of_sentiment, conceptual, 'Whether sentimental form is natural or contingent constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp13, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_early_encounter, ulysses_chp13, theater_ratio, 0, 0.65).
narrative_ontology:measurement(theater_mid_encounter, ulysses_chp13, theater_ratio, 5, 0.78).
narrative_ontology:measurement(theater_late_encounter, ulysses_chp13, theater_ratio, 10, 0.81).

% Extraction over time
narrative_ontology:measurement(extract_early_encounter, ulysses_chp13, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(extract_mid_encounter, ulysses_chp13, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(extract_late_encounter, ulysses_chp13, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp13, information_standard).
narrative_ontology:affects_constraint(ulysses_chp13, ulysses_molly_soliloquy).
narrative_ontology:affects_constraint(ulysses_chp13, ulysses_circe_episode).

% DUAL FORMULATION NOTE:
% The sentimental snare is downstream of broader constraints in how desire and narrative are mediated in early-20th-century literary culture. Upstream constraints include the publishing industry's preference for sentimental plots and the social prohibition on explicit discussion of female sexuality. The snare is the mechanism by which these upstream constraints become internalized in the characters' self-perception.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
