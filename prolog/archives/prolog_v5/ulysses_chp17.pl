% ============================================================================
% CONSTRAINT STORY: ulysses_chp17
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_ithaca_1904, []).

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
 *   constraint_id: ulysses_chp17
 *   human_readable: The Mathematical Catechism (7 Eccles Street)
 *   domain: technological/scientific/social
 *
 * SUMMARY:
 *   Chapter 17 of James Joyce's Ulysses (1922) presents Bloom and Stephen's
 *   return to 7 Eccles Street through an exhaustive catechism of questions
 *   and answers—a mathematical, scientific, and precise interrogation of
 *   their encounter. The constraint is the form itself: the demand that
 *   narrative closure, human emotion, and philosophical reflection be
 *   expressed through the cold apparatus of catechistic question-answer. This
 *   creates a structural tension between coordination (the modernist form
 *   enables a new kind of representation) and extraction (the form
 *   systematically displaces emotional authenticity and reader agency). The
 *   chapter's theater ratio has increased over a century: initially, readers
 *   encountered it as a formal innovation; now, it is performed as a
 *   canonical text that readers are expected to revere and struggle through,
 *   rather than genuinely accessed. The constraint exhibits all six types
 *   from different perspectives, making it a diagnostic case for how a single
 *   formal choice can be read as coordination (tradition), scaffolding
 *   (pedagogy), degradation (canon), snare (emotion), rope (modernism), or
 *   natural law (logic).
 *
 * KEY AGENTS:
 *   - Emotional Authenticity: Primary victim (powerless/trapped) — the catechism systematically displaces feeling; no exit from the form's cold precision
 *   - Stephen Dedalus & Leopold Bloom: Primary beneficiaries (institutional/arbitrage) — their encounter is preserved, represented, given structure; narrative priority
 *   - The Reader: Secondary victim (moderate/constrained) — must work through exhaustive catechism; gains clarity but loses emotional satisfaction; some exit via skimming or reinterpretation
 *   - Modernist Literary Tradition: Secondary beneficiary (institutional/arbitrage) — gains formal innovation; can adopt technique elsewhere; has agency
 *   - Literary Criticism & Pedagogy: Organized agents (organized/constrained) — built interpretive frameworks that bypass opacity; created sunset pathways
 *   - The Text as Canonical Object: Institutional actor (institutional/arbitrage) — maintains the chapter through reverence and institutional replication rather than functional communication
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks seeing catechism as necessary to logic rather than as Joyce's chosen constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp17, 0.38).
domain_priors:suppression_score(ulysses_chp17, 0.42).
domain_priors:theater_ratio(ulysses_chp17, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp17, extractiveness, 0.38).
narrative_ontology:constraint_metric(ulysses_chp17, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ulysses_chp17, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp17, tangled_rope).
narrative_ontology:human_readable(ulysses_chp17, "The Mathematical Catechism (7 Eccles Street)").
narrative_ontology:topic_domain(ulysses_chp17, "technological/scientific/social").

domain_priors:requires_active_enforcement(ulysses_chp17).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp17, rational_discourse_community).
narrative_ontology:constraint_beneficiary(ulysses_chp17, stephen_dedalus).
narrative_ontology:constraint_beneficiary(ulysses_chp17, leopold_bloom).
narrative_ontology:constraint_victim(ulysses_chp17, emotional_authenticity).
narrative_ontology:constraint_victim(ulysses_chp17, narrative_closure).
narrative_ontology:constraint_victim(ulysses_chp17, human_agency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMOTIONAL AUTHENTICITY (SNARE) — The catechistic form traps genuine emotional resolution. The reader and narrator cannot exit the constraint of cold, mathematical discourse when seeking closure. Maximum extraction of narrative meaning while suppressing human feeling. Theater-based performance of objectivity masks the emotional content being systematically displaced.
constraint_indexing:constraint_classification(ulysses_chp17, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE READER (TANGLED ROPE) — Constrained by Joyce's formal demand to read the catechism exhaustively, yet benefits from the clarity and precision of the mathematical form. The constraint both enables understanding (coordination function) and denies emotional satisfaction (extraction). Reader has some agency (can skim, can reinterpret) but significant costs (cognitive load, emotional frustration).
constraint_indexing:constraint_classification(ulysses_chp17, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MODERNIST LITERARY TRADITION (ROPE) — The catechism solves the modernist coordination problem: how to represent consciousness without Victorian sentimentality. The tradition benefits from the innovation (arbitrage—can use this technique elsewhere). Low effective extraction because the tradition has agency and can adopt or reject the form.
constraint_indexing:constraint_classification(ulysses_chp17, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LITERARY CRITICISM & PEDAGOGY (SCAFFOLD) — The catechism is a temporary pedagogical scaffold for teaching Modernism. Early 20th-century critics saw it as a form problem; later scholars (1960s onward) developed interpretive frameworks and teaching methods that bypass the form's opacity. The scaffold has a sunset: as critical apparatus matured, the chapter became accessible. Organized agents (universities, scholars) built exit pathways that reduce the constraint's force.
constraint_indexing:constraint_classification(ulysses_chp17, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE TEXT AS CANONICAL OBJECT (PITON) — The catechism persists in syllabi and critical discourse largely through institutional inertia and canonical status, not because it achieves its original function effectively. The form is performed—scholars perform reading it as a sign of literary sophistication—but the actual communicative function has atrophied. Theater ratio (0.68) reflects this degradation: much performative reverence, diminished actual reading.
constraint_indexing:constraint_classification(ulysses_chp17, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LOGICAL NECESSITY VIEW (MOUNTAIN) — From a civilizational/universal perspective, the catechism represents an irreducible logical constraint: the question-answer form is the fundamental structure of rational discourse itself. This perspective risks naturalizing a specific rhetorical choice (catechism) as a necessity of logic. The engine will detect this as a false summit—the logical form is available but not mandatory; Joyce chose it to extract meaning-making power from readers.
constraint_indexing:constraint_classification(ulysses_chp17, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp17_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp17, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp17, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp17, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp17_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The catechism extracts interpretive labor from the reader and displaces emotional closure, but it is not maximally extractive because the form is transparent (readers know exactly what is happening) and the modernist tradition validates the choice retrospectively. The extraction is justified as innovation, which reduces its perceived severity. Initial value (0.22) reflects that contemporary readers may have experienced it as pure formal experimentation; current value (0.38) reflects the century of canonical enforcement and critical performance. Suppression (0.42): Moderate. The catechistic form suppresses emotional expression, narrative momentum, and authorial intrusion—but the suppression is visible, not hidden. Readers understand why emotion is excluded. This is different from deceptive suppression (snare level); it is acknowledged constraint. Theater ratio (0.68): Moderately high. The catechism has become substantially performative over time. Early modernist readers engaged with the form as innovation; contemporary readers perform engagement with it as a canonical requirement. The gap between genuine reading and institutional performance has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification from the same formal data. The modernist tradition sees coordination (Rope)—they benefit from formal innovation with no cost. Literary pedagogy sees a temporary problem being solved (Scaffold)—critical apparatus built exit pathways that reduced opacity over generations. The canon sees a degraded ritual (Piton)—the form persists through reverence, not function. The reader sees mixed coordination and extraction (Tangled Rope)—clarity of structure gained at the cost of emotional satisfaction. Emotional authenticity sees pure extraction (Snare)—the form systematically displaces feeling with no alternative pathway. The analytical observer sees an immutable logical necessity (Mountain)—catechism as the fundamental structure of rational discourse—but the structural data reveals this as a false summit: the constraint is Joyce's chosen form, not a law of logic.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) is determined by their structural position relative to the catechism constraint. Emotional authenticity is powerless and trapped (d ≈ 0.95)—it bears extraction with no exit. Stephen and Bloom are institutional beneficiaries with arbitrage options (d ≈ 0.05)—they control the narrative frame and can deploy the form elsewhere. The reader is moderate power with constrained exit (d ≈ 0.55)—they can skip or reinterpret, but must work through the form to engage the text meaningfully. The modernist tradition is institutional with arbitrage (d ≈ 0.15)—the form is an innovation they can adopt or reject. Literary pedagogy is organized with constrained exit (d ≈ 0.50)—they built frameworks to bypass opacity, but the text remains canonical and non-negotiable. The piton perspective derives from the theater gate rather than from high effective extraction (χ). The mountain perspective at the analytical context is perspectival—the engine's false summit detector identifies it as naturalization of a contingent formal choice.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR FOR FORMAL CONSTRAINT ANALYSIS: This constraint resolves the mandatrophy by showing that all six types are legitimate perspectival readings of the same formal data. The mandatrophy is not 'which type is correct?' but 'which agent-position are you measuring from?' The analytical observer's mountain is a false summit (naturalized choice as logical necessity). The beneficiary's rope is their genuine experience (formal innovation with arbitrage). The pedagogical scaffold is a real structural feature (interpretive frameworks building sunset pathways). The piton is a real observation (canonical performance of engagement replacing actual communication). The snare is the emotional authenticity's structural reality (displacement with no exit). The tangled rope is the reader's mixed experience (structure gained, feeling lost). The theater ratio progression (0.35 → 0.68) reveals Goodhart drift: as the form became canonical, institutional performance of engagement increased while actual meaning-making decreased. The ratio crossing 0.50 at time-point 5 marks the shift from form-as-innovation to form-as-performance. No single type is 'the' answer—the presheaf of perspectives over the textual site IS the answer. The catechism cannot be resolved into a single classification; it is legitimately all six types simultaneously, viewed from different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emotional_suppression_mechanism,
    'Is the cold mathematical form suppressing emotion structurally, or does the suppression depend on the reader''s interpretive stance?',
    'Analysis of textual emotional cues embedded in the catechism (numbers as containers of feeling, precise measurements of longing); comparison with reader-response data from those who experience the chapter as emotionally powerful vs. those who find it cold',
    'If structural: suppression is a property of the form itself (ε remains ~0.38). If stance-dependent: suppression is observer-relative (ε could range 0.15–0.55 depending on reading practice). If hybrid: the tangled rope classification holds; different agents experience different suppression levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emotional_suppression_mechanism, conceptual, 'Whether emotional suppression is inherent to catechistic form or depends on reading practice').

omega_variable(
    coordination_vs_extraction_boundary,
    'Does the catechism coordinate a new literary technique (Modernist innovation) or primarily extract interpretive labor from the reader?',
    'Genealogical analysis: did subsequent authors adopt the catechistic form for its communicative benefits, or did they treat it as a solved problem to move beyond? Count of catechism-form works in post-1922 Modernism vs. other formal innovations.',
    'If coordination-dominant: the constraint is a Rope from the tradition''s perspective, not Tangled Rope. If extraction-dominant: the beneficiary (Joyce''s reputation, critical apparatus that grew around interpretation) extracted value at reader cost; classification shifts toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether catechism coordinates literary innovation or primarily extracts reader labor').

omega_variable(
    critical_apparatus_sufficiency,
    'Has the accumulated pedagogical and critical apparatus made the catechism genuinely accessible, or does it merely perform accessibility while maintaining core opacity?',
    'Longitudinal study: do students with full critical apparatus experience the catechism as comprehensible or as still-opaque-but-justified? Are the interpretations concordant or divergent across the critical tradition?',
    'If genuinely accessible: the scaffold sunset is real—the constraint''s force has declined as teaching methods matured. If performed accessibility: the piton classification strengthens—critical reverence masks continued dysfunction, and the constraint persists through theater rather than function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_apparatus_sufficiency, empirical, 'Whether critical apparatus has made the catechism genuinely accessible or merely performed accessibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp17, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulys_tr_t0, ulysses_chp17, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ulys_tr_t5, ulysses_chp17, theater_ratio, 5, 0.52).
narrative_ontology:measurement(ulys_tr_t10, ulysses_chp17, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(ulys_be_t0, ulysses_chp17, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ulys_be_t5, ulysses_chp17, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(ulys_be_t10, ulysses_chp17, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp17, information_standard).
narrative_ontology:affects_constraint(ulysses_chp17, modernist_opacity).
narrative_ontology:affects_constraint(ulysses_chp17, reader_interpretation_labor).
narrative_ontology:affects_constraint(ulysses_chp17, canonical_performance).

% DUAL FORMULATION NOTE:
% The catechism as formal choice is a constraint on narrative meaning-making. The chapter's modernist context (information_standard coordination) enables readers to understand it as innovation rather than mere obstruction. Upstream constraints (modernist convention of rejecting Victorian sentimentality) justify the downstream constraint (catechistic suppression of emotion). Downstream constraints (canonical reverence, pedagogical scaffolding) show how a formal innovation becomes a degraded institutional ritual.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ulysses_chp17, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
