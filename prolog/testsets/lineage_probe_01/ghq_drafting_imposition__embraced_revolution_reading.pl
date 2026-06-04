% ============================================================================
% CONSTRAINT STORY: ghq_drafting_imposition__embraced_revolution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ghq_drafting_imposition__embraced_revolution_reading, []).

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
 *   constraint_id: ghq_drafting_imposition__embraced_revolution_reading
 *   human_readable: GHQ Drafting Imposition — Embraced Revolution Reading
 *   domain: legal/constitutional/postcolonial
 *
 * SUMMARY:
 *   The constraint models a specific claim about constitutional legitimacy
 *   grounded in postwar Japanese constitutional experience. The 1947
 *   Constitution was drafted by occupying-power legal advisors (primarily
 *   MacArthur's Government Section) in six days, in English, with minimal
 *   Diet participation in the initial draft. The imposed constitution
 *   critique (one of the three readings in the kernel contest) argues this
 *   origin disqualifies the text's legitimacy — authentic constitutionalism
 *   requires indigenous authorship, and occupation documents carry structural
 *   constraints on future possibility regardless of current content merit.
 *   The embraced revolution reading counters: the text's legitimacy was not
 *   settled in 1947 but constituted through subsequent practice. The people —
 *   through the Diet, courts, and popular politics — sustained the text
 *   through seven decades WITHOUT AMENDMENT, despite supermajorities, regime
 *   changes, pressure from conservative revisionists, and evolving social
 *   circumstances. This sustained refusal to amend is not passive acceptance
 *   but active ratification: the people had the power to revise and chose not
 *   to. Each failed amendment attempt, each moment the Diet declined to call
 *   a constitutional convention, each generation's choice to interpret within
 *   the inherited frame rather than formally revise it — these acts
 *   CONSTITUTE the people's ownership of the text. The imposition is cured
 *   not by what happened in 1947 (hybrid authorship reading) but by what has
 *   happened since: seven decades of choosing to keep the constitution
 *   unchanged. The constraint's extractiveness declines over time (0.55 at
 *   t=0 to 0.18 at t=70) because the imposition argument is suppressed by
 *   mounting evidence of sustained acceptance. Theater_ratio also declines
 *   (0.62 to 0.48) because the original performative defense of legitimacy
 *   ("we must obey because occupation dictates") gives way to genuine popular
 *   ownership (interpretive practice constitutes legitimacy without
 *   theatrical assertion). The beneficiary is the constitution's living
 *   legitimacy — each generation's choice to sustain the text instead of
 *   revising it. The victim set that this reading dismisses is
 *   origin-essentialism (the claim that an occupied constitution cannot be
 *   legitimate regardless of time or content). By characterizing origin-focus
 *   as essentialism rather than as substantive concern, this reading
 *   dissolves the imposition claim.
 *
 * KEY AGENTS:
 *   - The Constitutional Text Itself: Neutral substrate (fixed_text authority grounding) — the constraint coordinates through shared reference to the inherited 1947 frame
 *   - The Interpreting Judiciary and Constitutional Community: Institutional beneficiary (institutional/arbitrage) — access to authoritative interpretive tradition; legitimate authority over constitutional meaning
 *   - The Diet and Legislative Bodies: Institutional actor (organized/constrained) — could amend the constitution but repeatedly chose not to; this refusal is the reading's core evidence of ratification
 *   - Successive Postwar Generations: Moderate agent (moderate/biographical) — experience both coordination (common legal frame) and constraint (inability to formally revise); bear extraction through path-locked choices of earlier generations
 *   - Constitutional Revisionists: Moderate actor (moderate/constrained) — seek amendment but constrained by supermajority requirements and interpretive sufficiency of existing text; partially victimized by the text's fixity
 *   - The Imposed Constitution Critique (as agent position): Victim in this reading's frame (identity_locked to origin-essentialism) — held to be essentialism (fixated on occupation origin) rather than legitimate concern about constitutional constraint; reading dismisses the critique rather than engaging it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ghq_drafting_imposition__embraced_revolution_reading, 0.18).
domain_priors:suppression_score(ghq_drafting_imposition__embraced_revolution_reading, 0.35).
domain_priors:theater_ratio(ghq_drafting_imposition__embraced_revolution_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ghq_drafting_imposition__embraced_revolution_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(ghq_drafting_imposition__embraced_revolution_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(ghq_drafting_imposition__embraced_revolution_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ghq_drafting_imposition__embraced_revolution_reading, rope).
narrative_ontology:human_readable(ghq_drafting_imposition__embraced_revolution_reading, "GHQ Drafting Imposition — Embraced Revolution Reading").
narrative_ontology:topic_domain(ghq_drafting_imposition__embraced_revolution_reading, "legal/constitutional/postcolonial").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ghq_drafting_imposition__embraced_revolution_reading, constitutional_legitimacy_through_sustained_acceptance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUTIONAL TEXT AS COORDINATION MECHANISM (ROPE) — The fixed English-language text functions as a coordination device: courts, legislatures, citizens, and commentators all interpret against the same inherited frame. The fixity enables common reference across seven decades. This perspective experiences the constraint as pure coordination (extractiveness low, χ ≤ 0.35). The text's origin matters less than its current function as shared interpretive anchor.
constraint_indexing:constraint_classification(ghq_drafting_imposition__embraced_revolution_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERPRETING JUDICIARY / CONSTITUTIONAL COMMUNITY (ROPE) — Judges, commentators, and legislative bodies treat the constitution as a legitimate frame to interpret around (rather than through formal amendment). The sustained interpretive practice CONSTITUTES legitimacy: each successful interpretation that adapts the text to new circumstances cures the imposition claim. The constraint is coordination through shared authority over the inherited text. Net beneficiary — access to authoritative interpretive tradition.
constraint_indexing:constraint_classification(ghq_drafting_imposition__embraced_revolution_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / LEGITIMACY THROUGH TEMPORAL STABILITY (MOUNTAIN) — From a civilizational analytical perspective, this reading treats sustained refusal to amend — across party rotations, regime pressures, and multiple founding generation deaths — as establishing an irreducible natural law of constitutional legitimacy: legitimacy emerges from the people's choice to keep the text unchanged, not from its origin. The seven-decade unamended status becomes constitutive of authority itself. No amendment capacity can revise this (short of a new founding moment), making it appear as a mountain. However, the schema recognizes this as a potential false summit: the appearance of immutability might actually derive from contingent institutional structures (amendment supermajorities, path dependence, veto coalitions) rather than from natural law.
constraint_indexing:constraint_classification(ghq_drafting_imposition__embraced_revolution_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: ORDINARY CITIZEN / CONSTITUTIONAL SUBJECT (TANGLED ROPE) — The citizen experiences coordination (common legal framework applying equally) but also constraint: the inability to formally revise the text means revision can only happen through reinterpretation, which the citizen cannot directly control. Some extraction occurs — the fixed text locks in earlier-generation choices that later generations did not explicitly consent to. But the extraction is limited by the fact that the text genuinely does coordinate behavior and provide predictability. The citizen benefits from the coordination while constrained by the inability to formally amend.
constraint_indexing:constraint_classification(ghq_drafting_imposition__embraced_revolution_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ghq_drafting_imposition__embraced_revolution_reading_tests).
:- end_tests(ghq_drafting_imposition__embraced_revolution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. This reading presents the sustained unamended status as a curing mechanism, not an extraction mechanism. The constraint does not extract from the people because the people CHOSE to sustain the text — they had amendment power and exercised the choice not to use it. Extractiveness declines over time (0.55→0.18) because the imposition argument loses force as sustained acceptance mounts. By t=70, the text's fixity is evidence of legitimacy, not of constraint. Suppression (0.35): Moderate. The imposition critique is suppressed — silenced by the reading's reframing of amendment refusal as ratification. But suppression is not total: the critique persists in academic discourse, in periodic revival proposals, and in comparative constitutional analysis. The reading suppresses it by reframing its terms (origin-essentialism vs. legitimate concern) rather than by institutional coercion. Theater_ratio (0.48): Moderate-low. This reading requires some performative assertion (the claim that refusal-to-amend constitutes active ratification is not transparent on the surface — it requires interpretive work). But less theater than the imposed constitution critique, which must perform legitimacy claims against the obvious fact of occupation. The declining theater ratio reflects that the constraint's legitimacy becomes increasingly self-evident as generations accumulate: it becomes harder and harder to deny that the people could have revised and chose not to.
 *
 * PERSPECTIVAL GAP:
 *   The interpretive judiciary sees pure coordination (Rope) — the fixed text enables legitimate authority and interpretive tradition. The moderate citizen sees mixed coordination and constraint (Tangled Rope) — they benefit from the common legal frame but cannot formally revise it. The origin-essentialist critic (represented in this reading as the imposed constitution perspective) sees structural constraint or snare — the occupation-drafted text locks in occupation-era choices. This reading dissolves the perspectival gap by arguing the critic is mistaken about what legitimacy requires: not origin purity but sustained acceptance. The gap persists in the corpus because the hybrid authorship reading and the imposed constitution critique offer competing accounts of whether the imposition was ever real (hybrid: no, Japanese participated; critique: yes, and participation does not cure occupation) or can be cured (embrace: yes, through sustained refusal to amend; critique: no, occupation constraints persist).
 *
 * DIRECTIONALITY LOGIC:
 *   This reading involves institutional beneficiaries (the judiciary, constitutional community, legislative bodies that benefit from authoritative interpretive access to the fixed text) and moderate agents (ordinary citizens who benefit from coordination but are constrained by inability to formally revise). The imposition argument is cast as originating from origin-essentialism (not from structural analysis of constraint), so there is no identified victim group in the structural sense — only a dismissed interpretive position. The constraint appears as pure coordination (Rope) from the institutional perspective because the fixed text delivers genuine coordination benefits. From the moderate/biographical perspective, it appears as Tangled Rope because the citizen benefits from coordination but is constrained by the text's fixity. The analytical perspective risks mountain classification (legitimacy through sustained acceptance is an immutable law of constitutional development), but this reading's own schema recognizes the falsifiability condition: the mountain classification depends on whether amendment refusal tracks path-dependent institutional factors or genuinely constitutes active ratification. If path dependence explains the fixity, the mountain collapses into rope or snare (the text persists not because the people chose it but because formal revision is institutionally hard).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by showing that a constraint can be cured through sustained acceptance rather than through formal revision or removal. The original imposition (high extractiveness, high suppression of alternatives to the occupation draft) is gradually replaced by voluntary coordination as generations choose to sustain the text. The tension between "coordination mechanism" and "extracted constraint" is resolved by time: the constraint becomes pure coordination as the imposition claim loses credibility through accumulated evidence of sustained refusal to amend. The reading's analytical power comes from making the curing mechanism (sustained refusal to amend) explicit: most legal analysis does not ask whether zero amendments across seven decades is evidence of legitimacy or of path dependence. This reading claims the former; the omega variables identify the empirical and conceptual uncertainty about which is correct.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_refusal_as_ratification_vs_path_dependence,
    'Is sustained refusal to amend the constitution evidence of active popular ratification of the text, or merely institutional path dependence (supermajority requirements, veto coalitions, and coordination problems making amendment hard regardless of popular preference)?',
    'Survey data on explicit constitutional preference; comparison of amendment attempt frequency and content to partisan cycles; analysis of whether amendment failure tracks supermajority requirements or reflects genuine popular attachment to current text',
    'If active ratification: the constraint is rope or mountain (legitimacy constituted through choice). If path dependence: the imposition critique gains force — the text persists not because the people chose it but because formal revision is institutionally difficult. Classification shifts from rope toward snare or tangled_rope from the moderate/biographical perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_refusal_as_ratification_vs_path_dependence, empirical, 'Whether amendment refusal constitutes active ratification or reflects institutional path dependence').

omega_variable(
    interpretive_curing_of_imposition_vs_post_hoc_legitimation,
    'Does sustained interpretive practice over seven decades genuinely cure the imposition claim (because the people''s act of sustained acceptance transforms an imposed text into an owned one), or does it merely disguise the imposition through post-hoc legitimation narratives that benefit from the fixed text''s authority?',
    'Historical analysis of amendment attempts (content, timing, partisan composition, stated rationales) to determine whether they target the text''s occupation origin or its specific content; examination of whether revision efforts cite democratic self-determination vs. substantive disagreement with current rules',
    'If curing: the embraced revolution reading is correct — the seven-decade unamended status is constitutive of legitimacy. If post-hoc legitimation: the interpretive layer is performing legitimacy rather than constituting it (piton-like dynamics). The classification remains rope but with higher theater_ratio and explicit recognition of performative elements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_curing_of_imposition_vs_post_hoc_legitimation, conceptual, 'Whether interpretive practice cures imposition or post-hoc legitimates it').

omega_variable(
    origin_essentialism_as_victim_vs_origin_as_legitimate_concern,
    'Is the imposed constitution critique correctly characterized as origin-essentialism (prioritizing the contingent fact of foreign drafting over current content and practice), or does it identify a legitimate structural concern: that occupying-power drafting constrains the scope of possible futures regardless of current content merit?',
    'Comparative constitutional analysis: do constitutions drafted domestically show different patterns of amendment frequency, scope, and substantive content evolution than those drafted under occupation? If occupation-drafted constitutions show systematically lower amendment rates or narrower scope of future revision, the critique identifies a real constraint rather than mere essentialism.',
    'If essentialism: the embraced revolution reading correctly dissolves the imposition concern through sustained acceptance. If legitimate concern: even sustained acceptance does not erase the structural constraint that occupation drafting imposes on future constitutional possibility space. The snare perspective gains force — the text''s fixity is not a virtue but a path dependence created by the imposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(origin_essentialism_as_victim_vs_origin_as_legitimate_concern, empirical, 'Whether origin concern is essentialism or identifies genuine constraint on constitutional futures').

omega_variable(
    kernel_reading_contest_structure,
    'Which reading of the GHQ drafting kernel is most defensible: this reading (embraced revolution — legitimacy through sustained refusal to amend), the hybrid authorship reading (Japanese participation in drafting and substantive amendments cured the imposition), or the imposed constitution critique (occupation origin disqualifies legitimacy regardless of content merit)?',
    'This is a conceptual/preference omega. The readings are not empirically falsifiable by additional data alone — they represent competing normative frames for constitutional legitimacy. Resolution requires: (1) clarifying which legitimacy criterion is primary (origin purity, substantive content, procedural ratification, sustained acceptance, democratic participation in drafting), (2) empirical analysis of what happened during 1945-1947 (hybrid authorship reading''s falsifiability condition) and what has happened since (embraced revolution reading''s falsifiability condition), and (3) judgment about whether legitimacy criteria are absolute (impositions are always illegitimate) or can be cured by time and acceptance.',
    'Different readings coexist across the constitutional community. The embraced revolution reading forecloses the imposed constitution critique (if legitimacy is constituted through sustained acceptance, then imposition cannot persist). The hybrid authorship reading coexists with this one (both assert curing mechanisms for the imposition concern, via different routes). The corpus should include all three readings as separate constraint stories, linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, preference, 'Which constitutional legitimacy reading is most defensible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ghq_drafting_imposition__embraced_revolution_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ghq_embrace_tr_t0, ghq_drafting_imposition__embraced_revolution_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement(ghq_embrace_tr_t20, ghq_drafting_imposition__embraced_revolution_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(ghq_embrace_tr_t40, ghq_drafting_imposition__embraced_revolution_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(ghq_embrace_be_t0, ghq_drafting_imposition__embraced_revolution_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ghq_embrace_be_t20, ghq_drafting_imposition__embraced_revolution_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(ghq_embrace_be_t40, ghq_drafting_imposition__embraced_revolution_reading, base_extractiveness, 40, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ghq_drafting_imposition__embraced_revolution_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ghq_drafting_imposition__embraced_revolution_reading, ghq_drafting_imposition__hybrid_authorship_reading).
narrative_ontology:affects_constraint(ghq_drafting_imposition__embraced_revolution_reading, ghq_drafting_imposition__imposed_constitution_critique).

% DUAL FORMULATION NOTE:
% The GHQ drafting kernel decomposes into three structurally distinct readings: embraced_revolution (legitimacy through sustained acceptance, ε=0.18, Rope), hybrid_authorship (legitimacy through Japanese participation in drafting, ε varies), and imposed_constitution_critique (structural constraint persisting despite content, ε varies). Each reading has different beneficiaries, victims, and extractiveness trajectories. The three readings coexist as live positions in postwar Japanese constitutional discourse. The embraced revolution reading forecloses the imposition critique (if legitimacy is constituted by sustained acceptance, imposition cannot be a live claim) but coexists with the hybrid authorship reading (both offer curing mechanisms for the imposition concern). The corpus should include all three constraint stories linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
