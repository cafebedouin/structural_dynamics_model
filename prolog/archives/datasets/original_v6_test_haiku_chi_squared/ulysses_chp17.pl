% ============================================================================
% CONSTRAINT STORY: ulysses_chp17
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp17, []).

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
 *   Chapter 17 ('Ithaca') of James Joyce's Ulysses presents the return of
 *   Leopold Bloom and Stephen Dedalus to 7 Eccles Street through an
 *   exhaustive catechism: hundreds of questions and answers covering domestic
 *   minutiae, scientific facts, historical data, and abstract speculations.
 *   The chapter abandons narrative voice and phenomenological interiority in
 *   favor of a cold, encyclopedic format. This structural choice creates a
 *   fundamental tension: the reader seeks emotional and narrative closure
 *   after a 16-chapter emotional and sensory journey, but the catechism
 *   format explicitly suppresses sentiment, embodied knowledge, and
 *   phenomenological immediacy. The constraint is neither purely extractive
 *   (Snare) nor purely coordinative (Rope), but a deliberate hybrid that
 *   coordinates rational discourse while extracting emotional closure — a
 *   Tangled Rope that uses formal suppression as its enforcement mechanism.
 *
 * KEY AGENTS:
 *   - The Reader: Primary victim (powerless/trapped) — seeks human connection; trapped in catechism's mechanical apparatus
 *   - The Literary Audience: Secondary victim (moderate/constrained) — benefits from formal innovation; constrained by convention and alienated by coldness
 *   - The Rational Discourse Tradition: Primary beneficiary (institutional/arbitrage) — catechism validates mathematical knowledge and question-answer epistemology; experiences no extraction
 *   - The Modernist Movement: Secondary beneficiary (organized/constrained) — modernism used the catechism temporarily (scaffold); eventually dissolved it in Molly's monologue
 *   - The Academic Commentary System: Institutional actor (institutional/arbitrage) — maintains catechism as canonical text through exegetical labor; theater_ratio reveals performative rather than functional engagement
 *   - Phenomenological Experience: Abstract victim (powerless/trapped) — sensation, emotion, embodied knowledge are systematically excluded; cannot exit the text
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp17, 0.38).
domain_priors:suppression_score(ulysses_chp17, 0.52).
domain_priors:theater_ratio(ulysses_chp17, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp17, extractiveness, 0.38).
narrative_ontology:constraint_metric(ulysses_chp17, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ulysses_chp17, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp17, tangled_rope).
narrative_ontology:human_readable(ulysses_chp17, "The Mathematical Catechism (7 Eccles Street)").
narrative_ontology:topic_domain(ulysses_chp17, "technological/scientific/social").

domain_priors:requires_active_enforcement(ulysses_chp17).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp17, rational_discourse_tradition).
narrative_ontology:constraint_beneficiary(ulysses_chp17, narrative_closure_machinery).
narrative_ontology:constraint_victim(ulysses_chp17, phenomenological_experience).
narrative_ontology:constraint_victim(ulysses_chp17, embodied_knowledge).
narrative_ontology:constraint_victim(ulysses_chp17, emotional_communion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: READER SEEKING EMOTIONAL CLOSURE (SNARE) — Trapped in the catechism's formal apparatus. Desires human communion between Bloom and Stephen, but the mathematical/scientific format actively suppresses emotional resonance. No exit from the text. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.42.
constraint_indexing:constraint_classification(ulysses_chp17, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: LITERARY AUDIENCE (TANGLED ROPE) — Constrained by convention (Ithaca is the canonical narrative closure). Benefits from the catechism's intellectual density, prestige, and formal innovation. Costs: alienation, mechanical coldness, suppression of sentiment. d≈0.58, f(d)≈0.77, σ=0.9 → χ≈0.30.
constraint_indexing:constraint_classification(ulysses_chp17, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RATIONAL DISCOURSE TRADITION (ROPE) — Benefits from catechism as coordination mechanism: question-answer establishes shared epistemology, mathematical rigor validates knowledge claims. Experiences extraction as negligible. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.04.
constraint_indexing:constraint_classification(ulysses_chp17, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MODERNIST LITERARY MOVEMENT (SCAFFOLD) — Organized response to Victorian sentiment. Catechism is temporary structural device: a sunset clause embedded in the text. As modernism matured, stream-of-consciousness and fragmentation replaced catechism's formal apparatus. The suppression (0.52) was tolerated because the form had a declared endpoint: Molly's monologue dissolves it. d≈0.35, f(d)≈0.30, σ=0.9 → χ≈0.10.
constraint_indexing:constraint_classification(ulysses_chp17, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ACADEMIC COMMENTARY SYSTEM (PITON) — Theater_ratio=0.68 (high). Scholars analyze the catechism's 'formal perfection' and 'structural mastery' in extensive exegesis, but the primary function (providing emotional or narrative closure) has atrophied. The academic machinery maintains the text through ritualized close reading; the catechism persists as an object of exegetical labor rather than genuine narrative function. Institutional inertia: Ithaca is canonical, so it must be important.
constraint_indexing:constraint_classification(ulysses_chp17, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the catechism coordinates rational knowledge through question-answer structure (rope function) while extracting emotional/phenomenological closure through mathematical suppression of sentiment (snare function). This is not a natural law but a deliberate formal choice. ε=0.38, suppression=0.52, theater=0.68 all point to hybrid exploitation: the reader's desire for closure is channeled through an apparatus that denies the channeling. d≈0.70, f(d)≈1.12, σ=1.0 → χ≈0.43.
constraint_indexing:constraint_classification(ulysses_chp17, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

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
 *   Extractiveness (0.38): Moderate. The catechism extracts emotional closure through formal suppression, but this is not total seizure — the reader still obtains intellectual closure and stylistic prestige. The extraction is hybrid with genuine coordination (establishing shared epistemology). Over the interval (0-14), extractiveness increases from 0.18 to 0.38 as the accumulation of cold facts increasingly suppresses residual warmth. Suppression (0.52): Moderate-high. The catechism systematically suppresses emotion, sensation, embodied knowledge, and human communion through its format. Alternatives (narrative prose, dialogue, monologue) exist and are present in the novel; the catechism chooses among them. Suppression is not total — Molly's monologue immediately follows, dissolving the catechism's constraint. Theater ratio (0.68): High. The catechism performs completeness and exhaustiveness through encyclopedic formatting and mathematical precision, but the primary function (narrative closure) is actually deferred or denied. Academic commentary treats the form as structurally necessary ('Joycean mastery'), but this is performative validation — the form works only as formal innovation, not as narrative completion. Theater increases from 0.35 to 0.68 as critical apparatus builds around the text, ritualizing its interpretation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates perspectival divergence across all six types. The reader sees pure extraction (Snare) — cold mechanism blocks desired connection. The rational tradition sees pure coordination (Rope) — catechism enables shared knowledge. The modernist movement saw temporary coordination with sunset (Scaffold) — the form worked for its era, then was dissolved. The academic system sees ritualized importance (Piton) — the form persists through commentary, not function. The moderate literary audience sees mixed coordination and suppression (Tangled Rope) — the novel both enlightens and alienates through the same device. The analytical observer sees deliberate hybrid exploitation (Tangled Rope) — Joyce coordinates rational discourse while extracting emotional closure, using formal suppression as the enforcement mechanism. The perspectival gaps are unbridgeable because the catechism IS the constraint — changing perspectives does not change the text, only reveals different structural relationships to its fixed form.
 *
 * DIRECTIONALITY LOGIC:
 *   Reader: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction relative to powerless agent. Literary audience: Victim/beneficiary + constrained → d≈0.58, f(d)≈0.77. Mixed because the audience both benefits (prestige, innovation) and suffers (alienation, coldness). Rational tradition: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; catechism validates their epistemology with minimal cost. Modernist movement: Beneficiary + constrained → d≈0.35, f(d)≈0.30. Low extraction due to organized agency and sunset perspective. Academic system: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification comes from theater gate, not directionality — the system benefits from having a prestigious text to analyze. Analytical observer: Neither + analytical → d≈0.70, f(d)≈1.12. Observes extraction mechanism (emotional suppression via format) and coordination mechanism (rational discourse via question-answer) operating simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   LITERARY CONSTRAINT EXEMPLAR: The mandatrophy is resolved by recognizing that the catechism is a deliberate formal choice that integrates coordination function (establishing shared epistemology through question-answer structure) with asymmetric extraction (suppressing emotional closure through mathematical coldness). This is not a misclassified pure extraction (Snare) disguised as coordination, nor a misclassified coordination (Rope) concealing hidden extraction. It is genuinely both: Joyce chose to couple these functions through the same formal apparatus. The Tangled Rope classification captures this hybrid. The perspectival divergence (reader sees Snare; tradition sees Rope; analyst sees Tangled Rope) reflects that different agents experience the same constraint differently based on their structural relationship to it. The constraint resolves mandatrophy by being transparent about its hybridity: the catechism works only as a formal device that simultaneously enables and suppresses. Theater_ratio (0.68) reveals the performative dimension: the form's importance is maintained by exegetical ritual, not by ongoing narrative function. Molly's monologue (which immediately follows) provides the emotional closure the catechism defers, suggesting that the sunset is baked into the chapter sequence itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_of_coldness,
    'Is the catechism''s mechanical coldness Joyce''s deliberate formal choice or an accidental consequence of adopting the catechism structure?',
    'Manuscript analysis (fragments, drafts); Joyce''s letters and recorded statements about Chapter 17 composition; comparison with alternate chapter structures he considered',
    'If deliberate: suppression (0.52) is a designed feature of the constraint, making it pure Tangled Rope (intentional hybrid). If accidental: the suppression may be unintended extraction, degrading the rope coordination function toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_of_coldness, empirical, 'Whether the catechism''s coldness is deliberate formal choice or structural accident').

omega_variable(
    closure_adequacy,
    'Does the catechism actually provide narrative closure for the reader, or does it defer/suppress closure while appearing to provide it?',
    'Literary response data (reader accounts, phenomenological studies); comparison of emotional state after Ithaca vs after Molly''s monologue; analysis of whether closure is perceived or theatrical',
    'If adequate closure: theater_ratio lower (~0.40), classification shifts toward pure Rope. If theatrical/deferred: theater_ratio confirmed at 0.68+, Piton characteristics dominate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(closure_adequacy, empirical, 'Whether the catechism delivers genuine narrative closure or theatrical deferral').

omega_variable(
    mathematical_necessity,
    'Is the catechism''s mathematical/scientific form necessary to the novel''s meaning or could equivalent intellectual content be delivered through other forms (dialogue, interior monologue, essay) without suppressing sensation?',
    'Comparative structural analysis with Joyce''s other works; examination of what intellectual work the mathematical form does that other forms could not; rewriting experiments',
    'If necessary: suppression (0.52) is a structural trade-off, not mere extraction. If contingent: the constraint becomes a pure snare masquerading as necessary, extracting closure through formal constraint rather than substantive need.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mathematical_necessity, conceptual, 'Whether mathematical form is structurally necessary or formally contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp17, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ithaca_tr_t0, ulysses_chp17, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ithaca_tr_t7, ulysses_chp17, theater_ratio, 7, 0.52).
narrative_ontology:measurement(ithaca_tr_t14, ulysses_chp17, theater_ratio, 14, 0.68).

% Extraction over time
narrative_ontology:measurement(ithaca_be_t0, ulysses_chp17, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(ithaca_be_t7, ulysses_chp17, base_extractiveness, 7, 0.28).
narrative_ontology:measurement(ithaca_be_t14, ulysses_chp17, base_extractiveness, 14, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp17, information_standard).
narrative_ontology:affects_constraint(ulysses_chp17, molly_monologue_restoration).
narrative_ontology:affects_constraint(ulysses_chp17, modernist_narrative_fragmentation).

% DUAL FORMULATION NOTE:
% The Mathematical Catechism is upstream of Molly's monologue in the novel's sequence, but structurally downstream of the modernist rejection of Victorian closure. The catechism is Joyce's synthesis: it adopts the rational/encyclopedic form to execute a modernist rupture with sentiment, while Molly's monologue restores sentiment (explicitly, through her voice) as the final narrative act. These constraints form a family where the catechism (ε=0.38, Tangled Rope) and Molly's restoration (ε≈0.15, Rope) are structurally interdependent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ulysses_chp17, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
