% ============================================================================
% CONSTRAINT STORY: ulysses_chp13
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:directionality_override/3,
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
 *   Chapter 13 of Joyce's Ulysses (published 1922) presents the encounter
 *   between Gerty MacDowell and Leopold Bloom on Sandymount Strand through
 *   Gerty's consciousness as mediated by sentimental literary discourse. The
 *   constraint is the sentimental narrative itself — the genre of feeling
 *   that structures how Gerty perceives herself, Bloom, and their encounter.
 *   This narrative is externally authored (by the cultural romance tradition,
 *   by magazine literature, by social instruction in feminine propriety) yet
 *   appears to Gerty as her own authentic selfhood. The snare operates
 *   through seduction rather than force: Gerty is not coerced into
 *   sentimentalism but rather finds it alluring, empowering, and the only
 *   language available for self-narration. Yet the constraint extracts her
 *   from authentic selfhood and mutual recognition. The encounter itself
 *   cannot be an authentic encounter because both participants are trapped in
 *   narrative structures that prevent genuine perception of the other. Gerty
 *   sees Bloom through the lens of romantic possibility; Bloom projects
 *   desire and fantasy onto Gerty; neither perceives the other as a
 *   self-aware agent operating within constraint.
 *
 * KEY AGENTS:
 *   - Gerty MacDowell: Primary victim (powerless/trapped/biographical) — trapped in sentimental self-narrative with no exit option; experiences the constraint as identity itself
 *   - Leopold Bloom: Secondary victim (moderate/constrained/biographical) — caught in the moment of encounter with no metacognitive escape; also constrained by desire and narrative fantasy
 *   - Gerty's Reflexive Awareness: Partial agent (moderate/constrained/biographical) — moments of self-consciousness that partially recognize the constraint but lack sufficient agency to escape it
 *   - Literary Sentimentalism: Beneficiary and mechanism (institutional/arbitrage/generational) — the cultural tradition that benefits from containing female subjectivity and mobility while appearing to honor and protect it
 *   - Patriarchal Social Order: Beneficiary and enforcer (institutional/arbitrage/generational) — maintains the constraint through gendered role expectations, limited female autonomy, and narrative authority over how women experience themselves
 *   - Early Modernist Literary Practice: Organized opposition (organized/constrained/continental) — Joyce and contemporaries explicitly working to dismantle the sentimental constraint through formal innovation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical/universal) — sees the constraint as simultaneously foundational to subjectivity and violently constraining of authentic selfhood
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp13, 0.68).
domain_priors:suppression_score(ulysses_chp13, 0.72).
domain_priors:theater_ratio(ulysses_chp13, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp13, extractiveness, 0.68).
narrative_ontology:constraint_metric(ulysses_chp13, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ulysses_chp13, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp13, snare).
narrative_ontology:human_readable(ulysses_chp13, "The Sentimental Snare (Sandymount Shore)").
narrative_ontology:topic_domain(ulysses_chp13, "social/artistic/psychological").

domain_priors:requires_active_enforcement(ulysses_chp13).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp13, gerty_self_narrative).
narrative_ontology:constraint_victim(ulysses_chp13, gerty_autonomous_selfhood).
narrative_ontology:constraint_victim(ulysses_chp13, bloom_ethical_integrity).
narrative_ontology:constraint_victim(ulysses_chp13, dyadic_authentic_encounter).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GERTY'S TRAPPED SUBJECTIVITY (SNARE) — Gerty is ensnared by the sentimental literary language that structures her self-perception. She lacks the metacognitive exit option to recognize how thoroughly she is authored by the constraints of her social world and the romantic narratives available to her. Trapped in the immediacy of her biographical moment, she experiences the constraint as destiny, not as extraction.
constraint_indexing:constraint_classification(ulysses_chp13, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: GERTY'S REFLEXIVE AWARENESS (TANGLED ROPE) — When Gerty's internal monologue shifts to direct address and self-justification, she demonstrates partial awareness of the constraint. She benefits from the romantic narrative (it structures her identity, provides coherence and meaning), but also bears the cost of its falseness. She experiences both coordination (the narrative solves the problem of how to be a young woman) and extraction (the narrative prevents authentic self-knowledge). This is her constrained, biographical-scale perspective.
constraint_indexing:constraint_classification(ulysses_chp13, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BLOOM'S ETHICAL POSITION (SNARE) — Bloom is entrapped in the moment with no exit. He perceives Gerty as the romantic narrative presents her, without access to the gap between that presentation and her awareness. He is caught in a structure of desire and mutual misrecognition that prevents authentic encounter. His powerlessness is different from Gerty's (he has more biographical mobility overall), but in this moment and place, he is trapped.
constraint_indexing:constraint_classification(ulysses_chp13, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: PATRIARCHAL SOCIAL CONVENTION (PITON) — The constraint is maintained by institutional norms and literary conventions that structure gender relations. From the perspective of the social order that benefits from the constraint (family structure, male mobility, female containment), the sentimental narrative is performative theater — the real function (controlling female sexuality and autonomy) is masked by the romantic framing. The constraint persists through inertia and theatrical maintenance despite eroding legitimacy.
constraint_indexing:constraint_classification(ulysses_chp13, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: EARLY MODERNIST LITERARY CRITIQUE (SCAFFOLD) — The modernist project (of which Ulysses is exemplary) explicitly aims to dismantle the sentimental constraint through formal decomposition. Joyce's stream-of-consciousness technique exposes the narrativization process itself, making visible what sentimentality had made invisible. This organized perspective sees the constraint as temporary — the new literary forms will create the conditions for authentic self-knowledge and encounter. The sunset is built into the form.
constraint_indexing:constraint_classification(ulysses_chp13, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/analytical vantage, the sentimental constraint is simultaneously a coordination mechanism (how subjects construct coherent identities within available cultural narratives) and an extraction mechanism (how those narratives constrain authentic selfhood and mutual recognition). The constraint cannot be eliminated without losing the binding function that allows subjectivity to form at all. The observer sees both the necessity and the violence of the constraint.
constraint_indexing:constraint_classification(ulysses_chp13, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp13_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp13, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp13, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp13, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp13, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp13_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Gerty is extracted from autonomous selfhood, mutual recognition capacity, and ethical agency. She is not killed or physically harmed, but her subjectivity is colonized by a narrative that serves interests other than her own authentic flourishing. The magnitude (0.68 rather than 0.80+) reflects that sentimentalism does provide some benefit — it gives Gerty a coherent self-narrative, social belonging, and erotic agency (even if misdirected). The extraction is not total deprivation but sophisticated capture of identity itself. Suppression (0.72): High. Gerty has almost no exit options from the sentimental constraint within her biographical and social context. The alternatives — being unmarked by romance, claiming autonomous desire, perceiving Bloom as merely another person rather than as the object of her narrative — are not available to her. Social norms, limited female education, genre conventions, and her own internalization of the constraint prevent departure. Bloom has slightly more exit capacity (he could walk away physically; he has more social mobility), but in the moment he too is suppressed by desire and narrative fantasy. Theater ratio (0.58): Moderate-high. The sentimental narrative involves significant performative content — Gerty's experience of herself is mediated by magazine discourse and romantic fiction; she is 'acting out' scripts rather than discovering authentic feeling. Yet there is genuine affective investment too; the line between authentic emotion and performance is blurred. The theater increases over the chapter interval as Gerty becomes more conscious of performing the role of romantic heroine.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits radical perspectival divergence. Gerty's trapped perspective (powerless/biographical/snare) experiences the constraint as destiny, meaning, and selfhood — she does not perceive it as extraction. Her reflexive perspective (moderate/biographical/tangled_rope) recognizes both benefit and cost but lacks sufficient agency to act on that recognition. Bloom's perspective (powerless/biographical/snare within this moment, though moderate/biographical/tangled_rope at civilizational scale) is caught without escape. The patriarchal institutional perspective (institutional/arbitrage/piton) maintains the constraint through theatrical performance of romantic protection while the real function (female containment) operates invisibly. The modernist literary perspective (organized/constrained/generational/scaffold) sees the constraint as temporary and destined for dissolution through formal innovation. The analytical observer (analytical/civilizational) recognizes the constraint as both foundational and violent — both constitutive of selfhood and destructive of authentic encounter. No single perspective comprehends the constraint fully; each sees it through their own structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Gerty is the primary victim: she bears the extraction of her autonomous selfhood, mutual recognition capacity, and authentic erotic agency. Her directionality value (d) is high, close to the powerless/trapped canonical value of 0.95, producing maximum f(d) and maximum experienced chi. She is the full target of the constraint's extraction. Her sentimental narrative (the beneficiary within the constraint structure) captures her from the inside — she experiences the extraction as her own desire and identity. At the literary tradition level, sentimentalism is the beneficiary: it structures how female subjectivity is narrated, contains female mobility, and provides the cultural authority for male desire. This institutional beneficiary has low directionality (d ≈ 0.15) and arbitrage exit options, producing negative chi. Bloom occupies an ambiguous position: he is partially a victim (caught in desire and narrative fantasy without authentic encounter) and partially a beneficiary (the romantic narrative gives his desire a coherent target and culturally validated expression). His directionality is moderate (d ≈ 0.60) — he experiences moderate extraction from the constraint's suppression of authentic encounter, but also moderate benefit from the narrative structure that enables his desire. The dyadic encounter itself (treated as a victim in the base properties) is the deepest victim: the possibility of authentic mutual recognition is extracted and replaced by narrative fantasy on both sides.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_baseline,
    'What constitutes ''authentic'' selfhood or encounter beneath the constraint? Is there a self prior to or independent of narrative structure?',
    'Philosophical analysis of narrative constitution of selfhood; empirical study of how pre-narrative or post-narrative selves differ from constrained ones',
    'If no pre-narrative self exists: the snare classification is incorrect — the constraint is foundational to subjectivity, making it a rope or mountain rather than pure extraction. If authentic selfhood is possible: the snare classification is validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authenticity_baseline, conceptual, 'Whether authentic selfhood exists prior to narrative constraint').

omega_variable(
    gerty_consent_structure,
    'Does Gerty consent to or actively choose the sentimental constraint, or is she entirely trapped by it?',
    'Textual analysis distinguishing strategic self-presentation from internalization; historical study of how young women in 1904 Dublin experienced choice within genre constraints',
    'If she actively chooses: constraint shifts toward tangled rope or scaffold (she benefits and consents, even if constrained). If entirely trapped: snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gerty_consent_structure, empirical, 'Degree of Gerty''s voluntary participation in the sentimental constraint').

omega_variable(
    bloom_culpability,
    'Is Bloom a victim of the constraint or a beneficiary who participates in the extraction?',
    'Ethical analysis of Bloom''s intentionality and agency; comparison with other male figures in Ulysses who more clearly exploit the constraint',
    'If beneficiary: Gerty is the sole victim and the snare is unambiguously extractive. If also victim: snare becomes tangled rope with both agents trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bloom_culpability, conceptual, 'Bloom''s structural position relative to the constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp13, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sent_snare_tr_t0, ulysses_chp13, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sent_snare_tr_t5, ulysses_chp13, theater_ratio, 5, 0.5).
narrative_ontology:measurement(sent_snare_tr_t10, ulysses_chp13, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(sent_snare_be_t0, ulysses_chp13, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(sent_snare_be_t5, ulysses_chp13, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(sent_snare_be_t10, ulysses_chp13, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp13, information_standard).
narrative_ontology:affects_constraint(ulysses_chp13, molly_soliloquy_1904).
narrative_ontology:affects_constraint(ulysses_chp13, stephen_aesthetic_capture_1904).

% DUAL FORMULATION NOTE:
% The sentimental snare is part of a constraint cluster examining how literary and romantic narratives capture subjectivity in Ulysses. Upstream: the broader patriarchal narrative tradition that provides the sentimental template. Downstream: Molly Bloom's soliloquy (where a different female consciousness resists and exceeds sentimentalism) and Stephen Dedalus's aesthetic theory (which attempts to formalize escape from narrative constraint). Each constraint in the family has its own extractiveness value reflecting the degree to which consciousness is captured or exceeds its narrative frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ulysses_chp13, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
