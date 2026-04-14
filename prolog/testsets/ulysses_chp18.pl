% ============================================================================
% CONSTRAINT STORY: ulysses_chp18
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp18, []).

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
 *   constraint_id: ulysses_chp18
 *   human_readable: The Penelopean Affirmation (7 Eccles Street)
 *   domain: social/psychological/biological
 *
 * SUMMARY:
 *   The Penelopean Affirmation at 7 Eccles Street models a constraint
 *   structure endemic to early 20th-century domestic life: the confinement of
 *   female persons to domestic space paired with cultural narratives of
 *   private interiority as authentic selfhood. Molly Bloom's unpunctuated
 *   monologue in Chapter 18 of Ulysses has been canonically read as an
 *   affirmation of female consciousness and agency — her final 'Yes' has been
 *   celebrated as a triumph of subjective authenticity. Yet the structural
 *   constraint is precisely that this authentic interiority is THE ONLY
 *   permitted domain of unmediated self-expression. External social
 *   participation requires performance; public voice is forbidden; economic
 *   autonomy is absent; mobility is controlled. The constraint exhibits all
 *   six DR types depending on the observer's structural position: a snare
 *   from Molly's perspective (trapped with no exit), a rope from the
 *   patriarchal institution's perspective (coordination mechanism securing
 *   compliance), a tangled rope from Leopold's perspective (both beneficiary
 *   and constrained), a piton from the literary canon's perspective
 *   (performative celebration masking material constraint), a scaffold from
 *   feminist organizing (historical-contingent, solvable through reframing),
 *   and a false mountain from the civilization-scale natural law view
 *   (biology-as-destiny). The theater_ratio (0.55) reflects the mixed
 *   functional-performative character: the monologue technique genuinely
 *   enables representation (functional), but canonical usage treats it as
 *   evidence of female agency while leaving material conditions unchanged
 *   (performative). The constraint's extractiveness (0.38) is moderate: the
 *   institution extracts significant domestic and sexual labor from Molly,
 *   but the extraction is not absolute — she retains some autonomy within the
 *   domestic sphere. The suppression (0.42) is substantial but incomplete:
 *   she is suppressed from public participation, but not from thinking or
 *   internal fantasy.
 *
 * KEY AGENTS:
 *   - Molly Bloom: Primary victim (powerless/trapped) — confined to domestic space, economically dependent, socially immobilized; her interiority is permitted but public voice is forbidden
 *   - Leopold Bloom: Secondary actor (moderate/constrained) — benefits from Molly's domestic labor and sexual availability but also constrained by social expectation and complicity in the structure
 *   - Patriarchal Marriage Institution: Primary beneficiary (institutional/arbitrage) — receives coordinated compliance from Molly's affirmation; no extraction perceived from institutional perspective because the 'Yes' solves the coordination problem
 *   - Dublin Social Structure: Ambient beneficiary (institutional/arbitrage) — maintains property, inheritance, and social control through regulation of female mobility and public voice
 *   - Literary Modernist Canon: Piton actor (institutional/arbitrage) — celebrates Molly's consciousness as modernist innovation; performs liberation through technique while naturalizing confinement through canonical silence on material conditions
 *   - Feminist Literary Critique: Organized observer (organized/constrained) — identifies the constraint structure and proposes reframing as path to refusal
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp18, 0.38).
domain_priors:suppression_score(ulysses_chp18, 0.42).
domain_priors:theater_ratio(ulysses_chp18, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp18, extractiveness, 0.38).
narrative_ontology:constraint_metric(ulysses_chp18, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(ulysses_chp18, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp18, tangled_rope).
narrative_ontology:human_readable(ulysses_chp18, "The Penelopean Affirmation (7 Eccles Street)").
narrative_ontology:topic_domain(ulysses_chp18, "social/psychological/biological").

domain_priors:requires_active_enforcement(ulysses_chp18).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp18, patriarchal_marriage_institution).
narrative_ontology:constraint_beneficiary(ulysses_chp18, domestic_containment_system).
narrative_ontology:constraint_victim(ulysses_chp18, molly_bloom_autonomy).
narrative_ontology:constraint_victim(ulysses_chp18, female_epistemic_voice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MOLLY BLOOM (SNARE) — Confined to domestic space (7 Eccles Street), economically dependent, socially immobilized. Her consciousness flows freely in solitude, but the constraint is that this interiority is the ONLY permitted domain of authentic expression. External world interaction requires performance; internal monologue is the trap disguised as freedom. She cannot exit marriage, cannot claim social voice, cannot refuse domestic role. The 'Yes' at the end is affirmed within the structure that constrains her.
constraint_indexing:constraint_classification(ulysses_chp18, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PATRIARCHAL MARRIAGE INSTITUTION (ROPE) — The institution experiences the constraint as coordination: Molly's affirmation (her 'Yes') renews the marriage contract, reproduces legitimacy, stabilizes the household. Her interiority is a coordination mechanism — her private assent enables public structure. No meaningful extraction from the institution's perspective; the constraint solves the problem of securing compliance without constant enforcement. The performative burden is minimal for institutional actors.
constraint_indexing:constraint_classification(ulysses_chp18, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: LEOPOLD BLOOM (TANGLED ROPE) — Constrained by social expectation, economic necessity, and his own complicity in the constraint structure. Benefits from Molly's affirmation (household stability, sexual availability) but also bears costs (his own autonomy is regulated, his infidelity carries social risk). Moderate power, constrained exit — he cannot simply leave without social degradation. The constraint is both coordination (marriage as mutual understanding) and extraction (of sexual/domestic labor from Molly).
constraint_indexing:constraint_classification(ulysses_chp18, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LITERARY MODERNIST CANON (PITON) — By 1922, the representation of female interiority through stream-of-consciousness technique has become a canonical marker of artistic innovation. The 'Yes' is celebrated as an affirmation of modernist technique, of literary authenticity, of Molly's subjectivity. But the theater is high: the canonical reading performs Molly's consciousness-as-liberation while leaving the material constraint (domestic confinement, economic dependence) structurally unaddressed. The technique is functional (it does enable representation), but the institutional use of it has become performative — citing Molly's voice as evidence of female agency in modernism, while the social structure that confines her remains naturalized. Theater ratio 0.55 reflects this mixed functional/performative use.
constraint_indexing:constraint_classification(ulysses_chp18, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EARLY FEMINIST LITERARY CRITIQUE (SCAFFOLD) — Organized feminist readers (from roughly 1960s onward) identify the constraint and propose alternative interpretations: Molly's 'Yes' is strategic affirmation within constraint, not transcendence of it. Feminist literary praxis treats Chapter 18 as a diagnostic text revealing the structure of domestic extraction. This perspective sees the constraint as temporary — as historical-contingent rather than eternal — and uses textual analysis as a sunset mechanism: making visible the constraint is the first step to refusing it. Organized agents have interpretive agency; they experience the constraint as solvable through reframing.
constraint_indexing:constraint_classification(ulysses_chp18, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a sufficiently abstract perspective, the affirmation of reproductive partnership (and thus reproduction itself) is an immutable biological necessity. The constraint appears as natural law: humans require mating pair-bonds for genetic reproduction; these bonds require affective confirmation; therefore, female affirmation is a structural inevitable. This perspective naturalizes the social constraint as biological imperative. However, this mountain classification is suspect — it confuses biological capacity (humans can reproduce) with social structure (one particular configuration of marriage, domestic confinement, and property transmission). The engine's false summit detector will flag this as naturalization.
constraint_indexing:constraint_classification(ulysses_chp18, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp18_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp18, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp18, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp18, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp18_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, trending upward. The institution extracts significant labor (domestic management, sexual availability, emotional labor, reproduction) from Molly. However, the extraction is not as severe as a pure snare (0.46+) because Molly retains substantial autonomy within the domestic sphere — she can think, fantasize, remember, refuse sexual availability on any given night. The interiority is genuinely free. What is constrained is not consciousness but external participation. Suppression (0.42): Substantial. Multiple barriers prevent Molly from entering public discourse (social expectation, lack of property rights, lack of economic independence, legal coverture in marital property, gender norms around public speech). But suppression is not total — women in 1904 Dublin DID participate in some public life, and Molly's own memories show she has experienced public space (her affair with Blazes Boylan, her interactions in shops, her past as a performer). Theater_ratio (0.55): Moderate-high, trending upward. The stream-of-consciousness technique is functionally innovative (it genuinely enables representation of female consciousness). But by 1922 and onward, canonical deployment of the technique becomes increasingly performative: citing Molly as evidence of female voice while the material condition (women's legal and economic position) remains constrained. The theater_ratio rises over the interval (0.35 → 0.55) as the canonical celebration grows while material conditions shift more slowly.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival divergence is extreme and diagnostic of the constraint's hybrid character. From Molly's position (powerless/trapped), the constraint is a snare: she cannot exit marriage, cannot claim public voice, cannot refuse domestic role. Her only freedom is internal monologue. From the institution's position (institutional/arbitrage), the constraint is a rope: her affirmation solves the coordination problem. No extraction is perceived because the institution benefits from her compliance without needing to enforce it continuously — her own 'Yes' reproduces the system. From Leopold's position (moderate/constrained), the constraint is tangled rope: he benefits from her domestic and sexual labor, but he is also constrained by the same social expectations and by her affirmation. From the feminist organizing position (organized/constrained), the constraint is a scaffold: historical-contingent, solvable, the technique of making it visible is the sunset mechanism. From the literary canon's position (institutional/arbitrage), the constraint is a piton: the modernist celebration of Molly's voice is performative — it does functional work (represents her consciousness) but increasingly theatrical work (performs female agency while leaving material constraints intact). From the civilizational analytical position, the temptation is to see a mountain (biological necessity of pair-bonding, reproduction), but structural analysis reveals this as false — the constraint is not the need for reproduction but the particular social configuration (confinement, coverture, exclusion from public participation) that 1904 Dublin imposed on women.
 *
 * DIRECTIONALITY LOGIC:
 *   The engine derives directionality (d) from each agent's beneficiary/victim status and exit options. Molly, as victim with trapped exit, receives maximum d (≈0.95 in structural terms), producing high f(d) ≈ 1.42 — she experiences maximum extractiveness. The institution, as beneficiary with arbitrage exit, receives low d (≈0.05), producing negative f(d) ≈ -0.12 — the institution experiences the constraint as net coordination, not extraction. Leopold, as mixed (benefits from extraction but also constrained), receives moderate d (≈0.55), producing f(d) ≈ 0.75 — moderate experienced extraction. The organized feminist perspective, with analytical tools and generational time horizon, derives d ≈ 0.40-0.50 (sees the constraint as solvable rather than immutable), producing f(d) in the rope-scaffold range. The piton classification for the literary canon derives not from high d (the canon does not experience extraction) but from high theater_ratio: the technique is functionally innovative but institutionally performative.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint exemplifies the mandatrophy resolved through perspectival multiplication. The naive classification is 'Is this a Rope (mutual affirmation enabling partnership) or a Snare (coercive extraction)?' The answer is BOTH — the same structural phenomenon is genuinely coordination from the institution's view (Rope) and genuine extraction from Molly's view (Snare). The constraint does not collapse to one type when more data arrives; rather, the six types ARE the data. The perspectival gap IS the message. The false summit (mountain from civilization view) alerts us that the 'natural law' framing naturalizes the historical-contingent. The piton (canon's performative celebration) reveals how institutional forces can misread snares as ropes by redefining what counts as freedom (internal voice as substitute for public voice). The scaffold (feminist reframing) shows that recognizing the structure is the first step to refusing it. Mandatrophy is resolved by accepting that the constraint has fundamentally different existence-conditions from different structural positions — it is not that one perspective is 'true' and others wrong, but that the constraint's reality IS the presheaf over all six perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    affirmation_autonomy_paradox,
    'Does Molly Bloom''s final affirmation represent authentic choice within constraint, or performative compliance disguised as choice?',
    'Textual analysis of the unpunctuated flow structure; comparison with other modernist representations of female consciousness; historical analysis of women''s agency within 1904 Dublin marriage',
    'If authentic choice: constraint is cooperative rope (mutual affirmation). If performative: constraint is snare (coerced assent). Classification hinges on whether the ''Yes'' carries real exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(affirmation_autonomy_paradox, conceptual, 'Whether Molly''s affirmation is authentic choice or performative compliance').

omega_variable(
    stream_of_consciousness_liberation_claim,
    'Does the literary technique of stream-of-consciousness constitute genuine epistemic liberation for the represented subject, or does it reproduce the constraint by aestheticizing confinement?',
    'Analysis of material conditions before and after 1922 for women in Dublin; examination of whether literary representation correlated with actual legal/economic changes; study of whether canonical celebration of Molly''s voice produced material change in women''s access to public speech',
    'If technique liberates: scaffold with real sunset mechanism. If technique aestheticizes: piton performing liberation while maintaining constraint. Theater_ratio interpretation depends on resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stream_of_consciousness_liberation_claim, empirical, 'Whether stream-of-consciousness technique provides epistemic liberation or aesthetic containment').

omega_variable(
    domestic_sphere_autonomy_boundary,
    'Is the autonomy of internal monologue (permitted within domestic space) structurally different from the autonomy of public voice (forbidden in social space), or is the distinction itself part of the extraction mechanism?',
    'Comparative analysis of female characters'' internal vs external voice in 1900-1925 literature; historical study of women''s access to public discourse during this period; analysis of whether ''private autonomy'' served as substitute for public participation',
    'If structurally different: two separate constraints (domestic interiority and public exclusion). If part of single mechanism: the private freedom is a valve limiting public exit. Extractiveness and suppression both scale differently depending on resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domestic_sphere_autonomy_boundary, empirical, 'Whether domestic interiority and public exclusion are separate constraints or components of one extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp18, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pene_tr_t0, ulysses_chp18, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pene_tr_t5, ulysses_chp18, theater_ratio, 5, 0.48).
narrative_ontology:measurement(pene_tr_t10, ulysses_chp18, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(pene_be_t0, ulysses_chp18, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(pene_be_t5, ulysses_chp18, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(pene_be_t10, ulysses_chp18, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp18, enforcement_mechanism).
narrative_ontology:affects_constraint(ulysses_chp18, coverture_legal_disability).
narrative_ontology:affects_constraint(ulysses_chp18, domestic_labor_invisibility).
narrative_ontology:affects_constraint(ulysses_chp18, female_epistemic_marginalization).

% DUAL FORMULATION NOTE:
% The Penelopean Affirmation is downstream of legal/economic structures (coverture, property law, labor law) and upstream of epistemic structures (canon, representation, narrative authority). Related constraints: legal disability of married women (coverture_legal_disability), invisibility of domestic labor in economic accounting (domestic_labor_invisibility), exclusion of female voices from public epistemology (female_epistemic_marginalization). This story focuses on the psychological/biographical manifestation; upstream stories address legal structure; downstream stories address epistemological impact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ulysses_chp18, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
