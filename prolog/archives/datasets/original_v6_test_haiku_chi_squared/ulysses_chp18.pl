% ============================================================================
% CONSTRAINT STORY: ulysses_chp18
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_penelope_1904, []).

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
 *   The Penelopean Affirmation (Chapter 18 of Ulysses) presents a constraint
 *   at the intersection of literary representation, gender, and consciousness
 *   itself. The chapter renders Molly Bloom's stream-of-consciousness thought
 *   as unpunctuated, flowing narrative, culminating in her affirmation 'Yes'
 *   — conventionally read as a moment of female agency, embodied knowledge,
 *   and sexual self-affirmation. Yet this same formal technique that claims
 *   to capture authentic female interiority simultaneously makes that
 *   interiority visible, legible, and subject to male authorial control and
 *   male readership interpretation. The constraint is a hybrid: it provides
 *   genuine coordination (making female consciousness narratively legible)
 *   while extracting (subjecting that consciousness to representation,
 *   judgment, and reduction to erotic/maternal archetypes). The theater ratio
 *   rises over time (0.42 → 0.68) as the chapter's scholarly reception
 *   becomes increasingly performative — celebrated as feminist triumph
 *   without continuous engagement with the extraction mechanism underneath.
 *   The extractiveness similarly rises (0.22 → 0.38) as second-order literary
 *   scholarship interprets and reinterprets Molly's voice, each
 *   interpretation further mediated by male-dominated literary authority.
 *   This is not a mountain (biological inevitability) but a contingent
 *   institutional extraction mechanism that is naturalized through appeals to
 *   female embodiment and authenticity.
 *
 * KEY AGENTS:
 *   - Molly Bloom (Fictional Character): Primary victim (powerless/trapped) — her consciousness is rendered publicly and remains subject to authorial and readerly interpretation with no exit option outside the text
 *   - Female Interiority (Suppressed Domain): Collective victim (powerless/trapped) — women's consciousness and embodied knowledge have been historically silenced; the chapter's coordination function (giving voice) simultaneously extracts visibility for control
 *   - Joyce / Male Modernist Author: Primary beneficiary (institutional/arbitrage) — captures aesthetic authority through rendering previously illegible female consciousness; establishes interpretive dominance over female subjectivity
 *   - Literary Canon Builders: Secondary beneficiary (institutional/arbitrage) — control the interpretation of the chapter's meaning and its position in cultural memory; maintain male-centered authority while canonizing 'progressive' representation
 *   - Feminist Literary Scholars: Organized agents (organized/constrained) — benefit from the chapter's validation of female interiority as narratively worthy; trapped in interpreting Joyce's representation rather than accessing direct women's expression
 *   - Male Readership (Implied): Beneficiary (powerful/mobile) — gains access to legible female interiority for aesthetic consumption and erotic appeal
 *   - Analytical Observer: Sees the false mountain — biological determinism naturalizing contingent institutional arrangements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp18, 0.38).
domain_priors:suppression_score(ulysses_chp18, 0.62).
domain_priors:theater_ratio(ulysses_chp18, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp18, extractiveness, 0.38).
narrative_ontology:constraint_metric(ulysses_chp18, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ulysses_chp18, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp18, tangled_rope).
narrative_ontology:human_readable(ulysses_chp18, "The Penelopean Affirmation (7 Eccles Street)").
narrative_ontology:topic_domain(ulysses_chp18, "social/psychological/biological").

domain_priors:requires_active_enforcement(ulysses_chp18).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp18, patriarchal_narrative_authority).
narrative_ontology:constraint_beneficiary(ulysses_chp18, literary_canonical_voice).
narrative_ontology:constraint_victim(ulysses_chp18, women_interiority_suppression).
narrative_ontology:constraint_victim(ulysses_chp18, female_embodied_knowledge).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MOLLY BLOOM (SNARE) — Her consciousness is rendered through a formal constraint (stream-of-consciousness unpunctuated form) designed to capture authentic female interiority, yet the very technique creates a new trap: her thoughts are made visible to male authorial control and male readership. She cannot exit this exposure without ceasing to be represented. d≈0.93, f(d)≈1.40, σ=0.8 → χ≈0.52.
constraint_indexing:constraint_classification(ulysses_chp18, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: DOMESTIC WOMAN (TANGLED ROPE) — The chapter's coordination function: it legitimizes female subjective experience as narratively worthy, breaking silence around embodied female consciousness. Simultaneously, it extracts her intimate thoughts for literary consumption and aesthetic judgment by a male-centered literary establishment. She benefits from representation and from Ulysses' artistic validation of her interiority; she bears the cost of exposure, judgment, and reduction to erotic/maternal archetypes. d≈0.58, f(d)≈0.78, σ=0.9 → χ≈0.36.
constraint_indexing:constraint_classification(ulysses_chp18, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: JOYCE / MALE MODERNIST AUTHORITY (ROPE) — Benefits from coordination: the unpunctuated stream renders previously illegible female consciousness as literary material, capturing what could not be captured in conventional narrative. This enables a new form of aesthetic control and intellectual authority — the ability to represent, judge, and canonize women's interiority. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.05. Net beneficiary; the constraint solves his authorial problem (how to represent female consciousness) while establishing his interpretive dominance.
constraint_indexing:constraint_classification(ulysses_chp18, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FEMINIST LITERARY READERS (TANGLED ROPE) — The Penelopean chapter's affirmation ('Yes') is claimed by feminist readers as a moment of female agency and self-affirmation, yet the chapter is embedded in Joyce's authorial control and male readership interpretation. Feminist scholarship benefits from the chapter's validation of female interiority (coordination gain); feminist scholars are simultaneously trapped in interpreting Joyce's representation rather than encountering direct women's expression (extraction). d≈0.52, f(d)≈0.68, σ=1.0 → χ≈0.26.
constraint_indexing:constraint_classification(ulysses_chp18, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LITERARY CRITICAL ESTABLISHMENT (PITON) — The chapter persists as a canonical moment of 'feminist literary achievement' largely through institutional repetition and curriculum embedding, despite scholarly question about whether unmediated stream-of-consciousness from a male author represents authentic female voice or its aesthetic capture. The critical consensus has become performative: the chapter is invoked as progressive without continuous engagement with the extraction mechanism. theater_ratio=0.68 reflects that scholarly celebration of the affirmation often bypasses structural critique of representation. d≈0.12, f(d)≈-0.08, σ=1.2 → χ≈-0.06. The establishment benefits from canonizing a woman-centered chapter without destabilizing male authorial authority.
constraint_indexing:constraint_classification(ulysses_chp18, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal biological/psychological perspective, the chapter's force derives from the irreducible fact of female embodied experience: menstruation, sexuality, maternal capacity, erotic desire. These are not representations but constraints on female embodied reality. The chapter's power is naturalized as simply 'giving voice to biological truth.' However, this naturalizes the artistic/institutional extraction mechanism — the chapter does not simply report biology; it aestheticizes and controls the representation of it. The mountain classification is a false summit: biological constraints are real, but their literary commodification is contingent. ε=0.38, suppression=0.62 → not a mountain (ε > 0.25). This perspective risks naturalizing the extraction as 'inevitable given the body.'
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
    constraint_indexing:constraint_classification(ulysses_chp18, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.38): Moderate. The chapter coordinates genuine novelty — it renders female consciousness narratively legible in a way that was rare in 1922. But the coordination is not costless; Molly's interiority is made visible for authorial and readerly judgment, creating an asymmetry: her thoughts are available to external interpretation while her interpretive authority over her own thoughts remains delegated to Joyce and subsequent male critics. Over the interval, extractiveness rises as the chapter becomes more centrally positioned in the canon and subject to more secondary interpretation. Suppression (0.62): Moderate-high. Significant barriers include: (1) the chapter's dependence on male authorship for legitimacy, (2) the reduction of female interiority to erotic/maternal archetypes, (3) male-centered literary institution's control over the chapter's interpretation, (4) publication barriers and career marginalization for women writers articulating alternative representations. Theater ratio (0.68): High and rising. In contemporary scholarship, the chapter is often invoked as a canonical moment of feminist literary achievement, yet the critical engagement often bypasses the structural extraction mechanism. The celebratory framing of the affirmation obscures the question of whose voice is actually being affirmed — Molly's or Joyce's idea of Molly. The ritual repetition of 'the affirmation as feminist triumph' has become performative.
 *
 * PERSPECTIVAL GAP:
 *   The Penelopean Affirmation demonstrates perspectival collapse and clarification simultaneously. Molly Bloom (powerless/trapped) experiences pure extraction (Snare): her consciousness is made visible without exit. The domestic woman (moderate/constrained) experiences mixed coordination and extraction (Tangled Rope): her interiority is validated as narratively worthy AND subjected to judgment. Joyce (institutional/arbitrage) experiences pure coordination (Rope): he solves the problem of representing female consciousness and establishes authorial authority. Feminist scholars (organized/constrained) experience mixed benefit and constraint (Tangled Rope): they gain scholarly material and canonical validation while remaining trapped in interpreting Joyce's representation. The literary establishment (institutional/arbitrage, piton) experiences degraded function disguised as maintenance: the critical consensus that the chapter is 'feminist' persists through repetition despite structural critique. The analytical observer risks a false mountain (biological naturalization): 'female embodied consciousness is inherently affirmative' — but this naturalizes the institutional extraction mechanism as inevitable. The perspectival gap reveals that the 'affirmation' is not univocal: it is Joyce's affirmation of Molly, male readership's affirmation of access to female interiority, and appropriable by feminist readers as female self-affirmation, but not identical to any of these.
 *
 * DIRECTIONALITY LOGIC:
 *   Molly Bloom: Victim + trapped → d≈0.93, f(d)≈1.40. Maximal extraction. Domestic woman (role): Victim + constrained + beneficiary (secondary) → d≈0.58, f(d)≈0.78. Significant extraction with partial coordination benefit. Joyce: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; the constraint solves his authorial problem. Feminist scholars: Victim (trapped in male interpretation) + beneficiary (canonical validation) + organized + constrained → d≈0.52, f(d)≈0.68. Mixed extraction and coordination. Literary establishment: Beneficiary (controls interpretation) + institutional + arbitrage → d≈0.12, f(d)≈-0.08. Net beneficiary; piton classification from theater gate (0.68 ≥ 0.70). Analytical observer: d≈0.72, f(d)≈1.15. False mountain classification risks naturalizing contingent extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing coordination (making female consciousness legible) from extraction (controlling the representation and interpretation of that consciousness). Joyce's modernist innovation is genuinely novel and coordinating — he solves the aesthetic problem of rendering female interiority in prose. Simultaneously, the constraint extracts: Molly's consciousness is made visible for judgment, reduction, and reinterpretation by male authority. The false mountain (biological naturalization: 'female sexuality is inherently affirmative') must be rejected. The chapter does not simply report female embodied experience; it aestheticizes, formalizes, and control it through the male author's representational authority. The ethical force of feminist claims to the chapter (that it validates female agency) is not false, but partial and dependent on overlooking the extraction mechanism. The constraint remains Tangled Rope: genuine coordination (legitimizing female consciousness as narratively worthy) plus asymmetric extraction (subjecting that consciousness to male authorial and readerly judgment). The fact that the extraction mechanism has become more theatrical over time (theater ratio rising) indicates degradation: the earlier modernist innovation (making female consciousness visible) is now ritualized and invoked without continuous structural critique.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authentic_female_voice_boundary,
    'Does stream-of-consciousness narrative technique capture authentic female interiority or aestheticize it under male authorial control?',
    'Comparative textual analysis with women writers'' own stream-of-consciousness (Woolf, Stein, Mansfield); analysis of narrative focalization and authorial omniscience; reader response studies comparing female and male interpretations',
    'If authentic capture: the constraint is primarily Rope (coordination) with secondary extraction. If aestheticization: constraint is primarily Snare (extraction) with pseudo-coordination. If both simultaneously: constraint remains Tangled Rope but the balance of beneficiary/victim shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authentic_female_voice_boundary, conceptual, 'Whether the technique captures or aestheticizes female consciousness').

omega_variable(
    affirmation_authorial_intent,
    'Is Molly''s final ''Yes'' authorial endorsement of female agency/sexuality or Joyce''s aesthetic judgment of female compliance and receptivity?',
    'Textual analysis of surrounding narrative framing; biographical analysis of Joyce''s statements about the character; comparison with Joyce''s representation of male characters'' affirmative moments; feminist philosophical reading of consent and affirmation under patriarchal conditions',
    'If agency affirmation: the extraction mechanism weakens; the coordination function strengthens. If aesthetic judgment of compliance: extraction mechanism is clarified; the false mountain (naturalizing female sexuality as inherent affirmation) is exposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(affirmation_authorial_intent, conceptual, 'Whose affirmation the final ''Yes'' represents').

omega_variable(
    embodied_knowledge_silencing,
    'Does the chapter''s validation of female embodied consciousness actually increase women''s epistemic authority in literary and philosophical discourse, or does it increase the visibility/consumption of female embodiment while authority remains male?',
    'Citation patterns and interpretive authority shifts in literary scholarship; analysis of whose interpretations of Molly''s consciousness are treated as authoritative; comparison of female-authored interpretations vs male-authored interpretations in critical canon; institutional analysis of whose voice controls the meaning of the affirmation',
    'If female epistemic authority increased: suppression (0.62) overstates the extraction. If authority remains male: suppression is accurately measured; the coordination function (giving voice) masks extraction (extracting voice-meaning-authority from female agents).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(embodied_knowledge_silencing, empirical, 'Whether visible interiority translates to epistemic authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp18, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(penelopean_theater_start, ulysses_chp18, theater_ratio, 0, 0.42).
narrative_ontology:measurement(penelopean_theater_mid, ulysses_chp18, theater_ratio, 30, 0.58).
narrative_ontology:measurement(penelopean_theater_end, ulysses_chp18, theater_ratio, 60, 0.68).

% Extraction over time
narrative_ontology:measurement(penelopean_extract_start, ulysses_chp18, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(penelopean_extract_mid, ulysses_chp18, base_extractiveness, 30, 0.3).
narrative_ontology:measurement(penelopean_extract_end, ulysses_chp18, base_extractiveness, 60, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp18, information_standard).
narrative_ontology:affects_constraint(ulysses_chp18, female_epistemic_suppression).
narrative_ontology:affects_constraint(ulysses_chp18, male_narrative_authority).
narrative_ontology:affects_constraint(ulysses_chp18, modernist_representation_techniques).

% DUAL FORMULATION NOTE:
% The Penelopean Affirmation decomposes into two related constraints: (1) Female embodied consciousness as an ineliminable fact (low ε, Mountain-like) and (2) The institutional extraction mechanism that makes that consciousness visible for male authorial and readerly control (ε=0.38, Tangled Rope). These are not the same constraint viewed differently; they have different ε values and different structural properties. The story focuses on the second constraint — the institutional arrangement that both validates and extracts from female interiority through literary representation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ulysses_chp18, analytical, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
