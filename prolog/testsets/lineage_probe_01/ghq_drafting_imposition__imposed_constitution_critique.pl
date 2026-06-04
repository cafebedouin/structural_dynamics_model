% ============================================================================
% CONSTRAINT STORY: ghq_drafting_imposition__imposed_constitution_critique
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ghq_drafting_imposition__imposed_constitution_critique, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ghq_drafting_imposition__imposed_constitution_critique
 *   human_readable: GHQ Imposed Constitution: Occupation Document Reading
 *   domain: legal/constitutional/doctrinal
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of a contested kernel: the
 *   question of whether the 1947 Japanese Constitution is legitimately
 *   binding given its origin under military occupation. The reading you are
 *   holding — the imposed_constitution_critique — asserts that the
 *   constitution's fundamental defect is not substantive (the content may be
 *   meritorious) but structural: it was drafted by foreigners in English in
 *   six days and enacted under duress, suppressing national authorship and
 *   autonomy at the founding. This reading sees the constraint as a Snare
 *   from the perspective of the postwar nation: trapped under a text it did
 *   not author, forced to live within a legitimacy framework that denies the
 *   occupation's reality, unable to revise without violating the very
 *   authority structure that claims to bind it. The competing siblings
 *   (embraced_revolution_reading: the nation made it theirs through seven
 *   decades of defensive refusal to amend; hybrid_authorship_reading: the
 *   drafting was a negotiation, not a dictation, with substantive Japanese
 *   inputs) offer different framings of the same historical events. Each
 *   reading constitutes a different constraint with a different beneficiary
 *   structure. This story traces the imposed_constitution_critique alone, as
 *   a self-contained ε-invariant claim.
 *
 * KEY AGENTS:
 *   - Postwar Nation / Occupied State (powerless/trapped): Primary victim — bears full extraction cost of duress-imposed legitimacy, forced to govern under an alien text without exit option during occupation period
 *   - GHQ / Occupation Authority (institutional/arbitrage): Beneficiary of the duress-imposition — controls the founding moment, benefits from rapid constitutional settlement on occupier's terms, extracts political stability from foreign-authored legitimacy
 *   - Japanese National Authorship (powerless/trapped): Victim set — suppressed at the founding by the English-language drafting process and occupier timeline, unable to participate in constitutional authorship
 *   - Revisionist Constitutional Politics (institutional/arbitrage): Secondary beneficiary — benefits from the occupation-narrative framing, which provides rhetorical grounds for constitutional revision arguments
 *   - Constitutional Legitimacy Institutions (institutional/arbitrage): Piton perspective — maintain the legitimacy performance across seven decades despite (or because of) the occupation shadow, extracting stability from institutional embedding
 *   - Constitutional Reform Movement (organized/mobile): Organized victim-faction — sees the constraint as temporary and revisable, pursues exit through Amendment Article 96
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ghq_drafting_imposition__imposed_constitution_critique, 0.62).
domain_priors:suppression_score(ghq_drafting_imposition__imposed_constitution_critique, 0.68).
domain_priors:theater_ratio(ghq_drafting_imposition__imposed_constitution_critique, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ghq_drafting_imposition__imposed_constitution_critique, extractiveness, 0.62).
narrative_ontology:constraint_metric(ghq_drafting_imposition__imposed_constitution_critique, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ghq_drafting_imposition__imposed_constitution_critique, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ghq_drafting_imposition__imposed_constitution_critique, snare).
narrative_ontology:human_readable(ghq_drafting_imposition__imposed_constitution_critique, "GHQ Imposed Constitution: Occupation Document Reading").
narrative_ontology:topic_domain(ghq_drafting_imposition__imposed_constitution_critique, "legal/constitutional/doctrinal").

domain_priors:requires_active_enforcement(ghq_drafting_imposition__imposed_constitution_critique).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ghq_drafting_imposition__imposed_constitution_critique, '2a945fac-6bb2-4b2a-8a54-6a7d187bf970').
narrative_ontology:cs_kernel_codification('2a945fac-6bb2-4b2a-8a54-6a7d187bf970', fixed_text).
narrative_ontology:cs_authority_grounding('2a945fac-6bb2-4b2a-8a54-6a7d187bf970', lineage).
narrative_ontology:cs_interpretation_layer_present('2a945fac-6bb2-4b2a-8a54-6a7d187bf970').
narrative_ontology:cs_reading_relation('2a945fac-6bb2-4b2a-8a54-6a7d187bf970', ghq_drafting_imposition__embraced_revolution_reading, coexists_with).
narrative_ontology:cs_reading_relation('2a945fac-6bb2-4b2a-8a54-6a7d187bf970', ghq_drafting_imposition__hybrid_authorship_reading, influences).
narrative_ontology:cs_axiom('2a945fac-6bb2-4b2a-8a54-6a7d187bf970', foundational, occupation_duress_illegitimacy).
narrative_ontology:cs_axiom_status(occupation_duress_illegitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2a945fac-6bb2-4b2a-8a54-6a7d187bf970', occupation_duress_illegitimacy, deontological).
narrative_ontology:cs_axiom('2a945fac-6bb2-4b2a-8a54-6a7d187bf970', secondary, national_authorship_suppression).
narrative_ontology:cs_axiom_status(national_authorship_suppression, holdable).
narrative_ontology:cs_axiom_grounding('2a945fac-6bb2-4b2a-8a54-6a7d187bf970', national_authorship_suppression, empirically_contingent).
narrative_ontology:cs_reference_frame('2a945fac-6bb2-4b2a-8a54-6a7d187bf970', occupied_nation_under_duress).
narrative_ontology:cs_drift_state('2a945fac-6bb2-4b2a-8a54-6a7d187bf970', contemporary_normalization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2a945fac-6bb2-4b2a-8a54-6a7d187bf970', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(ghq_drafting_imposition__imposed_constitution_critique, ghq_drafting_imposition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ghq_drafting_imposition__imposed_constitution_critique, revisionist_constitutional_politics).
narrative_ontology:constraint_beneficiary(ghq_drafting_imposition__imposed_constitution_critique, occupation_narrative).
narrative_ontology:constraint_victim(ghq_drafting_imposition__imposed_constitution_critique, indigenous_legitimacy_claim).
narrative_ontology:constraint_victim(ghq_drafting_imposition__imposed_constitution_critique, national_authorship).
narrative_ontology:constraint_victim(ghq_drafting_imposition__imposed_constitution_critique, postwar_sovereign_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POSTWAR NATION UNDER OCCUPATION (SNARE) — Trapped by the fact of military occupation and the duress under which the constitutional framework was enacted. Cannot unilaterally revise the fundamental law without violating the legitimacy framework that grounds it. Bears the full extraction cost: forced to live under a text imposed in six days by foreign actors in English, in a language most citizens cannot read. The material reality of military occupation suppresses alternatives to ratification.
constraint_indexing:constraint_classification(ghq_drafting_imposition__imposed_constitution_critique, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSTITUTIONAL REFORM ADVOCATES (TANGLED ROPE) — Constrained by the need to maintain constitutional legitimacy while critiquing the founding. These agents benefit from the constitution's substantive content (democratic protections, social rights) while bearing the extraction cost of its imposed legitimacy. The constraint coordinates genuine constitutional discourse (deliberation over the document's merits and defects) while extracting the cost of duress denial — advocates must argue for revision while accepting the occupied founding as binding precedent.
constraint_indexing:constraint_classification(ghq_drafting_imposition__imposed_constitution_critique, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HISTORICAL REVISIONISTS / OCCUPATION NARRATIVE (ROPE) — Benefits from the pure coordination function: establishing that the constitution's founding was a duress-imposed document solves a collective action problem among scholars and political actors seeking to document the occupation's reality. No significant suppression required — the narrative coordination is voluntary. The beneficiary here is not a corrupt actor but a factual account (occupation did occur, GHQ did draft in haste, English was the original language, duress was material). Rope classification reflects the genuine coordination value of historical clarity.
constraint_indexing:constraint_classification(ghq_drafting_imposition__imposed_constitution_critique, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSTITUTIONAL LEGITIMACY THEATER (PITON) — The constitutional text persists as the supreme law not because the occupation reading is suppressed or unknown, but because the performance of treating it as legitimate — through amendments within its Article 96 framework, through citation in courts, through civic education — has become detached from its founding claim. The legitimacy ritual persists through institutional inertia. The high theater_ratio (0.55) reflects that much of the constitution's authority now derives from seven decades of institutional use and citizen embrace, not from its original authority claim.
constraint_indexing:constraint_classification(ghq_drafting_imposition__imposed_constitution_critique, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL REVISION MOVEMENT (SCAFFOLD) — Organized actors arguing for explicit constitutional revision see this constraint as temporary: a transitional condition between the occupation's imposition and genuine postwar constitutional sovereignty. The constraint has a sunset clause embedded in Article 96 itself — the amendment mechanism that could permit thorough revision once political consensus shifts. Mobile exit options reflect that the organized coalition can pursue revision through legitimate constitutional process, though at high political cost.
constraint_indexing:constraint_classification(ghq_drafting_imposition__imposed_constitution_critique, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, occupation imposes constraints that appear structurally immutable: the victor's military power at T0 (1945) created a lock-in condition. No alternative was materially available to the occupied nation at the founding moment. This perspective treats the imposed constitution as a natural law of military occupation — irreversible given the power asymmetry at T0. However, the FSM engine will flag this as a false summit: the 'immutable' framing naturalizes what is actually a political choice about how to manage the occupation's legacy across seven decades.
constraint_indexing:constraint_classification(ghq_drafting_imposition__imposed_constitution_critique, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ghq_drafting_imposition__imposed_constitution_critique_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ghq_drafting_imposition__imposed_constitution_critique, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ghq_drafting_imposition__imposed_constitution_critique, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ghq_drafting_imposition__imposed_constitution_critique, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ghq_drafting_imposition__imposed_constitution_critique, TR),
    TR >= 0.70.

:- end_tests(ghq_drafting_imposition__imposed_constitution_critique_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderately high but not maximal. The constraint extracts from the occupied nation the cost of denying its own occupation: citizens must treat a foreign-authored document as their own supreme law, without the legitimacy claim that self-authorship would provide. The extraction is not total (0.78 at T0 during active occupation) because institutional embedding and seven-decade citizen embrace have provided partial legitimation. As time passes, the extractiveness decays slightly toward 0.62 as the shadow of occupation recedes and the constraint's force depends more on institutional inertia than on active suppression. Suppression (0.68): High. The constraint suppresses alternative constitutional narratives at the founding moment (T0 = 0.85, under active military occupation) and continues to suppress them through institutional and discursive means, though less absolutely as the regime embeds (0.68 at T50). Suppression remains substantial across the interval because the constitution's legitimacy claim rests on denying that it was imposed — acknowledging the occupation-narrative requires the constitutional order to confront its own origin defect. Theater ratio (0.55): Moderate. The constraint involves both real functional suppression (at T0, military occupation was genuine; the nation had no exit option) and performative legitimation (by T50, the constitutional text's authority derives substantially from being-treated-as-legitimate by institutions, not from original consent). The rising theater trajectory (0.30 → 0.55) reflects increasing institutionalization: the occupation's material power diminishes, but the legitimacy performance must intensify to maintain the constraint without material force.
 *
 * PERSPECTIVAL GAP:
 *   The six perspectives produce a wide gap because they occupy radically different structural positions relative to the occupation-imposition constraint. The postwar nation sees a Snare — trapped under alien law with maximum extraction. The reform movement sees a Tangled Rope — genuine constitutional coordination alongside origin-extraction. The revisionist narrative sees a Rope — pure coordination function (establishing historical truth). Constitutional institutions see a Piton — the legitimacy ritual persists through inertia and institutional embedding despite the occupation shadow. The revision movement sees a Scaffold — a temporary condition with a sunset path through Amendment Article 96. The civilizational analytical observer risks seeing a Mountain — the immutability of postwar settlement — but the FSM engine will identify this as a false summit naturalizing a political choice. The gap reveals that 'what is the constitution?' receives different answers depending on whether you are trapped under it, embedded in it, narrating it, or observing it from outside.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupied nation experiences maximum directionality toward extraction (d ≈ 0.95) at the founding moment, because the nation is the clear victim of duress and has no exit options. The occupation authority experiences minimum directionality (d ≈ 0.10) because it benefits from the arrangement and faces no suppression. As time passes and institutional embedding occurs, the directionality landscape becomes more complex: reform advocates occupy an intermediate position (d ≈ 0.55) — they benefit from the constitution's substantive content but bear the extraction cost of its origin duress. Constitutional institutions maintain low directionality (d ≈ 0.15) because they are beneficiaries of the legitimacy performance and have stakes in maintaining it. No directionality overrides are required — the structural data (occupation fact, national suppression, beneficiary profile of the occupation narrative) drives d automatically through the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the occupation-imposition reading is one coherent account among others, each legitimate within its reference frame. The mandatrophy is not 'is the constitution legitimate?' but 'from which position is legitimacy being claimed?' From the perspective of the occupation nation at T0, the answer is clear: No, it is a Snare. From the perspective of institutional embedding at T50, the answer is different: the legitimacy performance has succeeded through seven decades of use, making the piton classification appropriate. From the perspective of comparative constitutional law (analytical), the answer is: all postwar constitutions face origin questions; the Japanese case is not unique. The mandatrophy resolves by acknowledging that all answers are structurally coherent — the perspective-relative classification system IS the answer. No single type 'wins.' The presheaf over the observation site determines the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    duress_temporal_boundary,
    'At what historical moment does duress at the founding cease to constitute a structural constraint on legitimacy claims?',
    'Comparative analysis of postwar constitutions (West Germany, Italy, Japan): when did each society transition from ''imposed document'' framings to ''adopted framework'' framings? What institutional events or generational shifts marked the transition?',
    'If duress persists structurally across decades (no marked transition): snare classification is stable and extracted legitimacy persists. If duress dissolves through institutional embedding (seven-decade adoption): classification shifts toward piton (inertia) and the occupation constraint relaxes into a coordination problem about constitutional updating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duress_temporal_boundary, empirical, 'Temporal boundary of duress-imposed legitimacy defect').

omega_variable(
    english_language_suppression_mechanism,
    'Did the fact that the original draft was authored in English (not Japanese) materially suppress Japanese voices from the drafting process, or was this a technical accident of occupation logistics?',
    'Historical analysis: recovery of Japanese draft proposals submitted to GHQ, comparison to final English text, testimony from Japanese delegates about revision opportunities, linguistic analysis of how concepts translated or failed to translate between the Japanese proposals and English final form.',
    'If suppression was material and systematic (Japanese proposals rejected, vocabulary choices enforced by English-language constraint): extractiveness rises toward 0.75 (full snare). If English was neutral conveyance and Japanese proposals significantly shaped the outcome: extractiveness drops toward 0.40 (tangled rope becomes dominant).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(english_language_suppression_mechanism, empirical, 'Whether English-language drafting materially suppressed Japanese national authorship').

omega_variable(
    autonomy_merit_decoupling,
    'Can a constitution drafted under duress be simultaneously illegitimate in its origin and substantively meritorious in its content? Or does origin-duress taint the entire normative claim?',
    'Normative analysis: examination of competing philosophical positions on constitutional legitimacy (consent theories, historical theories, consequentialist merit theories) and how each handles origin defects. Empirical comparison: study how Japanese society actually resolved this — did adoption-through-refusal-to-amend constitute a post-hoc legitimation, or does the occupation shadow persist even across seven decades of institutional embedding?',
    'If autonomy and merit are decoupled (this reading holds): revision is required regardless of content quality, because autonomy is the prior claim. If they are coupled: content quality can redeem origin duress, and the reading becomes a position among others rather than a structural requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_merit_decoupling, conceptual, 'Decoupling of autonomy requirement from constitutional merit').

omega_variable(
    kernel_reading_contest,
    'Is this the reading of a genuinely contested kernel, or does the historical record establish a single correct account of the drafting?',
    'Examination of primary sources (GHQ memos, Japanese Diet records, delegate testimony, MacArthur''s papers), consensus in mainstream scholarship, and persistence of dissenting readings in contemporary Japanese political discourse.',
    'If the kernel is genuinely contested (multiple readings coexist in good faith): this reading is one live option among others, and sibling readings (embraced_revolution, hybrid_authorship) coexist with it. If historical scholarship has settled the factual questions: this reading''s empirical claims require revision, and the classification shifts based on what the settled facts establish.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Epistemic status of the kernel as genuinely contested vs. historically settled').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ghq_drafting_imposition__imposed_constitution_critique, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ghq_imposed_theater_t0_functional_duress, ghq_drafting_imposition__imposed_constitution_critique, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ghq_imposed_theater_t20_institutional_embedding, ghq_drafting_imposition__imposed_constitution_critique, theater_ratio, 20, 0.48).
narrative_ontology:measurement(ghq_imposed_theater_t50_legitimacy_performance, ghq_drafting_imposition__imposed_constitution_critique, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(ghq_imposed_extractiveness_t0_occupation_moment, ghq_drafting_imposition__imposed_constitution_critique, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(ghq_imposed_extractiveness_t20_postwar_settling, ghq_drafting_imposition__imposed_constitution_critique, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(ghq_imposed_extractiveness_t50_normalization, ghq_drafting_imposition__imposed_constitution_critique, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ghq_imposed_suppression_t0_military_occupation, ghq_drafting_imposition__imposed_constitution_critique, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(ghq_imposed_suppression_t20_embedded_in_institutions, ghq_drafting_imposition__imposed_constitution_critique, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(ghq_imposed_suppression_t50_normalized_constraint, ghq_drafting_imposition__imposed_constitution_critique, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ghq_drafting_imposition__imposed_constitution_critique, identity_coordination).
narrative_ontology:affects_constraint(ghq_drafting_imposition__imposed_constitution_critique, ghq_drafting_imposition__embraced_revolution_reading).
narrative_ontology:affects_constraint(ghq_drafting_imposition__imposed_constitution_critique, ghq_drafting_imposition__hybrid_authorship_reading).

% DUAL FORMULATION NOTE:
% The GHQ drafting imposition kernel decomposes into three structurally distinct constraint stories, each with its own ε value and beneficiary/victim structure. The imposed_constitution_critique reading (this story) has ε=0.62 and frames the constraint as a Snare centered on suppressed national authorship. The embraced_revolution_reading has different ε (lower, because the seven decades of institutional embedding and refusal-to-amend provides ex-post legitimation) and frames the same events as Rope/Scaffold. The hybrid_authorship_reading has yet different ε (lower still, because Japanese participation in the drafting reduces the suppression measure) and frames the founding as Tangled Rope. All three stories describe aspects of the same historical kernel, but each reading constitutes a logically distinct constraint claim with different metrics, different beneficiary structures, and different terminal classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
