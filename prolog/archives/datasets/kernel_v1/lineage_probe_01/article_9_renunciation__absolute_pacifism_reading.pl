% ============================================================================
% CONSTRAINT STORY: article_9_renunciation__absolute_pacifism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_renunciation__absolute_pacifism_reading, []).

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
 *   constraint_id: article_9_renunciation__absolute_pacifism_reading
 *   human_readable: Article 9 Absolute Pacifism Reading: Constitutional Text vs. Practiced Rearmament
 *   domain: constitutional_law/doctrinal_interpretation
 *
 * SUMMARY:
 *   Article 9 of the Japanese Constitution (1947) renounces war as a
 *   sovereign right and forbids the maintenance of 'war potential.' The
 *   absolute pacifism reading takes this language at face value: the text is
 *   clear, explicit, and complete. The Self-Defense Forces, established in
 *   1954 and expanded continuously since, are structurally unconstitutional
 *   under this reading. The extraction mechanism is political: successive
 *   cabinets have reinterpreted Article 9 through executive authority rather
 *   than pursuing formal amendment under Article 96 (which requires
 *   two-thirds supermajority and public referendum). The most dramatic shift
 *   occurred in 2014, when the cabinet reinterpreted the constitution to
 *   permit collective self-defense — a move that changed Japanese security
 *   policy without changing the constitutional text. From the absolute
 *   pacifism reading's perspective, this is extraction via institutional
 *   power: the text's plain meaning has been outvoted by practice, maintained
 *   through suppression (avoiding the democratic amendment process) and
 *   theater (claiming constitutional fidelity while substantively amending
 *   the law). The constraint exhibits the diagnostic signature of a contested
 *   constitutional kernel: multiple readings coexist (pacifism, self-defense
 *   interpretation, cabinet reinterpretation), each grounded in the same text
 *   but with incommensurable conclusions.
 *
 * KEY AGENTS:
 *   - Pacifist Constitutional Reading: Primary beneficiary (institutional/arbitrage) — its integrity depends on the text's plain meaning being honored, but it has been largely outvoted by institutional practice
 *   - Self-Defense Forces and Defense Ministry: Primary beneficiary-in-practice (institutional/arbitrage) — benefits from executive reinterpretation that permits their existence and expansion without formal amendment
 *   - Constitutional Text of Article 9: Primary victim (powerless/trapped) — bears the structural tension between its plain meaning and institutional override without remedy or amendment
 *   - Pacifist Scholars and Advocates: Secondary victim (moderate/constrained) — their reading remains doctrinally available but politically marginalized; constrained by institutional power imbalances
 *   - Article 96 Amendment Movement: Organized agents (organized/constrained) — advocates for formal constitutional amendment to resolve the contradiction through democratic process
 *   - Cabinet and Executive Branch: Institutional beneficiary (institutional/arbitrage) — exercises interpretive authority to maintain defense policy without submitting to the supermajority requirement of formal amendment
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the contingent post-war pacifist commitment as an immutable constraint on state sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_renunciation__absolute_pacifism_reading, 0.58).
domain_priors:suppression_score(article_9_renunciation__absolute_pacifism_reading, 0.72).
domain_priors:theater_ratio(article_9_renunciation__absolute_pacifism_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_renunciation__absolute_pacifism_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(article_9_renunciation__absolute_pacifism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_9_renunciation__absolute_pacifism_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_renunciation__absolute_pacifism_reading, tangled_rope).
narrative_ontology:human_readable(article_9_renunciation__absolute_pacifism_reading, "Article 9 Absolute Pacifism Reading: Constitutional Text vs. Practiced Rearmament").
narrative_ontology:topic_domain(article_9_renunciation__absolute_pacifism_reading, "constitutional_law/doctrinal_interpretation").

domain_priors:requires_active_enforcement(article_9_renunciation__absolute_pacifism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_renunciation__absolute_pacifism_reading, '59b04a59-dc2d-4fd2-9ed9-685d800a422f').
narrative_ontology:cs_kernel_codification('59b04a59-dc2d-4fd2-9ed9-685d800a422f', fixed_text).
narrative_ontology:cs_authority_grounding('59b04a59-dc2d-4fd2-9ed9-685d800a422f', lineage).
narrative_ontology:cs_interpretation_layer_present('59b04a59-dc2d-4fd2-9ed9-685d800a422f').
narrative_ontology:cs_reading_relation('59b04a59-dc2d-4fd2-9ed9-685d800a422f', article_9_renunciation__self_defense_interpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('59b04a59-dc2d-4fd2-9ed9-685d800a422f', article_9_renunciation__reinterpretation_2014_reading, coexists_with).
narrative_ontology:cs_axiom('59b04a59-dc2d-4fd2-9ed9-685d800a422f', foundational, article_9_plain_text_binding).
narrative_ontology:cs_axiom_status(article_9_plain_text_binding, holdable).
narrative_ontology:cs_axiom_grounding('59b04a59-dc2d-4fd2-9ed9-685d800a422f', article_9_plain_text_binding, deontological).
narrative_ontology:cs_axiom('59b04a59-dc2d-4fd2-9ed9-685d800a422f', foundational, formal_amendment_democratic_requirement).
narrative_ontology:cs_axiom_status(formal_amendment_democratic_requirement, holdable).
narrative_ontology:cs_axiom_grounding('59b04a59-dc2d-4fd2-9ed9-685d800a422f', formal_amendment_democratic_requirement, conventional).
narrative_ontology:cs_reference_frame('59b04a59-dc2d-4fd2-9ed9-685d800a422f', post_war_pacifist_commitment).
narrative_ontology:cs_drift_state('59b04a59-dc2d-4fd2-9ed9-685d800a422f', contemporary_post_2014_reinterpretation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('59b04a59-dc2d-4fd2-9ed9-685d800a422f', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(article_9_renunciation__absolute_pacifism_reading, article_9_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_renunciation__absolute_pacifism_reading, pacifist_constitutional_reading).
narrative_ontology:constraint_victim(article_9_renunciation__absolute_pacifism_reading, self_defense_forces_constitutional_status).
narrative_ontology:constraint_victim(article_9_renunciation__absolute_pacifism_reading, textual_integrity_of_article_9).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUTIONAL TEXT (SNARE) — The plain language of Article 9 ('war as a sovereign right... is renounced. The threat or use of force as means of settling international disputes is renounced... land, sea, and air forces, as well as other war potential, will never be maintained') has no exit from the SDF's existence. The text is structurally trapped: it says what it says, but practice has overridden it without formal amendment. Maximum extraction — the text bears all the cost of this contradiction without remedy.
constraint_indexing:constraint_classification(article_9_renunciation__absolute_pacifism_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PACIFIST SCHOLARS (TANGLED ROPE) — Constrained by institutional power imbalances: law schools teach both readings, but policy-making has selected the self-defense interpretation. The pacifist reading provides a genuine coordination function (it articulates the constitutional commitment to peace and renunciation) while simultaneously experiencing extraction (policy ignores this reading despite textual support). Scholars benefit from doctrinal coherence but suffer from political marginalization.
constraint_indexing:constraint_classification(article_9_renunciation__absolute_pacifism_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SELF-DEFENSE FORCES (ROPE) — Experiences the constraint as coordination: the defense establishment has navigated the Article 9 contradiction by redefining 'war potential' and 'self-defense' through executive interpretation rather than formal amendment. The SDF benefits from institutional arbitrage — they can maintain forces without constitutional amendment, claiming fidelity to the text through reinterpretation. This is coordination (enabling defense policy) layered with extraction (using executive authority to avoid democratic amendment).
constraint_indexing:constraint_classification(article_9_renunciation__absolute_pacifism_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: AMENDMENT ADVOCATES (SCAFFOLD) — Organized actors (constitutional scholars, some politicians, civil society groups) see a sunset pathway: formal amendment under Article 96 to explicitly authorize defense forces. This removes the performative contradiction between text and practice. The movement faces high constraints (two-thirds supermajority, public referendum) but has genuine agency and a defined exit. Amendment would resolve the extraction by making the authorization explicit.
constraint_indexing:constraint_classification(article_9_renunciation__absolute_pacifism_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CABINET RITUAL (PITON) — The 2014 cabinet decision reinterpreting Article 9 to permit collective self-defense is substantially performative: it invokes the appearance of constitutional fidelity while substantively amending the constitution through executive decree. The ritual persists through institutional inertia — avoiding the democratic burden of formal amendment — rather than because it resolves the underlying contradiction. The SDF operates under executive reinterpretation that the pacifist reading explicitly rejects, maintained through theater rather than through resolved disagreement.
constraint_indexing:constraint_classification(article_9_renunciation__absolute_pacifism_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational view, some defense capacity is inherent to state survival: no polity can renounce all capacity to defend itself and remain a polity. The constraint appears as an immutable property of political existence. However, the structural data reveals this as a false summit: the mountain classification naturalizes what is actually a contingent, negotiated institutional arrangement. Japan's post-war pacifism was chosen; the constitutional text was explicit; the violation is a doctrinal choice, not a natural law.
constraint_indexing:constraint_classification(article_9_renunciation__absolute_pacifism_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_renunciation__absolute_pacifism_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(article_9_renunciation__absolute_pacifism_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article_9_renunciation__absolute_pacifism_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_renunciation__absolute_pacifism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(article_9_renunciation__absolute_pacifism_reading, TR),
    TR >= 0.70.

:- end_tests(article_9_renunciation__absolute_pacifism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The absolute pacifism reading holds that the constitution explicitly forbids war potential, yet the SDF has grown from a constrained police reserve (1954) to a fully capable military establishment. This growth represents ongoing extraction: each expansion occurs under reinterpreted authority rather than democratically renewed commitment. The extractiveness value reflects the magnitude of the gap between textual meaning and institutional reality, compounded by the use of executive authority to avoid formal amendment. Theater ratio (0.65): Moderate-high. The cabinet's invocation of 'reinterpretation' rather than 'amendment' is performative: it preserves the appearance of constitutional fidelity while substantively changing the law. The 2014 collective self-defense decision exemplifies this theater — it claimed to merely interpret existing authority while crossing a major line in Japanese security doctrine. Suppression (0.72): High. Multiple suppression mechanisms operate: (1) democratic suppression — the two-thirds supermajority and referendum requirements make formal amendment difficult, creating incentive for executive end-runs; (2) interpretive suppression — constitutional courts have largely deferred to executive reinterpretation (consistent with Japanese judicial review doctrine); (3) epistemic suppression — law schools teach multiple readings as equally valid, marginalizing the pacifist reading despite its textual foundation. The measurements show increasing extractiveness, theater, and suppression over the interval from 1947 (constitutional adoption) to 2017 (post-2014 reinterpretation stabilization). At t=0, extractiveness was low (pacifism was policy). At t=25 (1970s, SDF expansion began), extractiveness rose as practice diverged from text. At t=70 (2017), extractiveness stabilized at 0.58 as the institutional arrangement settled into routine cabinet reinterpretation.
 *
 * PERSPECTIVAL GAP:
 *   The absolute pacifism reading produces a perspectival gap with the self-defense interpretation reading at the institutional level. From the defense ministry perspective, Article 9 permits self-defense through reinterpretation (Rope): the constraint is a coordination problem solved by clarifying that the text does not prohibit minimum necessary defense. From the pacifist reading perspective, the text is plain and the SDF's existence is extraction (Snare or Tangled Rope): coordination would require formal amendment, not executive reinterpretation. The analytical observer risks seeing a natural law (Mountain) — that all states require defense capacity — which naturalizes what is actually a post-war political choice to constrain military power. The cabinet ritual (Piton) reveals the theater: the reinterpretation invokes constitutional legitimacy while avoiding democratic accountability. The amendment movement (Scaffold) sees a sunset pathway: formal Article 96 amendment would resolve the contradiction by making defense authorization explicit, shifting the constraint from extraction to acknowledged coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value derives from the agent's structural relationship to the extraction flow. The constitutional text itself (trapped/powerless) experiences maximum d ≈ 0.95: it bears all the cost of the contradiction without remedy. Pacifist scholars (constrained/moderate) experience d ≈ 0.65: they have some voice in legal discourse but are marginalized in policy-making. The defense ministry (arbitrage/institutional) experiences low d ≈ 0.10: they are the primary beneficiary of executive reinterpretation, which permits their expansion without democratic override. The amendment movement (constrained/organized) experiences d ≈ 0.50: they have agency (organized) but face high barriers (supermajority requirement). The cabinet ritual (arbitrage/institutional) experiences near-zero d: it is the mechanism of extraction itself. The analytical observer (analytical/analytical) experiences canonical d ≈ 0.72: the analysis must work hard to avoid the false mountain classification that naturalizes institutional choice as law.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by clarifying that the absolute pacifism reading is ONE valid reading of a contested kernel, not THE reading. The mandatrophy question is not 'which classification is correct?' but 'how do different readings of the same constitutional text produce different structural analyses?' The tangled_rope classification holds for this reading because it shows genuine coordination function (the text articulates a commitment to peace) layered with extraction (policy ignores the text's plain meaning without formal amendment). The false mountain classification (natural law of state survival) is exposed through structural analysis: Japan chose pacifism; the constitution was explicit; the violation is doctrinal choice, not immutable constraint. The pacifist reading is internally coherent and doctrinally valid, even though it has been outvoted by institutional practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_versus_practice_incommensurability,
    'Can the plain text of Article 9 (''war potential will never be maintained'') coexist with the SDF''s existence as a state institution, or is the contradiction genuinely irresolvable within a single constitutional framework?',
    'Hermeneutic analysis: examine whether any coherent reading of ''war potential'' and ''self-defense forces'' can satisfy both the text and the institutional reality without redefining terms beyond recognition. Compare interpretive moves to established constitutional jurisprudence on living constitutionalism.',
    'If coexistence is possible: the self-defense interpretation is valid, absolute pacifism is one among equally legitimate readings, and the constraint reclassifies as Rope (coordination). If incommensurable: the absolute pacifism reading is structurally correct, practice has violated the constitution without amendment, and the constraint remains Tangled Rope or Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_versus_practice_incommensurability, conceptual, 'Whether textual meaning and institutional practice can coexist within a single framework').

omega_variable(
    authority_of_cabinet_reinterpretation,
    'Does executive reinterpretation of the constitution without formal amendment constitute amendment in fact, or does it preserve constitutional supremacy by remaining technically within interpretive authority?',
    'Comparative constitutional law: examine whether other democracies recognize executive reinterpretation as a valid amendment mechanism. Analyze the 2014 decision''s deviation from prior constitutional interpretation precedent to quantify the magnitude of the shift.',
    'If amendment in fact: the SDF''s legal basis is ultra vires, the cabinet exceeded constitutional authority, and pacifist scholars are correct that the text''s meaning has been outvoted (extraction via institutional power). If within interpretive authority: cabinet reinterpretation is a legitimate constitutional function, and the shift is justified by changing security circumstances.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_of_cabinet_reinterpretation, conceptual, 'Whether cabinet reinterpretation constitutes amendment without Article 96').

omega_variable(
    pacifist_reading_reversibility,
    'If the absolute pacifism reading were legally reinstated (via Article 96 amendment to delete the military, or via court reversal of 2014 cabinet decision), what would be the structural and geopolitical consequences, and does anticipated consequence justify the current extractive arrangement?',
    'Counterfactual analysis: model regional security dynamics if Japan were to enforce Article 9 literally (no SDF). Compare to historical scenarios (pre-1950 Japan, post-WW2 pacifist period). Assess whether the reading is genuinely irreversible or merely politically costly.',
    'If reversible at acceptable cost: the extraction is a political choice, not inevitable, and the tangled_rope classification holds. If irreversible or catastrophically costly: the extraction may be structurally necessary (transforms to rope or scaffold with justified oversight), and pacifism becomes performative rather than binding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pacifist_reading_reversibility, empirical, 'Whether literal Article 9 enforcement is geopolitically feasible').

omega_variable(
    reading_kernel_identity_contest,
    'Is the kernel of Article 9 genuinely contested among three incommensurable readings (absolute pacifism, self-defense interpretation, 2014 cabinet reinterpretation), or do the readings fall into a hierarchy where one is the authoritative interpretation and the others are marginalized doctrines?',
    'Institutionalist analysis: examine which reading is embedded in law school curricula, government policy, court precedent, and public law textbooks. Assess whether the pacifist reading retains live force in constitutional adjudication or has become a historical curiosity. Map the authority structure''s differential treatment of each reading.',
    'If genuinely contested: three separate constraint stories (three readings) with coexists_with relations. If hierarchical: one reading has consolidated authority, others are subordinated, and the relation shifts from coexists_with to influences or forecloses. This determines the constraint family structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_identity_contest, empirical, 'Whether the kernel admits three live readings or has collapsed to one').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_renunciation__absolute_pacifism_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_renunciation__absolute_pacifism_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(arti_tr_t25, article_9_renunciation__absolute_pacifism_reading, theater_ratio, 25, 0.55).
narrative_ontology:measurement(arti_tr_t70, article_9_renunciation__absolute_pacifism_reading, theater_ratio, 70, 0.65).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_renunciation__absolute_pacifism_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(arti_be_t25, article_9_renunciation__absolute_pacifism_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(arti_be_t70, article_9_renunciation__absolute_pacifism_reading, base_extractiveness, 70, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_renunciation__absolute_pacifism_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(arti_su_t25, article_9_renunciation__absolute_pacifism_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement(arti_su_t70, article_9_renunciation__absolute_pacifism_reading, suppression_requirement, 70, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_renunciation__absolute_pacifism_reading, identity_coordination).
narrative_ontology:affects_constraint(article_9_renunciation__absolute_pacifism_reading, article_9_renunciation__self_defense_interpretation_reading).
narrative_ontology:affects_constraint(article_9_renunciation__absolute_pacifism_reading, article_9_renunciation__reinterpretation_2014_reading).
narrative_ontology:affects_constraint(article_9_renunciation__absolute_pacifism_reading, collective_self_defense_cabinet_authority).

% DUAL FORMULATION NOTE:
% The absolute pacifism reading, self-defense interpretation reading, and 2014 cabinet reinterpretation reading form a constraint family decomposed along reading lines per ε-invariance. Each reading has distinct extractiveness (0.58 for pacifism, ~0.35 for self-defense interpretation, ~0.42 for cabinet reinterpretation) reflecting different structural assessments of the Article 9 kernel. The three readings coexist as live doctrinal positions but with different institutional authority and policy consequences. They are linked via network.affects_constraints and the cs_structure.reading_relations framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_9_renunciation__absolute_pacifism_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
