% ============================================================================
% CONSTRAINT STORY: reunification_amendments_1990__article_146_question_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reunification_amendments_1990__article_146_question_reading, []).

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
 *   constraint_id: reunification_amendments_1990__article_146_question_reading
 *   human_readable: Article 146 Question Reading: Constituent Power and the Path Not Taken
 *   domain: legal/constitutional_doctrine
 *
 * SUMMARY:
 *   The reunification of Germany in 1990 produced a constitutional problem
 *   that remains unresolved: the Basic Law was amended (via Article 79(2)) to
 *   incorporate five new Länder and update references to 'Germany,' but it
 *   was NOT replaced via Article 146's path of having 'the whole people'
 *   adopt a new constitution. Article 146 still stands textually, naming a
 *   path that was not taken. This reading asserts that the article's text
 *   remains open — the 'door' to constituent-power-driven constitutional
 *   replacement remains 'ajar' despite the institutional choice to proceed
 *   via accession and amendment rather than via Article 146's referendum
 *   mechanism. The constraint operates on the gap between what Article 146
 *   explicitly permits (a new constitution adopted by the whole people) and
 *   what actually occurred (amendment of the existing Basic Law by
 *   parliament). This gap is not merely historical or finished — it is an
 *   ongoing constitutional structure: the article persists as a living text
 *   that names an alternative legitimacy path, creating perpetual potential
 *   for revisionist challenge to the Basic-Law-as-permanent reading. The
 *   tension is between finality (the accession+amendment mechanism treats the
 *   Basic Law as complete and ongoing) and openness (Article 146 treats the
 *   Basic Law as explicitly provisional, awaiting potential replacement by
 *   genuine constituent-act). This reading of the kernel—the
 *   article_146_question_reading—instantiates constituent-power theory as the
 *   beneficiary (the reading validates constituent-power claims against
 *   institutional permanence) and Basic-Law-permanence doctrine as the victim
 *   (the reading continuously resurrects the question of the Basic Law's
 *   provisional rather than final status).
 *
 * KEY AGENTS:
 *   - Constituent Power Theorists & Revisionism Advocates: Primary beneficiaries (organized/constrained) — the Article 146 question reading supplies textual warrant for their theoretical position that the Basic Law is subordinate to constituent power
 *   - Basic Law Permanence Doctrine: Primary victim (institutional/constrained) — Article 146's continuing salience creates perpetual pressure on claims that the Basic Law's structure is final and immutable
 *   - The Unrealized Referendum Constituency: Secondary victim (powerless/trapped) — West and East German peoples who were never asked whether the Article 146 path was preferable; structurally displaced by the accession mechanism
 *   - Federal Constitutional Court & Parliamentary Consensus: Beneficiary-status institutional actor (institutional/arbitrage) — benefits from Article 146's coordination function (keeping constitutional succession questions structured and named) while enforcing institutional consensus against its invocation
 *   - Article 146 Text Itself: Performative structure (institutional/arbitrage) — the article supplies symbolic/theoretical standing without operative power (piton classification)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing suppressed constituent power as an immutable principle rather than recognizing institutional suppression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reunification_amendments_1990__article_146_question_reading, 0.38).
domain_priors:suppression_score(reunification_amendments_1990__article_146_question_reading, 0.52).
domain_priors:theater_ratio(reunification_amendments_1990__article_146_question_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reunification_amendments_1990__article_146_question_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(reunification_amendments_1990__article_146_question_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(reunification_amendments_1990__article_146_question_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reunification_amendments_1990__article_146_question_reading, tangled_rope).
narrative_ontology:human_readable(reunification_amendments_1990__article_146_question_reading, "Article 146 Question Reading: Constituent Power and the Path Not Taken").
narrative_ontology:topic_domain(reunification_amendments_1990__article_146_question_reading, "legal/constitutional_doctrine").

domain_priors:requires_active_enforcement(reunification_amendments_1990__article_146_question_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reunification_amendments_1990__article_146_question_reading, '8b334acc-6d8a-463a-9173-68c0569fc489').
narrative_ontology:cs_kernel_codification('8b334acc-6d8a-463a-9173-68c0569fc489', fixed_text).
narrative_ontology:cs_authority_grounding('8b334acc-6d8a-463a-9173-68c0569fc489', lineage).
narrative_ontology:cs_interpretation_layer_present('8b334acc-6d8a-463a-9173-68c0569fc489').
narrative_ontology:cs_reading_relation('8b334acc-6d8a-463a-9173-68c0569fc489', reunification_amendments_1990__accession_not_merger_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b334acc-6d8a-463a-9173-68c0569fc489', reunification_amendments_1990__treaty_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('8b334acc-6d8a-463a-9173-68c0569fc489', foundational, article_146_continues_operative_standing).
narrative_ontology:cs_axiom_status(article_146_continues_operative_standing, holdable).
narrative_ontology:cs_axiom_grounding('8b334acc-6d8a-463a-9173-68c0569fc489', article_146_continues_operative_standing, deontological).
narrative_ontology:cs_axiom('8b334acc-6d8a-463a-9173-68c0569fc489', foundational, constituent_power_subordinates_constitutional_form).
narrative_ontology:cs_axiom_status(constituent_power_subordinates_constitutional_form, holdable).
narrative_ontology:cs_axiom_grounding('8b334acc-6d8a-463a-9173-68c0569fc489', constituent_power_subordinates_constitutional_form, deontological).
narrative_ontology:cs_reference_frame('8b334acc-6d8a-463a-9173-68c0569fc489', constituent_power_normative_supremacy).
narrative_ontology:cs_drift_state('8b334acc-6d8a-463a-9173-68c0569fc489', contemporary_institutional_consensus, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8b334acc-6d8a-463a-9173-68c0569fc489', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(reunification_amendments_1990__article_146_question_reading, reunification_amendments_1990).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reunification_amendments_1990__article_146_question_reading, constituent_power_theorists).
narrative_ontology:constraint_beneficiary(reunification_amendments_1990__article_146_question_reading, constitutional_revisionism_advocates).
narrative_ontology:constraint_victim(reunification_amendments_1990__article_146_question_reading, basic_law_permanence_doctrine).
narrative_ontology:constraint_victim(reunification_amendments_1990__article_146_question_reading, unrealized_referendum_constituency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNREALIZED REFERENDUM CONSTITUENCY (SNARE) — The West and East German peoples were never asked whether Article 146's path (a new constitution adopted by the whole people) was preferable to the accession path actually taken. Trapped in a constitutional arrangement their structural position would have produced differently. Maximum extraction: the political choice was made FOR them, not WITH them. No exit from this particular constraint — the referendum never happened, and constitutional revision via the full Article 146 path is now politically infeasible.
constraint_indexing:constraint_classification(reunification_amendments_1990__article_146_question_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BASIC LAW PERMANENCE DOCTRINE THEORISTS (TANGLED ROPE) — Constrained by Article 146's textual standing, which supplies a coordination function (the text provides a named path for constitutional succession) alongside extraction (the article's continuing salience suppresses alternative framings of legitimacy). The doctrine benefits from having a clear text to defend, but faces ongoing extraction pressure from constituent-power theorists who invoke the same article to argue the Basic Law's sovereignty deficit. Mixed: genuine coordination (the article IS there, naming its successor) coexists with genuine asymmetric extraction (the permanence reading is continuously challenged by the text it purports to settle).
constraint_indexing:constraint_classification(reunification_amendments_1990__article_146_question_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL CONSTITUTIONAL COURT & PARLIAMENTARY CONSENSUS (ROPE) — Benefits from the coordination function Article 146 supplies (a named path, however dormant, for constitutional succession reduces legitimacy challenges to the current order). The institutional authority experiences Article 146 as a coordination mechanism: it keeps the succession question structured and channeled (into the formal amendment process), preventing more radical contestation. Low extraction — the institution has high exit capacity (can interpret the article permissively or strictly) and clear benefit from the naming/channeling function.
constraint_indexing:constraint_classification(reunification_amendments_1990__article_146_question_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUENT POWER THEORISTS & REVISIONISM ADVOCATES (TANGLED ROPE) — Experience Article 146 as both enabling and constraining. The article provides textual warrant for the constituent-power claim (the Basic Law explicitly contemplates its own replacement); this is genuine coordination of political theory. But the article is trapped in a permanent institutional structure (Basic Law eternity clauses, Article 79(3) prohibiting amendment of the federal system) that makes exercising Article 146's named path politically infeasible. They benefit from the article's validation of their theoretical position; they are extracted from by its institutional inaccessibility. Suppression runs high (the institutional consensus against Article 146 invocation), but not absolute (the article text itself remains available for reinterpretation).
constraint_indexing:constraint_classification(reunification_amendments_1990__article_146_question_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ARTICLE 146 ITSELF AS PERFORMATIVE STRUCTURE (PITON) — The article persists as a constitutional text with high symbolic but low operative force. It supplies a named path (coordination function) that is not actually available (institutional inertia prevents its invocation). The article's primary function is theatrical: it permits theorists to invoke constituent power without the institutional structure having to accommodate it. The text says 'the whole people can adopt a new constitution' but institutional consensus has rendered this path inaccessible. Theater ratio is high (0.68): the article's performative standing far exceeds its operative capacity.
constraint_indexing:constraint_classification(reunification_amendments_1990__article_146_question_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CONSTITUENT POWER NATURALISM (MOUNTAIN) — From a universal analytical perspective, constituent power (the basic authority of the people to constitute themselves politically) appears as an immutable principle of democratic sovereignty — a natural law of politics that cannot be suppressed by any legal text. This perspective sees Article 146 as confirming what must be true: the Basic Law is subordinate to constituent power, not sovereign over it. The legal text is merely naming an inevitability. However, the structural data contradicts this classification: the article's continuing dormancy despite constitutional textual standing, the institutional consensus against its invocation, and the deliberately designed eternity clauses that trap constituent power within permanent institutional boundaries suggest this is a false summit — constituent power is being naturalized when it is actually suppressed by institutional design.
constraint_indexing:constraint_classification(reunification_amendments_1990__article_146_question_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reunification_amendments_1990__article_146_question_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reunification_amendments_1990__article_146_question_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reunification_amendments_1990__article_146_question_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(reunification_amendments_1990__article_146_question_reading, TR),
    TR >= 0.70.

:- end_tests(reunification_amendments_1990__article_146_question_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low. The article_146_question_reading extracts from Basic-Law-permanence doctrine by continuously validating the claim that the Basic Law is provisional. But the extraction is not severe because the institutional structure has effectively neutralized Article 146's operative force — the reading has high theoretical standing but low political feasibility. The extracted 'resource' is legitimacy and doctrinal authority: the reading gains standing by invoking the text, while the permanence doctrine loses standing by defending a text that explicitly contemplates its own replacement. Suppression (0.52): Moderate-high. The institutional consensus against invoking Article 146 is substantial but not total — the text remains available for reinterpretation, and the theory-community can maintain the reading without legal penalty (academic freedom, theoretical pluralism). The suppression is enforced through institutional closure (the Constitutional Court would be unlikely to entertain Article 146-based challenges) and practical infeasibility (the eternity clauses make amendment-of-the-amendment procedurally daunting). Theater ratio (0.68): High and rising. The article has become increasingly performative over the 1990-2010 interval. In 1990, Article 146 was a live question (the accession path was explicitly chosen over the referendum path, making the article's status a genuine institutional choice). By 2010, Article 146 had settled into purely symbolic/theoretical standing — invoked by theorists but not seriously considered for actual invocation. The measurement trajectory shows theater increasing as operative force decreases.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival divergence is stark and revealing. The constituent-power theorists see a rope (coordination: Article 146 names a legitimate path; they can theoretically invoke it). The permanence doctrine sees a tangled_rope (mixed: the article coordinates succession questions but extracts legitimacy from permanence claims). The Federal Constitutional Court sees a rope (pure coordination: the article keeps the question structured). The unrealized constituency sees a snare (pure extraction: the referendum never happened, and the constitutional arrangement that resulted was chosen FOR them, not WITH them). The article itself, viewed performatively, is a piton (the text persists through institutional inertia, not actual function). The analytical observer who naturalizes constituent power sees a mountain (constituent power is an immutable principle) — but this is false-summit territory, because the data shows institutional suppression, not natural law. The gap reveals the reading's core claim: the door is ajar, but institutional consensus has decided not to walk through it.
 *
 * DIRECTIONALITY LOGIC:
 *   The article_146_question_reading's directionality is constructed from the structural position of constituent-power theory relative to institutional permanence. Constituent-power theorists are organized agents with constrained exit: they can maintain and develop the theoretical position but face institutional barriers to its practical implementation (the eternity clauses, the consensus against invocation). They are beneficiaries of the reading (it validates their theory) and victims of the institutional suppression (their theory lacks operative force). The Basic Law permanence doctrine is an institutional actor (institutional power) with high exit capacity (arbitrage: the doctrine can maintain or revise its theoretical commitments) but faces persistent extraction through the text's own language (Article 146 continuously undermines permanence claims). The unrealized referendum constituency is powerless with trapped exit: they were excluded from the choice, and the choice cannot be retroactively unmade or reframed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_146_invocation_feasibility,
    'Is Article 146''s path (new constitution by referendum) structurally inaccessible, or merely politically infeasible at present?',
    'Institutional analysis: empirical test would require determining whether constitutional amendment could remove the Article 79(3) eternity clauses. If amendment is conceptually possible, the path is constrained (political infeasibility); if amendment is constitutionally foreclosed, the path is trapped (structural inaccessibility). Counterfactual: would a supermajority committed to Article 146 invocation succeed in overriding the institutional consensus?',
    'If feasible: constituent-power reading''s suppression is moderate (constrained exit). If structurally inaccessible: suppression is high (trapped exit). Classification could shift from tangled_rope (moderate suppression) toward snare (high suppression) depending on which empirical answer holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_146_invocation_feasibility, empirical, 'Whether Article 146 path is structurally inaccessible or politically infeasible').

omega_variable(
    id_1990_constitutional_legitimacy_deficit,
    'Did the Article 79(3) path (accession + amendment) vs. the Article 146 path (new constitution by referendum) produce different legitimacy structures? Was the referendum that was NOT held a genuine structural alternative or a counterfactual artifact?',
    'Historical-comparative analysis: East German public opinion polling pre-unification regarding constitutional preference; legal theory analysis of whether Article 146 invocation would have been institutionally recognizable in 1990; interviews with framers of the Unification Treaty regarding why Article 146 was explicitly rejected.',
    'If referendum was a live structural alternative in 1990: extractiveness is higher (0.45+) and the snare classification is stronger for the unrealized-constituency perspective. If Article 146 was aspirational rather than feasible: extractiveness is lower (0.30-0.35) and the constraint is closer to piton (performative rather than extractive).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(id_1990_constitutional_legitimacy_deficit, empirical, 'Whether Article 146 path was a genuine structural alternative in 1990').

omega_variable(
    article_146_reading_forecast_contention,
    'This reading (Article 146 remains open, the door was left ajar) is ONE of three sibling readings of the reunification kernel. Which reading will dominate German constitutional discourse in the next generation? Will Article 146 be reinvoked, remain dormant, or be formally closed?',
    'Longitudinal institutional tracking: amendment attempts, court decisions, academic consensus shifts over the next 20-30 years. Key signal: whether any successor generation invokes Article 146 as warrant for constitutional revision (revival of the reading), or whether the basic law becomes formally immunized against Article 146-style challenge (foreclosure of the reading).',
    'Revival: the article_146_question_reading becomes doctrinally live; constituent-power theorists gain institutional standing; snare classification for unrealized-constituency could shift toward rope (recognized coordination path). Foreclosure: piton classification strengthens (pure performative); article becomes merely historical artifact; reading becomes ''overridden'' status in cs_structure axioms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_146_reading_forecast_contention, conceptual, 'Institutional future of Article 146 in German constitutional doctrine').

omega_variable(
    twin_reading_foreclosure_possibility,
    'Does THIS reading (Article 146 question) logically foreclose the accession_not_merger reading? Or do they coexist as party-dependent doctrinal commitments?',
    'Constitutional-law analysis: can both readings be held simultaneously within the framework of German constitutional doctrine? Answer: yes — one can maintain that accession did occur (factually) while also maintaining that Article 146 remains open (doctrinally). They address different aspects of the 1990 process: accession describes the legal mechanism; article_146_question_reading describes the normative possibility space left open.',
    'If coexists_with: both readings remain live doctrinal options. If forecloses: only one reading can be maintained within a single constitutional framework, and the other must be rejected. Current German constitutional consensus leans toward coexistence: both the accession mechanism and the article_146_question are acknowledged, with the institutional choice being to invoke accession (not merger/not treaty) while leaving Article 146 textually standing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(twin_reading_foreclosure_possibility, conceptual, 'Logical relationship between article_146_question reading and accession_not_merger reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reunification_amendments_1990__article_146_question_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reun146_tr_t0, reunification_amendments_1990__article_146_question_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(reun146_tr_t10, reunification_amendments_1990__article_146_question_reading, theater_ratio, 10, 0.62).
narrative_ontology:measurement(reun146_tr_t20, reunification_amendments_1990__article_146_question_reading, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(reun146_be_t0, reunification_amendments_1990__article_146_question_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(reun146_be_t10, reunification_amendments_1990__article_146_question_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(reun146_be_t20, reunification_amendments_1990__article_146_question_reading, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(reun146_su_t0, reunification_amendments_1990__article_146_question_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(reun146_su_t10, reunification_amendments_1990__article_146_question_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(reun146_su_t20, reunification_amendments_1990__article_146_question_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reunification_amendments_1990__article_146_question_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reunification_amendments_1990__article_146_question_reading, reunification_amendments_1990__accession_not_merger_reading).
narrative_ontology:affects_constraint(reunification_amendments_1990__article_146_question_reading, reunification_amendments_1990__treaty_constitutionalism_reading).
narrative_ontology:affects_constraint(reunification_amendments_1990__article_146_question_reading, german_constitutional_eternity_clauses).

% DUAL FORMULATION NOTE:
% The article_146_question_reading is one of three structurally distinct constraint stories that together model the contested reunification kernel. The three readings share a kernel (the constitutional meaning of 1990 unification) but produce different ε values and different beneficiary/victim structures. The accession_not_merger_reading models the reading favoring institutional continuity and legal stability (ε ~0.12, rope). The treaty_constitutionalism_reading models the reading favoring international instruments and bilateral constitution (ε ~0.34, tangled_rope). The article_146_question_reading (this story) models the reading favoring constituent power and constituent-act legitimacy (ε ~0.38, tangled_rope). The three stories are linked via network.affects_constraints to show the constraint family's internal dependencies. The article 146 constraint is downstream of both competing readings: if accession-doctrine dominates institutional consensus, Article 146 fades toward piton; if treaty constitutionalism is invoked, Article 146 is reframed as subordinate to treaty authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
