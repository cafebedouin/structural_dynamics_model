% ============================================================================
% CONSTRAINT STORY: japanese_constitution_1947__ghq_drafting_imposition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_japanese_constitution_1947__ghq_drafting_imposition, []).

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
 *   constraint_id: japanese_constitution_1947__ghq_drafting_imposition
 *   human_readable: The 1947 Japanese Constitution: GHQ Drafting and Imposition
 *   domain: political/legal/constitutional_authority
 *
 * SUMMARY:
 *   The 1947 Japanese Constitution was authored by MacArthur's General
 *   Headquarters staff in a single week (February 1945), translated into
 *   Japanese by the Japanese government, and presented as Japan's own
 *   constitutional choice through ratification by the Diet and submission to
 *   the people under the emperor's authority. This reading instantiates the
 *   contested claim that the Constitution is fundamentally an imposed
 *   document—the founding choice was exercised by the occupying power, not by
 *   Japanese constitutional actors. The Matsumoto draft, prepared by the
 *   Japanese government itself, was overwritten without substantive
 *   negotiation. The constraint operates at three levels: (1) the immediate
 *   suppression of autochthonous constitution-making in 1945-47; (2) the
 *   installation of a constitutional baseline that embeds occupation policy
 *   objectives (Article 9, rights catalog, emperor as symbol); (3) the
 *   perpetual requirement to suppress the origin story in order to maintain
 *   the Constitution's domestic legitimacy. The extractiveness decreases over
 *   the interval as the Constitution becomes embedded through institutional
 *   use and generational succession, but suppression and theater increase—the
 *   constraint's mechanism shifts from raw coercion to institutional inertia
 *   and legitimacy ritual.
 *
 * KEY AGENTS:
 *   - GHQ Reform Program (Institutional/Arbitrage): Primary beneficiary—the occupation's strategic interests in war renunciation, rights expansion, and emperor repositioning are locked in constitutional form. Frames the constraint as coordination and reform.
 *   - Japanese Government Apparatus (Moderate/Constrained): Intermediary victim—complicit in presentation but stripped of authorship. Faces extraction (loss of constitutional autonomy) and coordination benefit (ability to govern through law rather than military decree).
 *   - Japanese Constitutional Autonomy (Powerless/Trapped): Primary victim—the capacity for autochthonous constitution-making is suppressed entirely. The Matsumoto draft is rejected without negotiation. Experiences maximum extraction.
 *   - Post-Occupation Japanese Polity (Organized/Constrained): Downstream victim—inherits the imposed constitution as the legitimate baseline. Over time, institutional embedding and amendment capacity reduce extraction, but the founding legitimacy claim requires suppression of the imposition origins.
 *   - Constitutional Legitimacy Ritual (Institutional/Mobile): The myth that the 1947 Constitution is 'Japan's own'—ratified by the Diet, submitted to the people, bearing the emperor's seal—is substantially performative. The underlying fact is GHQ drafting. The ritual persists and functions through institutional inertia.
 *   - Analytical Observer (Analytical/Analytical): The civilizational perspective that sees occupation-imposed law as an inevitable fact of military victory, not a contingent institutional choice. Risks naturalizing coerced extraction as immutable law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(japanese_constitution_1947__ghq_drafting_imposition, 0.62).
domain_priors:suppression_score(japanese_constitution_1947__ghq_drafting_imposition, 0.68).
domain_priors:theater_ratio(japanese_constitution_1947__ghq_drafting_imposition, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(japanese_constitution_1947__ghq_drafting_imposition, extractiveness, 0.62).
narrative_ontology:constraint_metric(japanese_constitution_1947__ghq_drafting_imposition, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(japanese_constitution_1947__ghq_drafting_imposition, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(japanese_constitution_1947__ghq_drafting_imposition, tangled_rope).
narrative_ontology:human_readable(japanese_constitution_1947__ghq_drafting_imposition, "The 1947 Japanese Constitution: GHQ Drafting and Imposition").
narrative_ontology:topic_domain(japanese_constitution_1947__ghq_drafting_imposition, "political/legal/constitutional_authority").

domain_priors:requires_active_enforcement(japanese_constitution_1947__ghq_drafting_imposition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(japanese_constitution_1947__ghq_drafting_imposition, 'aa09cfa5-40af-4ad1-813a-8f5434979f02').
narrative_ontology:cs_kernel_codification('aa09cfa5-40af-4ad1-813a-8f5434979f02', formalized).
narrative_ontology:cs_authority_grounding('aa09cfa5-40af-4ad1-813a-8f5434979f02', extraction).
narrative_ontology:cs_interpretation_layer_present('aa09cfa5-40af-4ad1-813a-8f5434979f02').
narrative_ontology:cs_reading_relation('aa09cfa5-40af-4ad1-813a-8f5434979f02', japanese_constitution_1947__article_9_renunciation, influences).
narrative_ontology:cs_reading_relation('aa09cfa5-40af-4ad1-813a-8f5434979f02', japanese_constitution_1947__rights_catalog_1947, influences).
narrative_ontology:cs_reading_relation('aa09cfa5-40af-4ad1-813a-8f5434979f02', japanese_constitution_1947__symbol_emperor, influences).
narrative_ontology:cs_axiom('aa09cfa5-40af-4ad1-813a-8f5434979f02', foundational, ghe_authority_exercised_founding_choice).
narrative_ontology:cs_axiom_status(ghe_authority_exercised_founding_choice, holdable).
narrative_ontology:cs_axiom_grounding('aa09cfa5-40af-4ad1-813a-8f5434979f02', ghe_authority_exercised_founding_choice, empirically_contingent).
narrative_ontology:cs_axiom('aa09cfa5-40af-4ad1-813a-8f5434979f02', foundational, legitimacy_suppression_maintains_extraction).
narrative_ontology:cs_axiom_status(legitimacy_suppression_maintains_extraction, holdable).
narrative_ontology:cs_axiom_grounding('aa09cfa5-40af-4ad1-813a-8f5434979f02', legitimacy_suppression_maintains_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('aa09cfa5-40af-4ad1-813a-8f5434979f02', japanese_constitutional_autonomy).
narrative_ontology:cs_drift_state('aa09cfa5-40af-4ad1-813a-8f5434979f02', post_occupation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aa09cfa5-40af-4ad1-813a-8f5434979f02', '').
narrative_ontology:cs_kernel_id(japanese_constitution_1947__ghq_drafting_imposition, japanese_constitution_1947).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(japanese_constitution_1947__ghq_drafting_imposition, occupation_reform_program).
narrative_ontology:constraint_beneficiary(japanese_constitution_1947__ghq_drafting_imposition, allied_strategic_interests).
narrative_ontology:constraint_victim(japanese_constitution_1947__ghq_drafting_imposition, domestic_constitutional_autonomy).
narrative_ontology:constraint_victim(japanese_constitution_1947__ghq_drafting_imposition, matsumoto_draft_alternative).
narrative_ontology:constraint_victim(japanese_constitution_1947__ghq_drafting_imposition, japanese_sovereignty_in_founding).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The capacity for autochthonous constitution-making is suppressed entirely. Japan cannot reject the imposed draft without military/political catastrophe. The Matsumoto draft (prepared by the Japanese government itself) is overwritten without negotiation. Maximum extraction: the founding choice is exercised by the occupier; the victim bears the cost of losing control over the constitutional baseline.
constraint_indexing:constraint_classification(japanese_constitution_1947__ghq_drafting_imposition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% The Japanese government faces severe constraints: reject and face occupation authority; accept and be complicit in the imposition. But they also experience a coordination benefit — the GHQ draft provides institutional legitimacy and legal machinery that enables governance under occupation. They gain the ability to administer through constitutional forms rather than military decree. Mixed extraction and coordination: constrained by military power but enabled by the legal framework.
constraint_indexing:constraint_classification(japanese_constitution_1947__ghq_drafting_imposition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The occupation's strategic interests benefit directly: a constitution written to embed reforms (war renunciation, rights expansion, emperor as symbol) locks in the occupation's policy objectives. The program experiences this as pure coordination—communicating and implementing the reform agenda. Arbitrage exit options mean GHQ can modify or abandon the occupation without losing face; the constitution secures their objectives regardless. Net beneficiary.
constraint_indexing:constraint_classification(japanese_constitution_1947__ghq_drafting_imposition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% After occupation ends, the Japanese state faces a structured choice: the imposed constitution becomes the framework they inherited. They can amend it (constrained by the amendment procedure and post-occupation politics), but cannot easily reject it without reopening the foundational legitimacy question. The constraint persists as institutional inertia—the constitution, once embedded, becomes the legitimate baseline regardless of its imposed origins. Organized agents (political parties, civil society) experience both extraction (lack of authorship) and coordination (the stable legal framework enables governance). Extraction declines over time as ownership becomes less salient than functionality.
constraint_indexing:constraint_classification(japanese_constitution_1947__ghq_drafting_imposition, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The narrative that the 1947 Constitution is 'Japan's own' constitution—ratified by the Diet, submitted to the people, bearing the emperor's seal—is substantially performative. The underlying fact is imposition by occupying authority. Yet the ritual persists and functions: the constitution achieves practical legitimacy through use and amendment, and the historical fact of GHQ authorship becomes a matter of scholarly contention rather than political revocation. Theater ratio is moderate (0.58) because the constitution also possesses genuine functionality—it is not merely theater. But the legitimacy claim ('this is our constitution') requires suppression of the origin story.
constraint_indexing:constraint_classification(japanese_constitution_1947__ghq_drafting_imposition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% From a civilizational analytical perspective, occupation always produces imposed law—the victor's authority is a fact of war, and constitutional rewriting is inherent to military victory. This perspective sees the GHQ drafting as an unavoidable law of military occupation, not a contingent institutional choice. However, the beneficiaries (occupation reform program, allied interests) are identifiable, and the suppression mechanism is active and coercive. The engine's false summit detector will classify this as a naturalized extraction constraint, not a genuine immutable law.
constraint_indexing:constraint_classification(japanese_constitution_1947__ghq_drafting_imposition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(japanese_constitution_1947__ghq_drafting_imposition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(japanese_constitution_1947__ghq_drafting_imposition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(japanese_constitution_1947__ghq_drafting_imposition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(japanese_constitution_1947__ghq_drafting_imposition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(japanese_constitution_1947__ghq_drafting_imposition, TR),
    TR >= 0.70.

:- end_tests(japanese_constitution_1947__ghq_drafting_imposition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 at interval end, declining from 0.78 at onset): The extraction is initially maximal (0.78) because the occupation authority exercises the founding choice entirely and suppresses the Japanese government's own draft without negotiation. The extractiveness declines (0.72 at ratification in 1947, 0.62 at occupation end in 1952) as the Constitution becomes embedded through use and institutional legitimacy accumulates. The measuring point is 1952 (occupation end), reflecting the constraint's function during the occupation period and immediate post-occupation transition. If the measurement extended further (to 2026), extractiveness would decline further as generational turnover and constitutional amendment capacity increase Japanese authorship retroactively. But the interval [0,7] captures the founding extraction mechanism. Suppression (0.68 overall, declining slightly from 0.85): The suppression is the enforced gap between the formal origin story (the Constitution is Japan's own, ratified by the Diet) and the structural fact (it was written by GHQ in one week). Raw military coercion (0.85 at occupation onset) is gradually replaced by institutional coercion—the embedded baseline that is too costly to reject—and finally by legitimacy coercion (the suppression required to maintain that the Constitution is autochthonous). Theater ratio (0.58 overall, rising from 0.25 to 0.68): The theatrical content increases over the interval as the Constitution's presentation becomes increasingly formal and ritually performed. At occupation onset (t=0), the theater is minimal (0.25)—military occupation is explicitly coercive, not disguised. By ratification (t=2), theater rises (0.58) because the Constitution is now presented through formal legal procedures (Diet ratification, Diet vote) that perform Japanese autochthonous adoption. By occupation end (t=7), theater is highest (0.68) because the Constitution's legitimacy depends entirely on the suppression of the imposition origin story. Theater ratio does not reach piton threshold (0.70), indicating that the Constitution retains genuine functionality—it is not merely performative—but the legitimacy claim requires substantial performative work.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces a perspectival structure that reveals how the same founding event is classified radically differently depending on the observer's structural position. GHQ sees coordination (Rope): they are solving the post-war reform problem through constitutional embedding. The Japanese government sees mixed extraction and coordination (Tangled Rope): they are constrained by military power but enabled by the legal framework to govern. Japanese constitutional autonomy sees pure extraction (Snare): the founding choice is stolen, and there is no exit. Post-occupation political actors see degraded institutional practice (Piton): the constitutional legitimacy claim requires suppression of origins, but the Constitution has become functionally legitimate through time. The analytical observer risks seeing an immutable law of occupation (Mountain false summit): military victory always produces imposed law. The perspectival gap demonstrates that the constraint is real and extractive for the powerless victim (constitutional autonomy) but gradually disappears as institutional embedding and generational turnover convert extraction into coordination and legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from the agent's structural position: Who benefits? Who bears costs? What are the exit options? GHQ (institutional/arbitrage) derives d ≈ 0.05 (beneficiary + arbitrage exit = minimal extraction experienced). Japanese government (moderate/constrained) derives d ≈ 0.50 (both benefits from governance framework and costs from lost authorship + constrained exit = symmetric extraction). Japanese autonomy (powerless/trapped) derives d ≈ 0.95 (victim + trapped exit = maximal extraction). Post-occupation polity (organized/constrained) derives d ≈ 0.55 (victim of founding imposition but also benefits from stable constitutional framework + constrained exit = moderate-to-high extraction with coordination benefits). Constitutional legitimacy ritual (institutional/mobile) derives d ≈ 0.40 (neither pure beneficiary nor pure victim; maintains the myth that benefits Japanese legitimacy while suppressing the extraction fact). Analytical observer (analytical/analytical) derives d ≈ 0.72 (neither beneficiary nor victim; observes the structure from outside; risks naturalizing the constraint). The directionality chain confirms the tangled_rope classification: the constraint exhibits both genuine coordination function (governing through law, providing institutional stability) and asymmetric extraction (Japanese constitutional autonomy is suppressed, founding choice is exercised by the occupier).
 *
 * MANDATROPHY ANALYSIS:
 *   READING-SPECIFIC MANDATROPHY: This reading instantiates a classical mandatrophy: is the 1947 Constitution a coordinating institution that solved Japan's post-war governance problem (Rope from the beneficiary's perspective), or an extractive imposition that suppressed Japanese constitutional autonomy (Snare from the victim's perspective)? The tangled_rope classification resolves the mandatrophy by recognizing that BOTH are true: the Constitution provides genuine governance coordination AND represents a founding extraction. The constraint is hybrid, not mislabeled coordination masquerading as extraction. The mandatrophy is resolved by the perspectival structure: no single classification is 'correct'—the presheaf of perspectives over the observation site is the answer. Perspectives that experience the constraint as coordination (GHQ, later Japanese governments) classify it as rope. Perspectives that experience it as extraction (Japanese constitutional autonomy, scholars focused on imposition origins) classify it as snare. The analytical observer must resist the temptation to naturalize the occupation-imposed law as an immutable feature of military victory (mountain false summit) and instead recognize that the founding choice was made by specific actors (GHQ reform program) who benefited from a specific institutional outcome (the 1947 Constitution). METHODOLOGICALLY: This reading's relationship to its sibling kernels (Article 9 renunciation, rights catalog, symbol-emperor) confirms the multi-reading framework. Each sibling reading focuses on a different structural claim (military disarmament, rights regime, sovereignty relocation), and this reading focuses on the origin and ownership of the whole. The readings do not foreclose each other; they coexist and influence each other. The decomposition is justified by the ε-invariance principle: each claim has its own extractiveness and its own structural mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    matsumoto_draft_counterfactual,
    'If the Japanese government''s Matsumoto draft had been accepted and refined through negotiation, would the resulting constitution have had structurally similar reform content (war renunciation, rights expansion) or fundamentally different character?',
    'Textual analysis of the Matsumoto draft vs. the 1947 Constitution; comparison of reform vectors in post-war European constitutions (West Germany, Italy) drafted under occupation vs. post-war Japanese constitutions in later periods (if Japan had rejected 1947 and drafted independently). Identification of which reforms were specifically GHQ innovations vs. which represented post-war consensus reform globally.',
    'If Matsumoto draft would have produced similar reforms: imposition is morally significant but strategically redundant — extraction mechanism is political domination, not substantive policy divergence. If Matsumoto draft would have diverged sharply: GHQ drafting imposed specific policy outcomes that a domestic process would have rejected, strengthening the snare classification. If post-1952 amendments reveal sustained Japanese resistance to 1947 provisions: confirms extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(matsumoto_draft_counterfactual, empirical, 'Whether Matsumoto draft would have produced similar reform content').

omega_variable(
    legitimacy_accumulation_over_time,
    'Does the 1947 Constitution''s legitimacy increase as it becomes embedded through use, amendment, and generational turnover, independent of its imposed origins?',
    'Longitudinal public opinion data on constitutional legitimacy: pre-1952 (occupation period), 1952-1970 (immediate post-occupation), 1980-2000 (second/third generation), 2000-present (fourth+ generation). Measurement of how many Japanese citizens are aware of GHQ authorship vs. treat the constitution as autochthonous. Analysis of amendment patterns and political invocations of constitutional authority (does the source of authority shift from occupation to accumulated legitimacy?).',
    'If legitimacy increases significantly post-occupation independent of origin story: the snare classification degrades toward tangled_rope or scaffold as the constraint''s extraction mechanism weakens. If legitimacy remains contested or declines: the snare classification persists. If awareness of GHQ drafting correlates with legitimacy erosion: confirms that suppression of the origin story is necessary for the constraint''s function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_accumulation_over_time, empirical, 'Post-occupation legitimacy accumulation of the 1947 Constitution').

omega_variable(
    reading_specification_ambiguity,
    'Is this reading about the historical fact of GHQ drafting in 1945-47, or about the ongoing constraint imposed by the 1947 Constitution as a founding text that defines post-war Japanese statehood?',
    'Clarify the temporal scope: ε=0.62 reflects the period 1945-1952 (occupation + immediate post-occupation). If the reading is about the constraint at t=2026 (current legitimacy of the imposed constitution), ε would be lower—the mechanism has degraded through institutional embedding. Separate stories would be justified: one for the imposition mechanism (high ε, high suppression), one for the post-occupation constitution (lower ε, legitimacy effects).',
    'If scope is 1945-1952 only: ε=0.62, snare classification for powerless agents. If scope is perpetual (the constitution remains an imposition constraint forever): ε declines post-1952 as legitimacy accumulates. If scope is kernel-level (the 1947 Constitution as a contested origin claim in perpetuity): ε is the suppression required to maintain a legitimacy claim against knowledge of the imposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_specification_ambiguity, conceptual, 'Temporal scope of the imposition constraint: 1945-1952 or perpetual?').

omega_variable(
    occupation_strategic_necessity,
    'Was the rapid GHQ drafting (one week) strategically necessary due to post-war instability and time pressure, or was it a choice to prevent domestic alternatives?',
    'Historical analysis of the occupation timeline, competing constitutional drafts under consideration, and stated rationales from MacArthur''s correspondence and memoirs. Comparison with post-war occupation drafting timelines in Germany, Austria, and South Korea (how long did those processes take?). Analysis of whether a longer deliberation period with Japanese government input would have risked political instability or military conflict.',
    'If time pressure was genuine and significant: the snare classification softens—coercion is constrained by necessity, not arbitrary extraction. The constraint moves toward tangled_rope if the speed served both occupation reform objectives AND Japanese post-war stability. If the rapid timeline was chosen to prevent domestic alternatives: confirms the extraction mechanism and snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupation_strategic_necessity, empirical, 'Whether one-week drafting timeline was strategically necessary or chosen to suppress alternatives').

omega_variable(
    article_9_embedded_extraction,
    'Does Article 9 (war renunciation) represent an extraction mechanism that benefits the occupation, or a genuine shared reform objective that Japan would have adopted independently?',
    'Textual and historical analysis: did pre-war Japanese political thought include war-renunciation advocates? Did the Matsumoto draft include any limitation on military power? Comparison with post-war German Basic Law (which did NOT renounce war)—if Germany negotiated retention of military capacity while Japan did not, the difference reveals GHQ imposition. If post-1952 amendment politics show persistent Japanese pressure to revise Article 9, this confirms the extraction claim. If Article 9 becomes a source of national pride (as it has in some constituencies), the piton mechanism becomes active—performative acceptance of an imposed provision.',
    'If Article 9 was GHQ-imposed against Japanese preferences: high extraction, confirms snare. If it represented shared post-war pacifism: lower extraction, constraint degrades toward rope/coordination. If it is now accepted domestically but remains strategically valuable to prevent Japanese rearmament: extraction persists through the mechanism of legitimacy-suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_9_embedded_extraction, empirical, 'Whether Article 9 reflects GHQ extraction or shared reform objective').

omega_variable(
    reading_vs_sibling_kernels,
    'How does this reading (GHQ drafting imposition) relate structurally to the sibling readings (Article 9 renunciation, rights catalog, emperor symbolism)? Do they represent independent constraints or a single decomposed kernel?',
    'Each sibling reading claims a different structural claim: Article 9 focuses on military disarmament, rights catalog on individual dignity, symbol-emperor on sovereignty relocation. This reading (GHQ imposition) focuses on the origin and ownership of the whole. The readings are not independently verifiable—they are different aspects of the same founding event. The decomposition follows the ε-invariance principle: each reading has its own ε (Article 9''s extractiveness reflects the military constraint; rights catalog''s extractiveness reflects the rights regime; emperor''s reflects the sovereignty claim). This reading''s ε (0.62) reflects the extraction involved in the founding choice itself—the suppression of autochthonous constitution-making.',
    'Confirms the kernel-decomposition framing: the 1947 Constitution is not a single constraint but a presheaf of structurally distinct claims. Each reading is a different structural element of the same founding. The readings coexist and influence each other but do not foreclose each other.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_sibling_kernels, conceptual, 'Relationship between this reading and sibling kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(japanese_constitution_1947__ghq_drafting_imposition, 0, 7).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1945_raw_military_rule, japanese_constitution_1947__ghq_drafting_imposition, theater_ratio, 0, 0.25).
narrative_ontology:measurement(theater_1947_constitutional_performance, japanese_constitution_1947__ghq_drafting_imposition, theater_ratio, 2, 0.58).
narrative_ontology:measurement(theater_1952_legitimacy_ritual, japanese_constitution_1947__ghq_drafting_imposition, theater_ratio, 7, 0.68).

% Extraction over time
narrative_ontology:measurement(extraction_1945_occupation_onset, japanese_constitution_1947__ghq_drafting_imposition, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(extraction_1947_ratification, japanese_constitution_1947__ghq_drafting_imposition, base_extractiveness, 2, 0.72).
narrative_ontology:measurement(extraction_1952_occupation_end, japanese_constitution_1947__ghq_drafting_imposition, base_extractiveness, 7, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(suppression_1945_raw_coercion, japanese_constitution_1947__ghq_drafting_imposition, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(suppression_1947_formalization, japanese_constitution_1947__ghq_drafting_imposition, suppression_requirement, 2, 0.75).
narrative_ontology:measurement(suppression_1952_institutional_embedding, japanese_constitution_1947__ghq_drafting_imposition, suppression_requirement, 7, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(japanese_constitution_1947__ghq_drafting_imposition, enforcement_mechanism).
narrative_ontology:affects_constraint(japanese_constitution_1947__ghq_drafting_imposition, article_9_renunciation).
narrative_ontology:affects_constraint(japanese_constitution_1947__ghq_drafting_imposition, rights_catalog_1947).
narrative_ontology:affects_constraint(japanese_constitution_1947__ghq_drafting_imposition, symbol_emperor).
narrative_ontology:affects_constraint(japanese_constitution_1947__ghq_drafting_imposition, occupation_reform_program_japan).
narrative_ontology:affects_constraint(japanese_constitution_1947__ghq_drafting_imposition, postwar_japanese_sovereignty).

% DUAL FORMULATION NOTE:
% The 1947 Japanese Constitution is a contested kernel decomposed into four structural readings: article_9_renunciation (military disarmament), rights_catalog_1947 (rights expansion), symbol_emperor (sovereignty relocation), and ghq_drafting_imposition (origin and ownership). Each reading has its own ε, its own perspectives, and its own classification type. This reading (ghq_drafting_imposition) focuses on the founding choice extraction—the suppression of autochthonous constitution-making and the beneficiary status of the occupation reform program. The readings coexist and influence each other: this reading's classification as tangled_rope (mixed coordination and extraction) is the upstream constraint that enables the sibling readings to have their specific ε values. The occupation's imposition mechanism is the structural foundation for all downstream claims about Article 9, rights, and emperor repositioning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
