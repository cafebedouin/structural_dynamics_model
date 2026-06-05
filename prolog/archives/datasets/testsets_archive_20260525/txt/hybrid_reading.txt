% ============================================================================
% CONSTRAINT STORY: hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hybrid_reading
 *   human_readable: Hybrid Classical-Post-Classical Latin Standard (Institutional Accommodation Reading)
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   The hybrid Classical-post-Classical Latin standard represents one
 *   institutional reading of a contested kernel: how should users of Latin
 *   relate to Classical norms given that Latin has evolved significantly in
 *   ecclesiastical, legal, and technical domains since the Classical period?
 *   This reading instantiates a specific compromise: Classical authority is
 *   maintained as a pedagogical and prestige anchor, but post-Classical
 *   technical vocabulary (ecclesiastical neologisms, medieval legal
 *   terminology, scientific coinages) is selectively legitimized as
 *   'functional developments' rather than 'corruptions.' This creates a mixed
 *   coordination-extraction hybrid. The constraint coordinates institutional
 *   Latin use (universities, churches, legal systems need a usable standard)
 *   while extracting from those whose actual linguistic practice extends
 *   beyond the legitimized subset — dialect speakers, radical reformers, and
 *   those using broader medieval developments find their language
 *   delegitimized as 'barbaric' or 'degenerate.' The constraint's theater
 *   ratio (0.58, rising from 0.42) reflects that much of the enforcement
 *   relies on performative assertion of Classical superiority rather than
 *   functional necessity. The ecclesiastical and legal domains communicate
 *   perfectly well in their post-Classical forms; the constraint persists
 *   because institutional prestige and educational authority depend on
 *   maintaining Classical hierarchy.
 *
 * KEY AGENTS:
 *   - Institutional Adopters (institutional/arbitrage): Universities, ecclesiastical communities, legal codifiers. Beneficiaries — can use Latin for functional purposes while maintaining Classical prestige.
 *   - Dialect Speakers / Regional Users (powerless/trapped): Those whose actual linguistic practice extends beyond the legitimized post-Classical subset. Victims — face systematic delegitimization despite communicative functionality.
 *   - Reform-Minded Philologists (moderate/constrained): Scholars whose empirical research reveals legitimate post-Classical development but face institutional pressure to maintain Classical authority. Mixed extraction of intellectual labor toward prestige rather than evidence.
 *   - Ecclesiastical Communities (institutional/constrained): Specialized users who benefit from legitimized ecclesiastical vocabulary but remain subordinate to Classical authority in prestige hierarchy.
 *   - Philological Modernization Coalition (organized/constrained): Comparative linguists, historical linguists building evidence-based frameworks. See the hybrid reading as temporary accommodation with sunset.
 *   - Classical Prestige Maintenance System (institutional/arbitrage): Educational curricula, prestige academies, Classical pedagogy. Benefits from maintained hierarchy; enforces through theater.
 *   - Analytical Observer: Risks naturalizing institutional prestige hierarchy as linguistic inevitability (false summit candidate).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_reading, 0.38).
domain_priors:suppression_score(hybrid_reading, 0.52).
domain_priors:theater_ratio(hybrid_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(hybrid_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(hybrid_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_reading, tangled_rope).
narrative_ontology:human_readable(hybrid_reading, "Hybrid Classical-Post-Classical Latin Standard (Institutional Accommodation Reading)").
narrative_ontology:topic_domain(hybrid_reading, "historical_linguistics/philology/commitment_systems").

domain_priors:requires_active_enforcement(hybrid_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_reading, institutional_adopters).
narrative_ontology:constraint_beneficiary(hybrid_reading, ecclesiastical_communities).
narrative_ontology:constraint_victim(hybrid_reading, radical_reformers).
narrative_ontology:constraint_victim(hybrid_reading, dialect_speakers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIALECT SPEAKER / REGIONAL USER (SNARE) — Trapped in non-standard forms; faces systematic delegitimization of legitimate post-Classical technical vocabulary (medieval legal terms, ecclesiastical coinages, regional developments). No meaningful exit from the constraint — must either suppress native forms or accept stigmatization. Maximum extraction: linguistic autonomy sacrificed to institutional norm.
constraint_indexing:constraint_classification(hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REFORM-MINDED PHILOLOGIST (TANGLED ROPE) — Constrained by institutional pressure to maintain Classical authority while driven by empirical linguistic evidence of legitimate post-Classical development. Benefits from the constraint (career prestige via Classical expertise) but also bears costs (theoretical inadequacy, need to suppress own research findings). Significant extraction of intellectual labor toward institutional rather than empirical authority.
constraint_indexing:constraint_classification(hybrid_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL ADOPTER (ROPE) — Universities, ecclesiastical institutions, legal codifiers benefit from the hybrid standard. Classical authority legitimates their Latin use while domain-specific accommodations (ecclesiastical vocabulary, legal terminology) permit genuine communication. Experiences the constraint as coordination: the hybrid reading enables institutional Latin use without requiring speakers to entirely abandon functional post-Classical developments. Net beneficiary.
constraint_indexing:constraint_classification(hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PHILOLOGICAL MODERNIZATION COALITION (SCAFFOLD) — Organized agents (Humanist scholars, comparative philologists, linguistic historians) see the hybrid standard as a temporary accommodation with a sunset: full integration of post-Classical developments into a descriptive, evidence-based Latin standard. Low effective extraction because the coalition has agency and sees an evolutionary path. Sunset logic: as historical linguistics matures, the Classical authority claim becomes increasingly untenable; replacement by empirically-grounded historical periodization.
constraint_indexing:constraint_classification(hybrid_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CLASSICAL PRESTIGE MAINTENANCE SYSTEM (PITON) — The constraint persists through institutional theater rather than functional necessity. Educational curricula teach Classical Latin as the standard while acknowledging post-Classical reality only in specialized courses. Ecclesiastical and legal Latin function perfectly well in their domains, yet are treated as degradations rather than legitimate descendants. The performative assertion of Classical supremacy (via terminology like 'barbarism,' 'corruption,' 'degeneracy') substitutes for actual standards-based argument. Theater ratio reflects this performative content.
constraint_indexing:constraint_classification(hybrid_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — From a civilizational perspective, the hybrid reading can appear as a natural law: any living language contains internal tensions between innovation and tradition; Classical-post-Classical divergence is inherent to language evolution. However, this naturalizes what is actually a contingent institutional choice about which forms to legitimize and which to suppress. The engine's false summit detector will identify this as a reading that benefits specific institutional actors (Classical educators, prestige-maintaining academies) disguised as immutable linguistic fact.
constraint_indexing:constraint_classification(hybrid_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hybrid_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hybrid_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hybrid_reading, TR),
    TR >= 0.70.

:- end_tests(hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38, rising to 0.38 at end): Moderate and rising. The hybrid reading legitimizes some post-Classical development (reducing extraction compared to strict Ciceronian purism), but maintains suppression of forms outside the curated subset (creating extraction compared to continuity reading that would accept all post-Classical Latin). The trajectory shows accumulating extraction: initially (year 0, Byzantine period) post-Classical Latin was used without systematic delegitimization; by year 250 (medieval period) ecclesiastical and legal domains had developed specialized vocabulary but institutional suppression was informal; by year 500 (late medieval/early modern) the suppression became formal and performative (prestige hierarchies, grammars marking deviations). Suppression (0.52): Moderate-high. Barriers to use of non-legitimized post-Classical forms include institutional gate-keeping (university curricula demand Classical models), prestige penalties (speakers marked as uneducated or 'barbaric'), and educational misdirection (students learn Classical as the standard, post-Classical as degradation). However, suppression is not total — ecclesiastical and legal Latin remain functionally autonomous; they are suppressed in prestige, not in use. Theater ratio (0.58, rising from 0.42): Moderate-high and rising. The constraint's enforcement increasingly relies on performative assertion rather than functional necessity. Ecclesiastical Latin serves its domain perfectly well without Classical validation; the theater consists of the repeated assertion that Classical forms are superior, more 'correct,' more 'pure.' The rise reflects that as medieval Latin became more established and functionally autonomous, institutional suppression increasingly took the form of performative prestige claims rather than functional arguments.
 *
 * PERSPECTIVAL GAP:
 *   The hybrid reading generates substantial perspectival gaps across indexical positions. The institutional adopters see coordination (Rope) — the hybrid standard enables their Latin use. The organized philological coalition sees a temporary problem with a sunset (Scaffold) — historical linguistics will eventually displace the prestige hierarchy. The dialect speaker sees pure extraction (Snare) — their legitimate linguistic forms are delegitimized with no exit. The reform-minded philologist sees mixed extraction (Tangled Rope) — they benefit from Classical expertise but suffer from intellectual suppression. The prestige maintenance system sees its own degraded ritual (Piton) — performative Classical assertions substitute for functional necessity. The analytical observer risks the false summit — seeing Classical superiority as immutable rather than institutional. The gap between perspectives reveals that the question 'which forms are correct?' has no answer independent of institutional position.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) is determined by their relationship to the extraction flow. Institutional adopters benefit from the hybrid standard (low d, negative f(d)); they can use Latin functionally while maintaining prestige authority. Dialect speakers bear the full cost of delegitimization without exit (high d, maximum f(d)); they experience the constraint as pure extraction. Reform-minded philologists occupy an intermediate position (d ≈ 0.55): they benefit from Classical expertise and career prestige but are extracted from via suppression of their empirical research. The beneficiary/victim distinction maps precisely to this directionality: beneficiaries include those who gain authority from Classical legitimacy; victims include those whose actual linguistic practice is delegitimized. The institutional override of the derivation chain is minimal — the derived d values from beneficiary/victim + exit options already capture the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading resolves the mandatrophy by demonstrating that Tangled Rope is the correct classification when a constraint genuinely coordinates institutional Latin use while asymmetrically extracting from those outside the legitimized subset. The test: does the constraint serve a real coordination function? Yes — institutional users genuinely need a shared Latin standard that permits specialized vocabulary. Does it asymmetrically extract? Yes — some agents are beneficiaries (Classical prestige) while others are victims (delegitimization). Is active enforcement required? Yes — institutional gate-keeping, educational authority, and prestige hierarchies actively maintain the distinction. This satisfies all three Tangled Rope gates. The false summit at the analytical level is detected by the FSM: the analytical observer risks naturalizing institutional prestige as linguistic inevitability, but the presence of identified beneficiaries (institutional adopters who gain prestige authority) triggers FSM evaluation and suggests reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_distinction,
    'Which reading of the classical_latin_standard kernel does this constraint instantiate, and how do sibling readings produce different structural classifications?',
    'Comparative analysis across the three readings: (1) hybrid_reading (this file) — accommodates post-Classical technical vocabulary with selective legitimization; (2) continuity_reading — treats all post-Classical Latin as legitimate descendants, no suppression of medieval/ecclesiastical forms; (3) reconstruction_reading — strict Ciceronian purism, rejects all post-Classical developments as corruptions. Each reading has different beneficiary/victim sets and different extractiveness values.',
    'If continuity_reading were instantiated instead: extractiveness would drop (no suppression gate), no victims declared, constraint would classify as Rope across all perspectives. If reconstruction_reading were instantiated: extractiveness would rise (maximal suppression), victim set expands (all medieval speakers), constraint classifies as Snare from nearly all positions. The hybrid reading is the middle structural position.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Kernel reading selection and structural consequences').

omega_variable(
    ecclesiastical_vocabulary_legitimacy_threshold,
    'What criteria distinguish ''legitimate technical development'' (ecclesiastical neologisms, medieval legal coinages) from ''barbarism'' (non-functional deviation, mixing with non-Latin elements)?',
    'Empirical: trace frequency, functional necessity, cross-cultural adoption, and historical continuity of contested forms (e.g., ecclesiastical ''sacramentum'' vs spurious medieval coinages). Conceptual: whose institutional authority determines the distinction? (Classical purists vs ecclesiastical practitioners vs philologists vs historical linguists).',
    'If threshold is narrow (purist interpretation): many ecclesiastical and medieval forms reclassified as barbarisms; victim set expands; extractiveness rises toward Snare classification. If threshold is wide (historical interpretation): fewer victims; extractiveness drops toward Rope. The hybrid reading stakes a specific institutional position on this threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_vocabulary_legitimacy_threshold, conceptual, 'Criteria for distinguishing legitimate post-Classical development from corruption').

omega_variable(
    institutional_versus_scholarly_authority,
    'Is the hybrid standard enforced by institutional authority (universities, academies, church doctrinal bodies) or legitimized through scholarly consensus?',
    'Historical analysis: institutional codification records (university curricula, ecclesiastical councils, academy decrees) vs peer-reviewed linguistic scholarship. Does institutional enforcement precede or follow scholarly justification? Are dissenters punished institutionally or merely excluded from prestige circles?',
    'If primarily institutional enforcement: requires_active_enforcement should be true (confirmed); suppression mechanisms are structural (institutional gatekeeping). If primarily scholarly consensus: suppression is softer (academic prestige, citation leverage); constraint may reclassify toward Rope. The current setting (requires_active_enforcement: true) assumes institutional enforcement predominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_versus_scholarly_authority, empirical, 'Institutional enforcement vs scholarly legitimization').

omega_variable(
    post_classical_domain_scope,
    'Does the hybrid reading legitimize ALL post-Classical developments in specialized domains (ecclesiastical, legal, scientific), or only a curated subset?',
    'Textual analysis of authoritative grammars and stylistic guides claiming to adopt the hybrid reading. Enumerate which post-Classical forms are explicitly endorsed vs which are still marked as deviations or archaisms. Measure the proportion of medieval Latin vocabulary permitted in ecclesiastical vs secular domains.',
    'If broad legitimization: extractiveness drops (fewer victims, less suppression). If narrow curation (only forms palatable to Classical sensibilities): victims expand to include entire communities using broader post-Classical forms; extractiveness rises. Current (0.38) assumes selective/moderate legitimization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_classical_domain_scope, empirical, 'Scope of post-Classical forms legitimized by the hybrid reading').

omega_variable(
    false_summit_risk,
    'Is the hybrid Classical-post-Classical distinction a genuine natural law (language change is inherent and immutable), or a contingent institutional arrangement benefiting Classical educators and prestige-maintaining academies?',
    'Compare the hybrid standard to descriptive historical linguistics: are the Classical/post-Classical distinction categories used in modern linguistic analysis, or are they artifacts of prestige hierarchies? Do comparable languages (Greek, Sanskrit) use similar hierarchies, or do they integrate historical strata into unified descriptive frameworks? Is the mountain classification (mountain context at analytical level) warranted by genuine immutability or by institutional naturalization?',
    'If contingent institutional arrangement (false summit): the mountain classification at the analytical context is a misclassification; true type is Tangled Rope or Piton. If genuine natural law: the mountain classification holds; extractiveness ≤ 0.25; no beneficiaries declared. Current design assumes false summit risk — beneficiaries ARE declared, triggering FSM evaluation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_risk, conceptual, 'Whether Classical-post-Classical distinction is natural law or constructed hierarchy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybr_tr_t0, hybrid_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hybr_tr_t250, hybrid_reading, theater_ratio, 250, 0.5).
narrative_ontology:measurement(hybr_tr_t500, hybrid_reading, theater_ratio, 500, 0.58).

% Extraction over time
narrative_ontology:measurement(hybr_be_t0, hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(hybr_be_t250, hybrid_reading, base_extractiveness, 250, 0.31).
narrative_ontology:measurement(hybr_be_t500, hybrid_reading, base_extractiveness, 500, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_reading, information_standard).
narrative_ontology:boltzmann_floor_override(hybrid_reading, 0.12).
narrative_ontology:affects_constraint(hybrid_reading, continuity_reading).
narrative_ontology:affects_constraint(hybrid_reading, reconstruction_reading).
narrative_ontology:affects_constraint(hybrid_reading, ecclesiastical_latin_authority).
narrative_ontology:affects_constraint(hybrid_reading, medieval_legal_terminology).

% DUAL FORMULATION NOTE:
% The classical_latin_standard kernel admits three structurally distinct readings (hybrid, continuity, reconstruction) with different ε values, beneficiary/victim sets, and classification types. This story (hybrid_reading) represents the middle institutional position. It affects (and is affected by) the sibling readings and downstream constraints in specialized domains (ecclesiastical authority structures, medieval legal terminology systems). All three readings share the same kernel but instantiate different constraints due to different choices about which post-Classical forms to legitimize.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
