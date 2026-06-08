% ============================================================================
% CONSTRAINT STORY: hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hanafi_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: hanafi_reading
 *   human_readable: Hanafi School Analogical Reasoning Framework
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   The Hanafi reading of usul al-fiqh (Islamic legal methodology) emerged in
 *   8th-century Iraq and became the dominant legal school across Ottoman,
 *   Mughal, and Central Asian Muslim polities. Its methodological signature
 *   is expansive analogical reasoning (qiyas), supplementation by reasoned
 *   opinion (ra'y) where analogy reaches limits, and juristic preference
 *   (istihsan) permitting departure from strict analogy for public interest.
 *   This reading concentrates interpretive authority in jurists trained in
 *   rationalist legal reasoning, creating asymmetric extraction from
 *   textualist constraints and layperson legal certainty. The constraint
 *   coordinates legal development across vast geographic and temporal scales
 *   (genuine coordination function) while suppressing alternative
 *   interpretive pathways and minoritizing sibling legal schools. Theater
 *   ratio (0.35) reflects moderate performativity: classical usul methodology
 *   remains functional in family law and scholarly discourse but has become
 *   largely theatrical in domains where statutory codes have displaced
 *   jurist-mediated reasoning. Extractiveness (0.38) captures the career and
 *   institutional benefits flowing to Hanafi-trained jurists through
 *   interpretive latitude, moderated by the genuine coordination work the
 *   framework performs. Suppression (0.42) reflects barriers to non-jurist
 *   legal participation and the institutional dominance that marginalized
 *   competing schools, tempered by the fact that alternative readings persist
 *   and some exit is possible through legal pluralism or reform movements.
 *
 * KEY AGENTS:
 *   - Hanafi-Trained Jurists: Primary beneficiaries (institutional/arbitrage) — capture interpretive authority and institutional positions through methodological framework
 *   - Rationalist Legal Scholars: Secondary beneficiaries (institutional/arbitrage) — framework vindicates reason-based legal development
 *   - State Administrative Apparatus: Tertiary beneficiaries (institutional/arbitrage) — analogical flexibility enables legal adaptation to governance needs
 *   - Textualist Interpretive Constraint: Primary victim (powerless/trapped) — the reading's analogical expansiveness suppresses textual restrictiveness
 *   - Layperson Legal Certainty: Secondary victim (powerless/trapped) — bears cost of interpretive latitude through dependency on expert mediation
 *   - Minoritized Legal Schools: Tertiary victims (moderate/constrained) — marginalized by Hanafi institutional dominance but retain scholarly presence
 *   - Contemporary Reform Movement: Organized agents (organized/mobile) — building statutory alternatives with sunset logic
 *   - Post-Colonial State Legal System: Institutional actor (institutional/constrained) — maintains framework theatrically in residual domains
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hanafi_reading, 0.38).
domain_priors:suppression_score(hanafi_reading, 0.42).
domain_priors:theater_ratio(hanafi_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hanafi_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(hanafi_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(hanafi_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hanafi_reading, accessibility_collapse, 0.0).
narrative_ontology:constraint_metric(hanafi_reading, resistance, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hanafi_reading, tangled_rope).
narrative_ontology:human_readable(hanafi_reading, "Hanafi School Analogical Reasoning Framework").
narrative_ontology:topic_domain(hanafi_reading, "islamic_jurisprudence/legal_theory/comparative_law").

domain_priors:requires_active_enforcement(hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hanafi_reading, '5068a54b-a2d2-4f01-9f72-25c4fcc59b1d').
narrative_ontology:cs_kernel_codification('5068a54b-a2d2-4f01-9f72-25c4fcc59b1d', formalized).
narrative_ontology:cs_authority_grounding('5068a54b-a2d2-4f01-9f72-25c4fcc59b1d', lineage).
narrative_ontology:cs_interpretation_layer_present('5068a54b-a2d2-4f01-9f72-25c4fcc59b1d').
narrative_ontology:cs_reading_relation('5068a54b-a2d2-4f01-9f72-25c4fcc59b1d', hanafi_reading__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('5068a54b-a2d2-4f01-9f72-25c4fcc59b1d', hanafi_reading__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('5068a54b-a2d2-4f01-9f72-25c4fcc59b1d', hanafi_reading__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('5068a54b-a2d2-4f01-9f72-25c4fcc59b1d', foundational, reason_supplements_revelation_expansively).
narrative_ontology:cs_axiom_status(reason_supplements_revelation_expansively, holdable).
narrative_ontology:cs_axiom_grounding('5068a54b-a2d2-4f01-9f72-25c4fcc59b1d', reason_supplements_revelation_expansively, conventional).
narrative_ontology:cs_axiom('5068a54b-a2d2-4f01-9f72-25c4fcc59b1d', foundational, public_interest_overrides_strict_analogy).
narrative_ontology:cs_axiom_status(public_interest_overrides_strict_analogy, holdable).
narrative_ontology:cs_axiom_grounding('5068a54b-a2d2-4f01-9f72-25c4fcc59b1d', public_interest_overrides_strict_analogy, instrumental).
narrative_ontology:cs_reference_frame('5068a54b-a2d2-4f01-9f72-25c4fcc59b1d', prophetic_era_interpretive_latitude).
narrative_ontology:cs_drift_state('5068a54b-a2d2-4f01-9f72-25c4fcc59b1d', contemporary_codified_state_law, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5068a54b-a2d2-4f01-9f72-25c4fcc59b1d', '').
narrative_ontology:cs_kernel_id(hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hanafi_reading, hanafi_trained_jurists).
narrative_ontology:constraint_beneficiary(hanafi_reading, rationalist_legal_scholars).
narrative_ontology:constraint_beneficiary(hanafi_reading, state_administrative_apparatus).
narrative_ontology:constraint_victim(hanafi_reading, textualist_interpretive_constraint).
narrative_ontology:constraint_victim(hanafi_reading, layperson_legal_certainty).
narrative_ontology:constraint_victim(hanafi_reading, minoritized_legal_schools).
narrative_ontology:constraint_vindicates(hanafi_reading, reason_supplements_revelation).
narrative_ontology:constraint_vindicates(hanafi_reading, public_interest_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LAYPERSON (SNARE) — Trapped by the interpretive latitude qiyas grants jurists. Cannot predict legal outcomes without jurist mediation; bears the cost of expansive analogical reasoning through uncertainty and dependency on expert interpretation. The constraint extracts compliance while suppressing alternative paths to legal knowledge.
constraint_indexing:constraint_classification(hanafi_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MINORITIZED LEGAL SCHOOL (TANGLED ROPE) — Constrained by institutional dominance of Hanafi method in Ottoman and later state apparatus. Benefits from shared usul framework (coordination function) but bears costs of marginalization when Hanafi analogical expansiveness becomes state doctrine. Mixed extraction: the coordination enables cross-school dialogue; the institutional capture extracts legitimacy.
constraint_indexing:constraint_classification(hanafi_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HANAFI JURIST CLASS (ROPE) — Primary beneficiary of the framework. Arbitrage-level exit: can adopt textualist posture when politically expedient, expansive qiyas when state interest requires flexibility. Experiences the constraint as coordination: the methodological framework enables legal adaptation to new contexts while maintaining scholarly authority.
constraint_indexing:constraint_classification(hanafi_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: REFORM MOVEMENT (SCAFFOLD) — Organized actors (codification movements, modernist scholars, comparative law institutes) see the classical usul framework as transitional. The analogical method served a coordination function in pre-modern contexts; contemporary legal systems are building statutory codes and constitutional frameworks that sunset the jurist-mediated analogical reasoning model. Mobile exit: can adopt civil law frameworks, Islamic legal principles, or hybrid models.
constraint_indexing:constraint_classification(hanafi_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-COLONIAL APPARATUS (PITON) — The classical usul methodology persists in family law and personal status codes but has atrophied elsewhere. State legal systems maintain the analogical reasoning framework theatrically in domains where colonial legal transplants have already displaced it. The methodology is performed in judicial rhetoric while statutory codes do the actual work.
constraint_indexing:constraint_classification(hanafi_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The Hanafi framework coordinates legal development across geographic and temporal scales (genuine function) while simultaneously concentrating interpretive authority in a credentialed class and suppressing textualist constraints on jurist discretion (asymmetric extraction). The constraint is not naturalizable: the expansiveness of qiyas and the public-interest override of istihsan are methodological choices with identifiable beneficiaries, not inherent features of Islamic law.
constraint_indexing:constraint_classification(hanafi_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hanafi_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hanafi_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hanafi_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hanafi_reading, TR),
    TR >= 0.70.

:- end_tests(hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Hanafi framework concentrates interpretive authority in credentialed jurists and generates career benefits through analogical latitude, but the extraction is not as severe as pure rent-seeking because the framework performs genuine coordination work across scale. The value reflects real asymmetric benefit flows tempered by functional necessity. Suppression (0.42): Moderate. Significant barriers exist for non-experts to participate in legal interpretation, and the framework's institutional dominance marginalized competing schools (particularly textualist Zahiri, which was effectively suppressed by Abbasid-era Hanafi hegemony). However, suppression is not total: alternative schools persist, legal pluralism exists in some contexts, and reform movements have exit paths. Theater ratio (0.35): Moderate. The classical usul methodology remains functional in family law, waqf administration, and scholarly discourse, but has become performative in criminal law, constitutional matters, and commercial regulation where statutory codes do the actual work. The theater has increased over the interval as state codification displaced jurist-mediated reasoning, but the increase is modest because the framework retains real function in residual domains.
 *
 * PERSPECTIVAL GAP:
 *   The Hanafi jurist sees pure coordination (Rope): the framework enables legal adaptation and scholarly authority, solving the genuine problem of applying fixed texts to changing contexts. The layperson sees pure extraction (Snare): interpretive latitude makes legal outcomes unpredictable and creates dependency on expert mediation. The minoritized school sees mixed coordination and extraction (Tangled Rope): the shared usul framework enables cross-school dialogue (coordination) but Hanafi institutional dominance marginalizes alternative methods (extraction). The reform movement sees a temporary structure with a sunset (Scaffold): statutory codification is building alternative pathways. The post-colonial state sees degraded ritual (Piton): usul methodology persists theatrically where it has lost function. The analytical observer sees tangled rope: genuine coordination function coexisting with asymmetric extraction, neither naturalizable nor purely extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries with arbitrage exit (Hanafi jurists, state apparatus) experience low effective extraction — the framework flows resources toward them. Victims with trapped exit (textualist constraint, layperson certainty) experience high effective extraction — they bear costs with no escape. The minoritized legal schools occupy a middle position: constrained exit and mixed beneficiary/victim status yields moderate experienced extraction. The tangled rope classification for the analytical observer reflects that both coordination and extraction are structurally present: the framework genuinely coordinates legal development (low coordination-component extraction) while concentrating authority asymmetrically (higher extraction from that concentration). The piton classification for post-colonial systems derives from the theater gate (atrophied function maintained as performance) rather than from high experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that the Hanafi reading is neither pure natural law (the analytical classification is tangled rope, not mountain — the reading is not inherent to Islamic law but a methodological choice with identifiable beneficiaries) nor pure extraction (the coordination function is genuine — the framework does solve the real problem of legal development across scale). The perspectival spread demonstrates that all six types can legitimately appear: the beneficiary's rope, the victim's snare, the mixed-position tangled rope, the reformist's scaffold, the post-colonial piton, and the (absent here) false-summit mountain if one naturalized the reading as 'Islamic law itself.' The mandatrophy is resolved by indexical measurement, not by forcing a single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the Hanafi reading''s expansive analogical method a faithful interpretation of the usul al-fiqh kernel, or a jurist-class construction that benefits credentialed interpreters?',
    'Cross-reading comparison of beneficiary structures; historical analysis of which social groups gained authority as each school''s methodology crystallized; examination of whether the sibling readings with narrower analogical scope (Hanbali, Shafi''i) show different distributions of interpretive power.',
    'If constructed: the Hanafi reading instantiates a false summit — naturalizing jurist authority as Islamic legal method. If faithful: the reading''s coordination function is genuine, and the extraction flows from necessary expertise rather than rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether Hanafi expansiveness is kernel-faithful or jurist-benefiting construction').

omega_variable(
    istihsan_discretion_boundary,
    'What structural limits, if any, constrain istihsan (juristic preference) from becoming arbitrary jurist discretion?',
    'Doctrinal analysis of istihsan precedents; comparison of outcomes when istihsan invoked versus strict analogy followed; examination of whether istihsan systematically favors state or elite interests.',
    'If no structural limits: istihsan is an extraction mechanism disguised as public-interest doctrine. If limits exist and are enforced: istihsan is a legitimate coordination tool for adaptive governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(istihsan_discretion_boundary, empirical, 'Boundary between istihsan flexibility and arbitrary jurist power').

omega_variable(
    ray_epistemic_status,
    'Does ra''y (reasoned opinion) supplement revelation (coordination) or displace it (extraction)?',
    'Historical trajectory analysis: does the scope of ra''y expand over time in domains where textual sources exist? Comparison with Zahiri (literalist) school outcomes on identical legal questions.',
    'If supplements: ra''y fills genuine gaps in textual coverage (coordination function). If displaces: ra''y is a mechanism for jurist authority to override textual constraint (extraction from textualist claim).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ray_epistemic_status, empirical, 'Whether ra''y supplements or displaces textual authority').

omega_variable(
    sibling_reading_foreclosure,
    'Do the structural differences between Hanafi and sibling readings represent different interpretive choices within a shared commitment framework, or incompatible epistemologies?',
    'Analysis of mixed-method fatwas where jurists combine elements from multiple schools; examination of historical contexts where political authority enforced one reading over siblings; assessment of whether a single legal system could coherently adopt Hanafi analogical breadth and Hanbali textualist restrictiveness simultaneously.',
    'If different choices: readings coexist_with each other. If incompatible: some reading pairs foreclose each other. Determines the reading_relations topology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether sibling readings are compatible interpretive choices or incompatible epistemologies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hanafi_reading, 0, 900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanafi_theater_formative, hanafi_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hanafi_theater_classical, hanafi_reading, theater_ratio, 300, 0.25).
narrative_ontology:measurement(hanafi_theater_ottoman, hanafi_reading, theater_ratio, 600, 0.3).
narrative_ontology:measurement(hanafi_theater_contemporary, hanafi_reading, theater_ratio, 900, 0.35).

% Extraction over time
narrative_ontology:measurement(hanafi_extraction_formative, hanafi_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(hanafi_extraction_classical, hanafi_reading, base_extractiveness, 300, 0.32).
narrative_ontology:measurement(hanafi_extraction_ottoman, hanafi_reading, base_extractiveness, 600, 0.38).
narrative_ontology:measurement(hanafi_extraction_contemporary, hanafi_reading, base_extractiveness, 900, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(hanafi_suppression_formative, hanafi_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(hanafi_suppression_classical, hanafi_reading, suppression_requirement, 300, 0.35).
narrative_ontology:measurement(hanafi_suppression_ottoman, hanafi_reading, suppression_requirement, 600, 0.45).
narrative_ontology:measurement(hanafi_suppression_contemporary, hanafi_reading, suppression_requirement, 900, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hanafi_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(hanafi_reading, maliki_reading).
narrative_ontology:affects_constraint(hanafi_reading, shafii_reading).
narrative_ontology:affects_constraint(hanafi_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% The Hanafi reading is one of four constraint stories decomposing the 'usul al-fiqh' kernel. Each sibling reading has its own extractiveness value reflecting different beneficiary structures and different scope for jurist discretion. They are not the same constraint viewed from different angles — they are different institutional arrangements with different ε values. The confusion is in the umbrella term 'usul al-fiqh,' which collapses structurally distinct methodologies into a single label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
