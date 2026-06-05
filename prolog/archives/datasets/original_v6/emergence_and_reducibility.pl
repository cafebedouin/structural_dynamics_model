% ============================================================================
% CONSTRAINT STORY: emergence_and_reducibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emergence_and_reducibility, []).

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
 *   constraint_id: emergence_and_reducibility
 *   human_readable: Emergence and Reducibility in Scientific Explanation
 *   domain: philosophy_of_science/ontology
 *
 * SUMMARY:
 *   The emergence-reducibility constraint structures how scientific
 *   communities allocate credibility and resources across explanatory
 *   frameworks. At its surface, this appears to be a technical philosophical
 *   debate about whether higher-level properties can be explained by
 *   lower-level components. In reality, the constraint is an institutional
 *   asymmetry: reductionist explanations (breaking systems into components)
 *   receive systematically higher funding, publication venue prestige, and
 *   degree-granting authority than emergent-level explanations (properties
 *   that appear at collective scales). This creates a mixed
 *   coordination-extraction dynamic. The reductionist priority does
 *   coordinate scientific activity — it provides unified methodologies and
 *   mutual intelligibility across disciplines through a common reductive
 *   language. But this coordination function is paired with systematic
 *   extraction: emergent-level researchers must translate their insights into
 *   reductionist framing to gain institutional legitimacy, and frameworks
 *   that treat higher-level properties as causally efficacious (rather than
 *   merely complex computations of lower-level properties) are marginalized.
 *   The constraint has become more extractive over time as disciplinary
 *   specialization has deepened, making cross-scale explanation harder and
 *   forcing emergent-level researchers to maintain dual legitimacy in both
 *   frameworks or abandon institutional credibility.
 *
 * KEY AGENTS:
 *   - Emergentist Researchers: Primary victims (powerless/trapped) — cannot exit without abandoning career legitimacy; must subordinate emergence-level insights to reductionist framing for publication and funding
 *   - Reductionist Establishment: Primary beneficiary (institutional/arbitrage) — receives funding concentration, publication prestige, and regulatory authority over explanation standards; can migrate to new frameworks while maintaining institutional power
 *   - Cross-Scale Research Community: Secondary victim (moderate/constrained) — benefits from dual access to methods but constrained to maintain reductionist priority for credibility; genuine coordination function mixed with asymmetric extraction
 *   - Complexity Science Coalition: Organized agents (organized/constrained) — systems biology, climate modeling, networks research building alternative credibility structures with emergent ontologies as primary; has agency and exit path through institutional innovation
 *   - Philosophical Debate Community: Institutional actor (institutional/arbitrage) — maintains performative debate about emergence vs reducibility largely decoupled from empirical discovery; persists through publication incentives and inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating the institutional asymmetry as a logical feature of knowledge representation itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emergence_and_reducibility, 0.38).
domain_priors:suppression_score(emergence_and_reducibility, 0.52).
domain_priors:theater_ratio(emergence_and_reducibility, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emergence_and_reducibility, extractiveness, 0.38).
narrative_ontology:constraint_metric(emergence_and_reducibility, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(emergence_and_reducibility, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emergence_and_reducibility, tangled_rope).
narrative_ontology:human_readable(emergence_and_reducibility, "Emergence and Reducibility in Scientific Explanation").
narrative_ontology:topic_domain(emergence_and_reducibility, "philosophy_of_science/ontology").

domain_priors:requires_active_enforcement(emergence_and_reducibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emergence_and_reducibility, reductionist_research_programs).
narrative_ontology:constraint_beneficiary(emergence_and_reducibility, disciplinary_boundary_maintainers).
narrative_ontology:constraint_victim(emergence_and_reducibility, emergentist_frameworks).
narrative_ontology:constraint_victim(emergence_and_reducibility, cross_scale_explanatory_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGENTIST RESEARCHER (SNARE) — Trapped by funding and publication bias toward reductionist framing. Cannot exit without abandoning career legitimacy. Must translate emergent-level insights into reductionist language to gain institutional credibility. Bears full cost of the constraint without ability to contest the framing.
constraint_indexing:constraint_classification(emergence_and_reducibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CROSS-SCALE RESEARCH COMMUNITY (TANGLED ROPE) — Benefits from access to both reductionist methods and emergent-level concepts for integration and novel discovery. Constrained by need to maintain dual legitimacy in both frameworks. Genuine coordination function (integrating scales) combined with asymmetric extraction (pressure to subordinate emergence to reduction).
constraint_indexing:constraint_classification(emergence_and_reducibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REDUCTIONIST ESTABLISHMENT (ROPE) — Institutional beneficiary with arbitrage options (can migrate to new paradigms, access resources across disciplinary boundaries). Experiences the constraint as coordination: maintaining reductionist priority ensures methodological coherence and funding concentration. Net beneficiary through regulatory capture of explanation standards.
constraint_indexing:constraint_classification(emergence_and_reducibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPLEXITY SCIENCE COALITION (SCAFFOLD) — Organized agents (systems biology, climate modeling, complex networks communities) building alternative credibility structures with emergent-level ontologies as primary rather than subordinate. Sees the reductionist priority as temporary institutional barrier with sunset: growing recognition that complex systems have genuine level-specific properties that cannot be derived from component properties. Has agency and exit path through institutional innovation.
constraint_indexing:constraint_classification(emergence_and_reducibility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PHILOSOPHICAL DEBATE (PITON) — The emergence-vs-reducibility debate in philosophy of science is substantially performative. Both positions claim to defend reality's fundamental structure, but institutional philosophy has largely decoupled from empirical discovery processes. The debate persists through inertia and publication incentives rather than genuine theoretical or empirical stakes. High theater_ratio reflects that much argumentation addresses strawman versions of positions rather than actual explanatory practice in sciences.
constraint_indexing:constraint_classification(emergence_and_reducibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LOGICAL STRUCTURE VIEW (MOUNTAIN) — From a civilizational/universal perspective, some tension between emergence and reducibility is inherent to knowledge representation itself: any complex system admits multiple scales of description, and no single scale can capture all explanatory categories without loss of information. This perspective treats the constraint as a logical feature of how we represent physical systems — immutable and universal. However, the structural data contradicts the mountain classification — the engine will detect this as naturalization of what is actually an institutional asymmetry about which scales receive priority and legitimacy.
constraint_indexing:constraint_classification(emergence_and_reducibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emergence_and_reducibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(emergence_and_reducibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emergence_and_reducibility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(emergence_and_reducibility, TR),
    TR >= 0.70.

:- end_tests(emergence_and_reducibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The reductionist priority does extract resources and credibility from emergent-level frameworks, but the extraction is not maximal because reductionism has genuine explanatory power and because emergent-level research can proceed (albeit at lower prestige and funding). The value reflects that subordination is enforced but not total. The trajectory from 0.22 to 0.38 reflects increasing pressure as disciplinary specialization deepens and the complexity of systems requiring emergent-level understanding grows. Suppression (0.52): Moderate-high. Barriers to emergent-level research include publication bias (journals favor reductionist framing), funding allocation (grants committees weight mechanistic explanations more heavily), career risk (promotion criteria emphasize reductionist credentials), and knowledge translation (expressing emergent insights in reductionist language is cognitively costly). These barriers are significant but not insurmountable — emergence research exists and can access resources, but at substantially higher friction. Theater ratio (0.68): High and rising. The philosophical debate about emergence vs reducibility has become increasingly performative: much argumentation addresses strawman positions rather than actual explanatory practice; both sides claim to defend reality's fundamental structure, but the debate is largely decoupled from empirical discovery processes where emergence is increasingly recognized as necessary for explanation across multiple domains (consciousness, life, chemistry, climate systems, AI). The theater has risen as the gap between philosophical debate and scientific practice has widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates divergent classification across structural positions. The reductionist establishment sees pure coordination (Rope) — they are solving a genuine problem of methodological coherence and enabling cross-disciplinary translation through unified reductive language. The complexity science coalition sees a temporary institutional barrier with a sunset (Scaffold) — complexity science, systems biology, and climate modeling are building alternative credibility structures where emergent properties are treated as primary explanatory categories; this represents a real institutional shift with a sunset horizon of 15-25 years as complexity-based methods accumulate evidence and prestige. The cross-scale research community sees the mixed coordination-extraction (Tangled Rope) that is the true structural reality — genuine coordination through reductionist methods paired with systematic extraction of emergent-level legitimacy. The emergentist researchers see pure extraction (Snare) — they must translate their insights into a framework that denies the validity of what they are studying, with no alternative path to legitimacy. The philosophical debate sees its own performative ritual (Piton) — the debate persists as institutional theater about metaphysical foundations while the actual scientific work at higher scales proceeds with pragmatic acceptance that emergence is explanatorily necessary. The civilizational analytical observer risks treating the institutional asymmetry as a logical or mathematical necessity (Mountain), failing to see that the priority is contingent on funding structures, publication practices, and professional socialization rather than on the structure of knowledge itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the extraction flow. Reductionist beneficiaries with institutional power and arbitrage options (can access resources and maintain legitimacy across paradigms) experience low or negative effective extraction — the constraint flows toward them. Emergentist victims with no alternative legitimacy pathways (trapped) experience maximum extraction — they must pay the cost of translation or exit their research program. Cross-scale researchers (moderate power with constrained but real options) experience moderate extraction — they benefit from integration but pay a cognitive and career cost to maintain dual legitimacy. Organized complexity scientists (rising power, increasingly constrained by institutional innovation but with exit paths) experience lower effective extraction because they have agency and alternative pathways. The philosophical debate community experiences arbitrage-like optionality (can publish in multiple venues, can shift positions) so experiences low extraction from the constraint. The analytical observer's directionality is derived as analytical (d ≈ 0.72) — the external view that risks naturalizing the institutional asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by revealing the perspectival nature of emergence/reducibility classification. The mandatrophy is not 'which framework is true?' but 'which perspective are we measuring from and what institutional arrangements support that perspective?' The reductionist establishment's rope classification is their genuine institutional experience — reductionism does enable coordination and integration. The emergentist's snare classification is equally genuine — they face systematic barriers and asymmetric extraction. The complexity coalition's scaffold classification reflects real institutional innovation with sunset logic. The analytical observer's mountain is a false summit (naturalization of institutional preference). No single type is metaphysically 'correct' — the presheaf over the observation site (multiple institutional perspectives with different structurally-derived d values) is the answer. The increasing theater_ratio over time suggests the debate itself is becoming more divorced from empirical reality: as emergence becomes more empirically necessary (in systems biology, climate modeling, consciousness studies), the philosophical insistence on reducibility becomes more performative. This is diagnostic: rising theater_ratio in a constraint claimed to address fundamental reality is a strong signal that the constraint is institutional rather than epistemological.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reduction_in_principle_vs_practice,
    'Is the reductionist priority an epistemological claim about what is knowable in principle or an institutional preference for which methods and scales receive resources and credibility?',
    'Historical analysis of funding allocation and publication venue prestige by disciplinary framework; comparison of breakthrough discoveries credited to emergent-level insights vs reductionist decomposition',
    'If epistemological principle: the constraint is a mountain — reduction in principle admits all valid explanation types. If institutional preference: the constraint is a snare or tangled_rope — the priority is extractive and contingent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reduction_in_principle_vs_practice, conceptual, 'Whether reductionism is epistemological principle or institutional preference').

omega_variable(
    genuine_emergence_vs_complexity,
    'Can genuinely novel properties emerge at higher scales that are not in principle derivable from lower-scale descriptions (ontological emergence), or does ''emergence'' refer only to practical epistemological limitations in computing from lower scales (weak emergence)?',
    'Philosophical analysis of specific systems (consciousness, life, chemical properties) to identify whether level-specific causal powers exist; empirical tests for causal efficacy of higher-level properties independent of lower-level configuration',
    'If ontological emergence is real: emergentist frameworks have equal epistemological standing to reductionist ones — the constraint is institutional (snare/tangled_rope). If only weak emergence: the reductionist priority is justified — the constraint is rope or scaffold with sunset already occurring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_emergence_vs_complexity, conceptual, 'Whether emergence is ontological or epistemic').

omega_variable(
    disciplinary_fragmentation_vs_integration,
    'Does the emergence-reducibility constraint create disciplinary fragmentation (reducing explanatory integration), or does it maintain necessary methodological boundaries (supporting specialized depth)?',
    'Network analysis of citation patterns and collaborative structure across fields; measurement of inter-disciplinary translation costs and explanatory failures at disciplinary boundaries',
    'If fragmentation dominates: the constraint is extractive (snare/tangled_rope) — the institutional asymmetry is harming collective knowledge production. If boundaries are functional: the constraint is coordination (rope/scaffold) — the reductionist priority serves legitimate specialization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disciplinary_fragmentation_vs_integration, empirical, 'Whether constraint creates harmful fragmentation or functional boundaries').

omega_variable(
    artificial_vs_natural_emergence_priority,
    'In fields like artificial systems (AI, synthetic biology, engineering), does the emergence-reducibility constraint operate the same way as in natural science, or do different priorities apply?',
    'Comparative institutional analysis of artificial vs natural science communities; examination of whether artificial systems research grants more legitimacy to emergent-level explanations',
    'If artificial systems grant higher legitimacy to emergence: the constraint is revealed as discipline-specific institutional preference (snare in natural science, rope in artificial systems). If constraints are identical: the institutional asymmetry is more fundamental and harder to disrupt.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artificial_vs_natural_emergence_priority, empirical, 'Whether emergence-reducibility constraint differs in artificial vs natural sciences').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emergence_and_reducibility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emerg_tr_t0, emergence_and_reducibility, theater_ratio, 0, 0.45).
narrative_ontology:measurement(emerg_tr_t5, emergence_and_reducibility, theater_ratio, 5, 0.58).
narrative_ontology:measurement(emerg_tr_t10, emergence_and_reducibility, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(emerg_be_t0, emergence_and_reducibility, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(emerg_be_t5, emergence_and_reducibility, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(emerg_be_t10, emergence_and_reducibility, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emergence_and_reducibility, information_standard).
narrative_ontology:affects_constraint(emergence_and_reducibility, reductionism_in_neuroscience).
narrative_ontology:affects_constraint(emergence_and_reducibility, systems_biology_institutional_credibility).
narrative_ontology:affects_constraint(emergence_and_reducibility, consciousness_explanation_frameworks).

% DUAL FORMULATION NOTE:
% The emergence-reducibility constraint is decomposed in the network from three upstream empirical claims (reductionism's success in specific domains) and flows into specific disciplinary manifestations. The general constraint story captures the institutional asymmetry; downstream stories capture how this plays out in particular scientific domains with different empirical evidence for emergence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(emergence_and_reducibility, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
