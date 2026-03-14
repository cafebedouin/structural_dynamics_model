% ============================================================================
% CONSTRAINT STORY: conscious_experience_substrate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_conscious_experience_substrate, []).

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
 *   constraint_id: conscious_experience_substrate
 *   human_readable: Conscious Experience Substrate Constraint
 *   domain: philosophy_of_mind/neuroscience/cognitive_science
 *
 * SUMMARY:
 *   The constraint on conscious experience substrate defines the structural
 *   problem that phenomenal consciousness cannot be fully reduced to physical
 *   substrate properties, and institutional structures have evolved to
 *   suppress, marginalize, or reframe non-reductionist approaches. This
 *   creates a hybrid extraction mechanism: physicalist research programs
 *   benefit from the framing that consciousness is 'just' neural activity
 *   (justifying funding, defining research agendas, setting publication
 *   standards), while phenomenological understanding and alternatives to
 *   reductionism bear the costs of institutional suppression. The constraint
 *   exhibits genuine coordination function (neuroscientific collaboration,
 *   standardized methods, inter-institutional knowledge sharing) alongside
 *   asymmetric extraction (suppression of non-reductionist voices, career
 *   barriers for hard-problem researchers, strategic reframing of explanatory
 *   gaps as merely empirical). Theater has accumulated over the interval:
 *   early 20th-century debates about consciousness took the hard problem
 *   seriously; by late 20th century, institutional physicalism treats the
 *   hard problem as already dissolved or methodologically out of bounds. The
 *   theater is performative dismissal rather than genuine refutation.
 *
 * KEY AGENTS:
 *   - The Explanatory Gap: Primary victim (powerless/trapped) — structural impossibility of reducing subjective phenomenal properties to physical description; cannot advocate for itself; bears full cost of reductionist failure
 *   - Physicalist Research Programs: Primary beneficiary (institutional/arbitrage) — capture funding, prestige, and control over research agendas through framing consciousness as substrate-dependent problem
 *   - Neuroscientific Institutions: Secondary beneficiary (institutional/constrained) — benefit from constraint (generates research programs) but also constrained by persistent failure of reductionist programs to resolve hard problem
 *   - Phenomenological Researchers: Moderate victim (moderate/constrained) — face career barriers and funding discrimination for non-reductionist approaches; constrained by institutional pressure to work within physicalist frameworks
 *   - Cartesian Dualism Suppression Apparatus: Institutional enforcer (institutional/arbitrage) — maintains suppression through gatekeeping, peer review, and prestige allocation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choice (materialism as metaphysics) as immutable logical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(conscious_experience_substrate, 0.58).
domain_priors:suppression_score(conscious_experience_substrate, 0.72).
domain_priors:theater_ratio(conscious_experience_substrate, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(conscious_experience_substrate, extractiveness, 0.58).
narrative_ontology:constraint_metric(conscious_experience_substrate, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(conscious_experience_substrate, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(conscious_experience_substrate, tangled_rope).
narrative_ontology:human_readable(conscious_experience_substrate, "Conscious Experience Substrate Constraint").
narrative_ontology:topic_domain(conscious_experience_substrate, "philosophy_of_mind/neuroscience/cognitive_science").

domain_priors:requires_active_enforcement(conscious_experience_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(conscious_experience_substrate, physicalist_research_programs).
narrative_ontology:constraint_beneficiary(conscious_experience_substrate, neuroscientific_institutions).
narrative_ontology:constraint_victim(conscious_experience_substrate, consciousness_explanatory_gap).
narrative_ontology:constraint_victim(conscious_experience_substrate, phenomenal_understanding).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE EXPLANATORY GAP (SNARE) — The structural impossibility of explaining subjective phenomenal properties (qualia) in purely physical terms cannot exit or organize. Bears the full cost of the constraint: every reductionist explanation that succeeds materially fails to capture the hard problem. No mechanism for self-advocacy. Highest experienced extraction.
constraint_indexing:constraint_classification(conscious_experience_substrate, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: PHENOMENOLOGICAL RESEARCHER (TANGLED ROPE) — Constrained by funding structures and institutional pressure to publish within physicalist frameworks, but also benefits from access to neuroscientific data and collaborative networks. Experiences both coordination (shared research infrastructure) and extraction (career risk of non-reductionist positions). Moderate agency with significant cost.
constraint_indexing:constraint_classification(conscious_experience_substrate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHYSICALIST RESEARCH PROGRAMS (ROPE) — Institutional beneficiary. Experiences the constraint as a coordination mechanism: the demand for substrate explanations drives research agendas, funding allocation, and method standardization. Pure coordination without significant coercion from this vantage point. Net beneficiary with high arbitrage capacity.
constraint_indexing:constraint_classification(conscious_experience_substrate, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NEUROSCIENTIFIC INSTITUTIONS (TANGLED ROPE) — Institutional beneficiary with constraints. Benefits from the constraint (generates research problems, justifies funding, coordinates experimental programs) but also constrained by the persistent failure of reductionist research to resolve the hard problem. Genuine coordination function alongside asymmetric extraction from phenomenological understanding. Active enforcement through peer review and funding gatekeeping.
constraint_indexing:constraint_classification(conscious_experience_substrate, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CARTESIAN DUALISM SUPPRESSION APPARATUS (PITON) — The institutional machinery that performs strong skepticism toward non-physicalist ontologies is largely theater. The arguments (conceptual coherence, methodological naturalism) are recycled rather than novel; the suppress-by-default reflex persists through professional inertia rather than epistemic force. The apparatus maintains itself through institutional reinforcement rather than demonstrated explanatory power. Theater ratio dominates over actual function.
constraint_indexing:constraint_classification(conscious_experience_substrate, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a logic/information-theoretic perspective, the constraint appears immutable: any finite physical description of a system cannot capture the first-person subjective character of that system's experience because first-person and third-person are irreducibly different observational frames. This perspective sees the hard problem as a logical/mathematical ceiling. However, the structural data contradicts this — the constraint's suppression and theater ratio indicate institutional enforcement, not natural law.
constraint_indexing:constraint_classification(conscious_experience_substrate, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(conscious_experience_substrate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(conscious_experience_substrate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(conscious_experience_substrate, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(conscious_experience_substrate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(conscious_experience_substrate, TR),
    TR >= 0.70.

:- end_tests(conscious_experience_substrate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through asymmetric benefits for physicalist programs and institutional costs for alternatives. However, the extraction is not maximal (snare-level) because some coordination genuinely occurs — neuroscientific collaboration produces real knowledge. The value reflects that legitimate coordination and unjust extraction are intertwined. Suppression (0.72): High. Significant barriers to non-reductionist research include funding scarcity, publication bias, career risk, and institutional framing that treats the hard problem as already solved or methodologically illegitimate. These are not absolute (some non-reductionist work continues) but substantial enough to suppress institutional development of alternatives. Theater ratio (0.68): High. The institutional apparatus strongly performs dismissal of dualism and hard-problem arguments through standard skeptical formulae (logical coherence objections, methodological naturalism) rather than novel engagement. The arguments are recycled; the suppression is reflexive. The theater has grown as institutional confidence has increased despite lack of explanatory progress. Claimed type: Tangled Rope. The constraint has genuine coordination function (shared research infrastructure, collaborative knowledge production) alongside clear asymmetric extraction (suppression of alternatives, career barriers for non-reductionist researchers). Active enforcement through peer review and funding gatekeeping is visible and structural. All three tangled_rope gates are met.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence across power and exit dimensions. The beneficiary (physicalist programs) with arbitrage options sees pure coordination (rope) — they are solving problems and building knowledge. The institutional constraints (neuroscience) with constrained options see mixed coordination and extraction (tangled_rope) — the system works but also fails them through its persistent inadequacy. Phenomenological researchers with constrained career options see significant extraction (tangled_rope). The gap itself (explanatory structure) sees maximal extraction (snare) because it bears all costs without benefit. The suppression apparatus sees its own degraded ritual (piton) — performing dismissal without genuine epistemic force. The analytical observer risks seeing a natural law (mountain) — logical necessity of the hard problem — but this misclassifies a contingent institutional enforcement as unchangeable nature. The perspectival multiplicity reveals that the constraint is not about consciousness itself but about who controls consciousness research and how alternatives are suppressed.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position in the extraction flow. The explanatory gap (victim) faces maximum d because it is structurally targeted and has no exit options. Phenomenological researchers (moderate victim with constrained exit) experience high-but-not-maximal d based on career cost barriers. Physicalist programs (institutional beneficiary with arbitrage) experience low d — they can exit (redirect research) if reductionism fails, and they capture benefits from the constraint. Neuroscientific institutions face mixed d (constrained arbitrage) — they benefit from the constraint's research generation but are constrained by its persistent epistemic failure and the reputational risk of defending an indefensible position. The piton perspective derives from high theater ratio (~0.68) rather than from high experienced extraction — the suppression apparatus is maintained through inertia and institutional reinforcement, not from genuine explanatory power.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exemplifies false natural law (mountain wrongly applied). The analytical perspective at civilizational scope risks classifying the hard problem as a logical/mathematical impossibility — an immutable ceiling on reductionist explanation. But the base properties reveal institutional enforcement: suppression (0.72), theater (0.68), and active enforcement flags all indicate that this is not a natural law but a contingent institutional arrangement. The mandatrophy is resolved by distinguishing the hard problem (potentially logical, potentially empirical — still unresolved via omega variables) from the institutional constraint (extraction through suppression of alternatives). The hard problem may be a permanent feature of conscious explanation; the institutional monopoly on consciousness research is not. The false mountain classification reveals how dominant research programs naturalize their own enforcement as logical necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hard_problem_logical_or_empirical,
    'Is the hard problem of consciousness a logical/conceptual impossibility (mountain) or an empirical knowledge gap (snare/tangled_rope)?',
    'Analysis of whether future neuroscientific discovery could in principle dissolve the explanatory gap. If Chalmers'' logical zombie argument is sound, mountain. If empirical research could bridge the gap, then constraint is institutional/extractive.',
    'Mountain classification: constraint is unchangeable natural law, not institutional extraction. Tangled Rope/Snare: the suppression and enforcement are recognizable institutional arrangements that could be reformed. This distinction fundamentally alters policy implications.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hard_problem_logical_or_empirical, conceptual, 'Whether hard problem is logical necessity or empirical gap').

omega_variable(
    integrated_information_resolution,
    'Do integrated information theory (IIT) or other mathematical frameworks for consciousness provide sufficient explanatory closure to make the hard problem tractable?',
    'Empirical validation of IIT predictions against consciousness reports; demonstration that phi measures correlate with phenomenal content at the required precision; test whether IIT dissolves rather than restates the hard problem.',
    'If IIT succeeds: reductionist research program gains explanatory credibility (rope perspective becomes dominant). If IIT fails or merely restates gap: physicalist programs remain extractive despite institutional dominance (snare/tangled_rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integrated_information_resolution, empirical, 'Whether mathematical consciousness theories provide explanatory closure').

omega_variable(
    panpsychism_credibility_barrier,
    'Is the institutional dismissal of panpsychism (that consciousness is a fundamental feature rather than emergent property) based on genuine epistemic grounds or on suppression of non-reductionist alternatives?',
    'Comparative analysis: (a) the logical arguments against panpsychism vs (b) the institutional barriers to panpsychist research funding, publication, and hiring. If (a) is decisive and (b) is proportional, suppression is epistemic. If (b) far exceeds (a), suppression is institutional/extractive.',
    'If epistemic: constraint reflects genuine knowledge asymmetry (institutional enforcement justified). If institutional suppression: constraint exemplifies how dominant research programs exclude alternatives through gatekeeping rather than argument (snare/tangled_rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(panpsychism_credibility_barrier, empirical, 'Whether panpsychism suppression is epistemic or institutional').

omega_variable(
    emergence_vs_reduction_decidability,
    'Can consciousness be determined to be emergent (novel properties not reducible to parts) or fundamentally reductive (explicable via substrate) via any empirical test?',
    'Identification of experiments that would falsify emergence hypothesis (e.g., substrate-independent upload with perfect phenomenal continuity would falsify emergence). If no such test exists, the constraint is undecidable and the institutional enforcement of physicalism becomes normative rather than epistemic.',
    'If decidable: research programs have objective success conditions and constraint could resolve. If undecidable: institutional enforcement is choosing a normative frame (materialism as preferred metaphysics) and presenting it as empirical truth. This reframes the constraint as extractive suppression of alternative ontologies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergence_vs_reduction_decidability, conceptual, 'Whether emergence vs reduction is empirically decidable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(conscious_experience_substrate, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, conscious_experience_substrate, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cons_tr_t5, conscious_experience_substrate, theater_ratio, 5, 0.55).
narrative_ontology:measurement(cons_tr_t10, conscious_experience_substrate, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, conscious_experience_substrate, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cons_be_t5, conscious_experience_substrate, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cons_be_t10, conscious_experience_substrate, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(conscious_experience_substrate, information_standard).
narrative_ontology:affects_constraint(conscious_experience_substrate, neural_correlates_of_consciousness).
narrative_ontology:affects_constraint(conscious_experience_substrate, qualia_reduction_program).
narrative_ontology:affects_constraint(conscious_experience_substrate, dualism_institutional_suppression).

% DUAL FORMULATION NOTE:
% The conscious experience substrate constraint is upstream of specific neural/cognitive research programs (neural correlates, qualia reduction) but represents a distinct structural constraint on the institutional organization of consciousness research. Decomposition: hard problem (potentially mountain or tangled_rope depending on logical status) vs institutional suppression of alternatives (institutional extraction via piton/snare mechanisms). Network links show how institutional constraint propagates to downstream research programs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(conscious_experience_substrate, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
