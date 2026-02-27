% ============================================================================
% CONSTRAINT STORY: burali_forte_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_burali_forte_paradox, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: burali_forte_paradox
 *   human_readable: Burali-Forti Paradox
 *   domain: mathematical_logic/set_theory
 *
 * SUMMARY:
 *   The Burali-Forti paradox reveals a fundamental limit of formal systems:
 *   any consistent mathematical framework must distinguish between the
 *   collection of all ordinal numbers and legitimate sets. If the collection
 *   of all ordinals were itself a set, it would have an ordinal rank greater
 *   than any ordinal in it — a logical contradiction. This constraint emerged
 *   in 1897 when Cesare Burali-Forti recognized that Cantor's informal
 *   ordinal theory encountered a self-referential barrier. The paradox is not
 *   a bug in a particular axiomatization but a deep structural feature: every
 *   formal system that permits transfinite arithmetic encounters this same
 *   boundary. The constraint exhibits zero degrees of freedom across all
 *   mathematical foundations. All major responses (ZFC's Separation and
 *   Replacement axioms, NBG's proper class distinction, category-theoretic
 *   approaches) accept the constraint as immutable and work within its
 *   bounds.
 *
 * KEY AGENTS:
 *   - Mathematical Logic Community: Institutional observer (institutional/arbitrage) — treats the paradox as a law of reasoning, not contingent on human choices
 *   - Formal System Designers: Institutional architects (institutional/arbitrage) — design axiomatizations that respect the constraint rather than escape it
 *   - Students and Practitioners: End users (powerless/trapped) — encounter the constraint as an immovable barrier to naive intuition
 *   - Philosophers of Mathematics: Analytical observers (analytical/analytical) — debate whether the constraint reflects world-structure or formal-system structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(burali_forte_paradox, 0.12).
domain_priors:suppression_score(burali_forte_paradox, 0.03).
domain_priors:theater_ratio(burali_forte_paradox, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(burali_forte_paradox, extractiveness, 0.12).
narrative_ontology:constraint_metric(burali_forte_paradox, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(burali_forte_paradox, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(burali_forte_paradox, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(burali_forte_paradox, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(burali_forte_paradox, mountain).
narrative_ontology:human_readable(burali_forte_paradox, "Burali-Forti Paradox").
narrative_ontology:topic_domain(burali_forte_paradox, "mathematical_logic/set_theory").

domain_priors:emerges_naturally(burali_forte_paradox).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL LOGICIAN (MOUNTAIN) — The paradox is an irreducible logical limit: any formal system that permits unrestricted set formation must distinguish between the 'collection of all ordinals' (which cannot be a set without contradiction) and legitimate sets. This constraint is a natural law of mathematical ontology, not a contingent institutional arrangement. Zero degrees of freedom.
constraint_indexing:constraint_classification(burali_forte_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: SET THEORY COMMUNITY (MOUNTAIN) — Institutional mathematics accepts the constraint as immutable. All major formal systems (ZF, ZFC, NBG, MK) respond by restricting set formation (Separation, Replacement axioms) or distinguishing proper classes from sets. The constraint is inescapable — any alternative foundation encounters the same barrier under a different name. No arbitrage possible.
constraint_indexing:constraint_classification(burali_forte_paradox, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: NOVICE MATHEMATICIAN (MOUNTAIN) — First encountering the paradox, the student discovers an immutable fact: intuitive naïve set theory (the assumption that any property defines a set) is incoherent. No amount of cleverness or institutional work can restore the naïve approach. The paradox is a hard barrier to understanding.
constraint_indexing:constraint_classification(burali_forte_paradox, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(burali_forte_paradox_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(burali_forte_paradox, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(burali_forte_paradox, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(burali_forte_paradox, ExtMetricName, E),
    domain_priors:suppression_score(burali_forte_paradox, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(burali_forte_paradox),
    narrative_ontology:constraint_metric(burali_forte_paradox, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(burali_forte_paradox, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(burali_forte_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): The paradox extracts almost no cost from any agent — it is purely revelatory, not extractive. The constraint identifies what cannot be done (form a set of all ordinals), but this identification requires no coercion or institutional enforcement. The modest extractiveness reflects the cost of re-education when students discover naive set theory is incoherent. Suppression (0.03): Minimal. The paradox cannot be suppressed — once discovered, it is logically transparent. Every agent in mathematics acknowledges it. No alternatives exist that bypass the constraint. Theater ratio (0.08): Extremely low. The paradox presents as pure logic with no performative content. The formal proof is straightforward and noncontroversial. No institutional ritual or theatrical activity is required to maintain the constraint. The slight elevation above zero reflects minor variations in pedagogical framing across textbooks.
 *
 * PERSPECTIVAL GAP:
 *   UNIFORM MOUNTAIN: All three perspectives classify the Burali-Forti paradox identically as Mountain. There is no perspectival gap. This is the canonical case of a uniform-type constraint where the classification is invariant across all observer positions. The paradox is neither more nor less extractive depending on the observer's power level or exit options. This uniformity demonstrates that the constraint is a genuine natural law of logic, not a social arrangement that different actors experience differently. The consensus across perspectives (logician, institutional community, novice) is the hallmark of Mountain classification.
 *
 * DIRECTIONALITY LOGIC:
 *   MOUNTAIN CONSTRAINT — BENEFICIARY/VICTIM ANALYSIS NOT APPLICABLE: Mountains are natural laws. No agent benefits or bears cost from the Burali-Forti paradox in the structural sense required for directionality computation. The paradox is not extracted from anyone; it is discovered by everyone. There is no beneficiary asymmetry — no group that profits while others pay. The mathematical community collectively benefits from knowing the constraint (it prevents false theorems), but this is not extraction in the DR sense. There is no victim group. Directionality derivation is not performed for mountains, and no beneficiary/victim declarations are provided.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_commitment,
    'Is the paradox a constraint on the world''s actual structure, or a constraint on human formal systems for describing the world?',
    'Metaphysical analysis of whether ordinals are discovered or constructed; comparison of ontological costs across alternative formal systems',
    'If world-structure: the constraint is noumenal (true independently). If formal-system: the constraint is epistemic (an artifact of our axioms). Classification remains Mountain either way, but interpretation differs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontological_commitment, conceptual, 'Ontological status of the paradox constraint').

omega_variable(
    type_hierarchy_necessity,
    'Could a type-theoretic alternative (where objects have hierarchical types) eliminate the paradox-structure, or does the paradox re-emerge under different names in type systems?',
    'Formal investigation of whether type hierarchies truly avoid self-application or merely defer the constraint to a metalevel; proof-theoretic comparison of consistency strength',
    'If type systems avoid it: the constraint is specific to naive set theory (suggests decomposition). If it re-emerges: the constraint is universal to any recursively enumerable formal system (confirms Mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(type_hierarchy_necessity, empirical, 'Whether alternative formal systems escape the paradox structure').

omega_variable(
    intuitionistic_alternatives,
    'Do constructivist or intuitionistic approaches that reject the Law of Excluded Middle experience a weaker or structurally different version of the Burali-Forti constraint?',
    'Formal analysis of constructive set theory (CZF, IZF); comparison of what can and cannot be constructed in intuitionistic logic; proof that the paradox still emerges',
    'If constructivism weakens the constraint: suggests the paradox depends on classical logic assumptions. If constraint persists: strengthens Mountain classification by showing universality across constructive foundations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intuitionistic_alternatives, empirical, 'Whether constructivist logic avoids the paradox').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(burali_forte_paradox, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bfp_tr_t0, burali_forte_paradox, theater_ratio, 0, 0.05).
narrative_ontology:measurement(bfp_tr_t50, burali_forte_paradox, theater_ratio, 50, 0.08).
narrative_ontology:measurement(bfp_tr_t100, burali_forte_paradox, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(bfp_be_t0, burali_forte_paradox, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(bfp_be_t50, burali_forte_paradox, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(bfp_be_t100, burali_forte_paradox, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(burali_forte_paradox, information_standard).
narrative_ontology:affects_constraint(burali_forte_paradox, cantor_diagonal_argument).
narrative_ontology:affects_constraint(burali_forte_paradox, russell_paradox).
narrative_ontology:affects_constraint(burali_forte_paradox, limitation_theorem_hierarchy).

% DUAL FORMULATION NOTE:
% The Burali-Forti paradox is structurally upstream of the Russell paradox and the Gödel limitation hierarchy. All three are mountain constraints in mathematical logic, but they reveal different aspects of the same phenomenon: unrestricted self-reference in formal systems is incoherent. The network linkage shows how this constraint family establishes the universal bounds on what formal mathematics can express.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
