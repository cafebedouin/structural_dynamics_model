% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__physical_claim_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_church_turing_thesis__physical_claim_reading, []).

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
 *   constraint_id: church_turing_thesis__physical_claim_reading
 *   human_readable: Church-Turing Thesis as Physical Constraint (Physical Computability Claim)
 *   domain: philosophy_of_computation/foundations_of_computer_science
 *
 * SUMMARY:
 *   The Church-Turing thesis, as a physical claim about the universe, asserts
 *   that no physical process can compute functions beyond Turing-machine
 *   computability. This constraint story instantiates ONE reading of a
 *   contested kernel (the Church-Turing thesis itself). The physical reading
 *   interprets CT not as a mathematical definition of 'effective procedure'
 *   but as an empirical claim about what is physically possible. This reading
 *   is structurally distinct from the mathematical definition reading (which
 *   treats CT as tautological) and the epistemological boundary reading
 *   (which treats CT as a methodological limit on legitimate research). The
 *   physical reading has genuine bite: if true, it suppresses research into
 *   hypercomputation, oracle machines, and quantum computation theories that
 *   exceed Turing bounds. If false, it represents a false constraint —
 *   institutional enforcement of a thesis that has been refuted by physical
 *   discovery. The constraint exhibits all six classification types across
 *   different observer positions, making it a diagnostic exemplar for how
 *   reading-dependent constraints operate.
 *
 * KEY AGENTS:
 *   - Classical Computation Framework: Primary beneficiary (institutional/arbitrage) — gains epistemological monopoly, resource concentration, unquestioned legitimacy in curriculum and funding
 *   - Hypercomputation Research Communities: Primary victim (powerless/trapped) — face systematic suppression, publication barriers, funding denial, career risk for pursuing theories that exceed CT bounds
 *   - Quantum Supremacy Claimants: Secondary victim (powerless/trapped) — if their claims imply hypercomputation, they face delegitimization via CT enforcement
 *   - Mainstream TCS Community: Moderate actor (moderate/constrained) — benefits from CT's coordination and suppression effects; constrained by need to maintain consensus on boundaries
 *   - Mathematical Logic Establishment: Institutional beneficiary (institutional/arbitrage) — maintains authority over computational definitions; can arbitrage between readings
 *   - Future Hypercomputation Theorists: Organized agent (organized/mobile) — see CT physical reading as temporary barrier with eventual sunset if hypercomputation is realized
 *   - Disciplinary Boundary System: Institutional performer (institutional/arbitrage) — maintains CT's status through institutional machinery; benefits from boundary maintenance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, 0.48).
domain_priors:suppression_score(church_turing_thesis__physical_claim_reading, 0.62).
domain_priors:theater_ratio(church_turing_thesis__physical_claim_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__physical_claim_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__physical_claim_reading, "Church-Turing Thesis as Physical Constraint (Physical Computability Claim)").
narrative_ontology:topic_domain(church_turing_thesis__physical_claim_reading, "philosophy_of_computation/foundations_of_computer_science").

domain_priors:requires_active_enforcement(church_turing_thesis__physical_claim_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__physical_claim_reading, '9541923a-2477-433f-9e25-76a18932b467').
narrative_ontology:cs_kernel_codification('9541923a-2477-433f-9e25-76a18932b467', fixed_text).
narrative_ontology:cs_authority_grounding('9541923a-2477-433f-9e25-76a18932b467', extraction).
narrative_ontology:cs_interpretation_layer_present('9541923a-2477-433f-9e25-76a18932b467').
narrative_ontology:cs_reading_relation('9541923a-2477-433f-9e25-76a18932b467', church_turing_thesis__mathematical_definition_reading, forecloses).
narrative_ontology:cs_reading_relation('9541923a-2477-433f-9e25-76a18932b467', church_turing_thesis__epistemological_boundary_reading, influences).
narrative_ontology:cs_axiom('9541923a-2477-433f-9e25-76a18932b467', foundational, physical_process_limited_to_turing_computation).
narrative_ontology:cs_axiom_status(physical_process_limited_to_turing_computation, holdable).
narrative_ontology:cs_axiom_grounding('9541923a-2477-433f-9e25-76a18932b467', physical_process_limited_to_turing_computation, empirically_contingent).
narrative_ontology:cs_axiom('9541923a-2477-433f-9e25-76a18932b467', secondary, hypercomputation_physically_impossible).
narrative_ontology:cs_axiom_status(hypercomputation_physically_impossible, holdable).
narrative_ontology:cs_axiom_grounding('9541923a-2477-433f-9e25-76a18932b467', hypercomputation_physically_impossible, empirically_contingent).
narrative_ontology:cs_reference_frame('9541923a-2477-433f-9e25-76a18932b467', classical_computability_universality).
narrative_ontology:cs_drift_state('9541923a-2477-433f-9e25-76a18932b467', contemporary_quantum_supremacy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9541923a-2477-433f-9e25-76a18932b467', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__physical_claim_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, classical_computation_framework).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, hypercomputation_research_programs).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, quantum_advantage_interpretations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HYPERCOMPUTATION RESEARCHERS (SNARE) — Trapped by the physical reading's suppression: research proposals targeting computation beyond Turing limits face systematic skepticism, funding barriers, and publication rejection. No legitimate exit path if the thesis is enforced as physical law rather than definition. Bears extraction in the form of career risk and resource denial with minimal countervailing benefit.
constraint_indexing:constraint_classification(church_turing_thesis__physical_claim_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: QUANTUM SUPREMACY CLAIMS EXCEEDING CT BOUNDS (SNARE) — If interpreted quantum advantage claims imply hypercomputation, those claims face systematic suppression via the thesis's force as physical law. The constraints are identical to hypercomputation researchers: trapped by the framework, bearing extraction via delegitimization and funding denial.
constraint_indexing:constraint_classification(church_turing_thesis__physical_claim_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MAINSTREAM TCS COMMUNITY (TANGLED ROPE) — Benefits from CT as a unified framework for computability theory (genuine coordination of definitions, proof techniques, curriculum). Also benefits from the suppression effect: mainstream TCS excludes hypercomputation from legitimate research space, concentrating resources and prestige. Constrained by the need to maintain consensus and defend borders. Moderate experienced extraction due to coordination benefits offsetting some asymmetric resource concentration.
constraint_indexing:constraint_classification(church_turing_thesis__physical_claim_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: MATHEMATICAL LOGIC ESTABLISHMENT (ROPE) — Treats CT as definitional boundary (mathematical claim, not physical). Low suppression; treats hypercomputation as definitionally coherent but empirically unimplemented. Net beneficiary via preserved authority over computational definitions. Arbitrage option: can reframe to mathematical reading if physical reading becomes untenable.
constraint_indexing:constraint_classification(church_turing_thesis__physical_claim_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HYPERCOMPUTATION THEORISTS (FUTURE-ORIENTED SCAFFOLD) — Organized groups developing theory of hypercomputation (unconventional computing, oracle machines, quantum computation models beyond standard QC bounds) see the physical reading of CT as a temporary barrier. If hypercomputation is eventually realized physically, the constraint has a sunset. Current suppression is moderate-high, but organized agents see a path forward. Scaffold classification reflects the generational time horizon and the emerging research infrastructure.
constraint_indexing:constraint_classification(church_turing_thesis__physical_claim_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: DISCIPLINARY BOUNDARY MAINTENANCE (PITON) — The institutional machinery that enforces CT's status in curricula, funding criteria, and research legitimacy is largely performative. The theater ratio (0.58) reflects the sustained rhetorical work maintaining CT's universal applicability despite (a) mounting empirical edge cases in quantum computing, (b) theoretical frameworks beyond CT (hypercomputation, oracle machines), and (c) physical theories (quantum mechanics, relativity) that may violate classical computability assumptions. The machinery persists through institutional inertia and the benefit structure it creates, not because the physical claim is demonstrably true.
constraint_indexing:constraint_classification(church_turing_thesis__physical_claim_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / LOGICO-MATHEMATICAL NECESSITY VIEW (MOUNTAIN) — From a civilizational perspective viewing CT as a mathematical tautology about the nature of formal computation itself, the constraint appears immutable: if Turing-computability is the definition of effective procedure, nothing can exceed it by definition. However, this reading conflates the mathematical claim (definable via Church-Turing) with the physical claim (implementable in the physical universe). The false summit detection system will flag this as naturalization of a definitional boundary into a physical law.
constraint_indexing:constraint_classification(church_turing_thesis__physical_claim_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / PHYSICAL READING (IDENTITY-LOCKED TANGLED ROPE) — An analyst whose disciplinary identity and career trajectory are fused with CT-as-physical-law cannot easily perceive alternatives without abandoning their epistemic frame. This perspective demonstrates the oracle gap (Theorem 4 U₄): the identity-locked analytical position cannot see that CT is one reading among siblings; only cross-position analysis reveals the committer structure. The analyst might see genuine coordination (mathematical foundation) and genuine extraction (suppression of hypercomputation) simultaneously, classifying as tangled rope, but cannot perceive why the reading-identity fusion prevents seeing the alternative readings.
constraint_indexing:constraint_classification(church_turing_thesis__physical_claim_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__physical_claim_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(church_turing_thesis__physical_claim_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(church_turing_thesis__physical_claim_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(church_turing_thesis__physical_claim_reading, TR),
    TR >= 0.70.

:- end_tests(church_turing_thesis__physical_claim_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The physical reading creates asymmetric benefit for the classical framework (gain monopoly, suppress alternatives) while imposing costs on hypercomputation research. However, the extraction is not maximal (0.66+) because: (a) the suppression is partly motivated by legitimate theoretical concerns (CT provably limits Turing-computable functions), not pure institutional rent-seeking; (b) mainstream TCS does provide genuine coordination benefits (shared definitions, proof techniques, pedagogical clarity); (c) the reading itself remains contested — it is not presented as obvious physical law but requires active defense. The trajectory shows accumulating extractiveness over time (0.32 → 0.48) as quantum computing advances force more rhetorical and institutional work to defend CT's physical scope against empirical challenges. Suppression (0.62): High. Research proposals targeting hypercomputation or oracle computation face systematic skepticism from funding bodies, journal reviewers, and hiring committees. The suppression is real but not absolute — hypercomputation research is academically possible, just heavily constrained. The suppression_requirement measurement shows increasing institutional work (0.48 → 0.62) to defend boundaries as quantum computing enables claims that approach or challenge CT bounds. Theater ratio (0.58): Moderate-high. Much of the institutional defense of CT's physical scope is performative: rhetorical reaffirmations of CT's universality in contexts (quantum computation, DNA computing, exotic physics) where the thesis is not directly threatened. The theater has increased over time as the gap between CT's stated universality and its challenged scope has widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits the full perspectival spectrum. Hypercomputation researchers see a snare: suppression without exit, extraction without benefit. Mainstream TCS sees tangled rope: genuine coordination benefits (unified framework) mixed with asymmetric extraction (suppression of alternatives). Mathematical logic sees rope: CT as pure coordination mechanism, no extraction. The open hypercomputation theorists see scaffold: the constraint as temporary, with a sunset as quantum computation develops. The boundary maintenance system sees piton: performative ritual sustaining a framework whose primary function has degraded. The analytical observer risks seeing mountain (CT as immutable logical necessity), but the structural data reveals false summit: identifiable beneficiaries, suppression of research, theater protecting a contingent boundary. The perspectival gap between the mathematical reading (rope/mountain) and the physical reading (tangled rope/snare) is the core of the kernel dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) and experienced extractiveness (χ) is computed from the agent's power, exit options, and structural role. Hypercomputation researchers: trapped with no exit (d ≈ 0.95) → high f(d) → high χ, classifying as snare. Mainstream TCS: moderate power, constrained exit, mixed beneficiary/victim status (d ≈ 0.55) → moderate f(d) → moderate χ, classifying as tangled rope. Mathematical logic: institutional power, arbitrage exit, pure beneficiary status (d ≈ 0.15) → low f(d) → low/negative χ, classifying as rope. The canonical fallback values produce the expected classifications when structural data is absent. The identity_locked analytical perspective (perspective 8) instantiates the oracle gap: an analyst whose professional identity is fused with the physical reading cannot perceive the reading distinction from within their frame; only cross-position analysis reveals the committer structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the physical reading of CT is structurally distinct from its siblings and requires its own constraint story. The extractiveness (0.48) reflects that the physical reading creates real suppression (beneficiary gains monopoly, victims lose research opportunity) but also genuine coordination (CT provides unified framework for complexity theory and computability). The tangled rope classification is appropriate: not pure coordination (victim perspective is snare), not pure extraction (beneficiary perspective is rope), but hybrid. The false summit alert fires because the analytical observer perspective risks treating the physical claim as mountain (logical necessity), when the structural data (beneficiaries, suppression, increasing theater) indicates it is more accurately tangled rope with contingent institutional support.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_vs_mathematical_instantiation,
    'Is the Church-Turing thesis a claim about mathematical definitions (what counts as ''effective procedure'') or about physical implementation (what physical processes can compute)?',
    'Explicit definitional parsing: does CT claim that no function can be computed (mathematical), or that no physical process can compute beyond Turing bounds (physical)? Empirical test: if a physical system exhibits non-Turing computation, does this refute CT or merely refute its physical reading?',
    'Mathematical reading: CT is tautological (mountain). Physical reading: CT is empirically testable (tangled rope or scaffold). This omega is the core structural distinction between readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_vs_mathematical_instantiation, conceptual, 'Whether CT is a mathematical definition or a physical law').

omega_variable(
    quantum_computation_interpretation_ambiguity,
    'Do quantum computers that achieve ''quantum advantage'' implement hypercomputation, or do they remain within Turing limits via polynomial-time reduction?',
    'Formal analysis of claimed quantum advantage: does it exceed Turing-computable functions, or only reduce polynomial-time factors within Turing bounds? Empirical verification of specific supremacy claims (boson sampling, QAOA, Shor''s algorithm).',
    'If quantum advantage stays within Turing bounds: physical reading of CT survives, suppression of hypercomputation research continues. If quantum advantage exceeds Turing bounds: CT''s physical reading is refuted; research suppression loses justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_computation_interpretation_ambiguity, empirical, 'Whether quantum advantage exceeds Turing-computable functions').

omega_variable(
    oracle_machine_physical_realizability,
    'Can oracle machines (which compute beyond standard Turing limits by assumption) be physically instantiated, or are they mathematically coherent but physically impossible?',
    'Theoretical physics analysis: do known physical laws (quantum mechanics, relativity, thermodynamics) permit oracle implementation? Experimental search for physical systems exhibiting oracle-like behavior (infinite information density, instant queries, unbounded memory).',
    'If oracle machines are physically impossible: physical reading of CT holds (with caveats about oracle boundary). If physically realizable: physical reading is refuted; constraint type shifts to scaffold (temporary suppression dissolving as technology develops).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(oracle_machine_physical_realizability, empirical, 'Whether oracle machines are physically realizable').

omega_variable(
    reading_committer_boundary,
    'Is this constraint (physical claim reading) a genuine alternative reading of the Church-Turing kernel, or a confusion of categories that blurs mathematical and physical domains?',
    'Meta-analysis: does the physical reading''s core premise (no physical process exceeds Turing limits) form a coherent, distinct position that differs substantively from the mathematical reading (Turing-computable = effective procedure)? Do different communities adopt different readings, or is this a false distinction?',
    'If genuine reading: three-way family of constraints (mathematical, physical, epistemological) captures real disagreement. If false distinction: physical reading should decompose into separate constraints (quantum computation claim, thermodynamic bound claim, etc.) rather than form a unified reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_boundary, conceptual, 'Whether physical reading is a genuine alternative or categorical confusion').

omega_variable(
    suppression_motivation_ambiguity,
    'Is the suppression of hypercomputation research (measured in this story at 0.62) motivated by legitimate epistemological boundary-setting (preventing fruitless research directions), or by institutional investment in the classical framework?',
    'Historical analysis: when hypercomputation proposals are rejected, what are the stated vs. actual grounds? Do review comments reflect theoretical impossibility proofs (legitimate) or dismissal of the category itself (institutional)? Comparison to other ''fringe'' research areas that eventually succeeded.',
    'If legitimate boundary: suppression is a coordination mechanism (moderate extracted value is fair cost for preventing wasted effort). If institutional: suppression is extractive overhead, and ε should rise toward 0.62+ (snare territory). Current 0.48 assumes mixed motivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_motivation_ambiguity, empirical, 'Whether suppression is legitimate boundary-setting or institutional extraction').

omega_variable(
    hypercomputation_victim_identity,
    'Who are the actual victims of the physical reading''s suppression? Is it hypercomputation researchers (identifiable community), or abstract theoretical possibilities (no identifiable victim set)?',
    'Institutional mapping: identify funded hypercomputation research programs, principal investigators, graduate students explicitly targeting beyond-Turing computation. Measure career impact and resource allocation disparities relative to mainstream TCS.',
    'If identifiable victims exist: snare classification for hypercomputation perspective is justified. If no victim community: suppression may be performative (piton gate), and the constraint''s extractiveness is theatrical rather than material.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hypercomputation_victim_identity, empirical, 'Whether hypercomputation research forms an identifiable victim community').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__physical_claim_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ct_physical_tr_t0, church_turing_thesis__physical_claim_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ct_physical_tr_t20, church_turing_thesis__physical_claim_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(ct_physical_tr_t40, church_turing_thesis__physical_claim_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(ct_physical_be_t0, church_turing_thesis__physical_claim_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ct_physical_be_t20, church_turing_thesis__physical_claim_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(ct_physical_be_t40, church_turing_thesis__physical_claim_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(ct_physical_su_t0, church_turing_thesis__physical_claim_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(ct_physical_su_t20, church_turing_thesis__physical_claim_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(ct_physical_su_t40, church_turing_thesis__physical_claim_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__physical_claim_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__epistemological_boundary_reading).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, quantum_computation_beyond_classical_bounds).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, oracle_machine_physical_realizability).

% DUAL FORMULATION NOTE:
% The Church-Turing thesis decomposes into structurally distinct claims with different ε values and different victims: (1) MATHEMATICAL READING (ε ≈ 0.05, Mountain) — definitional tautology, no suppression, no victims; (2) PHYSICAL READING (ε ≈ 0.48, Tangled Rope, THIS STORY) — empirical claim, suppresses hypercomputation research, creates identifiable victims; (3) EPISTEMOLOGICAL READING (ε ≈ 0.30, Rope/Scaffold) — methodological boundary, modest coordination benefits, minimal suppression. Each reading has different sibling relationships in the constraint family. The network links show how disputes about CT's true reading structure the landscape of hypercomputation research, quantum advantage claims, and the philosophy of computation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
