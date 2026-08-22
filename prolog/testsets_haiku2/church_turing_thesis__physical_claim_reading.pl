% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__physical_claim_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: church_turing_thesis__physical_claim_reading
 *   human_readable: Church-Turing Thesis as Physical Constraint on Computation
 *   domain: philosophy_of_computation/foundations
 *
 * SUMMARY:
 *   The Church-Turing thesis is a foundational claim in computer science and
 *   mathematical logic. This constraint story instantiates ONE reading of the
 *   contested kernel: the physical-claim reading, which asserts that no
 *   physical process can compute functions beyond Turing-machine
 *   computability. This reading treats the thesis as an empirical hypothesis
 *   about the universe, not a mathematical definition or boundary-marking
 *   principle. Under this reading, if a physical system could compute a
 *   non-Turing function, the thesis would be false. The reading creates
 *   structural asymmetry: classical computation researchers benefit from the
 *   thesis as a canonical organizing principle; hypercomputation researchers
 *   are suppressed by its use to delegitimize research into the frontier. The
 *   sibling readings (mathematical-definition and epistemological-boundary)
 *   are NOT described here; they are separate constraints with different ε
 *   values and different victim/beneficiary sets.
 *
 * KEY AGENTS:
 *   - classical_computation_researchers: institutional beneficiary, controls legitimacy framework
 *   - hypercomputation_researchers: moderate-power payer, identity-locked targets of suppression
 *   - quantum_supremacy_advocates: powerful payer, resource-rich but epistemically gatekept
 *   - peer_review_authorities: institutional agenda-setter, enforces boundary via publication control
 *   - funding_agencies: powerful agenda-setter, channels resources away from frontier research
 *   - physics_community: observer, has standing to test the thesis empirically but is excluded from the canonical CS conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, 0.58).
domain_priors:suppression_score(church_turing_thesis__physical_claim_reading, 0.67).
domain_priors:theater_ratio(church_turing_thesis__physical_claim_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__physical_claim_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__physical_claim_reading, "Church-Turing Thesis as Physical Constraint on Computation").
narrative_ontology:topic_domain(church_turing_thesis__physical_claim_reading, "philosophy_of_computation/foundations").

domain_priors:requires_active_enforcement(church_turing_thesis__physical_claim_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__physical_claim_reading, '1eea719e-2cee-4fc3-bf23-db2873fd7756').
narrative_ontology:cs_kernel_codification('1eea719e-2cee-4fc3-bf23-db2873fd7756', fixed_text).
narrative_ontology:cs_authority_grounding('1eea719e-2cee-4fc3-bf23-db2873fd7756', extraction).
narrative_ontology:cs_interpretation_layer_present('1eea719e-2cee-4fc3-bf23-db2873fd7756').
narrative_ontology:cs_reading_relation('1eea719e-2cee-4fc3-bf23-db2873fd7756', church_turing_thesis__mathematical_definition_reading, forecloses).
narrative_ontology:cs_reading_relation('1eea719e-2cee-4fc3-bf23-db2873fd7756', church_turing_thesis__epistemological_boundary_reading, influences).
narrative_ontology:cs_axiom('1eea719e-2cee-4fc3-bf23-db2873fd7756', foundational, physical_turing_ceiling).
narrative_ontology:cs_axiom_status(physical_turing_ceiling, holdable).
narrative_ontology:cs_axiom_grounding('1eea719e-2cee-4fc3-bf23-db2873fd7756', physical_turing_ceiling, empirically_contingent).
narrative_ontology:cs_axiom('1eea719e-2cee-4fc3-bf23-db2873fd7756', secondary, empirical_universality_of_turing_equivalence).
narrative_ontology:cs_axiom_status(empirical_universality_of_turing_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('1eea719e-2cee-4fc3-bf23-db2873fd7756', empirical_universality_of_turing_equivalence, empirically_contingent).
narrative_ontology:cs_reference_frame('1eea719e-2cee-4fc3-bf23-db2873fd7756', turing_physical_computability_ceiling).
narrative_ontology:cs_drift_state('1eea719e-2cee-4fc3-bf23-db2873fd7756', contemporary_quantum_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1eea719e-2cee-4fc3-bf23-db2873fd7756', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__physical_claim_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, classical_computation_researchers).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, theoretical_cs_establishment).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, hypercomputation_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, quantum_supremacy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classical computation theory forms the legitimate research foundation; the thesis vindicates their theoretical framework and grants them standing to evaluate claims about the boundaries of computation. They sustain the thesis through peer review, citation conventions, and curriculum authority. Their position benefits from the thesis because it establishes Turing-computability as the natural ceiling for physical computation, making their theoretical apparatus structurally relevant to physical questions.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, classical_computation_researchers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__physical_claim_reading, classical_computation_researchers, agenda_setter).

% Research into non-Turing computation (oracle machines, infinite-time computation, real-number computation, analog computation beyond discrete steps). Their research is structurally delegitimized by the physical-claim reading: if the thesis is empirically true, their research targets the impossible. They face publication barriers, funding skepticism, and career risk. Exit would require abandoning a research identity built around exploring computation's frontier.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, hypercomputation_researchers, payer,
    moderate, biographical, identity_locked, global).

% Claim quantum computers can solve certain problems faster than any Turing machine (though still in finite time). Under the physical-claim reading, if true they would refute the thesis, but the thesis creates interpretive pressure to either reclassify quantum speedups as still Turing-equivalent or deny their existence. They have resources to pursue research but face epistemic gatekeeping from the classical establishment.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, quantum_supremacy_advocates, payer,
    powerful, biographical, constrained, global).

% Control journal access, conference slots, and citation legitimacy. They enforce the thesis by flagging hypercomputation papers as speculative or unfalsifiable, creating a filter that suppresses the transmission of alternative ideas into the credentialed conversation. They benefit because the thesis provides a canonical boundary that makes editorial decisions appear neutral rather than preferential.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, peer_review_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Allocate research resources. Programs framed around 'understanding computation's limits' get funded; programs explicitly framed around 'exceeding Turing-computability' face skepticism or rejection. Agencies rarely state this as a policy; it emerges through grant review criteria that treat the thesis as settled physics rather than empirical hypothesis.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, funding_agencies, agenda_setter,
    powerful, generational, arbitrage, national).

% Observes but does not control the thesis's fate. Physicists studying quantum computing, analog systems, or exotic models of spacetime computation would have standing to test whether the thesis holds empirically, but the thesis's enforcement by the CS establishment limits the institutional pathways for such investigation.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, physics_community, observer,
    institutional, generational, analytical, global).

% Recognizes that Church's Thesis (the mathematical definition reading) and Turing's work (epistemological or physical reading) are distinct claims. They can do mathematics without endorsing any particular reading's empirical status, but the conflation of readings in the CS teaching canon obscures this distinction.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, mathematics_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__physical_claim_reading, classical_computation_researchers).
narrative_ontology:fixing_cost_class(church_turing_thesis__physical_claim_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a canonical, unified criterion (Turing-computability) for determining what functions can be computed in principle, enabling researchers to ask well-defined questions about computation's boundaries without infinite regress over what 'computable' means.
% TRANSFER_FUNCTION: Moves research legitimacy away from hypercomputation research toward classical computation theory. Hypercomputation researchers must spend effort defending their research direction as non-crazy rather than pursuing technical questions; classical researchers get established authority. Peer review and funding decisions channel resources accordingly.
% ABSENT_VOICES: Physicists who might empirically test whether the thesis holds in quantum regimes or exotic physical systems are absent from the canonical conversation. Alternative computational paradigms (optical, biological, chemical) are not represented in the debates about what the thesis actually constrains. The research communities for these domains are structurally not invited to adjudicate.
% DISAPPEARANCE_RATIONALE: If the physical-claim reading disappeared (replaced by the mathematical-definition or epistemological readings), hypercomputation research would be reframed as mathematically coherent even if physically impossible — a coordinate shift that would legitimize the research direction without proving it succeeds. Funding and publication barriers would dissolve not because hypercomputation works, but because the thesis would no longer suppress it as empirically presumptuous.
% FOUNDING_PROBLEM: Early computation theory needed a way to formalize what 'effectively computable' means; Church and Turing gave independent characterizations. The physical-claim reading emerged later as the thesis was repurposed to answer the empirical question: does the physical universe respect this boundary?
% FOUNDING_PROBLEM_CORROBORATION: Logicians and theoretical computer scientists confirm the founding problem (defining effective computability) was real and the thesis solved it mathematically. Physicists and experimental researchers outside the CS establishment question whether the thesis's mathematical success entitles it to make claims about physical possibility. No corroboration from hypercomputation researchers exists; they would dispute both the founding problem statement and the claim that the thesis settles it.
narrative_ontology:disappearance_verdict(church_turing_thesis__physical_claim_reading, contested).
narrative_ontology:founding_problem_status(church_turing_thesis__physical_claim_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__physical_claim_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(church_turing_thesis__physical_claim_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__physical_claim_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__physical_claim_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(church_turing_thesis__physical_claim_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.58 reflects the constraint's asymmetric benefit structure: the thesis benefits a broad establishment (classical theory, peer review gatekeepers, funding allocators) by providing a canonical legitimacy criterion, while concentrating costs on researchers exploring computation beyond that boundary. Suppression at 0.67 captures the enforcement mechanisms: peer review gatekeeping, funding barriers, and career-path closure for those who pursue hypercomputation. The measurement series show accumulating suppression (0.45→0.67 from t=0 to t=60), indicating that enforcement infrastructure hardened over the interval as quantum computing and exotic physical models generated pressure. Theater ratio at 0.41 reflects moderate performativity: the thesis is genuinely intellectually respectable, but an increasing portion of its maintenance is defensive (arguments about why quantum supremacy claims don't refute it, why oracle machines aren't 'real physics') rather than generative (proofs that expand classical computation theory). The measured extractiveness plateaus at t=60-80, suggesting enforcement saturation — the thesis is now so institutionalized that further suppression gains are minimal. Accessibility collapse at 0.62 reflects the reality that alternatives (hypercomputation, stronger-than-Turing models) are not physically or conceptually impossible — they remain accessible as research ideas — but the institutional apparatus creates very high friction to pursue them. Resistance at 0.71 indicates substantial and active resistance from quantum computing advocates and frontier researchers, which is why suppression must remain high.
 *
 * PERSPECTIVAL GAP:
 *   The classical-computation institutional seat and the hypercomputation-researcher seat compute radically different types. From the institutional perspective (peer-review boards, funding agencies, established researchers), the constraint appears as coordination: it provides a canonical, mathematically rigorous boundary that makes possible a unified field of inquiry. From the hypercomputation-researcher perspective, the same structure appears as extraction: legitimacy is withdrawn from their questions, funding is denied, publication barriers are erected. The engine computes this divergence from the structural data (beneficiary vs. victim, institutional vs. moderate power, arbitrage vs. identity-locked exit). The authored claim (tangled_rope) reflects the institutional reading — there IS genuine coordination (defining what 'computable' means) — while the metrics reflect the extractive reality that the coordination is asymmetric and requires active suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical researchers and peer-review authorities are beneficiaries (d near 0.0): they gain legitimacy, authority over the boundary, and protection of their research domain. Hypercomputation researchers are targets (d near 1.0): they bear the cost of suppression, identity-locking (their career is entangled with the question of whether hypercomputation is possible), and constrained exit (they can pursue hypercomputation in a basement with no funding, but not in credentialed institutions). Quantum supremacy advocates occupy a complex position: powerful (resources, publicity), but their claims are under continuous reinterpretation pressure (forced to argue their speedups are still Turing-equivalent, or that they misunderstood the experiment). The funding agencies derive d from their role as agenda-setters: they benefit from the thesis because it reduces uncertainty (they can fund research they know is within-bounds), but they are not the primary capturer of extraction (that role belongs to the classical establishment). Physics observers have analytical exit (they can study the question independently) but are not part of the CS institutional hierarchy.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows the signature of mandatrophy: the founding problem (defining effective computability) is largely solved and institutionalized; the constraint persists not because it solves that problem continuously, but because the institutional structure built around it generates path-dependent inertia. The measurement series show extractiveness rising to 0.58 and then plateauing, while suppression stays high — this is the pattern of a constraint that has become mostly performance (defending the boundary) rather than functional (advancing the coordination it was built for). The theater_ratio rising to 0.41 supports this reading. However, the constraint has not yet reached piton status because genuine coordination persists (the boundary is intellectually real and useful), and beneficiaries still actively defend it rather than merely preserving it ceremonially. A Tangled Rope classification captures this: there is real coordination (the unified boundary), real extraction (the suppression of alternatives), and it requires active enforcement (peer review, funding gatekeeping). The constraint would move toward Piton if the theater ratio continued rising and extractiveness stabilized while suppression had to increase just to maintain the status quo.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_testability_boundary,
    'Is the thesis empirically testable as a physical claim, or is it fundamentally non-falsifiable because any apparent non-Turing-computation can be reinterpreted as Turing-equivalent under a broader physical model?',
    'Attempt to construct a concrete physical system that computes a provably non-Turing function (oracle access, infinite-time operation, real-number representation). If such a system is built and the classical establishment accepts the result, the thesis is falsified; if they reinterpret the system as still within Turing bounds, the thesis reveals itself as non-falsifiable and should be reclassified as mathematical definition or epistemological boundary rather than empirical claim.',
    'If non-falsifiable, the constraint''s classification shifts from Tangled Rope (empirically contested, suppressing genuine alternatives) to Rope or even Snare (a definitional boundary misrepresented as physical claim, suppressing competitors to a convention). If falsifiable and someone constructs a counterexample, the constraint''s type inverts to Scaffold or degrades to Piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_testability_boundary, empirical, 'Whether the thesis can be empirically tested or is non-falsifiable by construction.').

omega_variable(
    reading_conflation_ambiguity,
    'To what extent is the suppression measured in this constraint attributable to active gatekeeping of the physical-claim reading specifically, versus structural gatekeeping of hypercomputation research regardless of which reading (mathematical, epistemological, or physical) is invoked?',
    'Comparative analysis: survey rejection reasons for hypercomputation papers; distinguish between rejections citing the thesis as an empirical constraint (''this contradicts Church-Turing'') versus rejections citing the thesis as a mathematical definition (''this asks a question outside the definition of computable'') versus rejections citing impracticality (''this requires unphysical resources''). If rejections cluster around empirical-claim framing, the suppression is reading-specific. If they are indifferent to the reading, the suppression is structural to hypercomputation research, independent of which reading of the kernel is adopted.',
    'If reading-specific, the physical-claim reading is the active suppressor; changing to the mathematical-definition reading could reduce suppression while preserving coordination. If structural, suppression persists under any reading and indicates the problem is not the thesis''s empirical status but the research community''s boundaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_conflation_ambiguity, empirical, 'Whether suppression is due to the physical-claim reading or to pre-existing gatekeeping of hypercomputation.').

omega_variable(
    quantum_supremacy_refutation_scenario,
    'If quantum computers demonstrably solve specific decision problems faster than any known Turing machine, and physicists accept this as genuine non-Turing speedup, does the physical-claim reading of Church-Turing collapse or does the CS establishment reclassify the speedup as still within Turing-equivalence under a broader model?',
    'Empirical result from quantum computing experiments followed by institutional response from the CS establishment. Observe whether the establishment accepts the result as a refutation or reinterprets quantum speedup as a special case of Turing computation under a redefined physical model.',
    'Acceptance of refutation would force reclassification to Scaffold (the constraint was temporary, opening new physics). Reinterpretation as within-bounds would demonstrate the thesis as non-falsifiable and reveal the constraint as Snare (suppression defended by interpretive flexibility rather than empirical support).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_supremacy_refutation_scenario, empirical, 'Whether quantum supremacy claims can refute the physical-claim reading or are re-absorbed into Turing-equivalence.').

omega_variable(
    reading_identification_in_canon,
    'To what extent do teaching materials, textbooks, and canonical papers explicitly distinguish between the mathematical-definition reading, the epistemological-boundary reading, and the physical-claim reading? Or are they conflated in the standard presentation?',
    'Audit of 20–30 leading textbooks and Stanford Encyclopedia entries on Church-Turing, comparing how much each source disambiguates the readings versus presenting ''the Church-Turing thesis'' as a single claim.',
    'If conflation is dominant, the suppression of hypercomputation research is amplified by a reading-fusion that makes it hard to articulate alternatives (the physical reading is used to suppress research the mathematical reading would permit). If disambiguation is standard, suppression is more transparent and contestable. This affects how visible the constraint''s structure is to researchers outside the establishment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identification_in_canon, conceptual, 'Whether the thesis''s multiple readings are canonically distinguished or conflated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__physical_claim_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t0, church_turing_thesis__physical_claim_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(chur_tr_t10, church_turing_thesis__physical_claim_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(chur_tr_t20, church_turing_thesis__physical_claim_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(chur_tr_t40, church_turing_thesis__physical_claim_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(chur_tr_t60, church_turing_thesis__physical_claim_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(chur_tr_t80, church_turing_thesis__physical_claim_reading, theater_ratio, 80, 0.41).

% Extraction over time
narrative_ontology:measurement(chur_be_t0, church_turing_thesis__physical_claim_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(chur_be_t10, church_turing_thesis__physical_claim_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(chur_be_t20, church_turing_thesis__physical_claim_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(chur_be_t40, church_turing_thesis__physical_claim_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(chur_be_t60, church_turing_thesis__physical_claim_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(chur_be_t80, church_turing_thesis__physical_claim_reading, base_extractiveness, 80, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t0, church_turing_thesis__physical_claim_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(chur_su_t10, church_turing_thesis__physical_claim_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(chur_su_t20, church_turing_thesis__physical_claim_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(chur_su_t40, church_turing_thesis__physical_claim_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(chur_su_t60, church_turing_thesis__physical_claim_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(chur_su_t80, church_turing_thesis__physical_claim_reading, suppression_requirement, 80, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__physical_claim_reading, information_standard).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__physical_claim_reading, 0.05).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% The Church-Turing thesis is a single persisting kernel (formalized in Turing's 1936 paper and Church's Lambda-Calculus work) that admits three structurally distinct readings: (1) Mathematical Definition — the thesis stipulates what we mean by 'effectively computable' (true by convention, non-empirical). (2) Epistemological Boundary — the thesis marks the frontier of formally provable computation, regardless of physical possibility. (3) Physical Claim — the thesis is an empirical hypothesis about what the physical universe can compute. Each reading instantiates a different constraint with different ε, beneficiary/victim structure, and classification. The physical-claim reading (this story) treats the thesis as an empirical constraint on computation; it forecloses the mathematical-definition reading's non-empirical status and influences the epistemological-boundary reading by adding physical testability as a criterion. The three stories form a constraint family and should be analyzed together to understand how a single intellectual artifact (the thesis) functions differently under different framings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
