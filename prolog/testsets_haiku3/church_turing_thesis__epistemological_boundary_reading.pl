% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__epistemological_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_church_turing_thesis__epistemological_boundary_reading, []).

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
 *   constraint_id: church_turing_thesis__epistemological_boundary_reading
 *   human_readable: Church-Turing Thesis as Epistemological Boundary (Proof-Theoretic Reading)
 *   domain: philosophy_of_mathematics/computability_theory/foundations
 *
 * SUMMARY:
 *   The Church-Turing thesis is a statement about the boundary between
 *   formally knowable computation and what lies beyond. Under the
 *   epistemological-boundary reading, the thesis does not claim that Turing
 *   computability is all that computation *is* in nature or in principle; it
 *   claims that Turing computability is exactly what we can *prove*
 *   computable using formal methods. This reading treats the thesis as a
 *   methodological constraint on proof theory: it defines what counts as a
 *   valid computability proof, which narrows the legitimacy space for
 *   non-constructive arguments and hypercomputational claims. The constraint
 *   benefits proof-theoretic communities by giving them a rigorous
 *   demarcation; it extracts a cost from non-Turing-aligned research programs
 *   by excluding them from the frame of 'computability' altogether. The claim
 *   is Tangled Rope (genuine coordination function in proof-theoretic
 *   methodology, plus asymmetric extraction excluding other research areas);
 *   the metrics reflect low-to-moderate extractiveness because the extraction
 *   works through definitional/category exclusion rather than overt coercion.
 *
 * KEY AGENTS:
 *   - proof_theoretic_metamathematics: institutional beneficiary — stakes rigorous proof standards on the boundary
 *   - constructive_proof_communities: organized beneficiary — uses the boundary to distinguish valid from invalid proof methods
 *   - hypercomputation_programs: powerless payer — claims to expand computation excluded from legitimacy
 *   - non_constructive_computability_claims: powerless payer — identity-locked to classical mathematics, constrained by the boundary
 *   - physical_computation_research: organized payer — excluded from 'formal' computability discourse
 *   - mathematical_foundationalists: agenda-setters — enforce the boundary through peer review and norms
 *   - quantum_computation_advocates: powerful excluded parties — cannot claim to have transcended formal computability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__epistemological_boundary_reading, 0.42).
domain_priors:suppression_score(church_turing_thesis__epistemological_boundary_reading, 0.31).
domain_priors:theater_ratio(church_turing_thesis__epistemological_boundary_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__epistemological_boundary_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__epistemological_boundary_reading, "Church-Turing Thesis as Epistemological Boundary (Proof-Theoretic Reading)").
narrative_ontology:topic_domain(church_turing_thesis__epistemological_boundary_reading, "philosophy_of_mathematics/computability_theory/foundations").

domain_priors:requires_active_enforcement(church_turing_thesis__epistemological_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__epistemological_boundary_reading, 'e5b73ace-2603-41af-a748-cde9480fc0ff').
narrative_ontology:cs_kernel_codification('e5b73ace-2603-41af-a748-cde9480fc0ff', formalized).
narrative_ontology:cs_authority_grounding('e5b73ace-2603-41af-a748-cde9480fc0ff', lineage).
narrative_ontology:cs_interpretation_layer_present('e5b73ace-2603-41af-a748-cde9480fc0ff').
narrative_ontology:cs_reading_relation('e5b73ace-2603-41af-a748-cde9480fc0ff', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5b73ace-2603-41af-a748-cde9480fc0ff', church_turing_thesis__physical_claim_reading, influences).
narrative_ontology:cs_axiom('e5b73ace-2603-41af-a748-cde9480fc0ff', foundational, formal_computability_coextensive_with_turing_computability).
narrative_ontology:cs_axiom_status(formal_computability_coextensive_with_turing_computability, holdable).
narrative_ontology:cs_axiom_grounding('e5b73ace-2603-41af-a748-cde9480fc0ff', formal_computability_coextensive_with_turing_computability, conventional).
narrative_ontology:cs_axiom('e5b73ace-2603-41af-a748-cde9480fc0ff', foundational, proof_theoretic_knowability_distinct_from_physical_realizability).
narrative_ontology:cs_axiom_status(proof_theoretic_knowability_distinct_from_physical_realizability, holdable).
narrative_ontology:cs_axiom_grounding('e5b73ace-2603-41af-a748-cde9480fc0ff', proof_theoretic_knowability_distinct_from_physical_realizability, deontological).
narrative_ontology:cs_reference_frame('e5b73ace-2603-41af-a748-cde9480fc0ff', turing_equivalence_as_proof_theoretic_boundary).
narrative_ontology:cs_drift_state('e5b73ace-2603-41af-a748-cde9480fc0ff', contemporary_hypercomputation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e5b73ace-2603-41af-a748-cde9480fc0ff', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, proof_theoretic_metamathematics).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, constructive_proof_communities).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, hypercomputation_programs).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_claims).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, physical_computation_research).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The epistemological boundary reading anchors the proof-theoretic program: computability becomes coextensive with formal demonstrability, which makes the boundary between decidable and undecidable problems a precise, enforceable distinction within logic and set theory. Proof-theoretic research operates under the tacit assumption that 'formally knowable computation' is the right target. Benefits from the constraint by having a stable, mathematically rigorous demarcation.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, proof_theoretic_metamathematics, beneficiary,
    institutional, generational, arbitrage, global).

% Constructivists (Bishop-style, intuitionist, and related frameworks) use the boundary reading to distinguish constructively valid proofs from non-constructive arguments. The constraint supports their disciplinary claim that 'constructible' is a meaningful subcategory of 'computable.' Can exit by adopting a different reading, but the boundary reading gives them a formal foundation for their distinctions.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, constructive_proof_communities, beneficiary,
    organized, generational, mobile, global).

% Research programs exploring hypercomputational models (oracle machines, infinite-time Turing machines, analog computers, quantum algorithms beyond simulation) must operate in the knowledge that their proposals are not claims about formal computability in the proof-theoretic sense. They cannot claim to expand the boundary of formally knowable computation; they can only claim to expand what is physically or logically possible. The constraint excludes them from the legitimacy frame of 'computability proof'.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, hypercomputation_programs, payer,
    moderate, biographical, constrained, global).

% Mathematical arguments that prove a function is computable by non-constructive means (e.g., by appeal to the law of excluded middle without providing an explicit algorithm) are excluded from the proof-theoretic framework's legitimacy. Under the boundary reading, such proofs do not establish formal computability; they establish only that the existence of an algorithm follows from classical logic. Trapped because the identity of mathematics is fused with classical proof methods.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_claims, payer,
    powerless, biographical, identity_locked, global).

% Researchers investigating whether physical processes (quantum systems, biological networks, optical computers) can compute beyond Turing's bound face a structural barrier: under the boundary reading, any such computation would not be 'formally knowable computation' but rather 'what physics permits.' Their research is not silenced but is re-categorized outside the boundary the thesis marks. They bear the cost of being excluded from computability discourse proper; their findings are treated as empirical/physical, not foundational.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, physical_computation_research, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__epistemological_boundary_reading, physical_computation_research, excluded).

% Mathematicians and logicians who work in proof theory, recursion theory, and metamathematics set and defend the boundary. They enforce it through peer review, journal gatekeeping, and disciplinary norms about what counts as a 'computability result.' They determine which proofs and which proposals fall inside or outside the boundary.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, mathematical_foundationalists, agenda_setter,
    institutional, generational, analytical, global).

% Researchers claiming quantum computers can solve problems intractable for classical Turing machines are excluded from claiming they have expanded the boundary of formally computable functions. They must frame their results as 'quantum speedup' or 'complexity-class separation' rather than 'hypercomputation.' The constraint prevents them from making foundational claims about the nature of computation itself.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, quantum_computation_advocates, excluded,
    powerful, biographical, constrained, global).

% Philosophers and cognitive scientists who argue that human cognition can perform non-Turing-computable operations face a boundary: the epistemological reading excludes such arguments from the frame of 'formally knowable computation.' They must either deny that formal knowability applies to cognition or accept that human thought contains non-computable elements outside the thesis's scope. Either way, they bear the cost of operating in a constrained legitimacy space.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, philosophy_of_mind_physicalists, excluded,
    moderate, biographical, constrained, global).

% An analytical seat tracking how the boundary reading operates: which communities enforce it, which resist it, what costs and benefits accrue, and how the boundary might shift if other readings became dominant.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, mathematical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__epistemological_boundary_reading, proof_theoretic_metamathematics).
narrative_ontology:fixing_cost_class(church_turing_thesis__epistemological_boundary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a precise, formally defensible boundary between what counts as 'formally knowable computation' and what does not. Enables mathematicians to partition proofs into those that establish computability (Turing-computable functions) and those that do not (non-constructive proofs, hypercomputational schemes). Solves the coordination problem of having a shared, rigorous standard for what 'computable' means in proof-theoretic work.
% TRANSFER_FUNCTION: Transfers legitimacy and research funding from hypercomputation programs, non-constructive proof methods, and physical-computation research toward proof-theoretic metamathematics and constructive proof communities. A function once classified as 'computable' under a non-boundary reading becomes 'not formally knowable as computable' under this reading — the researchers working on such functions lose the claim to be studying computation itself.
% ABSENT_VOICES: Researchers in quantum computation, analog computation, and oracle machines would object if heard: they would argue that the boundary is an epistemic choice, not a fact about what computation is, and that their research expands the natural concept of computability. Physical systems researchers would argue the thesis conflates 'formal provability' with 'what is computable in nature.' Non-constructive mathematicians would argue that classical proof methods establish real mathematical truths about computability.
% DISAPPEARANCE_RATIONALE: If this reading of the thesis disappeared and a physical-claim reading became dominant, the landscape would rearrange: hypercomputation would become a legitimate research area within computability theory proper; non-constructive proofs would count as computability proofs if they satisfy physical realizability; quantum and biological computation research would be reframed as expanding, not challenging, the boundary. But if the mathematical-definition reading dominated instead, the constraint would persist (converted to pure convention) and the same bodies of research would remain outside — the boundary would hold but the *reason* for it would shift from epistemological to definitional.
% FOUNDING_PROBLEM: Early computability theory (Gödel, Church, Turing, 1930s) lacked a rigorous, unified notion of what 'effective procedure' or 'computable function' meant. Different formalisms (lambda calculus, Turing machines, recursive functions) appeared independently. The thesis was the claim that all these formalisms are equivalent and that their equivalence class defines the boundary of formal computability.
% FOUNDING_PROBLEM_CORROBORATION: Within proof-theoretic and metamathematical communities, the founding problem is considered solved and well-understood (Church-Turing equivalence is proved, undecidability results are rigorous). But outside this frame — in quantum computing, philosophy of mind, and physics — the status is contested: some argue the founding problem was about *nature*, not formalism, and therefore remains open. Independent corroboration from physics researchers and quantum-computing advocates: they affirm that questions about physical computability are not settled by the mathematical equivalence and remain live. No unified external voice; the boundary's persistence is affirmed by proof theorists, disputed by others.
narrative_ontology:disappearance_verdict(church_turing_thesis__epistemological_boundary_reading, contested).
narrative_ontology:founding_problem_status(church_turing_thesis__epistemological_boundary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__epistemological_boundary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(church_turing_thesis__epistemological_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__epistemological_boundary_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__epistemological_boundary_reading_tests).
:- end_tests(church_turing_thesis__epistemological_boundary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the constraint operates through definitional exclusion and legitimacy gatekeeping, not through direct coercion or resource transfer. A researcher studying hypercomputation is not prevented from publishing; they are prevented from claiming their work expands the boundary of Turing computability. Suppression is lower (0.31) because the boundary is defended through peer-review norms and category enforcement, not through active suppression of dissent — hypercomputation research proceeds, but is re-categorized. Theater is low (0.18) because the boundary serves a genuine proof-theoretic function (it does organize metamathematical work) even though it also excludes alternative research programs. The measurement series shows slight rise in extractiveness and suppression over 90 years (0.35 → 0.42 base_extractiveness, 0.20 → 0.31 suppression_requirement): as quantum computing and hypercomputation research programs matured and accumulated results, the boundary had to be more actively defended to prevent their reintegration into 'computability' discourse. Theater rises slightly (0.10 → 0.18) because defensive rhetoric has become more elaborate as challenges mounted.
 *
 * PERSPECTIVAL GAP:
 *   The power difference between institutional proof theorists (powerful, controlling publication and discourse) and individual hypercomputation researchers (moderate to powerless) generates structural asymmetry. A powerful actor like a quantum-computing company can maintain that their algorithms expand what is computable (in the physical sense) while accepting that they don't expand formal Turing computability; a powerless researcher cannot make both claims without institutional support.
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation chain runs: beneficiaries (proof theorists, constructivists) have high power and arbitrage-grade exit (they can switch to other readings if the boundary breaks); victims (non-constructivists, hypercomputation researchers) have moderate to powerless positions and constrained or identity-locked exit. The overrides are minimal: the structural derivation already captures the asymmetry correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (defining 'effective computability' rigorously) is well-solved within proof theory but contested outside it. The boundary reading's persistence depends on the institutional dominance of proof-theoretic frameworks in computer science and mathematics curricula. If the founding problem is considered DEAD (formal computability is now well-understood and stable), then the constraint's continued enforcement would mark it as either zombie (surviving from inertia) or as having shifted its function entirely to excluding alternative research programs. The engine's tangled_rope classification holds precisely because there IS a real coordination function (rigorous proof standards) AND real asymmetric extraction (exclusion of non-Turing-aligned research). The theater_ratio suggests the exclusion function is becoming more performative over time (rising ratio) as the foundational work is completed and only defense remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formalism_vs_nature_boundary,
    'Is the boundary between ''formally knowable computation'' and ''what nature permits'' a sharp, principled distinction or an epistemic choice?',
    'Examination of whether hypercomputation results (oracle machines, infinite-time TMs, physical hypercomputation) ever achieve consensus acceptance within mainstream mathematics. If hypercomputational claims permeate proof theory itself, the boundary was epistemic choice, not structural necessity. If they remain excluded by institutional gatekeeping, the boundary is maintained by power, not by logical necessity.',
    'If the boundary is revealed as an epistemic choice, the constraint should be reclassified from Tangled Rope (genuine coordination + extraction) toward Snare (extraction with coordination cover). If it is structural, the Tangled Rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formalism_vs_nature_boundary, conceptual, 'Whether the formalism-vs-nature boundary is intrinsic or maintained by institutional choice.').

omega_variable(
    non_constructive_proof_legitimacy,
    'Are non-constructive proofs of computability mathematically valid within the epistemological reading, or are they genuinely excluded from the frame?',
    'Close analysis of how constructive and classical mathematicians debate proofs that establish computability via the law of excluded middle. If classical proofs are recognized as computing the same boundary (under a different epistemic justification), the constraint is more boundary than fence. If classical proofs are consistently treated as outside the computability frame, the exclusion is real.',
    'If non-constructive proofs are legitimate alternate paths to the same boundary, extractiveness should be downgraded (0.42 → 0.25): the constraint becomes pure coordination. If they are genuinely excluded, extractiveness holds: the constraint enforces a proof method, not just a boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_constructive_proof_legitimacy, empirical, 'Whether non-constructive proofs are treated as alternative validations or as exclusions from computability discourse.').

omega_variable(
    quantum_computation_boundary_expansion,
    'If quantum computers demonstrably outcompute classical Turing machines on some functions, does that create pressure to revise the boundary of ''formally knowable computation'' to include quantum-realizable algorithms?',
    'Analysis of mathematical and foundational literature over the next 20–30 years as quantum algorithms mature. If hyperpolynomial quantum speedups become standard and mathematicians begin to argue for their inclusion in computability theory, the boundary becomes contestable. If the mathematical community insists on maintaining Turing computability as the standard, the boundary persists despite the empirical challenge.',
    'Pressure to revise the boundary would mark the constraint as increasingly theatrical (boundary defended by institutional will, not by logical necessity) — theater_ratio would rise significantly. Maintenance of the boundary would suggest it is genuinely foundational to proof-theoretic practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quantum_computation_boundary_expansion, empirical, 'Whether empirical expansion of physical computation capacity will force revision of the formal boundary.').

omega_variable(
    epistemological_reading_gatekeeping_mechanism,
    'What specific institutional and social mechanisms enforce the epistemological reading and exclude alternatives?',
    'Empirical study of peer review in computability-theory journals, hiring patterns in mathematical logic departments, which textbooks become canonical, how graduate students are taught to frame computability problems. Catalog which venues accept hypercomputation research and which exclude it.',
    'If gatekeeping is exercised explicitly (rejection of papers on ''out-of-scope'' grounds), suppression_requirement is well-measured at 0.31. If gatekeeping is more subtle (papers accepted but re-categorized, citations withheld), suppression might be higher (0.40+) than measured. If gatekeeping is minimal and alternative readings simply coexist at lower prestige levels, suppression should be downgraded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemological_reading_gatekeeping_mechanism, empirical, 'The operational enforcement mechanisms that maintain the boundary reading as dominant in mathematical practice.').

omega_variable(
    sibling_reading_foreclosure,
    'Do the epistemological-boundary reading and the physical-claim reading truly coexist, or does the institutional dominance of the boundary reading foreclose the physical reading within mathematical foundations?',
    'Check whether physicists studying quantum/analog/hypercomputation frame their results as ''beyond formal computability'' (coexistence) or whether mathematical literature treats physical computation as simply not in the domain of computability theory (foreclosure). If coexistence is stable and both readings retain research programs, the relation is coexists_with. If one reading dominates and the other is systematically excluded from ''real'' computability discourse, the relation is influences (boundary reading influences the power structure of the physical reading).',
    'Affects the reading_relations declaration in cs_structure. If the boundary reading forecloses the physical reading, the structure_relations should say ''forecloses''; if they coexist at different institutional sites, ''coexists_with''; if the boundary reading creates conditions that marginalize the physical reading without logical contradiction, ''influences''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether the epistemological reading structurally excludes the physical claim reading or merely marginalizes it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__epistemological_boundary_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t0, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(chur_tr_t15, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(chur_tr_t30, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(chur_tr_t45, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 45, 0.17).
narrative_ontology:measurement(chur_tr_t60, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(chur_tr_t90, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 90, 0.18).

% Extraction over time
narrative_ontology:measurement(chur_be_t0, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(chur_be_t15, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(chur_be_t30, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(chur_be_t45, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 45, 0.42).
narrative_ontology:measurement(chur_be_t60, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 60, 0.43).
narrative_ontology:measurement(chur_be_t90, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 90, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t0, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(chur_su_t15, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 15, 0.23).
narrative_ontology:measurement(chur_su_t30, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 30, 0.26).
narrative_ontology:measurement(chur_su_t45, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 45, 0.29).
narrative_ontology:measurement(chur_su_t60, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 60, 0.31).
narrative_ontology:measurement(chur_su_t90, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 90, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__epistemological_boundary_reading, information_standard).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__epistemological_boundary_reading, 0.05).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__physical_claim_reading).

% DUAL FORMULATION NOTE:
% The Church-Turing thesis decomposes into three structurally distinct constraints, each instantiating a different reading of the same kernel commitment. The epistemological-boundary reading defines formally knowable computation via proof-theoretic methods, creating a coordination frame for rigorous metamathematics while extracting costs from non-Turing-aligned research programs. The mathematical-definition reading treats the thesis as a neutral convention stipulating meaning. The physical-claim reading asserts the thesis as an empirical hypothesis about nature's computational limits. These readings have different ε values (low coordination cost for definition reading, moderate extraction for boundary reading, high contestation for physical reading), different victim sets, and different stakeholder structures. They are linked through the network because the boundary reading's institutional dominance constrains how the other readings can be adopted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
