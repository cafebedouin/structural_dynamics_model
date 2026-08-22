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
 *   human_readable: Church-Turing Thesis (Physical Claim Reading)
 *   domain: philosophy_of_mathematics/foundations_of_computation
 *
 * SUMMARY:
 *   The Church-Turing thesis claims that no physical process can compute
 *   functions beyond Turing-machine computability. This reading treats the
 *   thesis as an empirical statement about the laws of physics, subject in
 *   principle to falsification by the discovery of hypercomputation or
 *   quantum processes exceeding Turing bounds. Under this reading, the
 *   classical computation establishment benefits from the thesis's
 *   enforcement as settled truth: it closes off research into alternative
 *   computational paradigms and legitimates their own framework as the
 *   universal model. Hypercomputation researchers and quantum supremacy
 *   claimants bear the cost: their work faces systematic suppression
 *   justified by appeal to the thesis as proven physics. The constraint is
 *   tangled rope because it coordinates genuine unification (all branches of
 *   computation theory use the same model) while simultaneously extracting
 *   legitimacy and resources from alternative research programs. This reading
 *   is distinct from the mathematical-definition reading (which treats
 *   Church-Turing as a true-by-stipulation definition) and the
 *   epistemological-boundary reading (which treats it as marking the limit of
 *   formally provable computability, independent of physical realizability).
 *   The reading-specific ε is moderate (0.58): the empirical claim is
 *   contestable by future physics, but institutional enforcement is real and
 *   rising.
 *
 * KEY AGENTS:
 *   - classical_computation_establishment: institutional beneficiary with agenda-setter power — enforces the thesis as settled physics
 *   - hypercomputation_researchers: constrained-exit victims bearing suppression justified by the thesis
 *   - quantum_supremacy_claimants: powerful institutions caught between claiming quantum advantage and the constraint's denial of non-Turing physical processes
 *   - funding_agencies: institutional agenda-setters allocating resources based on the thesis's status
 *   - physics_community: institutional beneficiaries who gain simplification by treating CT as physical law
 *   - mathematical_platonists: excluded from dominant discourse; their definitional reading is sidelined by the physical interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, 0.58).
domain_priors:suppression_score(church_turing_thesis__physical_claim_reading, 0.67).
domain_priors:theater_ratio(church_turing_thesis__physical_claim_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__physical_claim_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__physical_claim_reading, "Church-Turing Thesis (Physical Claim Reading)").
narrative_ontology:topic_domain(church_turing_thesis__physical_claim_reading, "philosophy_of_mathematics/foundations_of_computation").

domain_priors:requires_active_enforcement(church_turing_thesis__physical_claim_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__physical_claim_reading, 'ce52acbc-bfb5-4fde-bdaa-33a8a6a2e6d3').
narrative_ontology:cs_kernel_codification('ce52acbc-bfb5-4fde-bdaa-33a8a6a2e6d3', fixed_text).
narrative_ontology:cs_authority_grounding('ce52acbc-bfb5-4fde-bdaa-33a8a6a2e6d3', extraction).
narrative_ontology:cs_interpretation_layer_present('ce52acbc-bfb5-4fde-bdaa-33a8a6a2e6d3').
narrative_ontology:cs_reading_relation('ce52acbc-bfb5-4fde-bdaa-33a8a6a2e6d3', church_turing_thesis__mathematical_definition_reading, forecloses).
narrative_ontology:cs_reading_relation('ce52acbc-bfb5-4fde-bdaa-33a8a6a2e6d3', church_turing_thesis__epistemological_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('ce52acbc-bfb5-4fde-bdaa-33a8a6a2e6d3', foundational, mathematical_equivalence_entails_physical_universality).
narrative_ontology:cs_axiom_status(mathematical_equivalence_entails_physical_universality, holdable).
narrative_ontology:cs_axiom_grounding('ce52acbc-bfb5-4fde-bdaa-33a8a6a2e6d3', mathematical_equivalence_entails_physical_universality, empirically_contingent).
narrative_ontology:cs_axiom('ce52acbc-bfb5-4fde-bdaa-33a8a6a2e6d3', foundational, closure_of_computational_space).
narrative_ontology:cs_axiom_status(closure_of_computational_space, holdable).
narrative_ontology:cs_axiom_grounding('ce52acbc-bfb5-4fde-bdaa-33a8a6a2e6d3', closure_of_computational_space, empirically_contingent).
narrative_ontology:cs_reference_frame('ce52acbc-bfb5-4fde-bdaa-33a8a6a2e6d3', mathematical_equivalence_as_physical_law).
narrative_ontology:cs_drift_state('ce52acbc-bfb5-4fde-bdaa-33a8a6a2e6d3', contemporary_quantum_and_hypercomputation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ce52acbc-bfb5-4fde-bdaa-33a8a6a2e6d3', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__physical_claim_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, classical_computation_establishment).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, hypercomputation_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, quantum_supremacy_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, quantum_supremacy_claimants).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, physics_community).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, funding_agencies).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, computational_complexity_theorists).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, quantum_information_theorists).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, analog_computation_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the research agenda by treating Church-Turing as settled empirical truth. Controls peer review in computation theory, algorithms, and complexity theory. Publishes papers affirming the thesis, rejects papers claiming hypercomputation, and frames alternative models as disproven. Justifies this gatekeeping as enforcement of scientific standards, not institutional capture. Collects legitimacy and research dominance from the constraint's enforcement.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, classical_computation_establishment, agenda_setter,
    institutional, generational, arbitrage, global).

% Propose and develop formal models of hypercomputation (oracle machines, analog computers, physical systems with infinite precision or continuous time). Face systematic publication and funding friction: papers are rejected on grounds that hypercomputation is 'settled to be impossible by Church-Turing'; grant proposals are denied because the thesis is treated as proven physics. Can work outside mainstream academia (independent research, non-academic journals, international programs) but at career cost. Their exit options are constrained because institutional prestige and employment depend on mainstream publication and funding.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, hypercomputation_researchers, payer,
    moderate, biographical, constrained, global).

% Claim quantum computers compute functions faster than classical ones (Shor's factorization, Grover's search, sampling problems). Must frame quantum advantage within Turing computability: the functions quantum algorithms compute are Turing-computable, just faster. This framing protects them from the thesis but constrains their claims. If they asserted quantum computation exceeds Turing bounds, the constraint would suppress them. They occupy a tense middle position: they believe they have discovered genuine computational advantage, but the thesis forces them to describe it as 'speedup within Turing bounds,' which some physicists read as 'no real computational novelty.'
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, quantum_supremacy_claimants, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__physical_claim_reading, quantum_supremacy_claimants, beneficiary).

% Gains research simplification from treating Church-Turing as physical law. It constrains the design space of experiments and theories: no need to test whether nature permits hypercomputation, because the thesis (read as physics) rules it out logically. Some physicists take the thesis as background fact; others see it as an empirical question to be resolved by experiment. Most are indifferent to the thesis's philosophical status and treat it as a useful simplifying assumption.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, physics_community, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__physical_claim_reading, physics_community, observer).

% Allocate research funds based on the thesis's status. When treated as settled (CT = true physics), hypercomputation gets minimal support; when treated as open, it attracts exploratory funding. Agencies benefit from the thesis because it simplifies prioritization and allows dismissal of alternative-computation proposals as 'based on disproven ideas.' They have the power to reclassify funding if they chose to treat Church-Turing as an open empirical question.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, funding_agencies, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__physical_claim_reading, funding_agencies, beneficiary).

% Advocate that Church-Turing is a mathematical definition of 'effective computability,' true by stipulation in the abstract realm of mathematics. Want the thesis debated on foundational grounds (what makes something computable in principle?), not on physics. The physical reading sidelines this debate by treating Church-Turing as settled empirical fact. They can publish in philosophy and foundational mathematics journals but are excluded from dominant discourse in computer science, physics, and funding decisions. Their exclusion is structural: the physical reading's dominance makes the definitional debate seem moot.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, mathematical_platonists, excluded,
    moderate, generational, constrained, global).

% Build theory (P vs. NP, algorithm analysis, computability) on the assumption that all physically realizable algorithms are Turing machines. The thesis anchors their field. They benefit because it keeps the foundation of their discipline unchallenged. If the thesis failed (quantum or hypercomputation exceeded Turing bounds), their proofs about the limits of Turing computation would become less authoritative. They have minimal institutional suppression because their work aligns with the thesis.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, computational_complexity_theorists, beneficiary,
    institutional, biographical, mobile, global).

% Study what quantum mechanics can compute. The thesis constrains their work: any claimed quantum advantage must stay within Turing bounds or face the charge of violating settled physics. Some theorists accept this constraint as justified; others view it as an empirical question still open. They publish freely but must frame results within the Turing framework, which shapes their conceptual vocabulary and research questions.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, quantum_information_theorists, observer,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__physical_claim_reading, quantum_information_theorists, payer).

% Propose continuous, differential-equation-based computation models (abstract analog computers, optical computing, neuromorphic systems). The thesis, if true physically, says these cannot exceed Turing power. They face publication and funding friction: the thesis is invoked to dismiss analog models as 'theoretically impossible in principle,' not just practically difficult. They can work in engineering and interdisciplinary journals but face gatekeeping in theoretical computer science.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, analog_computation_researchers, payer,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__physical_claim_reading, classical_computation_establishment).
narrative_ontology:fixing_cost_class(church_turing_thesis__physical_claim_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified framework for talking about 'effective computability' across mathematics, theoretical computer science, physics, and philosophy. All domains agree on a single model (Turing machines and equivalent formalisms), enabling theorems from one field to apply to others and researchers from different backgrounds to collaborate on well-defined problems.
% TRANSFER_FUNCTION: Transfers research legitimacy and resources from hypercomputation, analog computation, and non-Turing-bounded models toward classical computation and Turing-based theory. Researchers proposing alternatives lose publication venues, funding, and institutional prestige; their work is labeled pseudoscience or disproven. The classical computation establishment collects the legitimacy and monopoly on what counts as 'serious' computational research.
% ABSENT_VOICES: Mathematical platonists and intuitionist logicians who argue the thesis is a definition, not empirical fact, are excluded from dominant research institutions and funding decisions. Hypercomputation researchers who claim the thesis is still open have voices in specialized journals but are systematically devalued in mainstream CS and physics. Philosophers of mathematics questioning the thesis's empirical status are rarely heard in how computational research is actually funded and evaluated. Their absence is structural: the physical reading marginalizes their foundational objections.
% DISAPPEARANCE_RATIONALE: If the thesis as physical law vanished overnight (accepted as falsifiable or reframed as merely mathematical), the research landscape in computer science and physics would reorganize. Hypercomputation research would immediately attract serious funding and publication; quantum supremacy would be discussed without the constraint of staying within Turing bounds; analog and unconventional computation models would be pursued with institutional support. Funding agencies would reweight priorities. The coordination function (unified definition) might persist, but the extraction function (suppression of alternatives) would cease.
% FOUNDING_PROBLEM: Turing, Church, Gödel, and Post each independently formalized 'effective computability' using different methods (Turing machines, lambda calculus, recursive functions, canonical systems). The founding problem was: Are these definitions equivalent? Is there a unique, universal notion of what we mean by a computable function? This problem was motivated by foundational questions in logic and mathematics about what can be proven and computed.
% FOUNDING_PROBLEM_CORROBORATION: The mathematical equivalence of Turing, Church, Gödel, and Post models is proven and accepted across all domains — the founding problem of definition-matching is solved. However, the interpretation of this mathematical result as an empirical claim about physics is contested. Turing himself wrote that the thesis was an assumption or hypothesis, not a proven fact. Hypercomputation researchers (Ord, Shagrir, Copeland) and quantum information theorists (Deutsch) attest from outside the classical establishment that the physical reading remains open. Computer scientists in the classical establishment and most physicists treat the thesis as settled. The corroboration is divided: mathematical elegance and institutional convenience support the physical reading, but empirical physics and foundational philosophy do not provide external corroboration independent of the thesis-using community itself.
narrative_ontology:disappearance_verdict(church_turing_thesis__physical_claim_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__physical_claim_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__physical_claim_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.15 (1936, when the thesis was Turing's explicit hypothesis) to 0.58 (2026, when it dominates institutional practice). The rise reflects institutional consolidation: early on, the thesis was debated as an open question; by the 1970s–80s, computer science and physics increasingly treated it as settled empirical fact; by 2005+, it became background assumption in funding and publication decisions. Suppression follows a similar arc but steeper (0.18 → 0.67), because active enforcement machinery built up over time: journal gatekeeping, funding review processes, and institutional dismissal of hypercomputation as pseudoscience. Theater (0.42 currently) reflects the fact that public justification invokes coordination benefits ('unified model of computation') and physical necessity ('no physics known to violate CT'), but institutional practice prioritizes closure (preventing alternative models from getting resources). The measurement grid uses one shared time axis so that all three metrics are measured at each point, enabling coherent temporal analysis. The basis 'observed' indicates historical records of publication trends, funding patterns, and discourse shifts documented in philosophy of computing and history of CS literature.
 *
 * PERSPECTIVAL GAP:
 *   From the classical-computation establishment seat, the thesis appears as a unified, proven, boundary-setting principle that enables better theory and research. They see coordination and progress. From the hypercomputation-researcher seat, the thesis appears as institutional gatekeeping justified by invocation of settled physics that is actually still open empirically. They see suppression of alternative inquiry. From the physics seat, the thesis is background — a simplifying principle that constrains what experiments need to be designed to rule out. No perspective is wrong; they are reading the same constraint from seats with different structural positions relative to its enforcement. The engine computes this divergence: an institutional beneficiary and a suppressed researcher will generate different type classifications from the same structural data, which is exactly the measurement the framework is designed to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The classical_computation_establishment sits at the beneficiary end (d ≈ 0.1): they set the constraint's terms and collect institutional legitimacy and resources from its enforcement. Hypercomputation_researchers sit at the target end (d ≈ 0.9): their work is suppressed by appeal to the thesis, they face funding denial, and their exit options are constrained (they can work outside mainstream academia, but that is costly). Quantum_supremacy_claimants sit near the target end (d ≈ 0.75) because they must frame quantum advantage within Turing bounds or face the charge of violating settled physics, even though quantum mechanics may in principle allow hypercomputational processes. Physics_community_beneficiaries and complexity_theorists sit near beneficiary (d ≈ 0.2–0.3) because the constraint simplifies their research frameworks without active suppression of their work. The directionality divergence is structural: an institutional actor (classical computation) benefits from closure against non-Turing models; individual researchers in alternative models bear the cost. No override is needed; the structural data (beneficiary/victim, power, exit options) generates the correct d values via derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (defining 'effective computability' uniformly across Turing, Church, Gödel, Post) was solved: the classical models are mathematically equivalent. The mandatrophy question is: is this mathematical equivalence an empirical claim about physics, or merely a definitional achievement? The physical reading assumes the equivalence IS empirical — that because all formalizations converge on one model, no physical process can compute differently. But the founding problem itself was about mathematical definitions, not physics. If the founding problem is read as solved (we have a unified definition), the constraint's continued enforcement as physics law is mandatrophic: the founding problem's solution does not entail the physical claim. Institutional suppression of hypercomputation now persists because the thesis has become background assumption, not because the founding problem demands it. The constraint is therefore partly mandatrophic: the coordination function (unified definition) is solved; the extraction function (suppression of non-Turing research) persists because the institutional establishment treats the definitional result as physical law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_vs_definitional_status,
    'Is Church-Turing an empirical claim about physics, a mathematical definition true by stipulation, or an epistemological boundary marking what can be formally proven?',
    'Physics discovers a physical process that computes beyond Turing bounds (oracle access, infinite-precision analog computation, hypercomputation in curved spacetime), OR consensus shifts in philosophy of mathematics / computer science about the thesis''s nature (e.g., Gödel''s view that it is a definite mathematical claim becomes dominant), OR formal work proves all proposed non-Turing models collapse to Turing equivalence.',
    'If the thesis is merely definitional, the constraint''s enforcement as empirical physics is unwarranted and the suppression of hypercomputation is mandatrophic. The constraint would reclassify from tangled rope to piton (institutional inertia maintaining a defunct claim). If an empirical counterexample emerges, the constraint''s type inverts: what is now suppression becomes protection against false physics. If consensus shifts to the epistemological reading, enforcement grounds shift but type may stabilize.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_vs_definitional_status, empirical, 'The metaphysical and epistemological status of Church-Turing remains contested despite mathematical equivalence proofs.').

omega_variable(
    quantum_supremacy_boundary,
    'Can quantum computation exceed Turing computability in principle, or are all quantum speedups reducible to faster Turing simulation?',
    'Rigorous proof that quantum algorithms (Shor, Grover, quantum simulation) stay within Turing bounds despite polynomial speedup, OR discovery of a physical quantum process that provably computes a function no Turing machine can (e.g., non-recursive function, true oracle access), OR consensus among quantum information theorists on what counts as ''quantum advantage''.',
    'If quantum supremacy claims are proven to respect Turing bounds, the constraint''s suppression of hypercomputation is vindicated and extractiveness may decrease (the threat is contained). If quantum processes exceed Turing bounds, the constraint is empirically false and the victims (quantum researchers) would be absolved of suppression, reclassifying the structure. If the question remains open, the constraint persists as contested, maintaining suppression justified by unresolved physics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_supremacy_boundary, empirical, 'Whether quantum mechanics permits computation beyond the Turing limit remains theoretically open despite recent quantum-supremacy demonstrations.').

omega_variable(
    institutional_capture_of_thesis,
    'Is the thesis''s enforcement a response to its truth value, or a response to institutional convenience (classical computation''s institutional dominance)?',
    'Historical and sociological analysis of funding flows, publication decisions, and academic hiring; comparison with domains where competing paradigms retained institutional support despite settling on a dominant model; interviews with researchers in hypercomputation, analog computation, and quantum information about barriers they face.',
    'If institutional capture is primary, the constraint is a snare (extraction justified by false neutrality), not rope (coordination justified by genuine unification). Therapy would require institutional restructuring to permit alternative paradigms equal research standing. If truth-tracking is primary, enforcement is justified and the victims are pursuing impossible research. The omega is resolvable but only through detailed institutional history, not pure mathematics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_of_thesis, empirical, 'Whether the dominance of Church-Turing in institutional practice reflects its truth or institutional path-dependence.').

omega_variable(
    false_summit_candidate,
    'Does the universal agreement on the mathematical equivalence of Turing, Church, Gödel, and Post models mask beneficiaries who profit from treating the equivalence as physics?',
    'Analyze who gains resources, legitimacy, or research dominance from the physical reading: do classical-computation researchers and funding agencies benefit from closing off hypercomputation research? Is the ''universal agreement'' on mathematics used as cover for institutional gatekeeping against alternatives?',
    'If beneficiaries exist (classical-computation establishment, certain funding agencies, complexity theorists), the constraint may be a false summit: appearing as natural law (universal mathematical equivalence) while actually extracting resources for an established research program. This would support reclassification as tangled rope or snare, depending on how much coordination vs. extraction is present.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_candidate, conceptual, 'Whether Church-Turing appears as natural law but actually benefits identifiable institutional actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__physical_claim_reading, 1936, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__physical_claim_reading, theater_ratio, 1936, 0.08).
narrative_ontology:measurement(chur_tr_t1960, church_turing_thesis__physical_claim_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(chur_tr_t1985, church_turing_thesis__physical_claim_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement(chur_tr_t2005, church_turing_thesis__physical_claim_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(chur_tr_t2015, church_turing_thesis__physical_claim_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(chur_tr_t2026, church_turing_thesis__physical_claim_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__physical_claim_reading, base_extractiveness, 1936, 0.15).
narrative_ontology:measurement(chur_be_t1960, church_turing_thesis__physical_claim_reading, base_extractiveness, 1960, 0.28).
narrative_ontology:measurement(chur_be_t1985, church_turing_thesis__physical_claim_reading, base_extractiveness, 1985, 0.42).
narrative_ontology:measurement(chur_be_t2005, church_turing_thesis__physical_claim_reading, base_extractiveness, 2005, 0.54).
narrative_ontology:measurement(chur_be_t2015, church_turing_thesis__physical_claim_reading, base_extractiveness, 2015, 0.56).
narrative_ontology:measurement(chur_be_t2026, church_turing_thesis__physical_claim_reading, base_extractiveness, 2026, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__physical_claim_reading, suppression_requirement, 1936, 0.18).
narrative_ontology:measurement(chur_su_t1960, church_turing_thesis__physical_claim_reading, suppression_requirement, 1960, 0.35).
narrative_ontology:measurement(chur_su_t1985, church_turing_thesis__physical_claim_reading, suppression_requirement, 1985, 0.52).
narrative_ontology:measurement(chur_su_t2005, church_turing_thesis__physical_claim_reading, suppression_requirement, 2005, 0.63).
narrative_ontology:measurement(chur_su_t2015, church_turing_thesis__physical_claim_reading, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(chur_su_t2026, church_turing_thesis__physical_claim_reading, suppression_requirement, 2026, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__physical_claim_reading, information_standard).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__physical_claim_reading, 0.06).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% The Church-Turing thesis decomposes into three structurally distinct constraints corresponding to three coherent readings: (1) PHYSICAL_CLAIM_READING (this file): treats CT as empirical physics, ε moderate (0.58), type tangled rope, suppresses hypercomputation research; (2) MATHEMATICAL_DEFINITION_READING: treats CT as a true-by-stipulation definition, ε near zero (coordination with no extraction), type rope; (3) EPISTEMOLOGICAL_BOUNDARY_READING: treats CT as marking the limit of formal provability, ε low-moderate (permits hypercomputation as open empirical question), type rope. The readings share the same kernel (unified effective computability) but produce different constraint types and different beneficiary/victim structures. The physical reading is the only one that actively suppresses alternative research; the other readings treat CT as settled mathematically while remaining agnostic on physics. Each reading is a complete constraint with its own ε, stakeholders, and type; they are linked by network.affects_constraints because the physical reading's dominance institutionally 'crowds out' the alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
