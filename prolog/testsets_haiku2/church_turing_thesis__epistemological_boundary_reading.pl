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
 *   domain: philosophy_of_mathematics/foundations_of_computation
 *
 * SUMMARY:
 *   The Church-Turing thesis, in this epistemological reading, asserts that
 *   the class of functions provably computable in formal systems is exactly
 *   the class of Turing-computable functions — it marks a boundary between
 *   what we can formally know to be computable and what lies outside that
 *   boundary. This reading treats the thesis as a methodological gate: it
 *   defines what counts as a valid computability proof, not as a claim about
 *   the physical universe or as a pure mathematical definition. The
 *   constraint operates because the thesis has become embedded in curriculum,
 *   hiring, and publication norms such that research pursuing alternative
 *   models of computation (hypercomputation, analog computation, quantum
 *   advantage) must either align with Church-Turing or bear the burden of
 *   arguing outside 'formally knowable computation.' The reading is contested
 *   by the mathematical-definition reading (which treats the thesis as true
 *   by convention, not by epistemic necessity) and the physical reading
 *   (which makes empirical claims about what nature can compute). This story
 *   focuses on the epistemological reading's operation as a boundary-setting
 *   constraint within mathematics and computer science.
 *
 * KEY AGENTS:
 *   - proof_theory_methodology: Institutional beneficiary — the thesis stabilizes proof-theoretic methodology and makes it reproducible across researchers
 *   - computability_textbook_canon: Institutional beneficiary — the thesis crystallizes into pedagogical and research canon, making it the default framework taught and applied
 *   - hypercomputation_research_programs: Powerless victim — facing systematic exclusion from mainstream legitimacy because the thesis defines their work as outside 'formally knowable computation'
 *   - quantum_computational_advantage_claims: Powerful victim with mobility — operate within Church-Turing but are constrained when the thesis is deployed to dismiss quantum advantage as 'not really more computable'
 *   - non_constructive_computability_claims: Moderate victim — intuitionistic, constructive, and unconventional models bear the burden of reconciling with or arguing against the thesis
 *   - mathematical_logicians: Institutional agenda-setter — administer the thesis through textbooks, journals, and professional standards; could reformulate but face inertial pressure
 *   - constructive_mathematicians: Excluded voice — would contest the thesis's conflation of proof-theoretic computability with mathematical knowability but are absent from canonical discourse
 *   - empirical_physics_community: Observer seat — tests the thesis's physical reading against empirical data; retains authority to contest at boundaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__epistemological_boundary_reading, 0.38).
domain_priors:suppression_score(church_turing_thesis__epistemological_boundary_reading, 0.52).
domain_priors:theater_ratio(church_turing_thesis__epistemological_boundary_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__epistemological_boundary_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__epistemological_boundary_reading, "Church-Turing Thesis as Epistemological Boundary (Proof-Theoretic Reading)").
narrative_ontology:topic_domain(church_turing_thesis__epistemological_boundary_reading, "philosophy_of_mathematics/foundations_of_computation").

domain_priors:requires_active_enforcement(church_turing_thesis__epistemological_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__epistemological_boundary_reading, '710cbfb2-11bf-48b3-a9e6-5569a01cc5a3').
narrative_ontology:cs_kernel_codification('710cbfb2-11bf-48b3-a9e6-5569a01cc5a3', formalized).
narrative_ontology:cs_authority_grounding('710cbfb2-11bf-48b3-a9e6-5569a01cc5a3', extraction).
narrative_ontology:cs_interpretation_layer_present('710cbfb2-11bf-48b3-a9e6-5569a01cc5a3').
narrative_ontology:cs_reading_relation('710cbfb2-11bf-48b3-a9e6-5569a01cc5a3', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('710cbfb2-11bf-48b3-a9e6-5569a01cc5a3', church_turing_thesis__physical_claim_reading, influences).
narrative_ontology:cs_axiom('710cbfb2-11bf-48b3-a9e6-5569a01cc5a3', foundational, formal_proof_exhausts_computability).
narrative_ontology:cs_axiom_status(formal_proof_exhausts_computability, holdable).
narrative_ontology:cs_axiom_grounding('710cbfb2-11bf-48b3-a9e6-5569a01cc5a3', formal_proof_exhausts_computability, conventional).
narrative_ontology:cs_axiom('710cbfb2-11bf-48b3-a9e6-5569a01cc5a3', secondary, turing_machine_adequacy_for_proof_systems).
narrative_ontology:cs_axiom_status(turing_machine_adequacy_for_proof_systems, holdable).
narrative_ontology:cs_axiom_grounding('710cbfb2-11bf-48b3-a9e6-5569a01cc5a3', turing_machine_adequacy_for_proof_systems, empirically_contingent).
narrative_ontology:cs_reference_frame('710cbfb2-11bf-48b3-a9e6-5569a01cc5a3', proof_theoretic_computability_equivalence).
narrative_ontology:cs_drift_state('710cbfb2-11bf-48b3-a9e6-5569a01cc5a3', quantum_and_analog_emergence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('710cbfb2-11bf-48b3-a9e6-5569a01cc5a3', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, proof_theory_methodology).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, computability_textbook_canon).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_claims).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, hypercomputation_research_programs).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, quantum_computational_advantage_claims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, quantum_computational_advantage_claims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The thesis operationalizes proof-theoretic work by providing a canonical boundary for what counts as computable. Researchers in mathematical logic, recursion theory, and computability theory use the thesis as the baseline against which all other claims are measured. It enables reproducible, comparable results across research groups and institutions. The methodology benefits from the thesis's stability — changing the definition would require renegotiating the entire field's foundations.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, proof_theory_methodology, beneficiary,
    institutional, generational, arbitrage, global).

% The thesis is embedded in canonical textbooks (Sipser, Hopcroft & Ullman, Rogers, Odifreddi) that define the graduate-level curriculum in computability theory, complexity theory, and algorithmic foundations. Students learn the thesis as a settled fact. Degree programs, professional certifications, and hiring rubrics reference the thesis and its textbook formulations. Changing the canon would require coordinated curricular revision across hundreds of institutions — the institutional cost is prohibitive.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, computability_textbook_canon, beneficiary,
    institutional, generational, constrained, global).

% Researchers working with intuitionistic logic, constructive mathematics, or domain-specific computational frameworks face the burden of reconciling their work with the canonical thesis. They must either (a) show that their model is subsumed within Turing-computability, (b) argue that they are studying a different concept of 'computability' and accept its non-canonical status, or (c) directly challenge the thesis and accept the legitimacy cost. The thesis constrains their research framing and publication options.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_claims, payer,
    moderate, biographical, constrained, global).

% Hypercomputation researchers (Copeland, Shagrir, Etesi, others pursuing super-Turing computation) operate at the periphery of legitimate computer science and mathematical logic. The thesis is deployed against them: their work is dismissed as 'not really computable' or 'not formally provable' because it falls outside the Church-Turing boundary. They face barriers to publication in mainstream journals, difficulty securing funding, and professional marginalization. Many hypercomputation researchers identify their work as their core professional project, making exit costly in identity terms. The thesis directly excludes their research program from the definition of 'formally knowable computation.'
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, hypercomputation_research_programs, payer,
    powerless, biographical, identity_locked, global).

% Quantum computing researchers (IBM, Google, others) occupy a complex structural position. They benefit from the thesis's definition of 'computable' — it provides the baseline against which quantum advantage is measured. However, they are constrained by skeptics who deploy the thesis to argue that quantum computers cannot compute functions beyond Turing-computable ones, only implement Turing-computable functions faster. When quantum advantage claims are dismissed as 'just polynomial speedup, still within BQP ⊂ PSPACE,' the thesis is the gatekeeping mechanism. Quantum researchers have high exit options (can pivot to quantum simulation, optimization, or other applications) and powerful institutional backing (major corporate funding), so they are less suppressed than hypercomputation researchers but still face constraint.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, quantum_computational_advantage_claims, payer,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__epistemological_boundary_reading, quantum_computational_advantage_claims, beneficiary).

% Constructive mathematics (intuitionism, finitism, Bishop's constructivism) would argue that the thesis conflates proof-theoretic Turing-computability with existence and knowability, erasing a distinction they hold as foundational. A constructivist would say: 'A function is constructively computable if we have an algorithm to compute it; a function is Turing-computable if it satisfies the Church-Turing thesis's definition. These are not the same. The thesis imposes a classical framework on constructive mathematics.' Constructivists are present in mathematics (Bishop schools, intuitionist logic communities) but absent from canonical computability discourse. Their objections are rarely engaged in mainstream computer science curricula.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, constructive_mathematicians, excluded,
    moderate, generational, constrained, global).

% Researchers studying continuous computation (differential analyzers, analog circuits, neural networks, dynamical systems) cannot represent their work in the Turing-machine framework without discretizing and potentially losing essential continuous properties. They would advocate for a broader definition of computation that includes continuous models. However, they are systematically absent from computability theory textbooks and courses. The thesis forecloses their voice: if all computation is discrete Turing-computation, then continuous models are not 'really' computing in the canonical sense.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, analog_computation_theorists, excluded,
    powerless, biographical, trapped, global).

% Mathematical logicians and proof theorists set the agenda for what the thesis means and how it is applied. They write the textbooks, referee the papers, control the journals, and set the hiring standards. They administer the thesis's scope: is it a proof-theoretic boundary, a claim about all mathematics, a physical claim? They have high institutional power and can reshape the constraint, but they face strong inertial pressure to maintain the canonical framing. Changing the thesis would require them to acknowledge that decades of textbooks and curricula over-claimed its scope — a legitimacy cost they prefer to avoid.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, mathematical_logicians, agenda_setter,
    institutional, generational, arbitrage, global).

% Physicists study what the universe actually does. They operate semi-independently of the thesis's proof-theoretic reading: quantum mechanics, relativity, and cosmology are developed and tested on their own merits. However, the thesis affects physics interpretively: physicists debate whether quantum mechanics violates Church-Turing, whether relativistic computation is possible, whether the universe is a giant Turing machine (digital physics). Physicists can contest the thesis's physical reading and do empirical work to test it, giving them high authority at this boundary. They are not trapped by the thesis because their primary authority comes from empirical observation and theory, not from canonical proof-theoretic definitions.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, empirical_physics_community, observer,
    powerful, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__epistemological_boundary_reading, computability_textbook_canon).
narrative_ontology:fixing_cost_class(church_turing_thesis__epistemological_boundary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, formal criterion for what counts as 'computable' in proofs and theorems: researchers working in computability theory, complexity theory, and logic can align on a single boundary, making results comparable and debates tractable. Without this common referent, each researcher could stipulate their own notion of computability, fragmenting the field.
% TRANSFER_FUNCTION: Moves authority from individual research programs' self-defined computability models into the canonical Turing-machine framework. Researchers pursuing non-Turing models (hypercomputation, analog computation, quantum advantage) must bear the burden of proof and legitimacy cost; canonical researchers inherit the thesis's legitimacy without having to defend it. The thesis transfers credibility asymmetrically: toward the center (Turing framework) and away from the periphery (alternative models).
% ABSENT_VOICES: Constructivist mathematicians, analog computation theorists, and hypercomputation researchers would contest the thesis's claim that proof-theoretic Turing-computability is coextensive with mathematical knowability. They argue the thesis illicitly restricts the epistemology of computation by excluding non-classical logics, continuous models, and unconventional physical substrates. Their absence from canonical textbooks and theorem-proving institutions means their objections are not systematically addressed in the field's pedagogy.
% DISAPPEARANCE_RATIONALE: If the thesis vanished (i.e., if the field abandoned Church-Turing as the canonical boundary), the immediate world would not reorganize — the Turing machine would remain a historically important model and a useful reference point. However, research trajectories would shift: hypercomputation programs would gain legitimacy, quantum advantage claims would stop being contested by the thesis, and constructive mathematics would no longer need to frame itself as a minority position. The field's institutional structure (hiring, funding, canonical curricula) would need to pluralize what counts as 'knowably computable.' The deeper question — whether the thesis *must* disappear or can be reframed as one boundary among several — divides the stakeholders.
% FOUNDING_PROBLEM: In the 1930s, the recursion theorists (Church, Gödel, Turing, Post) faced a problem: 'effective computability' was used intuitively in metamathematics and logic, but the term lacked a formal definition. Different researchers were using it differently. The thesis (Church's conjecture, Turing's formulation) proposed that the intuitive notion matches the formal class of Turing-computable functions, providing a common language.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (need for a formal boundary on 'effective computability') was real in 1936 — Gödel and others document the definitional ambiguity. Church and Turing's solution (equating intuitive with formal) solved the immediate problem within proof theory and enabled Gödel's incompleteness proofs and Turing's halting theorem to be stated precisely. However, by the 1980s, the problem had shifted: physical processes (quantum, analog, optical) began raising the question of whether the thesis claims something about physics or only about formal proof-theoretic boundaries. Non-canonical researchers (hypercomputation, constructive mathematics) attest the founding problem is solved narrowly (for proof-theoretic recursion) but that the thesis's scope has been overextended to claim authority over all computability concepts. Mainstream logicians attest the founding problem remains live (definitional clarity is still needed), which justifies the thesis's continued canonical status.
narrative_ontology:disappearance_verdict(church_turing_thesis__epistemological_boundary_reading, contested).
narrative_ontology:founding_problem_status(church_turing_thesis__epistemological_boundary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__epistemological_boundary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(church_turing_thesis__epistemological_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__epistemological_boundary_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.38): the thesis does extract legitimacy and credibility from alternative models and concentrates it toward the canonical Turing framework. However, the extraction is not pure — it does solve a genuine coordination problem (common language for computability proofs). Over the interval from 1936 to 2024, extractiveness rose from ~0.15 (when the thesis was newly proposed and still contested) to 0.38 (where it has stabilized as institutionalized canon). The rise reflects the thesis becoming embedded in curriculum and hiring. Suppression is moderately high (0.52): the constraint persists partly because the thesis is actively reinforced in textbooks and professional standards, but also because researchers in non-Turing models face real barriers (funding, publication, legitimacy). The barrier is not primarily external coercion but rather the internalization of the thesis's authority. Theater ratio is modest (0.22): the security/coordination function is real (proof-theoretic researchers genuinely benefit from a common boundary), but a growing share of the constraint's enforcement activity defends the thesis's scope beyond where the proof-theoretic case is strongest (e.g., applying it to physical systems or dismissing hypercomputation without engagement with the technical arguments). Accessibility collapse is high (0.71): once researchers understand the thesis's canonical status, alternatives seem closed off — you can work within Turing-computability, or you can step outside and lose legitimacy. Resistance is moderate (0.48): some researchers actively contest the thesis's scope (especially in quantum computing and hypercomputation), but they face suppression (publication barriers, funding scarcity) and internalized doubt (the thesis's long institutional history makes challenging it seem audacious).
 *
 * PERSPECTIVAL GAP:
 *   This reading's operation as a constraint is most visible from the powerless and identity-locked research programs (hypercomputation). From their perspective, the thesis is an enforcement mechanism that uses mathematical prestige to foreclose legitimate inquiry. From the agenda-setter's perspective, the thesis is a tool for maintaining rigor and coherence in the field. The quantum computing community occupies an intermediate position: they benefit from the thesis's legitimacy (it provides the standard definition of 'computable' that their work must relate to) and are harmed by its extension (the thesis is used to argue that quantum advantage is 'not really computational,' just faster implementation of Turing-computable functions). This perspectival divergence is where the tangled-rope classification emerges: the same constraint provides genuine coordination for the center and real extraction for the periphery.
 *
 * DIRECTIONALITY LOGIC:
 *   Proof-theory-methodology and computability-textbook-canon are beneficiaries (d near 0.0): they collect the thesis's benefits directly — stability, pedagogical authority, institutional embedding. Mathematical logicians as agenda-setters are near-beneficiaries (d ~0.1-0.2): they administer the constraint and benefit from its stability but do not extract rents in the economic sense; their power comes from the constraint's existence, not from personal gain. Hypercomputation researchers are victims (d near 1.0): their identity is fused with their research program (identity_locked exit), they bear the cost of exclusion and delegitimation, and they have very low power (powerless) and constrained exit. Quantum researchers are partial victims (d ~0.7): they are powerful and have mobile exit options, so their d is lower than for powerless victims, but they still bear suppression when the thesis is deployed against quantum advantage claims. Constructive mathematicians and analog theorists are excluded but would be high-d victims (identity_locked, trapped exit) if they were seated. No directionality overrides are needed; the structural data (power, exit, beneficiary/victim roles) derives the directionality naturally.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (definitional clarity on 'effective computability') was solved in the 1930s within proof theory — Church and Turing provided a formal boundary that enabled Gödel's incompleteness proofs and the development of computability theory. The thesis served its founding function through the mid-20th century. By the 1980s–2000s, the problem had morphed: the rise of quantum computing, analog computation, and hypercomputation research raised the question of whether the thesis's scope should be extended to physical processes or remain proof-theoretic. The field did not resolve this; instead, the thesis remained canonical in textbooks while the scope-extension question was left informal and contested. The constraint now persists partly by inertia (it is what was taught in textbooks; changing it requires coordinated curricular revision) and partly by active enforcement (hypercomputation and alternative models face publication and funding barriers that reinforce the thesis's gatekeeping role). This is a classic mandatrophy pattern: the founding problem is substantially solved for the proof-theoretic domain, but the constraint's scope has been informally extended beyond that domain without institutional renegotiation. The field would benefit from clarifying whether the thesis is meant to be a proof-theoretic boundary (narrow scope, uncontested) or a constraint on all computability concepts (broad scope, highly contested). This story does not resolve the mandatrophy; it documents it as an omega variable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proof_theoretic_vs_semantic_scope,
    'Does the thesis''s scope extend only to proof-theoretic decidability (what can be proven in formal systems) or to all mathematical knowability (what functions exist or are well-defined)?',
    'Formalize the thesis''s quantification explicitly: is it ''∀f: f is provably computable ↔ f is Turing-computable'' (proof-theoretic scope) or ''∀f: f is mathematically knowable ↔ f is Turing-computable'' (maximal scope)? If proof-theoretic, the thesis does not foreclose constructivist or intuitionistic computability; if maximal, it does.',
    'A narrow interpretation leaves room for alternative computability concepts within mathematics; a maximal interpretation uses the thesis to suppress or exclude them. The type classification (tangled-rope vs. snare) depends on whether the extractiveness is from scope-overreach or from legitimate methodological gatekeeping.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proof_theoretic_vs_semantic_scope, conceptual, 'Whether the thesis is a proof-theoretic boundary or a maximal boundary on all mathematical computability.').

omega_variable(
    institutional_embedding_vs_epistemic_necessity,
    'Is the thesis''s canonical status in textbooks and curricula a reflection of its epistemological correctness, or is it an institutional path-dependency that has made revision costly?',
    'Compare historical curricula from institutions that adopted the thesis early vs. those that resisted; examine whether the thesis''s canonical status in hiring and funding correlates with its empirical defensibility or with sunk institutional costs (textbooks, degree requirements, hiring rubrics).',
    'If institutional embedding is independent of epistemic necessity, the suppression metric is higher than the constructive value of the boundary would justify — the constraint is more Snare-like than Rope-like. If embedding follows from defensibility, it is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_embedding_vs_epistemic_necessity, empirical, 'Whether the thesis''s institutional canonical status reflects justified epistemology or path-dependent embedding.').

omega_variable(
    alternative_computability_legitimacy,
    'Are hypercomputation, analog computation, and quantum computational advantage meaningfully different models of computation, or are they all subsumed within Turing-computability at the right level of abstraction?',
    'Rigorous technical analysis of whether the Turing-Church thesis applies to each alternative model when the model is formalized precisely. Do hypercomputation theorists have coherent formal definitions of what they mean by ''hypercomputable''? Can analog computation be discretized without losing essential properties?',
    'If the alternatives have coherent formal definitions and are genuinely distinct from Turing-computability, the thesis''s gatekeeping role is unjustified and the constraint is extractive. If the alternatives collapse into Turing-computability under formalization, the thesis''s scope is correct and the constraint is justified methodological gatekeeping.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_computability_legitimacy, empirical, 'Whether alternative computational models are genuinely distinct or subsumable within Church-Turing.').

omega_variable(
    physical_realizability_separation,
    'Is the epistemological boundary (what we can formally prove computable) independent of the physical claim (what physical systems can compute)?',
    'Formalize the thesis with explicit quantifiers distinguishing the proof-theoretic claim from physical claims. Show whether a function can be proven Turing-computable without assuming anything about physical processes that might realize it.',
    'If the claims are separable, then the epistemological reading avoids making false claims about physics, and the thesis is defensible within proof theory regardless of quantum mechanics or relativity. If they are entangled, the thesis is making physical claims it does not overtly acknowledge, and its institutional authority is partly illegitimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_realizability_separation, conceptual, 'Whether the epistemological boundary is independent of or entangled with physical claims about computation.').

omega_variable(
    non_constructive_mathematics_exclusion,
    'Does the thesis''s limitation to proof-theoretic Turing-computability exclude legitimate mathematical reasoning from non-classical logics (intuitionistic, paraconsistent, relevant logic)?',
    'Examine whether theorems proved in intuitionistic or constructive logic can be translated into equivalent theorems in Turing-computability without loss of content. Do non-classical frameworks generate computability concepts that Turing-computability does not capture?',
    'If non-classical frameworks generate genuinely distinct computability concepts, the thesis''s canonical status wrongly monopolizes the epistemic authority for defining ''computability.'' If the frameworks reduce to Turing-computability, the thesis''s scope is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_constructive_mathematics_exclusion, empirical, 'Whether non-classical mathematical logic generates computability concepts genuinely distinct from Church-Turing.').

omega_variable(
    reading_boundary_vs_reading_entanglement,
    'Is this epistemological reading (the thesis marks a proof-theoretic boundary) coherently separable from the mathematical-definition and physical-claim readings, or do they interweave such that the boundary reading presupposes one of the other readings?',
    'Analyze whether a consistent epistemological reading can be maintained without taking a position on whether the thesis is (a) true by convention or (b) an empirical claim about nature. If the epistemological boundary requires resolving one or both of those questions, the reading is not independent.',
    'If the readings are separable, each can be formalized as a distinct constraint with independent type classifications. If they are entangled, one reading over-determines the others, and the kernel is not truly three-way contested but rather one reading that encompasses the others.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_vs_reading_entanglement, conceptual, 'Whether the epistemological boundary reading is logically independent or entangled with the definitional and physical readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__epistemological_boundary_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement(chur_tr_t1960, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1960, 0.08).
narrative_ontology:measurement(chur_tr_t1985, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(chur_tr_t2005, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(chur_tr_t2024, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1936, 0.15).
narrative_ontology:measurement(chur_be_t1960, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1960, 0.28).
narrative_ontology:measurement(chur_be_t1985, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1985, 0.35).
narrative_ontology:measurement(chur_be_t2005, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement(chur_be_t2024, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1936, 0.18).
narrative_ontology:measurement(chur_su_t1960, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1960, 0.35).
narrative_ontology:measurement(chur_su_t1985, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1985, 0.48).
narrative_ontology:measurement(chur_su_t2005, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2005, 0.51).
narrative_ontology:measurement(chur_su_t2024, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__epistemological_boundary_reading, information_standard).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__epistemological_boundary_reading, 0.05).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, godel_incompleteness_proof_structure).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, turing_halting_problem_decidability).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Church-Turing thesis kernel. The mathematical-definition reading treats the thesis as a convention (true by stipulation, not empirically testable) and generates a different victim set (non-conventional computability models) and lower extractiveness. The physical-claim reading treats the thesis as an empirical hypothesis about the universe and generates quantum-mechanics and relativistic-computing as victims. All three readings share the kernel (the historical thesis-statement) but instantiate different constraint structures. The epistemological reading focuses on the thesis's role in defining the boundary between formally knowable and unknowable computation within proof-theoretic systems. Network edges represent downstream causal influence: the epistemological reading influences (but does not foreclose) the physical reading because accepting the epistemological boundary makes the physical reading's empirical claim harder to sustain — if formal proof-theoretic computability is the boundary, what grounds a physical claim that exceeds it?

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
