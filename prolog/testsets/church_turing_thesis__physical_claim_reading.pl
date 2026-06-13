% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__physical_claim_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: church_turing_thesis__physical_claim_reading
 *   human_readable: Church-Turing Thesis (Physical Claim Reading)
 *   domain: philosophy_of_mathematics_and_computation
 *
 * SUMMARY:
 *   The Church-Turing thesis occupies a rare epistemic position: it is taught
 *   as a mathematical theorem in computer science, invoked as a physical law
 *   in physics and philosophy, and debated as an empirical claim in
 *   foundations research. This story instantiates the physical-claim reading:
 *   the thesis as an empirical hypothesis about the universe — no physical
 *   process can compute functions beyond Turing-machine computability. Under
 *   this reading, the thesis is not a definition (mathematical reading) or a
 *   boundary of provability (epistemological reading), but a falsifiable
 *   claim about what physics permits. The constraint operates by treating
 *   this empirical question as already settled, suppressing research
 *   directions that take the question seriously and reframing powerful
 *   dissenters (quantum supremacy researchers) into the Turing-bounded
 *   framework. The beneficiary is the foundational computer science
 *   establishment; the victims are hypercomputation researchers and
 *   physical-computation-boundary investigators whose careers are shaped by
 *   operating against a thesis they believe remains empirically open.
 *
 * KEY AGENTS:
 *   - turing_machine_foundational_research: institutional beneficiary, high power, collects the constraint's coordination and extraction benefits through canonical status, textbook presence, and research funding concentration
 *   - hypercomputation_researchers: moderate power, constrained exit (constrained: leaving research means abandoning a scientific question they believe is open), bear publication bias and peer dismissal
 *   - physical_computation_dissenters: powerless, identity_locked exit (their intellectual identity is fused with the conviction that the thesis is empirically contestable), internalize the suppression even after attempting exit
 *   - quantum_supremacy_claimants: powerful, constrained exit, face forced reframing from 'hypercomputation' into 'polynomial speedup' — enforcement applied to powerful agents
 *   - epistemology_of_computation_philosophers: analytical observers, see the constraint's structure without bearing its enforcement costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__physical_claim_reading, 0.58).
domain_priors:suppression_score(church_turing_thesis__physical_claim_reading, 0.67).
domain_priors:theater_ratio(church_turing_thesis__physical_claim_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(church_turing_thesis__physical_claim_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__physical_claim_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__physical_claim_reading, "Church-Turing Thesis (Physical Claim Reading)").
narrative_ontology:topic_domain(church_turing_thesis__physical_claim_reading, "philosophy_of_mathematics_and_computation").

domain_priors:requires_active_enforcement(church_turing_thesis__physical_claim_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__physical_claim_reading, 'ebacf445-cdf8-4e84-81fe-f487f8fa2f64').
narrative_ontology:cs_kernel_codification('ebacf445-cdf8-4e84-81fe-f487f8fa2f64', fixed_text).
narrative_ontology:cs_authority_grounding('ebacf445-cdf8-4e84-81fe-f487f8fa2f64', extraction).
narrative_ontology:cs_interpretation_layer_present('ebacf445-cdf8-4e84-81fe-f487f8fa2f64').
narrative_ontology:cs_reading_relation('ebacf445-cdf8-4e84-81fe-f487f8fa2f64', church_turing_thesis__mathematical_definition_reading, forecloses).
narrative_ontology:cs_reading_relation('ebacf445-cdf8-4e84-81fe-f487f8fa2f64', church_turing_thesis__epistemological_boundary_reading, influences).
narrative_ontology:cs_axiom('ebacf445-cdf8-4e84-81fe-f487f8fa2f64', foundational, computability_is_physical_empirical_claim).
narrative_ontology:cs_axiom_status(computability_is_physical_empirical_claim, holdable).
narrative_ontology:cs_axiom_grounding('ebacf445-cdf8-4e84-81fe-f487f8fa2f64', computability_is_physical_empirical_claim, empirically_contingent).
narrative_ontology:cs_axiom('ebacf445-cdf8-4e84-81fe-f487f8fa2f64', foundational, turing_boundary_physically_falsifiable).
narrative_ontology:cs_axiom_status(turing_boundary_physically_falsifiable, holdable).
narrative_ontology:cs_axiom_grounding('ebacf445-cdf8-4e84-81fe-f487f8fa2f64', turing_boundary_physically_falsifiable, empirically_contingent).
narrative_ontology:cs_reference_frame('ebacf445-cdf8-4e84-81fe-f487f8fa2f64', open_empirical_computability_boundary).
narrative_ontology:cs_drift_state('ebacf445-cdf8-4e84-81fe-f487f8fa2f64', contemporary_quantum_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ebacf445-cdf8-4e84-81fe-f487f8fa2f64', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__physical_claim_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, turing_machine_foundational_research).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, hypercomputation_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, physical_computation_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, quantum_supremacy_claimants).
narrative_ontology:constraint_beneficiary(church_turing_thesis__physical_claim_reading, formal_verification_community).
narrative_ontology:constraint_victim(church_turing_thesis__physical_claim_reading, quantum_supremacy_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The academic and computational establishment (university departments, research institutes, textbook authors, funding bodies prioritizing foundational computer science) that treats Turing computability as the definitive, settled boundary of the computable. Collects benefits from the constraint's operation: research funding flows to work extending and refining Turing-bounded theory; textbooks canonicalize the thesis; conference prestige and journal acceptance go to researchers defending the framework. No enforcement cost — their research agenda aligns with the constraint's operation.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, turing_machine_foundational_research, beneficiary,
    institutional, generational, arbitrage, global).

% Scientists and theorists investigating whether physical systems might compute functions beyond Turing boundaries (abstract hypercomputation models, quantum systems, analog systems, unconventional computing). Bear career costs directly: research directions are labeled speculative, incoherent, or contrarian; peer review is skeptical; journal acceptance is harder; funding is limited; institutional prestige is lower. Exit is constrained because leaving the research means abandoning a scientific question they believe is genuinely open — the question is not expensive for them to investigate, but the constraint makes operating within the mainstream prohibitively costly.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, hypercomputation_researchers, payer,
    moderate, biographical, constrained, global).

% Individual researchers and theorists whose intellectual identity is fused with the conviction that the Church-Turing thesis is an empirical claim that might be falsified. Their professional identity, publications, and framing of the computability problem are constituted through the belief that the thesis remains open. Exit would require abandoning not just a research direction but a foundational commitment to empirical inquiry into what physics permits. The constraint operates through publication bias, peer dismissal, and career advancement barriers — suppression is both external and internalized through repeated exposure to institutional rejection of their framing.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, physical_computation_dissenters, payer,
    powerless, biographical, identity_locked, global).

% Organizations and researchers with substantial resources claiming quantum computers exceed classical capability on specific problems. Occupy an ambiguous position: they benefit from quantum research funding and institutional prestige (role=beneficiary), but face enforced reframing of their claims. Powerful actors with constrained exit: they cannot abandon quantum research (too much invested) but their claims that quantum systems exceed classical Turing computation are consistently translated/reframed by the mainstream into 'quantum systems efficiently solve problems Turing machines can solve in polynomial time.' This forced translation is the active enforcement mechanism applied to powerful dissidents: they are permitted to claim speed-up, not speed-of-computation-itself. The constraint operates by making certain framings of quantum capability invisible, even to the researchers who discovered them.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, quantum_supremacy_claimants, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__physical_claim_reading, quantum_supremacy_claimants, beneficiary).

% Engineers, logicians, and tool developers using Turing-bounded computational models to prove program correctness, system properties, and decidability boundaries. Benefit from the constraint because it provides a stable, universally recognized framework for determining what is and is not computable. The thesis grounds their methodology: if something is unprovable, it is uncomputable (within Turing bounds), so no algorithm can solve it. Their research agenda aligns with the constraint's operation — no enforcement cost, high coordination benefit.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, formal_verification_community, beneficiary,
    organized, generational, mobile, global).

% Philosophers and logicians analyzing what the Church-Turing thesis actually claims, what would count as evidence for or against it under different interpretations, and how the mathematical and physical readings differ. Take testimony from physical researchers, formal theorists, and quantum experimenters. Examine the thesis's history, its logical structure, and what its falsification would require. Analytical seat: they perceive the constraint's structure without bearing enforcement costs directly or collecting direct benefits.
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, epistemology_of_computation_philosophers, observer,
    moderate, generational, analytical, global).

% Physicists investigating fundamental physical processes (quantum mechanics, condensed matter, relativistic physics, quantum gravity) have formal expertise in what physical systems can do and how to measure their capabilities. They are structurally excluded from the Church-Turing debate: the thesis is treated as a mathematical fact by computer scientists and as a settled theoretical matter by foundational researchers. Were they formally invited to the empirical-claim conversation, they would bring experimental design, measurement standards, and falsifiability criteria. Their exclusion preserves the constraint by keeping the empirical question confined to computer science methodology and away from physics-based investigation. They are trapped because the question is empirical (within their domain) but institutionally closed (outside their conversation).
narrative_ontology:constraint_stakeholder(church_turing_thesis__physical_claim_reading, physics_community_at_large, excluded,
    institutional, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__physical_claim_reading, turing_machine_foundational_research).
narrative_ontology:fixing_cost_class(church_turing_thesis__physical_claim_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, universally recognized answer to 'what is computable?' — a single boundary that allows theoretical computer science, formal verification, complexity theory, and algorithm design to operate under a shared framework without reopening foundational questions for every new physical substrate or computational model. Enables proof of uncomputability and undecidability across the entire field.
% TRANSFER_FUNCTION: Moves research resources, publication prestige, faculty hiring, and institutional legitimacy from hypercomputation research toward Turing-bounded theoretical computer science. Redirects funding bodies to support work extending the Turing-bounded framework rather than exploring alternative computational models. Researchers challenging the physical-claim reading bear career friction and suppression; researchers defending the thesis enjoy canonical status and mainline funding.
% ABSENT_VOICES: Experimentalists and theoretical physicists who would bring falsifiability standards and experimental design expertise to the question of whether physical systems can exceed Turing computation; quantum researchers who would frame their own discoveries as hypercomputation rather than accepting forced reframing into polynomial speedup; hypercomputation theorists outside academic computer science who would articulate alternative computational models as genuinely different rather than as exotic mathematical exercises. These voices are excluded because the thesis is treated as a closed mathematical/formal question, not an empirical one open to physics-based investigation.
% DISAPPEARANCE_RATIONALE: If the physical-claim reading and its enforcement machinery vanished, theoretical computer science would reorganize around open boundaries rather than a settled thesis. Complexity theory would frame problems as 'Turing-computable-in-polynomial-time' rather than 'computable.' Formal verification would mark its Turing-bounded assumptions explicitly. Quantum research would recover its hypercomputation-hypothesis language rather than translating claims into polynomial-speedup terms. Research funding would split between Turing-bounded work and hypercomputation investigation. The coordination function (shared framework) would persist in attenuated form; the extraction and suppression would cease. Physics would shift toward empirical investigation of computational limits as fundamental questions. The constraint's disappearance would be rearrangement, not unchanged equilibrium.
% FOUNDING_PROBLEM: In the 1930s, mathematicians and logicians (Church, Gödel, Post, Turing) independently formulated what they believed to be computability. Their models — lambda-calculus, general recursion, production systems, Turing machines — were shown to be extensionally equivalent. The founding problem was: does this equivalence reflect a fundamental fact about computation (the thesis), or a bias in the mathematical models they examined? Equivalence of formal models offered compelling evidence but was not proof of what physics permits.
% FOUNDING_PROBLEM_CORROBORATION: Turing himself, Church, and Gödel attested to the founding problem's reality and proposed the thesis as a solution. Post-World-War-II foundational researchers (Rogers, Myhill, Rice) documented the ambiguity between mathematical and physical interpretations but treated the thesis as formally settled. Contemporary dissenters (Copeland, Deutsch, physicists investigating quantum computation) attest the founding problem remains live: the equivalence of 1930s-era mathematical models says nothing about what modern physics permits. No external source of corroboration has definitively settled the question in the thesis's favor — only the institutional consensus of computer science establishment treating it as settled. The thesis has acquired the status of received wisdom rather than demonstrated fact.
narrative_ontology:disappearance_verdict(church_turing_thesis__physical_claim_reading, contested).
narrative_ontology:founding_problem_status(church_turing_thesis__physical_claim_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__physical_claim_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(church_turing_thesis__physical_claim_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.58 at interval end, rising from 0.38 at t0) because the constraint's benefit is real and genuine: a unified, settled framework for computability IS valuable to theoretical computer science and enables formal verification to operate. The extraction component is the suppression of the empirical question and the redirection of resources away from hypercomputation research. The temporal trajectory shows the constraint tightening over the interval: as quantum computing scaled and made more powerful claims, the suppression requirement rose (from 0.45 to 0.67 over t0-t25, then plateaued), indicating that the constraint required more active enforcement to contain dissident research. Theater ratio rises slowly and plateaus at 0.28: the constraint's performative component (invoking 'mathematical proof' to settle an empirical question, defending the thesis as settled in textbooks while quantum experiments push against its boundaries) is present but not dominant — the coordination and extraction functions are more substantial than the theater. Accessibility collapse is moderate (0.62): alternatives (hypercomputation, unconventional computing models, physical-limit-exploration research programs) have not disappeared; they persist outside the mainstream, available to researchers willing to accept career costs. Resistance is high (0.71): the constraint meets continuous, organized resistance from quantum researchers, physics-minded computer scientists, and foundational researchers who treat the thesis as empirically open. The shared measurement grid ensures every metric is authored at every time point examined.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (turing_machine_foundational_research) and the payers should compute very differently. From the foundational establishment's seat, the constraint operates as a genuine coordination mechanism enabling decades of productive theoretical work; from the hypercomputation researchers' seat, the same structure is enforced suppression of an empirically open question. The engine should compute the foundational establishment as perceiving the constraint as rope (genuine coordination with modest extraction cost), while hypercomputation researchers perceive it as snare (suppression with no coordination benefit to them). This perspectival gap IS the point — the author does not reconcile it; the engine measures it from the structural data (beneficiary vs. payer role, high vs. constrained exit, institutional vs. moderate power).
 *
 * DIRECTIONALITY LOGIC:
 *   The turing_machine_foundational_research seat is a structural beneficiary: it collects the constraint's benefits (unified framework, research funding, canonical status, textbook presence, organizational advantage in hiring and prestige). Directionality for this seat is low (~0.1–0.25): it is subsidized by the constraint's operation. The hypercomputation_researchers seat is a structural payer: they bear publication friction, reduced funding, career costs, and the suppression of their research direction. Their exit is constrained (leaving means abandoning a scientific question they believe is open), which moderates their mobility. Directionality for this seat is high (~0.75–0.85). The physical_computation_dissenters seat is similarly high directionality but with identity_locked exit, which amplifies their extraction burden — they carry the suppression internalized, even after external suppression reduces. The quantum_supremacy_claimants seat is powerful but constrained (they cannot exit quantum research without loss), and they face the enforcement of reframing rather than direct suppression. Their directionality should be moderate (~0.55–0.65): they have power and some arbitrage (quantum research has independent value), but the enforced reframing is a form of extraction. No override is needed; the structural derivation should produce these values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits both a live coordination function and a genuine extraction mechanism, which defines tangled rope. The coordination function is real: a unified framework for computability enables theoretical computer science, formal verification, and algorithmic foundations research to operate under a shared understanding of what is computable. This is not a false coordination story — the founding problem (what is computable?) has a genuine answer within each mathematical model, and the equivalence of models is a real mathematical fact. However, the physical-claim reading of the thesis asserts that the logical equivalence of formal models in the 1930s settles an empirical question about physical reality — a much stronger claim. The constraint operates by treating this empirical question as already settled, which suppresses research directions that would investigate whether physical systems might exceed Turing computability. The extraction is the redirection of resources away from hypercomputation research and the career costs borne by researchers who treat the question as open. This is active enforcement: dissident views are reframed (quantum supremacy becomes 'polynomial speedup'), publications are biased, and career advancement is slowed for those who frame hypercomputation as an empirical question. The mandatrophy question is: does the founding problem (what is computable?) remain live, or has it been solved? Under the physical-claim reading, it remains live — the equivalence of formal models does not settle what physics permits. Therefore, mandatrophy has NOT been resolved; the constraint's original function (exploring the boundaries of computability) continues to be suppressed, and the constraint persists as extractive enforcement of a settled conclusion that remains empirically contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_vs_mathematical_boundary,
    'Is the Church-Turing thesis fundamentally a claim about mathematical formalizability (what we can prove computable) or about physical reality (what physical systems can compute)?',
    'Experimental investigation: if a physical system is constructed and demonstrated to compute a non-Turing-computable function with reproducible results, the physical reading is falsified and the mathematical reading remains. If decades of investigation across multiple physical substrates (quantum, analog, optical, relativistic) fail to produce such a system, the physical reading gains support.',
    'If the thesis is fundamentally mathematical, the constraint''s claimed type would shift toward rope (pure coordination, no extraction of suppression). If it is physical and falsifiable, the constraint remains tangled rope (coordination plus suppression). The two readings cannot coexist in a single constraint under ε-invariance — they are genuinely different constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_vs_mathematical_boundary, conceptual, 'Whether the thesis makes a claim about formal provability or physical possibility').

omega_variable(
    quantum_supremacy_status,
    'Do quantum computers exceed Turing computability on specific problems (hypercomputation), or do they achieve polynomial speedup on Turing-computable problems?',
    'Continued experimental work on quantum systems: if problems are solved that are proven to be non-Turing-computable (e.g., oracle-separation results experimentally instantiated), hypercomputation is demonstrated. If all achievements can be mapped to polynomial speedup on Turing problems, the Turing boundary holds.',
    'If quantum supremacy is hypercomputation, the physical reading of the thesis is falsified, the constraint dissolves, and hypercomputation research shifts from victim to mainstream. If quantum supremacy is polynomial speedup, the constraint persists and suppression of hypercomputation continues.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantum_supremacy_status, empirical, 'Whether recent quantum computing claims demonstrate hypercomputation or enhanced classical simulation').

omega_variable(
    identity_locked_suppression_internalization,
    'For physical_computation_dissenters with identity_locked exit, how much of the measured suppression is structural (external barriers to publication and funding) versus internalized (the researchers have adopted skepticism of their own research directions after repeated rejection)?',
    'Post-exit trajectory: if researchers leaving the field or the constraint for alternative positions report that suppression persists in their cognition and career orientation despite external barriers being removed, internalization is significant. If suppression drops sharply upon exit, it was primarily structural.',
    'If internalization is high, the constraint''s effective suppression is understated by the structural measure — the researchers carry the suppression with them and are unable to pursue alternative directions even when barriers drop. This would amplify the mandatrophy concern: the constraint persists in cognitive form even if institutional enforcement weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_suppression_internalization, empirical, 'Whether suppression of dissenters is structural or internalized').

omega_variable(
    kernel_reading_mutual_exclusivity,
    'Can the mathematical definition reading and the physical claim reading coexist as live positions within the same institutional framework, or does adoption of one reading logically foreclose the other?',
    'Careful analysis of the logical structure of each reading''s core claims and examination of institutional practice: if both readings are endorsed simultaneously by the same institutional actors (e.g., ''the thesis is a mathematical definition AND an empirical claim''), they coexist. If institutions consistently choose one and reject the other, they foreclose mutually.',
    'If the readings coexist, the constraint family is a network of independent constraints. If they foreclose mutually, the ε-invariance principle requires decomposition into separate constraints whose axioms are explicitly contradictory. The reading_relations and axioms in cs_structure depend on this determination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_mutual_exclusivity, conceptual, 'Whether the kernel readings are logically independent or mutually exclusive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__physical_claim_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t0, church_turing_thesis__physical_claim_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(chur_tr_t0, observed).
narrative_ontology:measurement(chur_tr_t5, church_turing_thesis__physical_claim_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(chur_tr_t5, observed).
narrative_ontology:measurement(chur_tr_t10, church_turing_thesis__physical_claim_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(chur_tr_t10, observed).
narrative_ontology:measurement(chur_tr_t15, church_turing_thesis__physical_claim_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(chur_tr_t15, observed).
narrative_ontology:measurement(chur_tr_t20, church_turing_thesis__physical_claim_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(chur_tr_t20, observed).
narrative_ontology:measurement(chur_tr_t25, church_turing_thesis__physical_claim_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(chur_tr_t25, observed).
narrative_ontology:measurement(chur_tr_t30, church_turing_thesis__physical_claim_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(chur_tr_t30, observed).
narrative_ontology:measurement(chur_tr_t40, church_turing_thesis__physical_claim_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(chur_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(chur_be_t0, church_turing_thesis__physical_claim_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(chur_be_t0, observed).
narrative_ontology:measurement(chur_be_t5, church_turing_thesis__physical_claim_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(chur_be_t5, observed).
narrative_ontology:measurement(chur_be_t10, church_turing_thesis__physical_claim_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(chur_be_t10, observed).
narrative_ontology:measurement(chur_be_t15, church_turing_thesis__physical_claim_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement_basis(chur_be_t15, observed).
narrative_ontology:measurement(chur_be_t20, church_turing_thesis__physical_claim_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(chur_be_t20, observed).
narrative_ontology:measurement(chur_be_t25, church_turing_thesis__physical_claim_reading, base_extractiveness, 25, 0.57).
narrative_ontology:measurement_basis(chur_be_t25, observed).
narrative_ontology:measurement(chur_be_t30, church_turing_thesis__physical_claim_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(chur_be_t30, observed).
narrative_ontology:measurement(chur_be_t40, church_turing_thesis__physical_claim_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(chur_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t0, church_turing_thesis__physical_claim_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(chur_su_t0, observed).
narrative_ontology:measurement(chur_su_t5, church_turing_thesis__physical_claim_reading, suppression_requirement, 5, 0.51).
narrative_ontology:measurement_basis(chur_su_t5, observed).
narrative_ontology:measurement(chur_su_t10, church_turing_thesis__physical_claim_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(chur_su_t10, observed).
narrative_ontology:measurement(chur_su_t15, church_turing_thesis__physical_claim_reading, suppression_requirement, 15, 0.63).
narrative_ontology:measurement_basis(chur_su_t15, observed).
narrative_ontology:measurement(chur_su_t20, church_turing_thesis__physical_claim_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(chur_su_t20, observed).
narrative_ontology:measurement(chur_su_t25, church_turing_thesis__physical_claim_reading, suppression_requirement, 25, 0.67).
narrative_ontology:measurement_basis(chur_su_t25, observed).
narrative_ontology:measurement(chur_su_t30, church_turing_thesis__physical_claim_reading, suppression_requirement, 30, 0.67).
narrative_ontology:measurement_basis(chur_su_t30, observed).
narrative_ontology:measurement(chur_su_t40, church_turing_thesis__physical_claim_reading, suppression_requirement, 40, 0.67).
narrative_ontology:measurement_basis(chur_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__physical_claim_reading, information_standard).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__physical_claim_reading, 0.12).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__physical_claim_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% The Church-Turing thesis kernel decomposes into three structurally distinct constraints: the mathematical_definition_reading (true by stipulation, ε~0.02, mountain or rope), the epistemological_boundary_reading (formal knowability boundary, ε~0.35, rope or tangled rope), and this physical_claim_reading (empirical claim about physics, ε~0.58, tangled rope or snare). Each reading instantiates a different constraint with different beneficiary/victim structures and different types. The ε-invariance principle requires separate constraint stories because the empirical status and falsifiability conditions differ fundamentally across readings. All three stories are linked via network.affects_constraints as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(church_turing_thesis__physical_claim_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
