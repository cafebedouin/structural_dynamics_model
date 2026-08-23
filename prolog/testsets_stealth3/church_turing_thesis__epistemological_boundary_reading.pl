% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__epistemological_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Church-Turing Thesis, Epistemological Boundary Reading
 *   domain: philosophy of mathematics / philosophy of computation / foundations of computer science
 *
 * SUMMARY:
 *   This story instantiates the epistemological boundary reading of the
 *   Church-Turing thesis: the claim that the functions we can formally PROVE
 *   computable are exactly the Turing-computable ones, whatever physics might
 *   permit. On this reading the thesis is a methodological exclusion - it
 *   fixes what counts as a valid computability proof - and the standing
 *   arrangement under contest is the adjudication regime that certifies
 *   computability claims against effective-proof form. Per the kernel-reading
 *   epsilon rule, the referent of extractiveness is that standing regime,
 *   assessed by this reading's own lights (this reading endorses the boundary
 *   as sound epistemology), never the pluralist or hypercomputational
 *   alternative; the reading-indexed value is therefore low-to-moderate,
 *   concentrated at the margins where rival programs are priced out. KEY
 *   AGENTS (by structural relationship): proof_theoretic_establishment
 *   (institutional/arbitrage) - administers the certification venues and
 *   curricula; constructive_proof_communities (organized/constrained) -
 *   primary beneficiaries whose proof style is exactly what the boundary
 *   validates, mildly locked by identity of method;
 *   complexity_and_recursion_theorists (organized/mobile) - beneficiaries
 *   whose canonical objects presuppose the Turing model;
 *   formal_verification_industry (powerful/mobile) - beneficiaries riding
 *   certificate-to-behavior translation; hypercomputation_physicists
 *   (moderate/constrained) - primary targets whose positive results are
 *   uncertifiable; nonconstructive_computability_claimants (moderate/mobile)
 *   - targets whose unwitnessed existence claims are discounted;
 *   philosophers_of_computation (moderate/analytical) - observers mapping the
 *   structure. The claimed type and the metrics are independent authored
 *   facts: I claim tangled_rope because the structure coordinates the formal
 *   sciences through one certification standard while the same standard
 *   extracts legitimacy from rival programs under active enforcement; the
 *   metrics describe operation as I observe it.
 *
 * KEY AGENTS:
 *   - proof_theoretic_establishment: agenda-setter (institutional/arbitrage) - adjudicates which computability claims count as formally certified; runs the venues and curricula that maintain the boundary
 *   - constructive_proof_communities: primary beneficiary (organized/constrained) - effective-proof style is exactly what the criterion validates; mild identity-lock to proof-style capital
 *   - complexity_and_recursion_theorists: beneficiary (organized/mobile) - canonical objects defined relative to Turing computability
 *   - formal_verification_industry: beneficiary (powerful/mobile) - certificate-to-behavior translation depends on the criterion's stability
 *   - hypercomputation_physicists: primary payer (moderate/constrained) - physical-process computability claims cannot be certified under the criterion
 *   - nonconstructive_computability_claimants: payer (moderate/mobile) - unwitnessed existence-of-algorithm arguments sit outside formal knowability
 *   - philosophers_of_computation: analytical observer (moderate/analytical) - maps the readings and contests scope; nothing flows through them
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__epistemological_boundary_reading, 0.35).
domain_priors:suppression_score(church_turing_thesis__epistemological_boundary_reading, 0.45).
domain_priors:theater_ratio(church_turing_thesis__epistemological_boundary_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__epistemological_boundary_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__epistemological_boundary_reading, "Church-Turing Thesis, Epistemological Boundary Reading").
narrative_ontology:topic_domain(church_turing_thesis__epistemological_boundary_reading, "philosophy of mathematics / philosophy of computation / foundations of computer science").

domain_priors:requires_active_enforcement(church_turing_thesis__epistemological_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__epistemological_boundary_reading, '5b387cb8-5a83-42bc-a93d-82931b7a24a1').
narrative_ontology:cs_kernel_codification('5b387cb8-5a83-42bc-a93d-82931b7a24a1', distributed).
narrative_ontology:cs_authority_grounding('5b387cb8-5a83-42bc-a93d-82931b7a24a1', expertise).
narrative_ontology:cs_interpretation_layer_present('5b387cb8-5a83-42bc-a93d-82931b7a24a1').
narrative_ontology:cs_reading_relation('5b387cb8-5a83-42bc-a93d-82931b7a24a1', church_turing_thesis__mathematical_definition_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b387cb8-5a83-42bc-a93d-82931b7a24a1', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_axiom('5b387cb8-5a83-42bc-a93d-82931b7a24a1', foundational, formal_knowledge_of_computability_requires_effective_proof).
narrative_ontology:cs_axiom_status(formal_knowledge_of_computability_requires_effective_proof, holdable).
narrative_ontology:cs_axiom_grounding('5b387cb8-5a83-42bc-a93d-82931b7a24a1', formal_knowledge_of_computability_requires_effective_proof, conventional).
narrative_ontology:cs_axiom('5b387cb8-5a83-42bc-a93d-82931b7a24a1', secondary, boundary_certification_underwrites_undecidability_results).
narrative_ontology:cs_axiom_status(boundary_certification_underwrites_undecidability_results, holdable).
narrative_ontology:cs_axiom_grounding('5b387cb8-5a83-42bc-a93d-82931b7a24a1', boundary_certification_underwrites_undecidability_results, instrumental).
narrative_ontology:cs_reference_frame('5b387cb8-5a83-42bc-a93d-82931b7a24a1', effective_proof_demarcation_regime).
narrative_ontology:cs_drift_state('5b387cb8-5a83-42bc-a93d-82931b7a24a1', contemporary_post_quantum_mainstreaming, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('5b387cb8-5a83-42bc-a93d-82931b7a24a1', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, constructive_proof_communities).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, complexity_and_recursion_theorists).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, formal_verification_industry).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, hypercomputation_physicists).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, nonconstructive_computability_claimants).
narrative_ontology:constraint_vindicates(church_turing_thesis__epistemological_boundary_reading, turing_analysis_of_effective_method).
narrative_ontology:constraint_vindicates(church_turing_thesis__epistemological_boundary_reading, entcheidungsproblem_undecidability).
narrative_ontology:constraint_vindicates(church_turing_thesis__epistemological_boundary_reading, equivalence_of_convergent_computability_formalisms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the journals, conferences, curricula, and referee pools where computability claims are certified. Accepts a claim that a function is computable when the proof is effectively checkable and decodes to a machine procedure; routes non-conforming claims to specialist venues. Could widen admission criteria but would spend accumulated trust and force re-review of the published canon; in return it holds adjudication authority and enjoys the coordination benefits of a uniform standard.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, proof_theoretic_establishment, agenda_setter,
    institutional, generational, arbitrage, global).

% Work in proof styles (intuitionistic type theory, proof assistants such as Coq, Agda, and Lean) in which every proof of a computability statement mechanically yields the algorithm. Each strengthening of the shared certification criterion raises the exchange value of their output; adopting classical methods instead would strand their distinctive capital, so their participation in the standard is steady and invested.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, constructive_proof_communities, beneficiary,
    organized, generational, constrained, global).

% Define their central objects (recursive functions, complexity classes, reductions) relative to the Turing-machine model; the shared criterion makes their results automatically canonical across mathematics and computer science. They could migrate to neighboring quantitative fields, but would trade accumulated standing for the cost of starting over.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, complexity_and_recursion_theorists, beneficiary,
    organized, biographical, mobile, global).

% Builds verifiers, certified compilers, and assured-software products whose commercial promise rests on proofs translating into guaranteed behavior. Stability of the certification criterion is infrastructure for them; they benefit from it without administering it, and could redirect effort to adjacent assurance markets if the standard ever wobbled.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, formal_verification_industry, beneficiary,
    powerful, biographical, mobile, global).

% Study relativistic spacetimes, supertask constructions, and analog or quantum scenarios proposed to compute beyond Turing machines. Under the prevailing certification criterion their positive results cannot be registered as established knowledge of computability, so their findings circulate mainly in philosophy-of-physics journals and specialist workshops; grant lines and mainline-journal access are thin, and pivoting to conventional computational physics means abandoning the program's questions.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, hypercomputation_physicists, payer,
    moderate, biographical, constrained, global).

% Establish that an algorithm exists using excluded middle, compactness, or choice arguments without producing a witness. Referees increasingly ask for effective content, and unwitnessed claims are discounted as conditional; converting to a constructive style is possible at the cost of reworking methods and losing some results, so exit is real but priced.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, nonconstructive_computability_claimants, payer,
    moderate, biographical, mobile, global).

% Analyze what the thesis asserts, distinguish its readings, and publish critiques of its scope. They move between both communities' venues, face no certification pressure themselves, and neither receive nor pay anything through the criterion.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, philosophers_of_computation, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__epistemological_boundary_reading, diffuse).
narrative_ontology:fixing_cost_class(church_turing_thesis__epistemological_boundary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single shared criterion for certifying claims of the form 'f is computable': a claim counts as formally established exactly when an effectively checkable proof yields, or decodes to, a Turing-machine procedure. Researchers across mathematics, logic, and computer science can verify, accumulate, and build on one another's computability results without renegotiating what counts as evidence.
% TRANSFER_FUNCTION: Moves methodological legitimacy and career resources along the conformity line: certification, citations, grant eligibility, and curriculum slots flow to computability claims produced in effective-proof form, while claims of computability arrived at by non-constructive argument or attributed to physical processes are demoted to conjecture or speculation, and their proponents absorb the status and funding cost of the demotion.
% ABSENT_VOICES: Proponents of non-Turing models of computation deliberate mostly outside the adjudicating venues: their objections appear in philosophy-of-physics outlets and specialist workshops rather than the core logic and computer-science publications that maintain the criterion, and infinitary-proof traditions, whose computability notions diverge from Turing's, never held a seat in the boundary-setting conversation. Both groups would contest the criterion's scope if seated.
% DISAPPEARANCE_RATIONALE: Without the boundary, 'provably computable' would fragment by proof tradition: classical, constructive, infinitary, and physical-process attributions would each carry separate certification regimes. Undecidability and intractability results would lose their uniform baseline, since they quantify over the Turing-bounded class; proof assistants and verified-software toolchains would lose the semantic anchor linking certificate to behavior; and hypercomputation and non-constructive claims would compete for foundational status instead of being priced at the margin.
% FOUNDING_PROBLEM: Before 1936, 'effective procedure' was an intuitive notion, precise enough to guide practice but too vague for negative results. Hilbert's Entscheidungsproblem demanded a sharp demarcation of mechanical calculation; Turing, Church, Post, and Kleene supplied convergent analyses, and the epistemological boundary crystallized as the settlement that formal knowledge of computability is exactly what effective proof can express, hence bounded by machine computability.
% FOUNDING_PROBLEM_CORROBORATION: Historians of mathematics and philosophers of computation, including critics of the boundary's scope such as Copeland and Piccinini, corroborate that the pre-1936 vagueness problem was real and that the Turing analysis resolved it; their disagreement concerns whether the settlement should also govern attributions of computability to physical systems, not whether the founding problem existed. Corroboration therefore comes substantially from outside the beneficiary set.
narrative_ontology:disappearance_verdict(church_turing_thesis__epistemological_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__epistemological_boundary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__epistemological_boundary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(church_turing_thesis__epistemological_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__epistemological_boundary_reading, 0.35, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.35: the certification standard is load-bearing coordination whose routine costs are borne broadly and lightly, while the asymmetric charges - demotion of unwitnessed claims, uncertifiability of physical-process attributions, thinner funding for rival programs - land on identifiable minorities; hence low-to-moderate rather than negligible. Suppression 0.45 is authored as a RAW structural property, deliberately unscaled: enforcement here is epistemic and social (refereeing norms, curricula, funding gatekeeping), not coercive in the legal sense, and the engine applies scope/directionality scaling only to extractiveness. Accessibility collapse 0.55: alternatives persist and are visible (constructivism thrives, hypercomputation literature exists, pluralist logics publish), but the mainline practitioner population behaves as if no alternative certification exists. Resistance 0.45: sustained critique from philosophy of computation and the hypercomputation fringe, marginal in effect but persistent. Theater 0.20: ritual restatement of the thesis in textbooks and lectures grows with canonization, but the underlying verification function is live, keeping performative share low. The measurement series run on one shared grid (1936, 1960, 1980, 2000, 2015, 2026) with every tracked metric authored at every point. Trajectories: extraction accumulates slowly with canonization; suppression requirement rises through the institutionalization era (curricula, hardened refereeing) then eases slightly after 2015 as proof assistants automate compliance and lower the marginal cost of holding the line - enforcement capacity is now embedded in tooling rather than expended in dispute.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently and the engine computes that divergence from structural data. From the establishment seat the arrangement reads as standard-keeping: a rope it built and staffs, with the arbitration exit making dissent cheap to ignore. From the constructive-community seat it reads as vindication - their method IS the standard. From the hypercomputation-physicist seat the identical structure operates as methodological foreclosure: a program whose outputs can never be certified, with constrained exit because the program's questions are constitutive of the researchers' trajectories. The nonconstructive claimant seat is intermediate: partial discounting of some outputs, with a real but priced exit (conversion to constructive style). Same nominal discipline, same global standing, differentiated entirely by position relative to the criterion and by exit structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place constructive_proof_communities, complexity_and_recursion_theorists, and formal_verification_industry near the subsidy end (d near 0): the criterion appreciates their output at no charge to them, and their mobile-or-valued exits damp further. Victim declarations place hypercomputation_physicists near the full-target end (d near 1): their positive results are systematically uncertifiable and their constrained exit amplifies exposure. Nonconstructive_computability_claimants derive a damped target value (mobile exit within the proof culture pulls them off the pole, roughly 0.55-0.6): they pay on some outputs only. The establishment is the one seat the derivation chain handles poorly - it declares neither beneficiary nor victim position because it administers rather than collects through a group listing, so a directionality override pins it at 0.25: nearer the beneficiary end (the uniform standard subsidizes its adjudication authority) while bearing genuine coordination and re-review costs. Philosophers_of_computation carry the analytical atom and sit outside the flow. Global spatial scope modestly amplifies effective extraction at the target seats per the engine's scope modifier.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards two opposite mislabels. As pure snare: wrong, because the coordination function is primary and genuine - a single certification standard solves a real collective-action problem in verification, accumulation, and interoperability of results, and the extraction rides on that function rather than constituting it. As pure rope: also wrong, because the same structure that subsidizes conforming programs actively prices rival programs out of legitimacy, requires continuous enforcement (refereeing, curricula, funding norms) to hold, and leaves identifiable payers - the tangled-rope signature. Mandatrophy status: the founding problem (vagueness of 'effective method' blocking negative results) remains live, externally corroborated, so the mandate has not outlived its function; no sunset clause exists (nothing transitional is promised); theater is low, and although the receipt surface is diffuse with prohibitive fixing cost - the combination that elsewhere flags vestigiality - the cost here reflects a genuine re-certification burden on a functioning canon, not inertia around a dead function. The constraint is neither piton nor scaffold; the hybrid reading stands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the kernel church_turing_thesis (instantiating epistemological_boundary_reading). Which structural elements - victim set, epsilon, enforcement locus - change under the sibling readings mathematical_definition_reading and physical_claim_reading?',
    'Read the sibling stories'' base_properties and victim declarations; diff them against this story''s declarations and computed classifications.',
    'Under the definitional reading the victim set empties (a convention wrongs no one) and the type drifts mountain/rope-ward; under the physical reading the victim set expands to physical-hypercomputation attributions and epsilon becomes hostage to cosmology and noise results. Cross-reading comparison, not within-story adjustment, is the correct instrument.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel-level contest: three readings, three constraints, linked via network.affects_constraints.').

omega_variable(
    theorem_norm_boundary_ambiguity,
    'Is the provable-equals-Turing boundary a theorem-level necessity of effectively checkable proof systems, or a maintained disciplinary norm doing independent work?',
    'Metamathematical analysis characterizing whether any effectively checkable proof system could certify computability beyond Turing machines; if provably none can, the core is mountain-like and the sociological enforcement merely tracks it.',
    'If the core is mountain-like, part of the measured extraction is misattributed coordination cost and the coordination face strengthens; if the boundary is a norm doing independent work (admitting infinitary or choice-based certification would be feasible but institutionally refused), the hybrid classification deepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theorem_norm_boundary_ambiguity, conceptual, 'Whether the epistemic boundary is a logical limit wearing institutional clothing or a constructed standard with genuine discretionary content.').

omega_variable(
    hypercomputation_physical_viability,
    'Do any physically realizable processes compute beyond Turing-machine power (Malament-Hogarth spacetimes, Zeno machines, exact analog computation)?',
    'Cosmological observation of whether accessible spacetimes admit Malament-Hogarth structure, combined with noise and decoherence bounds on analog precision.',
    'If viable processes exist, the boundary''s ''regardless of physical possibility'' clause becomes a costly exclusion of real capability and epsilon at the margins rises sharply; if none exist, the exclusion costs little and this reading''s low-to-moderate epsilon is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hypercomputation_physical_viability, empirical, 'Empirical viability of the excluded alternative, the main lever on boundary-region extraction.').

omega_variable(
    nonwitnessing_proof_scope,
    'Exactly which classical proofs of computability claims fall outside formal knowability under this reading: only non-effective arguments, or also effective-system proofs that establish existence without extracting a witness?',
    'Proof-theoretic case audit of accepted results invoking Koenig''s lemma, compactness, or choice en route to computability conclusions, tracking whether referees demand witness extraction.',
    'A narrow scope shrinks the victim set and lowers epsilon; a broad scope (any non-witnessing route discounted) enlarges the victim set. The expected structural delta for this reading lives precisely here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nonwitnessing_proof_scope, conceptual, 'Where the boundary of the boundary sits for non-constructive proof practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__epistemological_boundary_reading, 1936, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ctt_epistemic_boundary_tr_t1936, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement(ctt_epistemic_boundary_tr_t1960, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1960, 0.08).
narrative_ontology:measurement(ctt_epistemic_boundary_tr_t1980, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(ctt_epistemic_boundary_tr_t2000, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2000, 0.16).
narrative_ontology:measurement(ctt_epistemic_boundary_tr_t2015, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2015, 0.19).
narrative_ontology:measurement(ctt_epistemic_boundary_tr_t2026, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2026, 0.2).

% Extraction over time
narrative_ontology:measurement(ctt_epistemic_boundary_be_t1936, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1936, 0.08).
narrative_ontology:measurement(ctt_epistemic_boundary_be_t1960, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1960, 0.14).
narrative_ontology:measurement(ctt_epistemic_boundary_be_t1980, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement(ctt_epistemic_boundary_be_t2000, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2000, 0.29).
narrative_ontology:measurement(ctt_epistemic_boundary_be_t2015, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2015, 0.33).
narrative_ontology:measurement(ctt_epistemic_boundary_be_t2026, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2026, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(ctt_epistemic_boundary_su_t1936, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1936, 0.1).
narrative_ontology:measurement(ctt_epistemic_boundary_su_t1960, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1960, 0.25).
narrative_ontology:measurement(ctt_epistemic_boundary_su_t1980, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1980, 0.38).
narrative_ontology:measurement(ctt_epistemic_boundary_su_t2000, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2000, 0.44).
narrative_ontology:measurement(ctt_epistemic_boundary_su_t2015, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2015, 0.47).
narrative_ontology:measurement(ctt_epistemic_boundary_su_t2026, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2026, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__epistemological_boundary_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__physical_claim_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the Church-Turing thesis' covers three structurally distinct claims, each with its own epsilon, victim set, and classification. The mathematical_definition_reading is upstream (its stipulative ground is cited by the other two readings); the physical_claim_reading is the empirical flank; this epistemological_boundary_reading sits between, bracketing physics while endorsing a substantive knowability boundary. Each story links the other two via affects_constraints; epsilon values differ by design and are not reconciled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
