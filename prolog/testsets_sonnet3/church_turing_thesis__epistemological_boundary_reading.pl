% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__epistemological_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Church-Turing Thesis as Epistemological Boundary of Provable Computability
 *   domain: philosophy_of_mathematics/foundations_of_computer_science
 *
 * SUMMARY:
 *   The Church-Turing thesis is colloquially treated as a single settled
 *   claim, but it decomposes into at least three structurally distinct
 *   assertions. This story is about the reading under which the thesis fixes
 *   the boundary of formally knowable computation: a function counts as
 *   computable, in the relevant epistemic sense, if and only if it is
 *   Turing-computable, and this holds regardless of what physics might
 *   permit. Under this reading the thesis functions simultaneously as (a) a
 *   genuine, historically vindicated coordination achievement — unifying
 *   Turing machines, lambda calculus, and recursive functions into one
 *   trusted standard that ended foundational fragmentation — and (b) an
 *   active methodological exclusion that determines which proofs, models, and
 *   career paths count as legitimate computability theory. The coordination
 *   function is real; so is the asymmetric cost borne by researchers whose
 *   formal or non-constructive work falls outside the boundary as currently
 *   policed.
 *
 * KEY AGENTS:
 *   - classical_recursion_theorists: agenda_setter/beneficiary (institutional/arbitrage) — administer the boundary through peer review and curricula
 *   - hypercomputation_researchers: payer (moderate/constrained) — formally rigorous models excluded from the legitimate-computability category
 *   - non_constructive_mathematics_proponents: payer (moderate/constrained) — existence proofs demoted rather than engaged
 *   - physical_computability_theorists: payer (moderate/constrained) — physical arguments held structurally irrelevant by the reading's own design
 *   - philosophers_of_mathematics: observer (analytical) — document the three-way conflation under one colloquial label
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__epistemological_boundary_reading, 0.32).
domain_priors:suppression_score(church_turing_thesis__epistemological_boundary_reading, 0.41).
domain_priors:theater_ratio(church_turing_thesis__epistemological_boundary_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__epistemological_boundary_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__epistemological_boundary_reading, "Church-Turing Thesis as Epistemological Boundary of Provable Computability").
narrative_ontology:topic_domain(church_turing_thesis__epistemological_boundary_reading, "philosophy_of_mathematics/foundations_of_computer_science").

domain_priors:requires_active_enforcement(church_turing_thesis__epistemological_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__epistemological_boundary_reading, '56c0d6ac-5944-4e79-9a6f-b94f8a8621a4').
narrative_ontology:cs_kernel_codification('56c0d6ac-5944-4e79-9a6f-b94f8a8621a4', formalized).
narrative_ontology:cs_authority_grounding('56c0d6ac-5944-4e79-9a6f-b94f8a8621a4', expertise).
narrative_ontology:cs_interpretation_layer_present('56c0d6ac-5944-4e79-9a6f-b94f8a8621a4').
narrative_ontology:cs_reading_relation('56c0d6ac-5944-4e79-9a6f-b94f8a8621a4', church_turing_thesis__mathematical_definition_reading, influences).
narrative_ontology:cs_reading_relation('56c0d6ac-5944-4e79-9a6f-b94f8a8621a4', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_axiom('56c0d6ac-5944-4e79-9a6f-b94f8a8621a4', foundational, provability_fixes_the_boundary_of_the_knowable).
narrative_ontology:cs_axiom_status(provability_fixes_the_boundary_of_the_knowable, holdable).
narrative_ontology:cs_axiom_grounding('56c0d6ac-5944-4e79-9a6f-b94f8a8621a4', provability_fixes_the_boundary_of_the_knowable, empirically_contingent).
narrative_ontology:cs_axiom('56c0d6ac-5944-4e79-9a6f-b94f8a8621a4', secondary, physical_possibility_is_irrelevant_to_formal_computability_status).
narrative_ontology:cs_axiom_status(physical_possibility_is_irrelevant_to_formal_computability_status, holdable).
narrative_ontology:cs_axiom_grounding('56c0d6ac-5944-4e79-9a6f-b94f8a8621a4', physical_possibility_is_irrelevant_to_formal_computability_status, conventional).
narrative_ontology:cs_reference_frame('56c0d6ac-5944-4e79-9a6f-b94f8a8621a4', post_hilbert_entscheidungsproblem_unification).
narrative_ontology:cs_drift_state('56c0d6ac-5944-4e79-9a6f-b94f8a8621a4', contemporary_hypercomputation_debate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('56c0d6ac-5944-4e79-9a6f-b94f8a8621a4', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, classical_recursion_theorists).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, computability_journal_editors).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, computer_science_curriculum_designers).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, hypercomputation_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, non_constructive_mathematics_proponents).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, physical_computability_theorists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, working_programmers_and_engineers).
narrative_ontology:constraint_vindicates(church_turing_thesis__epistemological_boundary_reading, extensional_equivalence_of_formal_computability_models).
narrative_ontology:constraint_vindicates(church_turing_thesis__epistemological_boundary_reading, turing_machine_as_canonical_computability_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and teach the received proof-theoretic framework in which 'computable' means 'Turing-computable' (equivalently lambda-definable, mu-recursive, etc.). They referee papers, set curricula, and adjudicate what counts as a valid computability proof. Their professional standing and the coherence of a century of results rest on the boundary holding as stated.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, classical_recursion_theorists, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__epistemological_boundary_reading, classical_recursion_theorists, beneficiary).

% Decide which submitted proofs and claimed counterexamples get published as legitimate computability results versus rejected as category errors (physical claims mistaken for mathematical ones, or vice versa). They enforce the boundary at the point of publication.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, computability_journal_editors, agenda_setter,
    institutional, generational, arbitrage, global).

% Build theory-of-computation coursework around the thesis as settled epistemological ground truth, which simplifies teaching and certification but forecloses classroom engagement with contested edges (hypercomputation, physical Church-Turing debates) as anything but historical curiosities.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, computer_science_curriculum_designers, beneficiary,
    organized, generational, constrained, national).

% Propose models (infinite-time Turing machines, Zeno machines, oracle-based hypercomputers, analog neural architectures with real-valued weights) that formally compute functions outside the recursive set. Their work is routinely classified as 'not real computability theory' by the boundary's gatekeepers, limiting publication venues, funding, and citation even when the mathematics is rigorous, because the thesis (in this reading) treats the boundary as fixing what counts as knowable computation, not merely as one convention among others.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, hypercomputation_researchers, payer,
    moderate, biographical, constrained, global).

% Work with existence proofs and non-constructive methods (classical logic, choice-dependent arguments) that establish results about functions without exhibiting an algorithm. Under the epistemological-boundary reading, such results are systematically treated as not establishing computability in the relevant sense, even when they establish existence or definability by other legitimate mathematical standards. Their proofs are demoted rather than refuted.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, non_constructive_mathematics_proponents, payer,
    moderate, generational, constrained, global).

% Investigate whether physical processes (quantum measurement, relativistic computation, analog systems) might realize computations outside the Turing-computable set. Under this reading, their physical arguments are held structurally irrelevant to the boundary of 'formally knowable' computation — the thesis is defined to be indifferent to physical possibility, which forecloses their empirical findings from mattering to the proof-theoretic classification, regardless of what physics ultimately permits.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, physical_computability_theorists, payer,
    moderate, biographical, constrained, global).

% Rely on the practical stability the boundary provides: knowing which problems are formally undecidable (halting problem, etc.) lets them stop searching for algorithms that provably cannot exist. They benefit from the boundary's clarity regardless of its deeper philosophical status.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, working_programmers_and_engineers, beneficiary,
    organized, biographical, mobile, global).

% Analyze the thesis's status — convention, empirical claim, or epistemological boundary — without a stake in which reading wins. They document how the three readings diverge in what they exclude and why the label 'Church-Turing thesis' conflates them.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, philosophers_of_mathematics, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable standard against which claims of computability are proven or refuted, allowing mathematicians across traditions (lambda calculus, recursive functions, Turing machines) to trust that a proof of computability in one formalism transfers to all others — genuine coordination that eliminated decades of potential fragmentation in the foundations of the discipline.
% TRANSFER_FUNCTION: Moves publication access, citation authority, curricular legitimacy, and funding priority from researchers whose formal models exceed or evade Turing-equivalence (hypercomputation, non-constructive existence claims, physically-motivated computability arguments) to researchers and institutions whose work stays within the classical proof-theoretic framework.
% ABSENT_VOICES: Hypercomputation researchers and non-constructive mathematicians are rarely seated on the editorial boards and curriculum committees that adjudicate the boundary; when they object that the boundary conflates a proof-theoretic convention with an epistemological limit, they publish in specialty venues outside the mainstream computability community, where their objections do not reach the gatekeepers who could revise the boundary's application.
% DISAPPEARANCE_RATIONALE: If the epistemological-boundary reading vanished, the underlying mathematics (Turing-computability, the halting problem, recursion theory) would remain fully intact — nothing about the theorems changes. What would rearrange is the gatekeeping function: journals and curricula would need a different, likely more permissive, standard for what counts as a legitimate computability result, and hypercomputation/non-constructive work would gain a currently foreclosed path to mainstream legitimacy. The mathematics is a mountain; the boundary's role as an epistemological gatekeeping standard is not.
% FOUNDING_PROBLEM: In the 1930s, multiple independent formalizations of 'effectively calculable function' (Turing machines, lambda calculus, general recursive functions) needed to be shown equivalent so mathematicians could trust a single notion of computability across foundational programs, resolving Hilbert's Entscheidungsproblem and grounding a rigorous theory of what algorithms can and cannot do.
% FOUNDING_PROBLEM_CORROBORATION: Working programmers and engineers outside the recursion-theory community corroborate that the practical coordination problem (agreement on undecidability results) remains live and genuinely solved. Philosophers of mathematics, an analytical seat with no stake in either the extraction or the coordination reading, corroborate that the epistemological-boundary framing has drifted from solving the original equivalence problem toward policing which computability claims count as legitimate — a genealogical shift documented independently of both the beneficiary community (recursion theorists) and the payer community (hypercomputation researchers).
narrative_ontology:disappearance_verdict(church_turing_thesis__epistemological_boundary_reading, contested).
narrative_ontology:founding_problem_status(church_turing_thesis__epistemological_boundary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__epistemological_boundary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(church_turing_thesis__epistemological_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__epistemological_boundary_reading, 0.32, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored low-to-moderate (0.32 at present) because the constraint's cost to excluded researchers is real but narrow — reputational and publication access, not material deprivation, and the mathematics itself remains available to anyone regardless of gatekeeping outcomes. Suppression (0.41) tracks the hardening of institutional gatekeeping (named journals, tenure committees, textbook canon) over ninety years, distinct from and higher than extraction because exclusion from the legitimate category is more forcefully enforced than the actual resource cost it imposes. Theater ratio stays low (0.12) because the coordination function — proof interchangeability across formal systems — remains substantively functional, not performative; what has grown is the gatekeeping layer riding on top of that genuine function, not a hollowing-out of the function itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical recursion theorists and journal editors are structural beneficiaries: they administer the boundary and their intellectual capital is denominated in the boundary holding. Hypercomputation researchers, non-constructive mathematics proponents, and physical computability theorists are targets: each supplies rigorous formal work that the boundary, as currently applied, classifies as outside legitimate computability theory, regardless of the internal rigor of their arguments. Working programmers are genuine beneficiaries with mobile exit (they can ignore the philosophical dispute and use the practical results either way) — their benefit does not depend on the gatekeeping function, only on the underlying mathematics, which is why they are not payers even though they operate 'inside' the boundary daily.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling in two directions. Calling this a pure Snare would ignore the genuine, historically vindicated coordination function (equivalence of formal computability models) that the thesis provided and that working programmers and engineers still benefit from daily. Calling it a pure Rope would ignore that the boundary is actively enforced against rigorous, formally coherent alternative computability claims (hypercomputational models, non-constructive existence proofs) by an identifiable gatekeeping apparatus (editors, curriculum committees) whose institutional position depends on the boundary's current scope. Tangled Rope captures both: the coordination achievement is real and the asymmetric enforcement cost is also real, running through the same structure (the accepted definition of 'computable').
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    convention_versus_epistemic_limit,
    'Is the epistemological-boundary reading actually distinguishable from the mathematical-definition reading, or does the boundary''s apparent epistemic force just smuggle in the stipulative convention as though it were a discovered limit?',
    'Trace whether working recursion theorists, when pressed, defend the boundary''s epistemic force (claims about what CAN be proven, full stop) or retreat to convention (claims about what WE MEAN by ''computable'') when challenged by hypercomputation proposals — a resolved defense pattern would disambiguate which reading is operative in practice.',
    'If the boundary collapses into pure convention under pressure, this reading''s claimed methodological-exclusion function is weaker than authored and the constraint moves toward Rope; if the epistemic-limit framing holds under sustained challenge, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convention_versus_epistemic_limit, conceptual, 'Whether the epistemological-boundary reading is stable or collapses into the definitional reading under scrutiny.').

omega_variable(
    gatekeeping_apparatus_necessity,
    'Is the gatekeeping function (excluding hypercomputation and non-constructive proofs from the ''legitimate computability'' category) necessary to preserve the coordination achievement, or is it a separable enforcement layer that could be relaxed without threatening the underlying equivalence result?',
    'Examine journals and subfields (e.g. computability-in-analysis, constructive reverse mathematics) that already publish boundary-adjacent work without apparent damage to the classical coordination framework, as a natural experiment in separability.',
    'If separable, the exclusionary enforcement is pure extraction riding on a real coordination core, sharpening the Tangled Rope reading toward Snare-adjacent; if inseparable, the enforcement is closer to a necessary cost of maintaining the coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gatekeeping_apparatus_necessity, conceptual, 'Whether the exclusionary enforcement layer is separable from the coordination function it rides on.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the epistemological-boundary reading''s victim set diverge from the physical-claim reading''s victim set — is physical_computability_theorists'' exclusion under THIS reading (proof-theoretic irrelevance) genuinely distinct from their exclusion under the physical-claim reading (empirical falsifiability), or do the two readings converge in practice on the same researchers for the same underlying reason?',
    'Compare specific rejected papers/grant proposals from physical computability theorists to identify whether reviewers invoke the ''not a valid computability proof'' framing (this reading) or the ''physically impossible'' framing (physical_claim_reading) — the reviewer''s stated rationale locates which reading is operative in the rejection.',
    'If reviewers consistently invoke the epistemic-boundary framing rather than the physical-impossibility framing, this reading''s distinct victim-set claim is corroborated; if reviewers conflate the two, the three-reading decomposition may be less clean in practice than in principle, though it remains valid analytically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, empirical, 'Whether this reading''s victim set is empirically distinguishable from the physical_claim_reading''s in actual gatekeeping practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__epistemological_boundary_reading, 1936, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1936, 0.02).
narrative_ontology:measurement(chur_tr_t1960, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1960, 0.03).
narrative_ontology:measurement(chur_tr_t1985, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 1985, 0.05).
narrative_ontology:measurement(chur_tr_t2000, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(chur_tr_t2012, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2012, 0.1).
narrative_ontology:measurement(chur_tr_t2026, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 2026, 0.12).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1936, 0.08).
narrative_ontology:measurement(chur_be_t1960, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1960, 0.12).
narrative_ontology:measurement(chur_be_t1985, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 1985, 0.18).
narrative_ontology:measurement(chur_be_t2000, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2000, 0.24).
narrative_ontology:measurement(chur_be_t2012, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2012, 0.28).
narrative_ontology:measurement(chur_be_t2026, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 2026, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t1936, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1936, 0.15).
narrative_ontology:measurement(chur_su_t1960, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1960, 0.2).
narrative_ontology:measurement(chur_su_t1985, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 1985, 0.28).
narrative_ontology:measurement(chur_su_t2000, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2000, 0.34).
narrative_ontology:measurement(chur_su_t2012, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2012, 0.38).
narrative_ontology:measurement(chur_su_t2026, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 2026, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__epistemological_boundary_reading, information_standard).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__epistemological_boundary_reading, 0.05).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__physical_claim_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings decomposed from the colloquial 'Church-Turing thesis' label per the ε-invariance principle. mathematical_definition_reading treats the thesis as a stipulative convention (ε near zero, no epistemic overreach, likely Mountain or Rope). physical_claim_reading treats it as an empirical claim about physical realizability, with a victim set of physical-computation researchers challenged on physical rather than proof-theoretic grounds. This story (epistemological_boundary_reading) occupies the middle ground: ε low-to-moderate, Tangled Rope, with a victim set defined by proof-theoretic exclusion (hypercomputation formalisms, non-constructive proofs) rather than either pure convention-acceptance or pure physical-possibility dispute. The three readings share the historical event (Turing/Church 1936) and the underlying mathematics but diverge sharply in what they claim, what they exclude, and who pays.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
