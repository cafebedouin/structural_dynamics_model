% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__mathematical_definition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_church_turing_thesis__mathematical_definition_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: church_turing_thesis__mathematical_definition_reading
 *   human_readable: Church-Turing Thesis as Stipulative Definition of Effective Computability
 *   domain: philosophy_of_mathematics/foundations_of_computer_science
 *
 * SUMMARY:
 *   The Church-Turing thesis is colloquially treated as a single claim, but
 *   it conflates at least three structurally distinct assertions: an
 *   empirical claim about physical computation (the physical_claim_reading),
 *   a claim about the outer boundary of formally provable computability (the
 *   epistemological_boundary_reading), and — the reading instantiated here —
 *   a stipulative mathematical definition fixing what 'effective
 *   computability' means by convention. Under this reading, the thesis is not
 *   a hypothesis that could be empirically falsified by discovering a
 *   physical hypercomputer; it is closer to a naming convention, made
 *   compelling by the (independently proven, non-conventional) fact that
 *   several different formal systems developed in isolation turned out to be
 *   extensionally equivalent. The extraction and suppression profile here is
 *   deliberately near-mountain: no enforcement apparatus exists, no victim
 *   set exists, and any subfield is free to adopt a narrower or different
 *   stipulation for local purposes without contradicting this one, because a
 *   definition cannot be violated, only declined.
 *
 * KEY AGENTS:
 *   - mathematical_logicians: primary beneficiary (institutional/analytical) — use the shared vocabulary to compare formalisms
 *   - computer_science_researchers: secondary beneficiary (institutional/mobile) — apply the convention across CS subfields, free to adopt narrower local definitions
 *   - cross_formalism_communication: the coordination function itself, named as a non-agent beneficiary for completeness
 *   - philosophers_of_mathematics: analytical observer — assesses whether this reading is genuinely conventional or smuggles empirical content
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__mathematical_definition_reading, 0.03).
domain_priors:suppression_score(church_turing_thesis__mathematical_definition_reading, 0.02).
domain_priors:theater_ratio(church_turing_thesis__mathematical_definition_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__mathematical_definition_reading, rope).
narrative_ontology:human_readable(church_turing_thesis__mathematical_definition_reading, "Church-Turing Thesis as Stipulative Definition of Effective Computability").
narrative_ontology:topic_domain(church_turing_thesis__mathematical_definition_reading, "philosophy_of_mathematics/foundations_of_computer_science").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__mathematical_definition_reading, 'f1714090-94af-439d-8f33-28835b0e9751').
narrative_ontology:cs_kernel_codification('f1714090-94af-439d-8f33-28835b0e9751', formalized).
narrative_ontology:cs_authority_grounding('f1714090-94af-439d-8f33-28835b0e9751', expertise).
narrative_ontology:cs_interpretation_layer_present('f1714090-94af-439d-8f33-28835b0e9751').
narrative_ontology:cs_reading_relation('f1714090-94af-439d-8f33-28835b0e9751', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_reading_relation('f1714090-94af-439d-8f33-28835b0e9751', church_turing_thesis__epistemological_boundary_reading, influences).
narrative_ontology:cs_axiom('f1714090-94af-439d-8f33-28835b0e9751', foundational, computability_is_stipulated_not_discovered).
narrative_ontology:cs_axiom_status(computability_is_stipulated_not_discovered, holdable).
narrative_ontology:cs_axiom_grounding('f1714090-94af-439d-8f33-28835b0e9751', computability_is_stipulated_not_discovered, conventional).
narrative_ontology:cs_axiom('f1714090-94af-439d-8f33-28835b0e9751', secondary, definitional_claims_are_not_empirically_falsifiable).
narrative_ontology:cs_axiom_status(definitional_claims_are_not_empirically_falsifiable, holdable).
narrative_ontology:cs_axiom_grounding('f1714090-94af-439d-8f33-28835b0e9751', definitional_claims_are_not_empirically_falsifiable, conventional).
narrative_ontology:cs_reference_frame('f1714090-94af-439d-8f33-28835b0e9751', extensional_equivalence_convention).
narrative_ontology:cs_drift_state('f1714090-94af-439d-8f33-28835b0e9751', contemporary_complexity_theory_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f1714090-94af-439d-8f33-28835b0e9751', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__mathematical_definition_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, mathematical_logicians).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, computer_science_researchers).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, cross_formalism_communication).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, extensional_equivalence_of_computability_formalisms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use the stipulated equivalence of lambda-definability, Turing-computability, and general recursiveness as a stable shared vocabulary for proving results about computability without re-deriving the equivalence each time. They adopted the convention because it demonstrably let independently-developed formalisms interoperate; nothing compels the adoption beyond its usefulness.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, mathematical_logicians, beneficiary,
    institutional, civilizational, analytical, global).

% Rely on the convention to say 'computable' and mean the same thing across programming-language theory, complexity theory, and algorithm design. Could in principle adopt a different stipulation for a specialized purpose (e.g. restricted models for feasibility results) and often do, without contradicting the general convention.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, computer_science_researchers, beneficiary,
    institutional, generational, mobile, global).

% The practice of comparing results proved in different computational formalisms (recursive functions, lambda calculus, Turing machines, register machines) depends on a shared referent for 'computable.' This is not an agent but the coordination function itself, named here for completeness.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, cross_formalism_communication, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(church_turing_thesis__mathematical_definition_reading, cross_formalism_communication).

% Examine whether the thesis, under this reading, is properly classified as a definition (unfalsifiable by construction) or smuggles empirical content about what physical or mental processes count as 'effective.' They take no side flow of costs or benefits from the constraint itself, but their analysis is the primary source of the omega variables below.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, philosophers_of_mathematics, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__mathematical_definition_reading, diffuse).
narrative_ontology:fixing_cost_class(church_turing_thesis__mathematical_definition_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a single technical meaning for 'effective computability' so that results proved in different formalisms (recursive functions, lambda calculus, Turing machines) can be compared and combined without re-litigating what 'computable' means each time.
% TRANSFER_FUNCTION: Moves nothing extractive between parties; it moves terminological clarity from a state of multiple competing informal notions of 'effective procedure' to a single formally fixed referent, available to anyone who adopts the convention.
% ABSENT_VOICES: No party is excluded from the conversation in a way that matters to this reading — a stipulative definition has no constituency it silences, since anyone free to reject the convention and use a different formal definition for their own purposes (and many specialized sub-fields do, e.g. defining more restrictive complexity classes).
% DISAPPEARANCE_RATIONALE: If this specific reading of the thesis vanished overnight, mathematics would not rearrange: the underlying theorems (equivalence of lambda-definability, Turing-computability, and general recursiveness) remain true regardless of whether anyone adopts them as the definition of 'effective computability.' The convention could be renamed, re-stipulated, or dropped in favor of a different technical vocabulary with no change to any proof. This is close to the mountain end of the spectrum precisely because there is no enforcement machinery and no victim whose situation would change.
% FOUNDING_PROBLEM: In the 1930s, multiple independent formalizations of 'mechanical procedure' or 'effective calculability' (Godel/Herbrand recursive functions, Church's lambda calculus, Turing's machines, Post's systems) needed a common name once they were proven extensionally equivalent — logicians needed to say one thing and mean the same formal object.
% FOUNDING_PROBLEM_CORROBORATION: Working mathematicians and computer scientists across unrelated subfields (complexity theory, programming language semantics, recursion theory) independently use 'computable' to mean the same formal class without dispute, which is itself evidence the coordination problem the definition solves is still being solved by it; no outside party has identified a rival technical definition that has displaced it, and no institution profits from maintaining this particular reading over an alternative stipulation.
narrative_ontology:disappearance_verdict(church_turing_thesis__mathematical_definition_reading, world_unchanged).
narrative_ontology:founding_problem_status(church_turing_thesis__mathematical_definition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__mathematical_definition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(church_turing_thesis__mathematical_definition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__mathematical_definition_reading, 0.03, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(church_turing_thesis__mathematical_definition_reading_tests).
:- end_tests(church_turing_thesis__mathematical_definition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.02-0.03) across the entire interval because a stipulative definition, on its own terms, extracts nothing from anyone — there is no rent, no toll, no asymmetric transfer, just a shared label attached to an independently-proven equivalence class. Suppression is likewise near zero: nothing coerces adoption of this particular naming convention, and no alternative naming is blocked. Theater ratio stays low and nearly flat (0.03-0.05) — there is negligible performative activity because there is no enforcement to perform. Accessibility collapse is moderate (0.3) rather than mountain-high, reflecting that alternative technical stipulations remain genuinely available and are in fact used in specialized subfields (feasible computability, hypercomputation studies) without incident — this is what most sharply distinguishes this reading from the physical_claim_reading, where accessibility collapse would be much higher if the claim were true, since no physical alternative would exist. Resistance is low (0.08): the convention meets essentially no organized opposition because there is nothing to resist — you can simply decline to use the term this way.
 *
 * DIRECTIONALITY LOGIC:
 *   Every named beneficiary sits near the pure-beneficiary end of directionality: mathematical_logicians and computer_science_researchers gain a working vocabulary and pay no structural cost for its existence, because the convention is opt-in and cost-free to reject. There is no victim group under this reading — the schema's tangled_rope and snare gates correctly do not apply, and I have not declared victims because none exist within this specific reading's structure. This is the central structural delta from the sibling readings: the physical_claim_reading would need to name whoever bears the cost of an empirical claim's potential falsity (researchers pursuing hypercomputation who would be told their program is impossible), and the epistemological_boundary_reading would need to name whoever is denied recognition for provably-computable-but-not-Turing-computable claims (there are none, by the boundary reading's own lights, but the framing itself does epistemic work that this reading's framing does not attempt).
 *
 * MANDATROPHY ANALYSIS:
 *   There is no mandatrophy question here in the usual sense, because the founding problem (needing one name for the class of extensionally-equivalent formal notions of effective procedure) is still live and still solved by exactly this convention with no institutional apparatus that could have outlived its function — there is no apparatus. The founding_problem_status is authored 'live' rather than 'dead' or 'contested' because working mathematicians across unrelated subfields still independently converge on this usage, which is itself the corroboration: no captured institution needs to defend the convention because no institution collects rents from it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_vs_empirical_content_smuggling,
    'Does the ''mathematical definition'' framing actually stay pure, or does the historical argument FOR adopting this particular definition (Turing''s analysis of what a human ''computer'' following mechanical rules could do) quietly smuggle in an empirical or phenomenological claim about human/mechanical calculation that the stipulative framing then launders as pure convention?',
    'Close textual analysis of Turing''s 1936 argument and Church''s and Kleene''s parallel arguments, distinguishing the formally stipulative content (the equivalence proofs among lambda calculus, recursive functions, and Turing machines) from the informal motivating analysis (why THIS equivalence class, rather than some other, deserves the name ''effective computability''). If the motivating analysis is doing indispensable justificatory work, the definition reading is not as clean as claimed.',
    'If the stipulation is shown to rest on an unstated empirical or phenomenological premise about mechanical calculation, this reading''s low ε and ''true by convention'' framing would be undermined, and some of its structure would collapse toward the epistemological_boundary_reading or even the physical_claim_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_vs_empirical_content_smuggling, conceptual, 'Whether the stipulative definition secretly depends on an empirical or phenomenological premise.').

omega_variable(
    convention_selection_arbitrariness,
    'Is the specific convention (identifying ''effective computability'' with Turing/lambda/recursive-function equivalence) arbitrary among equally coherent alternatives, or is it privileged by something beyond convenience — e.g. by being the unique convention that all independently-derived formalisms converge on?',
    'Survey whether any historically or mathematically serious alternative formalization of ''effective procedure'' has ever been proposed that is NOT extensionally equivalent to this class, and assess whether its absence is evidence of non-arbitrariness or simply of the field''s path-dependence on the first successful formalizations.',
    'If genuinely arbitrary, this reading is a pure coordination convention (rope, as claimed). If privileged by convergence, the ''true by convention'' framing may understate how much objective content the definition carries, pushing the story toward mountain rather than rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(convention_selection_arbitrariness, conceptual, 'Whether the definitional convention is arbitrary or privileged by cross-formalism convergence.').

omega_variable(
    sibling_reading_boundary_location,
    'Exactly where does the disagreement between this reading and the epistemological_boundary_reading live — is it a dispute about what the thesis SAYS, or only about what significance to attach to an agreed-upon mathematical fact?',
    'Compare formal statements: if both readings agree on the extensional equivalence class and differ only in whether to call it ''the definition of effective computability'' or ''the boundary of formally knowable computation,'' the disagreement is about significance/labeling, not content — which would argue for treating them as one constraint rather than two.',
    'If the disagreement is purely about significance rather than content, decomposing into separate stories (as instructed) may overstate the structural distinctness of the readings; if the disagreement is about actual content (e.g. whether provably-computable-but-not-Turing-computable is a coherent category under one reading and not the other), the decomposition is well-founded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_boundary_location, conceptual, 'Whether this reading and the epistemological_boundary_reading differ in content or only in interpretive significance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__mathematical_definition_reading, 1936, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1936, 0.03).
narrative_ontology:measurement(chur_tr_t1954, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1954, 0.03).
narrative_ontology:measurement(chur_tr_t1972, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1972, 0.04).
narrative_ontology:measurement(chur_tr_t1990, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(chur_tr_t2008, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2008, 0.05).
narrative_ontology:measurement(chur_tr_t2026, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2026, 0.05).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1936, 0.02).
narrative_ontology:measurement(chur_be_t1954, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1954, 0.02).
narrative_ontology:measurement(chur_be_t1972, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1972, 0.02).
narrative_ontology:measurement(chur_be_t1990, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1990, 0.03).
narrative_ontology:measurement(chur_be_t2008, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 2008, 0.03).
narrative_ontology:measurement(chur_be_t2026, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 2026, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(church_turing_thesis__mathematical_definition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__mathematical_definition_reading, information_standard).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__mathematical_definition_reading, 0.01).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the church_turing_thesis kernel. mathematical_definition_reading (this file) authors the lowest ε and treats the thesis as a stipulative naming convention with no victims and no enforcement. physical_claim_reading treats the same label as an empirical claim about physical computation, with much higher stakes and a potential victim class (researchers whose hypercomputation programs would be foreclosed if the claim is true and enforced as settled science). epistemological_boundary_reading treats the label as marking the edge of formally provable computability, independent of physical realizability, and carries its own contested status in constructivist and proof-theoretic circles. The three share a historical origin (Church, Turing, Kleene, Post, 1930s) but diverge sharply on ε, victim structure, and enforcement — exactly the decomposition the ε-invariance principle requires rather than a single story with an observable-selection parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
