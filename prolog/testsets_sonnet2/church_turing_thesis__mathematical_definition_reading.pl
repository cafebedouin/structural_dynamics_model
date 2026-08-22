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
 *   constraint_id: church_turing_thesis__mathematical_definition_reading
 *   human_readable: Church-Turing Thesis as Stipulative Definition of Effective Computability
 *   domain: mathematics/philosophy_of_computation
 *
 * SUMMARY:
 *   This constraint is the mathematical-definition reading of the
 *   Church-Turing thesis: the claim that Turing-computability,
 *   lambda-definability, and general recursiveness are STIPULATED to jointly
 *   constitute what mathematicians and computer scientists mean by 'effective
 *   computability.' On this reading the thesis is not an empirical claim
 *   about physics (that is a sibling constraint) nor an epistemological claim
 *   about the boundary of provable knowledge (also a sibling). It is a
 *   terminological convention — analogous to defining 'meter' — adopted
 *   because it lets a large, coherent body of theory (recursion theory,
 *   complexity theory, computability theory) proceed with a stable shared
 *   vocabulary. Because it is a definition, it has no victims: no one can be
 *   extracted from by a stipulation, only inconvenienced by having to learn
 *   or relearn a convention.
 *
 * KEY AGENTS:
 *   - mathematical_logicians: agenda_setter/beneficiary (institutional/analytical) — set and maintain the convention
 *   - computer_science_curriculum_designers: beneficiary (organized/mobile) — inherit stable teaching vocabulary
 *   - theoretical_computer_scientists: beneficiary (organized/mobile) — build results on the stipulated equivalence
 *   - philosophers_of_mathematics: observer (analytical/analytical) — assess the modal status of the claim
 *   - hypercomputation_theorists: excluded (moderate/mobile) — pursue a different, non-conflicting question under the same label
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__mathematical_definition_reading, 0.03).
domain_priors:suppression_score(church_turing_thesis__mathematical_definition_reading, 0.05).
domain_priors:theater_ratio(church_turing_thesis__mathematical_definition_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__mathematical_definition_reading, rope).
narrative_ontology:human_readable(church_turing_thesis__mathematical_definition_reading, "Church-Turing Thesis as Stipulative Definition of Effective Computability").
narrative_ontology:topic_domain(church_turing_thesis__mathematical_definition_reading, "mathematics/philosophy_of_computation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__mathematical_definition_reading, 'f91f606e-809f-4396-b6dc-f2fe48b7ebe6').
narrative_ontology:cs_kernel_codification('f91f606e-809f-4396-b6dc-f2fe48b7ebe6', formalized).
narrative_ontology:cs_authority_grounding('f91f606e-809f-4396-b6dc-f2fe48b7ebe6', expertise).
narrative_ontology:cs_interpretation_layer_present('f91f606e-809f-4396-b6dc-f2fe48b7ebe6').
narrative_ontology:cs_reading_relation('f91f606e-809f-4396-b6dc-f2fe48b7ebe6', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_reading_relation('f91f606e-809f-4396-b6dc-f2fe48b7ebe6', church_turing_thesis__epistemological_boundary_reading, influences).
narrative_ontology:cs_axiom('f91f606e-809f-4396-b6dc-f2fe48b7ebe6', foundational, computability_equivalence_is_stipulated_not_discovered).
narrative_ontology:cs_axiom_status(computability_equivalence_is_stipulated_not_discovered, holdable).
narrative_ontology:cs_axiom_grounding('f91f606e-809f-4396-b6dc-f2fe48b7ebe6', computability_equivalence_is_stipulated_not_discovered, conventional).
narrative_ontology:cs_axiom('f91f606e-809f-4396-b6dc-f2fe48b7ebe6', secondary, definitional_claims_are_not_empirically_falsifiable).
narrative_ontology:cs_axiom_status(definitional_claims_are_not_empirically_falsifiable, holdable).
narrative_ontology:cs_axiom_grounding('f91f606e-809f-4396-b6dc-f2fe48b7ebe6', definitional_claims_are_not_empirically_falsifiable, conventional).
narrative_ontology:cs_reference_frame('f91f606e-809f-4396-b6dc-f2fe48b7ebe6', stipulative_convergence_definition).
narrative_ontology:cs_drift_state('f91f606e-809f-4396-b6dc-f2fe48b7ebe6', contemporary_complexity_theory_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f91f606e-809f-4396-b6dc-f2fe48b7ebe6', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__mathematical_definition_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, mathematical_logicians).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, computer_science_curriculum_designers).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, theoretical_computer_scientists).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, turing_computability_equals_effective_computability_by_stipulation).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, recursion_theory_terminological_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and teach the stipulated equivalence of lambda-definability, Turing-computability, and general recursiveness as the agreed meaning of 'effective computability.' They set the terminological convention through journal usage, textbooks, and curricula. Nothing forces them to adopt this definition except that it lets a well-developed theory (recursion theory, complexity theory) proceed on stable footing.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, mathematical_logicians, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__mathematical_definition_reading, mathematical_logicians, beneficiary).

% Use the settled definition to build automata theory and computability courses without re-litigating what 'algorithm' means each semester. They could adopt a different formalism if one proved more pedagogically useful; nothing traps them to this one except convenience and consensus.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, computer_science_curriculum_designers, beneficiary,
    organized, generational, mobile, global).

% Build complexity theory, decidability results, and reducibility arguments on the stipulated identity of the formalisms. If the definition were revised, most existing theorems would need restatement, but no one is harmed by the current convention — they simply inherit a stable vocabulary.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, theoretical_computer_scientists, beneficiary,
    organized, generational, mobile, global).

% Examine whether the thesis, read this way, is a convention rather than a discovery, and whether treating it as 'true by definition' forecloses the empirical and epistemological readings of the same label. They take no stake in the outcome beyond getting the modal status right.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, philosophers_of_mathematics, observer,
    analytical, civilizational, analytical, universal).

% Explore models (oracle machines, infinite-time Turing machines) that exceed Turing-computability. Under the definitional reading their work is not refuted — it is simply talking about a different concept than 'effective computability' as stipulated. They are not victims of this reading, but they are not addressed by it either; the definitional convention has nothing to say about whether their models are physically realizable.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, hypercomputation_theorists, excluded,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a single stable technical meaning for 'effective computability' across mathematical logic and theoretical computer science, so that lambda calculus, Turing machines, and general recursive functions can be treated as interchangeable formalizations without repeated re-derivation of their equivalence's significance.
% TRANSFER_FUNCTION: Moves nothing between parties — no rents, no costs, no coerced compliance. What it moves is terminological load: researchers no longer bear the cost of re-justifying which formalism counts as 'an algorithm' in each new paper; that cost is paid once, collectively, by the convention's adoption.
% ABSENT_VOICES: Hypercomputation theorists and physicists interested in whether nature exceeds Turing-computability are not silenced by this reading — the definitional reading is explicitly indifferent to their question, since it is not making a claim about physical reality. Their objection, if any, is that the same label ('Church-Turing thesis') gets used for their empirical question, causing conflation with this purely definitional one.
% DISAPPEARANCE_RATIONALE: If the stipulative convention vanished overnight, the underlying mathematical equivalence theorems (proved by Turing, Kleene, Church, Post) would still hold; only the shared label for 'this is what we mean by effective computability' would need to be re-established. Practicing mathematicians would re-converge on essentially the same convention within a short time because the equivalence proofs, not the label, do the load-bearing work.
% FOUNDING_PROBLEM: In the 1930s, multiple independent formalizations of 'algorithm' (lambda calculus, general recursiveness, Turing machines) needed a name for their proven common extension, so that results in one formalism could be understood as results about computability as such rather than as an artifact of a particular notation.
% FOUNDING_PROBLEM_CORROBORATION: Working logicians and computer scientists outside the original Church-Turing-Kleene circle (e.g., subsequent generations of complexity theorists, and philosophers of mathematics such as Copeland and Sieg, who are not beneficiaries of any specific formalism's dominance) continue to affirm that a stable shared vocabulary for computability is still needed and still serves this function; no one attests the terminological need has disappeared.
narrative_ontology:disappearance_verdict(church_turing_thesis__mathematical_definition_reading, world_unchanged).
narrative_ontology:founding_problem_status(church_turing_thesis__mathematical_definition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__mathematical_definition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored near zero (0.03) because a stipulative definition transfers nothing coercively — at most it imposes the minor cost of learning a shared vocabulary, which is the standard cost of any coordination convention. Suppression is low (0.05): no one is coerced to accept the definition, though there is mild social pressure within the field to use standard terminology in publication. theater_ratio is low (0.05) because the convention performs exactly the function it claims (terminological stability) with negligible gap between stated and actual function. accessibility_collapse is moderate (0.4) rather than mountain-high: alternative formalizations (e.g., register machines, Post systems) remain fully accessible and provably equivalent — the convention collapses which LABEL wins, not which mathematical content is expressible. resistance is low (0.08): the main resistance in the literature is philosophical debate about whether the thesis is a definition, an empirical claim, or an epistemic boundary — that is a debate about MODAL STATUS, not resistance to the definitional content itself.
 *
 * PERSPECTIVAL GAP:
 *   There is little seat divergence here because a stipulative definition, correctly read, has no target/beneficiary asymmetry of the kind that produces divergent classification. Mathematical logicians (agenda_setter) and theoretical computer scientists (beneficiary) both experience this as pure coordination gain. The one seat that could diverge — hypercomputation_theorists (excluded) — experiences not extraction but mere non-address: the definitional reading is silent on their question rather than adversarial to it. This is structurally different from exclusion-as-suppression; they are excluded from a debate the definitional reading isn't having.
 *
 * DIRECTIONALITY LOGIC:
 *   All named beneficiaries sit near the full-beneficiary end of directionality: they collect the coordination gain (shared vocabulary, stable theorem base) and bear no extraction from the constraint's operation. There is no victim group because a definition cannot be violated or extracted from — you can only fail to adopt it, which is not a cost the definition imposes but a cost of non-participation in the shared vocabulary, symmetric with declining to use any technical term.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as rope (pure coordination) rather than tangled_rope or mountain matters because it is tempting to either (a) treat the thesis as settled natural law (mountain), which overclaims its epistemic status and forecloses the genuinely separate empirical and epistemological questions, or (b) treat disagreement about its modal status as evidence of hidden extraction (tangled_rope), which misdiagnoses a philosophical disagreement about KIND OF CLAIM as a material conflict of interest. Neither party benefits from suppressing an alternative convention — anyone free to propose a better one, and the field has occasionally adjusted terminology (e.g., 'recursive' to 'computable' in later literature) without institutional resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definitional_vs_substantive_status,
    'Is the Church-Turing thesis genuinely a pure stipulation (like defining ''prime number''), or does the FELT NECESSITY of the equivalence — the fact that every independently-motivated formalization of ''algorithm'' converged on the same class of functions — smuggle in an implicit empirical or epistemic claim that mere definitions do not carry?',
    'Philosophical analysis of whether stipulative definitions can be ''surprising'' in the way the equivalence of lambda calculus, Turing machines, and recursive functions was surprising to Church, Turing, and Kleene themselves — a genuinely arbitrary stipulation would not have this convergence property, suggesting the definitional reading may understate what the thesis captures.',
    'If the convergence is evidence of an underlying fact rather than a free choice, this reading may be parasitic on (or a disguised version of) the epistemological_boundary_reading rather than a fully independent stipulation — weakening its claim to zero extraction, since a definition that merely re-labels a discovered fact inherits some of the discovery''s contestability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definitional_vs_substantive_status, conceptual, 'Whether the definitional reading''s claimed conventionality is itself fully independent of the empirical/epistemic readings it is meant to be disjoint from.').

omega_variable(
    single_label_conflation_risk,
    'Does using the single label ''Church-Turing thesis'' for three structurally distinct claims (definitional, empirical, epistemological) create a systematic risk that arguments valid for one reading are illegitimately transferred to another — e.g., citing the definitional reading''s near-certainty to support confidence in the empirical reading''s contested claim?',
    'Textual/citation analysis of how the thesis is invoked across physics-of-computation papers versus mathematical logic papers versus philosophy-of-mind papers, checking whether authors register which reading they are using.',
    'If conflation is common, the mathematical_definition_reading''s near-zero extraction is sometimes borrowed illegitimately to lend false certainty to the physical_claim_reading, which is genuinely contested — this would not change this reading''s own ε, but would indicate this reading''s stability is exploited as reputational cover elsewhere in the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(single_label_conflation_risk, empirical, 'Whether the shared label enables illegitimate transfer of certainty from this reading to its more contested siblings.').

omega_variable(
    beneficiary_naturalness_ambiguity,
    'Given that this constraint declares beneficiaries (mathematical_logicians, curriculum_designers, theoretical_computer_scientists) while also being claimed as a rope with near-zero extraction rather than mountain, is there any risk that the convention is being presented as more purely coordinative than it is — e.g., does the dominance of the Turing-machine formalism in curricula (as opposed to equally valid alternatives) confer institutional advantage on those already trained in it, functioning as a mild, unacknowledged path-dependency lock-in?',
    'Survey of computability-theory curricula and hiring/publication norms to check whether alternative but equivalent formalizations (e.g., register machines) are treated as fully interchangeable in practice, or whether Turing-machine fluency has become a de facto credentialing filter independent of the underlying mathematical equivalence.',
    'If a credentialing lock-in exists, a small fraction of the currently near-zero extraction may reflect institutional path-dependency rather than pure coordination benefit, which would be relevant if this reading were ever re-evaluated as a rope-adjacent tangled_rope at the level of academic gatekeeping rather than at the level of the mathematical content itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_naturalness_ambiguity, empirical, 'Whether curricular dominance of one equivalent formalization creates soft institutional lock-in beyond pure terminological coordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__mathematical_definition_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t0, church_turing_thesis__mathematical_definition_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement(chur_tr_t15, church_turing_thesis__mathematical_definition_reading, theater_ratio, 15, 0.04).
narrative_ontology:measurement(chur_tr_t30, church_turing_thesis__mathematical_definition_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(chur_tr_t45, church_turing_thesis__mathematical_definition_reading, theater_ratio, 45, 0.05).
narrative_ontology:measurement(chur_tr_t60, church_turing_thesis__mathematical_definition_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(chur_tr_t75, church_turing_thesis__mathematical_definition_reading, theater_ratio, 75, 0.05).
narrative_ontology:measurement(chur_tr_t90, church_turing_thesis__mathematical_definition_reading, theater_ratio, 90, 0.05).

% Extraction over time
narrative_ontology:measurement(chur_be_t0, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(chur_be_t15, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 15, 0.02).
narrative_ontology:measurement(chur_be_t30, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 30, 0.03).
narrative_ontology:measurement(chur_be_t45, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 45, 0.03).
narrative_ontology:measurement(chur_be_t60, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 60, 0.03).
narrative_ontology:measurement(chur_be_t75, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 75, 0.03).
narrative_ontology:measurement(chur_be_t90, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 90, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(church_turing_thesis__mathematical_definition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__mathematical_definition_reading, information_standard).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__mathematical_definition_reading, 0.02).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the colloquial label 'the Church-Turing thesis' per the ε-invariance principle: this story (mathematical_definition_reading, rope, ε≈0.03) is the least contested and most stable member of the family; church_turing_thesis__physical_claim_reading treats the thesis as a falsifiable empirical claim about physical computability limits (higher contestability, different ε); church_turing_thesis__epistemological_boundary_reading treats it as a claim about the boundary of formally provable computability (also distinct ε and victim structure). The definitional reading is upstream in the sense that its stability is often cited (sometimes illegitimately, per omega single_label_conflation_risk) as grounds for confidence in the other two readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
