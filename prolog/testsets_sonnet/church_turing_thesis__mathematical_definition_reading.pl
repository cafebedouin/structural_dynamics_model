% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__mathematical_definition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   domain: philosophy_of_mathematics/foundations_of_computer_science
 *
 * SUMMARY:
 *   The Church-Turing thesis, colloquially referred to as a single claim,
 *   actually conflates at least three structurally distinct assertions. This
 *   story isolates one of them: the reading under which the thesis is nothing
 *   more than a stipulative mathematical definition fixing what
 *   mathematicians mean by 'effective computability,' grounded in the proven
 *   equivalence of Turing machines, general recursive functions, and lambda
 *   calculus. Under this reading the thesis is true by convention — it cannot
 *   be empirically falsified because it is not an empirical claim, and it has
 *   no victims because a definition cannot be violated, only adopted or not
 *   adopted. This is deliberately narrower than the physical_claim_reading
 *   (an empirical claim about what physical processes can compute) and the
 *   epistemological_boundary_reading (a claim about the limits of formal
 *   provability). Those are separate constraints with separate ε values,
 *   linked here via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - mathematical_logic_community: agenda_setter/beneficiary (institutional/arbitrage) — stipulates and maintains the convention
 *   - computer_science_researchers: beneficiary (organized/arbitrage) — uses the stable vocabulary freely
 *   - textbook_authors_and_educators: beneficiary (moderate/mobile) — teaches the settled convention
 *   - philosophers_of_physical_computation: excluded (moderate/mobile) — their question is bracketed, not answered, by this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__mathematical_definition_reading, 0.03).
domain_priors:suppression_score(church_turing_thesis__mathematical_definition_reading, 0.04).
domain_priors:theater_ratio(church_turing_thesis__mathematical_definition_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__mathematical_definition_reading, rope).
narrative_ontology:human_readable(church_turing_thesis__mathematical_definition_reading, "Church-Turing Thesis as Stipulative Definition of Effective Computability").
narrative_ontology:topic_domain(church_turing_thesis__mathematical_definition_reading, "philosophy_of_mathematics/foundations_of_computer_science").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__mathematical_definition_reading, '05930425-9fc9-4383-a794-8b953f34fd6d').
narrative_ontology:cs_kernel_codification('05930425-9fc9-4383-a794-8b953f34fd6d', formalized).
narrative_ontology:cs_authority_grounding('05930425-9fc9-4383-a794-8b953f34fd6d', expertise).
narrative_ontology:cs_interpretation_layer_present('05930425-9fc9-4383-a794-8b953f34fd6d').
narrative_ontology:cs_reading_relation('05930425-9fc9-4383-a794-8b953f34fd6d', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_reading_relation('05930425-9fc9-4383-a794-8b953f34fd6d', church_turing_thesis__epistemological_boundary_reading, influences).
narrative_ontology:cs_axiom('05930425-9fc9-4383-a794-8b953f34fd6d', foundational, computability_is_stipulated_not_discovered).
narrative_ontology:cs_axiom_status(computability_is_stipulated_not_discovered, holdable).
narrative_ontology:cs_axiom_grounding('05930425-9fc9-4383-a794-8b953f34fd6d', computability_is_stipulated_not_discovered, conventional).
narrative_ontology:cs_axiom('05930425-9fc9-4383-a794-8b953f34fd6d', foundational, thesis_is_not_empirically_falsifiable).
narrative_ontology:cs_axiom_status(thesis_is_not_empirically_falsifiable, holdable).
narrative_ontology:cs_axiom_grounding('05930425-9fc9-4383-a794-8b953f34fd6d', thesis_is_not_empirically_falsifiable, conventional).
narrative_ontology:cs_reference_frame('05930425-9fc9-4383-a794-8b953f34fd6d', convergent_formalism_equivalence_1936).
narrative_ontology:cs_drift_state('05930425-9fc9-4383-a794-8b953f34fd6d', contemporary_computability_theory, gap(stable, minor, true)).
narrative_ontology:cs_created_at('05930425-9fc9-4383-a794-8b953f34fd6d', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__mathematical_definition_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, mathematical_logic_community).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, computer_science_researchers).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, textbook_authors_and_educators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, working_mathematicians_outside_logic).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, recursive_functions_equal_turing_computable_functions).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, lambda_definability_equals_effective_computability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopted the equivalence of recursive functions, lambda-definability, and Turing computability as the stipulated meaning of 'effective computability' in the 1930s. They maintain the definition because it converges multiple independently-derived formalisms into one stable technical term, letting proofs move freely between formalisms without re-deriving equivalence each time. They can revise the convention if a superior formal explication emerged, but none has displaced it in nine decades.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, mathematical_logic_community, agenda_setter,
    institutional, civilizational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__mathematical_definition_reading, mathematical_logic_community, beneficiary).

% Use the stipulated definition as the shared baseline for complexity theory, computability theory, and algorithm design. The convention lets them say 'computable' without re-litigating what the word means in every paper. They freely adopt or bracket the definition depending on context; nothing compels them to use it beyond its usefulness.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, computer_science_researchers, beneficiary,
    organized, generational, arbitrage, global).

% Teach the thesis as the definitional anchor for computability theory courses. They benefit from a stable, uncontested technical vocabulary that does not require resolving philosophical disputes about physical computation to teach the mathematics. They could teach an alternative formalism but the convention's near-universal adoption makes switching costly in coordination terms, not coercive ones.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, textbook_authors_and_educators, beneficiary,
    moderate, generational, mobile, global).

% Encounter the term 'computable function' in analysis, number theory, or other fields and rely on the stipulated definition as settled terminology, without needing to engage the underlying convention-formation debate at all.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, working_mathematicians_outside_logic, beneficiary,
    moderate, biographical, mobile, global).

% Would object that reading the thesis as 'mere convention' understates or evades the empirical question of whether physical processes can outrun Turing computability (hypercomputation, physical Church-Turing debates). Under this reading their question is simply off-topic rather than answered — they are not consulted because the definitional reading brackets their concern rather than engaging it.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, philosophers_of_physical_computation, excluded,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a single, stable technical meaning for 'effective computability' by stipulating the equivalence of Turing computability, general recursiveness, and lambda-definability, so that mathematicians and computer scientists can use the term without re-deriving or re-arguing its content in every context.
% TRANSFER_FUNCTION: Moves nothing between parties — this reading transfers no resource, cost, or advantage. It fixes vocabulary. Any 'cost' is the ordinary cost of learning a technical term, borne symmetrically by everyone who enters the field.
% ABSENT_VOICES: Philosophers of physical computation and researchers exploring hypercomputation models are not addressed by this reading at all — their question (can physics exceed Turing computability?) is bracketed as a separate, empirical matter under the sibling physical_claim_reading, not adjudicated here.
% DISAPPEARANCE_RATIONALE: If this stipulative convention vanished, the field would not lose any physical fact, but it would lose its shared technical vocabulary: every paper and proof relying on 'computable = Turing computable = recursive = lambda-definable' would need a replacement anchor, and decades of accumulated results phrased in these terms would need re-statement or re-verification of the convergence they now take for granted. The disruption is coordination-cost, not empirical loss.
% FOUNDING_PROBLEM: In the 1930s, multiple independent formalizations of 'mechanical computation' (Turing machines, general recursive functions, lambda calculus, Post systems) had been proposed with no shared vocabulary to say whether they captured the same intuitive notion or different ones. Mathematicians needed a stipulated anchor point to talk about computability at all.
% FOUNDING_PROBLEM_CORROBORATION: Independent formal-methods researchers and complexity theorists outside the original 1930s logic community (a field that itself no longer 'benefits' in any rent-collecting sense) continue to build results on the convergence of these formalisms; the proof-theoretic equivalences (Kleene, Turing 1937) are independently re-verifiable by anyone, which is itself the corroboration a stipulative mathematical convention can offer — its truth is not testimony-dependent the way an institutional claim would be.
narrative_ontology:disappearance_verdict(church_turing_thesis__mathematical_definition_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__mathematical_definition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__mathematical_definition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored near-zero (0.03) because a stipulative definition transfers nothing between parties — there is no rent, no toll, no asymmetric cost. Suppression is likewise near-zero (0.04): no one is coerced into using the term, and researchers who prefer alternative formalizations (e.g., non-classical computability notions) are free to use them, they simply are not using 'the' Church-Turing vocabulary. Theater ratio is very low (0.05) because there is essentially no performative overhead — using the definition IS the function. Accessibility collapse is moderate-high (0.6) not because alternatives are suppressed, but because the convergence proof (Turing 1937, Kleene) is so robust that no serious rival formalization of 'effective computability' has displaced it in nine decades — the collapse is epistemic convergence, not coercion. Resistance is very low (0.08): the convention is essentially uncontested at the level of the mathematics itself; what IS contested (physical computability, epistemic limits) belongs to the sibling readings, not to this one.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap within this reading — mathematical logicians, computer scientists, and textbook writers all experience the convention identically, as a useful stable anchor. The real perspectival divergence occurs ACROSS readings, not within this one: someone treating 'the Church-Turing thesis' as an empirical claim about physics will experience a completely different constraint (see physical_claim_reading, where genuine contestation and epistemic stakes exist).
 *
 * DIRECTIONALITY LOGIC:
 *   All named parties in this reading are beneficiaries or neutral users of a shared convention; none are victims because a definition, properly understood as a convention, cannot extract from anyone — it can only be adopted, ignored, or superseded. The mathematical_logic_community sits closest to agenda_setter because it originated and maintains the convention, but even here 'setting the agenda' is closer to 'proposing a useful convention that stuck' than to enforcement. Philosophers of physical computation are marked excluded not because they are harmed, but because their genuinely live question (can physical processes exceed Turing computability?) is simply not addressed by this reading — that question belongs to physical_claim_reading.
 *
 * MANDATROPHY ANALYSIS:
 *   There is no mandatrophy risk in this reading properly bounded: the founding problem (needing a shared technical vocabulary for 'mechanical computation') remains fully live — every computability-theory paper still needs the term to mean something stable. The classification as coordination (rope) rather than tangled_rope or snare reflects that no party pays a hidden cost through this constraint; the risk of mislabeling would arise only if this reading were illegitimately stretched to cover the physical or epistemological claims, which is precisely the confusion the kernel decomposition into three separate stories is designed to prevent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    convention_vs_discovery_ambiguity,
    'Is the convergence of Turing machines, recursive functions, and lambda calculus a pure stipulative convention (we CHOSE to call this bundle ''effective computability''), or did mathematicians DISCOVER that these three independently-motivated formalisms happen to coincide, which then makes the ''definition'' feel forced rather than free?',
    'Historical analysis of whether the equivalence proofs (Turing 1937, Kleene) were sought BECAUSE the formalisms were suspected to converge (discovery framing) or whether the convention was adopted first and the proofs merely confirmed compatibility (stipulation framing). This is likely irresolvable as a pure historical fact since both motivations were present simultaneously in the 1930s correspondence.',
    'If the convergence is better read as a discovery of a robust natural kind rather than an arbitrary stipulation, the mathematical_definition_reading shades toward the physical_claim_reading or epistemological_boundary_reading in spirit, even while remaining formally a definition. This would not change ε (still near-zero) but would affect how confidently this reading can be described as ''merely conventional.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convention_vs_discovery_ambiguity, conceptual, 'Whether the thesis-as-definition is arbitrary stipulation or discovered convergence of independently-motivated formalisms.').

omega_variable(
    definitional_beneficiary_extraction_check,
    'Do the beneficiaries of a stable technical definition (the mathematical logic community, computer science researchers, textbook authors) ever convert that stability into a form of extraction — e.g., gatekeeping publication or credentialing around adherence to the ''standard'' definition, penalizing heterodox computability formalisms?',
    'Survey of peer-review and curriculum practices for evidence that departures from the standard Church-Turing vocabulary (e.g., hypercomputation papers, oracle-machine heterodoxy) face elevated rejection rates attributable to terminological non-conformity rather than substantive weakness.',
    'If such gatekeeping exists, some of what is classified here as pure coordination (rope) would need to be re-examined for a tangled_rope component — the beneficiary group would then be extracting career/publication advantage from enforced terminological conformity, which the rope claim assumes does not occur.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(definitional_beneficiary_extraction_check, empirical, 'Whether definitional stability is ever weaponized as career/publication gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__mathematical_definition_reading, 1936, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1936, 0.03).
narrative_ontology:measurement(chur_tr_t1954, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1954, 0.04).
narrative_ontology:measurement(chur_tr_t1972, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1972, 0.04).
narrative_ontology:measurement(chur_tr_t1990, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(chur_tr_t2008, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2008, 0.05).
narrative_ontology:measurement(chur_tr_t2026, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2026, 0.05).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1936, 0.02).
narrative_ontology:measurement(chur_be_t1954, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1954, 0.02).
narrative_ontology:measurement(chur_be_t1972, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1972, 0.03).
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
% This story is one of three linked readings of the church_turing_thesis kernel. mathematical_definition_reading (this file) has by far the lowest ε — it is a stipulative convention with no victim set and near-zero contestation. physical_claim_reading carries an empirical wager about physical computability that could in principle be falsified by a hypercomputer, and epistemological_boundary_reading concerns the limits of formal provability independent of physical realizability. The three should never be merged into one ε value; each is a genuinely distinct constraint sharing only the colloquial label 'Church-Turing thesis.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
