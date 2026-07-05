% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__epistemological_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Church-Turing Thesis as Epistemological Boundary of Provable Computability
 *   domain: philosophy_of_mathematics_and_computation
 *
 * SUMMARY:
 *   This story isolates ONE of three structurally distinct claims
 *   colloquially bundled as 'the Church-Turing thesis.' Here the thesis
 *   functions as a METHODOLOGICAL BOUNDARY on what counts as a formally valid
 *   proof of computability: a function is 'provably computable' if and only
 *   if a Turing-equivalent formalism can be exhibited to compute it,
 *   independent of whether some physical process might compute it by other
 *   means. This is neither the mathematical-definition reading (a stipulative
 *   convention, true by fiat, with no victims) nor the physical-claim reading
 *   (an empirical thesis about the physical universe, falsifiable by
 *   hypercomputational physics). Under THIS reading, the thesis operates as
 *   an actively enforced disciplinary gate: it coordinates a shared,
 *   cumulative proof standard across computability theory and complexity
 *   theory (the genuine coordination function), while simultaneously
 *   excluding non-constructive computability claims and physically-motivated
 *   hypercomputation proposals from counting as legitimate computability
 *   results (the extraction). The enforcement is real — journal referees,
 *   hiring committees, and curricula actively police the boundary — which is
 *   why this reading, unlike its siblings, carries a victim set and requires
 *   active enforcement to hold.
 *
 * KEY AGENTS:
 *   - computability_theorists: agenda_setter (institutional/arbitrage) — set and police the proof standard
 *   - complexity_theory_researchers: beneficiary (organized/constrained) — build hierarchies on the fixed standard
 *   - proof_theoretic_journal_gatekeepers: agenda_setter/beneficiary (institutional/arbitrage) — enforce the boundary at publication
 *   - non_constructive_computability_claimants: payer (moderate/constrained) — results reclassified as non-proofs
 *   - hypercomputation_researchers: payer (powerless/trapped) — excluded from core discipline
 *   - analog_and_physical_computation_theorists: excluded (powerless/trapped) — structurally outside the conversation
 *   - philosophy_of_mathematics_observers: observer (analytical/analytical) — tracks the reading divergence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__epistemological_boundary_reading, 0.28).
domain_priors:suppression_score(church_turing_thesis__epistemological_boundary_reading, 0.42).
domain_priors:theater_ratio(church_turing_thesis__epistemological_boundary_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(church_turing_thesis__epistemological_boundary_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__epistemological_boundary_reading, tangled_rope).
narrative_ontology:human_readable(church_turing_thesis__epistemological_boundary_reading, "Church-Turing Thesis as Epistemological Boundary of Provable Computability").
narrative_ontology:topic_domain(church_turing_thesis__epistemological_boundary_reading, "philosophy_of_mathematics_and_computation").

domain_priors:requires_active_enforcement(church_turing_thesis__epistemological_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__epistemological_boundary_reading, 'e36f5fcd-e3b1-4278-b702-5ff088135454').
narrative_ontology:cs_kernel_codification('e36f5fcd-e3b1-4278-b702-5ff088135454', formalized).
narrative_ontology:cs_authority_grounding('e36f5fcd-e3b1-4278-b702-5ff088135454', practice).
narrative_ontology:cs_interpretation_layer_present('e36f5fcd-e3b1-4278-b702-5ff088135454').
narrative_ontology:cs_reading_relation('e36f5fcd-e3b1-4278-b702-5ff088135454', church_turing_thesis__mathematical_definition_reading, influences).
narrative_ontology:cs_reading_relation('e36f5fcd-e3b1-4278-b702-5ff088135454', church_turing_thesis__physical_claim_reading, coexists_with).
narrative_ontology:cs_axiom('e36f5fcd-e3b1-4278-b702-5ff088135454', foundational, provability_requires_formal_exhibition).
narrative_ontology:cs_axiom_status(provability_requires_formal_exhibition, holdable).
narrative_ontology:cs_axiom_grounding('e36f5fcd-e3b1-4278-b702-5ff088135454', provability_requires_formal_exhibition, conventional).
narrative_ontology:cs_axiom('e36f5fcd-e3b1-4278-b702-5ff088135454', secondary, physical_realizability_irrelevant_to_proof_standard).
narrative_ontology:cs_axiom_status(physical_realizability_irrelevant_to_proof_standard, holdable).
narrative_ontology:cs_axiom_grounding('e36f5fcd-e3b1-4278-b702-5ff088135454', physical_realizability_irrelevant_to_proof_standard, instrumental).
narrative_ontology:cs_reference_frame('e36f5fcd-e3b1-4278-b702-5ff088135454', hilbert_program_effective_procedure_standard).
narrative_ontology:cs_drift_state('e36f5fcd-e3b1-4278-b702-5ff088135454', post_hypercomputation_challenge_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('e36f5fcd-e3b1-4278-b702-5ff088135454', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, computability_theorists).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, complexity_theory_researchers).
narrative_ontology:constraint_beneficiary(church_turing_thesis__epistemological_boundary_reading, proof_theoretic_journal_gatekeepers).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_claimants).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, hypercomputation_researchers).
narrative_ontology:constraint_victim(church_turing_thesis__epistemological_boundary_reading, analog_and_physical_computation_theorists).
narrative_ontology:constraint_vindicates(church_turing_thesis__epistemological_boundary_reading, turing_computability_as_formal_standard).
narrative_ontology:constraint_vindicates(church_turing_thesis__epistemological_boundary_reading, recursion_theoretic_equivalence_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and police the standard that a function counts as 'proved computable' only when a Turing-equivalent formalism (recursive functions, lambda calculus, register machines) exhibits it. They referee journals, set curricula, and adjudicate what qualifies as a valid computability proof. Their professional standing and the coherence of complexity theory depend on this boundary holding.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, computability_theorists, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__epistemological_boundary_reading, computability_theorists, beneficiary).

% Build an entire hierarchy of tractability (P, NP, PSPACE, etc.) on top of the assumption that the relevant notion of 'computation' is fixed and Turing-equivalent. If the boundary were loosened, foundational theorems would need re-derivation. They have strong incentive to treat the boundary as settled and rarely question it.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, complexity_theory_researchers, beneficiary,
    organized, generational, constrained, global).

% Decide whether a submitted computability or decidability proof is accepted as rigorous. A claim of computing a function 'non-constructively' or via an oracle/physical process outside the Turing-equivalent formalism is routinely rejected as not a proof of computability at all, regardless of its mathematical content, unless translated into an accepted formalism.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, proof_theoretic_journal_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__epistemological_boundary_reading, proof_theoretic_journal_gatekeepers, beneficiary).

% Mathematicians who establish that a function or problem is 'effectively decidable' via non-constructive existence arguments (e.g. using excluded middle over infinite search spaces, or set-theoretic devices) find their results reclassified as not constituting a computability proof in the accepted sense. They must either produce an explicit Turing-equivalent algorithm or have their result excluded from the computability canon, even when the mathematical content is otherwise accepted.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, non_constructive_computability_claimants, payer,
    moderate, biographical, constrained, global).

% Study models (infinite-time Turing machines, Zeno machines, oracle machines, relativistic computers) that formally compute functions beyond the Turing-computable set. Their work is treated by the mainstream community as a curiosity outside 'real' computability theory rather than as a competing formal boundary, largely because publication venues, funding panels, and hiring committees use the epistemological reading to exclude such models from the core discipline.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, hypercomputation_researchers, payer,
    powerless, biographical, trapped, global).

% Argue that certain analog or continuous physical processes might realize computations outside the Turing-equivalent formal hierarchy. Because the epistemological boundary reading defines 'provable computability' independently of physical realizability, their claims are structurally unable to count as computability proofs even if physically demonstrated — they are not part of the conversation that sets the formal standard.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, analog_and_physical_computation_theorists, excluded,
    powerless, biographical, trapped, global).

% Study the thesis's status as a boundary-setting methodological commitment rather than a mathematical theorem or an empirical claim, tracking how the three readings (epistemological, definitional, physical) diverge in their victim sets and enforcement mechanisms.
narrative_ontology:constraint_stakeholder(church_turing_thesis__epistemological_boundary_reading, philosophy_of_mathematics_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__epistemological_boundary_reading, diffuse).
narrative_ontology:fixing_cost_class(church_turing_thesis__epistemological_boundary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, shared standard for what counts as a valid proof that a function is computable, allowing mathematicians and computer scientists across the world to build cumulative results (decidability, complexity classes, reducibility hierarchies) on a common formal foundation rather than each adjudicating computability claims from scratch.
% TRANSFER_FUNCTION: Moves epistemic legitimacy and publication/career capital toward those whose work fits the Turing-equivalent formal mold, and away from those pursuing non-constructive or extra-Turing-equivalent notions of computability — regardless of the mathematical or physical merit of the excluded work.
% ABSENT_VOICES: Hypercomputation researchers and analog/physical computation theorists would argue that the boundary conflates 'what our current formalisms can capture' with 'what is knowable in principle,' but they are structurally excluded from the venues (journals, funding panels, core curricula) that set the standard they are being measured against.
% DISAPPEARANCE_RATIONALE: If the epistemological boundary reading vanished, the core mathematics of Turing computability itself would be unaffected (it would remain true that Turing machines compute a specific class of functions), but the METHODOLOGICAL GATEKEEPING — what counts as a legitimate computability proof — would reopen. Complexity theorists dispute that anything would change (the boundary is simply correct); excluded researchers argue the entire discipline's exclusionary practice would need re-examination.
% FOUNDING_PROBLEM: Early 20th-century mathematicians needed a rigorous, agreed notion of 'effective procedure' to resolve foundational questions (Hilbert's Entscheidungsproblem, the nature of provability) where an informal, intuitive notion of 'mechanically computable' was inadequate for proof.
% FOUNDING_PROBLEM_CORROBORATION: Computability theorists and complexity researchers (the benefiting parties) attest the problem remains live and the boundary remains the correct formal answer. Independent philosophers of mathematics and historians of computing (outside the benefiting community) corroborate that the original foundational problem was substantially resolved by the 1950s, and that continued use of the boundary to exclude hypercomputation and physical-computation research functions increasingly as disciplinary boundary-maintenance rather than active resolution of an open foundational question.
narrative_ontology:disappearance_verdict(church_turing_thesis__epistemological_boundary_reading, contested).
narrative_ontology:founding_problem_status(church_turing_thesis__epistemological_boundary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__epistemological_boundary_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(church_turing_thesis__epistemological_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__epistemological_boundary_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is low-to-moderate (0.28 at interval end) because the boundary's cost falls narrowly — on researchers pursuing non-Turing-equivalent computability claims — while the vast majority of the discipline is simply unaffected or actively served by the standard. Suppression (0.42) is meaningfully higher than extraction because maintaining the boundary requires active gatekeeping at journals, hiring panels, and funding bodies; this is a raw structural fact about enforcement machinery, not scaled by scope or power. Theater ratio stays very low (0.10) throughout: the enforcement is substantively functional (it genuinely produces a coherent cumulative theory), not performative. Accessibility collapse is moderate (0.55): a hypercomputation researcher can still publish, just not within the core computability canon — alternatives are marginalized, not annihilated, distinguishing this from a mountain's near-total collapse. Resistance (0.45) reflects real, organized pushback from hypercomputation and non-constructive-mathematics communities over decades, which is inconsistent with pure natural law.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (computability theorists, journal gatekeepers), the boundary is simply the correct formal criterion — asking 'what is provably computable' obviously means 'provable within an accepted formal system,' so there is no exclusion, only rigor. From the payer/excluded seats (hypercomputation researchers, non-constructive claimants), the identical rule is experienced as a closed door: their results are mathematically or physically motivated but structurally cannot register as computability proofs. The engine computes these as different seat-level types from the same structural data; the divergence is not a measurement error but the actual social fact of gatekeeping.
 *
 * DIRECTIONALITY LOGIC:
 *   Computability theorists and journal gatekeepers sit near the beneficiary end: they set the rule and their institutional standing depends on it (d low). Complexity theorists are structural beneficiaries at one remove — they don't administer the boundary but their entire theoretical apparatus rests on it holding (d low-moderate). Non-constructive claimants and hypercomputation researchers are targets: the same rule that coordinates the discipline is the rule that excludes their results from the canon (d high). Analog/physical computation theorists are excluded rather than merely taxed — they are not merely paying a cost within the system, they are outside the system that sets the cost, which is why they carry 'excluded' rather than 'payer' as primary role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Hilbert's Entscheidungsproblem, rigorizing 'effective procedure') was substantially resolved by the 1950s. What persists past that resolution is the boundary's use as an ongoing GATEKEEPING mechanism rather than as the answer to a still-open question. This is exactly the tangled-rope signature the mandatrophy analysis is built to catch: a genuine, historically necessary coordination function (a shared computability standard enabled cumulative complexity theory) has been retained past the point where its exclusionary force is still solving the original problem, and is now also serving disciplinary boundary-maintenance. Classifying this as pure mountain would hide the exclusion of hypercomputation research; classifying it as pure snare would deny the real, still-functioning coordination benefit complexity theory derives from a stable proof standard. Tangled rope holds both facts simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_versus_convention_ambiguity,
    'Is the epistemological boundary reading actually separable from the mathematical-definition reading, or does the ''proof standard'' just collapse into ''the stipulated definition of computability'' once examined closely?',
    'Examine cases where a non-Turing-equivalent formal system (e.g. certain infinite-time or oracle-relative systems) is proposed as an alternative computability standard: if the discipline treats this as a live methodological dispute (supporting the boundary reading) versus dismisses it as definitionally incoherent (supporting the definitional reading), that resolves which reading is operative in practice.',
    'If the boundary collapses into pure stipulation, this reading''s victim set (non-constructive claimants, hypercomputation researchers) dissolves into people who simply misunderstand a definition rather than people excluded by an enforced standard, and the constraint would reclassify toward mountain or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_versus_convention_ambiguity, conceptual, 'Whether the epistemological-boundary reading is structurally distinct from the definitional reading or reduces to it under scrutiny.').

omega_variable(
    gatekeeping_versus_genuine_rigor,
    'Is the active enforcement (journal rejection of non-Turing-equivalent computability claims) protecting genuine mathematical rigor that non-constructive proofs actually lack, or is it disciplinary boundary-maintenance that excludes mathematically valid but differently-framed results?',
    'Case-by-case review by neutral logicians (outside both the mainstream computability community and the excluded communities) of specific rejected non-constructive computability claims, assessing whether the mathematical content is sound under a weaker or different formal standard.',
    'If rejections consistently track genuine gaps in constructive content, extraction is lower than authored and the constraint is closer to rope. If rejections track formalism-mismatch rather than mathematical unsoundness, extraction is higher than authored and the constraint drifts toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gatekeeping_versus_genuine_rigor, empirical, 'Whether enforcement tracks genuine rigor gaps or formalism-parochialism.').

omega_variable(
    sibling_reading_disambiguation_committer,
    'Which of the three kernel readings (epistemological, definitional, physical) is actually operative when a working mathematician or computer scientist invokes ''the Church-Turing thesis'' in ordinary practice?',
    'Discourse analysis of how the thesis is invoked across textbooks, referee reports, and grant rejections — tracking whether invocations function as boundary-policing (this reading), stipulative definition (sibling), or empirical physical claim (sibling).',
    'If most invocations in practice are boundary-policing (as authored here), this reading carries the bulk of the real-world enforcement weight and the sibling readings are comparatively low-stakes philosophical positions; if most invocations are definitional, the enforcement apparatus described here is overstated and the true operative reading is closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_disambiguation_committer, conceptual, 'Which of the three sibling readings is empirically dominant in actual disciplinary practice, and what that implies about this reading''s true weight.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__epistemological_boundary_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t0, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(chur_tr_t0, observed).
narrative_ontology:measurement(chur_tr_t15, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 15, 0.06).
narrative_ontology:measurement_basis(chur_tr_t15, observed).
narrative_ontology:measurement(chur_tr_t30, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 30, 0.07).
narrative_ontology:measurement_basis(chur_tr_t30, observed).
narrative_ontology:measurement(chur_tr_t45, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 45, 0.08).
narrative_ontology:measurement_basis(chur_tr_t45, observed).
narrative_ontology:measurement(chur_tr_t60, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 60, 0.09).
narrative_ontology:measurement_basis(chur_tr_t60, observed).
narrative_ontology:measurement(chur_tr_t75, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 75, 0.1).
narrative_ontology:measurement_basis(chur_tr_t75, observed).
narrative_ontology:measurement(chur_tr_t90, church_turing_thesis__epistemological_boundary_reading, theater_ratio, 90, 0.1).
narrative_ontology:measurement_basis(chur_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(chur_be_t0, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(chur_be_t0, observed).
narrative_ontology:measurement(chur_be_t15, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement_basis(chur_be_t15, observed).
narrative_ontology:measurement(chur_be_t30, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 30, 0.19).
narrative_ontology:measurement_basis(chur_be_t30, observed).
narrative_ontology:measurement(chur_be_t45, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 45, 0.22).
narrative_ontology:measurement_basis(chur_be_t45, observed).
narrative_ontology:measurement(chur_be_t60, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 60, 0.25).
narrative_ontology:measurement_basis(chur_be_t60, observed).
narrative_ontology:measurement(chur_be_t75, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 75, 0.27).
narrative_ontology:measurement_basis(chur_be_t75, observed).
narrative_ontology:measurement(chur_be_t90, church_turing_thesis__epistemological_boundary_reading, base_extractiveness, 90, 0.28).
narrative_ontology:measurement_basis(chur_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(chur_su_t0, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(chur_su_t0, observed).
narrative_ontology:measurement(chur_su_t15, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 15, 0.32).
narrative_ontology:measurement_basis(chur_su_t15, observed).
narrative_ontology:measurement(chur_su_t30, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 30, 0.35).
narrative_ontology:measurement_basis(chur_su_t30, observed).
narrative_ontology:measurement(chur_su_t45, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 45, 0.37).
narrative_ontology:measurement_basis(chur_su_t45, observed).
narrative_ontology:measurement(chur_su_t60, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 60, 0.39).
narrative_ontology:measurement_basis(chur_su_t60, observed).
narrative_ontology:measurement(chur_su_t75, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 75, 0.41).
narrative_ontology:measurement_basis(chur_su_t75, projected).
narrative_ontology:measurement(chur_su_t90, church_turing_thesis__epistemological_boundary_reading, suppression_requirement, 90, 0.42).
narrative_ontology:measurement_basis(chur_su_t90, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__epistemological_boundary_reading, information_standard).
narrative_ontology:boltzmann_floor_override(church_turing_thesis__epistemological_boundary_reading, 0.05).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__mathematical_definition_reading).
narrative_ontology:affects_constraint(church_turing_thesis__epistemological_boundary_reading, church_turing_thesis__physical_claim_reading).

% DUAL FORMULATION NOTE:
% Part of the Church-Turing kernel family (3 stories). mathematical_definition_reading has near-zero ε (stipulative convention, no enforced victims). physical_claim_reading has ε contingent on physical hypercomputation evidence (empirical, falsifiable, different victim set — claimed physical models rather than mathematical proof practices). THIS story (epistemological_boundary_reading) is distinguished by an active, enforced methodological gate with an identifiable victim set drawn from mathematical and computational research communities. All three share the same kernel text but instantiate structurally distinct constraints per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
