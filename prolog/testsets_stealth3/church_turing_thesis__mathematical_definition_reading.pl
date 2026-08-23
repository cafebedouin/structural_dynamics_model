% ============================================================================
% CONSTRAINT STORY: church_turing_thesis__mathematical_definition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   domain: philosophy_of_mathematics/foundations_of_computation
 *
 * SUMMARY:
 *   In this reading, the Church-Turing thesis is not a hypothesis about the
 *   world but a stipulation: 'effectively computable' means computable by a
 *   Turing machine (equivalently, lambda-definable or general-recursive). It
 *   is true by convention, unfalsifiable by construction, and its function is
 *   terminological alignment — one fixed predicate so that undecidability
 *   results, reductions, and equivalence theorems across mathematics, logic,
 *   and computer science refer to the same class of functions without
 *   restatement. The colloquial label 'Church-Turing thesis' spans three
 *   structurally distinct claims; per the epsilon-invariance principle this
 *   story authors only the definitional reading, linked to its siblings (the
 *   empirical physics claim and the epistemological boundary claim) through
 *   the network. Claim/metric independence is preserved: the claim is pure
 *   coordination, and the authored metrics independently describe near-zero
 *   extraction with a slowly rising share of ceremonial invocation around a
 *   live convention. KEY AGENTS (by structural relationship): -
 *   computability_theorists: primary beneficiary (organized/mobile) -
 *   theoretical_computer_scientists: secondary beneficiary (organized/mobile)
 *   - logic_textbook_authors: agenda setter (organized/constrained) -
 *   hypercomputation_physicists: excluded voice (moderate/mobile) -
 *   philosophy_of_mind_arguers: excluded voice (moderate/mobile) -
 *   historians_of_logic: analytical observer (analytical/analytical)
 *
 * KEY AGENTS:
 *   - computability_theorists: primary beneficiary (organized/mobile) — consume and reproduce the fixed definition in daily theorem-proving
 *   - theoretical_computer_scientists: secondary beneficiary (organized/mobile) — build complexity theory and semantics on the shared predicate
 *   - logic_textbook_authors: agenda setter (organized/constrained) — fix the canonical formulation in reference works and curricula
 *   - hypercomputation_physicists: excluded voice (moderate/mobile) — study beyond-Turing processes under forced parallel terminology
 *   - philosophy_of_mind_arguers: excluded voice (moderate/mobile) — computability claims narrowed by the stipulated meaning
 *   - historians_of_logic: analytical observer (analytical/analytical) — attest the genealogy from outside the user communities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__mathematical_definition_reading, 0.07).
domain_priors:suppression_score(church_turing_thesis__mathematical_definition_reading, 0.05).
domain_priors:theater_ratio(church_turing_thesis__mathematical_definition_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, extractiveness, 0.07).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__mathematical_definition_reading, rope).
narrative_ontology:human_readable(church_turing_thesis__mathematical_definition_reading, "Church-Turing Thesis as Stipulative Definition of Effective Computability").
narrative_ontology:topic_domain(church_turing_thesis__mathematical_definition_reading, "philosophy_of_mathematics/foundations_of_computation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__mathematical_definition_reading, '12cb4d77-1b2a-4cda-99fb-4507d7db8285').
narrative_ontology:cs_kernel_codification('12cb4d77-1b2a-4cda-99fb-4507d7db8285', formalized).
narrative_ontology:cs_authority_grounding('12cb4d77-1b2a-4cda-99fb-4507d7db8285', expertise).
narrative_ontology:cs_interpretation_layer_present('12cb4d77-1b2a-4cda-99fb-4507d7db8285').
narrative_ontology:cs_reading_relation('12cb4d77-1b2a-4cda-99fb-4507d7db8285', church_turing_thesis__epistemological_boundary_reading, influences).
narrative_ontology:cs_reading_relation('12cb4d77-1b2a-4cda-99fb-4507d7db8285', church_turing_thesis__physical_claim_reading, influences).
narrative_ontology:cs_axiom('12cb4d77-1b2a-4cda-99fb-4507d7db8285', foundational, effective_computability_is_stipulative).
narrative_ontology:cs_axiom_status(effective_computability_is_stipulative, holdable).
narrative_ontology:cs_axiom_grounding('12cb4d77-1b2a-4cda-99fb-4507d7db8285', effective_computability_is_stipulative, conventional).
narrative_ontology:cs_axiom('12cb4d77-1b2a-4cda-99fb-4507d7db8285', secondary, definition_forecloses_no_physical_claim).
narrative_ontology:cs_axiom_status(definition_forecloses_no_physical_claim, holdable).
narrative_ontology:cs_axiom_grounding('12cb4d77-1b2a-4cda-99fb-4507d7db8285', definition_forecloses_no_physical_claim, instrumental).
narrative_ontology:cs_reference_frame('12cb4d77-1b2a-4cda-99fb-4507d7db8285', stipulated_equivalence_convention).
narrative_ontology:cs_drift_state('12cb4d77-1b2a-4cda-99fb-4507d7db8285', contemporary_hypercomputation_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('12cb4d77-1b2a-4cda-99fb-4507d7db8285', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__mathematical_definition_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, computability_theorists).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, theoretical_computer_scientists).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, computation_model_equivalence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Prove theorems about which functions are and are not effectively computable, working inside the fixed meaning the convention supplies. The shared definition lets undecidability and reduction results cite one another without restating terms. A member who prefers a different meaning can define one in their own papers, at the price of fragmenting the literature their results need to land in.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, computability_theorists, beneficiary,
    organized, generational, mobile, global).

% Build complexity theory, programming-language semantics, and verification on the same fixed notion; algorithms courses teach it as background furniture. They gain portability of results across subfields. Exit is ordinary career mobility — the definition belongs to the field, not to any member, and no one is bound to it personally.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, theoretical_computer_scientists, beneficiary,
    organized, biographical, mobile, global).

% Choose which formulation of the definition appears in canonical texts and reference works, and their choices propagate through curricula and citation practice. They administer the convention rather than profit from it; printing a nonstandard definition would isolate their text from the literature it summarizes.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, logic_textbook_authors, agenda_setter,
    organized, generational, constrained, global).

% Study proposed physical processes — Malament-Hogarth spacetimes, infinite-time regimes, quantum arguments — that would compute beyond Turing-machine power if realized. Because 'effective computability' is already taken by the stipulated meaning, their objects must be named under new labels such as 'physical computability' or 'hypercomputation', and referees ask them to keep the vocabularies apart. They are not seated where the definition is fixed; their recourse is parallel terminology, not revision.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, hypercomputation_physicists, excluded,
    moderate, biographical, mobile, global).

% Use computability talk in debates about mind and mechanism, including whether physical brains exceed Turing power. The fixed meaning narrows what their claims can literally assert, so they either adopt the stipulated notion or coin their own; neither path revises the convention. They publish outside the venues where the definition is maintained.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, philosophy_of_mind_arguers, excluded,
    moderate, biographical, mobile, global).

% Trace how the informal 1930s notion of effective calculability became the modern stipulation, and how the competing interpretations of the thesis diverged from one another. They collect and attest the genealogy without administering or using the definition in proof.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, historians_of_logic, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__mathematical_definition_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns terminology across mathematics, logic, and computer science: one fixed meaning for 'effectively computable' so that undecidability results, reductions, and equivalence theorems refer to the same class of functions without restatement.
% TRANSFER_FUNCTION: Transfers no money, labor, or goods. It transfers definitional authority: the power to say what 'effectively computable' denotes moves from open contest to a settled stipulation, and correspondingly transfers a naming burden outward — anyone wanting a broader notion must mint and defend new terms.
% ABSENT_VOICES: Hypercomputation physicists and broad-reading philosophers of mind would ask that 'effective computability' remain open or carry physical content; they are outside the rooms (journals, curricula, reference works) where the definition is fixed, and their objection surfaces only as the requirement to coin parallel terms.
% DISAPPEARANCE_RATIONALE: If the stipulation vanished overnight, every undecidability claim, reduction, and equivalence theorem would lose its shared referent: papers would need to restate which definition of effective computability they use, curricula would fork, and the literature would re-coordinate — most likely back onto the same identification, but only after a period of terminological churn. The mathematics itself survives; the arrangement's coordination product does not.
% FOUNDING_PROBLEM: In the 1930s 'effectively calculable' named an informal intuition — finite, mechanical, rule-following procedure — with no precise extension, and proving the Entscheidungsproblem undecidable required fixing that extension. Church (lambda-definability) and Turing (machine computability) supplied candidate precisifications that proved coextensive, and the community adopted the identification as the meaning of the term.
% FOUNDING_PROBLEM_CORROBORATION: Historians of logic, attesting from outside the user communities, document the founding problem: Hilbert's Entscheidungsproblem demanded a precise boundary for mechanical procedure, and the 1936 papers of Church and Turing were direct responses to it. Even the reading's critics — philosophers of computation who dispute the definitional status — concede that the informal notion required precisification, corroborating the problem's reality independently of the benefiting parties. No beneficiary attests alone.
narrative_ontology:disappearance_verdict(church_turing_thesis__mathematical_definition_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__mathematical_definition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__mathematical_definition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(church_turing_thesis__mathematical_definition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__mathematical_definition_reading, 0.07, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.07 (interval end): the convention effects no material transfer; its only costs are conformity friction on dissenting terminology, and those costs accrue to no seat. Suppression is 0.05 and static — maintenance is reputational and editorial, with no enforcement-capacity buildup or decay to trace, so no suppression_requirement series is authored (a static enforcement picture is carried by the scalar, per the temporal-authoring rule). Theater_ratio rises 0.05 to 0.22 as a growing share of the thesis's public life becomes ritual invocation in textbooks and popularizations while its operative use — anchoring undecidability statements — remains routine; the function is intact, so the rise signals ceremony accreting around a live tool, not decay. Accessibility_collapse is 0.55: alternative stipulations remain fully available to anyone willing to define differently, but practical fragmentation costs half-close them in effect. Resistance is 0.18: episodic dissent (interaction-based computability proposals, broad-reading advocacy) without sustained opposition. Both series share one seven-point time grid (1936-2025) so no metric row is sampled against another's gaps.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats the convention is free infrastructure: results port across subfields and terms never need restatement. From the agenda_setter seat it is stewardship: the work is choosing formulations that keep the literature coherent, rewarded in reputation rather than rent. From the excluded seats the same stability reads as a closed door — the predicate they need is pre-emptied, and their only available motion is parallel coinage. The engine computes these per-seat differences from power, exit, and role data; a beneficiary seat computing near-pure coordination alongside an excluded seat computing mild imposition is the expected divergence, not an inconsistency.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (computability_theorists, theoretical_computer_scientists) are declared, driving their derived directionality toward the beneficiary pole; no victims are declared because a definition cannot be violated, only departed from — there is no transfer for anyone to bear. The agenda_setter administers without collecting: no rent flows through the seat. The excluded groups are deliberately left out of the victim array: their cost is a naming externality (forced neologism), documented in their situations and probed by the renaming_burden_materiality omega rather than asserted as extraction. Suppression is authored as a raw structural property (0.05) and is not scaled; only extractiveness is scaled by directionality and scope in the engine's computation — with near-zero base extraction, scaling leaves every seat's effective extraction negligible.
 *
 * MANDATROPHY ANALYSIS:
 *   Two misreadings are prevented. First, the rising theater trajectory invites a degraded-inertia verdict — heavy ceremonial invocation around an old convention — but the operative function (fixing the predicate every undecidability proof quantifies over) is fully exercised, so the mandate is live and mandatrophy_resolved is deliberately not declared. Second, the kernel's sibling readings carry real contests (a falsifiable physics claim, a provability boundary); folding those contests into this story would fabricate extraction and victims where the definition has none. The epsilon-invariance decomposition keeps this reading's epsilon at its own near-zero value, preserving the distinction between a working terminological convention and the disputes that surround it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_status,
    'This constraint instantiates only the mathematical-definition reading of the church_turing_thesis kernel; what structurally changes if a sibling reading (physical_claim_reading, epistemological_boundary_reading) is adopted instead?',
    'Compare the three sibling stories'' epsilon values, beneficiary/victim sets, and computed types; track which reading governs a given citational context (definition-use versus empirical-prediction-use versus provability-boundary-use).',
    'Under the physical reading the same sentence becomes falsifiable with potential victim sets among research programs staked on beyond-Turing computation; under the epistemological reading it bounds formal knowledge; under this reading it is unfalsifiable by construction and bears no victim set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_status, conceptual, 'Committer structure: this story is one reading of a three-reading kernel; sibling adoption changes epsilon, victim structure, and type.').

omega_variable(
    consolidation_genealogy,
    'Did the thesis function as a stipulation from 1936, or did it begin as a substantive identification (Church''s and Turing''s analyses of mechanical procedure) later consolidated into a definition by Kleene-era textbook practice?',
    'Close reading of Church 1936, Turing 1936-37, Kleene 1952, and subsequent canonical textbook formulations for the shift from argued identification to stipulated definition.',
    'If stipulative from the start, the convention''s authority is purely conventional; if consolidated later, part of its acceptance inherits the force of the original equivalence arguments, which changes how much traction a rival definition could gain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consolidation_genealogy, empirical, 'Whether definitional status was original to the 1936 papers or a later textbook consolidation.').

omega_variable(
    renaming_burden_materiality,
    'Do excluded groups (hypercomputation researchers, broad-reading philosophers of mind) bear material costs from the fixed definition — forced neologism, referee friction — and if so, are those costs captured by any seat?',
    'Publication and review-practice audit of papers proposing beyond-Turing or nonstandard computability notions: acceptance rates, terminological demands from referees, citation patterns relative to standard-terminology work.',
    'Systematic captured costs would push the story toward a hybrid coordination/extraction profile; incidental uncaptured friction leaves the pure-coordination classification intact with epsilon near zero.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renaming_burden_materiality, empirical, 'Materiality and capture status of the convention''s terminological externality on excluded groups.').

omega_variable(
    authority_framing_underdetermination,
    'Is the authority adjudicating this definition the mathematical community''s demonstrated expertise, or the textbook canon operating as a transmission lineage?',
    'Test which framing better predicts behavior: expertise predicts the definition''s standing is continuously re-earned as new computation models are located by theorem; lineage predicts canon inertia independent of demonstrable adequacy.',
    'Under the lineage framing, authority_grounding shifts from expertise to lineage, the interpretive layer reads as chain-of-transmission, and drift assessment would weight canon inertia more heavily; the expertise framing treats the definition as perpetually re-justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_framing_underdetermination, conceptual, 'CS-framing under-determination between expertise and lineage readings of definitional authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__mathematical_definition_reading, 1936, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ctt_math_def_tr_t1936, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1936, 0.05).
narrative_ontology:measurement_basis(ctt_math_def_tr_t1936, observed).
narrative_ontology:measurement(ctt_math_def_tr_t1952, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1952, 0.08).
narrative_ontology:measurement_basis(ctt_math_def_tr_t1952, observed).
narrative_ontology:measurement(ctt_math_def_tr_t1965, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement_basis(ctt_math_def_tr_t1965, observed).
narrative_ontology:measurement(ctt_math_def_tr_t1980, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1980, 0.13).
narrative_ontology:measurement_basis(ctt_math_def_tr_t1980, observed).
narrative_ontology:measurement(ctt_math_def_tr_t1995, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1995, 0.16).
narrative_ontology:measurement_basis(ctt_math_def_tr_t1995, observed).
narrative_ontology:measurement(ctt_math_def_tr_t2010, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement_basis(ctt_math_def_tr_t2010, observed).
narrative_ontology:measurement(ctt_math_def_tr_t2025, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2025, 0.22).
narrative_ontology:measurement_basis(ctt_math_def_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(ctt_math_def_be_t1936, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1936, 0.03).
narrative_ontology:measurement_basis(ctt_math_def_be_t1936, observed).
narrative_ontology:measurement(ctt_math_def_be_t1952, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1952, 0.04).
narrative_ontology:measurement_basis(ctt_math_def_be_t1952, observed).
narrative_ontology:measurement(ctt_math_def_be_t1965, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1965, 0.05).
narrative_ontology:measurement_basis(ctt_math_def_be_t1965, observed).
narrative_ontology:measurement(ctt_math_def_be_t1980, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1980, 0.05).
narrative_ontology:measurement_basis(ctt_math_def_be_t1980, observed).
narrative_ontology:measurement(ctt_math_def_be_t1995, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1995, 0.06).
narrative_ontology:measurement_basis(ctt_math_def_be_t1995, observed).
narrative_ontology:measurement(ctt_math_def_be_t2010, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 2010, 0.06).
narrative_ontology:measurement_basis(ctt_math_def_be_t2010, observed).
narrative_ontology:measurement(ctt_math_def_be_t2025, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 2025, 0.07).
narrative_ontology:measurement_basis(ctt_math_def_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(church_turing_thesis__mathematical_definition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__mathematical_definition_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% 'Church-Turing thesis' is a colloquial label spanning three structurally distinct constraints (epsilon-invariance decomposition). This story authors the stipulative-definition reading: epsilon ~0.07, no victim set, coordination of terminology, unfalsifiable by construction. church_turing_thesis__physical_claim_reading authors the empirical universe claim (falsifiable, with potential victim sets among research programs staked on beyond-Turning computation). church_turing_thesis__epistemological_boundary_reading authors the formal-knowledge boundary claim. This reading is upstream: the predicate it fixes is what the other two quantify over, so affects_constraints edges run from this story to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
