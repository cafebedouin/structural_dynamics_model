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
 *   constraint_id: church_turing_thesis__mathematical_definition_reading
 *   human_readable: Church-Turing Thesis as Stipulative Definition of Effective Computability
 *   domain: philosophy_of_mathematics/foundations_of_computation
 *
 * SUMMARY:
 *   This story instantiates the mathematical-definition reading of the
 *   church_turing_thesis kernel: the thesis-sentence is a stipulation fixing
 *   what 'effective computability' means, true by convention and carrying no
 *   empirical content. Its operation is terminological alignment across
 *   mathematics and computer science: once the stipulation is adopted,
 *   undecidability and incompleteness results acquire a determinate object,
 *   and a theorem proven against Turing machines transfers automatically to
 *   lambda calculus, recursive functions, and Post systems through the
 *   equivalence theorems. Per the committer-frame rules, the sibling readings
 *   (physical_claim_reading, epistemological_boundary_reading) are separate
 *   constraints in separate files with their own epsilon values and party
 *   structures; nothing about them is averaged into this story. The
 *   claim/metric split is deliberate: claimed_type records the structure I
 *   believe true (voluntary coordination among net beneficiaries, no
 *   suppressed alternatives), while the metrics record descriptive operating
 *   values (near-zero residual costs). KEY AGENTS (by structural
 *   relationship): - mathematical_logicians: primary beneficiary
 *   (organized/mobile) - gain precise, transferable undecidability results; -
 *   theoretical_computer_scientists: primary beneficiary (organized/mobile) -
 *   canonical baseline for decidability and complexity statements; -
 *   formal_methods_practitioners: secondary beneficiary
 *   (moderate/constrained) - machine-checked libraries encode the definition;
 *   - journal_editors_and_textbook_authors: agenda-setter
 *   (institutional/mobile) - administer transmission channels and light
 *   conformity norms; - constructive_mathematics_schools: excluded voice
 *   (organized/mobile) - maintain parallel effectiveness stipulations outside
 *   the default-setting conversation; - philosophers_of_computation:
 *   analytical observer (moderate/analytical) - see the full kernel
 *   structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(church_turing_thesis__mathematical_definition_reading, 0.06).
domain_priors:suppression_score(church_turing_thesis__mathematical_definition_reading, 0.04).
domain_priors:theater_ratio(church_turing_thesis__mathematical_definition_reading, 0.07).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, theater_ratio, 0.07).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(church_turing_thesis__mathematical_definition_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(church_turing_thesis__mathematical_definition_reading, rope).
narrative_ontology:human_readable(church_turing_thesis__mathematical_definition_reading, "Church-Turing Thesis as Stipulative Definition of Effective Computability").
narrative_ontology:topic_domain(church_turing_thesis__mathematical_definition_reading, "philosophy_of_mathematics/foundations_of_computation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(church_turing_thesis__mathematical_definition_reading, 'fe39ad63-c01f-44cf-98ed-8a50bc0487ce').
narrative_ontology:cs_kernel_codification('fe39ad63-c01f-44cf-98ed-8a50bc0487ce', formalized).
narrative_ontology:cs_authority_grounding('fe39ad63-c01f-44cf-98ed-8a50bc0487ce', expertise).
narrative_ontology:cs_interpretation_layer_present('fe39ad63-c01f-44cf-98ed-8a50bc0487ce').
narrative_ontology:cs_reading_relation('fe39ad63-c01f-44cf-98ed-8a50bc0487ce', church_turing_thesis__physical_claim_reading, forecloses).
narrative_ontology:cs_reading_relation('fe39ad63-c01f-44cf-98ed-8a50bc0487ce', church_turing_thesis__epistemological_boundary_reading, forecloses).
narrative_ontology:cs_axiom('fe39ad63-c01f-44cf-98ed-8a50bc0487ce', foundational, effective_computability_is_stipulative).
narrative_ontology:cs_axiom_status(effective_computability_is_stipulative, holdable).
narrative_ontology:cs_axiom_grounding('fe39ad63-c01f-44cf-98ed-8a50bc0487ce', effective_computability_is_stipulative, conventional).
narrative_ontology:cs_axiom('fe39ad63-c01f-44cf-98ed-8a50bc0487ce', secondary, thesis_carries_no_empirical_content).
narrative_ontology:cs_axiom_status(thesis_carries_no_empirical_content, holdable).
narrative_ontology:cs_axiom_grounding('fe39ad63-c01f-44cf-98ed-8a50bc0487ce', thesis_carries_no_empirical_content, conventional).
narrative_ontology:cs_reference_frame('fe39ad63-c01f-44cf-98ed-8a50bc0487ce', stipulative_definition_convention).
narrative_ontology:cs_drift_state('fe39ad63-c01f-44cf-98ed-8a50bc0487ce', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fe39ad63-c01f-44cf-98ed-8a50bc0487ce', '').
narrative_ontology:cs_kernel_id(church_turing_thesis__mathematical_definition_reading, church_turing_thesis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, mathematical_logicians).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, theoretical_computer_scientists).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, formal_methods_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(church_turing_thesis__mathematical_definition_reading, journal_editors_and_textbook_authors).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, lambda_calculus_turing_equivalence).
narrative_ontology:constraint_vindicates(church_turing_thesis__mathematical_definition_reading, undecidability_transfer_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State and prove undecidability and incompleteness results against a single fixed meaning of 'effectively computable'. Because several formalisms (Turing machines, lambda calculus, recursive functions) have been proved equivalent, a result established for one holds for all; the shared definition is what makes that transfer routine. Adoption is voluntary; a logician who prefers another formalism restates results at no professional cost.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, mathematical_logicians, beneficiary,
    organized, generational, mobile, global).

% Use the fixed meaning as the baseline for decidability and complexity statements - reductions, hardness results, and separations are all phrased against it. The definition costs them nothing to follow and saves them from re-arguing what 'algorithm' means in every paper. Leaving it would mean relabeling their entire result base.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, theoretical_computer_scientists, beneficiary,
    organized, biographical, mobile, global).

% Maintain machine-checked libraries (proof assistants, verified-compilation stacks) in which the standard definition is encoded. Switching to a different effectiveness notion would orphan those libraries, so they follow the convention even where a constructive variant would suit their philosophy; the switching cost, not any penalty, is what binds them.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, formal_methods_practitioners, beneficiary,
    moderate, biographical, constrained, global).

% Decide which usages appear in the venues and curricula where the definition is transmitted. They ask authors to define terms and gently steer nonstandard uses of 'computable' toward the canonical one; they hold no sanction beyond revision requests, and they themselves gain a stable vocabulary to edit against.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, journal_editors_and_textbook_authors, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(church_turing_thesis__mathematical_definition_reading, journal_editors_and_textbook_authors, beneficiary).

% Work with effectiveness notions tied to constructive proof (e.g., Markov algorithms, BISH conventions) and publish their alternatives in their own venues. They have argued for decades that the canonical stipulation bakes in classical assumptions; the argument is heard but has not moved the default-setting channels - introductory curricula and standard references present the classical stipulation without their amendment.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, constructive_mathematics_schools, excluded,
    organized, generational, mobile, global).

% Study what the thesis-sentence is - definition, empirical law, or epistemic boundary - and how the readings relate. They collect nothing and pay nothing under any of the readings; their seat is analytical, and they are the parties most likely to notice if the definitional reading quietly hardens into something stronger.
narrative_ontology:constraint_stakeholder(church_turing_thesis__mathematical_definition_reading, philosophers_of_computation, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(church_turing_thesis__mathematical_definition_reading, diffuse).
narrative_ontology:fixing_cost_class(church_turing_thesis__mathematical_definition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes one precise referent for 'effectively computable' so that undecidability, incompleteness, and reducibility results can be stated once and applied across every adequate formalism; replaces a family of informal notions (algorithm, effective procedure, mechanical method) with a single canonical term that textbooks, journals, and proof libraries can share.
% TRANSFER_FUNCTION: Nothing material moves. The only thing transferred is default status: the stipulated meaning becomes the assumed referent in publications and curricula, and any deviating usage must announce and defend itself. No money, labor, or resources flow to any party under this reading.
% ABSENT_VOICES: Constructive-mathematics schools would amend the stipulation (their effectiveness notions are proof-tied) and are present in specialist literature but marginal to the channels where the default is set - introductory curricula, standard references, mainstream editorial practice. Educators in adjacent disciplines inherit the term without ever encountering the debate. Hypercomputation researchers would reserve 'computable' differently for physical processes; their objection targets the sibling physical reading more than this one.
% DISAPPEARANCE_RATIONALE: If the stipulation vanished overnight, 'computable' would revert to an ambiguous informal term: undecidability theorems would lose a determinate object, cross-formalism citation would break, and proof-library interfaces would reference an undefined notion. The rearrangement would be shallow and self-healing - the community would re-stipulate, almost certainly identically, within months - but during the gap the shared infrastructure of computability statements would not resolve.
% FOUNDING_PROBLEM: Before 1936, 'effective method' was informal, and Hilbert's Entscheidungsproblem demanded a provable boundary: to show a problem undecidable, one needed an exact characterization of mechanical procedure so that non-existence of an algorithm could be a theorem rather than an impression.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: the Hilbert-Ackermann formulation of the Entscheidungsproblem (1928) and the Goedel incompleteness context predate and independently attest the problem; historians of logic confirm the definition was built to answer it. None of these sources benefits from the convention's persistence.
narrative_ontology:disappearance_verdict(church_turing_thesis__mathematical_definition_reading, world_rearranges).
narrative_ontology:founding_problem_status(church_turing_thesis__mathematical_definition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(church_turing_thesis__mathematical_definition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(church_turing_thesis__mathematical_definition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(church_turing_thesis__mathematical_definition_reading, 0.06, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon is 0.06: the stipulation imposes only a residual rigidity cost - occasional awkwardness fitting classical phrasing to constructive or physical contexts - and nothing is transferred from anyone to anyone. Suppression is 0.04 and is a raw structural value, unscaled by power or scope: it consists of editorial house-style pressure (revision requests toward canonical usage), not coercion; no exit is blocked, and every named party retains a mobile or merely cost-bearing exit. Theater is 0.07: the definition performs real work in every undecidability proof and every complexity statement; almost nothing about its maintenance is ceremonial. Accessibility_collapse is 0.35: the equivalent formalisms remain fully available (adopting any of them IS adopting this convention), and non-equivalent stipulations survive at the margins in the constructive and hypercomputation literatures. Resistance is 0.12: decades of constructive-school objection and philosophical quibbling, never organized opposition. The two tracked series share one six-point grid (1936-2026); both decline gently as the convention settled from a debated proposal into invisible infrastructure. Suppression_requirement is intentionally not tracked: the enforcement picture is static at house-style intensity throughout, which the scalar already captures. No oscillation is present, so no cyclical machinery is invoked.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is muted inside this reading because the arrangement is near-symmetric - that is itself the finding. What divergence exists: the agenda-setter experiences the convention as an administered norm (something steered), beneficiaries experience it as free infrastructure (something used), and the excluded seat experiences it as a closed default-setting conversation (something argued with from outside). The sharp perspectival gaps in this kernel live BETWEEN readings, not within this one: holders of the physical reading experience the same sentence as a bet about nature, and holders of the epistemological reading as a boundary of the provable - those experiences belong to the sibling files.
 *
 * DIRECTIONALITY LOGIC:
 *   All three beneficiary seats derive low directionality (the arrangement subsidizes them with clarity at negligible cost). The agenda-setting seat sits near-symmetric with a slight beneficiary tilt: editors spend a little effort steering usage and receive a stable vocabulary in return. The excluded seat (constructive schools) derives neither subsidy nor charge from THIS convention - they run a parallel stipulation - which is why no victim set is declared and no directionality override is needed: the structural derivation already places every seat correctly. Notably, no seat is trapped or identity_locked; near-universal mobility is precisely why effective extraction stays pinned near the coordination floor despite the definition's ubiquity.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification risk here runs in both directions. Read as a mountain ('computability simply IS Turing-computability'), the convention's contingency disappears and the stipulation masquerades as natural law - hence emerges_naturally stays false and the beneficiaries are declared openly. Read as a snare (editorial steering as coercion), a house-style preference inflates into suppression - hence suppression is authored at 0.04 with the mechanism named. On mandatrophy proper: the founding problem (making undecidability provable) is live, so no sunset applies and none is declared; this is steady-state coordination, not a transitional arrangement. One receipt-surface caveat is recorded deliberately: gain_flow='diffuse' plus fixing_cost='prohibitive' arithmetically resembles the piton cell, but the piton test fails on both prongs - the function is not atrophied (theater 0.07; the definition does load-bearing work everywhere) and the cost asymmetry points the healthy way (nothing is broken; replacement would cost enormously and buy nothing). The cell collision is a labeling artifact, not a decay signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading (mathematical_definition_reading) of the church_turing_thesis kernel; the sibling readings physical_claim_reading and epistemological_boundary_reading instantiate different constraints with different epsilon values and party structures. Which identification does the community''s actual usage support?',
    'Meta-linguistic analysis of how the thesis-sentence is deployed: count contexts where it is invoked as a definition (proofs, textbooks, library documentation) versus as a claim (physics-of-computation papers); whichever identification dominates canonical references resolves the question.',
    'If usage shifts toward the physical or epistemological identification, this constraint dissolves into the corresponding sibling file and its near-zero epsilon, beneficiary structure, and rope classification lapse with it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Which reading of the church_turing_thesis kernel governs: stipulation, physical law, or epistemic boundary.').

omega_variable(
    adequacy_criterion_contentfulness,
    'Does the stipulation''s claimed adequacy to the informal notion of ''algorithm'' make it answerable to anything beyond convention (Kreisel-style sharpening), or is adequacy itself part of the stipulation?',
    'Analyze whether the adequacy criterion could fail - i.e., whether a formalism could be proved equivalent to Turing machines yet rejected as failing to capture effective procedure; if such a case is conceivable and actionable, adequacy carries content.',
    'If adequacy is contentful, epsilon rises above the convention floor and the constraint migrates toward the epistemological_boundary_reading''s territory; if it is stipulative all the way down, this file stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adequacy_criterion_contentfulness, conceptual, 'Whether definitional adequacy smuggles empirical or conceptual content into a supposedly pure stipulation.').

omega_variable(
    rival_stipulation_displacement_risk,
    'Could a rival stipulation of effectiveness (constructive, proof-tied, or oracle-inclusive) displace the canonical definition in curricula and machine-checked libraries?',
    'Track curriculum standards, proof-assistant library interfaces, and textbook editions over time for shifts in which effectiveness notion is presented as the default.',
    'Displacement would raise resistance and suppression above the authored values and stress the rope classification; continued entrenchment confirms it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rival_stipulation_displacement_risk, empirical, 'Risk that the canonical stipulation is displaced by a rival effectiveness convention.').

omega_variable(
    editorial_pressure_classification,
    'Is the editorial steering toward canonical usage ordinary house style or the thin edge of enforced conformity?',
    'Compare revision-request rates and acceptance outcomes for papers using nonstandard computability terminology against matched controls using standard terminology.',
    'A significant differential would raise suppression above 0.04 and warrant re-examination of the voluntary-adoption premise; parity confirms the house-style classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(editorial_pressure_classification, empirical, 'Whether editorial conformity pressure constitutes meaningful suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(church_turing_thesis__mathematical_definition_reading, 1936, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chur_tr_t1936, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1936, 0.16).
narrative_ontology:measurement_basis(chur_tr_t1936, observed).
narrative_ontology:measurement(chur_tr_t1956, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1956, 0.14).
narrative_ontology:measurement_basis(chur_tr_t1956, observed).
narrative_ontology:measurement(chur_tr_t1976, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1976, 0.11).
narrative_ontology:measurement_basis(chur_tr_t1976, observed).
narrative_ontology:measurement(chur_tr_t1996, church_turing_thesis__mathematical_definition_reading, theater_ratio, 1996, 0.09).
narrative_ontology:measurement_basis(chur_tr_t1996, observed).
narrative_ontology:measurement(chur_tr_t2016, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2016, 0.08).
narrative_ontology:measurement_basis(chur_tr_t2016, observed).
narrative_ontology:measurement(chur_tr_t2026, church_turing_thesis__mathematical_definition_reading, theater_ratio, 2026, 0.07).
narrative_ontology:measurement_basis(chur_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(chur_be_t1936, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1936, 0.09).
narrative_ontology:measurement_basis(chur_be_t1936, observed).
narrative_ontology:measurement(chur_be_t1956, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1956, 0.08).
narrative_ontology:measurement_basis(chur_be_t1956, observed).
narrative_ontology:measurement(chur_be_t1976, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1976, 0.07).
narrative_ontology:measurement_basis(chur_be_t1976, observed).
narrative_ontology:measurement(chur_be_t1996, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 1996, 0.065).
narrative_ontology:measurement_basis(chur_be_t1996, observed).
narrative_ontology:measurement(chur_be_t2016, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 2016, 0.06).
narrative_ontology:measurement_basis(chur_be_t2016, observed).
narrative_ontology:measurement(chur_be_t2026, church_turing_thesis__mathematical_definition_reading, base_extractiveness, 2026, 0.06).
narrative_ontology:measurement_basis(chur_be_t2026, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(church_turing_thesis__mathematical_definition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(church_turing_thesis__mathematical_definition_reading, information_standard).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__physical_claim_reading).
narrative_ontology:affects_constraint(church_turing_thesis__mathematical_definition_reading, church_turing_thesis__epistemological_boundary_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Church-Turing thesis' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing one kernel: this file (stipulative definition; epsilon ~0.06; no victims; rope), church_turing_thesis__physical_claim_reading (contingent claim about physical processes; contested epsilon; live empirical stakes), and church_turing_thesis__epistemological_boundary_reading (boundary of provable computation; intermediate epsilon). This reading is upstream in one narrow sense: the definition supplies the fixed referent about which the other two readings dispute, so both siblings cite it while disputing its status. All three files link one another via affects_constraints; measuring epsilon on the union label would average a convention with a bet about nature and produce a value belonging to neither.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
