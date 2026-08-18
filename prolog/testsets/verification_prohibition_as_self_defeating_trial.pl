% ============================================================================
% CONSTRAINT STORY: verification_prohibition_as_self_defeating_trial
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_verification_prohibition_as_self_defeating_trial, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: verification_prohibition_as_self_defeating_trial
 *   human_readable: The Look-Back Prohibition (Verification-Forbidden Trial Structure)
 *   domain: folk-legal/ritual/narrative
 *
 * SUMMARY:
 *   Across widely separated folk-legal and mythic traditions recurs a
 *   distinctive trial structure: the petitioner succeeds only by refraining
 *   from the single act — looking back, checking, asking, confirming — that
 *   would tell them whether they are succeeding. Because no other evidentiary
 *   channel exists, the trial does not merely test resolve against
 *   pre-existing doubt; its design manufactures the doubt it then treats as
 *   the petitioner's failing. This story treats the structural pattern itself
 *   as the constraint (not any one tale's plot), authored at the level where
 *   the rule recurs across the corpus of trial-narratives and where a durable
 *   custodian/narrative-authority class administers and retells it. The
 *   primary observable is the ratio of trials failed specifically via
 *   petitioner-initiated verification (as opposed to failures from other
 *   causes) against the theoretical success rate absent the manufactured
 *   doubt, together with whether any alternative evidence channel is ever
 *   offered.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(verification_prohibition_as_self_defeating_trial, 0.81).
domain_priors:suppression_score(verification_prohibition_as_self_defeating_trial, 0.72).
domain_priors:theater_ratio(verification_prohibition_as_self_defeating_trial, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(verification_prohibition_as_self_defeating_trial, extractiveness, 0.81).
narrative_ontology:constraint_metric(verification_prohibition_as_self_defeating_trial, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(verification_prohibition_as_self_defeating_trial, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(verification_prohibition_as_self_defeating_trial, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(verification_prohibition_as_self_defeating_trial, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(verification_prohibition_as_self_defeating_trial, snare).
narrative_ontology:human_readable(verification_prohibition_as_self_defeating_trial, "The Look-Back Prohibition (Verification-Forbidden Trial Structure)").
narrative_ontology:topic_domain(verification_prohibition_as_self_defeating_trial, "folk-legal/ritual/narrative").

domain_priors:requires_active_enforcement(verification_prohibition_as_self_defeating_trial).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(verification_prohibition_as_self_defeating_trial, ritual_custodian_class).
narrative_ontology:constraint_beneficiary(verification_prohibition_as_self_defeating_trial, narrative_authority_lineage).
narrative_ontology:constraint_victim(verification_prohibition_as_self_defeating_trial, petitioner_class_across_generations).
narrative_ontology:constraint_vindicates(verification_prohibition_as_self_defeating_trial, trial_by_faith_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Undertakes the trial (the folkloric structure recurs across Orpheus-and-Eurydice-type tales, faith ordeals, and vow-of-silence tests): the sole condition for success is refraining from the one act — looking back, asking, checking, confirming — that would verify the condition holds. Every channel of evidence about whether the trial is succeeding runs through the forbidden act itself. The petitioner is asked to sustain certainty without ever being permitted the operation that would produce certainty, and the accumulated doubt this manufactures is what most often triggers the forbidden act and the trial's failure. Exit is effectively impossible: refusing the trial forfeits its outcome by default (the person, object, or standing waited on is lost regardless); the only paths through are compliance with an unfalsifiable rule or the sanctioned failure the rule is built to eventually produce.
narrative_ontology:constraint_stakeholder(verification_prohibition_as_self_defeating_trial, petitioner_class_across_generations, payer,
    powerless, generational, trapped, local).

% Composes, transmits, and adjudicates the trial's rule across retellings and enactments. Determines what counts as 'looking back,' when the trial began and ended, and whether a given failure was 'truly' the forbidden act or something excusable. Because the rule structurally guarantees a nonzero failure rate independent of petitioner virtue, the custodian class always has a supply of cautionary failures to cite as proof the rule is necessary and the trial's stakes are real. Never itself subject to the trial; never required to hold doubt without verification.
narrative_ontology:constraint_stakeholder(verification_prohibition_as_self_defeating_trial, ritual_custodian_class, agenda_setter,
    institutional, civilizational, arbitrage, regional).

% The tellers, priests, and moral instructors who use the trial's recurring failures as the exemplary lesson about faith, patience, or discipline. Their authority is renewed each time a petitioner fails, since the failure is read as evidence of human weakness rather than as evidence of the rule's design. They benefit from the trial's persistence as a teaching structure regardless of whether any given petitioner succeeds.
narrative_ontology:constraint_stakeholder(verification_prohibition_as_self_defeating_trial, narrative_authority_lineage, beneficiary,
    institutional, civilizational, arbitrage, continental).

% The rare petitioners who complete the trial without breaking the prohibition. Their success is cited by the custodian class as proof the rule is fair and passable, but they have no voice in how the rule is transmitted afterward or whether the built-in doubt-manufacture that nearly broke them is ever acknowledged as structural rather than personal failing.
narrative_ontology:constraint_stakeholder(verification_prohibition_as_self_defeating_trial, successful_petitioner_exemplars, excluded,
    powerless, biographical, trapped, local).

% Comparative mythologists and legal anthropologists who study the recurrence of the verification-prohibition structure across unrelated cultures and note the structural peculiarity: the sole evidentiary channel for confirming the condition is the same act that voids it. They can document the failure ratio and the absence of alternative evidence channels but hold no power to alter the rule's transmission.
narrative_ontology:constraint_stakeholder(verification_prohibition_as_self_defeating_trial, folklore_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its most charitable reading, the trial purports to coordinate trust-building under uncertainty — training the petitioner to act on sustained commitment without continuous external confirmation, a virtue genuinely useful in contexts where verification is costly or destabilizing.
% TRANSFER_FUNCTION: Moves standing, favor, a beloved person, or restored status from the withheld outcome to the petitioner ONLY on completion, while moving narrative capital and adjudicative authority to the custodian and narrative-authority classes on every retelling of a failure — success is rare and privately held, failure is frequent, public, and institutionally reusable.
% ABSENT_VOICES: The petitioners who failed are the only ones with direct testimony about what the doubt actually felt like from inside the trial, and they are structurally silenced by the trial's own logic — a failed petitioner has, by definition, broken the rule and forfeited standing to describe what the rule cost them; their account is absorbed into the cautionary-tale genre rather than treated as evidence against the rule's design.
% DISAPPEARANCE_RATIONALE: If the verification-prohibition structure vanished — replaced by trials that permit an independent evidentiary channel — the custodian and narrative-authority classes would lose their primary renewable proof-text (failures) and would need a different mechanism to teach the same virtue; petitioners would gain the ability to check their standing at low cost, eliminating the largest single cause of trial failure, which is not misconduct but doubt manufactured by the absence of any check.
% FOUNDING_PROBLEM: In contexts where continuous verification is genuinely destructive to the thing being verified (trust that dissolves under surveillance, a rescue that fails if the rescuer looks back too soon), a rule against verification-seeking behavior encodes a real epistemic-practical tradeoff: some outcomes are only achievable by NOT checking.
% FOUNDING_PROBLEM_CORROBORATION: The custodian and narrative-authority classes attest the founding problem is still live — that unverifiable trust remains meaningful precisely because it withstands doubt. Folklore analysts and successful petitioner exemplars, from outside the benefiting classes, attest that in the vast majority of documented retellings no alternative evidentiary channel was ever offered even in principle, which suggests the rule was never calibrated to a genuine tradeoff but designed to guarantee the doubt it punishes; independent comparative-mythology scholarship supports the shifted-function reading.
narrative_ontology:disappearance_verdict(verification_prohibition_as_self_defeating_trial, world_rearranges).
narrative_ontology:founding_problem_status(verification_prohibition_as_self_defeating_trial, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(verification_prohibition_as_self_defeating_trial, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-07-25',
    'uke_narrative', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'temperature=0.2,max_tokens=8192').
narrative_ontology:story_seed(verification_prohibition_as_self_defeating_trial, 'none', 1).
narrative_ontology:epsilon_provenance(verification_prohibition_as_self_defeating_trial, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(verification_prohibition_as_self_defeating_trial_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(verification_prohibition_as_self_defeating_trial, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(verification_prohibition_as_self_defeating_trial_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high and rising (0.60 to 0.81) because the structure's cost to petitioners compounds across retellings: each generation inherits the same doubt-manufacturing design with no accumulated fix, while the custodian class's authority is progressively reinforced by the growing catalogue of citable failures. Theater ratio is authored as substantial and rising (0.32 to 0.58) because an increasing share of the trial's apparent solemnity — its warnings, its liturgical framing, its cautionary retellings — serves to dramatize and naturalize the prohibition rather than to test any genuine virtue; the ritual performance around the trial grows even as the trial's actual discriminating power (separating worthy from unworthy petitioners) does not improve, since the forbidden act is the ONLY channel and structurally guarantees noise. Suppression rises in tandem (0.50 to 0.72) as the custodian class's interpretive authority over what counts as 'looking back' hardens into settled doctrine, foreclosing petitioner appeals to ambiguity.
 *
 * PERSPECTIVAL GAP:
 *   From the custodian seat, the trial is a coordination device teaching a real virtue (sustained trust without continuous confirmation) and its failures validate the lesson. From the petitioner seat, the same structure is an unfalsifiable extraction: no confirmation is ever available, so the accumulating doubt is not a personal failing but a designed feature, and failure is punished identically whether the underlying condition would have held or not. The engine's per-seat computation should register this divergence structurally: the custodian's institutional power, civilizational time horizon, and arbitrage exit place it near the beneficiary end of directionality, while the petitioner's powerless, trapped, generational position places it near the full-target end.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (ritual_custodian_class, narrative_authority_lineage) hold institutional power, arbitrage exit, and long time horizons — they administer and retell the rule but are never themselves subject to it, so directionality derives near the beneficiary end. Petitioner_class_across_generations is powerless, trapped (refusing the trial forfeits the outcome by default), and generational — directionality derives near the full-target end. Successful_petitioner_exemplars are excluded rather than coordinated: their rare successes are appropriated as proof-of-fairness without giving them any voice in the rule's future transmission, which is why they are marked excluded rather than beneficiary despite having 'won.'
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that some trust-based outcomes are only achievable without continuous verification — may once have named a real epistemic tradeoff worth encoding into a trial. But the corroboration record shows the rule was essentially never paired with any alternative evidentiary channel, even a costly or delayed one, across the documented corpus. That absence is the tell: a genuine coordination device solving 'verification is destructive here' would eventually generate SOME confirmatory alternative (a trusted witness, a delayed check, a partial signal) as the practice matured. Its total absence across generations indicates the doubt-manufacture is not incidental cost but the load-bearing mechanism — hence snare rather than tangled_rope. Classifying it as snare rather than mountain prevents the trial from being mistaken for an inevitable feature of trust itself; classifying it as snare rather than rope prevents its coordination-flavored justification from being taken as evidence against the asymmetric extraction the structural data show.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_tradeoff_vs_manufactured_doubt,
    'Is there any documented instance of the verification-prohibition trial being paired with a genuine, low-cost alternative evidence channel that petitioners were permitted to use without voiding the trial?',
    'Systematic comparative-folklore survey across independent cultural traditions cataloguing whether any variant of the trial structure ever offers a sanctioned alternative confirmation method, and whether such variants show measurably lower forbidden-act failure rates.',
    'If genuine alternative-channel variants exist and show lower failure rates, the base structure without such channels is more clearly pure extraction (supporting snare); if no variant ever offers an alternative, the near-universality of the pure form suggests the doubt-manufacture is structurally intrinsic to the trial''s cultural function, strengthening the snare classification further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_tradeoff_vs_manufactured_doubt, empirical, 'Whether any documented trial variant offers a non-forbidden verification channel.').

omega_variable(
    petitioner_agency_vs_designed_trap,
    'Should the trial be read as testing petitioner virtue (a real, if harsh, coordination function) or as a designed trap where failure is structurally overdetermined regardless of petitioner conduct?',
    'Formal analysis of the failure-rate distribution: if failures cluster near points of maximal induced uncertainty (e.g., just before an expected but withheld confirmation) rather than uniformly across the trial''s duration, this supports the designed-trap reading over the virtue-test reading.',
    'A designed-trap finding would push the classification more firmly toward snare with high confidence; a genuine virtue-test finding (uniform failure distribution driven by petitioner variation in resolve) would push toward tangled_rope, since a real coordination function would then coexist with the harsh cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(petitioner_agency_vs_designed_trap, conceptual, 'Whether failure timing patterns indicate designed doubt-manufacture versus genuine virtue-testing.').

omega_variable(
    cross_cultural_naturalness_claim,
    'Does the structure''s recurrence across unrelated cultures indicate it emerges naturally from universal features of trust and uncertainty (a mountain-like claim some custodians make), or does it indicate convergent institutional design serving similar custodian-class interests across societies?',
    'Compare societies with strong centralized ritual-custodian classes against societies with weak or absent such classes, controlling for trial-narrative prevalence and structure.',
    'If the pattern correlates strongly with custodian-class strength rather than appearing independent of institutional structure, this undermines any natural-law framing and supports the snare classification; if it appears independent of custodian strength, a rope or mountain reading becomes more defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_cultural_naturalness_claim, conceptual, 'Whether cross-cultural recurrence indicates natural emergence or convergent institutional design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(verification_prohibition_as_self_defeating_trial, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(veri_tr_t0, verification_prohibition_as_self_defeating_trial, theater_ratio, 0, 0.32).
narrative_ontology:measurement(veri_tr_t8, verification_prohibition_as_self_defeating_trial, theater_ratio, 8, 0.38).
narrative_ontology:measurement(veri_tr_t16, verification_prohibition_as_self_defeating_trial, theater_ratio, 16, 0.45).
narrative_ontology:measurement(veri_tr_t24, verification_prohibition_as_self_defeating_trial, theater_ratio, 24, 0.5).
narrative_ontology:measurement(veri_tr_t32, verification_prohibition_as_self_defeating_trial, theater_ratio, 32, 0.54).
narrative_ontology:measurement(veri_tr_t40, verification_prohibition_as_self_defeating_trial, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(veri_be_t0, verification_prohibition_as_self_defeating_trial, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(veri_be_t8, verification_prohibition_as_self_defeating_trial, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(veri_be_t16, verification_prohibition_as_self_defeating_trial, base_extractiveness, 16, 0.71).
narrative_ontology:measurement(veri_be_t24, verification_prohibition_as_self_defeating_trial, base_extractiveness, 24, 0.75).
narrative_ontology:measurement(veri_be_t32, verification_prohibition_as_self_defeating_trial, base_extractiveness, 32, 0.78).
narrative_ontology:measurement(veri_be_t40, verification_prohibition_as_self_defeating_trial, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(veri_su_t0, verification_prohibition_as_self_defeating_trial, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(veri_su_t8, verification_prohibition_as_self_defeating_trial, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(veri_su_t16, verification_prohibition_as_self_defeating_trial, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(veri_su_t24, verification_prohibition_as_self_defeating_trial, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(veri_su_t32, verification_prohibition_as_self_defeating_trial, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(veri_su_t40, verification_prohibition_as_self_defeating_trial, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(verification_prohibition_as_self_defeating_trial, identity_coordination).
narrative_ontology:boltzmann_floor_override(verification_prohibition_as_self_defeating_trial, 0.08).
narrative_ontology:affects_constraint(verification_prohibition_as_self_defeating_trial, trial_by_ordeal_evidentiary_structure).
narrative_ontology:affects_constraint(verification_prohibition_as_self_defeating_trial, faith_based_healing_prohibition_on_doubt).

% DUAL FORMULATION NOTE:
% This story isolates the verification-prohibition structural pattern as its own constraint, distinct from any single narrative instantiation (Orpheus/Eurydice, folk vow-of-silence tales, faith-ordeal structures). Sibling constraints that share the doubt-punishment mechanism but differ in evidentiary structure (e.g., trials with third-party witnesses) should be authored separately and linked here rather than folded into a single averaged ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(verification_prohibition_as_self_defeating_trial, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
