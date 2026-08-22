% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__remedial_reading, []).

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
 *   constraint_id: equal_protection_commitment__remedial_reading
 *   human_readable: Equal Protection — Remedial (Antisubordination) Reading
 *   domain: constitutional law/political philosophy/social policy
 *
 * SUMMARY:
 *   The Equal Protection Clause is a contested kernel; this story
 *   instantiates its remedial reading — the clause forbids state perpetuation
 *   of caste and permits race-conscious measures to dismantle subordination.
 *   The standing arrangement under contest is that remedial arrangement
 *   itself: a doctrinal regime in which courts police the line between
 *   caste-perpetuating and remedial classifications, state actors operate
 *   race-conscious programs under strict scrutiny, historically subordinated
 *   groups receive access and the anti-caste guarantee, and historically
 *   privileged applicants bear denied access. Assumptions stated: the
 *   interval maps to US constitutional history (t=0 is 1868, ratification;
 *   t=155 is 2023, SFFA); epsilon is authored for this arrangement only, by
 *   this reading's own lights, over the standing arrangement under contest —
 *   never the reading's endorsed alternative and never averaged across
 *   sibling readings. The colloquial label 'equal protection' covers three
 *   structurally distinct claims with different epsilon, different
 *   beneficiaries, and different victims; per the epsilon-invariance
 *   decomposition they are separate stories linked through the network, not
 *   one story with a measurement parameter. KEY AGENTS (by structural
 *   relationship): - supreme_court: Agenda setter (institutional/analytical)
 *   — authoritative interpreter; draws and redraws the remedial line -
 *   remedial_state_actors: Beneficiary with payer exposure
 *   (institutional/constrained) — operates remedial programs under
 *   authorization and litigation risk - historically_subordinated_groups:
 *   Primary beneficiary (organized/constrained) — receives remedial access
 *   and the anti-caste guarantee - historically_privileged_applicants:
 *   Primary target (organized/constrained) — bears denied access under
 *   remedial measures - non_listed_minority_applicants: Excluded voice
 *   (moderate/constrained) — classified without recognized remedial status -
 *   constitutional_scholars: Analytical observer — maps structure and drift,
 *   holds no stake
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, 0.55).
domain_priors:suppression_score(equal_protection_commitment__remedial_reading, 0.48).
domain_priors:theater_ratio(equal_protection_commitment__remedial_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__remedial_reading, "Equal Protection — Remedial (Antisubordination) Reading").
narrative_ontology:topic_domain(equal_protection_commitment__remedial_reading, "constitutional law/political philosophy/social policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__remedial_reading, '1ff62b04-cb4e-4e71-ba9f-de8fab7021a3').
narrative_ontology:cs_kernel_codification('1ff62b04-cb4e-4e71-ba9f-de8fab7021a3', fixed_text).
narrative_ontology:cs_authority_grounding('1ff62b04-cb4e-4e71-ba9f-de8fab7021a3', lineage).
narrative_ontology:cs_interpretation_layer_present('1ff62b04-cb4e-4e71-ba9f-de8fab7021a3').
narrative_ontology:cs_reading_relation('1ff62b04-cb4e-4e71-ba9f-de8fab7021a3', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('1ff62b04-cb4e-4e71-ba9f-de8fab7021a3', equal_protection_commitment__diversity_reading, influences).
narrative_ontology:cs_axiom('1ff62b04-cb4e-4e71-ba9f-de8fab7021a3', foundational, caste_perpetuation_forbidden).
narrative_ontology:cs_axiom_status(caste_perpetuation_forbidden, holdable).
narrative_ontology:cs_axiom_grounding('1ff62b04-cb4e-4e71-ba9f-de8fab7021a3', caste_perpetuation_forbidden, deontological).
narrative_ontology:cs_axiom('1ff62b04-cb4e-4e71-ba9f-de8fab7021a3', foundational, remedial_race_consciousness_permitted).
narrative_ontology:cs_axiom_status(remedial_race_consciousness_permitted, holdable).
narrative_ontology:cs_axiom_grounding('1ff62b04-cb4e-4e71-ba9f-de8fab7021a3', remedial_race_consciousness_permitted, instrumental).
narrative_ontology:cs_reference_frame('1ff62b04-cb4e-4e71-ba9f-de8fab7021a3', reconstruction_antisubordination_framework).
narrative_ontology:cs_drift_state('1ff62b04-cb4e-4e71-ba9f-de8fab7021a3', post_sffa_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('1ff62b04-cb4e-4e71-ba9f-de8fab7021a3', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__remedial_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, historically_subordinated_groups).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, remedial_state_actors).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, historically_privileged_applicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, remedial_state_actors).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, antisubordination_principle).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, caste_abolition_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Equal Protection Clause and decides which state racial classifications stand. Each term brings challenges that let it redraw the line between forbidden and permitted uses of race; its precedents bind every other seat in this story. It cannot leave the adjudication role — its only movement is revising doctrine — and it holds no direct stake in admissions or contracts, but its institutional authority rises and falls with the doctrine it administers.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% State universities, contracting offices, and legislatures that operate race-conscious programs aimed at widening access for historically subordinated groups. The doctrine's remedial permission is what authorizes these programs; without it the same measures would be forbidden. Operating a program also exposes the actor to strict-scrutiny litigation, and several programs have been struck down, so the actor holds authorization and legal risk in the same hand. Exit would mean abandoning the programs and the populations they serve.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, remedial_state_actors, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__remedial_reading, remedial_state_actors, payer).

% Groups whose members were historically excluded or subordinated by state action. They receive access to universities, contracts, and public offices through race-conscious measures, and they hold the clause's guarantee against any state effort to re-subordinate them. They cannot exit the racial classification system that names them; their stake runs entirely through the doctrine's continued recognition that subordination is a constitutional harm.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_subordinated_groups, beneficiary,
    organized, generational, constrained, national).

% Applicants from historically advantaged groups who are denied seats, contracts, or positions that race-conscious measures allocate elsewhere. They bear the doctrine's costs directly and individually. They have organized litigation vehicles that have won narrowing rulings at the Supreme Court, but their exit is limited: wherever they transact with state actors, the doctrine's jurisdiction follows.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_privileged_applicants, payer,
    organized, biographical, constrained, national).

% Applicants from groups the remedial framework does not recognize as beneficiaries — their remedial status is contested or denied, yet the same schemes classify them. They would argue the beneficiary list is under-inclusive and that they bear the burdens of classification without its protections, but the doctrinal conversation is organized around the recognized beneficiary groups and the organized opposition, and they have no settled seat in it.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, non_listed_minority_applicants, excluded,
    moderate, biographical, constrained, national).

% Map the doctrine's structure, trace its movement across eras, and publish the competing readings. They hold no stake in outcomes and no power over the doctrine; their seat is analytical.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__remedial_reading, historically_subordinated_groups).
narrative_ontology:fixing_cost_class(equal_protection_commitment__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state action around a shared rule for racial classification: the state may not act to perpetuate caste, and may act deliberately — by race-conscious means — to dismantle subordination. It solves the collective problem a multiracial polity faces of binding state power against hierarchy while permitting deliberate repair of hierarchy's legacy.
% TRANSFER_FUNCTION: Moves access and opportunity — university admissions, public contracts, public offices — from applicants of historically privileged groups to applicants of historically subordinated groups, through race-conscious measures the doctrine authorizes; and moves adjudicative authority over every such measure to the courts.
% ABSENT_VOICES: Applicants from groups the remedial framework does not recognize would object that the beneficiary list is under-inclusive; they sit outside the doctrinal conversation, which is organized around the recognized beneficiaries and the organized colorblind opposition. Their claims surface only as asymmetric footnotes in strict-scrutiny litigation rather than as a seated voice.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, states could re-entrench racial classifications without doctrinal barrier — the post-1877 record of Black Codes and segregation shows what follows when the guarantee loses enforcement — while every existing remedial program would lose its authorization and collapse into litigation. Both the prohibition and the permission structure would need rebuilding; arrangements across admissions, contracting, and voting rights depend on it.
% FOUNDING_PROBLEM: The Fourteenth Amendment was ratified to abolish the caste system created by slavery: to prevent states from re-subordinating the freed population through Black Codes, segregation, and disenfranchisement. The remedial reading takes that founding problem as the clause's core meaning.
% FOUNDING_PROBLEM_CORROBORATION: Reconstruction-era legislative history (the 39th Congress's debates, the Freedmen's Bureau Act) and the post-1877 record of state Black Codes corroborate that the founding problem was real. Its current status is attested only from within the contest: sociological disparity research (income, incarceration, education) attests 'live' from outside the benefiting parties; the colorblind coalition's litigation record attests 'substantially resolved.' Corroboration exists for the problem's existence; no seat outside the contest is neutral on its status.
narrative_ontology:disappearance_verdict(equal_protection_commitment__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__remedial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_commitment__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__remedial_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_commitment__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.55 sits in the inversion band: the arrangement extracts from whichever seat stands on the wrong side of the remedial line at a given moment — caste-perpetuating state governments during Reconstruction, historically privileged applicants in the program era — so the scalar is high across eras even as its direction reverses. Suppression 0.48 is authored as a raw structural property (only extractiveness is scaled by the engine, via directionality and scope): the arrangement demands continuous active enforcement, since every remedial program draws strict-scrutiny review and every caste-perpetuating measure draws challenge. Theater 0.38: enforcement was materially real at Reconstruction and at Brown, largely performative across the Plessy trough, and a performative share has returned after SFFA as the reading's live operation narrows to scholarship and legacy domains. Accessibility_collapse 0.40: the sibling readings remain fully live positions, so the arrangement collapses few alternatives — which is why resistance (0.70) stays high: an organized colorblind coalition has won the narrowing rulings (Croson, Adarand, SFFA). The measurement series runs on one shared seven-point grid; the trajectory oscillates rather than drifting monotonically, tracking enforcement-capacity cycles (Reconstruction buildup, post-1877 collapse, Brown-era revival, post-Croson narrowing) rather than intermittent reinforcement — the oscillation is a side effect of judicial-composition and political cycles, not itself an extraction mechanism. The base_properties scalars are end-state values measured at the final grid point (post-SFFA). The claimed type is authored from structure — genuine anti-caste coordination plus asymmetric extraction under active enforcement — independently of these metric values.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats should compute differently. From historically privileged applicants' position the arrangement operates as extraction: access denied by a classification they did not consent to and cannot exit. From historically subordinated groups' position the same arrangement operates as protection and repair: an anti-caste guarantee plus remedial access. From remedial state actors' position it is authorization entangled with litigation risk — the same doctrine that permits their programs supplies the standard by which the programs are struck down. From the Court's position it is doctrine to be administered; the seat that draws the line bears neither side's costs directly. The engine computes these per-seat classifications from the structural data; the divergence between them is the measurement this story exists to take, and it is why the expected epsilon band is high despite the arrangement's genuine coordination core.
 *
 * DIRECTIONALITY LOGIC:
 *   historically_subordinated_groups are declared beneficiaries with constrained exit: the arrangement subsidizes them (access plus the anti-caste guarantee) and they cannot leave the classification system that names them, so their derived directionality sits near the beneficiary end and effective extraction inverts into subsidy. remedial_state_actors are beneficiaries with a secondary payer position — authorization flows to them, strict-scrutiny risk flows from them — placing them low-to-mid rather than at the floor. historically_privileged_applicants are declared victims with constrained exit: the costs land on them directly and individually and the doctrine's jurisdiction follows them into any transaction with state actors, placing them near the full-target end and amplifying their effective extraction. The supreme_court administers rather than collects; its mid-range directionality reflects an institutional stake in the doctrine's operation without a seat among either the subsidized or the taxed. No directionality overrides were authored: the beneficiary/victim declarations plus exit options produce the correct structure, and the cross-era inversion is carried by the temporal series rather than by per-agent corrections.
 *
 * MANDATROPHY ANALYSIS:
 *   The remedial reading carries a built-in transitional logic: remediation is justified by the subordination it dismantles, so if subordination ended, the remedial authorization would lose its warrant. The reading thus has an implicit sunset its beneficiaries have no incentive to declare — the seats positioned to declare completion are the seats that benefit from non-completion, and the implicit_sunset_adjudication omega holds that question open. Classifying this arrangement as pure extraction would erase the genuine anti-caste coordination the post-Reconstruction record shows was necessary; classifying it as pure coordination would erase the real, identifiable costs borne by historically privileged applicants. The tangled-rope claim preserves both halves. The R5 fields route the obsolescence question: founding_problem_status is contested rather than dead, so the status-by-verdict mismatch flag should not fire — but if the colorblind coalition's trajectory continues and the remedial arrangement survives only as scholarship and legacy programs, the theater_ratio series (0.22 to 0.38 across the last two grid points) is the early drift signal of a mandate outliving its enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_inversion,
    'This constraint is the remedial_reading of the equal_protection_commitment kernel. Would the sibling readings restructure the beneficiary/victim set so completely that the classification computed here does not describe the arrangement the siblings govern — and where exactly do the readings disagree?',
    'Generate the sibling stories and compare computed per-seat classifications. equal_protection_commitment__colorblind_reading removes remedial beneficiaries entirely (symmetric protection, no transfer, no victim set of denied applicants); equal_protection_commitment__diversity_reading substitutes institutions as beneficiaries and applicants as diversity inputs. The disagreement is located in one structural element: whether the clause permits race-conscious state action at all (colorblind vs. remedial), and if so, what end justifies it (remediation vs. diversity).',
    'If the siblings compute as symmetric coordination while this reading computes as a hybrid with asymmetric extraction, the extraction measured here is reading-relative, not arrangement-relative — and conversely, the colorblind reading''s symmetric framing may conceal extraction the remedial reading makes visible. Cross-reading comparison is the only resolution; no within-reading data settles it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_inversion, conceptual, 'Committer-frame omega: beneficiary/victim structure and epsilon are reading-relative; sibling readings restructure them entirely.').

omega_variable(
    remedial_extraction_justification,
    'Is the burden borne by historically privileged applicants a coordination cost of dismantling subordination, or the construction of a new preference structure that entrenches rather than dismantles?',
    'Longitudinal outcome studies: do race-conscious remedial programs measurably reduce subordination disparities over generational time without entrenching permanent preference? Compare programs with and without sunset or periodic-review mechanisms.',
    'If remediation demonstrably dismantles subordination, the burden weights toward coordination cost and the arrangement sits toward the coordination side of the hybrid; if programs entrench preference, the burden weights toward pure extraction and the hybrid claim fails toward the extractive pole.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedial_extraction_justification, empirical, 'Whether the burden on privileged applicants is coordination cost or new preference formation.').

omega_variable(
    implicit_sunset_adjudication,
    'Does the remedial authorization carry an implicit sunset — and if so, who adjudicates when remediation is complete, given that the seats positioned to declare completion are the seats that benefit from non-completion?',
    'Doctrinal analysis of the tests courts actually apply for remedial necessity (Croson-era strict scrutiny, SFFA-era endpoint reasoning) combined with disparity-trajectory data; identify whether any institution holds both the standing and the incentive to declare completion.',
    'If an implicit sunset exists but cannot be adjudicated, the arrangement is transitional support maintained as permanent by its beneficiaries — the obsolescence risk is structural rather than incidental, and the theater_ratio trajectory becomes the leading indicator.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_sunset_adjudication, conceptual, 'Whether the remedial authorization is transitional and who could declare completion.').

omega_variable(
    enforcement_direction_ambiguity,
    'The strict-scrutiny machinery built to police remedial programs became the instrument that dismantled them (Croson, Adarand, SFFA). Do the suppression_requirement measurements track enforcement of the remedial arrangement or enforcement against it?',
    'Comparative doctrine across jurisdictions and eras: classify each strict-scrutiny application by whether it upheld or struck the remedial measure, then recompute the suppression series direction.',
    'If most enforcement capacity after Croson operated against the reading, the falling suppression tail understates the coercive demand the arrangement still places on its remaining beneficiaries, and the series should be re-read as enforcement-of versus enforcement-against.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_direction_ambiguity, empirical, 'Whether measured enforcement capacity serves the reading or opposes it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__remedial_reading, 0, 155).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_commitment__remedial_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(equa_tr_t0, observed).
narrative_ontology:measurement(equa_tr_t28, equal_protection_commitment__remedial_reading, theater_ratio, 28, 0.42).
narrative_ontology:measurement_basis(equa_tr_t28, observed).
narrative_ontology:measurement(equa_tr_t56, equal_protection_commitment__remedial_reading, theater_ratio, 56, 0.58).
narrative_ontology:measurement_basis(equa_tr_t56, observed).
narrative_ontology:measurement(equa_tr_t86, equal_protection_commitment__remedial_reading, theater_ratio, 86, 0.3).
narrative_ontology:measurement_basis(equa_tr_t86, observed).
narrative_ontology:measurement(equa_tr_t110, equal_protection_commitment__remedial_reading, theater_ratio, 110, 0.22).
narrative_ontology:measurement_basis(equa_tr_t110, observed).
narrative_ontology:measurement(equa_tr_t130, equal_protection_commitment__remedial_reading, theater_ratio, 130, 0.26).
narrative_ontology:measurement_basis(equa_tr_t130, observed).
narrative_ontology:measurement(equa_tr_t155, equal_protection_commitment__remedial_reading, theater_ratio, 155, 0.38).
narrative_ontology:measurement_basis(equa_tr_t155, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_commitment__remedial_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement_basis(equa_be_t0, observed).
narrative_ontology:measurement(equa_be_t28, equal_protection_commitment__remedial_reading, base_extractiveness, 28, 0.32).
narrative_ontology:measurement_basis(equa_be_t28, observed).
narrative_ontology:measurement(equa_be_t56, equal_protection_commitment__remedial_reading, base_extractiveness, 56, 0.28).
narrative_ontology:measurement_basis(equa_be_t56, observed).
narrative_ontology:measurement(equa_be_t86, equal_protection_commitment__remedial_reading, base_extractiveness, 86, 0.48).
narrative_ontology:measurement_basis(equa_be_t86, observed).
narrative_ontology:measurement(equa_be_t110, equal_protection_commitment__remedial_reading, base_extractiveness, 110, 0.52).
narrative_ontology:measurement_basis(equa_be_t110, observed).
narrative_ontology:measurement(equa_be_t130, equal_protection_commitment__remedial_reading, base_extractiveness, 130, 0.55).
narrative_ontology:measurement_basis(equa_be_t130, observed).
narrative_ontology:measurement(equa_be_t155, equal_protection_commitment__remedial_reading, base_extractiveness, 155, 0.55).
narrative_ontology:measurement_basis(equa_be_t155, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_commitment__remedial_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(equa_su_t0, observed).
narrative_ontology:measurement(equa_su_t28, equal_protection_commitment__remedial_reading, suppression_requirement, 28, 0.3).
narrative_ontology:measurement_basis(equa_su_t28, observed).
narrative_ontology:measurement(equa_su_t56, equal_protection_commitment__remedial_reading, suppression_requirement, 56, 0.24).
narrative_ontology:measurement_basis(equa_su_t56, observed).
narrative_ontology:measurement(equa_su_t86, equal_protection_commitment__remedial_reading, suppression_requirement, 86, 0.58).
narrative_ontology:measurement_basis(equa_su_t86, observed).
narrative_ontology:measurement(equa_su_t110, equal_protection_commitment__remedial_reading, suppression_requirement, 110, 0.62).
narrative_ontology:measurement_basis(equa_su_t110, observed).
narrative_ontology:measurement(equa_su_t130, equal_protection_commitment__remedial_reading, suppression_requirement, 130, 0.55).
narrative_ontology:measurement_basis(equa_su_t130, observed).
narrative_ontology:measurement(equa_su_t155, equal_protection_commitment__remedial_reading, suppression_requirement, 155, 0.48).
narrative_ontology:measurement_basis(equa_su_t155, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the equal_protection_commitment kernel (epsilon-invariance principle): the colloquial label 'equal protection' covers three structurally distinct readings with different epsilon, different beneficiary/victim sets, and different failure modes. colorblind_reading: symmetric prohibition, no remedial beneficiaries, no denied-applicant victims. diversity_reading: institutions as beneficiaries, race as one input among many. remedial_reading (this story): subordinated groups as beneficiaries, privileged applicants as victims, high epsilon from the inversion dynamic. The readings are separate constraint stories linked through this network, never one story with a measurement parameter. Doctrinal causality runs both ways: the remedial reading's retreat under strict scrutiny (Croson, Adarand, SFFA) created the operating environment in which the diversity reading rose (Bakke), while the colorblind coalition's narrowing rulings now govern this arrangement's operating space — hence the upstream/downstream edges in both directions across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
