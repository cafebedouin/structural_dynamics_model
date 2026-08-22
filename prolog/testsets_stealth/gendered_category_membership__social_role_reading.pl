% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__social_role_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__social_role_reading, []).

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
 *   constraint_id: gendered_category_membership__social_role_reading
 *   human_readable: Gendered Category Membership — Social Role Reading (Sustained Performance and Recognition Test)
 *   domain: social ontology/political philosophy/bioethics
 *
 * SUMMARY:
 *   In the social-role reading of gendered category membership, a person
 *   counts as a woman or a man insofar as they sustainably perform the
 *   category's local repertoire and are recognized by others as doing so.
 *   Membership is therefore an achievement maintained interaction by
 *   interaction rather than a birth fact or a private declaration: every
 *   stranger who reads your presentation casts a micro-vote, and the
 *   aggregate of those votes is your standing. The arrangement solves a real
 *   coordination problem — strangers can sort one another instantly, with no
 *   registry — while imposing a continuous performance tax whose burden
 *   concentrates on those whose performance is costly or ambiguous: trans
 *   women whose recognition is unstable, and cis women whose presentation
 *   departs from local feminine standards. Gatekeeping is radically
 *   distributed; there is no office to petition and no rulebook to amend,
 *   only the accumulated sanctioning habits of everyone already securely
 *   inside. This file instantiates one reading of a contested kernel; the
 *   sibling readings are separate constraint files, and the committer
 *   structure is recorded in the omega variables and kernel_context rather
 *   than folded into this constraint's metrics. KEY AGENTS (by structural
 *   relationship): - reliably_performing_members: Primary beneficiary
 *   (moderate/constrained) — sustained adequate performance buys frictionless
 *   recognition - established_category_gatekeepers: Distributed agenda-setter
 *   (moderate/constrained) — administers the criterion through everyday
 *   sanctioning while collecting smooth belonging - non_passing_trans_women:
 *   Primary target (powerless/identity_locked) — bears conditional, revocable
 *   inclusion - gender_nonconforming_cis_women: Secondary target
 *   (moderate/constrained) — bears challenge risk despite birth
 *   categorization - nonbinary_refusers_of_category: Excluded voice
 *   (powerless/trapped) — the binary recognition regime offers them no seat -
 *   social_ontology_observers: Analytical observer (analytical/analytical) —
 *   sees the full distributed structure
 *
 * KEY AGENTS:
 *   - reliably_performing_members: Primary beneficiary (moderate/constrained) — their native performance earns default recognition in nearly every interaction
 *   - established_category_gatekeepers: Distributed agenda-setter with beneficiary secondary role (moderate/constrained) — applies the test case by case through sanctioning habits absorbed rather than written
 *   - non_passing_trans_women: Primary target (powerless/identity_locked) — every new interaction re-runs the admission test on them
 *   - gender_nonconforming_cis_women: Secondary target (moderate/constrained) — birth categorization gives them no protection under this reading's test
 *   - nonbinary_refusers_of_category: Excluded (powerless/trapped) — would renegotiate the two-box structure itself but hold no seat from which to
 *   - social_ontology_observers: Analytical observer (analytical/analytical) — documents how the test works, what it costs, and how it drifts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__social_role_reading, 0.46).
domain_priors:suppression_score(gendered_category_membership__social_role_reading, 0.58).
domain_priors:theater_ratio(gendered_category_membership__social_role_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__social_role_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__social_role_reading, "Gendered Category Membership — Social Role Reading (Sustained Performance and Recognition Test)").
narrative_ontology:topic_domain(gendered_category_membership__social_role_reading, "social ontology/political philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__social_role_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__social_role_reading, '7a7fe6b0-27c4-4950-b722-42df3144cd5c').
narrative_ontology:cs_kernel_codification('7a7fe6b0-27c4-4950-b722-42df3144cd5c', distributed).
narrative_ontology:cs_authority_grounding('7a7fe6b0-27c4-4950-b722-42df3144cd5c', practice).
narrative_ontology:cs_interpretation_layer_present('7a7fe6b0-27c4-4950-b722-42df3144cd5c').
narrative_ontology:cs_reading_relation('7a7fe6b0-27c4-4950-b722-42df3144cd5c', gendered_category_membership__biological_sex_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a7fe6b0-27c4-4950-b722-42df3144cd5c', gendered_category_membership__gender_identity_reading, influences).
narrative_ontology:cs_axiom('7a7fe6b0-27c4-4950-b722-42df3144cd5c', foundational, recognition_by_others_constitutive).
narrative_ontology:cs_axiom_status(recognition_by_others_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('7a7fe6b0-27c4-4950-b722-42df3144cd5c', recognition_by_others_constitutive, conventional).
narrative_ontology:cs_axiom('7a7fe6b0-27c4-4950-b722-42df3144cd5c', secondary, sustained_performance_maintains_membership).
narrative_ontology:cs_axiom_status(sustained_performance_maintains_membership, holdable).
narrative_ontology:cs_axiom_grounding('7a7fe6b0-27c4-4950-b722-42df3144cd5c', sustained_performance_maintains_membership, conventional).
narrative_ontology:cs_reference_frame('7a7fe6b0-27c4-4950-b722-42df3144cd5c', interactional_recognition_regime).
narrative_ontology:cs_drift_state('7a7fe6b0-27c4-4950-b722-42df3144cd5c', contemporary_self_id_and_bio_revival_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7a7fe6b0-27c4-4950-b722-42df3144cd5c', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__social_role_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, reliably_performing_members).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, established_category_gatekeepers).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, non_passing_trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, gender_nonconforming_cis_women).
narrative_ontology:constraint_vindicates(gendered_category_membership__social_role_reading, social_kinds_constituted_by_collective_recognition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Move through everyday life in ways that reliably trigger recognition from strangers and acquaintances alike: their dress, speech, manner, and bodily presentation match what their local community reads as their category. They rarely think about membership because interactions confirm it by default. What flows to them is frictionless processing — correct forms of address, uncontested access to gendered spaces and services, belonging that requires no defense. What flows from them is the performance itself: continuous, mostly unremarked upkeep of presentation, and participation in the sanctioning that greets those who perform less convincingly. Exit would mean deliberately breaking performance and absorbing the resulting loss of recognition; most never consider it.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, reliably_performing_members, beneficiary,
    moderate, biographical, constrained, global).

% Long-standing members whose own membership has never been questioned and who therefore hold, in every interaction, a small piece of the recognition authority: the raised eyebrow, the corrected pronoun, the denied entry, the warm welcome that confirms a newcomer. No committee assigns this role; it accrues to anyone securely inside. They administer the criterion by applying it case by case, guided by local norms they absorbed rather than wrote, and they collect the same frictionless belonging other reliable performers receive. Their horizon extends to transmission — teaching children, correcting peers — and exit looks like the same costly self-disruption it does for any member.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, established_category_gatekeepers, agenda_setter,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__social_role_reading, established_category_gatekeepers, beneficiary).

% Live as women while their presentation does not reliably trigger recognition from strangers; every new interaction re-runs the admission test. What flows to them is conditional belonging: accepted where they pass, challenged, excluded, or endangered where they do not, with the verdict made afresh by whoever they meet. What flows from them is intensive performance labor — voice training, dress, grooming, constant self-monitoring — aimed at stabilizing a recognition that others grant or withhold. Leaving the pursuit of recognition would mean surrendering the life they are building as themselves; staying means the test never ends. Community networks among trans women soften isolation but cannot confer the recognition that only interacting strangers can.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, non_passing_trans_women, payer,
    powerless, biographical, identity_locked, global).

% Were categorized female at birth and live as women, but their appearance or manner departs from local feminine performance standards — short hair, masculine dress, muscular builds, indifference to makeup. Under a membership test run on performance and recognition, their birth categorization does not protect them: they report being challenged in women's restrooms, stared at, questioned, or refused service. What flows to them is partial belonging that must occasionally be defended; what flows from them is either costly additional performance or the vigilance of anticipating challenge. Their exit option is assimilating their presentation at real psychological cost; some instead seek out communities with looser recognition standards.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_nonconforming_cis_women, payer,
    moderate, biographical, constrained, global).

% Reject membership in both categories and ask instead to be read outside the binary. The recognition regime they live under has no seat for that request: every interaction pushes them toward one box or the other, and their refusals read, to gatekeepers, as failed performance rather than principled exit. What flows to them is chronic misrecognition; what flows from them is correction labor — repeated explanations, chosen-pronoun assertions — that the surrounding practice is not built to honor. They are affected by the criterion's operation but hold no position inside it from which the criterion could be renegotiated.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, nonbinary_refusers_of_category, excluded,
    powerless, biographical, trapped, global).

% Philosophers, sociologists, and historians who study how gender categories are constituted and maintained. They take testimony from all the other seats, compare recognition practices across cultures and eras, and publish analyses of how the performance-and-recognition test works, what it costs, and how it changes. They collect nothing from the arrangement and bear none of its sanctions; their stake is analytic.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, social_ontology_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__social_role_reading, diffuse).
narrative_ontology:fixing_cost_class(gendered_category_membership__social_role_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Shared, mutually legible gender categories let strangers coordinate on first contact — forms of address, facility use, dress expectations, deference patterns — without negotiation or record-keeping. The performance-and-recognition test produces this legibility locally: anyone can assess it, no central registry is needed, and the assessment travels with the person.
% TRANSFER_FUNCTION: Moves recognition — and the belonging, safety, and smooth interaction that ride on it — to those who sustain adequate performance; moves performance labor and vigilance onto members, concentrated on those whose performance is costly or ambiguous; and moves sanctioning attention from the member-mass onto those who fail or refuse the test.
% ABSENT_VOICES: Nonbinary and agender people who decline both categories are structurally absent: the regime offers no seat from which to bargain, so their objection — that the entire recognition apparatus presupposes two boxes — registers only as individual noncompliance. Voices questioning the recognition test itself, rather than particular outcomes, appear mainly in academic venues far from everyday gatekeeping.
% DISAPPEARANCE_RATIONALE: If the performance-and-recognition test vanished overnight, every gendered practice that relies on it — restroom and facility sorting, address forms, dress codes, sports and shelter classifications, deference habits — would lose its working admission filter. Either another criterion (birth markers or self-declaration) would replace it and the world would rearrange around that test, or gendered sorting would dissolve into case-by-case negotiation; either way, the current arrangement of billions of daily interactions reorganizes.
% FOUNDING_PROBLEM: Pre-modern societies needed to sort persons into two legally and ritually consequential categories — marriage, inheritance, labor division, religious office — without documents or biological testing. Sustained performance plus community recognition was the available verification technology: you knew someone's place by how they lived and how others treated them.
% FOUNDING_PROBLEM_CORROBORATION: Historians of gender and of law attest the documentary-verification genealogy from outside any beneficiary seat; sociologists of everyday interaction attest that face-to-face sorting still runs on performance cues that paperwork does not reach; trans-studies and disability scholars attest the costs the test imposes. No party inside the regime's beneficiary set originated these accounts.
narrative_ontology:disappearance_verdict(gendered_category_membership__social_role_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__social_role_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__social_role_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gendered_category_membership__social_role_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__social_role_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__social_role_reading_tests).
:- end_tests(gendered_category_membership__social_role_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.46 — low-to-moderate: the performance tax is real and continuous, but most members pay it cheaply because their performance is native, and the first-contact coordination return is genuine. Suppression (0.58) is moderate and decentralized: nothing formally bars departure, but departure forfeits recognition, and recognition is the currency of ordinary social life; the sanctioning that enforces the test is real but distributed, so no single lever disables it. Theater (0.32) is moderate-low: much enforcement is functional legibility work, but a growing share is boundary performance — publicized challenges, symbolic rule-making — that maintains the test's aura more than its output. Accessibility collapse (0.48): alternatives persist — affirming subcultures, self-ID jurisdictions, online communities — but none replaces the test in mainstream face-to-face life. Resistance (0.62) is high: trans-liberation organizing, deliberate misperformance, and scholarly demolition of the test's pretense to naturalness all contest it openly. The measurement series share one grid (T=0..30, six points, every tracked metric authored at every point). Base extractiveness declines through a liberalizing stretch, then upticks as boundary scrutiny remobilizes; theater rises monotonically as maintenance grows more symbolic; suppression_requirement traces a U — enforcement capacity decayed for roughly two decades, then was deliberately rebuilt — which is why the story tracks enforcement capacity temporally instead of leaving suppression static. Suppression here is mixed structural and internalized; the split is carried by its omega variable rather than forced into the scalar. Claim and metrics are authored independently: the tangled-rope claim states the structure I believe true, and the engine computes per-seat types from the data.
 *
 * PERSPECTIVAL GAP:
 *   The gatekeeper seat and the payer seats compute different types from identical structure: from inside, the test feels like the water everyone swims in — a working coordination device; from the non-passing trans seat, the same test is a recurring examination with stakes attached to every encounter. The two victim seats diverge further: a gender-nonconforming cis woman can rhetorically appeal to sibling criteria (her birth categorization) when challenged, even though this reading gives that appeal no force — a refuge trans members lack under every sibling except the identity reading. Identity-lock dynamics bind the trans seats: the category being sought is constitutive of the self pursuing it, so exit from the test would mean social death of the affirmed self; the lock is strongest early in transition and loosens for members whose recognition has been stable for years — the constraint binds a cohort and a career stage, not a fixed property. Coalition potential converts some individual powerlessness: trans advocacy networks pool individually powerless seats into organized voice, which is part of why resistance is authored high despite powerless targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Reliably performing members are declared beneficiaries: the test subsidizes them with default recognition, so their derived directionality sits near the beneficiary pole. Established gatekeepers are agenda-setters who also collect — administering the test costs them little and secures their own standing — keeping them near the beneficiary pole as well. Non-passing trans women are declared victims with identity-locked exit and no power: they sit nearest the full-target pole, and the engine amplifies their effective extraction accordingly. Gender-nonconforming cis women are victims with moderate power and constrained exit: targeted, but less absolutely, since costly assimilation remains available to them. Nonbinary refusers are excluded rather than coordinated: the test has no seat for them, and their exclusion marks the enforcement surface's edge rather than a position inside it. No directionality overrides are authored: the beneficiary/victim declarations plus the power and exit atoms already produce the intended spread.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — sorting persons into two legally and ritually consequential categories without records — is partially solved (bureaucracy now carries formal classification) but still live at the scale where the test actually operates: face-to-face contact, where documents do not travel. Because the problem's status is contested rather than dead, the arrangement is not mandatrophy-resolved, and the classification resists two symmetrical errors: reading the distributed sanctioning as mere custom (which would flatten the constraint into a benign coordination device and miss the conditional-membership burden on trans and nonconforming members), and reading the performance tax as pure extraction (which would erase the genuine first-contact coordination the test delivers). The tangled-rope claim keeps both halves on the table; the engine's per-seat computation decides how the halves distribute across seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the social_role_reading of the gendered_category_membership kernel. What changes structurally if a community adopts the biological_sex_reading (membership fixed by immutable birth markers) or the gender_identity_reading (membership fixed by self-declaration) instead?',
    'Compare jurisdictions and communities that have shifted between readings: the victim set, the enforcement surface, and the performance-cost burden all move with the adopted reading.',
    'Under the biological reading, trans women are excluded categorically and cis women are secure; under the identity reading, inclusion is unconditional and performance costs largely vanish; under this reading, inclusion is conditional and costs concentrate on non-passing trans members and nonconforming cis women. Beneficiary/victim structure and epsilon are reading-relative; cross-reading comparison must use the linked sibling files, not this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this constraint is one of three readings of the gendered-category kernel; siblings are separate constraint files.').

omega_variable(
    victim_asymmetry_ambiguity,
    'Do trans members facing unstable recognition and gender-nonconforming cis women bear this constraint''s costs symmetrically, or does one group absorb systematically more?',
    'Disaggregated incident data on challenge, exclusion, and violence events at gendered boundaries, sorted by group and setting.',
    'If burdens are strongly asymmetric, the derived directionality for the heavier-burdened seat approaches the full-target pole and the constraint''s overall profile shifts toward the extractive end; if roughly symmetric, the ambiguous-victim structure authored here stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_asymmetry_ambiguity, empirical, 'Whether the two declared victim seats bear equal or asymmetric exclusion risk.').

omega_variable(
    passing_default_vs_affirmative_endorsement,
    'Is inclusion granted by unmarked default when performance passes, or does it require affirmative endorsement from others — and do the two mechanisms bind different communities?',
    'Ethnographic comparison across communities: most everyday settings run on unmarked default acceptance; some trans-affirming and religious communities require explicit affirmation of membership.',
    'Affirmative-endorsement settings require continuous active enforcement and extract more per interaction; default settings enforce only at the margin. The actively-enforced reading of this constraint holds robustly only where endorsement is affirmative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(passing_default_vs_affirmative_endorsement, conceptual, 'Whether the recognition gate operates as silent default or as demanded endorsement.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression carried by others'' sanctioning (structural) or by members'' own internalized vigilance that persists where sanctioning stops?',
    'Post-exit trajectory: members who relocate to affirming communities — if performance-vigilance and anticipatory fear persist after external sanctioning ends, a substantial share is internalized.',
    'If largely internalized, effective suppression exceeds the structural measure and exit does not release members; the finding routes through this omega rather than rewriting the declared scalar directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in the recognition regime.').

omega_variable(
    distributed_fixability,
    'With gatekeeping distributed across millions of interactors, does any seat exist that could cheaply fix or dissolve the performance-and-recognition test, or is fixing prohibitive for every candidate?',
    'Historical case studies of rapid localized norm shifts (institutional pronoun adoption, workplace dress-code liberalization) contrasted with population-scale persistence of the test.',
    'Demonstrated cheap local fixes would revise the fixing-cost classification downward at institutional scale; population-scale persistence supports the prohibitive classification authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributed_fixability, conceptual, 'Whether any seat can cheaply alter a radically distributed gatekeeping arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__social_role_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gcm_social_role_tr_t0, gendered_category_membership__social_role_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(gcm_social_role_tr_t6, gendered_category_membership__social_role_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(gcm_social_role_tr_t12, gendered_category_membership__social_role_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(gcm_social_role_tr_t18, gendered_category_membership__social_role_reading, theater_ratio, 18, 0.29).
narrative_ontology:measurement(gcm_social_role_tr_t24, gendered_category_membership__social_role_reading, theater_ratio, 24, 0.31).
narrative_ontology:measurement(gcm_social_role_tr_t30, gendered_category_membership__social_role_reading, theater_ratio, 30, 0.32).

% Extraction over time
narrative_ontology:measurement(gcm_social_role_be_t0, gendered_category_membership__social_role_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(gcm_social_role_be_t6, gendered_category_membership__social_role_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(gcm_social_role_be_t12, gendered_category_membership__social_role_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(gcm_social_role_be_t18, gendered_category_membership__social_role_reading, base_extractiveness, 18, 0.44).
narrative_ontology:measurement(gcm_social_role_be_t24, gendered_category_membership__social_role_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(gcm_social_role_be_t30, gendered_category_membership__social_role_reading, base_extractiveness, 30, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(gcm_social_role_su_t0, gendered_category_membership__social_role_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(gcm_social_role_su_t6, gendered_category_membership__social_role_reading, suppression_requirement, 6, 0.59).
narrative_ontology:measurement(gcm_social_role_su_t12, gendered_category_membership__social_role_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(gcm_social_role_su_t18, gendered_category_membership__social_role_reading, suppression_requirement, 18, 0.54).
narrative_ontology:measurement(gcm_social_role_su_t24, gendered_category_membership__social_role_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(gcm_social_role_su_t30, gendered_category_membership__social_role_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__social_role_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__gender_identity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'who counts as a woman or a man' decomposes into three structurally distinct constraints sharing one kernel: biological_sex_reading (costs concentrated in categorical exclusion of trans members; cis members secure), social_role_reading (this file; conditional inclusion, distributed performance tax), and gender_identity_reading (minimal performance cost; inclusion by declaration). Each file authors its own epsilon, beneficiaries, and victims; the files are linked pairwise through affects_constraints. Historical ordering: the biological reading supplied the background against which the distributed performance-and-recognition test operated; this reading now supplies the terms of contest against the identity reading's self-declaration premise, which is why the reading relation to the identity sibling is influences rather than bare coexistence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
