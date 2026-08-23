% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__social_role_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Recognition-Gated Gender Category Membership (Social Role Reading)
 *   domain: social ontology/political philosophy/bioethics
 *
 * SUMMARY:
 *   Under the social-role reading, gender category membership is not fixed at
 *   birth and not secured by private self-declaration: it is constituted by
 *   sustained social performance that draws recognition from others.
 *   Membership is therefore always conditional and always revocable — held
 *   only as long as the performance keeps eliciting uptake. This distributes
 *   gatekeeping across millions of daily micro-interactions rather than
 *   concentrating it in any office, and it prices membership in continuous
 *   labor for anyone whose embodiment does not spontaneously elicit
 *   recognition. Trans women are included conditionally, on passing;
 *   gender-nonconforming cis women discover the test fires on their bodies
 *   too; intersex people are absorbed by early assignment rather than
 *   consulted. This file instantiates ONE reading of the contested kernel
 *   gendered_category_membership; the biological-sex and gender-identity
 *   readings are separate constraint files with their own epsilon, victim
 *   sets, and classifications, and are not folded into this one. Epsilon's
 *   referent is the standing recognition-performative arrangement itself,
 *   assessed by this reading's own lights — not any sibling's endorsed
 *   alternative. KEY AGENTS (by structural relationship): -
 *   gender_conforming_cis_adults: primary beneficiary and distributed
 *   administrator (moderate/mobile) — recognition flows to them unbidden;
 *   they wield it in micro-interactions -
 *   conditionally_passing_trans_individuals: dual-positioned
 *   payer/beneficiary (moderate/identity_locked) — inclusion purchased with
 *   continuous, revocable performance - nonpassing_trans_individuals: primary
 *   target (powerless/trapped) — bears the boundary enforcement without
 *   access to the performance remedy - gender_nonconforming_cis_women:
 *   collateral target (moderate/constrained) — the recognition test fires on
 *   their bodies as well - sex_segregated_service_providers: institutional
 *   administrator (institutional/constrained) — converts distributed
 *   recognition into door-level decisions - intersex_individuals: excluded
 *   voice (powerless/trapped) — the managed counterexample -
 *   human_rights_tribunals: analytical observer (institutional/analytical) —
 *   sees the structure accumulate across cases
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__social_role_reading, 0.45).
domain_priors:suppression_score(gendered_category_membership__social_role_reading, 0.55).
domain_priors:theater_ratio(gendered_category_membership__social_role_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__social_role_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__social_role_reading, "Recognition-Gated Gender Category Membership (Social Role Reading)").
narrative_ontology:topic_domain(gendered_category_membership__social_role_reading, "social ontology/political philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__social_role_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__social_role_reading, '9359fb87-51fe-4e8a-aad3-e2dcdaaa0d30').
narrative_ontology:cs_kernel_codification('9359fb87-51fe-4e8a-aad3-e2dcdaaa0d30', distributed).
narrative_ontology:cs_authority_grounding('9359fb87-51fe-4e8a-aad3-e2dcdaaa0d30', practice).
narrative_ontology:cs_interpretation_layer_present('9359fb87-51fe-4e8a-aad3-e2dcdaaa0d30').
narrative_ontology:cs_reading_relation('9359fb87-51fe-4e8a-aad3-e2dcdaaa0d30', gendered_category_membership__biological_sex_reading, coexists_with).
narrative_ontology:cs_reading_relation('9359fb87-51fe-4e8a-aad3-e2dcdaaa0d30', gendered_category_membership__gender_identity_reading, influences).
narrative_ontology:cs_axiom('9359fb87-51fe-4e8a-aad3-e2dcdaaa0d30', foundational, recognition_constitutes_membership).
narrative_ontology:cs_axiom_status(recognition_constitutes_membership, holdable).
narrative_ontology:cs_axiom_grounding('9359fb87-51fe-4e8a-aad3-e2dcdaaa0d30', recognition_constitutes_membership, conventional).
narrative_ontology:cs_axiom('9359fb87-51fe-4e8a-aad3-e2dcdaaa0d30', secondary, sustained_performance_accrual).
narrative_ontology:cs_axiom_status(sustained_performance_accrual, holdable).
narrative_ontology:cs_axiom_grounding('9359fb87-51fe-4e8a-aad3-e2dcdaaa0d30', sustained_performance_accrual, conventional).
narrative_ontology:cs_reference_frame('9359fb87-51fe-4e8a-aad3-e2dcdaaa0d30', sustained_performative_recognition).
narrative_ontology:cs_drift_state('9359fb87-51fe-4e8a-aad3-e2dcdaaa0d30', contemporary_visibility_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9359fb87-51fe-4e8a-aad3-e2dcdaaa0d30', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__social_role_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, gender_conforming_cis_adults).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, nonpassing_trans_individuals).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, conditionally_passing_trans_individuals).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, gender_nonconforming_cis_women).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, conditionally_passing_trans_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Move through gendered social life without ever having to establish membership: their presentation draws immediate correct address, facility access, and unmarked belonging. They also administer the standard in thousands of unnoticed micro-decisions — pronoun choices, corrections, compliments, doorway judgments — without any body ever convening to set rules. Leaving the arrangement would mean trading effortless legibility for nothing, so exit never comes up.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_conforming_cis_adults, beneficiary,
    moderate, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__social_role_reading, gender_conforming_cis_adults, agenda_setter).

% Hold recognized membership only while a continuous performance — voice, dress, movement, paperwork, history management — keeps drawing others' uptake. Inclusion is real and valued: correct address, safer facility use, ordinary belonging. But any single interaction can expose the performance and revoke the recognition, so the work never stops. Stopping the performance would collapse the membership they live through day to day, which makes quitting unthinkable rather than merely costly.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, conditionally_passing_trans_individuals, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__social_role_reading, conditionally_passing_trans_individuals, beneficiary).

% Cannot secure stable recognition however long the performance runs: address stays wrong, facilities stay hazardous, belonging stays provisional. They carry the arrangement's boundary enforcement — challenge, scrutiny, correction — while being unable to purchase what the performance buys others. There is nowhere to stand outside the sorting: every interaction re-runs the test.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, nonpassing_trans_individuals, payer,
    powerless, biographical, trapped, global).

% Tall, broad, flat-chested, or otherwise masculine-presenting cis women whom the recognition test flags anyway: challenged entering women's changing rooms, questioned at shelter doors, asked to account for themselves in spaces they have always used. They can spend money and discomfort modifying presentation, or absorb repeated public challenge. Their exposure shows the test runs on performance signals rather than on anything they chose.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_nonconforming_cis_women, payer,
    moderate, biographical, constrained, national).

% Shelters, prisons, sports governing bodies, and facility operators turn a distributed social standard into door-level decisions, one user at a time. Whichever way they decide, they absorb litigation risk, guidance churn, press attention, and user conflict. They cannot decline to decide: statute, funders, and users force a judgment call at every threshold.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, sex_segregated_service_providers, agenda_setter,
    institutional, generational, constrained, national).

% Born with variations the two-category sorting has no clean answer for, they were typically assigned early and expected to perform the assignment quietly. When communities argue about who counts, they are discussed more than consulted. Their existence is the standing edge case the arrangement manages rather than hears.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, intersex_individuals, excluded,
    powerless, generational, trapped, global).

% Adjudicate membership disputes case by case — facility access, documentation, sports eligibility — and their rulings feed back into provider guidance and public expectation. They watch the whole structure accumulate across cases without collecting from it or bearing its daily costs.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, human_rights_tribunals, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__social_role_reading, gender_conforming_cis_adults).
narrative_ontology:fixing_cost_class(gendered_category_membership__social_role_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Shared gender categories let strangers coordinate interaction without negotiation: pronoun selection, facility allocation, address forms, dress expectations, and kinship terms are resolved by a common classification. Recognition-based membership supplies that classification through observable performance, making category assignment fast and mutually checkable.
% TRANSFER_FUNCTION: Moves recognition goods — access to sex-segregated spaces, correct address, freedom from challenge, unmarked belonging — to those whose sustained performance secures others' uptake; moves continuous performance labor and exposure-to-challenge onto those whose embodiment does not spontaneously elicit recognition; and concentrates membership-judging authority in the recognizing crowd.
% ABSENT_VOICES: Intersex individuals, whose bodily variation the arrangement absorbs by early assignment rather than consultation, and nonpassing trans people in communities where their testimony carries no weight precisely because they lack the recognition the arrangement distributes — the gatekeeper's veto extends to the gatekept's account of the gate.
% DISAPPEARANCE_RATIONALE: Every gendered arrangement — facilities, documents, forms of address, dress codes, sports categories, kinship terminology — presupposes a working membership criterion. Overnight removal would force case-by-case renegotiation of all of them, and the recognitional habits of millions of daily interactions would lose their object.
% FOUNDING_PROBLEM: Coordinating social roles and expectations across a population with varied bodies, in societies organized around a two-category division of labor, kinship, and reproduction — strangers needed a fast, shared way to sort interaction partners.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological and historical scholarship outside the beneficiary set attests both the ubiquity of gender-sorting coordination problems and the deep variability of membership criteria across societies and eras — variability that corroborates the coordination need while undercutting any claim that this particular recognition arrangement is its necessary solution. No source outside the benefiting parties attests that the current criteria are fixed; the benefiting majority are their principal attestors.
narrative_ontology:disappearance_verdict(gendered_category_membership__social_role_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__social_role_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__social_role_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gendered_category_membership__social_role_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__social_role_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

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
 *   Claim and metrics are authored independently. The claimed type is tangled_rope because the structure possesses all three canonical elements: a genuine coordination function (shared categories resolve pronouns, facilities, address, and dress expectations once socially instead of per-encounter), asymmetric extraction (continuous performance labor and challenge exposure fall on those whose embodiment does not spontaneously elicit recognition, while effortless legibility flows to those it does), and active enforcement (distributed social sanctioning, increasingly codified in provider guidance and case law). Metrics are authored descriptively: epsilon 0.45 is low-to-moderate because the arrangement delivers the coordinated good to anyone who meets the criterion and the criterion is in principle open, yet it prices membership in unending labor for one seat and denies it outright to another; suppression 0.55 reflects diffuse but pervasive sanctioning with no central police; theater_ratio 0.30 is low-moderate because much gendered performance is constitutive rather than decorative, while a growing share of justification talk ('safety', 'fairness') performs reassurance over decisions that run on recognition habit; accessibility_collapse 0.45 because alternatives (self-declaration norms, identity-first address) exist and spread in enclaves — understanding the arrangement does not close the exits; resistance 0.60 because sustained contestation from trans advocacy, gender-critical countermovements, and intersex objection keeps the criterion politically alive. Suppression is authored as a raw structural property; only extractiveness gets scaled by directionality and scope in the engine's computation. The temporal series run on one shared grid (points 0–30 at step 5) so every tracked metric is authored at every examined time point; the rising suppression_requirement series tracks real enforcement-capacity change — institutional codification hardening as self-declaration alternatives spread — not mere extraction drift.
 *
 * PERSPECTIVAL GAP:
 *   From the conforming member's seat the arrangement is invisible infrastructure — coordination that simply works, experienced as rope. From the conditionally passing seat it is a job that never ends, with membership revocable at any exposed interaction. From the nonpassing seat it is a wall administered politely, experienced as trap. Same structure, three experienced types; the engine computes this divergence per seat from the structural data, and the divergence — not any single verdict — is the finding. The service-provider seat adds a fourth reading: an administrator absorbing litigation and reputational whiplash whichever way it decides, collecting none of the gains.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declaration places gender_conforming_cis_adults near the beneficiary end (low d, subsidized by the arrangement they also administer — their dual beneficiary/agenda_setter position is the distributed-gatekeeping structure itself). The victim declarations place the three payer seats near the target end, with exit modulation doing real work: nonpassing trans individuals are trapped (damped nothing, amplified fully), conditionally passing trans individuals are identity_locked (the performance is constitutive of the membership they live through, so exit is unthinkable rather than merely costly), and nonconforming cis women are constrained (presentation modifiable at cost). The dual seat derives mid-range, which is correct. One override is authored: sex_segregated_service_providers carry no beneficiary/victim declaration, so derivation would hand them the institutional power-atom fallback; their actual position sits target-side of center (d=0.35) because they absorb the arrangement's conflict costs — litigation, guidance churn, user confrontation — while collecting none of its gains. The override touches the institutional atom generally; the only other institutional agent is the tribunal observer, whose analytical exit removes it from the extraction computation.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the structure as pure rope would erase the conditional, revocable character of trans inclusion and the challenge exposure of nonconforming cis women — the coordination story would launder the performance levy. Reading it as pure snare would erase the genuine coordination delivered to everyone who meets the criterion and the criterion's openness in principle — the extraction story would deny that shared categories solve a real problem. Tangled rope holds both halves: coordination function real, extraction asymmetric, enforcement continuous. On genealogy: the founding coordination problem (sorting interaction partners across bodily diversity) is still live even though its current solution is contested, so no mandatrophy resolution is declared and no zombie flag is invited — the mismatch consumer should find status=contested paired with verdict=world_rearranges, a live dispute rather than a dead mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which constitutive criterion — immutable biological markers, self-declared identity, or sustained performance with others'' recognition — actually fixes gender category membership, and does fixing it differently change who counts?',
    'Comparative institutional analysis of jurisdictions and communities operating each criterion: track membership disputes, exclusion incidents, and litigation outcomes under each regime.',
    'Each sibling reading produces a structurally different constraint: a biological-marker reading concentrates exclusion categorically on trans people; an identity reading relocates gatekeeping disputes to bad-faith-declaration policing; this recognition reading distributes both exclusion risk and performance burden across everyday interaction. Cross-reading comparison belongs to the engine layer over the linked sibling files, not inside this story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'This constraint is one reading of the gendered_category_membership kernel; the sibling readings are separate constraint files with their own epsilon and victim sets.').

omega_variable(
    victim_structure_symmetry,
    'Do trans individuals and gender-nonconforming cis women bear the arrangement''s costs symmetrically, or does one seat dominate the harm distribution?',
    'Incident reporting on facility challenges, misrecognition rates, and discrimination complaints disaggregated by seat.',
    'If cis women''s challenge exposure rivals trans exclusion, the arrangement''s costs are broader than a trans-focused framing suggests and cross-seat resistance coalitions become likelier; if trans harms dominate, the arrangement operates closer to targeted exclusion than distributed gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_structure_symmetry, empirical, 'Whether the ambiguous victim structure splits evenly between trans individuals and nonconforming cis women.').

omega_variable(
    passing_resource_stratification,
    'How strongly does success under the recognition criterion track resources — wealth, transition timing, body morphology, geographic mobility — rather than effort or duration of performance?',
    'Outcome data on recognition success correlated with socioeconomic and medical-access variables.',
    'High resource-tracking would make the arrangement doubly stratifying (membership by class as well as by performance), raising effective extraction above the authored scalar and sharpening the asymmetry between the two trans seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(passing_resource_stratification, empirical, 'Whether the performance criterion functions as a class filter.').

omega_variable(
    enforcement_locus_drift,
    'Is gatekeeping genuinely distributed across social interactions, or is it concentrating in institutional codification — provider guidance, case law, documentation rules?',
    'Track the share of membership disputes resolved by institutional ruling versus interpersonal judgment over the interval.',
    'Concentration would push suppression above the authored scalar and shift the arrangement toward centrally enforced operation; continued diffusion keeps enforcement cheap, deniable, and hard to litigate against.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_locus_drift, empirical, 'Locus of enforcement: distributed interactional gatekeeping versus institutional codification.').

omega_variable(
    suppression_internalization_split,
    'How much of the measured suppression is structural (door policies, document checks, public challenge norms) versus internalized (anticipatory self-policing of presentation that persists where rules relax)?',
    'Post-relaxation trajectory studies: in jurisdictions adopting self-declaration, measure whether anticipatory performance labor persists among people already socialized under recognition norms.',
    'If internalization dominates, effective suppression exceeds the structural measure and outlives formal reform — targets carry the gatekeeper with them after exit; if structural dominates, rule change rapidly lowers suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized suppression mechanism in a distributed social constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__social_role_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gcm_social_role_tr_t0, gendered_category_membership__social_role_reading, theater_ratio, 0, 0.24).
narrative_ontology:measurement(gcm_social_role_tr_t5, gendered_category_membership__social_role_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(gcm_social_role_tr_t10, gendered_category_membership__social_role_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(gcm_social_role_tr_t15, gendered_category_membership__social_role_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement(gcm_social_role_tr_t20, gendered_category_membership__social_role_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(gcm_social_role_tr_t25, gendered_category_membership__social_role_reading, theater_ratio, 25, 0.29).
narrative_ontology:measurement(gcm_social_role_tr_t30, gendered_category_membership__social_role_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(gcm_social_role_be_t0, gendered_category_membership__social_role_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(gcm_social_role_be_t5, gendered_category_membership__social_role_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(gcm_social_role_be_t10, gendered_category_membership__social_role_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(gcm_social_role_be_t15, gendered_category_membership__social_role_reading, base_extractiveness, 15, 0.41).
narrative_ontology:measurement(gcm_social_role_be_t20, gendered_category_membership__social_role_reading, base_extractiveness, 20, 0.43).
narrative_ontology:measurement(gcm_social_role_be_t25, gendered_category_membership__social_role_reading, base_extractiveness, 25, 0.44).
narrative_ontology:measurement(gcm_social_role_be_t30, gendered_category_membership__social_role_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gcm_social_role_su_t0, gendered_category_membership__social_role_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(gcm_social_role_su_t5, gendered_category_membership__social_role_reading, suppression_requirement, 5, 0.44).
narrative_ontology:measurement(gcm_social_role_su_t10, gendered_category_membership__social_role_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(gcm_social_role_su_t15, gendered_category_membership__social_role_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement(gcm_social_role_su_t20, gendered_category_membership__social_role_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(gcm_social_role_su_t25, gendered_category_membership__social_role_reading, suppression_requirement, 25, 0.53).
narrative_ontology:measurement(gcm_social_role_su_t30, gendered_category_membership__social_role_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__social_role_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__gender_identity_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'what makes someone a man or a woman' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints: biological-marker constitution, identity self-declaration, and performative recognition. Each has its own epsilon, victim set, enforcement surface, and classification; measuring membership by one criterion yields a different constraint than measuring it by another. This file authors the recognition reading only. The upstream/downstream texture runs both ways — biological essentialism is cited against recognition-gating, and identity-based law erodes it — so the family is linked pairwise rather than ranked; cross-reading comparison belongs to the engine layer over the linked files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gendered_category_membership__social_role_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
