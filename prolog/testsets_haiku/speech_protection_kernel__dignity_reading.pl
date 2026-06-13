% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__dignity_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: speech_protection_kernel__dignity_reading
 *   human_readable: Speech Protection Conditional on Equal Dignity Maintenance (Dignity Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The dignity reading instantiates one pole of a contested constitutional
 *   kernel: whether speech protection is categorically guaranteed or
 *   conditional on not functioning as subordination of groups. Under this
 *   reading, the First Amendment (or constitutional speech guarantees in
 *   other democracies) protects speech as a means of individual and political
 *   expression, but not speech that functions as structural subordination —
 *   the systematic degradation of a group's political, social, or economic
 *   standing through linguistic/communicative acts. Group-targeting speech
 *   (hate speech, group libel, dehumanization campaigns) that denies target
 *   groups' equal dignity is unprotected. This reading conflicts with the
 *   absolutist reading (speech is categorically protected) and influences the
 *   harm-threshold and marketplace readings (which acknowledge harms but
 *   debate the mechanism). The claim/metric divergence is deliberate:
 *   extraction appears high (0.62) because enforcement requires courts to
 *   restrict speech — a straightforward transfer of authorial freedom from
 *   speaker to institutional adjudication — but the constraint's coordination
 *   function is real (it solves the problem of how equal-dignity polities
 *   manage speech that subordinates). This is a tangled_rope: the
 *   coordination is genuine, the extraction asymmetric, and enforcement
 *   active.
 *
 * KEY AGENTS:
 *   - target_group_members (identity-locked, moderate power, excluded from exit): beneficiaries; protected from subordinating speech
 *   - absolute_speech_advocates (powerful, biographical horizon, constrained exit): payers; lose doctrinal certainty that absolutism governs
 *   - marginalized_publishers_with_group_targeting_intent (powerless, trapped exit, local scope): payers; speech acts foreclosed
 *   - constitutional_courts (institutional power, analytical exit, national scope): agenda-setters; adjudicate subordination boundary and enforce equal-dignity test
 *   - absolutist_constitutional_tradition (analytical, civilizational horizon): excluded; the reading's core axiom rejects its foundational claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, 0.62).
domain_priors:suppression_score(speech_protection_kernel__dignity_reading, 0.71).
domain_priors:theater_ratio(speech_protection_kernel__dignity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__dignity_reading, "Speech Protection Conditional on Equal Dignity Maintenance (Dignity Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__dignity_reading, "constitutional/political").

domain_priors:requires_active_enforcement(speech_protection_kernel__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__dignity_reading, '3f7949a2-3cda-4445-900b-3d8049087eca').
narrative_ontology:cs_kernel_codification('3f7949a2-3cda-4445-900b-3d8049087eca', formalized).
narrative_ontology:cs_authority_grounding('3f7949a2-3cda-4445-900b-3d8049087eca', lineage).
narrative_ontology:cs_interpretation_layer_present('3f7949a2-3cda-4445-900b-3d8049087eca').
narrative_ontology:cs_reading_relation('3f7949a2-3cda-4445-900b-3d8049087eca', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('3f7949a2-3cda-4445-900b-3d8049087eca', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f7949a2-3cda-4445-900b-3d8049087eca', speech_protection_kernel__marketplace_reading, influences).
narrative_ontology:cs_reading_relation('3f7949a2-3cda-4445-900b-3d8049087eca', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('3f7949a2-3cda-4445-900b-3d8049087eca', foundational, group_dignity_is_indivisible_from_individual_dignity).
narrative_ontology:cs_axiom_status(group_dignity_is_indivisible_from_individual_dignity, holdable).
narrative_ontology:cs_axiom_grounding('3f7949a2-3cda-4445-900b-3d8049087eca', group_dignity_is_indivisible_from_individual_dignity, deontological).
narrative_ontology:cs_axiom('3f7949a2-3cda-4445-900b-3d8049087eca', foundational, speech_functioning_as_subordination_is_unprotected).
narrative_ontology:cs_axiom_status(speech_functioning_as_subordination_is_unprotected, holdable).
narrative_ontology:cs_axiom_grounding('3f7949a2-3cda-4445-900b-3d8049087eca', speech_functioning_as_subordination_is_unprotected, empirically_contingent).
narrative_ontology:cs_reference_frame('3f7949a2-3cda-4445-900b-3d8049087eca', constitutional_equal_dignity_primacy).
narrative_ontology:cs_drift_state('3f7949a2-3cda-4445-900b-3d8049087eca', contemporary_pluralist_democracies, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('3f7949a2-3cda-4445-900b-3d8049087eca', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__dignity_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, target_group_members).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, constitutional_courts).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, absolute_speech_advocates).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, marginalized_publishers_with_group_targeting_intent).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, group_dignity_is_indivisible_from_individual_dignity).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, subordination_is_a_speech_effect).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain protection from speech that functions to subordinate their group as a class — hate speech, group libel, systematic dehumanization campaigns. Their group membership is not chosen; exit from the group is not a realistic option. The reading asserts their dignity as group members is inseparable from their dignity as individuals; speech that structures their systematic subordination contradicts their equal standing in the political community. They benefit from the constraint without bearing its administrative cost; courts adjudicate on their behalf.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, target_group_members, beneficiary,
    moderate, generational, identity_locked, national).

% Face the constraint that their argument — that listener harm never justifies restriction — is not the governing rule under this reading. They lose the doctrinal certainty that all speech criticism (even group targeting) is protected; instead, protection is conditional on not functioning as subordination. Their ability to advocate the absolutist position itself is protected (courts do not restrict absolutist meta-discourse), but its adoption as governing doctrine is foreclosed by this reading's core axiom. They are powerful enough to mount sustained legal challenges, but constrained by the doctrine's institutional embedding.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, absolute_speech_advocates, payer,
    powerful, biographical, constrained, national).

% Are prevented from publishing content that targets specific groups as inferior, subhuman, or threats to the body politic — the speech acts this reading explicitly unprotects. They have no alternative channel (trapped exit, small reach, no institutional backing). Their speech act is foreclosed, not merely disfavored. The power asymmetry (powerless actor targeting speech acts forbidden by institutional authority) creates structural suppression. They lack resources to challenge the constraint in court; their speech is simply not tolerated.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, marginalized_publishers_with_group_targeting_intent, payer,
    powerless, biographical, trapped, local).

% Adjudicate which speech is group-targeting, which is subordinating, and which falls outside protection — a necessarily contentious and iterative process. They carry the enforcement machinery: determining the boundary between protected criticism of groups and unprotected group targeting; applying the equal-dignity test to fact-patterns; interpreting what constitutes functional subordination versus disagreement. Their authority grounds itself in the constitutional text's equal-protection and dignity commitments, not in speaker autonomy. They have no personal stake in the outcome; their role is technical enforcement of a doctrine they did not choose.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% The intellectual tradition grounded in categorical speech protection is structurally absent from the dignity-reading framework — the reading's core axiom directly rejects its foundational claim that speaker autonomy is categorically protected. A constitutional tradition that adopted the absolutist reading would not be here; this reading's emergence presupposes that absolutism has been contested and partially overridden by the equal-dignity axiom. Absolutist scholars and jurists would argue that group dignity can be protected through counter-speech and civil-rights enforcement, not through speech restriction.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, absolutist_constitutional_tradition, excluded,
    analytical, civilizational, analytical, national).

% Have no individual representation in the constraint structure but are the category-level beneficiaries whose dignity maintenance is the constraint's justification. Their structural position (targets of potential subordinating speech, unable to exit group status) is the reason the constraint exists, but they do not sit as named agenda-setters or enforcers — the constraint is enforced by courts on their collective behalf, creating an agency asymmetry. Their benefit depends on correct judicial identification of subordinating speech, which is empirically uncertain and contention-prone.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, hate_speech_target_groups, observer,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__dignity_reading, constitutional_courts).
narrative_ontology:fixing_cost_class(speech_protection_kernel__dignity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables equal political community membership by preventing speech that functions as systematic subordination of groups within that community. Solves the coordination problem of how to maintain the common status-floor below which no group can be driven by speech acts alone, thereby preserving the political equality necessary for deliberation between groups.
% TRANSFER_FUNCTION: Transfers the authority to restrict certain speech acts (group-targeting, subordinating speech) from speaker discretion to courts applying an equal-dignity test. The restriction moves protection from maximalist speaker autonomy to conditioned speaker autonomy, trading speech-reach for equal-group-standing in the political order.
% ABSENT_VOICES: Speakers intending group subordination are structurally excluded from the protection boundary; their argument would be that speech targeting groups is political expression protected categorically. Minority groups who might argue for stronger proactive protections (not just exclusion of hate speech, but affirmative speech guarantees) are also absent from the enforcement structure — enforcement is reactive to speech acts, not constructive of alternative speech capacity.
% DISAPPEARANCE_RATIONALE: If the dignity-conditional constraint vanished overnight, the political order's asymmetry would invert: group-targeting speech that was blocked would become available; equal-dignity claims would lose their doctrinal anchor in speech law; groups that depended on protection from subordinating speech would have to rely on social counter-speech or exit (which they cannot exercise because group membership is identity-locked). The political community's self-conception as a space of equal membership would be structurally altered.
% FOUNDING_PROBLEM: Speech freedom in pluralist societies created a coordination problem: maximizing speaker freedom (absolutist reading) enables systematic subordination of minority groups through speech, undermining their equal membership in the political community. The dignity reading solves this by making equal dignity — not speech autonomy — the foundational value, rendering protection conditional.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts across democracies with equal-dignity provisions (Canada, Germany, South Africa, European Union) attest the problem is live: speech targeting groups as inferior threatens political equality. Absolutist advocates (US First Amendment scholars, libertarian speech theorists) attest the founding problem is overstated and the cure (restricting speech) worse than the disease. International human rights bodies and critical race theorists outside the speech-law establishment corroborate that group-targeting speech functions as subordination; conservative legal scholars and marketplace-of-ideas theorists dispute both the mechanism and the remedy.
narrative_ontology:disappearance_verdict(speech_protection_kernel__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__dignity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__dignity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__dignity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness of 0.62 reflects the straightforward restriction of speaker liberty: speech that would be protected under absolutism is unprotected under dignity. The measurement series show extraction rising from 0.38 (early phases, doctrine not yet settled, speaker uncertainty) to 0.62 (mature enforcement, boundaries more established). Theater_ratio rises from 0.22 to 0.48, indicating increasing performative enforcement: courts must articulate why a particular speech act functions as subordination (a complex, iterative, partly rhetorical task), and enforcement increasingly involves public messaging about group dignity rather than simple suppression. Suppression_requirement rises from 0.54 to 0.71, indicating that maintaining the constraint requires active court engagement with each borderline case — passive rules cannot sort subordinating from non-subordinating speech. The coercion grid shows asymmetric dynamics across levels: class-level suppression (targeting entire groups' exposure to subordinating speech) is higher throughout than individual-level suppression (individual speakers subject to restriction); structural-level resistance is highest (the constitutional order itself is built on equal-dignity values), while organizational-level resistance is lower (media organizations, academic institutions sometimes defend speakers against the dignity constraint). The imbalance suggests the constraint operates more cleanly at system level (constitutional legitimacy) than at ground level (actual enforcement).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (constitutional courts) computes this as a real coordination mechanism solving a genuine problem: how to preserve equal dignity while permitting speech. The court's seat has high resistance to the constraint (structural legitimacy, institutional backing, doctrinal coherence) and analytical exit (no personal stake in speaker liberty). The payer seats (absolutist advocates, marginalized publishers) compute it as pure extraction: their speech liberty is transferred to courts without compensation, and the courts' authority is opaque/iterative (no bright-line rule for what subordinates). Marginalized publishers, in particular, face trapped exit and powerless positioning: the constraint forecloses their speech without alternative channels. Absolute-speech advocates face constrained exit: they can still advocate absolutism, but cannot win its adoption as doctrine. Target-group members occupy a beneficiary seat but with identity-locked exit: their benefit is passive (protection from others' speech) rather than active (they cannot exit group status even if the protection fails). The engine's per-seat computation should show courts and target-group seats estimating lower extraction/higher coordination, while payer seats estimate higher extraction. The gap reflects structural asymmetry: the constraint solves a collective-action problem for groups (how to be equally heard despite asymmetric speech power) by restricting individuals (speakers). From individual speaker perspective, it is pure extraction; from group perspective, it is coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Target-group members have directionality near 0.3 (net beneficiaries under this reading): they gain protection from subordinating speech, no extraction cost to them personally. Their identity_locked exit means the protection cannot be easily bypassed — they are locked into the group structure the dignity constraint protects. Absolute-speech advocates have directionality near 0.7 (net targets): they lose the doctrinal certainty that speech is categorically protected; their constrained exit means they can still write and speak, but not under the frame they prefer (absolutism is foreclosed). Marginalized publishers with group-targeting intent have directionality near 0.85 (near-full targets): their speech is foreclosed; they have trapped exit (no alternative publishing venue); they are powerless in relation to courts. Constitutional courts sit near 0.5 (symmetric by design): they enforce the constraint but do not profit from it; their institutional power is used on behalf of both coordination (equal dignity) and extraction (restricting speaker liberty). The directionality derivation chain runs: beneficiary/victim + exit → d. Target groups benefit + identity-locked → low d. Payer groups (absolute advocates) pay + constrained exit → moderate-high d. Publisher groups pay + trapped exit + powerless → very high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem is live: political communities continue to experience the coordination problem of how to preserve equal dignity when some speech functions to subordinate groups. However, mandatrophy is possible: if empirical evidence showed that speech restriction does not reduce group subordination (that courts use equal-dignity doctrine to suppress minoritarian voices more than group-targeting speakers), the founding problem would shift from 'live' to 'dead' while enforcement persists. The theater_ratio's rise to 0.48 suggests some degree of performative enforcement: courts increasingly narrate why a speech act subordinates rather than simply suppressing it, which could be either genuine evolution in doctrine (lower theater, higher functionality) or defensive theater-building (higher theater, lower functionality). The mandatrophy signal would trigger if: (1) founding_problem_status shifts to 'dead' (the group-subordination problem is solved by alternative means like affirmative speech policies) while the restriction persists; (2) theater_ratio rises above 0.6 (performative maintenance dominates actual function); (3) enforcement becomes visibly asymmetric (courts restrict marginalized speech more than group-targeting speech). Current measurements do not yet signal mandatrophy, but the rising extraction and theater suggest drift toward it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subordination_vs_offense_boundary,
    'What empirically distinguishes speech that functions as structural subordination of a group from speech that is merely offensive or disagreeable to group members?',
    'Comparative case law analysis across jurisdictions: identify patterns in when courts find subordination vs. offense; interdisciplinary evidence (sociology, critical theory, speech-act analysis) on the difference between group targeting that degrades standing and group targeting that provokes disagreement.',
    'If the boundary is unstable or vague, the constraint''s enforcement becomes highly contentious and potentially arbitrary (suppression rises, theater_ratio rises as enforcement becomes performative). If stable, enforcement can be more predictable and the constraint''s extraction component may be more clearly separable from its coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_vs_offense_boundary, empirical, 'Whether subordination is empirically distinguishable from offense in speech effects.').

omega_variable(
    equal_dignity_axiom_foreclosure_scope,
    'Does the equal-dignity axiom (this reading''s foundational claim) logically foreclose all sibling readings, or only the absolutist reading?',
    'Formal logical analysis: test whether harm_threshold_reading, democratic_participation_reading, and marketplace_reading can coexist with equal-dignity as foundational commitments in a single constitutional framework, or whether they require denial of group-dignity primacy.',
    'If equal dignity forecloses only absolutism, the other readings coexist with this one; if it forecloses all others, this reading''s kernel is a single-modal attractor (extreme rarity). The resolution determines whether the constraint is part of a genuine kernel contest or occupies the only defensible position within its constitutional tradition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(equal_dignity_axiom_foreclosure_scope, conceptual, 'Logical scope of the equal-dignity axiom''s foreclosure.').

omega_variable(
    identity_lock_vs_choice_in_group_membership,
    'For which groups does this reading treat membership as identity-locked (unchosen, non-exit-able), and for which does it treat it as more fluid?',
    'Case-by-case analysis of court decisions on what counts as a group for subordination purposes: race, ethnicity, national origin, religion, gender, sexual orientation, disability, political ideology. Identify whether courts consistently treat membership as locked or apply different standards per category.',
    'If identity-lock is applied inconsistently, the constraint''s directionality varies by group (some groups get trapped/identity_locked, others get constrained/mobile), creating asymmetric protection and potential extractiveness on groups the reading treats as less identity-locked. High inconsistency would suggest the constraint''s equal-dignity claim is not applied equally across groups.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_vs_choice_in_group_membership, empirical, 'Variation in identity-lock treatment across group categories.').

omega_variable(
    court_enforcement_as_alternative_subordination,
    'Does court enforcement of the equal-dignity constraint itself function as subordination of marginalized speakers (powerless, identity-locked publishers with group-targeting intent)?',
    'Analysis of enforcement patterns: are marginalized speakers disproportionately prosecuted under hate-speech restrictions while powerful speakers avoid enforcement through framing, institutional protection, or resource? Historical comparison with prior regimes where subordination was enforced by courts claiming equal-protection rationales.',
    'If enforcement itself subordinates marginalized speakers asymmetrically, the constraint''s net effect on subordination may be zero or negative: it prevents one mode of subordination (speech-targeting groups) while enabling another (selective enforcement against powerless speakers). This would suggest the constraint trades subordination mechanisms rather than reducing subordination overall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(court_enforcement_as_alternative_subordination, empirical, 'Whether enforcement reproduces subordination rather than preventing it.').

omega_variable(
    intra_sibling_reading_relations_stability,
    'Are the declared reading-relations (forecloses, coexists_with, influences) stable across time, or do they shift as political contexts change?',
    'Historical analysis of how the readings have interacted in concrete constitutional moments: do jurisdictions oscillate between readings (suggesting coexistence), or do movements show one reading replacing another permanently (suggesting foreclosure)? Test whether harm_threshold_reading has historically foreclosed or merely influenced absolutist_reading, and vice versa.',
    'If relations are historically stable (e.g., dignity_reading consistently forecloses absolutist_reading), the kernel contest is enduring. If relations shift (e.g., absolutist_reading emerges as live again after periods of suppression), the reading-relation classification is time-relative and the constraint''s classification itself may drift across constitutional eras.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intra_sibling_reading_relations_stability, conceptual, 'Temporal stability of inter-reading logical relations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__dignity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__dignity_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(spee_tr_t0, projected).
narrative_ontology:measurement(spee_tr_t5, speech_protection_kernel__dignity_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(spee_tr_t5, observed).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__dignity_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement_basis(spee_tr_t10, observed).
narrative_ontology:measurement(spee_tr_t15, speech_protection_kernel__dignity_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement_basis(spee_tr_t15, observed).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__dignity_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(spee_tr_t20, observed).
narrative_ontology:measurement(spee_tr_t25, speech_protection_kernel__dignity_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(spee_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__dignity_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(spee_be_t0, projected).
narrative_ontology:measurement(spee_be_t5, speech_protection_kernel__dignity_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(spee_be_t5, observed).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__dignity_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(spee_be_t10, observed).
narrative_ontology:measurement(spee_be_t15, speech_protection_kernel__dignity_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(spee_be_t15, observed).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__dignity_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(spee_be_t20, observed).
narrative_ontology:measurement(spee_be_t25, speech_protection_kernel__dignity_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(spee_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__dignity_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(spee_su_t0, projected).
narrative_ontology:measurement(spee_su_t5, speech_protection_kernel__dignity_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(spee_su_t5, observed).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__dignity_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(spee_su_t10, observed).
narrative_ontology:measurement(spee_su_t15, speech_protection_kernel__dignity_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(spee_su_t15, observed).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__dignity_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(spee_su_t20, observed).
narrative_ontology:measurement(spee_su_t25, speech_protection_kernel__dignity_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(spee_su_t25, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=25
narrative_ontology:measurement(spee_grid_01, speech_protection_kernel__dignity_reading, accessibility_collapse(class), 0, 0.75).
narrative_ontology:measurement(spee_grid_02, speech_protection_kernel__dignity_reading, accessibility_collapse(class), 25, 0.78).
narrative_ontology:measurement(spee_grid_03, speech_protection_kernel__dignity_reading, accessibility_collapse(individual), 0, 0.55).
narrative_ontology:measurement(spee_grid_04, speech_protection_kernel__dignity_reading, accessibility_collapse(individual), 25, 0.61).
narrative_ontology:measurement(spee_grid_05, speech_protection_kernel__dignity_reading, accessibility_collapse(organizational), 0, 0.62).
narrative_ontology:measurement(spee_grid_06, speech_protection_kernel__dignity_reading, accessibility_collapse(organizational), 25, 0.7).
narrative_ontology:measurement(spee_grid_07, speech_protection_kernel__dignity_reading, accessibility_collapse(structural), 0, 0.68).
narrative_ontology:measurement(spee_grid_08, speech_protection_kernel__dignity_reading, accessibility_collapse(structural), 25, 0.72).
narrative_ontology:measurement(spee_grid_09, speech_protection_kernel__dignity_reading, resistance(class), 0, 0.71).
narrative_ontology:measurement(spee_grid_10, speech_protection_kernel__dignity_reading, resistance(class), 25, 0.73).
narrative_ontology:measurement(spee_grid_11, speech_protection_kernel__dignity_reading, resistance(individual), 0, 0.62).
narrative_ontology:measurement(spee_grid_12, speech_protection_kernel__dignity_reading, resistance(individual), 25, 0.71).
narrative_ontology:measurement(spee_grid_13, speech_protection_kernel__dignity_reading, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(spee_grid_14, speech_protection_kernel__dignity_reading, resistance(organizational), 25, 0.74).
narrative_ontology:measurement(spee_grid_15, speech_protection_kernel__dignity_reading, resistance(structural), 0, 0.75).
narrative_ontology:measurement(spee_grid_16, speech_protection_kernel__dignity_reading, resistance(structural), 25, 0.78).
narrative_ontology:measurement(spee_grid_17, speech_protection_kernel__dignity_reading, stakes_inflation(class), 0, 0.71).
narrative_ontology:measurement(spee_grid_18, speech_protection_kernel__dignity_reading, stakes_inflation(class), 25, 0.79).
narrative_ontology:measurement(spee_grid_19, speech_protection_kernel__dignity_reading, stakes_inflation(individual), 0, 0.48).
narrative_ontology:measurement(spee_grid_20, speech_protection_kernel__dignity_reading, stakes_inflation(individual), 25, 0.58).
narrative_ontology:measurement(spee_grid_21, speech_protection_kernel__dignity_reading, stakes_inflation(organizational), 0, 0.52).
narrative_ontology:measurement(spee_grid_22, speech_protection_kernel__dignity_reading, stakes_inflation(organizational), 25, 0.64).
narrative_ontology:measurement(spee_grid_23, speech_protection_kernel__dignity_reading, stakes_inflation(structural), 0, 0.58).
narrative_ontology:measurement(spee_grid_24, speech_protection_kernel__dignity_reading, stakes_inflation(structural), 25, 0.68).
narrative_ontology:measurement(spee_grid_25, speech_protection_kernel__dignity_reading, suppression(class), 0, 0.68).
narrative_ontology:measurement(spee_grid_26, speech_protection_kernel__dignity_reading, suppression(class), 25, 0.79).
narrative_ontology:measurement(spee_grid_27, speech_protection_kernel__dignity_reading, suppression(individual), 0, 0.48).
narrative_ontology:measurement(spee_grid_28, speech_protection_kernel__dignity_reading, suppression(individual), 25, 0.58).
narrative_ontology:measurement(spee_grid_29, speech_protection_kernel__dignity_reading, suppression(organizational), 0, 0.55).
narrative_ontology:measurement(spee_grid_30, speech_protection_kernel__dignity_reading, suppression(organizational), 25, 0.68).
narrative_ontology:measurement(spee_grid_31, speech_protection_kernel__dignity_reading, suppression(structural), 0, 0.54).
narrative_ontology:measurement(spee_grid_32, speech_protection_kernel__dignity_reading, suppression(structural), 25, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__dignity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_kernel produces five structurally distinct constraints, one per reading. Each reading instantiates a different ε (extraction), different beneficiary/victim structure, and different boundary for what counts as protected speech. The readings are linked by kernel contest: they are alternative framings of the same constitutional commitment, competing for institutional adoption. The dignity reading differs from absolutist by recognizing group harm as distinct from individual-speaker harm; it differs from harm-threshold by making equal dignity (not empirical harm) foundational; it differs from marketplace by rejecting the counter-speech solution for group-targeting speech; it differs from democratic-participation by not limiting protection-reduction to non-political speech (group targeting can be political). Each reading carries different directionality data: the dignit reading shows high directionality spread (target-group beneficiaries at low d, marginalized publishers at high d, courts at symmetric d), while absolutist would show flat low d across all speakers (all equally protected). Decomposition is necessary: a single 'speech protection' constraint could not capture this structural variation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__dignity_reading, powerless, 0.85).
constraint_indexing:directionality_override(speech_protection_kernel__dignity_reading, powerful, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
