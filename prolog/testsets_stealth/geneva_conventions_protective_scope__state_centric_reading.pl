% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__state_centric_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__state_centric_reading
 *   human_readable: Geneva Protective Scope — State-Centric Reading: Article 4 Combatant Criteria Gate
 *   domain: international_humanitarian_law/legal_theory/armed_conflict_studies
 *
 * SUMMARY:
 *   The Geneva protective regime reaches captured and targeted fighters
 *   through a gate: Article 4 of the Third Convention conditions
 *   prisoner-of-war status on membership in a state party's armed forces or
 *   on a militia meeting cumulative criteria — responsible command, a fixed
 *   distinctive sign recognizable at a distance, open carriage of arms,
 *   compliance with the laws of war. This story instantiates the
 *   state-centric reading of the protective-scope kernel: the gate is the
 *   treaty's operative scope rule, fighters failing the criteria are
 *   unprivileged belligerents outside treaty scope, and the reading's
 *   structural effect is to narrow the protected class and lower the burden
 *   attributed to state military operations in asymmetric conflicts. The
 *   claim/metric gap is deliberate and is the datum: the reading CLAIMS
 *   legitimate, consent-based reciprocity coordination (rope), while the
 *   authored metrics describe coordination fused with asymmetric extraction —
 *   the criteria were authored, are adjudicated, and are enforced by the
 *   parties that benefit from the exclusion. Epsilon is authored for the
 *   standing arrangement under contest (the Article 4 gate as operative scope
 *   of the regime), as the state-centric reading itself assesses it: the
 *   reading authorizes the exclusion rather than denying it, so its epsilon
 *   is moderate; a universal-rights reading over the identical referent would
 *   author substantially higher epsilon by refusing the justification.
 *
 * KEY AGENTS:
 *   - state_parties_to_geneva_conventions: agenda-setter and dual beneficiary (institutional/arbitrage) — authored the criteria, adjudicate status through national tribunals, and decide by treaty action whether the scope widens or holds
 *   - conventional_state_militaries: primary beneficiary (institutional/mobile) — retain combatant immunity and POW protections for their own uniformed forces while gaining targeting, detention, and prosecution latitude over those excluded
 *   - unprivileged_belligerents: primary target (powerless/trapped) — bear status-stripping: prosecution for the mere act of participation, detention without POW guarantees, targetability without combatant immunity
 *   - non_state_armed_group_fighters: secondary target (organized/trapped) — organized but seatless: no treaty capacity, no diplomatic-conference standing, status determined by their adversary
 *   - civilian_populations_in_conflict_zones: dual-positioned (powerless/trapped) — collect the distinction incentive the gate maintains; pay when excluded groups abandon distinction entirely
 *   - national_liberation_movements: excluded seat (organized/trapped) — conditionally granted combatant status by AP I Article 1(4), excluded wherever the gate's beneficiaries declined ratification
 *   - international_committee_red_cross: observer (organized/analytical) — custodial access without a vote; reads the protective scope broader than the gate
 *   - ihl_scholar_community: analytical observer — produces the doctrinal record (Tadic-line jurisprudence, customary-IHL study) in which the readings compete
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, 0.6).
domain_priors:suppression_score(geneva_conventions_protective_scope__state_centric_reading, 0.55).
domain_priors:theater_ratio(geneva_conventions_protective_scope__state_centric_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__state_centric_reading, rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__state_centric_reading, "Geneva Protective Scope — State-Centric Reading: Article 4 Combatant Criteria Gate").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__state_centric_reading, "international_humanitarian_law/legal_theory/armed_conflict_studies").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__state_centric_reading, '8057fc78-c1e0-420c-b8e8-d6fd9654bb3f').
narrative_ontology:cs_kernel_codification('8057fc78-c1e0-420c-b8e8-d6fd9654bb3f', fixed_text).
narrative_ontology:cs_authority_grounding('8057fc78-c1e0-420c-b8e8-d6fd9654bb3f', lineage).
narrative_ontology:cs_interpretation_layer_present('8057fc78-c1e0-420c-b8e8-d6fd9654bb3f').
narrative_ontology:cs_reading_relation('8057fc78-c1e0-420c-b8e8-d6fd9654bb3f', geneva_conventions_protective_scope__universal_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('8057fc78-c1e0-420c-b8e8-d6fd9654bb3f', geneva_conventions_protective_scope__hybrid_proportionality_reading, influences).
narrative_ontology:cs_axiom('8057fc78-c1e0-420c-b8e8-d6fd9654bb3f', foundational, protection_requires_lawful_combatant_status).
narrative_ontology:cs_axiom_status(protection_requires_lawful_combatant_status, holdable).
narrative_ontology:cs_axiom_grounding('8057fc78-c1e0-420c-b8e8-d6fd9654bb3f', protection_requires_lawful_combatant_status, conventional).
narrative_ontology:cs_axiom('8057fc78-c1e0-420c-b8e8-d6fd9654bb3f', foundational, reciprocity_conditions_protection).
narrative_ontology:cs_axiom_status(reciprocity_conditions_protection, holdable).
narrative_ontology:cs_axiom_grounding('8057fc78-c1e0-420c-b8e8-d6fd9654bb3f', reciprocity_conditions_protection, instrumental).
narrative_ontology:cs_axiom('8057fc78-c1e0-420c-b8e8-d6fd9654bb3f', secondary, state_consent_bounds_treaty_scope).
narrative_ontology:cs_axiom_status(state_consent_bounds_treaty_scope, holdable).
narrative_ontology:cs_axiom_grounding('8057fc78-c1e0-420c-b8e8-d6fd9654bb3f', state_consent_bounds_treaty_scope, conventional).
narrative_ontology:cs_reference_frame('8057fc78-c1e0-420c-b8e8-d6fd9654bb3f', article4_reciprocity_framework).
narrative_ontology:cs_drift_state('8057fc78-c1e0-420c-b8e8-d6fd9654bb3f', post_hamdan_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8057fc78-c1e0-420c-b8e8-d6fd9654bb3f', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, state_parties_to_geneva_conventions).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, non_state_armed_group_fighters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__state_centric_reading, combatant_civilian_distinction_principle).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__state_centric_reading, reciprocity_based_protection_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__state_centric_reading, state_consent_treaty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and ratified the 1949 Conventions; their diplomatic conferences author the criteria; their national tribunals determine who qualifies under Article 5; they decide whether to ratify AP I or accept broader readings. The gate's scope is what they consented it to be, and they retain the power to widen or narrow it by treaty action — while collecting the operational latitude the current scope preserves.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, state_parties_to_geneva_conventions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__state_centric_reading, state_parties_to_geneva_conventions, beneficiary).

% Operate under the gate in asymmetric conflicts: their uniformed forces retain combatant immunity and POW protections by meeting the criteria, while adversaries who fail them may be targeted without combatant immunity, detained without POW guarantees, and prosecuted for participation under domestic law. They collect the operational latitude a universal scope would surrender, and nothing binds them that their own state could not change.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, beneficiary,
    institutional, generational, mobile, global).

% Fighters who fail one or more of the criteria — irregular militia without a fixed distinctive sign, participants outside a responsible command structure, foreign fighters attached to no state party's forces. Once captured, the adversary's tribunal determines their status; failing the criteria they may be prosecuted for the mere act of fighting, held without POW protections, and in the strictest applications held for the duration of hostilities without the procedural guarantees POW status carries. Their status is decided entirely inside a framework they had no part in authoring.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents, payer,
    powerless, biographical, trapped, regional).

% Members of armed groups that are organizationally capable but seatless: they cannot ratify treaties, appear at the diplomatic conferences that author the criteria, or hold the reciprocal protections the criteria gate. Their compliance incentives are weakened — the protected-status reward is administered by their adversary, and the observable markers the criteria require (distinctive signs, open carriage of arms) can be tactically costly to adopt against a superior force.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, non_state_armed_group_fighters, payer,
    organized, biographical, trapped, regional).

% The distinction system the criteria incentivize is their main shield: fighters who wear fixed distinctive signs and carry arms openly are identifiable, which keeps combat away from those who cannot fight. They gain when the gate keeps that distinction meaningful; they pay when excluded groups, denied the protected-status reward, abandon distinction entirely and when fighting moves through their areas. They hold no seat in the framework; their protection is an externality of the bargain between the armed parties.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, civilian_populations_in_conflict_zones, beneficiary,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__state_centric_reading, civilian_populations_in_conflict_zones, payer).

% Movements fighting against colonial domination, alien occupation, or racist regimes were granted combatant status by AP I Article 1(4) — but only as against states that ratified AP I. The principal military powers never ratified, so fighters in those conflicts fall back outside the criteria gate. They sought a seat in the framework and received one only conditionally, by an instrument their adversaries declined to join.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, national_liberation_movements, excluded,
    organized, generational, trapped, regional).

% Custodian of the humanitarian-law framework: visits detainees, publishes commentary on the conventions, and has consistently read the protective scope more broadly than the strict criteria gate. It holds standing and access but no vote — its readings persuade; they do not bind status determinations.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, international_committee_red_cross, observer,
    organized, generational, analytical, global).

% Produces the doctrinal record within which the readings compete: the Tadic-line jurisprudence on non-international armed conflict, the ICRC customary-law study, and the extensive literature disputing whether the Article 4 criteria match the conflicts actually fought. No enforcement power; influence runs through tribunals, military legal advisors, and treaty diplomacy.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, ihl_scholar_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the combatant/civilian identification problem in armed conflict: fixed distinctive signs, open carriage of arms, and responsible command create verifiable markers separating fighters from civilians, and conditioning protected status on those markers gives both sides a reciprocal incentive to keep the distinction intact — the mechanism by which civilian populations are shielded from being caught between armed forces.
% TRANSFER_FUNCTION: Moves legal protection and immunity between classes of fighters according to criteria the state parties authored: fighters meeting Article 4 receive combatant immunity and POW protections; fighters failing them transfer to the capturing state's discretion — prosecutable for participation, detainable without POW guarantees, targetable without combatant immunity. The transfer runs from the excluded fighter's legal personhood to the capturing state's operational latitude.
% ABSENT_VOICES: The fighters whose status is determined have no seat in the framework that defines it: non-state armed groups cannot ratify treaties or appear at the diplomatic conferences that author the criteria, and individual fighters face status determinations by the adversary's tribunals. National liberation movements sought representation and received it only conditionally through AP I, which the principal military powers declined to ratify. The ICRC holds access without a vote.
% DISAPPEARANCE_RATIONALE: If the criteria gate vanished overnight, every captured fighter would claim POW status, detention regimes would reorganize around status-blind protection, states would lose the prosecution-for-participation lever the combatant-immunity line draws, and the incentive structure maintaining the combatant/civilian distinction would need a replacement — or the distinction itself would erode, shifting risk onto civilians. Both the gate's beneficiaries and its targets are organized around it; its disappearance forces rearrangement, not continuity.
% FOUNDING_PROBLEM: The 1949 Conventions were written out of the Second World War's record: captured fighters executed or held without status, protection made contingent on reprisal bargains, no verifiable line between fighter and civilian. Article 4's criteria were built to solve the identification-and-reciprocity problem — defining who counts as a lawful fighter entitled to protection, in terms both sides can verify, so that protection could be owed and honored between adversaries.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC — outside the benefiting parties, custodian of the framework — attests in its commentary and detention reports that the identification and protection problem persists in every current armed conflict. The historical record of the 1949 diplomatic conference attests the original problem. IHL scholars outside state delegations attest the problem is live while disputing whether state-authored criteria still serve it in asymmetric conflicts: corroboration of the problem's liveness is broad; corroboration of the gate as its solution is contested.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__state_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__state_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__state_centric_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.6: the gate imposes serious, acknowledged costs on a defined class — prosecution for participation, detention without POW guarantees, targetability without combatant immunity — costs the reading justifies as reciprocity but does not deny. Suppression is 0.55: the gate is held by legal machinery rather than overt force (Article 5 status tribunals, detention regimes, domestic prosecution), and the deciding tribunal belongs to the party that benefits from the exclusion. Theater is 0.35: status-determination reviews have repeatedly been criticized as pre-determined (the Guantanamo CSRT record), but the distinction and detention framework is substantially functional. Accessibility collapse is 0.45: the gate forecloses POW status and combatant immunity specifically; Common Article 3, customary humanitarian law, and human rights bodies persist as partial alternatives, so the exclusion does not collapse all protection. Resistance is 0.6: the ICRC, tribunal jurisprudence, and a large scholarly record contest the narrow scope, and the sibling readings of this kernel are live positions. The measurement series run on one shared time grid (points 0/15/30/45/60/75 of the interval, approximating 1949/1964/1979/1994/2009/2024): extraction climbs through the decolonization wars and the AP I non-ratification settlement, peaks in the war-on-terror application (unlawful-combatant designations, Guantanamo), and partially recedes after Hamdan; theater and enforcement suppression peak with it and partially recede. suppression_requirement is authored because this story specifically tracks enforcement-capacity change — the build-out and partial stand-down of the status-tribunal and detention machinery — not merely extraction drift. Fixing cost: widening the gate is prohibitive for its beneficiaries — the AP I route has been open since 1977, and the principal military powers have declined it for five decades because the cost (surrendering the exclusion's operational latitude in ongoing asymmetric conflicts, without assurance of reciprocity) exceeds the benefit they would collect.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats should compute different types from the same structure. From the state seat the gate is the treaty working as designed: criteria the parties consented to, verifiable reciprocity, protection flowing to those who keep the distinction. From the captured fighter's seat the same gate is the device that strips status and hands the adversary both the sword and the scale — the party that targets, detains, and prosecutes also decides whether the target qualified for protection. Same-level divergence among state parties at identical institutional power: a state fighting an insurgency on its own territory experiences the gate as essential order-maintenance and holds it tightly; a state whose nationals fight as irregulars abroad, or that faces expeditionary intervention, experiences the identical text as a standing threat to its own people — same power atom, opposite directionality, differentiated by which side of the gate its fighters land on.
 *
 * DIRECTIONALITY LOGIC:
 *   State parties and their militaries sit at the beneficiary end: the gate subsidizes their operations (targeting latitude, detention discretion, the prosecution lever) and they authored and adjudicate it. Unprivileged belligerents and non-state armed group fighters sit at the target end: they bear the transfer in full, and their exit is trapped — once captured, status is determined inside the adversary's framework, and the group as a whole has no treaty seat from which to contest the criteria. Civilian populations sit near symmetric: they collect the distinction incentive the gate maintains (identifiable fighters keep war off their backs) and pay when excluded groups, denied the protected-status reward, abandon distinction entirely. The ICRC and the scholarly community are analytical seats: they shape the contest but collect and pay nothing under the gate. Receipt: the gate's gains demonstrably accrue to the militaries of the gate's state parties — targeting latitude, detention discretion, the prosecution lever — which is why gain_flow names that seat rather than the treaty parties at large.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — verifiable identification of lawful fighters and reciprocal protection of the captured — is live and corroborated from outside the benefiting parties (ICRC custodial reporting, the 1949 diplomatic conference record, scholarly consensus on the problem's persistence), so this is not a mandatrophy case: the arrangement is not outliving its function, and the mismatch check (live founding problem crossed with the world_rearranges verdict) raises no zombie flag. The classification's work here is different: it keeps the reading's rope claim and the structural data from merging. Reading the gate as the pure coordination the reading claims would erase the victim set — the fighters whose status the gate strips; reading it as pure extraction would erase the civilian-protection function any replacement regime must preserve. The metrics describe the honest shape — coordination fused with extraction, actively enforced — and the per-seat computation is expected to return coordination-dominant classifications at the beneficiary seats and extraction-dominant classifications at the trapped payer seats. That divergence, not the claim, is the finding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the geneva_conventions_protective_scope kernel — the state_centric_reading. Which reading correctly instantiates the kernel, and what does the victim set become under each sibling?',
    'Treaty interpretation, state practice evolution, ICRC custodial determinations, and tribunal jurisprudence (Tadic line, Hamdan line): the reading that captures state practice and opinio juris becomes operative.',
    'Under universal_rights_reading, unprivileged belligerents enter the protected set, the victim set of state operations widens, and epsilon on state military operations rises substantially over the same referent. Under hybrid_proportionality_reading, the protected set scales by conflict type and the Article 4 gate survives only in international armed conflict. This story''s epsilon, beneficiaries, and victims are all reading-indexed; a sibling file over the identical referent authors different values by design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer omega: this file instantiates one reading of a contested kernel; the disagreement is located in whether combatant status gates protection, and sibling readings change the victim set and epsilon.').

omega_variable(
    reciprocity_selectivity,
    'Is the Article 4 reciprocity genuinely two-way in asymmetric conflicts — do state forces extend the promised protections to qualifying adversaries in the same conflicts where they deny status to irregulars?',
    'Comparative state practice within single conflicts: treatment of captured uniformed adversaries versus captured irregulars; POW-status compliance records; detention-regime audits and ICRC visit reports.',
    'If reciprocity is selective — honored only toward a state''s peers while the exclusion is applied to its asymmetric adversaries — the coordination function is cover and the arrangement computes closer to pure extraction; if reciprocity is genuine, the coordination-plus-cost structure holds and the exclusion is the price of the distinction system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_selectivity, empirical, 'Whether the reciprocity rationale operates symmetrically or only to the state''s advantage.').

omega_variable(
    customary_floor_ambiguity,
    'Does customary international law already extend a humane-treatment floor to unprivileged belligerents regardless of the treaty-scope gate, bounding the reading''s practical exclusion?',
    'ICRC customary international humanitarian law study, state practice and opinio juris on treatment of detained irregular fighters, detention and habeas jurisprudence across jurisdictions.',
    'If a robust customary floor exists, the reading''s practical reach is narrower than the formal exclusion suggests — the gate removes POW status and combatant immunity specifically, not all protection. If the floor is thin in practice, the exclusion''s real cost approaches total status-stripping and effective extraction rises accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_floor_ambiguity, empirical, 'Whether custom bounds the treaty gate''s practical reach over excluded fighters.').

omega_variable(
    self_adjudication_bias,
    'Does the capturing state''s dual role — beneficiary of the exclusion and adjudicator of who falls within it (Article 5 status tribunals) — make the gate''s application structurally biased toward the deciding party''s operational interests?',
    'Cross-state comparison of Article 5 tribunal compliance rates and outcomes; whether determinations track the legal criteria or the detaining power''s operational posture; judicial-review records (Hamdan, Boumediene line).',
    'If determinations systematically favor the deciding state''s interests, suppression and effective extraction are higher than the treaty text alone suggests, and the enforcement machinery is part of the exclusion rather than a check on it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_adjudication_bias, empirical, 'Structural bias risk from the beneficiary adjudicating its own gate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__state_centric_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geneva_state_centric_reading_tr_t0, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(geneva_state_centric_reading_tr_t0, observed).
narrative_ontology:measurement(geneva_state_centric_reading_tr_t15, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement_basis(geneva_state_centric_reading_tr_t15, observed).
narrative_ontology:measurement(geneva_state_centric_reading_tr_t30, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(geneva_state_centric_reading_tr_t30, observed).
narrative_ontology:measurement(geneva_state_centric_reading_tr_t45, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 45, 0.3).
narrative_ontology:measurement_basis(geneva_state_centric_reading_tr_t45, observed).
narrative_ontology:measurement(geneva_state_centric_reading_tr_t60, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 60, 0.44).
narrative_ontology:measurement_basis(geneva_state_centric_reading_tr_t60, observed).
narrative_ontology:measurement(geneva_state_centric_reading_tr_t75, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 75, 0.35).
narrative_ontology:measurement_basis(geneva_state_centric_reading_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(geneva_state_centric_reading_be_t0, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(geneva_state_centric_reading_be_t0, observed).
narrative_ontology:measurement(geneva_state_centric_reading_be_t15, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement_basis(geneva_state_centric_reading_be_t15, observed).
narrative_ontology:measurement(geneva_state_centric_reading_be_t30, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement_basis(geneva_state_centric_reading_be_t30, observed).
narrative_ontology:measurement(geneva_state_centric_reading_be_t45, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 45, 0.58).
narrative_ontology:measurement_basis(geneva_state_centric_reading_be_t45, observed).
narrative_ontology:measurement(geneva_state_centric_reading_be_t60, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(geneva_state_centric_reading_be_t60, observed).
narrative_ontology:measurement(geneva_state_centric_reading_be_t75, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 75, 0.6).
narrative_ontology:measurement_basis(geneva_state_centric_reading_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(geneva_state_centric_reading_su_t0, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(geneva_state_centric_reading_su_t0, observed).
narrative_ontology:measurement(geneva_state_centric_reading_su_t15, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement_basis(geneva_state_centric_reading_su_t15, observed).
narrative_ontology:measurement(geneva_state_centric_reading_su_t30, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement_basis(geneva_state_centric_reading_su_t30, observed).
narrative_ontology:measurement(geneva_state_centric_reading_su_t45, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 45, 0.52).
narrative_ontology:measurement_basis(geneva_state_centric_reading_su_t45, observed).
narrative_ontology:measurement(geneva_state_centric_reading_su_t60, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 60, 0.66).
narrative_ontology:measurement_basis(geneva_state_centric_reading_su_t60, observed).
narrative_ontology:measurement(geneva_state_centric_reading_su_t75, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 75, 0.55).
narrative_ontology:measurement_basis(geneva_state_centric_reading_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__universal_rights_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'the Geneva Conventions' protective scope' conflates structurally distinct claims about who the regime protects; per the epsilon-invariance principle this family decomposes it. The state-centric reading (this file) gates protection on Article 4 combatant criteria; the universal-rights reading installs a status-blind floor via Common Article 3 and human rights law; the hybrid-proportionality reading scales protection by conflict type. Each reading instantiates a different constraint with a different victim set and a different epsilon over the same referent. The state-centric reading is the upstream, text-anchored position: it is cited as the positive-law baseline the other two readings must displace, and its ratification politics (AP I non-ratification) sets the operating environment both siblings inherit — hence its edges run toward both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
