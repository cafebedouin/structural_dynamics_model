% ============================================================================
% CONSTRAINT STORY: marriage_authority__communal_autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__communal_autonomy_reading, []).

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
 *   constraint_id: marriage_authority__communal_autonomy_reading
 *   human_readable: Communal Autonomy Settlement in Marriage Authority (Personal Law Regime)
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This story instantiates the communal-autonomy reading of the
 *   marriage-authority kernel: in a religiously plural state, authority over
 *   marriage, divorce, and succession is grounded in each community's
 *   religious tradition; the state registers, enforces, and backs those norms
 *   but does not author them, and legislative amendment of any community's
 *   personal law requires that community's consent. The arrangement solves a
 *   real coordination problem — governing family life across deep normative
 *   diversity without majoritarian imposition — while routing adjudicative
 *   authority, amendment veto power, and the status rents attached to both to
 *   religious leadership. Intra-community dissenters (women facing unequal
 *   divorce and maintenance terms, interfaith couples with no neutral forum,
 *   reformists and apostates) pay through the same structure they cannot
 *   amend. On claim/metric independence: the manifest seeded this story with
 *   a moderate-epsilon rope hypothesis; after refinement the structural facts
 *   — named victims, active enforcement, consent-gated reform — make
 *   tangled_rope the honest story-level claim, while the coordination half
 *   the manifest saw remains real and is preserved in the metrics. Epsilon's
 *   referent is the standing personal-law arrangement, assessed by this
 *   reading's own lights, which is why epsilon is moderate rather than high
 *   despite dissenter costs this reading itself acknowledges.
 *
 * KEY AGENTS:
 *   - religious_leadership: primary beneficiary and effective agenda-setter within communities (institutional/arbitrage) — collects adjudicative authority, amendment veto, and status rents
 *   - state_recognition_authority: meta-level agenda-setter and incidental beneficiary (institutional/mobile) — enforces without authoring; collects social peace at low cost
 *   - traditionalist_community_majorities: beneficiary constituents (organized/identity_locked) — receive enforced continuity of inherited norms
 *   - women_under_personal_law: primary payer seat (moderate/trapped) — bear unequal terms; blocked from the amendment channel
 *   - interfaith_couples: payer seat falling between jurisdictions (moderate/constrained)
 *   - reformists_and_apostates: payer seat with identity-locked exit (powerless/identity_locked)
 *   - gender_equality_movements: excluded voice (organized/mobile) — object but sit outside the consent structure
 *   - constitutional_courts: analytical observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, 0.52).
domain_priors:suppression_score(marriage_authority__communal_autonomy_reading, 0.62).
domain_priors:theater_ratio(marriage_authority__communal_autonomy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__communal_autonomy_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__communal_autonomy_reading, "Communal Autonomy Settlement in Marriage Authority (Personal Law Regime)").
narrative_ontology:topic_domain(marriage_authority__communal_autonomy_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__communal_autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__communal_autonomy_reading, 'ad1683a2-a607-4335-b604-fc40c9dfd4a6').
narrative_ontology:cs_kernel_codification('ad1683a2-a607-4335-b604-fc40c9dfd4a6', distributed).
narrative_ontology:cs_authority_grounding('ad1683a2-a607-4335-b604-fc40c9dfd4a6', lineage).
narrative_ontology:cs_interpretation_layer_present('ad1683a2-a607-4335-b604-fc40c9dfd4a6').
narrative_ontology:cs_reading_relation('ad1683a2-a607-4335-b604-fc40c9dfd4a6', marriage_authority__secularist_reading, forecloses).
narrative_ontology:cs_reading_relation('ad1683a2-a607-4335-b604-fc40c9dfd4a6', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('ad1683a2-a607-4335-b604-fc40c9dfd4a6', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('ad1683a2-a607-4335-b604-fc40c9dfd4a6', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('ad1683a2-a607-4335-b604-fc40c9dfd4a6', foundational, communal_religious_authority_over_marriage).
narrative_ontology:cs_axiom_status(communal_religious_authority_over_marriage, holdable).
narrative_ontology:cs_axiom_grounding('ad1683a2-a607-4335-b604-fc40c9dfd4a6', communal_religious_authority_over_marriage, theological).
narrative_ontology:cs_axiom('ad1683a2-a607-4335-b604-fc40c9dfd4a6', foundational, community_consent_gates_personal_law_reform).
narrative_ontology:cs_axiom_status(community_consent_gates_personal_law_reform, holdable).
narrative_ontology:cs_axiom_grounding('ad1683a2-a607-4335-b604-fc40c9dfd4a6', community_consent_gates_personal_law_reform, deontological).
narrative_ontology:cs_reference_frame('ad1683a2-a607-4335-b604-fc40c9dfd4a6', communal_self_governance_settlement).
narrative_ontology:cs_drift_state('ad1683a2-a607-4335-b604-fc40c9dfd4a6', contemporary_gender_equality_jurisprudence, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ad1683a2-a607-4335-b604-fc40c9dfd4a6', '').
narrative_ontology:cs_kernel_id(marriage_authority__communal_autonomy_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, religious_leadership).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, traditionalist_community_majorities).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, women_under_personal_law).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, interfaith_couples).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, reformists_and_apostates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, state_recognition_authority).
narrative_ontology:constraint_vindicates(marriage_authority__communal_autonomy_reading, communal_autonomy_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__communal_autonomy_reading, legal_pluralism_principle).
narrative_ontology:constraint_vindicates(marriage_authority__communal_autonomy_reading, minority_cultural_self_determination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Heads the religious judiciaries and norm-authoring bodies of the recognized communities. Collects adjudicative jurisdiction over marriage, divorce, and succession, the fees and status attached to it, and an effective veto over legislative amendment of personal law through the community-consent requirement. Administers the courts that apply communal norms and arbitrates between accommodating and resisting state pressure. Its position is constituted by the arrangement; exit is not a meaningful category from where it stands.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, religious_leadership, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__communal_autonomy_reading, religious_leadership, agenda_setter).

% The legislature, cabinet, and registration organs that decide which communities' laws are enforceable, staff the enforcement machinery, and deliberately refrain from authoring substantive family-law norms. Collects social peace, community political support, and governance of family life at near-zero fiscal and authoring cost. Formally able to restructure the whole framework, but the legitimacy cost of doing so against organized communities has kept it in the enforcing-not-authoring posture.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, state_recognition_authority, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__communal_autonomy_reading, state_recognition_authority, beneficiary).

% Members whose marriage, divorce, and succession follow inherited communal norms enforced at public expense. Receive continuity of tradition, boundary maintenance, and legally backed certainty about family status without organizing any enforcement themselves. Leaving the arrangement means apostasy or conversion with loss of belonging, so their position is held by identity as much as by preference.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, traditionalist_community_majorities, beneficiary,
    organized, generational, identity_locked, national).

% Subject to communal divorce, maintenance, and custody terms that price them unequally. Cannot initiate amendment of those terms: the consent gate routes all change through the religious leadership whose institutions the terms sustain. Individual exit means abandoning family, often custody of children, and community standing. Coalition potential exists through women's movements and constitutional litigation, but both channels are slow, contested, and framed by the arrangement's defenders as external attack.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, women_under_personal_law, payer,
    moderate, biographical, trapped, national).

% Couples spanning community lines find no neutral civil marriage forum: they must convert, marry abroad and register the result domestically, or remain unrecognized with consequences for spousal rights, inheritance, and children's status. Emigration resolves the problem at the cost of leaving the country. They fall between the jurisdictions the arrangement maintains and pay for the gap.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, interfaith_couples, payer,
    moderate, biographical, constrained, national).

% Members who propose internal reform of communal norms find the proposal blocked at the consent gate it must pass; members who leave lose personal-law standing, inheritance expectations, and community ties at once. Their double bind — reform requires the consent of the institutions they seek to change, exit destroys the identity that made the norms theirs — is the arrangement's quietest cost.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, reformists_and_apostates, payer,
    powerless, biographical, identity_locked, national).

% Campaign for equality floors inside personal law and for an optional civil marriage track. Hold no seat in the community-consent structure their targets run through; their voice reaches the system only as litigation, protest, and international-review pressure, all of which the arrangement's beneficiaries classify as outside interference rather than participation.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, gender_equality_movements, excluded,
    organized, generational, mobile, national).

% Adjudicate collisions between communal norms and constitutional guarantees case by case. Take testimony from every seat, commission comparative analysis, and can erode the arrangement incrementally without authoring replacement norms. Analytical seat: neither collects from the arrangement nor bears its costs.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__communal_autonomy_reading, religious_leadership).
narrative_ontology:fixing_cost_class(marriage_authority__communal_autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Governs marriage, divorce, and succession across religiously heterogeneous populations without a single authoring authority: each community maintains a coherent, internally legitimate family-law regime, and the state supplies registration and enforcement, avoiding both the legitimacy cost of imposing uniform law on unwilling communities and the fiscal cost of building parallel adjudication.
% TRANSFER_FUNCTION: Moves adjudicative authority over family formation and dissolution from individuals and the state to community religious institutions; moves enforcement labor from the state to communities at no fiscal charge; and moves amendment initiative away from legislatures and dissenters to religious leadership through the community-consent requirement.
% ABSENT_VOICES: Women governed by restrictive personal law, interfaith couples, and internal reformists are subjects of the arrangement but absent from its authoring conversation: none holds a seat in the consent structure, and their objections reach the system only as constitutional litigation or street protest, which the reading classifies as external interference rather than voice. Secular citizens wanting an opt-out civil track are likewise unrepresented in community fora.
% DISAPPEARANCE_RATIONALE: Overnight disappearance leaves the population's marriages, divorces, and successions without an operative governing regime: either a uniform civil code replaces it — a massive, contested legislative rearrangement the consent structure currently blocks — or a registration vacuum ensues. Community institutions would lose jurisdiction and revenue, interfaith couples would gain a forum, and women's legal position would shift abruptly in whichever direction the replacement moved. Nothing about the status quo survives removal passively.
% FOUNDING_PROBLEM: Modernizing states inherited religiously divided populations whose family law they could not credibly author: imperial and colonial administrations (millet systems, colonial personal-law codifications, Mandate-era arrangements) delegated marriage and family regulation to established religious authorities to buy order, quiescence, and low administrative cost.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: comparative-law and imperial historiography documents the delegation settlements across millet, colonial-Indian, and Mandate-era administrations; cross-community elite acceptance at the founding moment is recorded in constituent-assembly debates; and international human-rights reviews repeatedly register the continuing diversity problem. Women's-movement testimony corroborates that the problem is live while disputing that the communal form is an acceptable solution — corroboration of the problem, contestation of the arrangement.
narrative_ontology:disappearance_verdict(marriage_authority__communal_autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__communal_autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__communal_autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__communal_autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__communal_autonomy_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__communal_autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__communal_autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.52): the arrangement transfers real authority and veto power to religious leadership and imposes uncompensated costs on dissenters, but the majority of governed members are net beneficiaries and the coordination function is genuine, which caps epsilon well below snare levels. Suppression (0.62) is structural, not theatrical: jurisdictional closure (no general civil-marriage track), the consent gate on amendment, and social and legal penalties on exit do the coercive work; per the framework, suppression is authored as a raw structural property and is not scaled by power or scope — only extractiveness is scaled, by the engine, from directionality and scope. Theater is low (0.20): religious courts genuinely adjudicate daily marriage, divorce, and succession disputes; ceremonial legitimation exists but is not load-bearing. Accessibility collapse (0.60) reflects that alternatives collapse substantially but not completely once the arrangement is understood — emigration, conversion, and occasional statutory carve-outs persist at high cost. Resistance (0.55) is sustained: women's movements, reformist currents, and equality litigation contest the arrangement continuously without displacing it. All three measurement series share one seven-point grid (t=0..60, step 10) so the engine samples aligned rows; the gentle upward drift in base_extractiveness models widening gap-driven extraction — static norms meeting rising equality expectations — not escalating predation. suppression_requirement is tracked because the story's dynamic is enforcement intensification: maintaining jurisdictional closure takes growing effort as dissent and circumvention rise. Boltzmann coordination type is identity_coordination: the dominant function is boundary maintenance — who may marry whom, how membership transmits into legal standing — and no floor override is declared, so the type default stands.
 *
 * PERSPECTIVAL GAP:
 *   From the religious_leadership and traditionalist_majority seats the arrangement computes as a rope: norms they endorse are enforced at public expense, amendment is protected from outsiders, and nothing is extracted from them. From the women_under_personal_law, interfaith_couples, and reformist seats the identical structure computes as enforced extraction with suppressed exits: they fund it with compliance, cannot amend it, and face trapped or identity-locked exit. The state seat sees cheap governance and political quiescence. The engine computes these per-seat classifications from the structural data; the story-level claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries map to low directionality: religious_leadership (collects jurisdiction and veto; arbitrage-grade position inside the system) and traditionalist_community_majorities (receive enforced continuity; identity-locked but benefiting) derive near the beneficiary pole. Payers map high: women_under_personal_law (trapped — exit means losing family, custody, and community) sit nearest the full-target end; interfaith_couples (constrained — emigration possible at cost) somewhat lower; reformists_and_apostates combine target-position costs with identity-locked exit, pushing effective extraction up despite the powerless power atom. state_recognition_authority is a near-symmetric-low special case: it collects legitimacy and fiscal savings without bearing the constraint's direct costs, but it bears the political risk of defending the arrangement. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms reproduce these relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — governing family law across religiously divided populations without majoritarian imposition — remains live wherever the arrangement operates, so this is not a mandatrophy case and no sunset applies. The classification discipline matters in both directions: calling the arrangement a pure rope (the manifest's seed hypothesis) would erase the dissenters whose costs the consent gate locks in; calling it a snare would erase the genuine coordination that keeps majority communities net-beneficiaries and would mispredict behavior, since majorities defend the arrangement voluntarily. Tangled rope keeps both halves on the books. The rising base_extractiveness series is the accumulation signal to watch: if the consent gate continues converting reform demand into judicial workaround and emigration, extraction accumulates without any change in the underlying norms — drift driven by the environment, not by renewed predation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (communal_autonomy_reading) of the contested kernel marriage_authority; which reading governs the arrangement, and what would each sibling reading change structurally?',
    'Constitutional-political development: enactment of a uniform civil code (secularist wins), consolidation of judicial equality floors (gender_rights or judicial_harmonization win), formal consociational entrenchment (federalist_millet wins), or continued consent-gated communal authorship (this reading holds).',
    'Switching readings relocates the authoring seat (communities vs legislature vs courts), deletes or preserves the consent gate, and redraws the victim set: under the secularist reading intra-community dissenters become ordinary litigants before a unified law; under this reading they are captive constituencies of communal institutions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: one reading of the marriage_authority kernel among five.').

omega_variable(
    disagreement_location_reform_channel,
    'Where exactly do the five readings disagree — is the dispute located in the locus of authoring authority, in the legitimate reform channel (community consent vs judicial or legislative override), or in both?',
    'Doctrinal analysis of what each sibling reading treats as non-negotiable: the secularist reading fixes the author (legislature exclusively), the gender-rights and harmonization readings fix the reform channel (courts may act without consent), the millet reading fixes the design rationale (deliberate anti-tyranny fragmentation), this reading fixes both locus (communities) and channel (consent-gated amendment).',
    'If the dispute is purely about the reform channel, partial settlements (judicial floors alongside communal authorship) are stable equilibria; if it is about the authoring locus itself, only one reading can ultimately survive, and intermediate arrangements are transitional rather than stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location_reform_channel, conceptual, 'Locates the structural element on which the kernel''s readings diverge.').

omega_variable(
    consent_gate_protective_or_extractive,
    'Is the community-consent requirement on legislative amendment a protective minority-rights mechanism or the enforcement core of religious-leadership rent?',
    'Comparative reform outcomes across communities and jurisdictions with and without consent gates: measure whether consent-gated systems show systematically slower rights-protective reform (divorce access, maintenance floors, custody parity) than otherwise similar non-gated systems, controlling for community conservatism.',
    'If protective, part of the measured extraction is the price of minority protection and the rope half of the arrangement is stronger than the metrics suggest; if extractive, the consent gate is the veto that locks in the payer seats'' position and the arrangement drifts toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_gate_protective_or_extractive, empirical, 'Whether the consent veto protects communities or extracts from their dissenters.').

omega_variable(
    dissenter_exit_genuineness,
    'Are the exit routes available to intra-community dissenters (conversion, foreign marriage, emigration) genuine alternatives or nominal ones priced out of reach?',
    'Take-up data: rates of foreign-marriage registration, conversion-for-marriage, and emigration among affected couples and women, plus revealed-cost studies of each route.',
    'If exits are genuine arbitrage, dissenters'' effective extraction is damped and the arrangement sits closer to the rope boundary; if nominal, the trapped and identity_locked exit atoms dominate and effective extraction for payer seats approaches the full-target end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissenter_exit_genuineness, empirical, 'Whether dissenter exit options are real substitutes or theoretical ones.').

omega_variable(
    membership_consensus_vs_identity_lock,
    'Does the compliant majority''s support for communal authorship reflect endorsement of the norms or identity lock that would persist even if endorsement faded?',
    'Attitude panel data separating stated preference for communal norms from stated preference for having communal institutions decide, tracked across generations and across exposure to civil-law alternatives.',
    'If support is preference-driven, the coordination half is robust and the arrangement is stable as a tangled rope; if identity lock does the work, the beneficiary base is softer than it appears and the arrangement''s persistence depends on enforcement plus fusion rather than consent — raising the weight of the suppression component in any recomputation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(membership_consensus_vs_identity_lock, conceptual, 'Whether majority participation is consensual coordination or identity-fused compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__communal_autonomy_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__communal_autonomy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__communal_autonomy_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t20, marriage_authority__communal_autonomy_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement_basis(marr_tr_t20, observed).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__communal_autonomy_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement_basis(marr_tr_t30, observed).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__communal_autonomy_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(marr_tr_t40, observed).
narrative_ontology:measurement(marr_tr_t50, marriage_authority__communal_autonomy_reading, theater_ratio, 50, 0.19).
narrative_ontology:measurement_basis(marr_tr_t50, observed).
narrative_ontology:measurement(marr_tr_t60, marriage_authority__communal_autonomy_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement_basis(marr_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__communal_autonomy_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t10, marriage_authority__communal_autonomy_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t20, marriage_authority__communal_autonomy_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(marr_be_t20, observed).
narrative_ontology:measurement(marr_be_t30, marriage_authority__communal_autonomy_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement_basis(marr_be_t30, observed).
narrative_ontology:measurement(marr_be_t40, marriage_authority__communal_autonomy_reading, base_extractiveness, 40, 0.51).
narrative_ontology:measurement_basis(marr_be_t40, observed).
narrative_ontology:measurement(marr_be_t50, marriage_authority__communal_autonomy_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement_basis(marr_be_t50, observed).
narrative_ontology:measurement(marr_be_t60, marriage_authority__communal_autonomy_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement_basis(marr_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__communal_autonomy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t10, marriage_authority__communal_autonomy_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t20, marriage_authority__communal_autonomy_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(marr_su_t20, observed).
narrative_ontology:measurement(marr_su_t30, marriage_authority__communal_autonomy_reading, suppression_requirement, 30, 0.59).
narrative_ontology:measurement_basis(marr_su_t30, observed).
narrative_ontology:measurement(marr_su_t40, marriage_authority__communal_autonomy_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(marr_su_t40, observed).
narrative_ontology:measurement(marr_su_t50, marriage_authority__communal_autonomy_reading, suppression_requirement, 50, 0.61).
narrative_ontology:measurement_basis(marr_su_t50, observed).
narrative_ontology:measurement(marr_su_t60, marriage_authority__communal_autonomy_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement_basis(marr_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__communal_autonomy_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'marriage authority in a plural state' decomposes into five readings of one kernel (marriage_authority), each a separate epsilon-invariant constraint: communal_autonomy_reading (this file — communities author, state enforces, consent-gated reform), secularist_reading (legislature authors exclusively; pluralism transitional), gender_rights_reading (judicially expanded equality overrides communal terms), federalist_millet_reading (fragmentation as deliberate consociational design), judicial_harmonization_reading (case-by-case constitutional floor without formal unification). The readings differ on the locus of authoring authority and the legitimate reform channel — not on observable selection — so each carries its own epsilon, beneficiaries, and victims; this file links all four siblings via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
