% ============================================================================
% CONSTRAINT STORY: marriage_authority__communal_autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Communal Autonomy Reading of Marriage Authority (State-Enforced Religious Personal Law)
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   In polities that legally recognize religious communities, marriage,
 *   divorce, and succession are governed by each community's own tradition:
 *   tribunals and councils issue rulings, and the state's registries and
 *   courts enforce them without drafting the substantive norms. This story
 *   instantiates ONE reading of the contested marriage_authority kernel — the
 *   communal autonomy reading — which holds that communities possess
 *   antecedent jurisdiction over intimate life and that state abstention from
 *   authorship is what protects religious liberty. The epsilon referent is
 *   the standing pluralist arrangement as this reading assesses it:
 *   sympathetic to the arrangement overall, yet unable to deny that
 *   intra-community dissenters and interfaith couples bear uncompensated
 *   costs — hence moderate epsilon rather than near-zero. The generation
 *   manifest labeled the hypothesis 'rope'; the authored claim is
 *   tangled_rope because the structural data (named beneficiaries AND victims
 *   plus required active enforcement) instantiate the hybrid
 *   coordination/extraction shape that category exists to capture — the
 *   refinement is recorded in uke_scope and here, not reconciled away.
 *   Sibling readings (secularist, gender_rights, federalist_millet,
 *   judicial_harmonization) are separate constraint files linked via
 *   network.affects_constraints; no sibling content is folded into this story
 *   beyond the omega variables and reading relations that locate it in the
 *   kernel contest. KEY AGENTS (by structural relationship): -
 *   religious_leadership: Primary beneficiary and norm-authoring
 *   agenda-setter (institutional/identity_locked) — collects jurisdiction,
 *   status, and a legislative consent veto - state_enforcement_institutions:
 *   Enforcing agenda-setter (institutional/constrained) — applies communal
 *   rulings, collects delegated governance, absorbs legitimacy costs -
 *   observant_community_laity: Net beneficiary with compliance costs
 *   (organized/constrained) - intra_community_dissenters: Primary target
 *   (powerless/trapped) — bound to norms they did not choose -
 *   interfaith_couples: Secondary target (powerless/trapped) — no recognized
 *   marriage path across community lines - civil_marriage_advocates: Excluded
 *   voice (organized/trapped) — locked out of amendment consultations -
 *   international_human_rights_bodies: Analytical observer
 *   (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, 0.44).
domain_priors:suppression_score(marriage_authority__communal_autonomy_reading, 0.58).
domain_priors:theater_ratio(marriage_authority__communal_autonomy_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__communal_autonomy_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__communal_autonomy_reading, "Communal Autonomy Reading of Marriage Authority (State-Enforced Religious Personal Law)").
narrative_ontology:topic_domain(marriage_authority__communal_autonomy_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__communal_autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__communal_autonomy_reading, '244bacee-607c-4dcc-b714-5c31435083b1').
narrative_ontology:cs_kernel_codification('244bacee-607c-4dcc-b714-5c31435083b1', distributed).
narrative_ontology:cs_authority_grounding('244bacee-607c-4dcc-b714-5c31435083b1', lineage).
narrative_ontology:cs_interpretation_layer_present('244bacee-607c-4dcc-b714-5c31435083b1').
narrative_ontology:cs_reading_relation('244bacee-607c-4dcc-b714-5c31435083b1', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('244bacee-607c-4dcc-b714-5c31435083b1', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('244bacee-607c-4dcc-b714-5c31435083b1', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('244bacee-607c-4dcc-b714-5c31435083b1', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('244bacee-607c-4dcc-b714-5c31435083b1', foundational, communal_traditional_authority_is_antecedent).
narrative_ontology:cs_axiom_status(communal_traditional_authority_is_antecedent, holdable).
narrative_ontology:cs_axiom_grounding('244bacee-607c-4dcc-b714-5c31435083b1', communal_traditional_authority_is_antecedent, conventional).
narrative_ontology:cs_axiom('244bacee-607c-4dcc-b714-5c31435083b1', secondary, state_abstention_from_authorship_protects_liberty).
narrative_ontology:cs_axiom_status(state_abstention_from_authorship_protects_liberty, holdable).
narrative_ontology:cs_axiom_grounding('244bacee-607c-4dcc-b714-5c31435083b1', state_abstention_from_authorship_protects_liberty, instrumental).
narrative_ontology:cs_reference_frame('244bacee-607c-4dcc-b714-5c31435083b1', communal_personal_status_autonomy).
narrative_ontology:cs_drift_state('244bacee-607c-4dcc-b714-5c31435083b1', contemporary, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('244bacee-607c-4dcc-b714-5c31435083b1', '').
narrative_ontology:cs_kernel_id(marriage_authority__communal_autonomy_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, religious_leadership).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, observant_community_laity).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, state_enforcement_institutions).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, intra_community_dissenters).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, interfaith_couples).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, observant_community_laity).
narrative_ontology:constraint_vindicates(marriage_authority__communal_autonomy_reading, communal_self_governance_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__communal_autonomy_reading, state_non_authorship_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and administers marriage, divorce, and succession according to the community's religious tradition; staffs the tribunals whose rulings the state's registries and courts carry into effect. Holds a consent veto over legislative amendments to personal law. Office, livelihood, and social standing are constituted by the arrangement itself; abandoning it would mean dissolving the very authority the office exists to exercise.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, religious_leadership, agenda_setter,
    institutional, generational, identity_locked, national).

% Registers communal rulings, adjudicates disputes by applying the relevant community's law, and provides coercive backup (court orders, police execution) behind tribunal decisions. Deliberately refrains from drafting substantive family norms and routes amendment proposals back to community bodies. Gains inexpensive, legitimate governance of intimate life across a diverse population; absorbs public criticism whenever an enforced ruling violates widely held equality expectations.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, state_enforcement_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Marries, divorces, and inherits under familiar traditional rules administered by trusted co-religionists. Receives continuity of identity and predictability in intimate life. Pays tribunal fees and tithes, accepts restrictions on intermarriage and remarriage as the price of belonging, and can leave only by losing the community that anchors family and social life.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, observant_community_laity, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__communal_autonomy_reading, observant_community_laity, payer).

% Members — disproportionately women, reformists, and the secular-minded — whose preferred marriage or divorce terms differ from the community's rules. Their cases are decided by tribunals applying rules they reject; civil-law alternatives are unavailable or unrecognized; exit means apostasy with loss of family ties, custody standing, and social world. Some litigate through state courts, which partially defer back to the communal forums.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, intra_community_dissenters, payer,
    powerless, biographical, trapped, national).

% Partners from different communities who cannot marry across community lines unless one side converts or submits to the stricter community's jurisdiction. Children's communal affiliation and legitimacy are contested. Relocation abroad or clandestine unions are the remaining paths, both carrying legal limbo at home.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, interfaith_couples, payer,
    powerless, biographical, trapped, national).

% Campaign for a universally available civil marriage option outside every community's jurisdiction. Hold no seat in amendment negotiations, which run exclusively through recognized religious leadership; their proposals routinely fail in committee. Many are themselves community members unable to marry civilly.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, civil_marriage_advocates, excluded,
    organized, biographical, trapped, national).

% Review the state's treaty compliance on freedom of religion, equality, and family rights; document tensions between communal autonomy and individual guarantees; issue recommendations and periodic reports. Hold no direct enforcement power over the arrangement; their findings feed domestic litigation and diplomatic pressure.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__communal_autonomy_reading, religious_leadership).
narrative_ontology:fixing_cost_class(marriage_authority__communal_autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains continuity of communal identity across generations and provides marriage, divorce, and succession governance that members trust and understand; historically it governed family formation across a religiously heterogeneous population where the state lacked legitimacy or capacity to impose a uniform code, shielding minority communities from majoritarian assimilation.
% TRANSFER_FUNCTION: Moves adjudicative authority and public legitimacy from individuals and the state to communal religious institutions; moves compliance costs onto dissenting members; channels state coercive power toward enforcing rulings the state did not author; moves status, income, and a consent veto over legislation to religious leadership.
% ABSENT_VOICES: Dissenting members, women whose testimony carries reduced weight in traditional forums, former members who absorbed apostasy costs, and civil-marriage advocates would object if seated; they are absent because amendment consultation runs exclusively through recognized religious leadership, so unanimity around the arrangement partly reflects who was never invited.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, large volumes of pending marriage, divorce, and succession cases would lose their governing law; community tribunals and leadership offices would lose their juridical basis; the state would have to improvise transitional codes under crisis conditions; interfaith couples would immediately gain access to recognized marriage — a wholesale rearrangement of intimate-life governance.
% FOUNDING_PROBLEM: Governing marriage, divorce, and succession across a religiously heterogeneous population without forcing assimilation to a majority code — protecting minority communities' intimate life from majoritarian domination at a time when the state could not credibly legislate family law uniformly.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on millet and colonial personal-law regimes corroborates the founding problem's existence and its original protective purpose, and state constitutional-debate archives corroborate it independently of the beneficiary set. Current liveness splits along the kernel contest: religious leadership attests the problem is live (communities still need protection); intra-community dissenters and international human rights treaty bodies attest it is superseded by citizenship-based equality guarantees. Genealogy is externally corroborated; the liveness verdict is disputed.
narrative_ontology:disappearance_verdict(marriage_authority__communal_autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__communal_autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__communal_autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__communal_autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__communal_autonomy_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__communal_autonomy_reading_tests).
:- end_tests(marriage_authority__communal_autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim/metric independence: claimed_type=tangled_rope states what the structure shows — a genuine coordination function (identity continuity, trusted adjudication, historical minority protection) fused with asymmetric extraction (leadership-captured jurisdiction, dissenters bound without consent), held together by active enforcement. Metrics are authored descriptively, independently of the claim. Extractiveness 0.44: this reading weighs communal liberty heavily, so the dissenters' burden registers as a real but partial deduction from legitimacy, not disqualification — consistent with the manifest's epsilon_moderate bin. Suppression 0.58 is a raw structural property, unscaled by power or scope: state coercive backup plus blocked civil alternatives plus social exit costs. Theater 0.24: tribunals perform real adjudication; a minority of activity is ceremonial maintenance of leadership prestige. Accessibility collapse 0.62: civil-marriage, exit, and state-override alternatives are degraded but not eliminated — this is not a natural law and alternatives visibly persist. Resistance 0.55: sustained reform litigation and cross-community women's movements — the coalition channel through which individually powerless dissenters aggregate leverage. The measurement series share one grid (t=0..50, step 10) across all three tracked metrics. Rising base_extractiveness and suppression_requirement model the codification ratchet: written codes freeze interpretations that living tradition might have relaxed, and each codification round hardens enforcement infrastructure — this is why suppression_requirement is authored at all (enforcement-capacity change is the traced dynamic, not a static backdrop). Theater drifts up slowly as parallel civil options grow and some council activity turns symbolic. The trajectory is a monotone ratchet with episodic reform bumps smoothed into the decadal grid; no oscillatory cycle is asserted.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute materially different classifications from identical structural data. The religious_leadership seat sits near the beneficiary pole with identity_locked exit: the arrangement subsidizes its office, and its holders experience it as legitimate tradition — a coordination-dominant, rope-flavored computation. The observant_community_laity seat nets positive but carries compliance costs and constrained exit: mild subsidy. The state seat is mixed: it collects delegated governance but pays legitimacy costs whenever an enforced ruling offends equality expectations — near symmetric. The intra_community_dissenters and interfaith_couples seats are trapped targets with amplified effective extraction — snare-flavored computations of the same tribunals the laity experiences as care. Same-level divergence: civil_marriage_advocates (organized, trapped) and observant_community_laity (organized, constrained) hold comparable resources and standing yet opposite directionalities — what separates them is position relative to the arrangement, not power. The engine computes these per-seat results; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive d downward: religious_leadership (identity_locked, institutional) derives near the beneficiary pole — the arrangement is the office; observant_community_laity (constrained) derives low-but-not-zero — net benefit with real compliance costs; state_enforcement_institutions, declared beneficiary via delegated governance yet bearing documented enforcement-legitimacy costs, derives mildly subsidized rather than deeply so. Victim declarations drive d upward: intra_community_dissenters (trapped) derive near full target; interfaith_couples (trapped, with no recognized path whatsoever) nearer still. Excluded and observer seats take neutral/analytical treatment. No directionality_overrides are used: the derivation chain from beneficiary/victim declarations plus exit options reproduces every seat's true position, and the two agenda_setters are differentiated by their declarations (leadership as pure beneficiary, state as beneficiary with cost absorption) rather than by overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification blocks two symmetric mislabels. Reading the arrangement as pure snare would erase its genuine coordination achievements — identity continuity across generations, adjudication members actually prefer, historical protection of minorities from majoritarian family codes — licensing abolitionist remedies that harm the net-beneficiary majority of community members. Reading it as pure rope would erase the dissenters' uncompensated burden and the leadership's consent veto that entrenches it. Tangled rope holds both truths: the same tribunal that comforts the observant laity binds the dissenter who never consented. On obsolescence: the founding problem (governing family law across a religiously heterogeneous population without forced assimilation) remains partially live, so the arrangement has not outlived its mandate wholesale — founding_problem_status 'contested', and the R5 mismatch consumer finds no dead-problem/world_rearranges contradiction. The rising extractiveness series nonetheless flags accumulation: the consent-veto mechanism lets leadership convert coordination capital into entrenched position faster than dissenters' exit widens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_status_of_membership,
    'Does membership in the community constitute informed consent to its family law, or are dissenters bound by an arrangement they never chose?',
    'Measure realistic exit costs: availability of recognized civil marriage, feasibility of forming breakaway congregations, practical apostasy penalties, and whether dissenters can contract out case-by-case without forfeiting family standing.',
    'If consent is genuine, the costs dissenters bear are accepted coordination prices and epsilon falls toward a rope profile; if exit is fictive, the burden falls on non-consenting parties and epsilon rises toward a snare profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_status_of_membership, empirical, 'Whether communal membership grounds consent to communal family law.').

omega_variable(
    kernel_reading_displacement,
    'This constraint is one reading of the marriage_authority kernel — which sibling reading displaces it, and what structural change follows?',
    'Constitutional adjudication, legislative action on a uniform civil code, and electoral realignment among the five declared readings; the engine tracks foreclosure signals across the sibling set via the linked network.',
    'Secularist displacement removes the victim set by replacing pluralism with a single legislature-authored code; gender_rights displacement converts dissenters'' claims into enforceable equality floors inside communal jurisdiction; federalist consolidation hardens the community consent veto and deepens the present structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_displacement, conceptual, 'Kernel contest: which reading of marriage authority prevails.').

omega_variable(
    codification_freeze_effect,
    'Does state enforcement freeze community norms that living tradition would otherwise have evolved, so that part of the burden dissenters bear is attributable to the enforcement choice rather than to the tradition itself?',
    'Compare interpretive flexibility in communities under state-enforced personal law against analogous communities in jurisdictions without enforcement (diaspora comparisons); measure doctrinal change rates before and after codification rounds.',
    'If enforcement freezes norms, responsibility shifts partially to the state''s design and withdrawing enforcement becomes a remedy short of abolishing communal authority; if norms are stable either way, the burden is internal to the tradition and the reading''s defense stands stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(codification_freeze_effect, conceptual, 'Whether state enforcement alters the norm content it enforces.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is dissenters'' compliance with communal rulings maintained by external enforcement (state backing, blocked civil alternatives) or by internalized obligation (the belief that contesting communal law betrays the community)?',
    'Post-intervention trajectories: whether dissenters assert individual rights once a state floor exists, without further enforcement; surveys of members who relocate to jurisdictions without communal enforcement.',
    'If internalized, effective suppression exceeds the structural measure and persists after enforcement withdrawal; if structural, removing state backing liberates dissenters quickly and the measured suppression overstates durable constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized maintenance of compliance among dissenters.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__communal_autonomy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__communal_autonomy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__communal_autonomy_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(marr_tr_t20, marriage_authority__communal_autonomy_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__communal_autonomy_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__communal_autonomy_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement(marr_tr_t50, marriage_authority__communal_autonomy_reading, theater_ratio, 50, 0.24).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__communal_autonomy_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(marr_be_t10, marriage_authority__communal_autonomy_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(marr_be_t20, marriage_authority__communal_autonomy_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(marr_be_t30, marriage_authority__communal_autonomy_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(marr_be_t40, marriage_authority__communal_autonomy_reading, base_extractiveness, 40, 0.43).
narrative_ontology:measurement(marr_be_t50, marriage_authority__communal_autonomy_reading, base_extractiveness, 50, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__communal_autonomy_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(marr_su_t10, marriage_authority__communal_autonomy_reading, suppression_requirement, 10, 0.49).
narrative_ontology:measurement(marr_su_t20, marriage_authority__communal_autonomy_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(marr_su_t30, marriage_authority__communal_autonomy_reading, suppression_requirement, 30, 0.56).
narrative_ontology:measurement(marr_su_t40, marriage_authority__communal_autonomy_reading, suppression_requirement, 40, 0.57).
narrative_ontology:measurement(marr_su_t50, marriage_authority__communal_autonomy_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__communal_autonomy_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the marriage_authority kernel: this file instantiates the communal_autonomy_reading only. The colloquial label 'who governs marriage' covers five structurally distinct claims with different beneficiary/victim sets and different epsilon values; per the epsilon-invariance principle each reading is a separate constraint, linked here. Upstream/downstream: the federalist_millet reading draws legitimacy from this reading's demonstrated operation (this -> federalist, influences), while judicial_harmonization and gender_rights erode this reading's jurisdiction from adjacent seats (mutual influence edges recorded on both sides). Sibling files: marriage_authority__secularist_reading, marriage_authority__gender_rights_reading, marriage_authority__federalist_millet_reading, marriage_authority__judicial_harmonization_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
