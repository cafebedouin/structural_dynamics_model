% ============================================================================
% CONSTRAINT STORY: marriage_authority__communal_autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_communal_autonomy_reading, []).

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
 *   human_readable: Communal Autonomy Reading: Religious Personal Law Enforced by the State
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   Under the communal autonomy reading, marriage authority is grounded in
 *   each community's religious tradition: communities author their own
 *   marriage, divorce, and succession norms, and the state enforces, through
 *   its courts, rules it did not write. Legislative amendment of the personal
 *   law requires community consent, which concentrates revision power in the
 *   very institutions the arrangement empowers. This file instantiates ONE
 *   reading of the contested kernel marriage_authority; the secularist,
 *   gender-rights, federalist-millet, and judicial-harmonization readings are
 *   separate constraints with their own epsilon values and victim sets,
 *   linked through the network section. Per the epsilon-referent rule,
 *   extractiveness here is authored for the standing communal personal-law
 *   arrangement as this reading itself assesses it: the reading endorses
 *   communal authorship and therefore credits the structure's coordination
 *   value while acknowledging the extractive edge it cannot deny, namely
 *   concentrated leadership rents and dissenters governed without a voice in
 *   revision. The manifest seed hypothesized a moderate-extraction rope;
 *   analysis refined the claim to tangled_rope because the structure exhibits
 *   BOTH a genuine coordination function and asymmetric extraction through
 *   the same machinery, with active enforcement holding it. Claim and metrics
 *   are independent authored facts.
 *
 * KEY AGENTS:
 *   - religious_leadership: agenda-setting beneficiary (institutional/arbitrage) — authors and administers communal norms, holds the amendment veto, collects adjudication and gatekeeping rents
 *   - state_judiciary: enforcing agenda-setter with incidental beneficiary position (institutional/constrained) — applies personal law it does not author; buys stability with deference
 *   - ordinary_community_members: dual-positioned rank-and-file (moderate/constrained) — receive identity continuity and dispute resolution, pay conformity costs and fees
 *   - intra_community_dissenters: primary target (moderate/trapped) — governed by rules they reject, with amendment blocked and exit socially catastrophic
 *   - women_under_personal_law: primary target (moderate/identity_locked) — bear the gendered incidence of divorce, maintenance, and succession rules regardless of personal endorsement
 *   - interfaith_couples: boundary-rule targets (moderate/trapped) — no domestic recognition channel; their blockage is the boundary function operating
 *   - civil_marriage_advocates: excluded voice (organized/constrained) — object outside a consent process that does not admit them
 *   - apex_constitutional_court: analytical observer (institutional/analytical) — enforces the settlement while incrementally testing it against constitutional guarantees
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, 0.4).
domain_priors:suppression_score(marriage_authority__communal_autonomy_reading, 0.62).
domain_priors:theater_ratio(marriage_authority__communal_autonomy_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__communal_autonomy_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__communal_autonomy_reading, "Communal Autonomy Reading: Religious Personal Law Enforced by the State").
narrative_ontology:topic_domain(marriage_authority__communal_autonomy_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__communal_autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__communal_autonomy_reading, '23757bdc-6b4d-4ca3-90c9-88a2f63748e4').
narrative_ontology:cs_kernel_codification('23757bdc-6b4d-4ca3-90c9-88a2f63748e4', formalized).
narrative_ontology:cs_authority_grounding('23757bdc-6b4d-4ca3-90c9-88a2f63748e4', lineage).
narrative_ontology:cs_interpretation_layer_present('23757bdc-6b4d-4ca3-90c9-88a2f63748e4').
narrative_ontology:cs_reading_relation('23757bdc-6b4d-4ca3-90c9-88a2f63748e4', marriage_authority__secularist_reading, forecloses).
narrative_ontology:cs_reading_relation('23757bdc-6b4d-4ca3-90c9-88a2f63748e4', marriage_authority__gender_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('23757bdc-6b4d-4ca3-90c9-88a2f63748e4', marriage_authority__federalist_millet_reading, coexists_with).
narrative_ontology:cs_reading_relation('23757bdc-6b4d-4ca3-90c9-88a2f63748e4', marriage_authority__judicial_harmonization_reading, forecloses).
narrative_ontology:cs_axiom('23757bdc-6b4d-4ca3-90c9-88a2f63748e4', foundational, communal_jurisdiction_is_primary).
narrative_ontology:cs_axiom_status(communal_jurisdiction_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('23757bdc-6b4d-4ca3-90c9-88a2f63748e4', communal_jurisdiction_is_primary, theological).
narrative_ontology:cs_axiom('23757bdc-6b4d-4ca3-90c9-88a2f63748e4', secondary, amendment_requires_community_consent).
narrative_ontology:cs_axiom_status(amendment_requires_community_consent, holdable).
narrative_ontology:cs_axiom_grounding('23757bdc-6b4d-4ca3-90c9-88a2f63748e4', amendment_requires_community_consent, conventional).
narrative_ontology:cs_reference_frame('23757bdc-6b4d-4ca3-90c9-88a2f63748e4', communal_traditional_authority).
narrative_ontology:cs_drift_state('23757bdc-6b4d-4ca3-90c9-88a2f63748e4', contemporary_constitutional_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('23757bdc-6b4d-4ca3-90c9-88a2f63748e4', '').
narrative_ontology:cs_kernel_id(marriage_authority__communal_autonomy_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, religious_leadership).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, intra_community_dissenters).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, women_under_personal_law).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, state_judiciary).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, ordinary_community_members).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, ordinary_community_members).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, interfaith_couples).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors and administers the communal normative order: interprets scripture and custom into operative marriage, divorce, and succession rules, adjudicates status disputes through community courts or recognized tribunals, and holds an effective veto over legislative amendment of the personal law. Collects adjudication fees, ceremonial income, and the standing influence that comes from controlling who counts as married, divorced, and legitimate. Its exit is not departure but repositioning: it invokes state enforcement when rulings favor it and religious autonomy when reform threatens it.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, religious_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% Applies and enforces whichever communal framework governs the parties before it while declining to author substantive family norms. Gains administrative relief and political stability from the settlement, since family-law conflict is handled by institutions with local legitimacy, and is bound by the constitutional settlement and precedent from unilaterally redesigning it.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, state_judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__communal_autonomy_reading, state_judiciary, beneficiary).

% Receive identity continuity, recognized ceremony, and familiar dispute resolution from the communal system, and reproduce it by marrying, registering, and litigating within it. They also fund its institutions, absorb its conformity pressures, and inherit its restrictions; drifting toward civil alternatives is possible but socially expensive.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, ordinary_community_members, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__communal_autonomy_reading, ordinary_community_members, payer).

% Reformers, skeptics, and minorities-within-the-community who reject portions of the communal law they remain governed by. They pay in foregone options (no civil marriage channel in classic forms of the system, amendment blocked without leadership consent) and in consequence: social ostracism, family rupture, sometimes loss of communal legal standing. Litigation and organizing are their main levers; full exit means abandoning the community that constitutes their world.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, intra_community_dissenters, payer,
    moderate, biographical, trapped, national).

% Bear the gendered incidence of the communal rules: divorce accessible to husbands on terms wives cannot invoke, maintenance and custody defaults, unequal shares in succession. Many endorse the tradition outright; the burdens fall regardless of endorsement, because their standing inside the community is constituted through the family roles the rules define. Leaving would dissolve the identity those roles anchor.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, women_under_personal_law, payer,
    moderate, biographical, identity_locked, national).

% Couples spanning communal boundaries find no domestic channel that recognizes their marriage under the settlement: each community's law gates its own members, and the civil alternative is absent or carries forfeiture of personal-law protections. Some marry abroad and import status; others cohabit unrecognized. Their blockage is the boundary-maintenance function operating as designed.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, interfaith_couples, payer,
    moderate, biographical, trapped, national).

% Secularist campaigners, gender-equality litigants, and minority-rights organizations arguing for an optional civil marriage channel and equality floors. They stand outside the consent process, since amendments require community consent and the communities speak through their leaderships, so their objections register in courts and elections but not where the rules are revised.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, civil_marriage_advocates, excluded,
    organized, biographical, constrained, national).

% Adjudicates collisions between personal law and constitutional guarantees case by case: enforcing the communal frameworks as the settlement requires while occasionally carving equality exceptions. It collects no fees and bears no conformity burdens; its incremental interventions are the pressure point through which rival understandings of marriage authority work.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, apex_constitutional_court, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__communal_autonomy_reading, religious_leadership).
narrative_ontology:fixing_cost_class(marriage_authority__communal_autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves two problems at once: inside each community, a single authoritative framework for marriage, divorce, legitimacy, and succession administered by adjudicators members recognize; across communities, peaceful coexistence under one sovereignty, since no community's practices are submitted to another's or to a majority-authored code.
% TRANSFER_FUNCTION: Moves authority over marital status and its incidents from individuals and the legislature to communal religious institutions; moves fees, deference, and compliance from community members to religious leadership; moves enforcement labor from the state's own norm-authoring apparatus to its courts applying borrowed rules.
% ABSENT_VOICES: Those governed most sharply by the rules have no seat where the rules are revised: intra-community dissenters, women seeking exits the communal law does not provide, interfaith couples, and civil-marriage advocates are all outside the consent process, which runs through community leaderships. Their objections surface in constitutional litigation and electoral politics, not in the amendment channel.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, every marriage, divorce, and succession in the affected communities would lose its governing framework simultaneously: courts would have no applicable law, pending status cases would collapse, and legislatures would face immediate pressure to author a uniform code, which is precisely the majoritarian consolidation the arrangement exists to prevent. Religious institutions would lose adjudication income and gatekeeping standing; dissenters would gain an open field.
% FOUNDING_PROBLEM: Communities with divergent religious marriage laws were brought under a single state and needed a settlement that preserved each community's status law without letting any community, or the new majority, legislate for the others; the arrangement answered the coexistence problem of the founding period.
% FOUNDING_PROBLEM_CORROBORATION: Constituent-assembly debates, colonial-era personal-law statutes, and comparative scholarship on millet-successor systems corroborate that the founding problem was real coexistence under one sovereignty. On status: minority-community leaderships and religious-freedom scholarship attest the problem is live; secularist legislators, gender-rights litigants, and uniform-code commissions attest from outside the benefiting parties that the problem as originally posed is solved or superseded and the arrangement now protects leadership position. The dispute itself is the finding; corroboration for the problem's existence is broad, corroboration that it still requires this arrangement is not.
narrative_ontology:disappearance_verdict(marriage_authority__communal_autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__communal_autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__communal_autonomy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__communal_autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__communal_autonomy_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored moderate (0.40) rather than high because the reading's own assessment credits the structure's real coordination output (recognized ceremony, legitimate adjudication, coexistence without a majority-imposed code) and locates extraction at the edges: leadership fee and gatekeeping rents, and the dissenters' foregone options. Suppression (0.62) exceeds extraction because the arrangement's persistence depends on machinery rather than preference: courts applying borrowed rules, community consequence against exit, and a consent requirement that converts every reform proposal into a negotiation with the benefited party. Theater is low (0.18): communal adjudication is mostly functional; the performative share is ceremonial ratification of outcomes already settled. Accessibility collapse is moderate (0.55): in classic forms there is no domestic civil-marriage channel, but migration, conversion, and opt-in civil statutes where enacted leave partial alternatives. Resistance (0.58) is sustained and organized (personal-law challenge litigation, equality campaigns, uniform-code politics) and is the main reason extraction has crept rather than jumped; the principal counterweight available to otherwise-weak dissenters is coalition formation with constitutional litigators and equality movements, which is why their power atom is moderate rather than powerless. Suppression is authored as a raw structural property and is not scaled by context; only extractiveness is scaled, by directionality and scope, in the engine. The temporal series share one seven-point grid (t=0,12,24,36,48,60,72) so every tracked metric is authored at every examined point. Suppression_requirement is tracked because the story's dynamic includes enforcement-capacity change: the consent convention hardened over the interval as failed reform attempts taught leaderships that entrenchment pays. All series rise gently, modeling extraction accumulation without crisis; end-state values equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the religious-leadership seat the arrangement is legitimate self-governance it staffs and defends; from the dissenters' and women's seats the same machinery is enforced subordination with the exit doors welded shut; from the state bench it is neutrality-by-deferral; from the excluded advocates' seat it is a closed loop in which the governed cannot reach the governors. The engine derives these divergences from the declared positions and exits; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   religious_leadership is declared beneficiary and holds arbitrage-grade repositioning, placing it near the beneficiary end of d: effective extraction damps toward subsidy, which is accurate, since the arrangement pays it. intra_community_dissenters and women_under_personal_law are declared victims with trapped and identity_locked exit respectively, placing them near the full-target end; identity lock matters here because the community constitutes the members' standing, so exit is not merely costly but self-dissolving. ordinary_community_members sit near symmetric: real coordination benefit, real conformity cost. state_judiciary draws a mild beneficiary tilt through its stability dividend despite paying enforcement labor. interfaith_couples are targets of the boundary function specifically. No directionality overrides are used: the beneficiary/victim declarations plus exit atoms already produce the structural relationships, and the override surface is keyed to power atoms, which would smear corrections across unrelated institutional seats sharing the same atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, coexistence among religious communities under one sovereignty, is genuinely contested rather than dead: minority communities attest it live, secularist and equality constituencies attest it superseded. Because founding_problem_status is contested and disappearance_verdict is world_rearranges, the mismatch consumer finds no zombie signature, and the low theater ratio corroborates functional persistence rather than ritual maintenance. The tangled_rope classification is what keeps the analysis honest in both directions: a pure-rope reading would erase the dissenters' and women's measurable burden behind the coordination story; a pure-snare reading would erase the coordination value ordinary members plainly receive and would mispredict the arrangement's resilience. Mandatrophy resolution here is not obsolescence but contested continuation: the mandate survives because enough parties still fear the alternative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the communal_autonomy_reading of the marriage_authority kernel; would any sibling reading change the structural classification?',
    'Not resolvable by data internal to this story: each sibling reading is a separate constraint file with its own epsilon and victim set; the contest resolves only through jurisprudential and political outcomes such as uniform-code enactment, equality jurisprudence, or consociational redesign.',
    'If the secularist reading prevailed, this constraint''s beneficiary structure dissolves into a legislative monopoly with communal dissenters relieved; if the gender-rights reading prevailed, the harmed set shifts to those the equality floor protects imperfectly; if the federalist-millet reading prevailed, the same plural structure is re-justified and epsilon likely falls.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Reading-contest routing: classification is valid for this reading of the kernel only.').

omega_variable(
    consent_veto_asymmetry,
    'Does the community-consent requirement for legislative amendment bind all communities uniformly, or does it protect minority personal law while the majority community''s law was codified without equivalent consent?',
    'Comparative statutory history: identify which communities'' laws were consolidated by ordinary majority legislation and which retain non-codified, consent-gated status.',
    'If asymmetric, the burden concentrates on minority-community dissenters who lack both internal voice and the majority''s statutory exit, pushing epsilon upward and the classification toward snare for the asymmetric variant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_veto_asymmetry, empirical, 'Whether the consent veto is symmetrical across communities.').

omega_variable(
    suppression_mechanism_split,
    'Is the dissenters'' non-exit structural (no civil channel, legal disability, economic dependence) or internalized (identity fusion making exit unthinkable)?',
    'Post-exit suppression trajectory: track dissenters who do exit through conversion, migration, or civil-statute opt-ins; if reported suppression persists after the barriers are removed, a substantial share is internalized.',
    'If substantially internalized, effective suppression exceeds the structural measure because the constraint travels with its targets, and remediation aimed only at legal channels underperforms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized share of measured suppression.').

omega_variable(
    identity_framing_cover_risk,
    'Is the identity_coordination framing genuine boundary maintenance that members value, or cover under which leadership position is protected?',
    'Revealed-preference test: offer an equivalent civil channel (ceremony recognition, dispute resolution, succession defaults) at comparable cost and observe uptake; sustained mass preference for communal forums indicates genuine identity coordination, elite-only use indicates cover.',
    'If cover, the identity-coordination complexity offset is being exploited and excess extraction above the type floor should be flagged for review rather than tolerated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_framing_cover_risk, conceptual, 'Whether identity framing justifies the coupling or masks extraction.').

omega_variable(
    epsilon_decomposition_risk,
    'Does epsilon remain invariant across observables, or do status-rule restriction (divorce access, succession shares) and leadership-rent capture (fees, gatekeeping) yield different extraction pictures?',
    'If the two observables stabilize at materially different epsilon values, decompose per the epsilon-invariance principle into a status-rules constraint and a leadership-economy constraint, linked by network edges.',
    'Decomposition would isolate the gendered status-rule component (likely higher epsilon, sharper victim set) from the institutional-economy component (lower epsilon, leadership-centered), changing per-seat classifications.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epsilon_decomposition_risk, empirical, 'Observable-stability check guarding epsilon invariance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__communal_autonomy_reading, 0, 72).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marriage_authority_communal_tr_t0, marriage_authority__communal_autonomy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(marriage_authority_communal_tr_t12, marriage_authority__communal_autonomy_reading, theater_ratio, 12, 0.11).
narrative_ontology:measurement(marriage_authority_communal_tr_t24, marriage_authority__communal_autonomy_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement(marriage_authority_communal_tr_t36, marriage_authority__communal_autonomy_reading, theater_ratio, 36, 0.14).
narrative_ontology:measurement(marriage_authority_communal_tr_t48, marriage_authority__communal_autonomy_reading, theater_ratio, 48, 0.15).
narrative_ontology:measurement(marriage_authority_communal_tr_t60, marriage_authority__communal_autonomy_reading, theater_ratio, 60, 0.17).
narrative_ontology:measurement(marriage_authority_communal_tr_t72, marriage_authority__communal_autonomy_reading, theater_ratio, 72, 0.18).

% Extraction over time
narrative_ontology:measurement(marriage_authority_communal_be_t0, marriage_authority__communal_autonomy_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(marriage_authority_communal_be_t12, marriage_authority__communal_autonomy_reading, base_extractiveness, 12, 0.33).
narrative_ontology:measurement(marriage_authority_communal_be_t24, marriage_authority__communal_autonomy_reading, base_extractiveness, 24, 0.35).
narrative_ontology:measurement(marriage_authority_communal_be_t36, marriage_authority__communal_autonomy_reading, base_extractiveness, 36, 0.36).
narrative_ontology:measurement(marriage_authority_communal_be_t48, marriage_authority__communal_autonomy_reading, base_extractiveness, 48, 0.38).
narrative_ontology:measurement(marriage_authority_communal_be_t60, marriage_authority__communal_autonomy_reading, base_extractiveness, 60, 0.39).
narrative_ontology:measurement(marriage_authority_communal_be_t72, marriage_authority__communal_autonomy_reading, base_extractiveness, 72, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(marriage_authority_communal_su_t0, marriage_authority__communal_autonomy_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(marriage_authority_communal_su_t12, marriage_authority__communal_autonomy_reading, suppression_requirement, 12, 0.54).
narrative_ontology:measurement(marriage_authority_communal_su_t24, marriage_authority__communal_autonomy_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(marriage_authority_communal_su_t36, marriage_authority__communal_autonomy_reading, suppression_requirement, 36, 0.58).
narrative_ontology:measurement(marriage_authority_communal_su_t48, marriage_authority__communal_autonomy_reading, suppression_requirement, 48, 0.59).
narrative_ontology:measurement(marriage_authority_communal_su_t60, marriage_authority__communal_autonomy_reading, suppression_requirement, 60, 0.61).
narrative_ontology:measurement(marriage_authority_communal_su_t72, marriage_authority__communal_autonomy_reading, suppression_requirement, 72, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__communal_autonomy_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'religious family law' conflates five structurally distinct claims about one kernel. This file is the communal-autonomy member of the family: epsilon is indexed to the standing communal arrangement as this reading assesses it (moderate), with religious_leadership as beneficiary and intra-community dissenters and women under personal law as victims. The secularist sibling authors high epsilon for the same standing arrangement (anomaly awaiting elimination); the federalist-millet sibling authors low epsilon (deliberate anti-tyranny design); the gender-rights and judicial-harmonization siblings author high epsilon with gendered victim sets. Structural flow: the federalist-millet reading supplies the neutral consociational rationale that stabilizes this reading's legitimacy conditions; the gender-rights and judicial-harmonization readings operate downstream as pressure on it. Family members are linked pairwise through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
