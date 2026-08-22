% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__sovereignty_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__sovereignty_restoration_reading, []).

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
 *   constraint_id: nsl_legal_text__sovereignty_restoration_reading
 *   human_readable: National Security Law as Sovereignty-Restoring Security Instrument
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This story instantiates the sovereignty-restoration reading of the
 *   National Security Law kernel: the law is understood, from this reading's
 *   own lights, as a legitimate exercise of sovereign authority to close a
 *   genuine security gap exposed by the 2019 unrest, restoring constitutional
 *   order rather than enclosing democratic space. Under this reading the
 *   coordination function is real — a jurisdiction facing sustained unrest
 *   and alleged foreign coordination gained a domestic legal mechanism to
 *   address secession, subversion, terrorism, and collusion — but the same
 *   structure that performs that coordination also transfers political and
 *   civic space away from a defined set of targets: 2019-era protest
 *   organizers, opposition politicians, and independent press whose conduct
 *   is read as having crossed from protest into security threat. This is why
 *   the claimed type is tangled_rope rather than rope: even by this reading's
 *   own account, the law both solves a coordination problem AND runs a
 *   directed transfer through the same enforcement apparatus, with named
 *   victims and a beneficiary coalition, requiring active enforcement (the
 *   security committee, designated courts, disqualification mechanisms) to
 *   hold. This reading's ε is authored at moderate rather than low precisely
 *   because, unlike a story that denied any extractive component, this
 *   reading concedes a real transfer onto political opposition while
 *   insisting the transfer is bounded, threat-proportionate, and distinct
 *   from the extractiveness a democratic-enclosure or jurisdictional-capture
 *   reading would author for the same standing arrangement. The referent for
 *   all three sibling readings is the same standing arrangement — the enacted
 *   and enforced National Security Law — evaluated by each reading's own
 *   lights; this reading's ε reflects its own account of who is targeted and
 *   why, not the enclosure or capture readings' higher figures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, 0.52).
domain_priors:suppression_score(nsl_legal_text__sovereignty_restoration_reading, 0.68).
domain_priors:theater_ratio(nsl_legal_text__sovereignty_restoration_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__sovereignty_restoration_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__sovereignty_restoration_reading, "National Security Law as Sovereignty-Restoring Security Instrument").
narrative_ontology:topic_domain(nsl_legal_text__sovereignty_restoration_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__sovereignty_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__sovereignty_restoration_reading, 'e8366201-4a18-43e5-9279-6f4e9964c286').
narrative_ontology:cs_kernel_codification('e8366201-4a18-43e5-9279-6f4e9964c286', formalized).
narrative_ontology:cs_authority_grounding('e8366201-4a18-43e5-9279-6f4e9964c286', extraction).
narrative_ontology:cs_interpretation_layer_present('e8366201-4a18-43e5-9279-6f4e9964c286').
narrative_ontology:cs_reading_relation('e8366201-4a18-43e5-9279-6f4e9964c286', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('e8366201-4a18-43e5-9279-6f4e9964c286', nsl_legal_text__jurisdictional_capture_reading, coexists_with).
narrative_ontology:cs_axiom('e8366201-4a18-43e5-9279-6f4e9964c286', foundational, sovereign_security_response_legitimate).
narrative_ontology:cs_axiom_status(sovereign_security_response_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('e8366201-4a18-43e5-9279-6f4e9964c286', sovereign_security_response_legitimate, empirically_contingent).
narrative_ontology:cs_axiom('e8366201-4a18-43e5-9279-6f4e9964c286', secondary, constitutional_order_precedes_procedural_liberty).
narrative_ontology:cs_axiom_status(constitutional_order_precedes_procedural_liberty, holdable).
narrative_ontology:cs_axiom_grounding('e8366201-4a18-43e5-9279-6f4e9964c286', constitutional_order_precedes_procedural_liberty, instrumental).
narrative_ontology:cs_reference_frame('e8366201-4a18-43e5-9279-6f4e9964c286', pre_2019_constitutional_settlement).
narrative_ontology:cs_drift_state('e8366201-4a18-43e5-9279-6f4e9964c286', post_enactment_enforcement_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e8366201-4a18-43e5-9279-6f4e9964c286', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, central_sovereign_authority).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, committee_for_safeguarding_national_security).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, pro_establishment_political_bloc).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, resident_population_seeking_stability).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_activists).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, protest_organizers).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, independent_journalists).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, opposition_politicians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, resident_population_seeking_stability).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, judiciary).
narrative_ontology:constraint_vindicates(nsl_legal_text__sovereignty_restoration_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(nsl_legal_text__sovereignty_restoration_reading, constitutional_order_restoration_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the law's provisions, determines which acts constitute secession, subversion, terrorism, or collusion with foreign forces, and whose decisions on national security matters are stated to be not amenable to judicial review. Operates with a mandate that the 2019 unrest constituted an existential threat to constitutional order requiring this instrument.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, committee_for_safeguarding_national_security, agenda_setter,
    institutional, generational, analytical, national).

% Holds that the 2019 unrest exposed a gap in the region's constitutional order that only a sovereign security law could close, consistent with the 'one country' half of the governing framework. Gains restored administrative and political control over a jurisdiction it regards as having drifted toward instability and foreign interference.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, central_sovereign_authority, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__sovereignty_restoration_reading, central_sovereign_authority, agenda_setter).

% Operates within a political environment stabilized by the law's removal of disruptive rival factions from legislative and civic contest. Benefits from reduced electoral and street-level competition and from being able to govern without the disruption the 2019 unrest represented.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, pro_establishment_political_bloc, beneficiary,
    organized, generational, mobile, national).

% Experienced the 2019 disruption to transport, commerce, and daily life directly and credits the law with restoring order and predictability. Some in this group also feel constrained in what they can say publicly, but weigh this against the return of functioning streets, courts, and markets.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, resident_population_seeking_stability, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__sovereignty_restoration_reading, resident_population_seeking_stability, payer).

% From the perspective of this reading, individuals who crossed from legitimate protest into acts the law defines as subversion or collusion bear the consequences of that crossing — prosecution, disqualification from office, or exile. Exit is largely unavailable without abandoning political identity or physical presence in the jurisdiction.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_activists, payer,
    powerless, biographical, trapped, local).

% Organizations and individuals who mobilized the 2019 demonstrations are treated, under this reading, as having tested the boundary between protest and insurrection; many face prosecution or dissolution. This reading regards their treatment as the necessary consequence of the security threat they posed, not as suppression of ordinary politics.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, protest_organizers, payer,
    powerless, biographical, trapped, local).

% Reporting that a security-focused reading characterizes as amplifying the 2019 unrest or foreign-linked destabilization efforts now carries legal risk. Some outlets closed; others self-censor or relocate operations, which this reading frames as a proportionate response to specific security-relevant conduct rather than blanket suppression.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, independent_journalists, payer,
    moderate, biographical, constrained, national).

% Legislators and candidates whose 2019-era positions or affiliations are read as having crossed into subversion have been disqualified, prosecuted, or driven from public life. This reading treats their removal as restoring a legislature capable of functioning within constitutional bounds rather than as elimination of legitimate opposition.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, opposition_politicians, payer,
    moderate, biographical, constrained, national).

% Criticized the law's scope and application from outside the jurisdiction. This reading treats their objections as illegitimate interference in a sovereign security matter and gives them no standing in the domestic legitimacy conversation, though their statements shape international sanctions and diplomatic posture.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, foreign_governments_and_ngos, excluded,
    powerful, generational, arbitrage, global).

% Adjudicates individual cases under the law's procedures, including provisions for closed proceedings and designated judges in security cases. Retains ordinary common-law function in non-security matters but operates within a security carve-out this reading holds to be a legitimate and narrow exception justified by the 2019 emergency.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, judiciary, observer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__sovereignty_restoration_reading, judiciary, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__sovereignty_restoration_reading, central_sovereign_authority).
narrative_ontology:fixing_cost_class(nsl_legal_text__sovereignty_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the jurisdiction's authorities with a legal instrument to identify, prosecute, and deter acts of secession, subversion, terrorism, and foreign collusion, closing what this reading identifies as a genuine gap exposed by the 2019 unrest in which sustained, sometimes violent unrest and alleged foreign involvement threatened constitutional order and public safety.
% TRANSFER_FUNCTION: Moves political and civic space away from opposition organizers, independent press, and street-level dissent, and toward central sovereign authorities and the security-review committee, in exchange for (in this reading's account) restored public order, functioning commerce, and diplomatic/administrative stability for the resident population and pro-establishment bloc.
% ABSENT_VOICES: The 2019 protest movement's rank-and-file participants, and the international governments/NGOs that contest the law's proportionality, are treated in this reading as either security threats or illegitimate external interference and are structurally excluded from the domestic legitimacy conversation the law's authority draws on.
% DISAPPEARANCE_RATIONALE: If the law were repealed overnight, this reading holds that the jurisdiction would lose its primary legal tool against a recurrence of coordinated unrest and foreign-linked destabilization; prosecutions would halt, disqualified politicians could seek to return to public life, and the security committee's authority would dissolve — a substantial rearrangement of the current political and legal landscape from this reading's vantage point.
% FOUNDING_PROBLEM: The 2019 unrest, in this reading's account, escalated from protest into sustained violence, disruption of governance, and alleged coordination with foreign actors, exposing an absence of adequate domestic legal tools to address secession, subversion, terrorism, and external collusion within the jurisdiction's own legal order.
% FOUNDING_PROBLEM_CORROBORATION: Central sovereign authority officials and pro-establishment commentators attest the founding problem was real and remains live (citing continued perceived foreign interference risk). Independent legal scholars outside the benefiting coalition — including bar association members and academic constitutional lawyers in the jurisdiction and abroad — dispute both the severity of the original threat and whether the law's scope is proportionate to it, and note that prosecutions have extended well beyond the 2019 unrest's participants to unrelated speech and association; this reading does not treat that scholarly dissent as dispositive but records it here as the corroboration this genealogy claim can produce from outside the beneficiary set.
narrative_ontology:disappearance_verdict(nsl_legal_text__sovereignty_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__sovereignty_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__sovereignty_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nsl_legal_text__sovereignty_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__sovereignty_restoration_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts at 0.35 near the law's founding moment (immediate post-2019 enactment, when this reading treats the security threat as most acute and the transfer as narrowly targeted) and rises to 0.52 by the interval's end as prosecutions and disqualifications extend, in this reading's own accounting, somewhat beyond the original 2019 participant set — a drift this reading treats as a genuine tension worth tracking rather than papering over. Suppression is authored higher (0.68) than extractiveness because the law's persistence depends on active, ongoing enforcement infrastructure (the security committee, closed proceedings, designated judges) rather than voluntary compliance; suppression is not scaled by directionality or scope in the engine's computation, only extractiveness is. Theater ratio stays low (0.2) because this reading holds the enforcement machinery to be substantively functional — prosecutions, disqualifications, and committee rulings are treated as doing real security work, not performing it — though the modest upward drift concedes some ceremonial hardening (e.g., ritualized loyalty declarations) over time.
 *
 * DIRECTIONALITY LOGIC:
 *   The central sovereign authority and the security committee sit at the beneficiary end: they set the agenda, are functionally unreviewable in security matters, and gain restored administrative control — d near the beneficiary pole. The pro-establishment bloc and the stability-seeking resident population are also beneficiaries in this reading, though the latter carries some secondary cost (constrained speech) alongside the benefit of restored order. Protest organizers, pro-democracy activists, opposition politicians, and independent journalists sit at the target end: trapped or constrained exit, direct legal jeopardy, and the specific victim status this reading assigns to those it judges to have crossed from protest into security threat. This is the key structural delta from the sibling readings: here, protesters and activists enter the victim set specifically AS security threats rather than as ordinary political dissenters, and the security committee enters the beneficiary set as a legitimate sovereign authority rather than as a capture mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists collapsing into either a pure-rope story (which would erase the named victims and the active-enforcement requirement) or a pure-snare story (which would erase the coordination function this reading holds to be genuine — a jurisdiction did face a real 2019 crisis). The tangled_rope classification is the honest middle: it preserves both the coordination claim (crisis response, restored order) and the extraction claim (targeted transfer against a defined political opposition) as simultaneously true within this reading's own account, which is what keeps the founding_problem_status authored as 'contested' rather than 'live' outright — this reading asserts the problem was live at founding but concedes, via the corroboration field, that outside scholarly attestation disputes whether it remains live at the scope now enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_proportionality_ambiguity,
    'Was the 2019 unrest a genuine existential threat to constitutional order justifying a sovereign security instrument, or was it primarily a political mobilization that a security framing recharacterizes after the fact to license suppression of opposition?',
    'Independent, non-partisan historical and legal assessment of 2019 events against comparable international thresholds for declared security emergencies, including assessment of the alleged foreign-collusion evidence presented in specific prosecutions.',
    'If the threat characterization is substantiated by independent review, this reading''s coordination-function claim strengthens and the tangled_rope''s coordination half predominates; if not substantiated, the reading collapses toward the democratic_enclosure_reading''s snare characterization for the same standing arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threat_proportionality_ambiguity, conceptual, 'Whether the 2019 unrest constituted a genuine constitutional-order threat or a political mobilization retrospectively recharacterized as a security emergency.').

omega_variable(
    scope_creep_beyond_2019_cohort,
    'Has enforcement under the law remained proportionate to the 2019 participant cohort this reading identifies as the founding problem, or has it extended to unrelated speech, association, and post-2019 conduct in a way this reading''s own founding narrative cannot account for?',
    'Systematic tracking of prosecution and disqualification case files against a defined 2019-participation criterion, conducted by a body independent of the security committee.',
    'Confirmed scope creep would raise the extractiveness this reading itself should author over time (already reflected partially in the rising measurement series) and would weaken the founding_problem_status claim of ''contested'' toward ''dead, persisting by inertia'' — pushing the classification toward snare or piton even within this reading''s frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_creep_beyond_2019_cohort, empirical, 'Whether enforcement scope has remained bounded to the founding 2019 cohort or drifted beyond it.').

omega_variable(
    sovereign_authority_vs_capture_framing,
    'Is the central sovereign authority''s role in this arrangement best framed as a legitimate exercise of constitutional sovereignty (this reading) or as jurisdictional capture eroding a previously autonomous legal system (the sibling jurisdictional_capture_reading)?',
    'Comparative institutional analysis of judicial independence, case outcomes, and legal procedure divergence before and after enactment, assessed against the jurisdiction''s prior common-law framework.',
    'This is the specific structural disagreement located between this reading and jurisdictional_capture_reading — the same authority-benefit fact is read here as sovereignty restoration and there as legal-system erosion; resolving it would not change either reading''s internal logic but would arbitrate which reading better fits the observed institutional trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereign_authority_vs_capture_framing, conceptual, 'Location of the disagreement with jurisdictional_capture_reading: sovereignty exercise versus legal-system capture, over the same central-authority benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__sovereignty_restoration_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nsl__tr_t12, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(nsl__tr_t24, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(nsl__tr_t36, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 36, 0.18).
narrative_ontology:measurement(nsl__tr_t48, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 48, 0.19).
narrative_ontology:measurement(nsl__tr_t60, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 60, 0.2).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nsl__be_t12, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(nsl__be_t24, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(nsl__be_t36, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 36, 0.5).
narrative_ontology:measurement(nsl__be_t48, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 48, 0.51).
narrative_ontology:measurement(nsl__be_t60, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(nsl__su_t12, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(nsl__su_t24, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement(nsl__su_t36, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 36, 0.66).
narrative_ontology:measurement(nsl__su_t48, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 48, 0.68).
narrative_ontology:measurement(nsl__su_t60, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__sovereignty_restoration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__democratic_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__jurisdictional_capture_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the nsl_legal_text kernel. democratic_enclosure_reading authors a higher ε and a snare classification for the same standing arrangement, with protesters/activists as unqualified victims and no legitimating coordination function credited. jurisdictional_capture_reading authors a distinct victim set (the common-law judiciary and legal profession) and frames the central authority's benefit as legal-system capture rather than sovereign restoration. All three share the enacted, enforced law as referent; each authors independent ε, beneficiary/victim structure, and classification from its own reading's lights, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
