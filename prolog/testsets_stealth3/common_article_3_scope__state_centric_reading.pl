% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__state_centric_reading, []).

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
 *   constraint_id: common_article_3_scope__state_centric_reading
 *   human_readable: Common Article 3 Scope — State-Centric Threshold Reading
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions sets a minimum-treatment
 *   floor for non-international armed conflict: humane treatment of
 *   detainees, care for the wounded, limits on reprisal. Whether and when
 *   that floor binds is a contested kernel with three live readings. This
 *   story instantiates ONE reading — the state-centric reading: the floor
 *   attaches only when violence crosses intensity and organization
 *   thresholds; below the line (riots, banditry, counter-narcotics
 *   operations, law-enforcement responses), no humanitarian floor applies and
 *   governments hold full discretion. The structural consequences named in
 *   the manifest are encoded here: high suppression of broader application,
 *   sub-threshold fighters and civilians excluded from the protected set,
 *   governments retaining maximum operational discretion. The claim and the
 *   metrics are independent authored facts: the constraint is CLAIMED as
 *   tangled_rope (a real classification-coordination function carrying an
 *   asymmetric transfer), while the metrics describe heavily extractive,
 *   actively enforced operation — the engine measures that divergence.
 *   Epsilon's referent is the standing threshold-gated application regime,
 *   assessed by this reading's own lights; the sibling stories author their
 *   own epsilon over the same referent. KEY AGENTS (by structural
 *   relationship): - national_governments: primary beneficiary and
 *   agenda-setter (institutional/arbitrage) — classify violence, collect the
 *   preserved discretion, defend the reading - defense_establishments:
 *   secondary beneficiary (institutional/constrained) — operate the shielded
 *   detention and interrogation practices - sub_threshold_irregular_fighters:
 *   primary target (powerless/trapped) — bear the loss of all
 *   minimum-treatment guarantees - gray_zone_civilian_populations: secondary
 *   target (powerless/trapped) — lose the floor exactly when violence peaks -
 *   icrc: cost-bearing intermediary (institutional/identity_locked) — mandate
 *   narrows with each below-threshold classification; cannot exit -
 *   un_human_rights_machinery: analytical observer (institutional/analytical)
 *   — documents what the gate excludes, no enforcement hook -
 *   human_rights_ngo_community: excluded voice (organized/mobile) —
 *   investigates and publishes from outside the rooms where scope is set -
 *   international_criminal_tribunals: analytical observer
 *   (institutional/analytical) — produces the threshold jurisprudence that
 *   feeds back on states
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, 0.74).
domain_priors:suppression_score(common_article_3_scope__state_centric_reading, 0.8).
domain_priors:theater_ratio(common_article_3_scope__state_centric_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__state_centric_reading, "Common Article 3 Scope — State-Centric Threshold Reading").
narrative_ontology:topic_domain(common_article_3_scope__state_centric_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(common_article_3_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__state_centric_reading, '9c6708f5-f140-487a-8b37-cc93bc32dc20').
narrative_ontology:cs_kernel_codification('9c6708f5-f140-487a-8b37-cc93bc32dc20', fixed_text).
narrative_ontology:cs_authority_grounding('9c6708f5-f140-487a-8b37-cc93bc32dc20', lineage).
narrative_ontology:cs_interpretation_layer_present('9c6708f5-f140-487a-8b37-cc93bc32dc20').
narrative_ontology:cs_reading_relation('9c6708f5-f140-487a-8b37-cc93bc32dc20', common_article_3_scope__expansive_human_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('9c6708f5-f140-487a-8b37-cc93bc32dc20', common_article_3_scope__icrc_customary_reading, influences).
narrative_ontology:cs_axiom('9c6708f5-f140-487a-8b37-cc93bc32dc20', foundational, classification_precedes_application).
narrative_ontology:cs_axiom_status(classification_precedes_application, holdable).
narrative_ontology:cs_axiom_grounding('9c6708f5-f140-487a-8b37-cc93bc32dc20', classification_precedes_application, conventional).
narrative_ontology:cs_axiom('9c6708f5-f140-487a-8b37-cc93bc32dc20', foundational, law_enforcement_outside_ihl_scope).
narrative_ontology:cs_axiom_status(law_enforcement_outside_ihl_scope, holdable).
narrative_ontology:cs_axiom_grounding('9c6708f5-f140-487a-8b37-cc93bc32dc20', law_enforcement_outside_ihl_scope, conventional).
narrative_ontology:cs_reference_frame('9c6708f5-f140-487a-8b37-cc93bc32dc20', sovereign_discretion_threshold_gate).
narrative_ontology:cs_drift_state('9c6708f5-f140-487a-8b37-cc93bc32dc20', contemporary_post_tadic_jurisprudence, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('9c6708f5-f140-487a-8b37-cc93bc32dc20', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__state_centric_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, national_governments).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, defense_establishments).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, sub_threshold_irregular_fighters).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, gray_zone_civilian_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, icrc).
narrative_ontology:constraint_vindicates(common_article_3_scope__state_centric_reading, war_crime_distinction_doctrine).
narrative_ontology:constraint_vindicates(common_article_3_scope__state_centric_reading, conflict_classification_threshold_doctrine).
narrative_ontology:constraint_vindicates(common_article_3_scope__state_centric_reading, sovereign_discretion_over_internal_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classify violence on their own territory and in operations abroad, deciding case by case whether the 1949 minimum-treatment article binds. Where violence is classified below the armed-conflict line, captured opponents fall under domestic criminal process with no international floor on interrogation, detention conditions, or prosecution. Collect the operational freedom this classification preserves, and defend the reading in treaty diplomacy, military-manual drafting, and formal objections to contrary interpretations. Can shift framing between the war paradigm and the crime paradigm as circumstances favor.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, national_governments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__state_centric_reading, national_governments, beneficiary).

% Run the detention, interrogation, and targeting operations that the below-threshold classification removes from international minimum guarantees. Legal advisers inside these services draft the guidance that keeps operations outside the humanitarian floor and represent the reading in interagency and allied consultations. Their doctrines and career structures are built around the classification system; abandoning it would mean accepting floor obligations in operations already underway.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, defense_establishments, beneficiary,
    institutional, biographical, constrained, national).

% Fight in conflicts their adversary classifies as banditry, terrorism, or internal disturbance rather than armed conflict. When captured they receive no humane-treatment guarantees, no wounded-care entitlement, and no judicial-process floor beyond whatever domestic law happens to provide; they can be held indefinitely and interrogated without external limit. They cannot exit the classification their captor assigns them.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, sub_threshold_irregular_fighters, payer,
    powerless, biographical, trapped, regional).

% Live in violence zones their government treats as policing problems. The minimum guarantees on humane treatment, medical care for the wounded, and limits on reprisal do not attach, so sieges of districts labeled disturbed, mass roundups, and denial of medical evacuation proceed outside humanitarian limits. Leaving the zone is often blocked by the same security operation that created the classification.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, gray_zone_civilian_populations, payer,
    powerless, generational, trapped, regional).

% Carries the mandate of neutral intermediary for the 1949 regime: visiting detainees, tracing the missing, delivering relief. Each below-threshold classification shrinks the population it may reach and the leverage it may exercise. It contests narrow scope through its customary-law work and confidential representations, yet cannot walk away from the regime without dissolving its own role, and depends on the very governments whose classifications it disputes for access.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, icrc, payer,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__state_centric_reading, icrc, observer).

% Treaty bodies, special procedures, and commissions of inquiry document detention conditions and lethal-force practices in below-threshold operations under human-rights instruments. Their findings create reputational and procedural pressure on the classification but carry no enforcement hook inside the humanitarian regime, so they observe and report from outside it.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, un_human_rights_machinery, observer,
    institutional, generational, analytical, global).

% Advocacy organizations investigate and publish on the treatment of captured fighters and residents of violence zones. They are not seated in the diplomatic conferences, military-manual drafting cycles, or interpretive processes where scope is argued; their route is publicity, litigation support, and coalition pressure, exercised from outside the rooms where the line is drawn.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, human_rights_ngo_community, excluded,
    organized, biographical, mobile, global).

% Ad hoc and hybrid courts prosecuting atrocities in internal conflicts produced the leading jurisprudence on when violence counts as armed conflict — organized groups, protracted intensity. Their classifications determine which acts fall within war-crimes law, feeding back into what governments expose themselves to; they analyze and decide but collect nothing from the arrangement.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, international_criminal_tribunals, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__state_centric_reading, national_governments).
narrative_ontology:fixing_cost_class(common_article_3_scope__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate criterion for when the 1949 minimum-treatment article binds, separating armed conflict from ordinary crime and preserving the distinct legal regimes that govern each.
% TRANSFER_FUNCTION: Moves legal discretion over the detention, interrogation, and treatment of captured fighters and residents of violence zones from international minimum-treatment obligations to national authorities; correspondingly moves protection away from everyone on the below-threshold side of the line.
% ABSENT_VOICES: Captured fighters and residents of below-threshold violence zones have no seat in treaty diplomacy, military-manual drafting, or interpretive conferences; their interests enter only through intermediaries (the neutral humanitarian agency, advocacy organizations) that states can exclude from access. Dissenting interpreters inside governments are bound by cabinet-level positions.
% DISAPPEARANCE_RATIONALE: If the threshold gate vanished overnight — if the minimum floor attached to any organized lethal violence regardless of classification — detention regimes in dozens of internal conflicts would rearrange immediately: interrogation practices, holding conditions, and prosecution choices now governed by domestic discretion alone would fall under international minimum guarantees, and governments would lose the legal architecture that lets them treat opponents as criminals rather than protected persons.
% FOUNDING_PROBLEM: After the Spanish Civil War and the internal conflicts of the 1940s showed atrocities in civil wars escaping every regulation, the 1949 drafters wrote a minimum-treatment article binding all parties to any non-international armed conflict — but left undefined when internal violence counts as such an armed conflict, delegating the boundary to interpretation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: the neutral humanitarian agency's commentaries and successive customary-law studies treat the classification question as unresolved and outcome-determinative; the ad hoc tribunal jurisprudence of the 1990s built its entire threshold analysis around it; academic humanitarian-law literature and UN commission-of-inquiry reports repeatedly flag classification as decisive for detainees. No source outside the beneficiary set attests that the problem is dead.
narrative_ontology:disappearance_verdict(common_article_3_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__state_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_article_3_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__state_centric_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74 at interval end) because the gate transfers the entire humanitarian floor away from persons in sub-threshold violence: the same rifleman, captured in a recognized conflict, gets guaranteed treatment, and captured one classification notch lower, gets whatever domestic law permits. Suppression is higher still (0.80) because the reading's persistence depends on actively excluding expansive application — access denial to investigators, interpretive lobbying in treaty bodies, military-manual drafting, formal objections to contrary jurisprudence — not on participant preference. Theater is moderate (0.45): a growing share of official engagement is declaratory commitment to humanitarian law generally, professed alongside narrowing of scope in particular cases. Accessibility collapse is low-moderate (0.45) because the alternative readings remain fully live and institutionally produced — UN bodies, tribunal jurisprudence, and customary-law studies continuously regenerate them — so understanding the gate does not exhaust the option space. Resistance is substantial (0.62): sustained interpretive pressure from every non-state seat. All three tracked series share one time grid (t = 0, 13, 26, 38, 51, 63, 76, mapping 1949–2025) so no metric row borrows another's endpoints; trajectories rise monotonically with the post-Cold-War and post-2001 proliferation of gray-zone conflicts, with no oscillation to model cyclically. Suppression is authored as a raw structural property and is not scaled by power or scope; extractiveness is what the engine scales by directionality and scope. Coalition note: the victim seats are fragmented across jurisdictions, politically stigmatized as criminals or terrorists, and denied the communication channels coalition formation requires, so their numbers do not currently convert into interpretive weight.
 *
 * PERSPECTIVAL GAP:
 *   From the government seat the gate is legitimate classification: it preserves the war/crime distinction that keeps humanitarian law determinate and keeps policing under accountable domestic law. From the fighter and civilian seats the same gate is the mechanism that removes every international guarantee exactly when violence is worst. The neutral humanitarian agency experiences it as a shrinking mandate it cannot resign without dissolving itself; tribunal and UN seats experience it as an object of analysis and pressure. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Governments sit at the beneficiary pole: they collect the discretion the gate preserves and hold arbitrage-grade exit — they can reframe any episode as crime or as war as convenience dictates — so their effective burden is near zero or negative. Defense establishments share the subsidy with less mobility. Fighters and gray-zone civilians sit at the target pole: they bear the full loss of the floor with trapped exit, so effective extraction lands on them at full strength. The neutral agency is nominally a cost-bearer with identity-locked exit, but it also draws its operational existence from the very regime the gate narrows — its relationship is partially subsidized, a nuance the structural derivation alone understates; no explicit override is authored because the override surface keys on power atoms and would misfire across the several institutional seats. UN and tribunal seats are analytical observers collecting nothing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — when does the humanitarian floor bind — is live in every current gray-zone conflict, so this is not a mandate outliving its function; declaring the mandate resolved would misread an active contest as vestigial drift. The tangled-rope classification matters in both directions: labeling the gate pure coordination would erase the fighters and civilians the gate excludes; labeling it pure extraction would erase the genuine classification function that gives humanitarian protections determinate content, since protections tied to protected-person status presuppose organized parties. The hybrid category holds both truths and routes the contest to the interpretive arena where it is actually fought.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_delta_ca3_scope,
    'This constraint is one reading of the common_article_3_scope kernel: what structurally changes if a sibling reading (expansive_human_rights_reading or icrc_customary_reading) gains interpretive dominance?',
    'Track which reading tribunal jurisprudence, treaty diplomacy, and national military manuals converge on across successive gray-zone conflicts.',
    'Expansive dominance moves sub-threshold fighters and civilians into the protected set and cuts government discretion sharply; customary dominance makes scope drift with recorded practice, eroding the fixed threshold; continued state-centric dominance preserves the current victim-set boundary and discretion allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_delta_ca3_scope, conceptual, 'Committer structure: sibling readings would redraw the victim set and the discretion allocation over the same 1949 text.').

omega_variable(
    threshold_naturalness_ambiguity,
    'Is the intensity/organization threshold a genuine structural feature of the war/crime distinction (guarantees tied to protected-person status presuppose organized parties), or a discretionary instrument drawn to maximize state exemption?',
    'Compare protection outcomes in conflicts just above versus just below the threshold, controlling for violence severity and duration.',
    'If the threshold tracks a real structural break, part of the measured burden is the price of meaningful classification; if not, the gate functions closer to pure cover and the arrangement drifts toward the purely extractive end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_naturalness_ambiguity, empirical, 'Whether the scope threshold is load-bearing classification or discretionary exemption.').

omega_variable(
    victim_set_boundary_drift,
    'Where does the organization threshold actually fall in contested cases (urban riots, cartel violence, mass protest met with lethal force), and is the line drawn to exclude precisely the cases states want unconstrained?',
    'Code state classification decisions across a panel of internal-violence episodes against objective intensity and organization indicators.',
    'Systematic correlation between state interest and below-threshold classification would confirm the gate operates as selective exemption, raising effective burden on the excluded population.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_boundary_drift, empirical, 'Boundary-drawing discretion at the threshold''s edge cases.').

omega_variable(
    customary_erosion_trajectory,
    'Will accumulating contrary practice and tribunal jurisprudence eventually dissolve the fixed-threshold reading, or can its defenders indefinitely discount contrary practice?',
    'Longitudinal coding of military manuals, reservations, and interpretive statements across successive conflicts.',
    'If erosion continues, the maintenance requirement keeps rising (an enforcement ratchet) until upkeep cost exceeds the discretion benefit and forces concession; if practice consolidates behind the reading, it stabilizes as settled scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_erosion_trajectory, empirical, 'Persistence prospects of the fixed-threshold reading against customary drift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__state_centric_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_article_3_scope__state_centric_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(comm_tr_t0, observed).
narrative_ontology:measurement(comm_tr_t13, common_article_3_scope__state_centric_reading, theater_ratio, 13, 0.2).
narrative_ontology:measurement_basis(comm_tr_t13, observed).
narrative_ontology:measurement(comm_tr_t26, common_article_3_scope__state_centric_reading, theater_ratio, 26, 0.28).
narrative_ontology:measurement_basis(comm_tr_t26, observed).
narrative_ontology:measurement(comm_tr_t38, common_article_3_scope__state_centric_reading, theater_ratio, 38, 0.33).
narrative_ontology:measurement_basis(comm_tr_t38, observed).
narrative_ontology:measurement(comm_tr_t51, common_article_3_scope__state_centric_reading, theater_ratio, 51, 0.38).
narrative_ontology:measurement_basis(comm_tr_t51, observed).
narrative_ontology:measurement(comm_tr_t63, common_article_3_scope__state_centric_reading, theater_ratio, 63, 0.42).
narrative_ontology:measurement_basis(comm_tr_t63, observed).
narrative_ontology:measurement(comm_tr_t76, common_article_3_scope__state_centric_reading, theater_ratio, 76, 0.45).
narrative_ontology:measurement_basis(comm_tr_t76, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_article_3_scope__state_centric_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(comm_be_t0, observed).
narrative_ontology:measurement(comm_be_t13, common_article_3_scope__state_centric_reading, base_extractiveness, 13, 0.48).
narrative_ontology:measurement_basis(comm_be_t13, observed).
narrative_ontology:measurement(comm_be_t26, common_article_3_scope__state_centric_reading, base_extractiveness, 26, 0.55).
narrative_ontology:measurement_basis(comm_be_t26, observed).
narrative_ontology:measurement(comm_be_t38, common_article_3_scope__state_centric_reading, base_extractiveness, 38, 0.6).
narrative_ontology:measurement_basis(comm_be_t38, observed).
narrative_ontology:measurement(comm_be_t51, common_article_3_scope__state_centric_reading, base_extractiveness, 51, 0.66).
narrative_ontology:measurement_basis(comm_be_t51, observed).
narrative_ontology:measurement(comm_be_t63, common_article_3_scope__state_centric_reading, base_extractiveness, 63, 0.71).
narrative_ontology:measurement_basis(comm_be_t63, observed).
narrative_ontology:measurement(comm_be_t76, common_article_3_scope__state_centric_reading, base_extractiveness, 76, 0.74).
narrative_ontology:measurement_basis(comm_be_t76, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_article_3_scope__state_centric_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(comm_su_t0, observed).
narrative_ontology:measurement(comm_su_t13, common_article_3_scope__state_centric_reading, suppression_requirement, 13, 0.58).
narrative_ontology:measurement_basis(comm_su_t13, observed).
narrative_ontology:measurement(comm_su_t26, common_article_3_scope__state_centric_reading, suppression_requirement, 26, 0.62).
narrative_ontology:measurement_basis(comm_su_t26, observed).
narrative_ontology:measurement(comm_su_t38, common_article_3_scope__state_centric_reading, suppression_requirement, 38, 0.68).
narrative_ontology:measurement_basis(comm_su_t38, observed).
narrative_ontology:measurement(comm_su_t51, common_article_3_scope__state_centric_reading, suppression_requirement, 51, 0.74).
narrative_ontology:measurement_basis(comm_su_t51, observed).
narrative_ontology:measurement(comm_su_t63, common_article_3_scope__state_centric_reading, suppression_requirement, 63, 0.78).
narrative_ontology:measurement_basis(comm_su_t63, observed).
narrative_ontology:measurement(comm_su_t76, common_article_3_scope__state_centric_reading, suppression_requirement, 76, 0.8).
narrative_ontology:measurement_basis(comm_su_t76, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, icrc_customary_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'CA3 scope' covers three structurally distinct claims held by different interpretive coalitions: a fixed threshold gate (this story), a universal minimum floor for any organized violence (expansive_human_rights_reading), and an evolving customary boundary tracked through state practice (icrc_customary_reading). Each yields a distinct epsilon, victim set, and enforcement profile over the same 1949 text; they are modeled as three linked stories, not one story with a measurement parameter. This reading links to both siblings: it competes with the expansive reading as a live coexisting position, and it exerts structural downstream pressure on the customary reading because state refusal to acknowledge scope-widening shapes the very practice record the customary reading tracks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
