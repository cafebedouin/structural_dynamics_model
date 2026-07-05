% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__hybrid_complementarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__hybrid_complementarity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: rome_statute_jurisdiction__hybrid_complementarity_reading
 *   human_readable: Rome Statute Jurisdiction — Hybrid Complementarity Reading
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   This constraint models the hybrid complementarity reading of the Rome
 *   Statute's jurisdictional architecture: the Court holds residual universal
 *   authority to prosecute atrocity crimes, but that authority is
 *   operationally deferential, activating only when national systems are
 *   'unwilling or unable' to prosecute genuinely. This reading treats the
 *   tension between universal aspiration and sovereign primacy as resolved,
 *   not merely postponed, through a genuine two-tier mechanism — but the
 *   resolution has produced a widening gap over time between the mechanism's
 *   formal universality and its operational selectivity, since activation
 *   still depends on state cooperation or Security Council referral, both of
 *   which correlate strongly with state power rather than the severity of
 *   alleged crimes.
 *
 * KEY AGENTS:
 *   - icc_prosecutorial_office: agenda_setter (institutional/analytical) — administers the unwilling-or-unable test
 *   - cooperating_state_parties: beneficiary/payer (institutional/constrained) — retain sovereignty, absorb occasional deference costs
 *   - non_party_powerful_states: excluded (powerful/arbitrage) — shape outcomes without treaty constraint
 *   - atrocity_survivors_in_noncooperating_states: payer (powerless/trapped) — bear the gap between formal universality and operational reach
 *   - un_security_council: agenda_setter/excluded (institutional/arbitrage) — inserts great-power political control outside complementarity logic proper
 *   - legal_scholars_and_human_rights_bodies: observer (analytical) — assess the coordination/extraction balance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.42).
domain_priors:suppression_score(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.35).
domain_priors:theater_ratio(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__hybrid_complementarity_reading, "Rome Statute Jurisdiction — Hybrid Complementarity Reading").
narrative_ontology:topic_domain(rome_statute_jurisdiction__hybrid_complementarity_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__hybrid_complementarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__hybrid_complementarity_reading, '6f1461f9-3e81-4ee4-bdb4-fb6815333b0d').
narrative_ontology:cs_kernel_codification('6f1461f9-3e81-4ee4-bdb4-fb6815333b0d', fixed_text).
narrative_ontology:cs_authority_grounding('6f1461f9-3e81-4ee4-bdb4-fb6815333b0d', lineage).
narrative_ontology:cs_interpretation_layer_present('6f1461f9-3e81-4ee4-bdb4-fb6815333b0d').
narrative_ontology:cs_reading_relation('6f1461f9-3e81-4ee4-bdb4-fb6815333b0d', rome_statute_jurisdiction__universalist_reading, influences).
narrative_ontology:cs_reading_relation('6f1461f9-3e81-4ee4-bdb4-fb6815333b0d', rome_statute_jurisdiction__sovereigntist_reading, influences).
narrative_ontology:cs_axiom('6f1461f9-3e81-4ee4-bdb4-fb6815333b0d', foundational, complementarity_as_genuine_synthesis).
narrative_ontology:cs_axiom_status(complementarity_as_genuine_synthesis, holdable).
narrative_ontology:cs_axiom_grounding('6f1461f9-3e81-4ee4-bdb4-fb6815333b0d', complementarity_as_genuine_synthesis, conventional).
narrative_ontology:cs_axiom('6f1461f9-3e81-4ee4-bdb4-fb6815333b0d', foundational, jurisdiction_conditionally_universal).
narrative_ontology:cs_axiom_status(jurisdiction_conditionally_universal, holdable).
narrative_ontology:cs_axiom_grounding('6f1461f9-3e81-4ee4-bdb4-fb6815333b0d', jurisdiction_conditionally_universal, instrumental).
narrative_ontology:cs_reference_frame('6f1461f9-3e81-4ee4-bdb4-fb6815333b0d', rome_conference_negotiated_compromise).
narrative_ontology:cs_drift_state('6f1461f9-3e81-4ee4-bdb4-fb6815333b0d', post_african_union_withdrawal_threats_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6f1461f9-3e81-4ee4-bdb4-fb6815333b0d', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_prosecutorial_office).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, cooperating_state_parties).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, atrocity_survivors_in_functioning_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, atrocity_survivors_in_noncooperating_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, defendants_subject_to_selective_referral).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, cooperating_state_parties).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__hybrid_complementarity_reading, complementarity_principle).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__hybrid_complementarity_reading, state_primary_jurisdiction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines admissibility by assessing whether a state is 'unwilling or unable' to genuinely investigate or prosecute. This gatekeeping power lets the Office selectively activate jurisdiction, and its institutional survival depends on states continuing to accept the complementarity framework rather than withdrawing or ignoring referrals.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_prosecutorial_office, agenda_setter,
    institutional, generational, analytical, global).

% Ratifying states retain first crack at prosecuting their own nationals, preserving sovereign control while gaining a legitimacy backstop and a burden-sharing mechanism for atrocity accountability. They pay through occasional deference to ICC admissibility rulings and diplomatic costs of cooperation, but keep the option of domestic prosecution to preempt ICC jurisdiction.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, cooperating_state_parties, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, cooperating_state_parties, payer).

% States that never ratified or that actively oppose the Statute's reach (through non-recognition, sanctions on the Court, or Security Council leverage) sit outside the treaty's consent structure entirely while still shaping which situations get referred or blocked. Their non-participation is not treated as objection in need of a hearing; it is simply absorbed as the limit of the Court's reach.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, non_party_powerful_states, excluded,
    powerful, civilizational, arbitrage, global).

% Where domestic courts are functioning but under political pressure, the threat of ICC admissibility can catalyze genuine domestic prosecution, giving survivors a route to accountability they would not otherwise have.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, atrocity_survivors_in_functioning_states, beneficiary,
    powerless, biographical, constrained, national).

% Where the state is a non-party or actively shields perpetrators, complementarity offers no relief: the Court's jurisdiction depends on state cooperation or Security Council referral that powerful patrons can block. Survivors bear the cost of a jurisdictional architecture that formally proclaims universal concern but operationally requires the consent of the very actors accused.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, atrocity_survivors_in_noncooperating_states, payer,
    powerless, biographical, trapped, local).

% Individuals from weaker or non-allied states face prosecution while individuals from powerful states shielded by Security Council veto power or non-ratification largely do not, despite comparable conduct. They experience the same legal architecture as selectively enforced rather than universally applied.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, defendants_subject_to_selective_referral, payer,
    moderate, biographical, trapped, national).

% Can refer situations involving non-party states to the Court or defer ongoing investigations for renewable twelve-month periods, inserting a layer of great-power political control over which universal claims get activated — a mechanism outside the complementarity logic proper but structurally load-bearing for how the hybrid actually operates.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, un_security_council, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, un_security_council, excluded).

% Assess whether complementarity functions as principled deference to functioning domestic systems or as a structural excuse for selective enforcement. Their assessments feed academic and diplomatic debate but do not bind the Court or states.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, legal_scholars_and_human_rights_bodies, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__hybrid_complementarity_reading, cooperating_state_parties).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__hybrid_complementarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Complementarity solves a genuine problem: it lets states retain primary jurisdiction over their own nationals (preserving sovereignty and domestic legal development) while providing a backstop mechanism that activates only when domestic systems genuinely fail, avoiding the need for a supranational court to supplant national judiciaries wholesale.
% TRANSFER_FUNCTION: Moves prosecutorial authority and legitimacy from national courts to the ICC only upon a finding of unwillingness or inability, and moves diplomatic and reputational costs from powerful non-cooperating states onto atrocity survivors and defendants in weaker or non-aligned states who lack the same insulation.
% ABSENT_VOICES: Non-party powerful states and victims in territories those states shield are structurally outside the treaty's negotiated compromise; they never had a seat in defining what 'unwilling or unable' means, yet the practical reach of the hybrid depends most on cases involving exactly such states.
% DISAPPEARANCE_RATIONALE: Cooperating states and the ICC bureaucracy would say the world rearranges significantly — domestic deterrence effects, complementarity-driven domestic reform, and the Court's institutional existence all depend on the mechanism. Critics in non-cooperating or victim states would say the world stays largely unchanged for them, since the mechanism was never operative in their situations regardless of its formal existence — the disagreement over disappearance effects is itself evidence of the hybrid's uneven operation.
% FOUNDING_PROBLEM: The 1998 Rome Conference needed to reconcile two irreconcilable negotiating blocs: states unwilling to cede jurisdiction over their nationals to a permanent international court, and states/NGOs pushing for a court with genuine independent authority to end impunity for mass atrocity. Complementarity was the negotiated compromise that let both blocs sign the same treaty.
% FOUNDING_PROBLEM_CORROBORATION: ICC officials and academic international-law commentators (e.g., from independent bar associations and UN human rights special rapporteurs, not ICC organs themselves) attest the founding problem remains live — domestic accountability gaps persist across multiple ongoing situations. Critics including African Union member states and independent legal scholars outside the Court's own structure attest the mechanism has calcified into selective enforcement disproportionately targeting weaker states, evidence drawn from referral patterns rather than from either side's self-interested framing.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__hybrid_complementarity_reading, contested).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__hybrid_complementarity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__hybrid_complementarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).
:- end_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) and rising because the coordination function (allowing states to retain sovereign primacy while gaining a legitimacy backstop) is genuine but has increasingly co-existed with selective enforcement patterns that concentrate costs on weaker or non-aligned states. Theater ratio rises past 0.4 by 2024 because an increasing share of the Court's public justification work — press statements about universal jurisdiction, symbolic indictments against non-cooperating heads of state who will never be arrested — performs universality the operational mechanism cannot deliver. Suppression is comparatively low (0.35) because states retain substantial exit and non-cooperation options; the hybrid's binding force depends on voluntary continued participation rather than coercive enforcement, which is precisely what distinguishes this reading from a pure enforcement-mechanism reading.
 *
 * PERSPECTIVAL GAP:
 *   From the ICC's and cooperating states' seats, this looks like a working two-tier system: sovereignty is respected until it is abused, and the backstop function is real. From the seat of survivors in non-cooperating states, the same architecture computes as extraction of legitimacy without a corresponding delivery of accountability — the Court claims universal concern but its actual jurisdiction is gated by exactly the kind of state consent it was designed to transcend. The engine's per-seat computation should reflect this: institutional/cooperating seats trend toward tangled_rope-as-functioning-coordination, while powerless/trapped seats trend toward tangled_rope-as-extraction-with-cover.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC prosecutorial office and cooperating states sit near the beneficiary end: they gain legitimacy, burden-sharing, and sovereignty preservation respectively. Atrocity survivors in non-cooperating states and defendants subject to selective referral sit near the target end: trapped exit options, no meaningful voice in admissibility determinations, and cost-bearing without commensurate benefit. Non-party powerful states and the Security Council occupy an unusual position — organizationally powerful with arbitrage-grade exit, they are excluded from the treaty's formal consent structure yet exercise outsized influence over which universal claims actually get activated, which is why they carry a dual agenda_setter/excluded role rather than a clean beneficiary/victim mapping.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling sovereignty resistance with anti-impunity demand) remains partially live — domestic accountability gaps persist in multiple ongoing conflicts — which prevents a clean 'dead mandate, zombie institution' classification. But the corroboration from outside the ICC's own organs (independent legal scholars, AU member states) suggests the mechanism has drifted toward selective enforcement in practice even where the formal justification (complementarity as principled deference) remains intact. This is precisely the tangled_rope signature: a genuine coordination function coexisting with asymmetric extraction through the same structure, requiring active administrative enforcement (the admissibility determination process) to hold together.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_reading_stability_ambiguity,
    'Does the complementarity mechanism represent a stable, principled synthesis of universal and sovereigntist claims, or is it an unstable compromise that different actors read as whichever sibling framing serves their interests in a given case?',
    'Track admissibility rulings and referral patterns over an extended interval: if ''unwilling or unable'' determinations correlate primarily with state power/alliance structure rather than the objective functioning of domestic courts, the hybrid reading is unstable and effectively collapses toward the sovereigntist reading in practice while retaining universalist rhetoric.',
    'If the hybrid is unstable, this constraint''s claimed_type may overstate the genuineness of the coordination function relative to the extraction it now masks; a finding of instability would support reclassifying toward snare for the affected victim seats specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_stability_ambiguity, conceptual, 'Whether the hybrid reading is a stable synthesis or an unstable compromise collapsing toward one sibling reading in practice.').

omega_variable(
    security_council_referral_asymmetry,
    'Is the Security Council referral/deferral mechanism (Articles 13(b) and 16) properly part of the complementarity architecture, or an external political layer bolted onto a legal mechanism that the hybrid reading under-weights?',
    'Comparative analysis of case outcomes for situations reaching the Court via state referral/proprio motu versus Security Council referral, controlling for severity of alleged crimes and state power of the accused''s nationality.',
    'If the Security Council layer accounts for most of the observed selectivity, the extraction is better modeled as a separate constraint (great-power veto over international justice) linked via network.affects_constraints rather than folded into this reading''s ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_council_referral_asymmetry, empirical, 'Whether Security Council referral power belongs inside this constraint''s structural account or should be decomposed into a linked sibling constraint.').

omega_variable(
    sibling_reading_framing_choice,
    'Was the hybrid_complementarity_reading chosen as the authoring frame because it best captures the Statute''s actual operational logic, or because it offers the most analytically interesting middle position between the two more extreme sibling readings?',
    'Cross-check against the drafting history (travaux préparatoires) of Article 17 to establish whether negotiators themselves understood complementarity as a genuine hybrid synthesis or as a sovereigntist concession dressed in universalist language.',
    'If drafting history shows complementarity was primarily a sovereigntist concession, this reading''s classification as tangled_rope (implying genuine coordination) may overstate the coordination function relative to the sovereigntist_reading''s more skeptical account.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_framing_choice, conceptual, 'Whether the hybrid framing reflects genuine drafting intent or an interpretive middle path chosen for analytical interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__hybrid_complementarity_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 1998, 0.2).
narrative_ontology:measurement(rome_tr_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2002, 0.25).
narrative_ontology:measurement(rome_tr_t2008, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2008, 0.32).
narrative_ontology:measurement(rome_tr_t2014, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2014, 0.4).
narrative_ontology:measurement(rome_tr_t2019, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2019, 0.45).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 1998, 0.22).
narrative_ontology:measurement(rome_be_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2002, 0.28).
narrative_ontology:measurement(rome_be_t2008, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2008, 0.33).
narrative_ontology:measurement(rome_be_t2014, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2014, 0.37).
narrative_ontology:measurement(rome_be_t2019, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2019, 0.4).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 1998, 0.2).
narrative_ontology:measurement(rome_su_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2002, 0.22).
narrative_ontology:measurement(rome_su_t2008, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2008, 0.26).
narrative_ontology:measurement(rome_su_t2014, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2014, 0.3).
narrative_ontology:measurement(rome_su_t2019, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2019, 0.33).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__hybrid_complementarity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.12).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, sovereigntist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the rome_statute_jurisdiction kernel, decomposed per the ε-invariance principle: universalist_reading (higher claimed universal authority, lower operational deference), sovereigntist_reading (jurisdiction strictly conditional on consent, minimal residual universal claim), and this hybrid_complementarity_reading (genuine two-tier synthesis with rising selective-enforcement drift). Each carries its own ε and stakeholder structure; they are linked here rather than merged because measuring 'the Rome Statute's jurisdiction' produces materially different ε depending on which reading of the complementarity/consent relationship the observer holds as primary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
