% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__judicial_activism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__judicial_activism_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: wto_dsb_authority__judicial_activism_reading
 *   human_readable: WTO DSB Judicial Activism — Interpretive Drift Creating New Obligations
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   The WTO Dispute Settlement Body was designed as a binding referee for
 *   trade disputes under the covered agreements. Under this reading, DSB
 *   panels and the Appellate Body have progressively expanded their
 *   interpretive authority beyond textual interpretation into de facto
 *   judicial legislation — creating new obligations through 'evolutionary
 *   interpretation,' 'gap-filling,' and 'judicial economy' reasoning that
 *   goes beyond the treaty mandate. Member states (particularly the US) have
 *   actively resisted compliance, blocked Appellate Body appointments, and
 *   challenged the legitimacy of rulings that read new obligations into
 *   agreements. Retaliation authorization is viewed as illegitimate when the
 *   underlying ruling exceeds the treaty text. The constraint's persistence
 *   depends on the institutional inertia of the DSB system and the
 *   professional ecosystem that benefits from it, while the coercive
 *   enforcement machinery (retaliation) is increasingly seen as the tool of
 *   judicial overreach rather than treaty compliance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, 0.68).
domain_priors:suppression_score(wto_dsb_authority__judicial_activism_reading, 0.72).
domain_priors:theater_ratio(wto_dsb_authority__judicial_activism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__judicial_activism_reading, snare).
narrative_ontology:human_readable(wto_dsb_authority__judicial_activism_reading, "WTO DSB Judicial Activism — Interpretive Drift Creating New Obligations").
narrative_ontology:topic_domain(wto_dsb_authority__judicial_activism_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__judicial_activism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__judicial_activism_reading, '4c807f6d-1749-4fb5-9cc5-b1e11f55fc5d').
narrative_ontology:cs_kernel_codification('4c807f6d-1749-4fb5-9cc5-b1e11f55fc5d', formalized).
narrative_ontology:cs_authority_grounding('4c807f6d-1749-4fb5-9cc5-b1e11f55fc5d', lineage).
narrative_ontology:cs_interpretation_layer_present('4c807f6d-1749-4fb5-9cc5-b1e11f55fc5d').
narrative_ontology:cs_reading_relation('4c807f6d-1749-4fb5-9cc5-b1e11f55fc5d', wto_dsb_authority__binding_referee_reading, forecloses).
narrative_ontology:cs_reading_relation('4c807f6d-1749-4fb5-9cc5-b1e11f55fc5d', wto_dsb_authority__advisory_coordination_reading, influences).
narrative_ontology:cs_axiom('4c807f6d-1749-4fb5-9cc5-b1e11f55fc5d', foundational, dsb_interpretive_authority_exceeds_treaty_mandate).
narrative_ontology:cs_axiom_status(dsb_interpretive_authority_exceeds_treaty_mandate, holdable).
narrative_ontology:cs_axiom_grounding('4c807f6d-1749-4fb5-9cc5-b1e11f55fc5d', dsb_interpretive_authority_exceeds_treaty_mandate, empirically_contingent).
narrative_ontology:cs_axiom('4c807f6d-1749-4fb5-9cc5-b1e11f55fc5d', foundational, retaliation_under_contested_rulings_is_illegitimate).
narrative_ontology:cs_axiom_status(retaliation_under_contested_rulings_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('4c807f6d-1749-4fb5-9cc5-b1e11f55fc5d', retaliation_under_contested_rulings_is_illegitimate, deontological).
narrative_ontology:cs_axiom('4c807f6d-1749-4fb5-9cc5-b1e11f55fc5d', secondary, member_states_retain_ultimate_policy_discretion_outside_explicit_treaty_text).
narrative_ontology:cs_axiom_status(member_states_retain_ultimate_policy_discretion_outside_explicit_treaty_text, holdable).
narrative_ontology:cs_axiom_grounding('4c807f6d-1749-4fb5-9cc5-b1e11f55fc5d', member_states_retain_ultimate_policy_discretion_outside_explicit_treaty_text, conventional).
narrative_ontology:cs_reference_frame('4c807f6d-1749-4fb5-9cc5-b1e11f55fc5d', uruguay_round_dispute_settlement_understanding).
narrative_ontology:cs_drift_state('4c807f6d-1749-4fb5-9cc5-b1e11f55fc5d', post_appellate_body_crisis, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('4c807f6d-1749-4fb5-9cc5-b1e11f55fc5d', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, dsb_panelists).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, appellate_body_members).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, trade_lawyers_specializing_in_wto).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, large_economies_with_litigation_capacity).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, developing_member_states).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, mid_size_economies_without_litigation_capacity).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, member_states_subject_to_retaliation).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, domestic_regulatory_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, large_economies_with_litigation_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Serve on rotating panels that interpret WTO agreements; their interpretive choices create binding precedent that expands obligations beyond treaty text; career advancement and professional reputation depend on the DSB system's authority and their role within it.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, dsb_panelists, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__judicial_activism_reading, dsb_panelists, beneficiary).

% Review panel reports and issue final rulings that establish interpretive precedent across the WTO system; their jurisprudence is treated as authoritative even by non-parties; professional standing is tied to the system's legitimacy as a judicial body.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, appellate_body_members, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__judicial_activism_reading, appellate_body_members, beneficiary).

% Build practices around complex WTO litigation; interpretive expansion creates demand for specialized counsel and repeat business; they shape the epistemic community that validates the DSB's authority.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, trade_lawyers_specializing_in_wto, beneficiary,
    organized, biographical, mobile, global).

% Have resources to bring and defend multiple cases; use the DSB to lock in market access gains and constrain competitors' policy space; also bear costs when rulings go against them but can absorb retaliation and compliance costs.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, large_economies_with_litigation_capacity, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__judicial_activism_reading, large_economies_with_litigation_capacity, payer).

% Lack litigation capacity to defend against expanded obligations; face compliance costs disproportionate to economic size; retaliation authorization against them is practically meaningless; excluded from shaping the interpretive jurisprudence.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, developing_member_states, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__judicial_activism_reading, developing_member_states, excluded).

% Cannot afford sustained WTO litigation; comply with expanded interpretations rather than challenge them; bear compliance costs without the ability to extract reciprocal concessions.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, mid_size_economies_without_litigation_capacity, payer,
    moderate, biographical, constrained, global).

% Face authorized trade retaliation for non-compliance with rulings they view as exceeding treaty mandate; retaliation often targets politically sensitive sectors; compliance means accepting obligations they never negotiated.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, member_states_subject_to_retaliation, payer,
    moderate, immediate, trapped, global).

% The policy space for domestic regulation in health, environment, and safety is progressively constrained by DSB interpretations that treat legitimate regulatory distinctions as disguised protectionism; no exit from the constraint without withdrawing from the WTO.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, domestic_regulatory_autonomy, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_non_agent(wto_dsb_authority__judicial_activism_reading, domestic_regulatory_autonomy).

% Monitor DSB proceedings and jurisprudence; document patterns of interpretive drift; provide alternative legal analyses but have no formal standing in proceedings.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, dispute_settlement_observers_ngos, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a binding mechanism for resolving trade disputes under WTO agreements, preventing unilateral retaliation and maintaining predictability in trade relations.
% TRANSFER_FUNCTION: Moves policy autonomy and regulatory discretion from member states (especially developing and mid-size economies) to the DSB/Appellate Body as interpretive authority, and from targeted states to complaining states through authorized retaliation.
% ABSENT_VOICES: Future governments bound by precedent they had no role in creating; domestic constituencies affected by compliance (workers, consumers, regulated industries) who have no standing in WTO proceedings; states that have not yet joined the WTO but will inherit the expanded jurisprudence.
% DISAPPEARANCE_RATIONALE: If the DSB's interpretive authority vanished overnight, member states would revert to negotiated settlements and political dispute resolution; the accumulated body of precedent expanding obligations would lose binding force; trade relations would reorganize around bilateral and plurilateral agreements rather than multilateral judicial rulings.
% FOUNDING_PROBLEM: GATT dispute settlement was blocked by losing parties; the WTO created a binding system where rulings could not be vetoed, ensuring compliance and preventing trade wars.
% FOUNDING_PROBLEM_CORROBORATION: The original negotiating record (Uruguay Round) confirms the founding problem was veto power, not interpretive authority; developing country negotiators at the time (e.g., India, Brazil) attested they did not consent to judicial lawmaking; the Appellate Body's own early reports (e.g., US-Gasoline, Japan-Alcohol) emphasized textual interpretation over gap-filling.
narrative_ontology:disappearance_verdict(wto_dsb_authority__judicial_activism_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__judicial_activism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__judicial_activism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(wto_dsb_authority__judicial_activism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__judicial_activism_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__judicial_activism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__judicial_activism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the systemic transfer of regulatory autonomy from member states to an unaccountable judicial layer — the interpretive drift functions as a ratchet that only expands obligations. Suppression (0.72) is high because resistance is met with authorized retaliation and the threat of systemic exclusion; the Appellate Body crisis (2019-present) shows the system cannot survive without active enforcement. Theater ratio (0.42) captures the growing gap between the DSB's claimed role (textual interpreter) and its actual function (norm entrepreneur). Accessibility collapse (0.58) is moderate — alternative dispute resolution exists but the WTO's centrality makes exit costly. Resistance (0.75) is high and rising — the US blocking of appointments, India's and South Africa's criticisms, and the turn to bilateral agreements all indicate active contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the DSB insider seat (panelists, AB members, trade lawyers), the system is a binding referee doing its job — interpretive evolution is necessary for a living treaty. From the developing state and mid-size economy seats, the same structure operates as an unconsented legislative body that extracts policy autonomy. The engine computes this divergence from the power/exit asymmetry: institutional actors with arbitrage exit experience the constraint as coordination; trapped and constrained actors experience it as extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   DSB panelists and Appellate Body members are structural beneficiaries — their authority, prestige, and professional ecosystem expand with each interpretive expansion (d near 0.0). Trade lawyers benefit from litigation complexity (d low). Large economies with litigation capacity are dual-positioned: they benefit from constraining competitors but pay when rulings constrain them (d ~0.5). Developing and mid-size economies are primary targets — they bear compliance costs without litigation capacity to shape jurisprudence (d near 1.0). Domestic regulatory autonomy is a non-agent victim — the policy space collapse is real but has no voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (veto power blocking dispute resolution) was solved by 1995. The DSB's interpretive expansion solves no founding problem — it creates a new one: judicial legislation without democratic consent. The mandate has atrophied into a self-justifying authority structure. The system persists because the beneficiaries (DSB professionals, large economies) control the agenda, while the payers lack collective exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_drift_boundary,
    'Where is the boundary between legitimate treaty interpretation and illegitimate judicial legislation in the WTO context?',
    'Comparative analysis of DSB/Appellate Body rulings against the Vienna Convention on the Law of Treaties Articles 31-33; identification of rulings that cannot be reconciled with textual, contextual, or object-and-purpose interpretation.',
    'If a defensible boundary exists, some rulings are legitimate interpretation and the constraint is tangled_rope (coordination + extraction). If no boundary can be drawn, the entire interpretive project is extractive — the constraint is a snare from inception.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_drift_boundary, conceptual, 'Whether the interpretive drift can be structurally separated from legitimate interpretation.').

omega_variable(
    retaliation_legitimacy_cascade,
    'Does the legitimacy of retaliation authorization depend on the legitimacy of the underlying ruling, or is retaliation a separate institutional commitment?',
    'Analysis of state practice when facing retaliation authorized under contested rulings — do states comply, retaliate in return, or exit? The US response to the Appellate Body crisis (blocking appointments rather than complying with contested rulings) is a key data point.',
    'If retaliation legitimacy cascades from ruling legitimacy, the enforcement machinery becomes a tool of extraction. If retaliation is a separate commitment, the system may survive interpretive contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliation_legitimacy_cascade, empirical, 'Whether enforcement legitimacy is derivative or independent.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the wto_dsb_authority kernel have a single defensible framing, or do the three readings represent genuinely distinct constraint structures?',
    'Structural comparison of the three readings'' beneficiary/victim sets, power distributions, and exit geographies. If the readings map to different structural configurations, they are distinct constraints linked by network.affects_constraints.',
    'If the readings are structurally distinct, the kernel is a family label, not a single constraint. Each reading generates its own classification. If they collapse to one structure, the dispute is perspectival, not structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel contains one constraint or a family of structurally distinct constraints.').

omega_variable(
    developing_state_coalition_potential,
    'Can developing and mid-size member states form an effective coalition to resist interpretive expansion, or are they structurally prevented by the DSB''s case-by-case architecture?',
    'Historical analysis of joint statements, Third World Network coordination, African Group and ACP positions in DSB reform proposals; assessment of whether collective action can overcome the litigation capacity asymmetry.',
    'If coalition power is feasible, the powerless/moderate victims have a path to agenda-setting — the constraint''s classification shifts. If the architecture prevents coalition, the extraction is structurally locked in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developing_state_coalition_potential, empirical, 'Whether victim-side coalition can alter the constraint''s power geometry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__judicial_activism_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__judicial_activism_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(wto__tr_t2000, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(wto__tr_t2005, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(wto__tr_t2010, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2010, 0.33).
narrative_ontology:measurement(wto__tr_t2015, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2015, 0.37).
narrative_ontology:measurement(wto__tr_t2020, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(wto__tr_t2024, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 1995, 0.25).
narrative_ontology:measurement(wto__be_t2000, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(wto__be_t2005, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(wto__be_t2010, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(wto__be_t2015, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(wto__be_t2020, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(wto__be_t2024, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 1995, 0.35).
narrative_ontology:measurement(wto__su_t2000, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(wto__su_t2005, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2005, 0.52).
narrative_ontology:measurement(wto__su_t2010, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(wto__su_t2015, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2015, 0.64).
narrative_ontology:measurement(wto__su_t2020, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(wto__su_t2024, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__judicial_activism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_dsb_authority__judicial_activism_reading, 0.12).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__advisory_coordination_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_appellate_body_crisis).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_reform_negotiations).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, bilateral_trade_agreement_proliferation).

% DUAL FORMULATION NOTE:
% This reading and its siblings (binding_referee_reading, advisory_coordination_reading) form the wto_dsb_authority constraint family. They share the same institutional referent (the DSB) but instantiate different constraints with different ε values, beneficiary/victim structures, and classifications. The judicial_activism_reading has the highest ε (0.68) because it assesses the standing arrangement as extractive judicial legislation. The binding_referee_reading would have lower ε (legitimate interpretation within mandate). The advisory_coordination_reading would have the lowest ε (coordination without binding extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_dsb_authority__judicial_activism_reading, institutional, 0.1).
constraint_indexing:directionality_override(wto_dsb_authority__judicial_activism_reading, powerful, 0.45).
constraint_indexing:directionality_override(wto_dsb_authority__judicial_activism_reading, moderate, 0.85).
constraint_indexing:directionality_override(wto_dsb_authority__judicial_activism_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
