% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__husk_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: preparedness_commitment__husk_reading
 *   human_readable: Preparedness Commitment as Memorial Performance (Husk Reading)
 *   domain: institutional/governance/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes the institutional performance of preparedness
 *   as a reading of the broader preparedness_commitment kernel. In the husk
 *   reading, preparedness routines have become ceremonial: drills are
 *   conducted, procedures documented, compliance metrics reported—all the
 *   formal apparatus of retention—while the underlying operational competence
 *   has attenuated. Responders know this; administrators preserve
 *   institutional stories that narrate continued preparedness. The constraint
 *   persists because the beneficiaries (administrators, drill staff) control
 *   the definition of success (form-compliance) and the mechanisms of
 *   enforcement (mandate, funding, professional identity lock). When real
 *   crises arrive, the apparatus fails, but failures are reinterpreted as
 *   evidence that more drills are needed—the constraint regenerates itself
 *   through post-crisis inquiry.
 *
 * KEY AGENTS:
 *   - Institutional administrators: agenda-setters who define preparedness through compliance metrics and control funding, enforcing the drill apparatus as institutional doctrine
 *   - Drill facilitators: beneficiary-payers whose professional identity is locked into the apparatus; they benefit from its perpetuation (employment, status) while paying the cognitive cost of running hollow procedures
 *   - At-risk populations: powerless, trapped, bearing the real cost when crises reveal non-functionality
 *   - Frontline responders: organized but constrained, they know from experience the apparatus fails under novel stress but face career penalties for saying so
 *   - Post-crisis inquiry bodies: observers who tend to reinforce the constraint by recommending 'more drills' rather than questioning whether drills work
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__husk_reading, 0.68).
domain_priors:suppression_score(preparedness_commitment__husk_reading, 0.72).
domain_priors:theater_ratio(preparedness_commitment__husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, theater_ratio, 0.81).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__husk_reading, piton).
narrative_ontology:human_readable(preparedness_commitment__husk_reading, "Preparedness Commitment as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_commitment__husk_reading, "institutional/governance/commitment_systems").

domain_priors:requires_active_enforcement(preparedness_commitment__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__husk_reading, '548ea298-22fa-402c-955a-fd028cb4a9ff').
narrative_ontology:cs_kernel_codification('548ea298-22fa-402c-955a-fd028cb4a9ff', formalized).
narrative_ontology:cs_authority_grounding('548ea298-22fa-402c-955a-fd028cb4a9ff', extraction).
narrative_ontology:cs_interpretation_layer_present('548ea298-22fa-402c-955a-fd028cb4a9ff').
narrative_ontology:cs_reading_relation('548ea298-22fa-402c-955a-fd028cb4a9ff', preparedness_commitment__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('548ea298-22fa-402c-955a-fd028cb4a9ff', preparedness_commitment__hybrid_reading, influences).
narrative_ontology:cs_axiom('548ea298-22fa-402c-955a-fd028cb4a9ff', foundational, preparedness_decoupled_from_competence).
narrative_ontology:cs_axiom_status(preparedness_decoupled_from_competence, holdable).
narrative_ontology:cs_axiom_grounding('548ea298-22fa-402c-955a-fd028cb4a9ff', preparedness_decoupled_from_competence, empirically_contingent).
narrative_ontology:cs_axiom('548ea298-22fa-402c-955a-fd028cb4a9ff', secondary, institutional_form_supersedes_function).
narrative_ontology:cs_axiom_status(institutional_form_supersedes_function, holdable).
narrative_ontology:cs_axiom_grounding('548ea298-22fa-402c-955a-fd028cb4a9ff', institutional_form_supersedes_function, instrumental).
narrative_ontology:cs_reference_frame('548ea298-22fa-402c-955a-fd028cb4a9ff', preparedness_as_living_doctrine).
narrative_ontology:cs_drift_state('548ea298-22fa-402c-955a-fd028cb4a9ff', contemporary_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('548ea298-22fa-402c-955a-fd028cb4a9ff', '2026-06-12T14:23:45Z').
narrative_ontology:cs_kernel_id(preparedness_commitment__husk_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, institutional_administrators).
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, drill_facilitators).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, at_risk_populations).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, frontline_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, drill_facilitators).
narrative_ontology:constraint_vindicates(preparedness_commitment__husk_reading, institutional_continuity_doctrine).
narrative_ontology:constraint_vindicates(preparedness_commitment__husk_reading, ritual_retention_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manage the institutional apparatus that schedules, runs, and documents preparedness drills and training cycles. They perceive preparedness primarily through compliance metrics: drills conducted, checkboxes marked, documentation complete. Defending the drill apparatus is conflated with defending actual readiness. Their career path depends on visible institutional function and low scandal, not on whether novel crises are actually handled well.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, institutional_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Professional identities are built around running and refining drills, designing training scenarios, and maintaining preparedness bureaucracy. They benefit from the perpetuation of the drill apparatus (employment, status, continuing institutional validation of their role). They also pay through the cognitive load of running hollow procedures repeatedly, and through the career penalty they would face for questioning whether the drills actually work.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, drill_facilitators, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__husk_reading, drill_facilitators, payer).

% Reside in jurisdictions covered by preparedness plans and drills. They bear the cost when actual crisis arrives and the apparatus fails to respond—they experience death, injury, displacement, economic loss. Their vulnerability is cited to justify the drill apparatus's existence, but their actual safety outcomes are decoupled from the form-compliance of the drills.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, at_risk_populations, payer,
    powerless, immediate, trapped, national).

% Firefighters, paramedics, emergency managers, police, and utility workers who execute response when real crisis arrives. They experience the gap between the scripted drill scenario and actual operational reality. They know from lived experience that the drills do not adequately train for novel crises, but speaking that knowledge openly creates professional friction and is often interpreted as insufficiency rather than as accurate feedback about the constraint.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, frontline_responders, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__husk_reading, frontline_responders, excluded).

% Conduct periodic reviews of preparedness. They tend to evaluate on the metrics the administrative apparatus reports (drills completed, training hours logged, funding distributed). They rarely have the operational context to challenge whether the reported metrics correlate with actual crisis response competence.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, legislative_oversight_bodies, observer,
    institutional, generational, analytical, national).

% Investigate after major crises when they occur. They often discover that the apparatus was non-functional despite apparent compliance, generating findings that recommend 'more training, better drills.' The constraint perpetuates itself by transforming failures into justifications for more of the same.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, post_crisis_inquiry_bodies, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__husk_reading, institutional_administrators).
narrative_ontology:fixing_cost_class(preparedness_commitment__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains institutional continuity of preparedness doctrine and narrative across generational turnover. Keeps disaster-response language, procedures, and organizational structures alive in institutional memory through repeated rehearsal, even when operational substance attenuates.
% TRANSFER_FUNCTION: Moves organizational resources (funding, personnel time, logistical capacity) from operational readiness (training novel scenarios, stress-testing gaps, adapting to revealed vulnerabilities) into memorial maintenance (running form-compliant drills, completing compliance documentation, perpetuating inherited procedures whose origins have been forgotten).
% ABSENT_VOICES: Frontline responders who have experienced real crises and know the apparatus failed. Communities that survived crises the apparatus was supposed to prevent. Independent operational auditors who might assess whether drills correlate with actual response capacity. Crisis-scenario designers working outside the institutional framework who might challenge whether inherited procedures match current threat landscapes.
% DISAPPEARANCE_RATIONALE: If the memorial performance apparatus vanished—no mandated drills, no institutional compliance documentation, no drill-cycle scheduling—institutions would face acute pressure to develop genuine operational readiness based on honest assessment of competence gaps. Actual crises would become more visible as failures. The apparatus's disappearance would force a choice: build real competence or accept actual risk. The constraint persists by deferring that choice indefinitely.
% FOUNDING_PROBLEM: Disaster preparedness doctrine emerged from historical crises (wars, natural disasters) that killed thousands through unpreparedness and communication breakdown. The founding need was to preserve knowledge of response protocols across generational turnover in professional cadres—to retain operational competence even as experienced personnel retired.
% FOUNDING_PROBLEM_CORROBORATION: The original founding problem (generational knowledge loss in emergency response cadres) is no longer the primary driver of the constraint. This is attested by: (1) crisis investigation reports that consistently find apparatus non-functionality despite compliance; (2) operational studies showing drill participation does not correlate with novel-crisis performance; (3) interviews with experienced responders noting drills teach inherited procedures, not adaptive capacity. The institutional beneficiaries attest the founding problem is still live and justify continued apparatus via it; independent operational analysts and post-crisis investigators attest the connection between the apparatus and actual preparedness has decayed.
narrative_ontology:disappearance_verdict(preparedness_commitment__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_commitment__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__husk_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_commitment__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_commitment__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater ratio is high (0.81) and rising: the constraint's operation is increasingly performative—drills check boxes, procedures are rehearsed, documentation is complete, but the relationship between drilling and actual response capacity has decayed. Extractiveness is moderate-high (0.68) and plateauing: the apparatus extracts resources (personnel time, funding, logistical capacity) that could be allocated to genuine competence-building (stress-testing novel scenarios, operational learning from near-misses, adaptive training). Suppression is also moderate-high (0.72) and rising slowly: the constraint persists partly through coercion (regulatory mandate, professional obligation, funding conditions) and partly through internalized suppression—responders and administrators alike believe the drills should work and interpret their failure as insufficient effort rather than as design failure. Accessibility_collapse is low (0.41): alternatives to the memorial apparatus exist (operational auditing, adaptive training regimes, honest competence assessment) but the institutional beneficiaries actively prevent their adoption. Resistance is moderate (0.58): frontline responders and some post-crisis investigators push back, but their resistance is absorbed into the apparatus ('see, we need better drills') rather than changing the constraint itself. The measurement series capture the constraint's attenuation arc: theater and suppression rising as the apparatus becomes more about defending itself than serving preparedness; extractiveness and suppression plateauing as the apparatus reaches a steady-state extraction equilibrium where the cost-to-perpetuate is stable.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional administrators' seat, the apparatus is genuine coordination—it preserves organizational memory, maintains procedures across generational turnover, keeps doctrine alive. From the frontline responders' and at-risk populations' seats, the same apparatus is theater masquerading as competence. The engine should compute these seats differently: the administrator's directionality should place them as beneficiary (low d, low effective extraction from their position); the responder and at-risk seats should compute as targets (high d). The administrator's claim—'we are preserving preparedness'—is structurally incompatible with the responder's knowledge—'the drills do not train us to handle novel crises.' The constraint persists because the administrators' position controls the institutional apparatus's definition and enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional administrators are beneficiaries: they control the apparatus, define success through form-compliance, and accrue benefit through career advancement and institutional validation. Directionality for this seat should be near 0.2 (low extraction from the beneficiary end). Drill facilitators are dual-positioned (beneficiary + payer): they benefit from apparatus perpetuation (employment, identity validation) but pay through cognitive dissonance and professional constraint (they run drills they know are insufficient). Their d should sit near 0.4–0.5 (slightly toward target, weighted by the identity-lock constraint on exit). At-risk populations are pure targets: powerless, trapped, bearing costs when competence fails. Their d should be near 0.85 (high extraction from the target end). Frontline responders are also targets but with more power and organized exit options (they can transfer, retire, speak to media): their d should be 0.65–0.75. Post-crisis inquiry bodies are analytical observers with institutional power but no direct extraction interest: d near 0.5. The key directionality insight: the constraint's persistence depends on keeping the beneficiary seat (administrators) structurally insulated from the cost signals that targets experience. When a novel crisis forces the cost signal through (deaths, economic loss), post-crisis inquiry typically captures and defuses it by recommending 'more drills'—the constraint regenerates itself.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits classic Piton structure: the founding problem (generational knowledge loss in emergency cadres) is no longer the primary driver of the apparatus's persistence. The apparatus survives through inertia (it is embedded in law, funding cycles, professional identity), through theater (drills feel like retention), and through institutionalized redefinition of failure as proof that more of the same is needed. No concentrated beneficiary profits enough to defend the apparatus against genuine competitive constraints—administrators benefit but not massively, drill facilitators face identity lock rather than enrichment. The cost to fix would be high (admitting preparedness failures, rebuilding competence from scratch, disrupting institutional identity) but is perpetually deferred because the apparatus insulates beneficiaries from the cost signal. This is precisely the Piton signature: the administrator could change it (they control the apparatus) but does not, because the cost to fix exceeds what they bear. Responders and at-risk populations bear the cost when crises happen but lack the institutional power to force change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) structural (regulatory mandate, funding conditions, professional obligation) or internalized (participants genuinely believe the drills are necessary, have accepted the form-compliance logic)?',
    'Observational evidence from jurisdictions that dismantled or substantially reduced the drill apparatus: do responder competence and crisis outcomes improve, stay the same, or worsen? Do participants report liberation or anxiety? Post-apparatus operational assessment against real-crisis performance.',
    'If suppression is primarily structural, dismantling the apparatus would be resisted by administrators but welcomed by responders; competence might improve. If suppression is primarily internalized, dismantling would trigger anxiety even among targets, and genuine competence-building would face internal resistance from responders who have internalized the ''more drills'' frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression in the preparedness apparatus.').

omega_variable(
    husk_vs_competence_reading_boundary,
    'Are there meaningful pockets of genuine competence-building within the broader husk apparatus (operational learning units, stress-testing teams, adaptive training cohorts) that are systematically suppressed by the administrative apparatus?',
    'Institutional ethnography: map where adaptive learning is occurring, track whether it is funded or defunded, measure whether responders participating in it have better real-crisis outcomes than those in form-compliant-only drills.',
    'If genuine competence elements are present but suppressed, the constraint is better characterized as actively extracting from competence (snare with coordination elements) rather than as pure memorial husk. If competence elements are absent, the husk reading holds cleanly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_vs_competence_reading_boundary, empirical, 'Whether the husk apparatus actively suppresses or merely neglects competence-building.').

omega_variable(
    crisis_visibility_vs_constraint_regeneration,
    'Does each major crisis that reveals the apparatus''s non-functionality trigger genuine reform, or does post-crisis inquiry systematically recommend ''more drills,'' thereby regenerating the constraint?',
    'Comparative analysis of post-crisis inquiry findings across multiple jurisdictions and crisis types (natural disasters, pandemics, industrial accidents) over the past 40 years: what fraction recommend structural overhaul vs. recommend more training?',
    'If crises trigger genuine reform attempts, the constraint''s persistence is a failure of implementation. If post-crisis inquiry systematically regenerates the constraint, the apparatus has captured the failure-response mechanism itself, making the constraint extraordinarily durable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(crisis_visibility_vs_constraint_regeneration, empirical, 'Post-crisis inquiry pattern: does it reform or regenerate the apparatus?').

omega_variable(
    kernel_reading_distinction_husk_vs_competence,
    'Is the boundary between husk_reading and competence_reading a matter of actual institutional structure (different agencies have different apparatus-to-competence ratios), or is it a difference in interpretive frame that the same institutional data can support?',
    'Detailed operational audit comparing institutions claiming the competence_reading (preparedness works because we drill) vs. husk_reading framing (preparedness is memorial, we fail when it matters). Do their actual crisis outcomes differ systematically, or do both appeal to the same outcome data through different narrative lenses?',
    'If husk and competence readings are interpretations of the same institutional structure with identical outcomes, they are reading a kernel—and the constraint should be classified as how institutional identity is locked into one reading. If they describe genuinely different institutional configurations with different outcomes, they are two separate constraints and should be decomposed into distinct stories.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_distinction_husk_vs_competence, conceptual, 'Whether husk and competence readings describe different structures or different interpretations of identical structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__husk_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t5, preparedness_commitment__husk_reading, theater_ratio, 5, 0.66).
narrative_ontology:measurement_basis(prep_tr_t5, observed).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__husk_reading, theater_ratio, 10, 0.71).
narrative_ontology:measurement_basis(prep_tr_t10, observed).
narrative_ontology:measurement(prep_tr_t15, preparedness_commitment__husk_reading, theater_ratio, 15, 0.74).
narrative_ontology:measurement_basis(prep_tr_t15, observed).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__husk_reading, theater_ratio, 20, 0.77).
narrative_ontology:measurement_basis(prep_tr_t20, observed).
narrative_ontology:measurement(prep_tr_t25, preparedness_commitment__husk_reading, theater_ratio, 25, 0.79).
narrative_ontology:measurement_basis(prep_tr_t25, observed).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__husk_reading, theater_ratio, 30, 0.8).
narrative_ontology:measurement_basis(prep_tr_t30, observed).
narrative_ontology:measurement(prep_tr_t35, preparedness_commitment__husk_reading, theater_ratio, 35, 0.8).
narrative_ontology:measurement_basis(prep_tr_t35, observed).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__husk_reading, theater_ratio, 40, 0.81).
narrative_ontology:measurement_basis(prep_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__husk_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t5, preparedness_commitment__husk_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement_basis(prep_be_t5, observed).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__husk_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement_basis(prep_be_t10, observed).
narrative_ontology:measurement(prep_be_t15, preparedness_commitment__husk_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(prep_be_t15, observed).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__husk_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(prep_be_t20, observed).
narrative_ontology:measurement(prep_be_t25, preparedness_commitment__husk_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(prep_be_t25, observed).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__husk_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(prep_be_t30, observed).
narrative_ontology:measurement(prep_be_t35, preparedness_commitment__husk_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(prep_be_t35, observed).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__husk_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(prep_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__husk_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(prep_su_t0, observed).
narrative_ontology:measurement(prep_su_t5, preparedness_commitment__husk_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(prep_su_t5, observed).
narrative_ontology:measurement(prep_su_t10, preparedness_commitment__husk_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(prep_su_t10, observed).
narrative_ontology:measurement(prep_su_t15, preparedness_commitment__husk_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(prep_su_t15, observed).
narrative_ontology:measurement(prep_su_t20, preparedness_commitment__husk_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(prep_su_t20, observed).
narrative_ontology:measurement(prep_su_t25, preparedness_commitment__husk_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(prep_su_t25, observed).
narrative_ontology:measurement(prep_su_t30, preparedness_commitment__husk_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(prep_su_t30, observed).
narrative_ontology:measurement(prep_su_t35, preparedness_commitment__husk_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(prep_su_t35, observed).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__husk_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(prep_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__husk_reading, 0.18).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_commitment kernel is contested across three readings: (1) competence_reading — preparedness as live exercised knowledge, maintaining operational capacity; (2) husk_reading (this constraint) — preparedness as memorial performance, operational competence attenuated; (3) hybrid_reading — preparedness as layered system, memorial elements stabilize commitment while competence elements maintain function. These are not different measurements of one constraint; they are different structural claims about the same institutional apparatus. The husk reading instantiates one constraint from this kernel. The sibling readings instantiate others. Network effects: the husk reading influences both siblings by demonstrating how form-compliance can decouple from competence, thereby creating the structural pressure that the competence and hybrid readings must respond to.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_commitment__husk_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
