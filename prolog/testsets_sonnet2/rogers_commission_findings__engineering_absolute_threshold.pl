% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__engineering_absolute_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_findings__engineering_absolute_threshold, []).

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
 *   constraint_id: rogers_commission_findings__engineering_absolute_threshold
 *   human_readable: Rogers Commission Reading: Engineering Absolute Safety Threshold (No-Fly Until Redesign Certified)
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   Following the 1986 Challenger disaster, the Rogers Commission documented
 *   that O-ring erosion under cold-temperature launch conditions was a known,
 *   unresolved engineering concern that management proceeded to launch
 *   through anyway. The commission's report is a single kernel text that
 *   different institutional actors read differently. This story instantiates
 *   the engineering_absolute_threshold reading: the findings establish a hard
 *   technical stopping rule — flight operations cease entirely until the
 *   O-ring joint is redesigned and certified, with engineers holding binding
 *   veto authority at Flight Readiness Review rather than merely advisory
 *   input. Two sibling readings of the SAME kernel text are NOT part of this
 *   story: the actuarial_risk_acceptance reading (flight resumes once a
 *   quantified failure probability is documented and accepted by an informed
 *   decision-maker, without requiring redesign) and the
 *   management_compliance_narrative reading (flight resumes once a documented
 *   process demonstrates risk awareness and mitigation effort, treating the
 *   requirement as procedural rather than substantive). Those readings carry
 *   different beneficiary/victim structures and are authored, if at all, as
 *   separate constraint files linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - flight_crew: primary beneficiary (powerless/trapped) — protected by the halt but has no procedural standing
 *   - solid_rocket_booster_engineers: agenda_setter (moderate/constrained) — holds the veto this reading establishes
 *   - launch_schedule_stakeholders: primary payer (powerful/constrained) — bears the cadence cost
 *   - program_management_office: institutional payer (institutional/constrained) — loses discretionary balancing authority
 *   - commercial_and_political_launch_customers: secondary payer (organized/constrained) — queued indefinitely, excluded from review
 *   - rogers_commission: analytical observer (institutional/analytical) — source of the contested kernel text
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__engineering_absolute_threshold, 0.22).
domain_priors:suppression_score(rogers_commission_findings__engineering_absolute_threshold, 0.86).
domain_priors:theater_ratio(rogers_commission_findings__engineering_absolute_threshold, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, extractiveness, 0.22).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(rogers_commission_findings__engineering_absolute_threshold, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__engineering_absolute_threshold, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__engineering_absolute_threshold, "Rogers Commission Reading: Engineering Absolute Safety Threshold (No-Fly Until Redesign Certified)").
narrative_ontology:topic_domain(rogers_commission_findings__engineering_absolute_threshold, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__engineering_absolute_threshold).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__engineering_absolute_threshold, '12c88dda-db8d-4c67-ae88-a57465638395').
narrative_ontology:cs_kernel_codification('12c88dda-db8d-4c67-ae88-a57465638395', formalized).
narrative_ontology:cs_authority_grounding('12c88dda-db8d-4c67-ae88-a57465638395', expertise).
narrative_ontology:cs_interpretation_layer_present('12c88dda-db8d-4c67-ae88-a57465638395').
narrative_ontology:cs_reading_relation('12c88dda-db8d-4c67-ae88-a57465638395', rogers_commission_findings__management_compliance_narrative, coexists_with).
narrative_ontology:cs_reading_relation('12c88dda-db8d-4c67-ae88-a57465638395', rogers_commission_findings__actuarial_risk_acceptance, influences).
narrative_ontology:cs_axiom('12c88dda-db8d-4c67-ae88-a57465638395', foundational, documented_uncorrected_failure_mode_is_sufficient_to_ground_fleet).
narrative_ontology:cs_axiom_status(documented_uncorrected_failure_mode_is_sufficient_to_ground_fleet, holdable).
narrative_ontology:cs_axiom_grounding('12c88dda-db8d-4c67-ae88-a57465638395', documented_uncorrected_failure_mode_is_sufficient_to_ground_fleet, deontological).
narrative_ontology:cs_axiom('12c88dda-db8d-4c67-ae88-a57465638395', foundational, engineering_veto_is_binding_not_advisory).
narrative_ontology:cs_axiom_status(engineering_veto_is_binding_not_advisory, holdable).
narrative_ontology:cs_axiom_grounding('12c88dda-db8d-4c67-ae88-a57465638395', engineering_veto_is_binding_not_advisory, conventional).
narrative_ontology:cs_reference_frame('12c88dda-db8d-4c67-ae88-a57465638395', pre_challenger_managerial_discretion_baseline).
narrative_ontology:cs_drift_state('12c88dda-db8d-4c67-ae88-a57465638395', post_columbia_reassessment, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('12c88dda-db8d-4c67-ae88-a57465638395', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, flight_crew).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, astronaut_corps).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__engineering_absolute_threshold, engineering_safety_function).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, launch_schedule_stakeholders).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, program_management_office).
narrative_ontology:constraint_victim(rogers_commission_findings__engineering_absolute_threshold, commercial_and_political_launch_customers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Astronauts who fly the vehicle have no independent means to verify O-ring safety margins and cannot decline a mission once assigned without career cost. This reading's threshold exists to protect them from a known, unresolved failure mode; they benefit from the halt but hold no seat at the table that decides whether it is honored.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, flight_crew, beneficiary,
    powerless, immediate, trapped, national).

% The engineers (Thiokol and NASA propulsion staff) who identified the O-ring erosion pattern under cold-temperature conditions. Under this reading they hold veto authority at Flight Readiness Review: a documented, uncertified failure mode is sufficient by itself to ground the fleet, independent of schedule or political pressure. Their technical judgment is the enforcement mechanism.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, solid_rocket_booster_engineers, agenda_setter,
    moderate, biographical, constrained, national).

% NASA program managers responsible for manifest commitments, launch cadence, and budget justification to Congress. A stand-down triggered by this threshold halts revenue-adjacent commercial and defense payloads, delays downstream missions, and exposes the program to funding scrutiny. They bear the schedule and reputational cost of honoring the engineering veto.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, launch_schedule_stakeholders, payer,
    powerful, biographical, constrained, national).

% The institutional layer that must translate a hard engineering stop into funding requests, contractor renegotiations, and public messaging. Under this reading their preferred discretion to weigh schedule against risk is foreclosed — the threshold is binary, not a factor to be balanced, which they experience as a loss of managerial authority.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, program_management_office, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__engineering_absolute_threshold, program_management_office, excluded).

% Satellite operators and government payload sponsors whose launches are queued behind Shuttle manifest slots. A certified-redesign gate with no fixed timeline extends their wait indefinitely and they have no standing in the Flight Readiness Review process that decides when the gate reopens.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, commercial_and_political_launch_customers, payer,
    organized, biographical, constrained, continental).

% The institutionalized post-Challenger safety review apparatus (independent safety panels, escape-velocity veto authority) that this reading's precedent creates and legitimizes. It gains durable standing and resources from the establishment of an engineering veto as binding rather than advisory.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, engineering_safety_function, beneficiary,
    moderate, civilizational, constrained, national).

% The presidential commission that investigated the Challenger disaster and issued the findings this constraint is a reading of. It has no ongoing enforcement role but its report is the textual kernel every reading, including this one, claims to instantiate.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__engineering_absolute_threshold, rogers_commission, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__engineering_absolute_threshold, flight_crew).
narrative_ontology:fixing_cost_class(rogers_commission_findings__engineering_absolute_threshold, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared, binding stopping rule between engineers and management: when a specific, characterized failure mode (O-ring erosion under cold-launch conditions) is documented and uncorrected, no launch proceeds regardless of schedule pressure, until an engineering fix is certified. This solves the genuine problem of schedule pressure systematically overriding known technical risk.
% TRANSFER_FUNCTION: Moves decision authority over launch/no-launch from program management and political/commercial stakeholders to the engineering function; moves cost from potential future catastrophic loss (crew, vehicle, program) onto present-day schedule delay, budget overrun, and customer wait time.
% ABSENT_VOICES: Commercial and political payload customers have no seat in the Flight Readiness Review and cannot contest the duration of a stand-down; program management's preference to weigh probability-of-failure against mission value is structurally excluded by this reading's binary framing, which is precisely what distinguishes it from the actuarial_risk_acceptance sibling.
% DISAPPEARANCE_RATIONALE: If this reading's binding engineering veto were removed, Flight Readiness Review would revert to a management-weighted judgment call (as it operated pre-Challenger), launch cadence would very likely resume faster, and the specific institutional guarantee that grounded the fleet for O-ring redesign would cease to have independent force — it would collapse into whichever sibling reading (compliance narrative or risk acceptance) governs in its place.
% FOUNDING_PROBLEM: The Challenger disaster occurred because documented engineering concern about O-ring performance in cold weather was overridden by launch-schedule and political pressure at the management level; the Rogers Commission was created to determine why the concern was not acted upon.
% FOUNDING_PROBLEM_CORROBORATION: Independent aerospace safety academics and the subsequent Columbia Accident Investigation Board (2003) attest that the underlying problem — schedule pressure overriding known engineering risk signals — recurred and was not durably solved by this reading's threshold, since the Columbia foam-strike concern was similarly normalized. NASA's own post-Challenger safety office and surviving Rogers Commission members attest the threshold framing was sound at the time; no source entirely outside program-affiliated parties confirms the engineering-veto reading was ever fully institutionalized as binding rather than advisory in practice.
narrative_ontology:disappearance_verdict(rogers_commission_findings__engineering_absolute_threshold, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__engineering_absolute_threshold, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__engineering_absolute_threshold, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rogers_commission_findings__engineering_absolute_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__engineering_absolute_threshold, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__engineering_absolute_threshold_tests).
:- end_tests(rogers_commission_findings__engineering_absolute_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.22) because, under this reading's own lights, the arrangement transfers decision authority and schedule cost rather than extracting rents — the 'cost' borne by launch stakeholders is the price of a genuine safety coordination function, not value siphoned to an unaccountable party. Suppression is authored very high (0.86-0.95) because the reading's defining feature is that it forecloses management discretion entirely: no schedule argument, political pressure, or probability estimate can override an uncorrected known failure mode. Suppression is front-loaded (post-Challenger enforcement was most rigid immediately after the disaster) and settles to a stable plateau as the redesign-certification norm becomes routine, which is why the suppression_requirement series declines slightly then flattens rather than rising — this models legitimate institutionalization, not decay of a coercive apparatus. Theater ratio stays low throughout: the accountability mechanism (independent safety panels, veto power) is exercised substantively, not merely performed, under this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Flight crew and the engineering safety function are beneficiaries: the threshold exists structurally to protect crew life and to give engineering judgment durable institutional weight it lacked before Challenger. Launch schedule stakeholders, program management, and commercial/political customers are payers: they bear the concrete cost (delay, budget exposure, queued payloads) of honoring a stop-rule they do not control and cannot negotiate down. The engineers who set the agenda are themselves only moderately powered and hold constrained exit — their veto is institutionally granted, not personally held, and can be revoked by a future management-favorable reading of the same kernel text (this is exactly what the sibling readings represent).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (schedule pressure overriding documented risk) is contested as live vs. dead: Columbia (2003) recurred through a structurally similar pathway (foam-strike risk normalized under schedule pressure), suggesting the engineering-veto threshold, even where nominally adopted, did not durably resolve the founding problem — it may have degraded toward the compliance_narrative reading in practice even where the absolute_threshold reading was the official one. This is precisely the divergence the R5 corroboration field is built to surface: the reading's own beneficiaries (flight crew, safety office) would attest the threshold worked as intended; an outside corroborator (CAIB) suggests the binding-veto character did not hold across the institution's later history. The tangled_rope classification for this reading follows from that gap: genuine coordination function (documented, exists, protects crew) coexists with a real, asymmetric cost falling on schedule/program/customer seats, sustained only by active enforcement of the veto — which is exactly the kind of arrangement that can be honored in the near term and eroded later without formal repeal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_binding_or_advisory,
    'Did the Rogers Commission findings, as actually operationalized inside NASA, establish a BINDING engineering veto (this reading) or merely an advisory input eventually absorbed into managerial risk-acceptance or compliance framing (the sibling readings)?',
    'Compare Flight Readiness Review records and override authority across the pre-Challenger, immediate post-Challenger, and pre-Columbia periods: if managerial override of a documented, uncertified engineering concern recurs (as with Columbia foam-strike), the binding-veto reading was not durably institutionalized in practice, regardless of its formal adoption.',
    'If the binding reading never held in practice, this story''s classification describes an aspirational/formal arrangement rather than the operative one, and the sibling management_compliance_narrative reading is the better description of NASA''s actual post-Rogers behavior over the following decades.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_binding_or_advisory, empirical, 'Whether the engineering veto was ever actually binding in institutional practice, or only formally declared.').

omega_variable(
    which_reading_the_commission_intended,
    'Which of the three readings (absolute threshold, risk acceptance, compliance narrative) did the Rogers Commission itself intend as the operative interpretation of its own findings?',
    'Close textual analysis of the Commission''s recommendations section versus its findings section — recommendations may prescribe process (compliance-narrative-compatible) while findings describe a specific uncorrected failure mode (absolute-threshold-compatible), meaning the kernel text itself may not disambiguate.',
    'If the kernel text is genuinely ambiguous between readings, no single reading can claim to be ''the'' correct interpretation of Rogers, and all three readings are equally legitimate structural claims — this bears on how much authority any one reading, including this one, can claim over the others.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_the_commission_intended, conceptual, 'Whether the kernel text itself determines a single correct reading or is irreducibly ambiguous across the three.').

omega_variable(
    coordination_extraction_durability,
    'Is the coordination function (crew safety via binding engineering veto) separable from the extraction cost it imposes (indefinite schedule delay on customers with no standing), or does the coordination function require exactly that degree of schedule sacrifice to remain credible?',
    'Examine whether a time-bounded or probabilistically-capped version of the veto (a middle position between this reading and actuarial_risk_acceptance) could deliver comparable safety outcomes with lower schedule cost — if so, the current all-or-nothing threshold contains extractable slack beyond the coordination requirement.',
    'If separable, part of the measured suppression is excess beyond what crew safety requires and the tangled_rope classification is well-founded; if inseparable, the arrangement is closer to a pure Rope with an unusually high but necessary suppression cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_durability, conceptual, 'Whether the veto''s schedule cost is the minimum necessary for the coordination function or exceeds it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__engineering_absolute_threshold, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roge_tr_t0, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 0, 0.05).
narrative_ontology:measurement(roge_tr_t4, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 4, 0.06).
narrative_ontology:measurement(roge_tr_t8, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 8, 0.08).
narrative_ontology:measurement(roge_tr_t12, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 12, 0.1).
narrative_ontology:measurement(roge_tr_t16, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 16, 0.12).
narrative_ontology:measurement(roge_tr_t20, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 20, 0.14).
narrative_ontology:measurement(roge_tr_t24, rogers_commission_findings__engineering_absolute_threshold, theater_ratio, 24, 0.15).

% Extraction over time
narrative_ontology:measurement(roge_be_t0, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(roge_be_t4, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 4, 0.14).
narrative_ontology:measurement(roge_be_t8, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 8, 0.18).
narrative_ontology:measurement(roge_be_t12, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 12, 0.2).
narrative_ontology:measurement(roge_be_t16, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 16, 0.21).
narrative_ontology:measurement(roge_be_t20, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(roge_be_t24, rogers_commission_findings__engineering_absolute_threshold, base_extractiveness, 24, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(roge_su_t0, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(roge_su_t4, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 4, 0.92).
narrative_ontology:measurement(roge_su_t8, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 8, 0.88).
narrative_ontology:measurement(roge_su_t12, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 12, 0.86).
narrative_ontology:measurement(roge_su_t16, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 16, 0.86).
narrative_ontology:measurement(roge_su_t20, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 20, 0.86).
narrative_ontology:measurement(roge_su_t24, rogers_commission_findings__engineering_absolute_threshold, suppression_requirement, 24, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__engineering_absolute_threshold, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rogers_commission_findings__engineering_absolute_threshold, 0.12).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__management_compliance_narrative).
narrative_ontology:affects_constraint(rogers_commission_findings__engineering_absolute_threshold, rogers_commission_findings__actuarial_risk_acceptance).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the rogers_commission_findings kernel, decomposed per the ε-invariance principle: engineering_absolute_threshold (this file, low ε ~0.22, tangled_rope, binding veto), management_compliance_narrative (procedural documentation substitutes for substantive fix — expected higher ε, extraction concentrated on whichever party's safety concern gets paperwork-only treatment), and actuarial_risk_acceptance (probabilistic risk acceptance by informed decision-makers — expected ε and victim structure differ again, since risk is formally accepted rather than a hard stop imposed). Each reading is authored as a separate story with its own ε, beneficiaries, and victims; they are linked here rather than merged because measuring 'the Rogers findings' one way (binding technical threshold) versus another (procedural compliance) versus a third (quantified risk acceptance) produces materially different extraction profiles — exactly the ε-invariance test for decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
