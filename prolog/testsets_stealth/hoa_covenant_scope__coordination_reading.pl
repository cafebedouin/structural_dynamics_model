% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__coordination_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: hoa_covenant_scope__coordination_reading
 *   human_readable: HOA Covenant as Infrastructure Coordination Compact (Coordination Reading)
 *   domain: economic/legal/governance
 *
 * SUMMARY:
 *   A recorded declaration of covenants governs a common-interest residential
 *   community: every owner owes a periodic assessment into a fund that
 *   maintains streets, shared building elements, and common facilities no
 *   household controls alone, and a short written rule set addresses boundary
 *   and nuisance spillovers. Enforcement is confined to recovering unpaid
 *   assessments (demand, lien) and remedying conditions that fail written,
 *   objective standards. This file instantiates the coordination_reading of
 *   the hoa_covenant_scope kernel; the behavioral_control_reading and
 *   extraction_reading are separate constraint stories linked through
 *   network.affects_constraints, per the epsilon-invariance decomposition of
 *   the colloquial label 'HOA covenant.' KEY AGENTS (by structural
 *   relationship): - hoa_board_of_directors: agenda-setting administrator
 *   (organized/constrained) — levies assessments, awards maintenance
 *   contracts, enforces the narrow rule set - all_homeowners: primary
 *   beneficiary and payer (moderate/constrained) — fund the commons and
 *   consume it symmetrically; hold electoral and amendment checks -
 *   assessment_averse_free_riders: marginal burdened seat
 *   (moderate/constrained) — would shirk common upkeep; the assessment
 *   removes that option - maintenance_vendors: incidental beneficiary
 *   (moderate/mobile) — paid from the fund at arm's length -
 *   tenant_residents: excluded voice (powerless/mobile) — bound by rules
 *   without vote or assessment standing - state_property_courts: analytical
 *   observer (institutional/analytical) — police the boundary of enforcement
 *   authority
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__coordination_reading, 0.15).
domain_priors:suppression_score(hoa_covenant_scope__coordination_reading, 0.2).
domain_priors:theater_ratio(hoa_covenant_scope__coordination_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__coordination_reading, rope).
narrative_ontology:human_readable(hoa_covenant_scope__coordination_reading, "HOA Covenant as Infrastructure Coordination Compact (Coordination Reading)").
narrative_ontology:topic_domain(hoa_covenant_scope__coordination_reading, "economic/legal/governance").

domain_priors:requires_active_enforcement(hoa_covenant_scope__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__coordination_reading, '3ac193a2-c777-4391-a552-c3143a07d17f').
narrative_ontology:cs_kernel_codification('3ac193a2-c777-4391-a552-c3143a07d17f', formalized).
narrative_ontology:cs_authority_grounding('3ac193a2-c777-4391-a552-c3143a07d17f', lineage).
narrative_ontology:cs_interpretation_layer_present('3ac193a2-c777-4391-a552-c3143a07d17f').
narrative_ontology:cs_reading_relation('3ac193a2-c777-4391-a552-c3143a07d17f', hoa_covenant_scope__behavioral_control_reading, influences).
narrative_ontology:cs_reading_relation('3ac193a2-c777-4391-a552-c3143a07d17f', hoa_covenant_scope__extraction_reading, influences).
narrative_ontology:cs_axiom('3ac193a2-c777-4391-a552-c3143a07d17f', foundational, covenant_authority_limited_to_shared_costs_and_externalities).
narrative_ontology:cs_axiom_status(covenant_authority_limited_to_shared_costs_and_externalities, holdable).
narrative_ontology:cs_axiom_grounding('3ac193a2-c777-4391-a552-c3143a07d17f', covenant_authority_limited_to_shared_costs_and_externalities, instrumental).
narrative_ontology:cs_axiom('3ac193a2-c777-4391-a552-c3143a07d17f', secondary, enforcement_requires_objectively_verifiable_harm).
narrative_ontology:cs_axiom_status(enforcement_requires_objectively_verifiable_harm, holdable).
narrative_ontology:cs_axiom_grounding('3ac193a2-c777-4391-a552-c3143a07d17f', enforcement_requires_objectively_verifiable_harm, conventional).
narrative_ontology:cs_reference_frame('3ac193a2-c777-4391-a552-c3143a07d17f', infrastructure_coordination_compact).
narrative_ontology:cs_drift_state('3ac193a2-c777-4391-a552-c3143a07d17f', contemporary_enforcement_record, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('3ac193a2-c777-4391-a552-c3143a07d17f', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__coordination_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, all_homeowners).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, maintenance_vendors).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, assessment_averse_free_riders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, all_homeowners).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__coordination_reading, private_covenant_infrastructure_finance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected owner-volunteers who set the annual assessment to cover the maintenance budget, solicit and award vendor contracts for paving, roofing, and common-area upkeep, maintain the reserve fund, and enforce the recorded rules — sending demand letters and recording liens when assessments go unpaid, and citing conditions only against written, objective standards. They serve unpaid terms, can be voted out at annual elections, and remain bound by the same assessments as their neighbors after leaving office.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, hoa_board_of_directors, agenda_setter,
    organized, biographical, constrained, local).

% Own homes in the community and pay the periodic assessment into the shared maintenance fund. In return they receive maintained private streets, covered building elements, insured common facilities, and uniform handling of boundary and nuisance issues. They vote on budgets, elect the board, and can amend or terminate the declaration by the supermajority the document specifies. Leaving means selling the home, with the amenity quality the assessments buy reflected in the sale price.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, all_homeowners, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__coordination_reading, all_homeowners, payer).

% Owners who place low value on the shared amenities and would prefer to spend nothing on common upkeep, relying on neighbors' contributions to keep streets and facilities usable. The assessment obligation removes that option: they owe the same dues as everyone else, and nonpayment escalates to liens. Their grievance is the obligation itself rather than any shortfall in what the fund delivers.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, assessment_averse_free_riders, payer,
    moderate, biographical, constrained, local).

% Paving, roofing, landscaping, and management firms that win the board's contracts and are paid out of the assessment fund at negotiated rates. They hold no governance role, bid alongside competitors, and can walk away from the community when a contract ends.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, maintenance_vendors, beneficiary,
    moderate, immediate, mobile, local).

% Rent households inside the community. They are subject to the same nuisance and common-area rules as owners but hold no vote, pay no assessment, and reach the board only through their landlords. Moving out at lease end is their main recourse.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, tenant_residents, excluded,
    powerless, immediate, mobile, local).

% Hear disputes over assessment collection, lien foreclosures, and alleged rule overreach. They decide whether enforcement stayed within the recorded declaration and statutory limits, and their published opinions shape what boards across the state attempt.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, state_property_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__coordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools each household's assessment into a single fund that maintains infrastructure no owner controls alone — private streets, shared roofs and facades, common facilities — and schedules that upkeep once for everyone; a parallel written rule set resolves boundary and nuisance spillovers between adjacent lots using verifiable standards.
% TRANSFER_FUNCTION: Moves assessment dollars from every owner household into the common fund, and from the fund to maintenance vendors and reserves; moves enforcement effort toward documented nonpayment and objectively verifiable nuisance conditions.
% ABSENT_VOICES: Tenant residents are bound by the rules but hold no vote and no seat at board meetings; future buyers inherit amendments passed before they arrive; owners of neighboring non-member properties affected by drainage or runoff choices have no forum. All three would press for notice-and-comment procedures the current process lacks.
% DISAPPEARANCE_RATIONALE: Private streets, shared roofs, and common facilities would lose their funding mechanism overnight: no owner could resurface a street alone, insurers and lenders would reprice or withdraw from uncovered common elements, and boundary disputes would fall to ad hoc litigation. Owner associations substantially equivalent to the current one would re-form within a few budget cycles.
% FOUNDING_PROBLEM: Developer-built common-interest communities contained infrastructure municipalities refused to accept — private streets, storm drains, shared buildings — and dense attached housing generated boundary and nuisance spillovers no single owner could manage. The declaration was written to finance and govern that shared stock.
% FOUNDING_PROBLEM_CORROBORATION: Municipal engineering departments' standing refusal to accept dedication of the private streets attests the financing problem from outside the owner body; federal mortgage agency eligibility standards for common-interest communities and published appellate opinions reciting the maintenance rationale corroborate it further, as does the urban-planning literature on common-interest development. None of these sources depends on board self-report.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hoa_covenant_scope__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__coordination_reading, 0.15, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__coordination_reading_tests).
:- end_tests(hoa_covenant_scope__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because assessments are sized to the maintenance budget and the benefit stream (usable streets, sound shared elements) is roughly congruent with what each household pays; the residual reflects contracting discretion and administrative overhead. Suppression is low (0.20) because coercion is limited to debt collection against actual dues and injunctive relief against documented violations, with electoral replacement and supermajority amendment open as exits. Theater is low (0.12): meetings and newsletters carry ritual weight, but the fund visibly converts to pavement and roofs. Accessibility collapse is moderate-low (0.40): selling, amending, municipalizing the streets, or buying in non-covenant housing stock remain workable alternatives at real but not prohibitive friction. Resistance is moderate-low (0.30): periodic assessment protests and contested board elections, no systematic opposition. Claim and metrics are independent authored facts: the rope claim rests on the structural data (symmetric benefit, narrow scope, live function, cheap amendment exit), while the metric values describe observed operation. Suppression is authored as a raw structural property; only extractiveness is scaled by the engine. The two measurement series share one time grid (0 to 24, step 4); a suppression_requirement series is deliberately omitted because the enforcement picture is static over the interval — the scalar carries it.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit the same recorded text differently. The board experiences the arrangement as stewardship — a budget it administers under fiduciary duty. Ordinary owners experience fair cost-sharing: dues in, pavement out. Assessment-averse owners experience an imposed obligation — the same dues read as confiscation of a shirking option they valued. Tenant residents experience rule-following without voice: obligations flow to them through landlords who hold the vote. The engine computes these divergences from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   All_homeowners sit near the beneficiary end: they fund the arrangement and receive its output, with constrained (not trapped) exit nudging d slightly above a pure-subsidy position. Maintenance_vendors sit nearest the beneficiary pole — paid at arm's length, fully mobile, holding no governance stake. Assessment_averse_free_riders are the nearest-to-target seat: they bear the full assessment while valuing the benefit least, so their derived d is elevated; with base epsilon at 0.15, even an amplified chi for this seat stays modest, which is the quantitative signature of the coordination reading. Tenant_residents bear rule burdens without assessment standing — a moderate d from the excluded position. State_property_courts hold the analytical seat with no material directionality. The engine owns the arithmetic; these declarations supply the structure it reads.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: the private streets still need resurfacing and the shared elements still need roofs, so the arrangement persists by function rather than inertia. Theater is low and nearly flat across the interval, gain_flow is affirmatively diffuse, and fixing_cost is cheap — owners can amend or dissolve by supermajority, so the arrangement survives only while owners judge it worth the dues. That combination is the opposite of a zombie profile: no concentrated capturer, no theatrical maintenance propping up an atrophied function, no prohibitively expensive removal shielding decay. Mandatrophy is not declared. The leading indicator to watch is the theater_ratio trajectory: a sustained rise past mid-range would signal proxy goals replacing the maintenance function and would date a transition out of the pure-coordination profile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_instantiation_audit,
    'This file instantiates the coordination_reading of kernel hoa_covenant_scope; does the standing arrangement''s actual enforcement record match this reading, or would the behavioral_control_reading or extraction_reading better describe the covenant as it operates?',
    'Code a multi-year sample of board enforcement actions and architectural decisions against the narrow-scope criterion: the share of actions targeting assessment recovery and objective nuisance versus aesthetics and fines; compare the resulting profile against the sibling stories'' authored epsilon and victim structures.',
    'If aesthetic and fine actions dominate the record, this reading misdescribes the arrangement and the low epsilon authored here attaches to a different constraint than the one operating; classification migrates toward the siblings'' structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_instantiation_audit, empirical, 'Which reading of the covenant kernel the standing arrangement actually instantiates.').

omega_variable(
    free_rider_burden_character,
    'Is the obligation borne by assessment-averse owners a constitutive coordination cost, or the first increment of a fine-based extraction layer?',
    'Track enforcement against delinquent accounts over time: cost-recovery liens sized to actual dues versus punitive fine schedules and selective escalation.',
    'Punitive or selective escalation would raise effective extraction on this seat and push the arrangement toward the extraction_reading''s structure; flat cost-recovery confirms the burden as the price of the commons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_rider_burden_character, empirical, 'Whether the free-rider seat''s burden is coordination cost or emerging extraction.').

omega_variable(
    benefit_symmetry_across_lots,
    'Are the benefits of the maintenance fund actually symmetric across owners, or do lot position and unit type concentrate them?',
    'Hedonic analysis comparing assessment incidence against amenity capitalization by lot position and unit type.',
    'Systematic asymmetry would make some owners net payers into others'' benefits, raising measured extraction above the symmetric baseline authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benefit_symmetry_across_lots, empirical, 'Whether the all_homeowners beneficiary declaration conceals concentrated incidence.').

omega_variable(
    scope_creep_trajectory,
    'Will the covenant''s enforcement scope stay limited to infrastructure cost recovery and objective nuisance, or drift toward aesthetic and behavioral rules?',
    'Longitudinal coding of declaration amendments and board rule adoptions across the interval; flag any new rule class lacking an infrastructure or externality rationale.',
    'Scope creep raises suppression and extractiveness jointly and would date a transition out of the pure-coordination profile; a flat scope preserves it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_creep_trajectory, empirical, 'Forward-looking drift risk in the covenant''s enforcement scope.').

omega_variable(
    cs_kernel_framing_underdetermination,
    'Is the commitment-system kernel the recorded declaration text itself, or the owner-consent tradition the declaration presupposes — and does the choice change the commitment-system classification?',
    'Test both framings against the drift record: under the text-kernel framing, amendments are revisions absorbed by the formalized kernel; under the consent-tradition framing, amendments are the kernel''s normal operation and only judicially compelled changes count as drift.',
    'The text-kernel framing makes the minor practice drift reported here load-bearing for t2 computation; the consent-tradition framing renders the reference frame effectively stable. Signals guiding the choice made here: the declaration is formally amendable and courts treat the recorded text as operative, favoring the text-kernel framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_kernel_framing_underdetermination, conceptual, 'Two coherent framings of the CS kernel with divergent drift consequences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__coordination_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__coordination_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(hoa__tr_t0, observed).
narrative_ontology:measurement(hoa__tr_t4, hoa_covenant_scope__coordination_reading, theater_ratio, 4, 0.07).
narrative_ontology:measurement_basis(hoa__tr_t4, observed).
narrative_ontology:measurement(hoa__tr_t8, hoa_covenant_scope__coordination_reading, theater_ratio, 8, 0.08).
narrative_ontology:measurement_basis(hoa__tr_t8, observed).
narrative_ontology:measurement(hoa__tr_t12, hoa_covenant_scope__coordination_reading, theater_ratio, 12, 0.09).
narrative_ontology:measurement_basis(hoa__tr_t12, observed).
narrative_ontology:measurement(hoa__tr_t16, hoa_covenant_scope__coordination_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement_basis(hoa__tr_t16, observed).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__coordination_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement_basis(hoa__tr_t20, observed).
narrative_ontology:measurement(hoa__tr_t24, hoa_covenant_scope__coordination_reading, theater_ratio, 24, 0.12).
narrative_ontology:measurement_basis(hoa__tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__coordination_reading, base_extractiveness, 0, 0.09).
narrative_ontology:measurement_basis(hoa__be_t0, observed).
narrative_ontology:measurement(hoa__be_t4, hoa_covenant_scope__coordination_reading, base_extractiveness, 4, 0.1).
narrative_ontology:measurement_basis(hoa__be_t4, observed).
narrative_ontology:measurement(hoa__be_t8, hoa_covenant_scope__coordination_reading, base_extractiveness, 8, 0.11).
narrative_ontology:measurement_basis(hoa__be_t8, observed).
narrative_ontology:measurement(hoa__be_t12, hoa_covenant_scope__coordination_reading, base_extractiveness, 12, 0.12).
narrative_ontology:measurement_basis(hoa__be_t12, observed).
narrative_ontology:measurement(hoa__be_t16, hoa_covenant_scope__coordination_reading, base_extractiveness, 16, 0.13).
narrative_ontology:measurement_basis(hoa__be_t16, observed).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__coordination_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement_basis(hoa__be_t20, observed).
narrative_ontology:measurement(hoa__be_t24, hoa_covenant_scope__coordination_reading, base_extractiveness, 24, 0.15).
narrative_ontology:measurement_basis(hoa__be_t24, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hoa_covenant_scope__coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__behavioral_control_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'HOA covenant' decomposes into three structurally distinct claims per the epsilon-invariance principle. This file authors the coordination_reading (low epsilon, symmetric benefit, narrow enforcement scope); hoa_covenant_scope__behavioral_control_reading and hoa_covenant_scope__extraction_reading author the aesthetic-uniformity and revenue/power readings with their own epsilon values, victim sets, and types. The upstream/downstream structure runs from this reading outward: judicial reliance on the maintenance-and-externality rationale sets the enforceability conditions under which the siblings' practices stand or fall, so the affects_constraints edges run from this file to both siblings. Each story carries a single stable epsilon; no observable-selection parameter mediates between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
