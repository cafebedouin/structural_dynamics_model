% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__collective_right_reading, []).

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
 *   constraint_id: second_amendment_arms_right__collective_right_reading
 *   human_readable: Second Amendment as State Militia Prerogative (Collective Right Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the collective-right reading of the Second
 *   Amendment kernel: the amendment protects the authority of state
 *   governments to organize and maintain militias, and confers no independent
 *   constitutional right on individuals to possess firearms outside that
 *   organized militia context. Under this reading, civilian firearm
 *   regulation faces no Second Amendment individual-rights barrier; the
 *   amendment's prefatory clause ('A well regulated Militia...') is read as
 *   defining and limiting the scope of the operative clause. This reading
 *   commanded substantial lower-court support through most of the 20th
 *   century (e.g., United States v. Miller, 1939, and subsequent circuit
 *   court doctrine) before being displaced as controlling federal doctrine by
 *   District of Columbia v. Heller (2008), which adopted the individual-right
 *   reading instead. It remains a live position in dissenting jurisprudence,
 *   comparative constitutional scholarship, and some state-level
 *   interpretation.
 *
 * KEY AGENTS:
 *   - state_governments: institutional beneficiary retaining militia-organizing authority
 *   - individual_gun_owners_outside_militia_service: powerless payer bearing regulatory exposure with no independent constitutional floor
 *   - federal_judiciary: agenda_setter applying the reading to adjudicate regulatory challenges
 *   - originalist_legal_scholars: excluded voice whose textual/historical arguments are resolved against within this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__collective_right_reading, 0.18).
domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, 0.28).
domain_priors:theater_ratio(second_amendment_arms_right__collective_right_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__collective_right_reading, "Second Amendment as State Militia Prerogative (Collective Right Reading)").
narrative_ontology:topic_domain(second_amendment_arms_right__collective_right_reading, "constitutional_law/political_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__collective_right_reading, '23f3a0c3-b184-4e28-b4a8-725a1f75f949').
narrative_ontology:cs_kernel_codification('23f3a0c3-b184-4e28-b4a8-725a1f75f949', fixed_text).
narrative_ontology:cs_authority_grounding('23f3a0c3-b184-4e28-b4a8-725a1f75f949', lineage).
narrative_ontology:cs_interpretation_layer_present('23f3a0c3-b184-4e28-b4a8-725a1f75f949').
narrative_ontology:cs_reading_relation('23f3a0c3-b184-4e28-b4a8-725a1f75f949', second_amendment_arms_right__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('23f3a0c3-b184-4e28-b4a8-725a1f75f949', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('23f3a0c3-b184-4e28-b4a8-725a1f75f949', foundational, prefatory_clause_limits_operative_scope).
narrative_ontology:cs_axiom_status(prefatory_clause_limits_operative_scope, holdable).
narrative_ontology:cs_axiom_grounding('23f3a0c3-b184-4e28-b4a8-725a1f75f949', prefatory_clause_limits_operative_scope, conventional).
narrative_ontology:cs_axiom('23f3a0c3-b184-4e28-b4a8-725a1f75f949', foundational, state_militia_organization_is_the_protected_interest).
narrative_ontology:cs_axiom_status(state_militia_organization_is_the_protected_interest, holdable).
narrative_ontology:cs_axiom_grounding('23f3a0c3-b184-4e28-b4a8-725a1f75f949', state_militia_organization_is_the_protected_interest, conventional).
narrative_ontology:cs_reference_frame('23f3a0c3-b184-4e28-b4a8-725a1f75f949', pre_heller_militia_centered_doctrine).
narrative_ontology:cs_drift_state('23f3a0c3-b184-4e28-b4a8-725a1f75f949', post_heller_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('23f3a0c3-b184-4e28-b4a8-725a1f75f949', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, gun_control_advocates).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, federal_regulatory_authority).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, individual_gun_owners_outside_militia_service).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, organized_militia_members).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, organized_militia_members).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, state_militia_organizing_authority).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, federalism_reserved_powers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, states retain constitutionally protected authority to organize, arm, and discipline militias without federal preemption of that specific function. States that want robust gun regulation face no Second Amendment obstacle to regulating individual firearm possession outside an organized militia context; the amendment simply does not reach that question under this reading.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, state_governments, beneficiary,
    institutional, generational, analytical, national).

% Advocacy organizations and legislators who want broad regulatory latitude over civilian firearm ownership benefit directly: this reading removes the individual-rights floor that would otherwise require heightened judicial scrutiny of gun control measures, leaving ordinary rational-basis or interest-balancing review as the operative standard.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, gun_control_advocates, beneficiary,
    organized, biographical, mobile, national).

% Federal legislative and administrative bodies gain expanded room to regulate, tax, register, or restrict civilian firearms possession because no individual constitutional right stands as a check on such measures outside the militia context; regulatory design is unconstrained by Second Amendment individual-rights litigation risk.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_regulatory_authority, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__collective_right_reading, federal_regulatory_authority, agenda_setter).

% Private citizens who own or wish to own firearms for self-defense, sport, or other purposes unconnected to organized militia service find no independent constitutional protection under this reading. Any regulation, restriction, licensing burden, or prohibition they face must be litigated on other grounds (due process, equal protection, state constitutional provisions) because the Second Amendment itself does not reach them. Exit is effectively foreclosed at the federal constitutional level; recourse depends entirely on state law or political mobilization.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, individual_gun_owners_outside_militia_service, payer,
    powerless, biographical, trapped, national).

% Members of state-organized militia units (historically) or National Guard-adjacent bodies retain the amendment's protection for arms connected to that service, but the protection is contingent on maintaining organized, state-sanctioned status — an individual who leaves militia service loses the constitutional footing this reading recognizes.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, organized_militia_members, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__collective_right_reading, organized_militia_members, payer).

% Courts applying this reading determine which regulations survive scrutiny by asking whether the challenged law burdens militia-connected arms use, not individual possession generally. This reading was the dominant judicial posture for most of the 20th century (culminating in doctrinal debate resolved differently in Heller, 2008) and remains the position advanced by dissenting justices and much of the gun-control legal academy.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Scholars and jurists who read the amendment as protecting a pre-existing individual right are excluded from this reading's operative framework by construction — their historical and textual arguments (the operative clause as codifying, not creating, an individual right) are treated as unpersuasive within this reading, not engaged as live alternatives inside it.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, originalist_legal_scholars, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__collective_right_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__collective_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves a federalism-era coordination problem: which level of government controls the organization and arming of military forces capable of resisting both foreign invasion and federal standing-army overreach. The amendment, on this reading, coordinates the relationship between state militia authority and a nascent federal government wary of disarming the states.
% TRANSFER_FUNCTION: Moves constitutional protection away from individual firearm possessors and toward state institutional authority over militia organization; correspondingly shifts regulatory latitude toward legislatures (state and federal) and away from individual claimants who might otherwise invoke a personal constitutional right to resist firearm regulation.
% ABSENT_VOICES: Individual rights claimants and originalist historians are structurally absent from this reading's operative logic — their textual and historical arguments about the prefatory/operative clause relationship are treated as resolved against them, not weighed as contested within the reading itself.
% DISAPPEARANCE_RATIONALE: If courts abandoned the collective-right reading entirely (as occurred substantially with Heller v. District of Columbia, 2008), gun control legislation historically upheld under it would face renewed individual-rights challenges — a real rearrangement for regulators and litigants. But because the reading was already displaced as controlling federal doctrine after Heller, its practical 'disappearance' has already happened in significant part; what remains contested is whether it persists as the correct reading (dissenting jurisprudence, some state constitutional interpretation) or as a historical artifact.
% FOUNDING_PROBLEM: At the founding, the newly formed states feared a federal standing army could be used to disarm state militias and consolidate coercive power, undermining state sovereignty and the republican check that armed, organized state militias provided against federal tyranny.
% FOUNDING_PROBLEM_CORROBORATION: State governments and gun-control-oriented legal scholars attest the militia-authority problem remains a live federalism concern, citing continued state control over National Guard organization. Individual-rights originalist scholars and the Heller majority (2008, per Justice Scalia) attest that the militia-only framing was always a mischaracterization of the founding-era text and that the true founding problem was preserving a pre-existing individual right against federal disarmament — corroboration for this reading's founding-problem narrative comes primarily from within its own supporting tradition (pre-Heller circuit court doctrine, some founding-era state militia statutes), not from a source outside the interpretive camps that benefit from it.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__collective_right_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_arms_right__collective_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__collective_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_arms_right__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__collective_right_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__collective_right_reading_tests).
:- end_tests(second_amendment_arms_right__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.18) because this reading's operative effect is primarily permissive of regulation, not extractive in the sense of transferring wealth or resources from a payer class to a concentrated beneficiary — the 'cost' borne by individual gun owners is the absence of a constitutional shield, not a resource transfer. Suppression is moderate (0.28): the reading does not itself coerce firearm surrender, but it removes the primary federal constitutional obstacle to state and federal regulatory suppression of civilian possession, which is a real (if indirect) suppressive effect on the excluded class. Accessibility collapse is moderate (0.35) because alternative interpretive routes (state constitutional individual-rights provisions, political mobilization) remain genuinely available even where this reading controls. Resistance is high (0.62) because this reading has been the subject of sustained, well-resourced originalist and individual-rights advocacy contesting it at every level, culminating in its doctrinal displacement in Heller.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and federal regulatory bodies sit near the beneficiary end: the reading expands their room to legislate without a countervailing individual-rights constraint, and they bear none of the reading's costs directly. Individual gun owners outside militia service sit near the target end: they lose access to a federal constitutional argument they would otherwise have under a sibling reading, and their exit options are effectively foreclosed at the constitutional level (though not eliminated at the state-law or political level, hence 'trapped' rather than a more extreme label). Organized militia members occupy a hybrid position — protected while militia-connected, exposed the moment that connection lapses.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state fear of federal disarmament of militias undermining republican checks on federal power) is contested as either still live (state governments continue to organize National Guard units, a live federalism interest) or effectively resolved by the professionalization of state militias into federally-integrated National Guard structures, which arguably moots the original anti-federal-tyranny concern this reading was built to address. This tension is exactly what the founding_problem_status field is designed to surface: a reading whose founding problem has been substantially mooted by institutional change (militia to National Guard) but whose interpretive framework persists risks becoming a piton-like relic in doctrine even while structurally coded here as a rope. This story does not resolve that; it names the contest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_operative_clause_relationship,
    'Does the prefatory clause (''A well regulated Militia, being necessary to the security of a free State'') limit the scope of the operative clause (''the right of the people to keep and bear Arms, shall not be infringed''), or does it merely announce one purpose among others the operative clause serves?',
    'This is a contested question of constitutional interpretation and founding-era linguistic/legal convention (grammatical analysis of 18th-century legal drafting, comparison to contemporaneous state constitutional militia clauses) that has been argued by historians and linguists on both sides without consensus resolution; it is not empirically resolvable in the way a factual dispute would be.',
    'If the prefatory clause is read as limiting (this reading''s premise), the collective-right reading follows and individual possession outside militia service receives no protection. If read as merely illustrative (the individual_right_reading''s premise), the operative clause stands independently and protects individual possession broadly. This is the central axis on which the kernel''s readings diverge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prefatory_operative_clause_relationship, conceptual, 'Whether the prefatory militia clause limits or merely illustrates the operative arms-bearing clause — the core interpretive fork between readings.').

omega_variable(
    militia_national_guard_continuity,
    'Is the modern National Guard the same institution this reading''s ''state militia'' refers to, such that the founding problem (state control over militia organization) remains live through the Guard, or has federalization of the Guard (Militia Act of 1903, subsequent federal funding and command integration) effectively mooted the state-militia-authority premise this reading depends on?',
    'Historical and legal analysis of the degree of federal versus state control over National Guard funding, command structure, and deployment authority; comparison to founding-era militia organization under state control.',
    'If the National Guard is substantially federalized, the founding problem this reading addresses (state militia autonomy against federal encroachment) may be effectively dead, undermining the reading''s continued doctrinal relevance even if the text is unchanged — a live candidate for mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_national_guard_continuity, empirical, 'Whether National Guard federalization has mooted the state-militia-authority premise underlying this reading.').

omega_variable(
    collective_right_naturalness_or_construction,
    'Is the collective-right reading a genuine recovery of founding-era original meaning, or a 20th-century judicial construction favoring regulatory latitude, retrofitted onto the text?',
    'Comparative analysis of state constitutional arms provisions contemporaneous with the federal amendment, founding-era commentary (Federalist Papers, ratification debates), and the historical trajectory of judicial interpretation from the 1790s through Miller (1939) to Heller (2008).',
    'If this reading is a later construction rather than original meaning, it weakens the reading''s claim to textual fidelity relative to the individual_right_reading; if it is a genuine original-meaning recovery, the individual_right_reading''s ascendance in Heller becomes the interpretive departure instead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_right_naturalness_or_construction, conceptual, 'Whether the collective-right reading reflects genuine original meaning or a later regulatory-era judicial construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__collective_right_reading, 1791, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_arms_right__collective_right_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement_basis(seco_tr_t1791, observed).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_arms_right__collective_right_reading, theater_ratio, 1900, 0.14).
narrative_ontology:measurement_basis(seco_tr_t1900, observed).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_arms_right__collective_right_reading, theater_ratio, 1939, 0.18).
narrative_ontology:measurement_basis(seco_tr_t1939, observed).
narrative_ontology:measurement(seco_tr_t1976, second_amendment_arms_right__collective_right_reading, theater_ratio, 1976, 0.2).
narrative_ontology:measurement_basis(seco_tr_t1976, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_arms_right__collective_right_reading, theater_ratio, 2008, 0.3).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2025, second_amendment_arms_right__collective_right_reading, theater_ratio, 2025, 0.22).
narrative_ontology:measurement_basis(seco_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1791, 0.12).
narrative_ontology:measurement_basis(seco_be_t1791, observed).
narrative_ontology:measurement(seco_be_t1900, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1900, 0.14).
narrative_ontology:measurement_basis(seco_be_t1900, observed).
narrative_ontology:measurement(seco_be_t1939, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1939, 0.15).
narrative_ontology:measurement_basis(seco_be_t1939, observed).
narrative_ontology:measurement(seco_be_t1976, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1976, 0.17).
narrative_ontology:measurement_basis(seco_be_t1976, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_arms_right__collective_right_reading, base_extractiveness, 2008, 0.22).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2025, second_amendment_arms_right__collective_right_reading, base_extractiveness, 2025, 0.18).
narrative_ontology:measurement_basis(seco_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(second_amendment_arms_right__collective_right_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__collective_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__collective_right_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, civic_republican_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the second_amendment_arms_right kernel, decomposed per the epsilon-invariance principle because the natural-language label 'the Second Amendment right' conflates structurally distinct claims about who holds the right and what it protects. The collective_right_reading (this story) authors low extraction (0.18) because, on its own terms, the reading primarily permits regulation rather than transferring resources; the individual_right_reading and civic_republican_reading are expected to author different beneficiary/victim structures and different ε values reflecting their distinct premises about who is protected and who is burdened. All three share the founding-era text and historical record but diverge on the prefatory/operative clause relationship, which is the axis this story's primary omega documents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
