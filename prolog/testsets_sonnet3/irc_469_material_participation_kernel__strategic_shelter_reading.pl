% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strategic_shelter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__strategic_shelter_reading, []).

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
 *   constraint_id: irc_469_material_participation_kernel__strategic_shelter_reading
 *   human_readable: Material Participation Threshold — Strategic Shelter Reading (IRC §469)
 *   domain: tax_law/real_estate_investment/regulatory_interpretation
 *
 * SUMMARY:
 *   IRC §469's material participation tests were designed to distinguish
 *   active business involvement from passive investment for purposes of the
 *   passive activity loss limitation. This story authors the STRATEGIC
 *   SHELTER READING: the seven regulatory tests (particularly the 100-hour
 *   test and the facts-and-circumstances catch-all) are permissively
 *   interpretable, self-reported hour logs are difficult to audit after the
 *   fact, and grouping elections let taxpayers aggregate activities across
 *   properties to clear the threshold. Under this reading the standard's low
 *   compliance friction is a feature that has been systematically exploited:
 *   it produces a broad qualifying population reachable primarily by those
 *   with the capital and advisory access to structure toward it, converting
 *   what the 1986 passive-loss rules intended to disallow back into
 *   deductible active-income offsets. This is deliberately a different
 *   constraint from the sibling strict_gatekeeper_reading, which holds that
 *   verifiable, substantial personal labor with a high documentation bar is
 *   what the statute actually requires — that reading's ε, beneficiary/victim
 *   structure, and classification are authored separately and are not blended
 *   here.
 *
 * KEY AGENTS:
 *   - high_income_real_estate_professionals: Primary beneficiary (powerful/arbitrage) — structures ownership and hours to clear the threshold
 *   - passive_loss_shelter_promoters: Agenda-setter (organized/mobile) — designs and sells structuring products around the permissive threshold
 *   - tax_advisory_firms: Beneficiary/agenda-setter (organized/mobile) — bills for ongoing advisory support the soft standard requires
 *   - general_tax_base and wage_earning_taxpayers: Primary victims (powerless/trapped) — absorb the aggregate revenue shortfall and the comparative disadvantage of an uneven standard
 *   - irs_examination_function: Institutional payer (institutional/constrained) — bears the audit burden of an unverifiable facts-and-circumstances standard
 *   - tax_policy_scholars: Analytical observer — documents the aggregate effect from outside the benefiting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, 0.61).
domain_priors:suppression_score(irc_469_material_participation_kernel__strategic_shelter_reading, 0.28).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strategic_shelter_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strategic_shelter_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strategic_shelter_reading, "Material Participation Threshold — Strategic Shelter Reading (IRC §469)").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strategic_shelter_reading, "tax_law/real_estate_investment/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strategic_shelter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strategic_shelter_reading, 'f1677aea-558b-48ff-8660-9ec722d72ba2').
narrative_ontology:cs_kernel_codification('f1677aea-558b-48ff-8660-9ec722d72ba2', formalized).
narrative_ontology:cs_authority_grounding('f1677aea-558b-48ff-8660-9ec722d72ba2', practice).
narrative_ontology:cs_interpretation_layer_present('f1677aea-558b-48ff-8660-9ec722d72ba2').
narrative_ontology:cs_reading_relation('f1677aea-558b-48ff-8660-9ec722d72ba2', irc_469_material_participation_kernel__strict_gatekeeper_reading, coexists_with).
narrative_ontology:cs_axiom('f1677aea-558b-48ff-8660-9ec722d72ba2', foundational, self_reported_hours_are_sufficient_evidentiary_basis).
narrative_ontology:cs_axiom_status(self_reported_hours_are_sufficient_evidentiary_basis, holdable).
narrative_ontology:cs_axiom_grounding('f1677aea-558b-48ff-8660-9ec722d72ba2', self_reported_hours_are_sufficient_evidentiary_basis, conventional).
narrative_ontology:cs_axiom('f1677aea-558b-48ff-8660-9ec722d72ba2', secondary, grouping_election_aggregation_reflects_genuine_unified_activity).
narrative_ontology:cs_axiom_status(grouping_election_aggregation_reflects_genuine_unified_activity, holdable).
narrative_ontology:cs_axiom_grounding('f1677aea-558b-48ff-8660-9ec722d72ba2', grouping_election_aggregation_reflects_genuine_unified_activity, instrumental).
narrative_ontology:cs_reference_frame('f1677aea-558b-48ff-8660-9ec722d72ba2', anti_shelter_congressional_intent_1986).
narrative_ontology:cs_drift_state('f1677aea-558b-48ff-8660-9ec722d72ba2', contemporary_advisory_industry_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f1677aea-558b-48ff-8660-9ec722d72ba2', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_real_estate_professionals).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, passive_loss_shelter_promoters).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisory_firms).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, general_tax_base).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, wage_earning_taxpayers).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, irs_examination_function).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strategic_shelter_reading, material_participation_is_objectively_measurable_by_hours).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Structure ownership entities, log hours against the seven material-participation tests (especially the 100-hour and facts-and-circumstances tests), and make grouping elections across multiple properties to aggregate hours and convert what would otherwise be passive rental losses into deductible losses against active income. Retain counsel to build a contemporaneous log after the fact if audited.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, high_income_real_estate_professionals, beneficiary,
    powerful, biographical, arbitrage, national).

% Design and sell structures — grouping elections, hour-tracking templates, real estate professional status packages — that push clients toward the threshold without crossing into activities that would draw scrutiny. Profit from the existence of a permissive, contestable threshold; would lose the product line if the standard hardened into a bright-line, high-documentation bar.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, passive_loss_shelter_promoters, agenda_setter,
    organized, biographical, mobile, national).

% Bill for engagement letters, hour logs, and grouping-election memoranda. Their revenue depends on the standard staying interpretively soft enough to require ongoing advisory support but not so soft it collapses under IRS challenge; they actively lobby against bright-line reform.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisory_firms, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisory_firms, agenda_setter).

% Bears the revenue shortfall created by systematic passive-loss deductions claimed under a permissive reading of material participation. Has no seat in how the threshold is administered and no practical way to contest an aggregate revenue effect distributed across millions of filers.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, general_tax_base, payer,
    powerless, generational, trapped, national).

% File W-2 income with no comparable aggressive-hour-counting avenue; the passive activity loss rules apply to them at face value while real estate investors with access to advisory structuring route around the same rules. Experience the asymmetry as an unlevel playing field but lack the capital or advisory access to replicate it.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, wage_earning_taxpayers, payer,
    powerless, biographical, trapped, national).

% Must audit self-reported hour logs and after-the-fact reconstructions against a facts-and-circumstances standard with no objective verification mechanism. Examination resources are chronically outmatched by the volume of grouping elections and the difficulty of disproving a taxpayer's own contemporaneous log; each successful shelter increases the burden on the next audit cycle.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, irs_examination_function, payer,
    institutional, generational, constrained, national).

% Hold rental property without the scale, time, or advisory budget to build a defensible hour-counting and grouping-election structure. Would benefit from a genuinely low-friction standard applied evenly, but in practice the permissive reading's benefits concentrate among taxpayers who can afford the structuring — their objection to the unevenness is never solicited in rulemaking or guidance.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, small_scale_passive_investors, excluded,
    moderate, biographical, constrained, national).

% Study aggregate revenue loss from passive activity loss shelters and publish analyses of how the material participation standard functions in practice versus on paper. Have no enforcement power but produce the empirical record the sibling gatekeeper reading cites.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_policy_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(irc_469_material_participation_kernel__strategic_shelter_reading, diffuse).
narrative_ontology:fixing_cost_class(irc_469_material_participation_kernel__strategic_shelter_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distinguishes active business involvement from passive capital deployment so that losses from genuinely operated businesses can offset active income while losses from mere capital placement cannot — a real distinction the tax code needs to draw somewhere.
% TRANSFER_FUNCTION: Moves tax liability from real-estate-investing taxpayers who can structure hours and elections toward the general tax base, which absorbs the resulting revenue shortfall through either higher rates elsewhere or reduced public spending.
% ABSENT_VOICES: Small-scale passive investors and wage-earning taxpayers who cannot access the structuring advisory market bear the comparative disadvantage of an uneven standard but have no forum — no notice-and-comment seat, no advisory access — in which that unevenness is raised.
% DISAPPEARANCE_RATIONALE: If the permissive reading were replaced overnight by a strict, high-documentation gatekeeper standard, a substantial share of currently-claimed passive loss deductions would be disallowed, the shelter-structuring advisory industry built around grouping elections and hour logs would contract sharply, and real estate investment structuring would shift toward vehicles that do not depend on material participation status.
% FOUNDING_PROBLEM: Congress enacted the passive activity loss rules in 1986 to stop taxpayers from using paper losses on passive investments (tax shelters) to offset unrelated active income, following widespread abuse of leveraged real estate and equipment-leasing shelters in the 1970s-80s.
% FOUNDING_PROBLEM_CORROBORATION: IRS revenue analyses and tax policy scholars outside the promoter/advisory industry attest that the permissive reading has substantially reopened the shelter dynamic the 1986 rules were built to close, citing concentration of claimed passive losses among high-income real-estate-professional filers. Shelter promoters and advisory firms attest the standard is functioning as intended — a workable, defensible line between active and passive involvement — but this attestation comes entirely from parties who profit from the standard remaining permissive.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strategic_shelter_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strategic_shelter_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strategic_shelter_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(irc_469_material_participation_kernel__strategic_shelter_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.61 and rising over the interval (0.38 to 0.61) to reflect a standard whose exploitability compounds as advisory structuring matures and normalizes across the investing population. Suppression is authored low (0.28) and only mildly rising because this reading does not depend on coercion or blocked exits — no one is forced to structure toward the threshold, and the mechanism is voluntary self-selection into a permissive interpretation, not enforced compliance. Theater ratio is authored moderately high and rising (0.30 to 0.58) because a growing share of the activity around the standard — hour logs, contemporaneous documentation built after the fact, grouping election memoranda — is compliance performance rather than the underlying active involvement the statute is meant to certify. Accessibility collapse is authored low-moderate (0.35): alternatives to the permissive reading (accepting passive treatment, or genuinely restructuring involvement) remain visible and available, they are simply less advantageous. Resistance is authored low-moderate (0.32): the IRS's facts-and-circumstances test gives some pushback capacity, but it is structurally outmatched, so realized resistance stays modest despite institutional standing.
 *
 * DIRECTIONALITY LOGIC:
 *   High-income real estate professionals and the advisory/promoter ecosystem sit near the beneficiary end of directionality: they collect the deduction or the fee, and their exit options (arbitrage, mobile) let them abandon any specific structuring approach if scrutiny rises without bearing the underlying cost. The general tax base and wage-earning taxpayers sit near the target end: they are powerless, trapped (no comparable avoidance avenue), and bear the transfer through general revenue effects. The IRS examination function is a distinctive institutional payer — powerful in principle but structurally constrained by the standard's unverifiability, so its directionality sits closer to target than its nominal institutional power would suggest without an override; no override was applied here because the derivation from victim/payer role plus constrained exit already captures this without contradiction.
 *
 * MANDATROPHY ANALYSIS:
 *   The 1986 founding problem — stopping abusive tax-shelter losses from offsetting active income — is authored as contested rather than flatly dead: the underlying distinction between active and passive involvement remains conceptually live (some taxpayers ARE materially, substantially involved), but this reading holds that the operative standard's permissiveness has let the shelter dynamic re-enter through a side door the statute meant to close. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (a real active/passive line does something) while still naming the asymmetric extraction (general tax base pays, structuring-capable investors and their advisors collect) that requires active IRS enforcement effort to even partially contain — collapsing this into pure snare would erase the fact that some material participation claims are genuine; collapsing it into rope would erase the documented systematic exploitation the founding-problem corroboration describes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hour_log_verifiability_ambiguity,
    'Are self-reported, often contemporaneously-reconstructed hour logs a genuinely permissive feature of the regulatory design, or evidence that IRS enforcement capacity — not the statute''s text — is what makes the standard exploitable?',
    'Compare audit outcomes and disallowance rates in jurisdictions or periods with enhanced IRS passive-activity examination funding against baseline periods; a large swing would indicate the permissiveness is an enforcement-capacity artifact rather than an inherent feature of the threshold.',
    'If enforcement-capacity-driven, the correct remediation is examination resourcing, not reinterpretation of the standard — which would narrow the gap between this reading and the strict_gatekeeper_reading without requiring either to be wrong about the text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hour_log_verifiability_ambiguity, empirical, 'Whether permissiveness is a textual feature or an enforcement-capacity artifact.').

omega_variable(
    reading_selection_evidentiary_basis,
    'Given that the same statutory text supports both this permissive reading and the sibling strict_gatekeeper_reading, what observable evidence should determine which reading better describes the standard''s actual operation across the filing population?',
    'Aggregate IRS statistics of pass-through entity passive loss claims by income decile and by whether real-estate-professional status was claimed, compared against Tax Court and audit adjustment rates for those claims.',
    'A finding that claims concentrate heavily among high-income filers with structuring access, with low downstream audit adjustment, supports this reading as the operative description; a finding of even distribution and high adjustment rates would support the sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_evidentiary_basis, conceptual, 'Which reading the statute''s actual operation, as opposed to its text, corroborates.').

omega_variable(
    grouping_election_permanence,
    'Once a taxpayer makes a grouping election, is the resulting aggregation treated as a durable structural feature of their tax position, or a revocable choice subject to IRS challenge on a facts-and-circumstances basis each year?',
    'Review of IRS guidance and Tax Court precedent on grouping election revocation and consistency requirements across tax years.',
    'High durability strengthens the shelter reading (structuring, once achieved, is stable and low-maintenance); low durability would raise the effective compliance friction this reading claims is low.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(grouping_election_permanence, empirical, 'Whether grouping elections are durable or subject to ongoing challenge.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strategic_shelter_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t0, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(irc__tr_t6, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 6, 0.36).
narrative_ontology:measurement(irc__tr_t12, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement(irc__tr_t18, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 18, 0.46).
narrative_ontology:measurement(irc__tr_t24, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 24, 0.51).
narrative_ontology:measurement(irc__tr_t30, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 30, 0.55).
narrative_ontology:measurement(irc__tr_t36, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 36, 0.58).

% Extraction over time
narrative_ontology:measurement(irc__be_t0, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(irc__be_t6, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(irc__be_t12, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(irc__be_t18, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 18, 0.53).
narrative_ontology:measurement(irc__be_t24, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(irc__be_t30, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(irc__be_t36, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 36, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t0, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(irc__su_t6, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 6, 0.2).
narrative_ontology:measurement(irc__su_t12, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 12, 0.22).
narrative_ontology:measurement(irc__su_t18, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 18, 0.24).
narrative_ontology:measurement(irc__su_t24, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 24, 0.26).
narrative_ontology:measurement(irc__su_t30, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 30, 0.27).
narrative_ontology:measurement(irc__su_t36, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 36, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strategic_shelter_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(irc_469_material_participation_kernel__strategic_shelter_reading, 0.1).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel__strict_gatekeeper_reading).

% DUAL FORMULATION NOTE:
% This constraint and irc_469_material_participation_kernel__strict_gatekeeper_reading are two readings of the same kernel (the material participation threshold under IRC §469). They share the statutory text but diverge on ε, beneficiary/victim structure, and classification: this reading (strategic_shelter_reading) authors substantial systematic extraction via permissive threshold-clearing (tangled_rope), while the sibling (strict_gatekeeper_reading) is expected to author low extraction consistent with a narrowly-gatekept, well-verified standard (plausibly rope or mountain-adjacent). The two are linked here rather than merged because forcing one ε to cover both readings would violate the ε-invariance principle — changing which reading is assumed changes the measured extraction, which means they are structurally different constraints sharing one legal text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
