% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strict_gatekeeper_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__strict_gatekeeper_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: irc_469_material_participation_kernel__strict_gatekeeper_reading
 *   human_readable: IRC §469 Material Participation Test — Strict Gatekeeper Reading
 *   domain: tax_law/real_estate_investment/regulatory_interpretation
 *
 * SUMMARY:
 *   This story instantiates the strict gatekeeper reading of the IRC §469
 *   material participation kernel: the position, corroborated by decades of
 *   Tax Court precedent, that qualifying as a materially-participating real
 *   estate professional requires verifiable, substantial personal labor
 *   supported by contemporaneous documentation — not merely hours that could
 *   plausibly have been worked. Under this reading the qualifying population
 *   is narrow, compliance friction is high, and passive losses are rarely
 *   deductible against ordinary income for investors who cannot produce
 *   audit-grade evidence, regardless of whether their actual involvement was
 *   genuine. This is one of two structurally distinct constraints sharing the
 *   §469 label; the sibling constraint (strategic_shelter_reading, not
 *   authored here) treats the same statutory text as a permissive threshold
 *   achievable through aggressive hour-counting and defensive grouping
 *   elections. The two readings diverge sharply on ε, qualifying population,
 *   and compliance burden, and are linked in network.affects_constraints
 *   rather than merged into one story.
 *
 * KEY AGENTS:
 *   - irs_enforcement_division: agenda-setter (institutional/analytical) — administers and enforces the strict evidentiary standard, collecting revenue on recharacterized passive losses
 *   - high_compliance_capacity_investors: beneficiary (powerful/arbitrage) — resourced to satisfy the documentation bar and uses its narrowness as competitive moat
 *   - tax_litigation_bar: beneficiary (organized/arbitrage) — collects recurring fees defending and contesting participation claims
 *   - working_landlords_with_day_jobs: victim (moderate/constrained) — performs real labor but lacks contemporaneous records to prove it
 *   - small_scale_real_estate_investors: victim (moderate/constrained) — cannot economically justify audit-proofing modest deductions
 *   - physician_and_professional_investors: victim (moderate/trapped) — genuine involvement routinely disbelieved given competing career demands
 *   - us_tax_court: observer (institutional/analytical) — operationalizes the strict standard through accumulated case law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.58).
domain_priors:suppression_score(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.62).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strict_gatekeeper_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strict_gatekeeper_reading, "IRC §469 Material Participation Test — Strict Gatekeeper Reading").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strict_gatekeeper_reading, "tax_law/real_estate_investment/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strict_gatekeeper_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'c0590577-faa8-432d-a234-4b0e3eb3c8f9').
narrative_ontology:cs_kernel_codification('c0590577-faa8-432d-a234-4b0e3eb3c8f9', fixed_text).
narrative_ontology:cs_authority_grounding('c0590577-faa8-432d-a234-4b0e3eb3c8f9', extraction).
narrative_ontology:cs_interpretation_layer_present('c0590577-faa8-432d-a234-4b0e3eb3c8f9').
narrative_ontology:cs_reading_relation('c0590577-faa8-432d-a234-4b0e3eb3c8f9', irc_469_material_participation_kernel__strategic_shelter_reading, coexists_with).
narrative_ontology:cs_axiom('c0590577-faa8-432d-a234-4b0e3eb3c8f9', foundational, participation_must_be_contemporaneously_evidenced).
narrative_ontology:cs_axiom_status(participation_must_be_contemporaneously_evidenced, holdable).
narrative_ontology:cs_axiom_grounding('c0590577-faa8-432d-a234-4b0e3eb3c8f9', participation_must_be_contemporaneously_evidenced, instrumental).
narrative_ontology:cs_axiom('c0590577-faa8-432d-a234-4b0e3eb3c8f9', secondary, documentation_burden_properly_allocated_to_claimant).
narrative_ontology:cs_axiom_status(documentation_burden_properly_allocated_to_claimant, holdable).
narrative_ontology:cs_axiom_grounding('c0590577-faa8-432d-a234-4b0e3eb3c8f9', documentation_burden_properly_allocated_to_claimant, conventional).
narrative_ontology:cs_reference_frame('c0590577-faa8-432d-a234-4b0e3eb3c8f9', anti_shelter_evidentiary_rigor).
narrative_ontology:cs_drift_state('c0590577-faa8-432d-a234-4b0e3eb3c8f9', post_tax_court_precedent_accumulation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c0590577-faa8-432d-a234-4b0e3eb3c8f9', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs_enforcement_division).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, high_compliance_capacity_investors).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_litigation_bar).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, working_landlords_with_day_jobs).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_scale_real_estate_investors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, physician_and_professional_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and audits material participation claims under Treas. Reg. 1.469-5T, demanding contemporaneous logs, calendars, and corroborating third-party evidence. Sets the evidentiary bar high enough that self-serving post-hoc reconstructions of hours are routinely disallowed in Tax Court. Collects additional tax revenue and penalty assessments whenever claimed passive-loss deductions are recharacterized as passive under this strict reading.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs_enforcement_division, agenda_setter,
    institutional, generational, analytical, national).

% Wealthy investors and real-estate professionals who can afford dedicated staff, contemporaneous time-tracking software, and specialized tax counsel to satisfy the strict documentation standard. They pass the gate reliably and use its narrowness as a competitive moat — their properly-documented material participation claims survive audit while under-resourced competitors' claims do not.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, high_compliance_capacity_investors, beneficiary,
    powerful, biographical, arbitrage, national).

% Tax attorneys and CPAs who charge substantial fees to build audit-ready participation logs, structure grouping elections defensively, and litigate contested hour-counts in Tax Court. The high documentation bar and its frequent contestation are the source of a recurring engagement stream; a looser standard would shrink this practice area.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_litigation_bar, beneficiary,
    organized, biographical, arbitrage, national).

% Own one or a few rental properties alongside full-time W-2 employment. They perform genuine management work — screening tenants, coordinating repairs, handling finances — but rarely keep contemporaneous logs because they are not professional record-keepers. Under this reading, their passive losses are disallowed against ordinary income even when their actual labor was substantial, because their documentation does not meet the evidentiary bar.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, working_landlords_with_day_jobs, payer,
    moderate, biographical, constrained, regional).

% Operate a handful of properties without the administrative infrastructure of larger operations. They cannot economically justify hiring counsel to build audit-proof time records for what may be a modest deduction, so they either forgo claiming material participation, accept passive treatment, or risk an audit loss on a claim they cannot adequately document.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_scale_real_estate_investors, payer,
    moderate, biographical, constrained, regional).

% High-income professionals who invest in short-term rentals or syndications seeking to offset ordinary income with real estate losses. The strict reading requires them to demonstrate genuine, verifiable substantial involvement despite demanding primary careers, and courts frequently reject their hour logs as reconstructed or implausible given their other full-time obligations, disallowing the intended tax benefit entirely.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, physician_and_professional_investors, payer,
    moderate, biographical, trapped, national).

% Adjudicates disputed material participation claims, repeatedly finding that self-serving, non-contemporaneous logs are insufficient evidence. Its accumulated case law is what operationalizes the strict gatekeeper reading in practice, distinguishing it from the more permissive reading some practitioners advocate.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, us_tax_court, observer,
    institutional, generational, analytical, national).

% The 1986 Congress that enacted §469 intended to curb abusive tax shelters where passive investors with no real involvement claimed paper losses. They are not present in current interpretive disputes, but the strict reading claims fidelity to this original anti-shelter purpose, while the permissive reading argues compliance-only formalism has drifted from that intent.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, passive_activity_loss_congress_drafters, excluded,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(irc_469_material_participation_kernel__strict_gatekeeper_reading, passive_activity_loss_congress_drafters).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(irc_469_material_participation_kernel__strict_gatekeeper_reading, diffuse).
narrative_ontology:fixing_cost_class(irc_469_material_participation_kernel__strict_gatekeeper_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distinguishes investors who genuinely operate a real estate trade or business (entitled to offset ordinary income with losses) from passive capital providers (whose losses should only offset passive income), preventing the tax shelter abuses §469 was enacted to stop.
% TRANSFER_FUNCTION: Moves tax benefit eligibility from investors who cannot produce audit-grade contemporaneous documentation to those with the resources to build and defend it — regardless of whether their underlying personal labor was actually comparable — and moves compliance fees from investor pockets to tax counsel and accounting practices.
% ABSENT_VOICES: Small landlords and dual-career professional investors who lose deductions on real, substantial labor because they lack contemporaneous logs are not represented in the case law or in IRS guidance drafting; their objection — that the documentation bar measures record-keeping capacity rather than actual participation — rarely surfaces outside individual, expensive Tax Court appeals.
% DISAPPEARANCE_RATIONALE: If the strict gatekeeper reading were replaced by the permissive reading overnight, a large population of currently-disallowed passive losses would become deductible, the tax litigation bar's defensive documentation practice would shrink substantially, and IRS audit yield from participation recharacterization would collapse — the qualifying population would expand sharply.
% FOUNDING_PROBLEM: Congress enacted the passive activity loss rules in 1986 specifically to end widespread use of paper tax shelters where wealthy, uninvolved investors deducted real estate losses against salary income with no genuine operational involvement.
% FOUNDING_PROBLEM_CORROBORATION: IRS enforcement personnel and the Tax Court's published opinions attest the underlying shelter-abuse problem persists and justifies a demanding evidentiary standard. Small-investor advocacy groups and several tax academics, outside the IRS and litigation bar's direct interest, attest that the strict reading has drifted from targeting abusive shelters toward penalizing legitimate but under-documented labor, disproportionately burdening investors without administrative infrastructure rather than the sophisticated shelter promoters the statute targeted.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strict_gatekeeper_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strict_gatekeeper_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that a meaningful share of disallowed deductions correspond to investors with genuine, substantial involvement whose labor simply wasn't logged in the form the standard demands — the gate filters on documentation capacity as much as on actual participation. Suppression (0.62) captures the structural bar: alternatives to producing contemporaneous, third-party-corroborated records are essentially foreclosed once an audit begins: retroactive reconstruction is disfavored and frequently rejected outright. Theater ratio (0.40) is non-trivial because a portion of the compliance apparatus — elaborate calendars, activity logs assembled defensively rather than contemporaneously in real time, boilerplate grouping election memoranda — exists primarily to survive audit scrutiny rather than to reflect or improve actual participation. Accessibility collapse (0.66) is moderately high: once an investor understands that self-serving reconstruction fails, viable alternatives (hiring documentation services, restructuring ownership, or abandoning the claim) narrow considerably. Resistance (0.55) is substantial and organized — professional associations of small landlords and real estate investor coalitions have lobbied for safe-harbor reforms, and individual Tax Court appeals represent ongoing friction against the standard.
 *
 * PERSPECTIVAL GAP:
 *   From the IRS enforcement division's seat, the strict standard is a legitimate anti-shelter safeguard functioning as intended — a tangled rope with a real coordination function (preventing abusive paper losses) that happens to require active enforcement. From the working landlord's seat, the same structure computes closer to extractive: substantial real labor is being disallowed not because it wasn't performed but because it wasn't logged in the specific form the gate demands, which the engine should register as asymmetric burden falling on a population with genuine involvement but low administrative capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   The IRS enforcement division and the Tax Court sit as agenda-setter/observer: they administer and interpret the standard without being extracted from by it. High-compliance-capacity investors and the tax litigation bar are structural beneficiaries — the narrow, document-heavy gate is precisely what their resources let them clear reliably while less-resourced competitors cannot, giving them a directionality near the beneficiary end (d low). Working landlords, small-scale investors, and professional investors are targets: their directionality sits near the full-target end because the constraint extracts tax benefit from them in proportion to their inability to produce audit-grade documentation, independent of their actual labor. Their exit options (constrained or trapped) reinforce this — abandoning a real estate investment to escape passive-loss treatment is not a live option for most.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — paper tax shelters with no genuine investor involvement — is largely addressed by the existence of any participation requirement at all; the contested question is whether the STRICT documentation-heavy operationalization of that requirement remains proportionate to the residual shelter-abuse risk, or whether it has hardened into a compliance-cost gate that primarily screens on record-keeping capacity. Classifying this as tangled_rope (rather than snare) preserves the genuine coordination function — distinguishing operators from passive shelter investors is a real problem — while still registering the asymmetric extraction falling on under-resourced but genuinely participating investors, avoiding both the error of dismissing it as pure extraction and the error of treating it as costless anti-abuse machinery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    documentation_bar_vs_actual_participation,
    'Does the contemporaneous-documentation requirement track genuine substantial participation, or does it primarily measure an investor''s administrative capacity and access to professional record-keeping resources?',
    'Comparative study of Tax Court outcomes: cross-reference disallowed claims against independently verifiable evidence of actual time spent (e.g., contractor records, tenant communications, property management system logs) to determine what fraction of disallowed claimants likely did perform substantial work.',
    'If documentation capacity is doing most of the work rather than actual participation, the strict reading''s extraction is substantially a wealth/resource-sorting mechanism riding on a genuine anti-shelter coordination function — supporting the tangled_rope classification over a pure-coordination rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_bar_vs_actual_participation, empirical, 'Whether the strict documentation standard measures participation or administrative capacity.').

omega_variable(
    kernel_reading_divergence_location,
    'The IRC 469 material participation kernel is read by different parties as either a strict evidentiary gatekeeper or a permissive, electively-satisfiable threshold. Where exactly does this disagreement live — in the statutory text, in Treasury Regulation 1.469-5T''s seven tests, or in Tax Court''s evidentiary standards for proving those tests were met?',
    'Textual and case-law analysis isolating whether divergence originates at the level of which hours count (regulatory test design) or at the level of what evidence suffices to prove hours were worked (evidentiary/procedural standard) — these are structurally distinct loci and a sibling reading might diverge on one axis while agreeing on the other.',
    'If the divergence is purely evidentiary (both readings agree on what counts, disagree on how it must be proven), the two readings are closer structural cousins than if they diverge on substantive test design; this affects how tightly the two constraint stories should be coupled in the network.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence_location, conceptual, 'Locating where the strict and permissive readings of the material participation kernel actually diverge.').

omega_variable(
    congressional_intent_fidelity,
    'Does the strict gatekeeper reading''s demanding evidentiary bar remain faithful to the 1986 Congress''s anti-shelter purpose, or has it drifted into a distinct, more restrictive standard than originally intended?',
    'Legislative history review (committee reports, floor statements) compared against the accumulated body of Tax Court decisions to assess whether the evidentiary demands imposed by case law exceed what the statutory text and original regulations contemplated.',
    'If the standard has drifted beyond original intent, the strict reading''s claimed anti-abuse coordination function is weaker than authored, and its classification should weight more heavily toward extraction; if faithful, the coordination function is more robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(congressional_intent_fidelity, conceptual, 'Whether the strict reading is faithful to or has drifted from 1986 legislative intent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0, 38).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(irc__tr_t8, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(irc__tr_t16, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(irc__tr_t24, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(irc__tr_t31, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 31, 0.38).
narrative_ontology:measurement(irc__tr_t38, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 38, 0.4).

% Extraction over time
narrative_ontology:measurement(irc__be_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(irc__be_t8, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(irc__be_t16, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(irc__be_t24, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(irc__be_t31, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 31, 0.57).
narrative_ontology:measurement(irc__be_t38, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 38, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(irc__su_t8, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(irc__su_t16, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(irc__su_t24, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(irc__su_t31, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 31, 0.6).
narrative_ontology:measurement(irc__su_t38, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 38, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strict_gatekeeper_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel__strategic_shelter_reading).

% DUAL FORMULATION NOTE:
% This story and irc_469_material_participation_kernel__strategic_shelter_reading are two readings of one contested kernel: the §469 material participation standard. This story (strict_gatekeeper_reading) authors a narrower qualifying population, higher compliance friction, and moderate-high extraction (0.58) concentrated on genuinely-participating but under-documented investors. The sibling story authors a wider qualifying population reachable through aggressive hour-counting and grouping elections, with a correspondingly different beneficiary/victim structure and likely different extraction profile. Per the ε-invariance principle, these are not the same constraint measured two ways — they are two constraints sharing a statutory kernel, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
