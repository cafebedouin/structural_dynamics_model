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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: irc_469_material_participation_kernel__strict_gatekeeper_reading
 *   human_readable: IRC §469 Material Participation Test — Strict Gatekeeper Reading
 *   domain: tax_law/real_estate_investment/regulatory_interpretation
 *
 * SUMMARY:
 *   The material participation test under IRC §469 and Treas. Reg. 1.469-5T
 *   requires taxpayers claiming active (non-passive) treatment of real estate
 *   losses to satisfy one of seven tests, most commonly 500+ hours of
 *   participation, substantiated by contemporaneous records. This story
 *   instantiates the STRICT GATEKEEPER reading: the position, increasingly
 *   favored in Tax Court rulings and IRS audit practice, that self-serving
 *   after-the-fact log reconstruction is inadequate and that only granular,
 *   contemporaneous, corroborated documentation satisfies the standard. Under
 *   this reading the qualifying population narrows sharply and passive losses
 *   become rarely deductible against ordinary income for anyone without
 *   disciplined real-time record-keeping — regardless of how much genuine
 *   labor they performed. The sibling reading (strategic_shelter_reading)
 *   treats the same statutory text as a permissive threshold achievable
 *   through aggressive hour-counting and grouping elections; that is a
 *   different constraint with a different ε, authored separately and linked
 *   via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - irs_enforcement_apparatus: agenda_setter (institutional/analytical) — administers and polices the documentation bar
 *   - high_engagement_operator_investors: beneficiary (moderate/mobile) — genuine operators whose status is validated by exclusion of casual claimants
 *   - tax_controversy_professionals: beneficiary (organized/arbitrage) — paid to build and defend documentation packages
 *   - passive_real_estate_investors: payer (powerless/constrained) — real labor, no contemporaneous logs, losses suspended
 *   - part_time_landlord_professionals: payer (moderate/constrained) — substantial aggregate hours but fails the bright-line threshold
 *   - dual_career_property_owners: payer/excluded (powerless/trapped) — informal, unlogged labor invisible to the regime
 *   - tax_court_and_treasury: observer (institutional/analytical) — adjudicates and could revise the standard
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
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strict_gatekeeper_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strict_gatekeeper_reading, "IRC §469 Material Participation Test — Strict Gatekeeper Reading").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strict_gatekeeper_reading, "tax_law/real_estate_investment/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strict_gatekeeper_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'f92a7ffe-dca1-498e-bdeb-6c5390da814f').
narrative_ontology:cs_kernel_codification('f92a7ffe-dca1-498e-bdeb-6c5390da814f', formalized).
narrative_ontology:cs_authority_grounding('f92a7ffe-dca1-498e-bdeb-6c5390da814f', extraction).
narrative_ontology:cs_interpretation_layer_present('f92a7ffe-dca1-498e-bdeb-6c5390da814f').
narrative_ontology:cs_reading_relation('f92a7ffe-dca1-498e-bdeb-6c5390da814f', irc_469_material_participation_kernel__strategic_shelter_reading, coexists_with).
narrative_ontology:cs_axiom('f92a7ffe-dca1-498e-bdeb-6c5390da814f', foundational, documentation_verifiability_is_the_operative_standard).
narrative_ontology:cs_axiom_status(documentation_verifiability_is_the_operative_standard, holdable).
narrative_ontology:cs_axiom_grounding('f92a7ffe-dca1-498e-bdeb-6c5390da814f', documentation_verifiability_is_the_operative_standard, conventional).
narrative_ontology:cs_axiom('f92a7ffe-dca1-498e-bdeb-6c5390da814f', secondary, unrecorded_labor_cannot_satisfy_material_participation).
narrative_ontology:cs_axiom_status(unrecorded_labor_cannot_satisfy_material_participation, holdable).
narrative_ontology:cs_axiom_grounding('f92a7ffe-dca1-498e-bdeb-6c5390da814f', unrecorded_labor_cannot_satisfy_material_participation, empirically_contingent).
narrative_ontology:cs_reference_frame('f92a7ffe-dca1-498e-bdeb-6c5390da814f', anti_shelter_bright_line_standard).
narrative_ontology:cs_drift_state('f92a7ffe-dca1-498e-bdeb-6c5390da814f', post_2010_audit_hardening_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f92a7ffe-dca1-498e-bdeb-6c5390da814f', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs_enforcement_apparatus).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, high_engagement_operator_investors).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_controversy_professionals).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, passive_real_estate_investors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, part_time_landlord_professionals).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, dual_career_property_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the seven-test material participation regulations under Treas. Reg. 1.469-5T, audits contemporaneous log evidence, and disallows passive loss deductions where documentation fails to meet the substantiation bar. Collects nothing directly but sets and polices the strictness of the reading; every hour-count dispute and Tax Court case that narrows what counts as verifiable participation strengthens this seat's enforcement leverage.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs_enforcement_apparatus, agenda_setter,
    institutional, civilizational, analytical, national).

% Real estate professionals and full-time operators who genuinely spend 500+ hours a year and can produce credible contemporaneous records. The strict reading validates their status and excludes casual competitors from claiming the same tax treatment, effectively raising the value of their qualifying status by narrowing who else can claim it.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, high_engagement_operator_investors, beneficiary,
    moderate, biographical, mobile, national).

% CPAs, tax attorneys, and audit-defense specialists who are paid to build and defend contemporaneous log packages, argue grouping elections, and litigate close-call hour disputes. The higher and more ambiguous the documentation bar, the more billable work exists; a permissive reading would shrink this practice area substantially.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_controversy_professionals, beneficiary,
    organized, biographical, arbitrage, national).

% Investors who hold rental property alongside a full-time unrelated job. They perform real work — screening tenants, coordinating repairs, handling books — but rarely keep contemporaneous logs because the work doesn't feel like something requiring documentation until an audit demands it retroactively. Under the strict reading their losses are suspended as passive, unusable against W-2 or business income regardless of the substantive labor performed.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, passive_real_estate_investors, payer,
    powerless, biographical, constrained, national).

% Own and self-manage a handful of properties while holding another primary occupation. They may clear substantial hours in aggregate but cannot satisfy the 750-hour real-property-trade-or-business threshold or produce logs granular enough to survive audit scrutiny. Their exit is to hire third-party management (undermining the participation claim entirely) or accept suspended losses.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, part_time_landlord_professionals, payer,
    moderate, biographical, constrained, national).

% Married couples where one spouse handles property management informally alongside caregiving or other unpaid labor that doesn't get logged in hours the way the regulation demands. Their actual participation is real but structurally invisible to the documentation regime, and they have no seat in how the seven tests get interpreted at audit.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, dual_career_property_owners, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strict_gatekeeper_reading, dual_career_property_owners, excluded).

% Adjudicate contested cases and periodically consider regulatory revision. Their rulings on what counts as credible contemporaneous evidence (calendar entries, after-the-fact reconstructions, narrative summaries) directly determine how strict the gatekeeper reading becomes in practice.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_court_and_treasury, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distinguishes investors who are genuinely running a trade or business from those who are passive capital providers, so that passive activity loss limitation rules under §469 apply to the correct population and ordinary-income shelter is reserved for active operators.
% TRANSFER_FUNCTION: Moves the benefit of loss deductibility from investors who cannot produce audit-grade documentation of substantial personal labor to investors and professionals who can meet or defend the documentation bar, and to the professionals paid to construct that defense.
% ABSENT_VOICES: Investors who perform real but informally-tracked labor (especially unpaid or caregiving-adjacent household participation) have no representation in how the seven tests get interpreted; their objection — that the documentation requirement measures record-keeping discipline rather than actual participation — surfaces only in individual Tax Court petitions, not in rulemaking.
% DISAPPEARANCE_RATIONALE: If the strict documentation bar vanished and material participation reverted to a good-faith substantial-involvement standard, a large population of part-time and dual-career property owners would immediately become eligible to deduct passive losses against ordinary income, materially shrinking taxable income for millions of filers and collapsing a substantial share of the tax controversy practice built around defending or attacking hour logs.
% FOUNDING_PROBLEM: Congress enacted §469 in 1986 to stop wealthy taxpayers from using tax-shelter partnerships and real estate syndications — where investors did no real work — to generate paper losses that offset unrelated wage and business income.
% FOUNDING_PROBLEM_CORROBORATION: IRS enforcement personnel and tax policy scholars outside the benefiting professional class attest the original shelter-abuse problem was real and substantially solved by the passive activity rules generally; independent Tax Court commentary and taxpayer advocate reports attest that the strict documentation reading now primarily screens out genuine small-scale operators with poor record-keeping rather than shelter abusers, who have largely adapted their paperwork to survive audit regardless of actual participation.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strict_gatekeeper_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strict_gatekeeper_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction is moderate-high (0.58 at interval end) because the strict reading does not deny a real coordination function — distinguishing shelter abuse from genuine operation was and remains a legitimate problem — but the documentation bar it has accreted extracts a widening tax cost from taxpayers whose participation is substantively real but administratively unprovable. Suppression (0.62) reflects the active enforcement machinery: audit selection criteria, contemporaneous-log requirements, and Tax Court precedent progressively foreclosing informal or reconstructed evidence. Theater ratio (0.40) captures that a meaningful share of compliance activity — retroactive calendar reconstruction, narrative memoranda produced for audit defense rather than real-time tracking — is performative documentation manufactured to satisfy the test rather than a byproduct of the underlying work itself. Accessibility collapse is moderate (0.50): informal record-keeping alternatives (calendars, texts, contractor invoices) persist but are treated as second-tier evidence, collapsing but not eliminating the alternative-documentation path. Resistance is high (0.70): this reading is contested constantly in Tax Court petitions and comment letters, unlike a genuine mountain which would meet negligible resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   The IRS enforcement apparatus and the professional class that services documentation disputes sit near the beneficiary end: the stricter the standard, the more enforcement leverage and billable defense work exists. High-engagement full-time operators also benefit — their genuine effort is validated precisely because the bar excludes casual competitors from the same tax treatment. Passive investors, part-time landlords, and especially dual-career owners with informally-tracked labor sit near the target end: their exit options are constrained or trapped (hiring a property manager destroys the participation claim; walking away forfeits the investment), so the extraction lands hardest on those least able to either document formally or exit the activity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — stopping paper-loss tax shelters with no real investor labor — was genuinely live in 1986 and remains partially live today. But the strict gatekeeper reading has drifted the mechanism from 'does this person do real work' toward 'can this person produce audit-proof paperwork,' which are increasingly different questions. That drift is why founding_problem_status is authored as contested rather than dead: the shelter-abuse problem persists in some segments (structured syndications), while for the ordinary dual-career landlord the documentation bar has become a proxy that fails to track the thing it was built to measure. This is exactly the tangled_rope signature — a genuine coordination function (sorting real operators from shelter abusers) co-existing with asymmetric extraction (informally-participating owners losing deductibility regardless of actual labor) sustained by active IRS/Tax Court enforcement of an increasingly narrow evidentiary standard.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_divergence_documentation_standard,
    'Is IRC §469''s material participation test, as a kernel, better read as a strict evidentiary gatekeeper (this story) or as a permissive threshold achievable through aggressive hour-counting and grouping elections (the sibling strategic_shelter_reading)?',
    'Track the trajectory of Tax Court rulings on what constitutes credible contemporaneous evidence — a continued hardening toward requiring real-time logs corroborates the strict reading; continued tolerance of reconstructed narrative summaries and aggressive grouping elections corroborates the permissive reading.',
    'Under this reading the qualifying population is narrow and compliance friction is high, with passive losses rarely deductible against ordinary income for taxpayers without disciplined documentation. Under the sibling reading the same statutory text permits a much larger qualifying population through favorable grouping elections and generous hour attribution, materially changing which taxpayers bear the cost of the passive activity rules.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence_documentation_standard, conceptual, 'Committer-frame ambiguity: which reading of the material participation kernel actually governs current enforcement practice.').

omega_variable(
    documentation_bar_natural_vs_constructed,
    'Is the contemporaneous-log requirement a natural consequence of needing verifiable evidence for any tax deduction claim, or a constructed, escalating standard that has drifted beyond what the 1986 anti-shelter purpose requires?',
    'Compare audit denial rates and Tax Court outcomes for taxpayers with credible-but-non-contemporaneous evidence (calendars, invoices, witness affidavits) across decades; a rising denial rate for substantively credible reconstructed evidence would indicate a constructed escalation rather than a natural evidentiary floor.',
    'If constructed and escalating, the strict reading functions increasingly as extraction dressed as verification; if natural and stable, the documentation bar is simply what any credible tax administration requires and the extraction reading overstates the case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_bar_natural_vs_constructed, empirical, 'Whether the strictness of the documentation standard reflects genuine verification needs or accreted extraction.').

omega_variable(
    informal_labor_visibility_gap,
    'Does the seven-test regulatory framework systematically fail to recognize household and caregiving-adjacent property management labor as material participation, and if so, is that a design gap or a deliberate exclusion?',
    'Empirical study of Tax Court petitions involving spousal or informal co-management arrangements, coded for whether denial turned on absence of real labor versus absence of documentation of real labor.',
    'If the gap is primarily documentation-based rather than labor-based, dual_career_property_owners are being extracted from for a record-keeping failure rather than a participation failure, strengthening the tangled_rope reading over a pure mountain or rope reading of the test.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(informal_labor_visibility_gap, empirical, 'Whether informally-tracked genuine labor is being denied recognition due to documentation form rather than participation substance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0, 38).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(irc__tr_t6, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(irc__tr_t12, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(irc__tr_t19, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 19, 0.33).
narrative_ontology:measurement(irc__tr_t26, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 26, 0.37).
narrative_ontology:measurement(irc__tr_t32, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(irc__tr_t38, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 38, 0.4).

% Extraction over time
narrative_ontology:measurement(irc__be_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(irc__be_t6, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(irc__be_t12, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(irc__be_t19, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 19, 0.51).
narrative_ontology:measurement(irc__be_t26, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 26, 0.55).
narrative_ontology:measurement(irc__be_t32, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(irc__be_t38, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 38, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(irc__su_t6, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 6, 0.46).
narrative_ontology:measurement(irc__su_t12, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(irc__su_t19, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 19, 0.56).
narrative_ontology:measurement(irc__su_t26, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 26, 0.59).
narrative_ontology:measurement(irc__su_t32, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 32, 0.61).
narrative_ontology:measurement(irc__su_t38, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 38, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strict_gatekeeper_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel__strategic_shelter_reading).

% DUAL FORMULATION NOTE:
% This story and irc_469_material_participation_kernel__strategic_shelter_reading are two readings of the same kernel (irc_469_material_participation_kernel): the statutory and regulatory text defining material participation. This reading (strict_gatekeeper) authors a narrower qualifying population, higher compliance friction, and a tangled_rope classification driven by genuine but increasingly proxy-decoupled documentation demands. The sibling reading treats the same text as a permissive, strategically-navigable threshold with a much larger effective qualifying population and different beneficiary/victim structure. Per the ε-invariance principle, these are authored as separate constraints with independent ε values rather than one story averaged across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
