% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strategic_shelter_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: IRC Â§469 Material Participation Permissive Threshold (Strategic Shelter Reading)
 *   domain: tax_law/regulatory_interpretation
 *
 * SUMMARY:
 *   IRC Â§469 and its attendant regulations establish 'material
 *   participation' tests that determine whether a taxpayer may deduct passive
 *   activity losses against active or portfolio income. This constraint
 *   instantiates the strategic_shelter_reading: the position that the
 *   statutory threshold is permissive, achievable through aggressive
 *   hour-counting across grouped activities, and functions in practice as a
 *   systematic enabler of passive loss deduction for wealthy, advised
 *   investors. The same statutory kernel supports a strict_gatekeeper_reading
 *   that would require verifiable, substantial personal labor. The
 *   claim/metric independence is maintained: the constraint is claimed as
 *   tangled_rope because it retains a genuine coordination function (loss
 *   limitation) while exhibiting substantial asymmetric extraction; the
 *   metrics describe a highly extractive, moderately theatrical regime
 *   without tuning the claim to match.
 *
 * KEY AGENTS:
 *   - high_net_worth_investors: Primary beneficiary (powerful/mobile) â capture passive loss deductions and wealth preservation
 *   - tax_advisory_industry: Secondary beneficiary (organized/mobile) â collects fees from structuring and defending participation strategies
 *   - general_taxpayers: Primary payer (powerless/trapped) â bear the shifted tax burden from successful shelters
 *   - honest_compliant_investors: Secondary payer (moderate/constrained) â pay higher relative tax rates by not utilizing aggressive strategies
 *   - irs: Agenda setter (institutional/analytical) â administers the rules and interprets the threshold
 *   - tax_reform_advocates: Excluded voice (moderate/constrained) â would tighten rules but are outside the administrative process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strategic_shelter_reading, 0.68).
domain_priors:suppression_score(irc_469_material_participation_kernel__strategic_shelter_reading, 0.55).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strategic_shelter_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strategic_shelter_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strategic_shelter_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strategic_shelter_reading, "IRC Â§469 Material Participation Permissive Threshold (Strategic Shelter Reading)").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strategic_shelter_reading, "tax_law/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strategic_shelter_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strategic_shelter_reading, '19ee8df7-1822-4826-8d29-c91e0865fee7').
narrative_ontology:cs_kernel_codification('19ee8df7-1822-4826-8d29-c91e0865fee7', formalized).
narrative_ontology:cs_authority_grounding('19ee8df7-1822-4826-8d29-c91e0865fee7', lineage).
narrative_ontology:cs_interpretation_layer_present('19ee8df7-1822-4826-8d29-c91e0865fee7').
narrative_ontology:cs_reading_relation('19ee8df7-1822-4826-8d29-c91e0865fee7', irc_469_material_participation_kernel__strict_gatekeeper_reading, coexists_with).
narrative_ontology:cs_axiom('19ee8df7-1822-4826-8d29-c91e0865fee7', foundational, formal_hours_and_elections_satisfy_materiality).
narrative_ontology:cs_axiom_status(formal_hours_and_elections_satisfy_materiality, holdable).
narrative_ontology:cs_axiom_grounding('19ee8df7-1822-4826-8d29-c91e0865fee7', formal_hours_and_elections_satisfy_materiality, conventional).
narrative_ontology:cs_axiom('19ee8df7-1822-4826-8d29-c91e0865fee7', secondary, passive_loss_sheltering_is_valid_planning).
narrative_ontology:cs_axiom_status(passive_loss_sheltering_is_valid_planning, holdable).
narrative_ontology:cs_axiom_grounding('19ee8df7-1822-4826-8d29-c91e0865fee7', passive_loss_sheltering_is_valid_planning, instrumental).
narrative_ontology:cs_reference_frame('19ee8df7-1822-4826-8d29-c91e0865fee7', elective_qualification_framework).
narrative_ontology:cs_drift_state('19ee8df7-1822-4826-8d29-c91e0865fee7', contemporary_tax_practice, gap(stable, minor, false)).
narrative_ontology:cs_created_at('19ee8df7-1822-4826-8d29-c91e0865fee7', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strategic_shelter_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, high_net_worth_investors).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisory_industry).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, general_taxpayers).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strategic_shelter_reading, honest_compliant_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own real estate and business interests structured to generate passive losses. Work with advisors to log hours across grouped activities and elect treatment that qualifies them as material participants, enabling deduction of losses against active and portfolio income. Can restructure holdings or shift to alternative tax-advantaged vehicles if rules change.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, high_net_worth_investors, beneficiary,
    powerful, biographical, mobile, national).

% Firms and individual practitioners who design, document, and defend material participation strategies for clients. They draft hour logs, advise on grouping elections, and represent clients in IRS examinations. Revenue depends on the complexity and perceived rigidity of the participation rules.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_advisory_industry, beneficiary,
    organized, biographical, mobile, national).

% Wage earners and passive investors who do not utilize aggressive loss strategies. They bear the aggregate tax burden shifted by those who successfully deduct passive losses against other income. Cannot opt out of the federal tax system or selectively benefit from the same structural elections without equivalent resources and advice.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, general_taxpayers, payer,
    powerless, generational, trapped, national).

% Real estate and business investors who report their activity conservatively, either because they lack advisory resources or because they interpret material participation as requiring genuine economic involvement. They pay higher effective tax rates relative to aggressive peers who utilize the same statutory framework.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, honest_compliant_investors, payer,
    moderate, biographical, constrained, national).

% Administers the material participation regulations through audit, guidance publication, and litigation. Reviews taxpayer hour logs and grouping elections, but faces resource constraints and statutory ambiguity in verifying the substantive economic reality behind claimed participation.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, irs, agenda_setter,
    institutional, generational, analytical, national).

% Academics, legislative staff, and public interest groups who argue that the hour-counting and grouping rules have been subverted into tax shelters. They publish studies and draft reform legislation but are structurally excluded from the regulatory interpretation and private letter ruling processes that shape day-to-day administration.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strategic_shelter_reading, tax_reform_advocates, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(irc_469_material_participation_kernel__strategic_shelter_reading, high_net_worth_investors).
narrative_ontology:fixing_cost_class(irc_469_material_participation_kernel__strategic_shelter_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distinguishes active participants (who may deduct losses against ordinary income) from passive investors (who may not), preventing unlimited sheltering of passive losses against active income and maintaining the integrity of the progressive tax base.
% TRANSFER_FUNCTION: Moves the burden of federal income tax from investors who qualify as material participants through aggressive hour-counting and grouping elections to the broader taxpayer base, while simultaneously transferring advisory fees from those investors to the tax planning industry.
% ABSENT_VOICES: Strict gatekeeper advocates, progressive tax policy groups, and some congressional committee staff argue that the hour-counting regime is a paper barrier and that grouping elections undermine the statute's anti-shelter purpose. They are structurally excluded from the IRS private letter ruling process, the tax court litigation that shapes precedent, and the Treasury regulatory drafting process where industry comments dominate.
% DISAPPEARANCE_RATIONALE: If the permissive material participation threshold and its aggressive interpretation vanished overnight, high-net-worth investors would lose a primary passive loss deduction channel, real estate syndication structures would reprice as after-tax returns shifted, the tax advisory industry would contract, and Treasury revenue would increase. The distribution of the federal income tax burden would shift materially toward those currently sheltering passive losses.
% FOUNDING_PROBLEM: The 1986 Tax Reform Act sought to eliminate widespread tax shelters in which passive investors used paper losses from limited partnerships and real estate activities to offset wages and active business income, draining federal revenue and violating horizontal equity.
% FOUNDING_PROBLEM_CORROBORATION: Congressional Budget Office and Joint Committee on Taxation revenue estimates from 1986 corroborate the original anti-shelter intent from outside the investor and advisor beneficiary set. The Treasury Inspector General for Tax Administration and Government Accountability Office have subsequently reported on passive loss compliance gaps, corroborating from an oversight seat that the current administration does not function as a strict gatekeeper.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strategic_shelter_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strategic_shelter_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strategic_shelter_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strategic_shelter_reading, 0.68, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.68) because the permissive hour-counting and grouping elections allow substantial passive loss deduction decoupled from genuine economic activity. Theater ratio is moderate (0.45) because the hour logs and election filings create a performative veneer of compliance that masks the underlying shelter function. Suppression is moderate (0.55): the constraint actively suppresses stricter alternatives through regulatory inertia, lobbying capture, and the complexity that makes legislative reform difficult. Accessibility collapse is moderate (0.50) because the formalistic compliance path is well-mapped for advised taxpayers while substantive economic standards have been institutionally marginalized. Resistance is moderate (0.40) because reform advocacy is persistent but diffuse and structurally excluded from the ruling process.
 *
 * PERSPECTIVAL GAP:
 *   The investor and advisor seats experience the constraint as a legitimate, administrable planning framework with clear rules and available elections. The general taxpayer and honest investor seats experience it as an asymmetric barrier that allows wealthy, advised taxpayers to reduce liability while they cannot. The IRS seat experiences it as an administrable but substantively ambiguous standard that it lacks resources to enforce rigorously. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   High_net_worth_investors and tax_advisory_industry are structural beneficiaries (low d) because the constraint subsidizes their tax positions and fee revenue. General_taxpayers and honest_compliant_investors are structural targets (high d) because they bear the relative cost of that subsidy through higher aggregate tax burden or reduced public services. The IRS sits near symmetric: it enforces the rule but does not personally capture the extraction, though its institutional authority and budget are bound up in administering the regime.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not a simple snare because IRC Â§469 genuinely addressed the 1980s tax shelter crisis by creating a categorization mechanism that limited passive loss deductions. The coordination function â distinguishing active from passive participation to protect the tax base â is real and historically necessary. However, the specific interpretive trajectory, the permissive regulatory guidance on hour counting, and the elective grouping mechanisms have layered substantial asymmetric extraction onto that coordination. Labeling it pure extraction would miss the genuine loss-limitation architecture that persists; labeling it pure coordination would miss the systematic sheltering and revenue shifting. Tangled rope is the structurally accurate classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legislative_intent_ambiguity,
    'Did Congress intend the grouping elections and hour tests to function as a permissive elective screen, or as a strict gatekeeper requiring genuine economic involvement?',
    'Analysis of 1986 TRA legislative history, conference reports, and subsequent floor statements; comparison with contemporaneous regulatory intent documents.',
    'If the legislative intent was strict gatekeeping, the current permissive administration represents severe practice drift and strengthens the extraction reading; if the intent was always a formalistic compromise, the strategic shelter reading is closer to the kernel''s designed function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_intent_ambiguity, conceptual, 'Whether the kernel''s origin was strict or permissive').

omega_variable(
    economic_substance_vs_formal_compliance,
    'Do the activities satisfying the material participation hour tests carry genuine economic substance, or are they predominantly formalistic compliance exercises?',
    'Empirical audit of a representative sample of taxpayer hour logs and grouping elections, matched against actual management decisions and capital deployment.',
    'If predominantly formalistic, the theater_ratio understates the performative nature of the constraint and the effective extraction is higher than measured; if substantive, the extraction is lower and the coordination function stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_substance_vs_formal_compliance, empirical, 'Whether participation is substantive or performative').

omega_variable(
    revenue_loss_quantification,
    'What is the measurable federal revenue loss attributable to passive loss deductions taken under the permissive material participation threshold?',
    'Treasury or Joint Committee on Taxation scoring of a counterfactual strict gatekeeper regime; IRS compliance study matched against actual deductions claimed.',
    'A quantified revenue loss would establish the concrete transfer magnitude from general taxpayers to sheltered investors; absence of measurable loss would weaken the extraction claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revenue_loss_quantification, empirical, 'Quantified revenue effect of the shelter reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strategic_shelter_reading, 0, 38).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t0, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(irc__tr_t6, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(irc__tr_t12, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(irc__tr_t18, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 18, 0.35).
narrative_ontology:measurement(irc__tr_t24, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(irc__tr_t30, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 30, 0.43).
narrative_ontology:measurement(irc__tr_t38, irc_469_material_participation_kernel__strategic_shelter_reading, theater_ratio, 38, 0.45).

% Extraction over time
narrative_ontology:measurement(irc__be_t0, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(irc__be_t6, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(irc__be_t12, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(irc__be_t18, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(irc__be_t24, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(irc__be_t30, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(irc__be_t38, irc_469_material_participation_kernel__strategic_shelter_reading, base_extractiveness, 38, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t0, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(irc__su_t6, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 6, 0.35).
narrative_ontology:measurement(irc__su_t12, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(irc__su_t18, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 18, 0.5).
narrative_ontology:measurement(irc__su_t24, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(irc__su_t30, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 30, 0.54).
narrative_ontology:measurement(irc__su_t38, irc_469_material_participation_kernel__strategic_shelter_reading, suppression_requirement, 38, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strategic_shelter_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the strategic_shelter_reading of the irc_469_material_participation_kernel. The sibling reading strict_gatekeeper_reading holds that the same statutory text requires verifiable substantial labor. The two readings share the same kernel (IRC Â§469 and attendant regulations) but instantiate structurally distinct constraints with different epsilon values, beneficiary/victim structures, and directionality profiles. They are linked by the kernel decomposition protocol, not by causal influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
