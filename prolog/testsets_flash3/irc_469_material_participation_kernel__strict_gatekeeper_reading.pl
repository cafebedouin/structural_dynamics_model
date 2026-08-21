% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strict_gatekeeper_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: IRC 469 Material Participation (Strict Gatekeeper Reading)
 *   domain: tax_law/real_estate_investment/regulatory_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'strict gatekeeper' reading of IRC Section
 *   469's material participation rules, which requires verifiable,
 *   substantial personal labor and a high documentation bar for passive
 *   losses to be deductible against ordinary income. This reading aims to
 *   narrow the qualifying population and increase compliance friction, making
 *   it difficult for investors to claim active status for activities that are
 *   primarily passive. It is one reading of the broader
 *   'irc_469_material_participation_kernel' which is contested by a
 *   'strategic_shelter_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.65).
domain_priors:suppression_score(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.78).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strict_gatekeeper_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strict_gatekeeper_reading, "IRC 469 Material Participation (Strict Gatekeeper Reading)").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strict_gatekeeper_reading, "tax_law/real_estate_investment/regulatory_interpretation").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strict_gatekeeper_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strict_gatekeeper_reading, '52536043-539f-4612-9892-ab6ab81135bf').
narrative_ontology:cs_kernel_codification('52536043-539f-4612-9892-ab6ab81135bf', fixed_text).
narrative_ontology:cs_authority_grounding('52536043-539f-4612-9892-ab6ab81135bf', lineage).
narrative_ontology:cs_interpretation_layer_present('52536043-539f-4612-9892-ab6ab81135bf').
narrative_ontology:cs_reading_relation('52536043-539f-4612-9892-ab6ab81135bf', irc_469_material_participation_kernel__strategic_shelter_reading, coexists_with).
narrative_ontology:cs_axiom('52536043-539f-4612-9892-ab6ab81135bf', foundational, tax_equity_requires_rigorous_distinction_between_active_and_passive_income).
narrative_ontology:cs_axiom_status(tax_equity_requires_rigorous_distinction_between_active_and_passive_income, holdable).
narrative_ontology:cs_axiom_grounding('52536043-539f-4612-9892-ab6ab81135bf', tax_equity_requires_rigorous_distinction_between_active_and_passive_income, deontological).
narrative_ontology:cs_axiom('52536043-539f-4612-9892-ab6ab81135bf', secondary, documentation_is_the_primary_evidence_of_participation).
narrative_ontology:cs_axiom_status(documentation_is_the_primary_evidence_of_participation, holdable).
narrative_ontology:cs_axiom_grounding('52536043-539f-4612-9892-ab6ab81135bf', documentation_is_the_primary_evidence_of_participation, conventional).
narrative_ontology:cs_reference_frame('52536043-539f-4612-9892-ab6ab81135bf', anti_abuse_fiscal_integrity).
narrative_ontology:cs_drift_state('52536043-539f-4612-9892-ab6ab81135bf', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('52536043-539f-4612-9892-ab6ab81135bf', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, us_treasury).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_preparers_and_advisors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_investors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_business_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from increased tax revenue due to disallowed passive losses and reduced tax shelter activity. Sets and enforces the regulations, interpreting 'material participation' strictly to maximize revenue and prevent abuse of tax provisions.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, us_treasury, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from the complexity and high documentation requirements, as investors need their expertise to navigate the rules and attempt to meet the strict participation tests. Their services become more valuable as compliance becomes harder.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_preparers_and_advisors, beneficiary,
    organized, biographical, mobile, national).

% Bear the cost of disallowed passive losses, increased compliance burden, and the need for meticulous, often burdensome, documentation of their time and activities. Their ability to deduct losses against ordinary income is severely restricted.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, real_estate_investors, payer,
    moderate, biographical, constrained, local).

% Similar to real estate investors, they face high hurdles to prove material participation in their businesses, especially if they have other income sources or multiple ventures. The strict documentation requirements divert time and resources from core business activities.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_business_owners, payer,
    moderate, biographical, constrained, local).

% Interpret and apply the IRC 469 regulations in specific cases, often ruling on the sufficiency of documentation and the nature of 'participation.' Their rulings shape the practical application of the strict gatekeeper reading.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_court_judges, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate tax policy by distinguishing between active business income (deductible losses) and passive investment income (non-deductible losses), preventing investors from using passive activities to shelter active income.
% TRANSFER_FUNCTION: Transfers potential tax deductions (passive losses) from real estate investors and small business owners to the US Treasury by disallowing their use against ordinary income, thereby increasing government revenue.
% ABSENT_VOICES: Advocates for small business and real estate investment would argue for more flexible participation rules, emphasizing the economic activity and risk undertaken by investors, but their concerns are often secondary to revenue protection and anti-abuse measures.
% DISAPPEARANCE_RATIONALE: If the material participation rules vanished, investors would immediately begin deducting passive losses against ordinary income, leading to a significant reduction in tax revenue for the US Treasury and a surge in tax shelter activity. The entire landscape of tax planning for real estate and business investments would fundamentally shift.
% FOUNDING_PROBLEM: The Tax Reform Act of 1986 sought to curb widespread tax shelter abuses where high-income individuals used passive investments to generate artificial losses, eroding the tax base and undermining fairness.
% FOUNDING_PROBLEM_CORROBORATION: The US Treasury and many tax policy experts attest that the problem of tax shelters remains live, requiring robust rules like material participation. While investors dispute the stringency, the underlying concern about income sheltering is widely acknowledged by independent fiscal policy analysts.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strict_gatekeeper_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strict_gatekeeper_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is substantial due to the disallowance of losses, effectively transferring wealth to the Treasury. Suppression (0.78) is high because the detailed documentation requirements and IRS enforcement make it very difficult to avoid compliance without significant risk. Theater ratio (0.20) is relatively low, as the IRS genuinely enforces these rules, and the documentation serves a real (if burdensome) function in verifying participation. The trend shows a slight increase in extractiveness and suppression over time as enforcement mechanisms mature and interpretations harden.
 *
 * PERSPECTIVAL GAP:
 *   From the Treasury's perspective, this is a necessary anti-abuse measure (a Rope or even a Mountain of fiscal prudence). From the investors' perspective, it's an overly burdensome Snare that unfairly restricts legitimate business deductions. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The US Treasury is the primary beneficiary and agenda-setter, gaining revenue from disallowed losses. Tax preparers and advisors also benefit from the complexity. Real estate investors and small business owners are the payers, bearing the direct financial cost of disallowed losses and the indirect cost of compliance. Tax court judges act as observers, interpreting the rules without direct financial gain from the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    documentation_burden_vs_abuse_prevention,
    'Is the high documentation burden imposed by the strict gatekeeper reading proportional to the actual risk of tax shelter abuse it prevents?',
    'Empirical study comparing compliance costs for legitimate active investors against the revenue saved from preventing abuse, potentially across different regulatory regimes.',
    'If the burden is disproportionate, it suggests the constraint is more extractive than necessary for its coordination function, potentially reclassifying it closer to a Snare. If proportional, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_burden_vs_abuse_prevention, empirical, 'Assesses the efficiency and fairness of the documentation requirements.').

omega_variable(
    strict_vs_permissive_interpretation,
    'Is the ''strict gatekeeper'' reading of material participation the most faithful interpretation of Congressional intent in IRC 469, or does it overreach in its anti-abuse stance?',
    'Legal analysis of legislative history, subsequent Congressional actions, and judicial precedent, potentially leading to a Supreme Court ruling clarifying the scope of ''material participation''.',
    'If a more permissive interpretation is found to align better with intent, the constraint''s extractiveness and suppression would be deemed excessive, pushing it towards a Snare. If the strict reading is upheld, its legitimacy as a Tangled Rope is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strict_vs_permissive_interpretation, conceptual, 'Examines the fidelity of the strict reading to legislative intent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(irc__tr_t6, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 6, 0.23).
narrative_ontology:measurement(irc__tr_t12, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(irc__tr_t18, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 18, 0.21).
narrative_ontology:measurement(irc__tr_t24, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement(irc__tr_t30, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(irc__be_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(irc__be_t6, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(irc__be_t12, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(irc__be_t18, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(irc__be_t24, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(irc__be_t30, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(irc__su_t6, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 6, 0.72).
narrative_ontology:measurement(irc__su_t12, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 12, 0.74).
narrative_ontology:measurement(irc__su_t18, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 18, 0.76).
narrative_ontology:measurement(irc__su_t24, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 24, 0.77).
narrative_ontology:measurement(irc__su_t30, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strict_gatekeeper_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
