% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__welfare_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__welfare_coordination_reading, []).

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
 *   constraint_id: federation_membership_kernel__welfare_coordination_reading
 *   human_readable: EU Welfare Coordination Regime (Posting-of-Workers / Social Security Coordination Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This story instantiates the welfare-coordination reading of the
 *   federation membership kernel: free movement is secured not by harmonizing
 *   welfare systems but by coordinating which member state's system applies
 *   to a mobile worker, while enforcing equal-treatment and anti-dumping
 *   floors against the crudest forms of wage undercutting. The 2004 Eastern
 *   enlargement and subsequent posted-worker growth (especially construction
 *   and road haulage) are the interval's inflection points — extraction rises
 *   sharply from 2004 as the wage differential between old and new member
 *   states became large enough to make posting profitable at scale, and the
 *   2018 Directive revision (tightening equal-pay-equal-place rules) marks
 *   the coordination apparatus visibly trying to re-tighten a structure that
 *   had drifted toward cost arbitrage. This is a distinct constraint from the
 *   integration_reading (which treats expansive ECJ interpretation of free
 *   movement as the operative mechanism) and from the
 *   member_sovereignty_reading (which treats national exclusion authority
 *   over economically inactive migrants as the operative mechanism) — each
 *   reading has its own ε, its own victim set, and its own classification,
 *   linked here only through the shared kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, 0.58).
domain_priors:suppression_score(federation_membership_kernel__welfare_coordination_reading, 0.52).
domain_priors:theater_ratio(federation_membership_kernel__welfare_coordination_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_kernel__welfare_coordination_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__welfare_coordination_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__welfare_coordination_reading, "EU Welfare Coordination Regime (Posting-of-Workers / Social Security Coordination Reading)").
narrative_ontology:topic_domain(federation_membership_kernel__welfare_coordination_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__welfare_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__welfare_coordination_reading, '7faaf983-41ef-44fc-8737-199cd8719289').
narrative_ontology:cs_kernel_codification('7faaf983-41ef-44fc-8737-199cd8719289', formalized).
narrative_ontology:cs_authority_grounding('7faaf983-41ef-44fc-8737-199cd8719289', extraction).
narrative_ontology:cs_interpretation_layer_present('7faaf983-41ef-44fc-8737-199cd8719289').
narrative_ontology:cs_reading_relation('7faaf983-41ef-44fc-8737-199cd8719289', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('7faaf983-41ef-44fc-8737-199cd8719289', federation_membership_kernel__member_sovereignty_reading, influences).
narrative_ontology:cs_axiom('7faaf983-41ef-44fc-8737-199cd8719289', foundational, coordination_not_harmonization_suffices).
narrative_ontology:cs_axiom_status(coordination_not_harmonization_suffices, holdable).
narrative_ontology:cs_axiom_grounding('7faaf983-41ef-44fc-8737-199cd8719289', coordination_not_harmonization_suffices, conventional).
narrative_ontology:cs_axiom('7faaf983-41ef-44fc-8737-199cd8719289', secondary, anti_dumping_floor_without_contribution_equalization).
narrative_ontology:cs_axiom_status(anti_dumping_floor_without_contribution_equalization, holdable).
narrative_ontology:cs_axiom_grounding('7faaf983-41ef-44fc-8737-199cd8719289', anti_dumping_floor_without_contribution_equalization, instrumental).
narrative_ontology:cs_reference_frame('7faaf983-41ef-44fc-8737-199cd8719289', regulation_883_2004_coordination_settlement).
narrative_ontology:cs_drift_state('7faaf983-41ef-44fc-8737-199cd8719289', post_2018_directive_revision, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7faaf983-41ef-44fc-8737-199cd8719289', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, labor_intermediary_firms).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, high_wage_host_employers_seeking_cost_labor).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, eu_commission_coordination_apparatus).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, posted_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, host_state_domestic_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, sending_state_fiscal_base).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, national_welfare_ministries).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__welfare_coordination_reading, low_wage_member_state_governments).
narrative_ontology:constraint_victim(federation_membership_kernel__welfare_coordination_reading, low_wage_member_state_governments).
narrative_ontology:constraint_vindicates(federation_membership_kernel__welfare_coordination_reading, subsidiarity_over_harmonization_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_kernel__welfare_coordination_reading, coordination_not_unification_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sent by home-country employers to work temporarily in higher-wage member states while remaining enrolled in home-country social security (A1 certificate), often for up to two years under levy exemption. Paid host-state minimum wage rates in some sectors but contribute to home-country systems at home-country rates, and in road haulage face cabotage arrangements that push effective take-home pay below both host and sometimes home benchmarks. Cannot easily contest misclassification or wage-floor violations from within the host state, and returning home to litigate is costly; their formal legal status depends entirely on the posting employer maintaining paperwork.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, posted_workers, payer,
    powerless, biographical, constrained, continental).

% Compete in the same labor market segments (construction, logistics, meat processing) against posted workers whose employers do not pay host-state social contributions, producing a persistent wage-cost gap regardless of nominal wage-floor compliance. Cannot exit the national labor market without their own cross-border mobility, and unions have limited standing to challenge postings that are technically compliant with EU coordination rules.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, host_state_domestic_workers, payer,
    moderate, biographical, constrained, national).

% Loses working-age contributors and taxpayers to posting and permanent outward migration while continuing to bear the costs of educating and training them; receives no transfer mechanism from destination states to offset lost fiscal capacity, and posting arrangements in particular return no revenue at all to the host state either, meaning value generated by the labor is captured mostly by the intermediary and the sending state's investment is stranded.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, sending_state_fiscal_base, payer,
    institutional, generational, trapped, national).

% Recruit and post workers across borders, structuring contracts to exploit the social-security coordination rules (home-state contribution rates, levy exemption windows, cabotage sequencing) to capture the wage-cost differential as margin. Can relocate corporate registration across member states to optimize which social security regime applies, and face limited enforcement capacity from under-resourced host-state labor inspectorates.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, labor_intermediary_firms, beneficiary,
    organized, biographical, arbitrage, continental).

% Use posted-worker contracts to access lower effective labor costs than hiring domestically, particularly in construction and logistics, while remaining compliant with formal EU rules. Can substitute freely among posting agencies and sending countries if enforcement tightens in one corridor.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, high_wage_host_employers_seeking_cost_labor, beneficiary,
    powerful, biographical, mobile, national).

% Administers the Posted Workers Directive, its 2018 revision, and social security coordination Regulation 883/2004, enforcing anti-social-dumping rules (equal pay for equal work in same place) while explicitly declining to harmonize welfare system design, contribution rates, or benefit levels across member states. Sets the coordination architecture and adjudicates disputes but has no independent enforcement force in member states and relies on national labor inspectorates of variable capacity.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eu_commission_coordination_apparatus, agenda_setter,
    institutional, civilizational, analytical, continental).

% Retain full authority over welfare system design, eligibility, and benefit levels within their own territory, which they value as core sovereignty, but must operate anti-social-dumping enforcement using domestic inspectorates that are chronically under-resourced relative to the scale of cross-border posting.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, national_welfare_ministries, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__welfare_coordination_reading, national_welfare_ministries, beneficiary).

% Benefit politically from framing posting as an export industry generating remittances and reducing domestic unemployment, while absorbing the long-run cost of workforce depletion in construction, healthcare, and skilled trades with no compensating transfer from destination states.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, low_wage_member_state_governments, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__welfare_coordination_reading, low_wage_member_state_governments, payer).

% Would argue for harmonized minimum contribution floors or a transfer mechanism compensating sending states, but have limited standing in EU-level rulemaking dominated by Commission technocrats and host-state social partners; their objections surface mainly through European Parliament committee testimony rather than binding negotiation.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__welfare_coordination_reading, eastern_and_southern_trade_unions, excluded,
    organized, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__welfare_coordination_reading, labor_intermediary_firms).
narrative_ontology:fixing_cost_class(federation_membership_kernel__welfare_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents a race-to-the-bottom collapse of free movement into either full welfare harmonization (politically impossible) or a total collapse of labor mobility (economically costly) by coordinating which state's social security system applies to a mobile worker at any given time, avoiding double contributions or coverage gaps while letting each member state keep its own welfare design.
% TRANSFER_FUNCTION: Moves labor-cost savings from posted workers' foregone host-state social contributions and wage-floor gaps to posting employers and labor intermediaries; moves fiscal investment in human capital from sending states to destination-state employers without compensating transfer; moves competitive pressure onto host-state domestic workers in the same labor market segments.
% ABSENT_VOICES: Sending-state and Eastern/Southern European trade unions object to the lack of a fiscal transfer mechanism and to levy-exemption windows that make posting cheaper than local hiring, but they have no seat in the Commission's technical coordination committees, which are dominated by Commission staff and host-state employer/labor-ministry representatives.
% DISAPPEARANCE_RATIONALE: If the coordination architecture (Regulation 883/2004, the Posted Workers Directive) disappeared overnight, cross-border postings would either collapse into full double social-security liability (ending most cost arbitrage) or require ad hoc bilateral treaties; sending states would need new mechanisms to prevent contribution gaps for their diaspora; host-state labor markets would lose the specific undercutting channel that posting currently provides, though direct permanent migration would likely intensify to fill some of the gap.
% FOUNDING_PROBLEM: Free movement of workers under the Treaty of Rome and subsequent single market completion required a mechanism to prevent workers from losing social protection when they crossed borders, and to prevent a legal vacuum in which no state's welfare system covered a mobile worker — while member states refused to cede control over welfare design, financing, or eligibility.
% FOUNDING_PROBLEM_CORROBORATION: The Commission and destination-state employers attest the founding problem (coverage gaps, double contributions) is still substantially live and the coordination regime functions as designed. Independent labor economists (e.g., European Trade Union Institute research, ILO reports on posting) and sending-state unions attest the coordination architecture has been substantially repurposed into a cost-arbitrage mechanism that the original coverage-gap problem no longer explains — the 2018 Directive revision and subsequent ECJ cases tightening equal-pay requirements are themselves evidence the Commission's own institutions recognize drift from founding purpose.
narrative_ontology:disappearance_verdict(federation_membership_kernel__welfare_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__welfare_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__welfare_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_kernel__welfare_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__welfare_coordination_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__welfare_coordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__welfare_coordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__welfare_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) reflects a real and growing gap between the wage-cost savings captured by posting employers/intermediaries and any coordination benefit returned to posted workers or sending states; the post-2018 plateau/slight decline reflects the Directive revision's partial bite. Suppression (0.52) is moderate rather than severe: posted workers are not physically coerced, but their legal status (A1 certificates, employer-controlled documentation, limited host-state standing) constrains effective exit from exploitative postings, and host-state domestic workers cannot exit the labor market segment being undercut. Theater ratio (0.34) captures that a meaningful share of 'anti-dumping enforcement' activity (inspections, compliance audits) is under-resourced relative to posting volume and functions partly as legitimating performance for a coordination architecture that structurally advantages cost arbitrage. Accessibility collapse is moderate (0.45): workers and unions can in principle contest specific postings through ECJ litigation (as in Laval, Viking, and post-2018 cases), so alternatives are not fully closed off, but the practical cost of doing so is high. Resistance is substantial (0.61): host-state unions, sending-state unions, and some member-state governments have actively contested aspects of the regime, producing the 2018 revision itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Posted workers, host-state domestic workers, and sending-state fiscal bases are declared victims because the coordination architecture's specific design choices (home-state contribution rates during posting, levy exemption windows, no compensating fiscal transfer to sending states) systematically move value toward posting employers and intermediaries. Labor intermediary firms and cost-seeking host employers are beneficiaries because they capture the wage-cost differential as margin or savings. The EU Commission's coordination apparatus sits as agenda_setter rather than a clean beneficiary or victim — it administers and periodically re-tightens the rules but does not itself collect the extracted value; low-wage sending-state governments carry a genuine dual role (political beneficiary of the 'export industry' framing, fiscal victim of the uncompensated brain/labor drain).
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function (preventing coverage gaps and double contributions for genuinely mobile workers) remains structurally live — a worker moving permanently between two member states does need exactly this coordination, and removing it would create real gaps. What has drifted is the posting-specific subset of the architecture: the 2-year levy exemption and home-state contribution rule were designed for short genuine secondments, not for the large-scale, intermediary-organized cost-arbitrage posting industry that emerged post-2004. Classifying this as tangled_rope rather than snare preserves the real coordination function (avoiding double coverage) while flagging the asymmetric extraction riding on it (cost arbitrage via contribution-rate differentials) — a pure snare classification would miss that permanently mobile workers with no arbitrage motive still need and benefit from the coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    posting_as_designed_vs_exploited_function,
    'Was the 2-year social-levy exemption and home-state-contribution rule designed anticipating large-scale intermediary-organized posting industries, or was it designed for genuine short-term secondments and subsequently captured by an industry structure that emerged only after the 2004/2007 enlargements created a large enough wage differential to make arbitrage profitable at scale?',
    'Legislative history analysis of the original 1996 Posted Workers Directive drafting record and comparison with Commission impact assessments preceding the 2018 revision; comparison of posting volumes and sectoral composition before and after 2004 enlargement.',
    'If the rule was designed anticipating this use, the extraction is a known and accepted design tradeoff rather than drift, weakening the tangled_rope classification toward a more deliberate snare; if it was captured post-hoc by an unanticipated intermediary industry, tangled_rope with an active-correction narrative (the 2018 revision) is the accurate reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(posting_as_designed_vs_exploited_function, empirical, 'Whether the posting cost-arbitrage function was anticipated in original design or emerged as unintended drift.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the welfare_coordination_reading''s boundary sit relative to the member_sovereignty_reading — is the anti-social-dumping enforcement structure itself an exercise of member-state sovereignty (each state protecting its own labor market) or a supranational coordination function imposed on member states?',
    'Analysis of whether anti-dumping enforcement action originates from national inspectorates acting under domestic political pressure (sovereignty reading) versus Commission infringement proceedings and ECJ preliminary rulings (coordination/integration reading) — track the initiating actor across a sample of enforcement actions 2010-2024.',
    'If enforcement is predominantly national-inspectorate-initiated, this reading''s beneficiary/victim structure overlaps substantially with the member_sovereignty_reading''s structure at the enforcement layer even though the two readings differ in their normative premise about what free movement fundamentally is; if predominantly Commission/ECJ-initiated, the readings remain more cleanly separated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Structural location of the boundary between the coordination reading and the sovereignty reading at the enforcement layer.').

omega_variable(
    sending_state_transfer_absence_deliberateness,
    'Is the absence of a fiscal transfer mechanism from destination states to sending states (to compensate for human capital investment lost through posting and permanent migration) a deliberate design choice reflecting member-state sovereignty over social policy, or an unaddressed gap the Commission lacks legal competence to fix under current Treaty architecture?',
    'Review of Treaty competence limits on fiscal transfers tied to labor mobility (Article 45 TFEU and related case law) and Commission position papers on cohesion funding as a partial substitute mechanism.',
    'If deliberate sovereignty choice, sending-state fiscal loss is a known and accepted cost of the coordination bargain; if a competence gap, it represents an unaddressed structural extraction that could in principle be fixed within the existing Treaty framework via cohesion fund reallocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sending_state_transfer_absence_deliberateness, conceptual, 'Whether the missing sending-state compensation mechanism is deliberate or an unaddressed competence gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__welfare_coordination_reading, 1971, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1971, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 1971, 0.1).
narrative_ontology:measurement(fede_tr_t1996, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 1996, 0.15).
narrative_ontology:measurement(fede_tr_t2004, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2004, 0.24).
narrative_ontology:measurement(fede_tr_t2012, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2012, 0.33).
narrative_ontology:measurement(fede_tr_t2018, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2018, 0.3).
narrative_ontology:measurement(fede_tr_t2024, federation_membership_kernel__welfare_coordination_reading, theater_ratio, 2024, 0.34).

% Extraction over time
narrative_ontology:measurement(fede_be_t1971, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 1971, 0.22).
narrative_ontology:measurement(fede_be_t1996, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 1996, 0.31).
narrative_ontology:measurement(fede_be_t2004, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2004, 0.46).
narrative_ontology:measurement(fede_be_t2012, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2012, 0.56).
narrative_ontology:measurement(fede_be_t2018, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2018, 0.6).
narrative_ontology:measurement(fede_be_t2024, federation_membership_kernel__welfare_coordination_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1971, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 1971, 0.2).
narrative_ontology:measurement(fede_su_t1996, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 1996, 0.28).
narrative_ontology:measurement(fede_su_t2004, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2004, 0.4).
narrative_ontology:measurement(fede_su_t2012, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2012, 0.47).
narrative_ontology:measurement(fede_su_t2018, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2018, 0.55).
narrative_ontology:measurement(fede_su_t2024, federation_membership_kernel__welfare_coordination_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__welfare_coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__welfare_coordination_reading, 0.12).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__welfare_coordination_reading, federation_membership_kernel__member_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'EU free movement' under the federation_membership_kernel. integration_reading treats expansive ECJ rights-maximization as the operative mechanism (different beneficiary/victim structure, likely lower measured extraction from that reading's own lights). member_sovereignty_reading treats national exclusionary authority over economically inactive migrants as the operative mechanism (different victim set: excluded economically-inactive migrants rather than posted workers). This welfare_coordination_reading is structurally distinct: its ε is authored specifically for the posting-of-workers/social-security-coordination arrangement, which has its own victim set (posted workers, host domestic workers, sending-state fiscal base) not shared identically with either sibling. All three should be read as separate constraints sharing a kernel, not as three measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
