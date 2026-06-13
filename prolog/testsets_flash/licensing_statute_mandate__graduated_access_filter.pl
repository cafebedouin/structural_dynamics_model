% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__graduated_access_filter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__graduated_access_filter, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: licensing_statute_mandate__graduated_access_filter
 *   human_readable: Statutory Credential Requirements as Graduated Access Filter
 *   domain: labor_economics/regulatory_policy/public_administration
 *
 * SUMMARY:
 *   This constraint models statutory credential requirements as a 'graduated
 *   access filter,' where the primary effect is to create tiered market
 *   access based on prior resource access and class. It is one reading of the
 *   'licensing_statute_mandate' kernel, emphasizing the exclusionary and
 *   rent-seeking aspects over public safety. The metrics reflect a high
 *   degree of extraction and suppression, consistent with a Snare, despite
 *   the claimed public safety coordination function.
 *
 * KEY AGENTS:
 *   - licensing_boards: Agenda-setter (institutional/constrained)
 *   - credentialed_professionals: Beneficiary (organized/mobile)
 *   - marginalized_workers: Payer (powerless/trapped)
 *   - uncredentialed_entrants: Payer (moderate/constrained)
 *   - public_consumers: Beneficiary (organized/mobile)
 *   - economic_analysts: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, 0.85).
domain_priors:suppression_score(licensing_statute_mandate__graduated_access_filter, 0.9).
domain_priors:theater_ratio(licensing_statute_mandate__graduated_access_filter, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, extractiveness, 0.85).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__graduated_access_filter, snare).
narrative_ontology:human_readable(licensing_statute_mandate__graduated_access_filter, "Statutory Credential Requirements as Graduated Access Filter").
narrative_ontology:topic_domain(licensing_statute_mandate__graduated_access_filter, "labor_economics/regulatory_policy/public_administration").

domain_priors:requires_active_enforcement(licensing_statute_mandate__graduated_access_filter).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__graduated_access_filter, 'aeb637ba-6372-4e18-b05b-cbb7fa8b9fe0').
narrative_ontology:cs_kernel_codification('aeb637ba-6372-4e18-b05b-cbb7fa8b9fe0', formalized).
narrative_ontology:cs_authority_grounding('aeb637ba-6372-4e18-b05b-cbb7fa8b9fe0', extraction).
narrative_ontology:cs_interpretation_layer_present('aeb637ba-6372-4e18-b05b-cbb7fa8b9fe0').
narrative_ontology:cs_reading_relation('aeb637ba-6372-4e18-b05b-cbb7fa8b9fe0', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('aeb637ba-6372-4e18-b05b-cbb7fa8b9fe0', licensing_statute_mandate__rent_seeking_suppression, coexists_with).
narrative_ontology:cs_axiom('aeb637ba-6372-4e18-b05b-cbb7fa8b9fe0', foundational, market_access_is_tiered_by_resource_access).
narrative_ontology:cs_axiom_status(market_access_is_tiered_by_resource_access, holdable).
narrative_ontology:cs_axiom_grounding('aeb637ba-6372-4e18-b05b-cbb7fa8b9fe0', market_access_is_tiered_by_resource_access, empirically_contingent).
narrative_ontology:cs_axiom('aeb637ba-6372-4e18-b05b-cbb7fa8b9fe0', foundational, credentialing_barriers_disproportionately_affect_marginalized).
narrative_ontology:cs_axiom_status(credentialing_barriers_disproportionately_affect_marginalized, holdable).
narrative_ontology:cs_axiom_grounding('aeb637ba-6372-4e18-b05b-cbb7fa8b9fe0', credentialing_barriers_disproportionately_affect_marginalized, empirically_contingent).
narrative_ontology:cs_reference_frame('aeb637ba-6372-4e18-b05b-cbb7fa8b9fe0', unfettered_market_access).
narrative_ontology:cs_drift_state('aeb637ba-6372-4e18-b05b-cbb7fa8b9fe0', contemporary_regulatory_state, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('aeb637ba-6372-4e18-b05b-cbb7fa8b9fe0', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, credentialed_professionals).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, licensing_boards).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, marginalized_workers).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, uncredentialed_entrants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, public_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the statutory credentialing requirements, setting standards, approving educational programs, and issuing licenses. Benefits from fees and maintains professional boundaries. Their mandate is to uphold public safety and professional integrity.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, licensing_boards, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from restricted labor supply, leading to higher wages and job security. They have successfully navigated the credentialing process, often with prior access to resources for education and training. They advocate for maintaining or increasing entry barriers.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, credentialed_professionals, beneficiary,
    organized, biographical, mobile, national).

% Are excluded from higher-paying, credentialed professions due to the high cost and time investment required for education and licensing. They often lack the financial resources, social capital, or educational background to meet the requirements, trapping them in lower-wage, uncredentialed work.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, marginalized_workers, payer,
    powerless, immediate, trapped, local).

% Aspire to enter credentialed professions but face significant barriers. They may invest heavily in education and training, only to find the licensing process opaque or excessively burdensome, leading to debt and delayed market entry. Their options are to comply, find alternative (often lower-status) work, or leave the field.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, uncredentialed_entrants, payer,
    moderate, biographical, constrained, regional).

% Are theoretically protected from incompetent practitioners by minimum standards, ensuring a baseline quality of service. However, they also face higher costs for services due to reduced competition and limited access to providers in underserved areas.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, public_consumers, beneficiary,
    organized, biographical, mobile, local).

% Study the impact of licensing on labor markets, wages, and consumer prices. They often highlight the anti-competitive effects and regressive impact on social mobility, providing data that challenges the public safety rationale.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, economic_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__graduated_access_filter, credentialed_professionals).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__graduated_access_filter, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a standardized baseline of competence and ethical conduct for professionals, theoretically ensuring public safety and trust in complex service domains.
% TRANSFER_FUNCTION: Transfers economic rents (higher wages, reduced competition) from marginalized workers and consumers to credentialed professionals and the institutions that administer licensing, by restricting labor supply and creating artificial scarcity.
% ABSENT_VOICES: Prospective workers from disadvantaged backgrounds, who are disproportionately affected by the barriers, are largely absent from the policy-making process that sets and reviews credentialing requirements. Their voices would highlight the class-sorting and exclusionary effects.
% DISAPPEARANCE_RATIONALE: If statutory credentialing vanished overnight, labor markets for these professions would immediately open, leading to a surge of new entrants, downward pressure on wages, and potentially a wider range of service quality. The professional landscape would reorganize around reputation, private certification, and market-based signaling, rather than state-mandated entry barriers.
% FOUNDING_PROBLEM: The founding problem was to protect the public from unqualified or unethical practitioners in fields requiring specialized knowledge, ensuring a minimum standard of service quality and public trust.
% FOUNDING_PROBLEM_CORROBORATION: Licensing boards and many credentialed professionals assert the public safety problem is still live, citing ongoing risks of harm from unqualified practice. Economic analysts and advocates for marginalized workers argue the problem is largely solved or overstated, and the current structure primarily serves to protect incumbent interests, with corroboration from empirical studies on labor market effects and social mobility.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__graduated_access_filter, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__graduated_access_filter, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__graduated_access_filter, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(licensing_statute_mandate__graduated_access_filter, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__graduated_access_filter_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__graduated_access_filter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the significant economic rents captured by credentialed professionals due to restricted labor supply. Suppression (0.90) is high because the statutory nature of the requirements creates formidable, legally enforced barriers to entry, with few viable alternatives for those seeking to practice. The theater ratio (0.20) is low, indicating that while some public safety function remains, a substantial portion of the enforcement effort is directed at maintaining market exclusivity rather than solely ensuring competence. Accessibility collapse (0.75) is high because the formal requirements make alternative paths to practice nearly impossible. Resistance (0.60) is moderate, coming from economic analysts and advocacy groups, but is often outmatched by the organized power of incumbent professionals.
 *
 * PERSPECTIVAL GAP:
 *   Credentialed professionals and licensing boards perceive this as a legitimate coordination mechanism for public safety, while marginalized workers and economic analysts experience it as an extractive snare. The engine's classification will likely diverge from the 'public_safety_coordination' claim due to the high extractiveness and suppression metrics, reflecting the 'graduated access filter' reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Licensing boards (agenda-setter) and credentialed professionals (beneficiary) are positioned to gain from the constraint, experiencing low directionality. Marginalized workers and uncredentialed entrants (payers) are the primary targets of extraction and suppression, experiencing high directionality due to their trapped or constrained exit options. Public consumers are mixed, benefiting from perceived quality but paying higher prices.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (public safety) is contested. While a foundational public safety problem existed, the current structure's persistence is increasingly attributed to the benefits it confers on incumbent professionals rather than solely addressing the original problem. This suggests a drift towards a Snare, where the coordination story serves as cover for extraction. The 'graduated_access_filter' reading highlights this mandatrophy, preventing mislabeling as a pure Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_safety_vs_market_access,
    'To what extent do current credentialing requirements genuinely enhance public safety versus primarily restricting market access for new entrants?',
    'Comparative analysis of public harm rates in jurisdictions with varying levels of credentialing stringency, or studies on the efficacy of alternative, less restrictive quality assurance mechanisms.',
    'If public safety benefits are marginal, the constraint''s extractiveness is more clearly attributable to rent-seeking, reinforcing its Snare classification. If public safety benefits are substantial and unique to the current structure, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_safety_vs_market_access, empirical, 'Distinguishing genuine public safety benefits from market access restriction.').

omega_variable(
    class_sorting_mechanism,
    'Are the differential barriers to credential acquisition (cost of education, time investment, social capital) an unavoidable consequence of ensuring competence, or do they primarily function as a class-sorting mechanism?',
    'Longitudinal studies tracking the socioeconomic background of successful vs. unsuccessful credential applicants, and analysis of policy interventions designed to reduce access barriers without compromising competence.',
    'If primarily a class-sorting mechanism, the constraint''s suppression and extractiveness are amplified for marginalized groups, solidifying its Snare classification and highlighting its regressive social impact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(class_sorting_mechanism, empirical, 'Assessing whether barriers are competence-based or class-based.').

omega_variable(
    reading_framing_legitimacy,
    'Is the ''graduated_access_filter'' reading a legitimate interpretation of the licensing statute''s actual effects, or is it an overly cynical framing that ignores genuine public benefits?',
    'Consensus among independent economic and sociological researchers on the primary observed effects of licensing, weighed against the stated intent of the statutes and the claims of licensing boards.',
    'If this reading is widely corroborated, it challenges the legitimacy of the ''public_safety_coordination'' framing. If it is dismissed as overly cynical, the constraint''s classification might lean more towards a Tangled Rope, acknowledging a stronger coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_legitimacy, conceptual, 'Validity of the ''graduated access filter'' framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__graduated_access_filter, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t1950, licensing_statute_mandate__graduated_access_filter, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(lice_tr_t1970, licensing_statute_mandate__graduated_access_filter, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(lice_tr_t1990, licensing_statute_mandate__graduated_access_filter, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(lice_tr_t2010, licensing_statute_mandate__graduated_access_filter, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(lice_tr_t2024, licensing_statute_mandate__graduated_access_filter, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(lice_be_t1950, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(lice_be_t1970, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(lice_be_t1990, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(lice_be_t2010, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(lice_be_t2024, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t1950, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(lice_su_t1970, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(lice_su_t1990, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 1990, 0.78).
narrative_ontology:measurement(lice_su_t2010, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(lice_su_t2024, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__graduated_access_filter, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('graduated_access_filter') of the 'licensing_statute_mandate' kernel. It focuses on the exclusionary and rent-seeking effects, contrasting with 'public_safety_coordination' and 'rent_seeking_suppression' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
