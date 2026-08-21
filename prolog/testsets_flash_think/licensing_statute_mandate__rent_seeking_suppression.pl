% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__rent_seeking_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__rent_seeking_suppression, []).

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
 *   constraint_id: licensing_statute_mandate__rent_seeking_suppression
 *   human_readable: Statutory Professional Licensing for Rent-Seeking
 *   domain: labor_economics/regulatory_policy/public_administration
 *
 * SUMMARY:
 *   This constraint story analyzes statutory professional licensing
 *   requirements from the 'rent_seeking_suppression' reading of the
 *   'licensing_statute_mandate' kernel. It posits that these requirements,
 *   while often justified by public safety, primarily function to restrict
 *   labor supply and extract economic rents for incumbent practitioners. The
 *   metrics reflect a high degree of extraction and suppression, with a
 *   growing theatrical component as the public safety justification becomes
 *   less credible over time.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, 0.75).
domain_priors:suppression_score(licensing_statute_mandate__rent_seeking_suppression, 0.8).
domain_priors:theater_ratio(licensing_statute_mandate__rent_seeking_suppression, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, extractiveness, 0.75).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(licensing_statute_mandate__rent_seeking_suppression, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__rent_seeking_suppression, snare).
narrative_ontology:human_readable(licensing_statute_mandate__rent_seeking_suppression, "Statutory Professional Licensing for Rent-Seeking").
narrative_ontology:topic_domain(licensing_statute_mandate__rent_seeking_suppression, "labor_economics/regulatory_policy/public_administration").

domain_priors:requires_active_enforcement(licensing_statute_mandate__rent_seeking_suppression).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__rent_seeking_suppression, 'cec1d052-e57d-4988-8118-d2a4b49d16a3').
narrative_ontology:cs_kernel_codification('cec1d052-e57d-4988-8118-d2a4b49d16a3', formalized).
narrative_ontology:cs_authority_grounding('cec1d052-e57d-4988-8118-d2a4b49d16a3', extraction).
narrative_ontology:cs_interpretation_layer_present('cec1d052-e57d-4988-8118-d2a4b49d16a3').
narrative_ontology:cs_reading_relation('cec1d052-e57d-4988-8118-d2a4b49d16a3', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('cec1d052-e57d-4988-8118-d2a4b49d16a3', licensing_statute_mandate__graduated_access_filter, coexists_with).
narrative_ontology:cs_axiom('cec1d052-e57d-4988-8118-d2a4b49d16a3', foundational, labor_scarcity_enhances_professional_value).
narrative_ontology:cs_axiom_status(labor_scarcity_enhances_professional_value, holdable).
narrative_ontology:cs_axiom_grounding('cec1d052-e57d-4988-8118-d2a4b49d16a3', labor_scarcity_enhances_professional_value, instrumental).
narrative_ontology:cs_axiom('cec1d052-e57d-4988-8118-d2a4b49d16a3', secondary, market_entry_barriers_protect_incumbents).
narrative_ontology:cs_axiom_status(market_entry_barriers_protect_incumbents, holdable).
narrative_ontology:cs_axiom_grounding('cec1d052-e57d-4988-8118-d2a4b49d16a3', market_entry_barriers_protect_incumbents, conventional).
narrative_ontology:cs_reference_frame('cec1d052-e57d-4988-8118-d2a4b49d16a3', incumbent_privilege_framework).
narrative_ontology:cs_drift_state('cec1d052-e57d-4988-8118-d2a4b49d16a3', contemporary_regulatory_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cec1d052-e57d-4988-8118-d2a4b49d16a3', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__rent_seeking_suppression, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, new_entrants).
narrative_ontology:constraint_victim(licensing_statute_mandate__rent_seeking_suppression, consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively lobby for and benefit from stringent licensing requirements, which limit competition and allow for higher prices for their services. They often sit on regulatory boards, influencing enforcement and new rule-making.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners, agenda_setter,
    organized, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__rent_seeking_suppression, incumbent_practitioners, beneficiary).

% Face significant financial and time barriers to entry due to extensive education, examination, and experience requirements. Many are unable to meet these requirements, effectively being excluded from the profession or forced into lower-paying, unregulated roles.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, new_entrants, payer,
    powerless, immediate, trapped, local).

% Pay higher prices for services due to the artificially restricted supply of qualified practitioners. While they may perceive a benefit from 'quality assurance,' the primary effect is reduced access and increased cost.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, consumers, payer,
    moderate, immediate, constrained, local).

% Administer and enforce the licensing statutes. Often composed of incumbent practitioners, their actions tend to reinforce existing barriers and protect the interests of those already licensed, rather than solely focusing on public safety.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, regulatory_boards, agenda_setter,
    institutional, generational, constrained, national).

% Argue against overly restrictive licensing, highlighting the negative impacts on access, affordability, and economic mobility. Their voices are often marginalized in legislative and regulatory processes dominated by professional associations.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, consumer_advocacy_groups, excluded,
    organized, generational, constrained, national).

% Create and amend the statutory framework for licensing. They are subject to lobbying from both incumbent professional groups and consumer advocates, often balancing competing interests with political expediency.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__rent_seeking_suppression, legislators, agenda_setter,
    institutional, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ostensibly coordinates minimum competence standards to protect the public from unqualified practitioners, ensuring a baseline of quality in professional services.
% TRANSFER_FUNCTION: Transfers economic rents from new entrants (via barriers to entry) and consumers (via higher prices) to incumbent practitioners, by artificially restricting the supply of labor.
% ABSENT_VOICES: Unlicensed but competent individuals, who are excluded from the profession, and a broader segment of the public who would benefit from lower-cost services, are not at the table. Consumer advocacy groups are present but often outmatched by well-funded professional lobbies.
% DISAPPEARANCE_RATIONALE: If statutory licensing vanished overnight, the labor market for these professions would rapidly expand, prices for services would likely decrease due to increased competition, and new models for credentialing (e.g., private certification, reputation systems) would emerge. The professional landscape would fundamentally reorganize.
% FOUNDING_PROBLEM: The founding problem was to protect the public from harm caused by incompetent or unethical practitioners, ensuring a minimum standard of professional quality.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent practitioners and regulatory boards claim the problem is still live and the statutes are essential for public safety. However, economic studies and consumer advocacy groups, from outside the benefiting parties, argue that the problem is largely solved or exaggerated, and the current structure primarily serves rent-seeking, not public protection.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__rent_seeking_suppression, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__rent_seeking_suppression, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__rent_seeking_suppression, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(licensing_statute_mandate__rent_seeking_suppression, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__rent_seeking_suppression, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__rent_seeking_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__rent_seeking_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) reflects the significant economic transfer from new entrants and consumers to incumbents. Suppression (0.8) is high due to the legal barriers to entry and the active enforcement by regulatory boards. The theater ratio (0.5) indicates that while some genuine public safety functions may exist, a substantial portion of the regulatory activity is performative, serving to legitimize the rent-seeking. Accessibility collapse (0.75) is high because the statutory requirements create substantial, often insurmountable, barriers for many potential practitioners. Resistance (0.6) is moderate, coming from new entrants, consumer groups, and some legislators, but is often outmatched by organized professional lobbies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of incumbent practitioners, the constraint is a legitimate 'rope' ensuring quality and professional standards. From the perspective of new entrants and consumers, it operates as a 'snare' designed to limit competition and extract wealth. The engine's classification will highlight this divergence from the claimed 'snare' type based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent practitioners are clear beneficiaries (d=0.0-0.1) as they capture rents and influence the rules. New entrants are direct targets (d=0.9-1.0) as they bear the costs of exclusion or compliance. Consumers are indirect targets (d=0.7-0.8) through higher prices. Regulatory boards, while ostensibly neutral, often act as agenda-setters for incumbents. Legislators are influenced by competing interests.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_safety_vs_rent_seeking_primary_function,
    'Is the primary function of this specific licensing regime genuinely public safety, or is it primarily rent extraction for incumbents?',
    'Empirical analysis comparing the stringency of requirements to actual public harm rates, and economic analysis of market entry barriers and price effects in comparable unregulated markets.',
    'If public safety is primary, the constraint might reclassify towards a ''rope'' or ''tangled_rope'' with lower extraction. If rent extraction is primary, the ''snare'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_safety_vs_rent_seeking_primary_function, empirical, 'Ambiguity regarding the true purpose of professional licensing statutes.').

omega_variable(
    competence_correlation_vs_artificial_scarcity,
    'To what extent do the statutory requirements genuinely correlate with the minimum competence necessary for public safety, versus creating artificial barriers to entry that do not enhance quality?',
    'Expert review of licensing curricula and exams against demonstrated job performance and public safety outcomes, and comparison with alternative, less restrictive credentialing models.',
    'If requirements are largely artificial, the ''suppression'' and ''extractiveness'' metrics are validated as excessive. If they are strongly correlated with competence, these metrics might be slightly lower, reflecting necessary coordination costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_correlation_vs_artificial_scarcity, empirical, 'Distinguishing genuine competence requirements from arbitrary entry barriers.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression for new entrants structural (legal barriers, financial costs) or internalized (discouragement, belief in the necessity of the barriers)?',
    'Surveys of aspiring practitioners on perceived vs. actual barriers, and observation of entry rates in jurisdictions with relaxed licensing requirements.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, as new entrants carry the suppression with them even if some barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for new entrants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__rent_seeking_suppression, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t1970, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(lice_tr_t1980, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 1980, 0.28).
narrative_ontology:measurement(lice_tr_t1990, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(lice_tr_t2000, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(lice_tr_t2010, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 2010, 0.47).
narrative_ontology:measurement(lice_tr_t2020, licensing_statute_mandate__rent_seeking_suppression, theater_ratio, 2020, 0.5).

% Extraction over time
narrative_ontology:measurement(lice_be_t1970, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(lice_be_t1980, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(lice_be_t1990, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(lice_be_t2000, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(lice_be_t2010, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 2010, 0.72).
narrative_ontology:measurement(lice_be_t2020, licensing_statute_mandate__rent_seeking_suppression, base_extractiveness, 2020, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t1970, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(lice_su_t1980, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(lice_su_t1990, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 1990, 0.67).
narrative_ontology:measurement(lice_su_t2000, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 2000, 0.73).
narrative_ontology:measurement(lice_su_t2010, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 2010, 0.77).
narrative_ontology:measurement(lice_su_t2020, licensing_statute_mandate__rent_seeking_suppression, suppression_requirement, 2020, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__rent_seeking_suppression, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
