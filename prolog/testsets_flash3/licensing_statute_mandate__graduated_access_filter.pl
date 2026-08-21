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
 *   constraint_id: licensing_statute_mandate__graduated_access_filter
 *   human_readable: Statutory Credential Requirements: Graduated Access Filter Reading
 *   domain: labor_economics/regulatory_policy/public_administration
 *
 * SUMMARY:
 *   This constraint story instantiates the 'graduated_access_filter' reading
 *   of the 'licensing_statute_mandate' kernel. It describes statutory
 *   credential requirements as a mechanism that creates tiered market access,
 *   disproportionately benefiting incumbent professionals and licensing
 *   boards while extracting from marginalized workers and new entrants
 *   lacking resources. The high extractiveness and suppression metrics
 *   reflect the structural barriers and economic transfers inherent in this
 *   reading. The claimed type is 'snare' because the coordination story
 *   (public safety) is seen as cover for the extractive function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, 0.85).
domain_priors:suppression_score(licensing_statute_mandate__graduated_access_filter, 0.92).
domain_priors:theater_ratio(licensing_statute_mandate__graduated_access_filter, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, extractiveness, 0.85).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__graduated_access_filter, snare).
narrative_ontology:human_readable(licensing_statute_mandate__graduated_access_filter, "Statutory Credential Requirements: Graduated Access Filter Reading").
narrative_ontology:topic_domain(licensing_statute_mandate__graduated_access_filter, "labor_economics/regulatory_policy/public_administration").

domain_priors:requires_active_enforcement(licensing_statute_mandate__graduated_access_filter).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__graduated_access_filter, '11bd072a-2a31-4bb6-a6d5-d2781013cc97').
narrative_ontology:cs_kernel_codification('11bd072a-2a31-4bb6-a6d5-d2781013cc97', formalized).
narrative_ontology:cs_authority_grounding('11bd072a-2a31-4bb6-a6d5-d2781013cc97', extraction).
narrative_ontology:cs_interpretation_layer_present('11bd072a-2a31-4bb6-a6d5-d2781013cc97').
narrative_ontology:cs_reading_relation('11bd072a-2a31-4bb6-a6d5-d2781013cc97', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('11bd072a-2a31-4bb6-a6d5-d2781013cc97', licensing_statute_mandate__rent_seeking_suppression, coexists_with).
narrative_ontology:cs_axiom('11bd072a-2a31-4bb6-a6d5-d2781013cc97', foundational, credentialing_creates_tiered_market_access).
narrative_ontology:cs_axiom_status(credentialing_creates_tiered_market_access, holdable).
narrative_ontology:cs_axiom_grounding('11bd072a-2a31-4bb6-a6d5-d2781013cc97', credentialing_creates_tiered_market_access, empirically_contingent).
narrative_ontology:cs_axiom('11bd072a-2a31-4bb6-a6d5-d2781013cc97', foundational, resource_access_determines_credential_acquisition).
narrative_ontology:cs_axiom_status(resource_access_determines_credential_acquisition, holdable).
narrative_ontology:cs_axiom_grounding('11bd072a-2a31-4bb6-a6d5-d2781013cc97', resource_access_determines_credential_acquisition, empirically_contingent).
narrative_ontology:cs_reference_frame('11bd072a-2a31-4bb6-a6d5-d2781013cc97', unfettered_labor_market).
narrative_ontology:cs_drift_state('11bd072a-2a31-4bb6-a6d5-d2781013cc97', contemporary_regulatory_state, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('11bd072a-2a31-4bb6-a6d5-d2781013cc97', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, credentialed_professionals).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, licensing_boards).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, marginalized_workers).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, new_entrants_without_resources).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, consumers_of_services).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from reduced competition and higher wages due to restricted labor supply. They advocate for maintaining or increasing credentialing requirements, framing them as essential for public safety and professional standards.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, credentialed_professionals, beneficiary,
    organized, biographical, mobile, national).

% Administer the credentialing process, collect fees, and enforce compliance. Their institutional mandate is to uphold professional standards, but their operations also serve to maintain the barriers to entry, benefiting the credentialed class.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, licensing_boards, agenda_setter,
    institutional, generational, constrained, national).

% Are excluded from higher-paying professions due to inability to meet costly and time-consuming credential requirements. They often have relevant skills but lack the financial resources or educational background to navigate the system, forcing them into lower-wage, less secure work.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, marginalized_workers, payer,
    powerless, immediate, trapped, local).

% Face significant upfront costs and time investments to acquire necessary credentials, creating a substantial barrier to market entry. They are forced to incur debt or delay career progression, disproportionately affecting those from lower socioeconomic backgrounds.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, new_entrants_without_resources, payer,
    moderate, biographical, constrained, regional).

% Pay higher prices for services due to reduced competition among credentialed professionals. While ostensibly protected by quality standards, they bear the economic cost of restricted supply.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, consumers_of_services, payer,
    moderate, immediate, constrained, local).

% Analyze the economic impact of licensing requirements, often highlighting the correlation between stringent licensing and reduced labor mobility, increased prices, and wealth inequality. Their findings challenge the public safety narrative.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, economic_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ostensibly coordinates minimum competence standards to protect public safety and ensure quality of service, providing a signal of reliability to consumers.
% TRANSFER_FUNCTION: Transfers economic rents from marginalized workers, new entrants, and consumers to incumbent credentialed professionals and the licensing boards that administer the system, by restricting labor supply and increasing barriers to entry.
% ABSENT_VOICES: Unlicensed but competent workers, particularly those from marginalized communities or with non-traditional training, are excluded from the formal labor market and lack a voice in setting credentialing standards. They would argue for alternative pathways to demonstrate competence and for reduced barriers.
% DISAPPEARANCE_RATIONALE: If statutory credential requirements vanished overnight, the labor market for many professions would immediately open, leading to increased competition, potentially lower service prices, and greater access for skilled but uncredentialed workers. The economic landscape would shift significantly, with incumbent professionals facing wage pressure and new entrants gaining opportunities.
% FOUNDING_PROBLEM: The original problem was to protect the public from incompetent or fraudulent practitioners in fields affecting health, safety, and welfare.
% FOUNDING_PROBLEM_CORROBORATION: Licensing boards and incumbent professionals assert the problem is still live, citing ongoing risks of consumer harm. Economic researchers and advocates for marginalized workers argue that while a minimal standard is valid, current requirements far exceed what is necessary for public safety and primarily serve to restrict competition; their analysis, based on cross-state comparisons and historical data, corroborates the shifted-function reading.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__graduated_access_filter, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__graduated_access_filter, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__graduated_access_filter, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(licensing_statute_mandate__graduated_access_filter, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__graduated_access_filter, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because the economic benefits to credentialed professionals (higher wages, reduced competition) are substantial, directly correlating with the costs borne by those excluded or forced to incur significant debt. Suppression (0.92) is severe due to the legal and financial barriers to entry, which are actively enforced by licensing boards and state statutes, effectively trapping marginalized workers in lower-tier jobs. The theater ratio is low (0.15) because, from this reading, the public safety function is largely a pretext, with most activity directly serving the extractive purpose. Accessibility collapse is high (0.78) as alternatives to credentialing for market access are severely limited or non-existent. Resistance (0.70) is significant, primarily from economic researchers, advocacy groups for workers' rights, and some political movements seeking deregulation or alternative credentialing pathways.
 *
 * PERSPECTIVAL GAP:
 *   Incumbent professionals and licensing boards perceive the constraint as a legitimate 'rope' or even a 'mountain' of necessary standards, ensuring quality and public trust. Marginalized workers and economic researchers, however, experience and analyze it as a 'snare' designed to restrict competition and extract rents. The engine's computation from the authored metrics and structural data will highlight this divergence from the claimed 'snare' type.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed professionals and licensing boards are clear beneficiaries (low d, potentially negative chi) as they gain economic rents and institutional power. Marginalized workers and new entrants without resources are direct victims (high d, high chi) as they face exclusion and economic hardship. Consumers are indirect payers (moderate d) through higher service costs. Economic researchers act as analytical observers (analytical d).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as pure coordination (a 'rope') by explicitly identifying the beneficiaries of the tiered access and the victims of the exclusion. It highlights that the persistence of the constraint is due to the active enforcement by those who benefit, rather than a genuine, unmet public safety need. The high extractiveness and suppression, coupled with the contested founding problem status, point to a mandate that has drifted from its original purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_safety_vs_economic_barrier,
    'What is the actual marginal contribution of current credentialing requirements to public safety, versus their effect as an economic barrier to entry?',
    'Empirical studies comparing public safety outcomes in jurisdictions with varying levels of credentialing stringency, controlling for other factors. Analysis of specific requirements to determine if they are genuinely competence-based or arbitrary.',
    'If the public safety contribution is minimal, the constraint''s justification as coordination collapses, strengthening its classification as a snare. If substantial, it would suggest a tangled rope or even a rope, requiring re-evaluation of extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_safety_vs_economic_barrier, empirical, 'Distinguishing genuine public safety function from economic gatekeeping.').

omega_variable(
    internalized_suppression_of_workers,
    'To what extent is the suppression experienced by marginalized workers internalized (e.g., belief in their own inadequacy, lack of awareness of alternatives) versus purely structural (legal barriers, financial costs)?',
    'Qualitative research and longitudinal studies tracking workers'' perceptions and behaviors after structural barriers are reduced or removed (e.g., through policy changes or alternative credentialing programs).',
    'If internalized suppression is significant, the effective suppression is higher than the structural measure suggests, as workers carry the barrier with them even if external conditions change. This would deepen the snare classification by highlighting the psychological dimension of extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_of_workers, empirical, 'Structural vs. internalized suppression mechanism for marginalized workers.').

omega_variable(
    framing_of_licensing_purpose,
    'Is the primary purpose of professional licensing to ensure public safety, or to manage labor supply and protect incumbent interests?',
    'Conceptual analysis of historical legislative intent, judicial interpretations, and the stated goals of professional associations versus the observed economic effects. This is a conceptual omega because it depends on which ''purpose'' is prioritized in the framing.',
    'Prioritizing public safety would shift the conceptual framing towards a coordination function, potentially reclassifying it as a tangled rope or rope. Prioritizing labor supply management reinforces the snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_licensing_purpose, conceptual, 'Conceptual framing of licensing''s core purpose.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__graduated_access_filter, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__graduated_access_filter, theater_ratio, 0, 0.3).
narrative_ontology:measurement(lice_tr_t10, licensing_statute_mandate__graduated_access_filter, theater_ratio, 10, 0.25).
narrative_ontology:measurement(lice_tr_t20, licensing_statute_mandate__graduated_access_filter, theater_ratio, 20, 0.2).
narrative_ontology:measurement(lice_tr_t30, licensing_statute_mandate__graduated_access_filter, theater_ratio, 30, 0.18).
narrative_ontology:measurement(lice_tr_t40, licensing_statute_mandate__graduated_access_filter, theater_ratio, 40, 0.16).
narrative_ontology:measurement(lice_tr_t50, licensing_statute_mandate__graduated_access_filter, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(lice_be_t10, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(lice_be_t20, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 20, 0.75).
narrative_ontology:measurement(lice_be_t30, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(lice_be_t40, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 40, 0.83).
narrative_ontology:measurement(lice_be_t50, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(lice_su_t10, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(lice_su_t20, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(lice_su_t30, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 30, 0.89).
narrative_ontology:measurement(lice_su_t40, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 40, 0.91).
narrative_ontology:measurement(lice_su_t50, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 50, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
