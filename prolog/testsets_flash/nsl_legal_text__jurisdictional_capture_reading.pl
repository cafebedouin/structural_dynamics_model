% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__jurisdictional_capture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__jurisdictional_capture_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nsl_legal_text__jurisdictional_capture_reading
 *   human_readable: National Security Law as Jurisdictional Capture
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This constraint story analyzes the National Security Law (NSL) in Hong
 *   Kong as a mechanism for jurisdictional capture, specifically focusing on
 *   its role in transplanting elements of the mainland legal system and
 *   eroding the autonomy of Hong Kong's common law. It is one reading of the
 *   broader 'nsl_legal_text' kernel, distinct from readings focused on
 *   democratic enclosure or sovereignty restoration. The metrics reflect a
 *   high degree of extraction of legal autonomy and significant suppression
 *   of independent legal institutions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, 0.78).
domain_priors:suppression_score(nsl_legal_text__jurisdictional_capture_reading, 0.85).
domain_priors:theater_ratio(nsl_legal_text__jurisdictional_capture_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__jurisdictional_capture_reading, snare).
narrative_ontology:human_readable(nsl_legal_text__jurisdictional_capture_reading, "National Security Law as Jurisdictional Capture").
narrative_ontology:topic_domain(nsl_legal_text__jurisdictional_capture_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__jurisdictional_capture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__jurisdictional_capture_reading, '420be44c-18de-44fa-be78-bf18c42a0b01').
narrative_ontology:cs_kernel_codification('420be44c-18de-44fa-be78-bf18c42a0b01', fixed_text).
narrative_ontology:cs_authority_grounding('420be44c-18de-44fa-be78-bf18c42a0b01', extraction).
narrative_ontology:cs_interpretation_layer_present('420be44c-18de-44fa-be78-bf18c42a0b01').
narrative_ontology:cs_reading_relation('420be44c-18de-44fa-be78-bf18c42a0b01', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('420be44c-18de-44fa-be78-bf18c42a0b01', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_axiom('420be44c-18de-44fa-be78-bf18c42a0b01', foundational, common_law_autonomy_is_subordinate).
narrative_ontology:cs_axiom_status(common_law_autonomy_is_subordinate, holdable).
narrative_ontology:cs_axiom_grounding('420be44c-18de-44fa-be78-bf18c42a0b01', common_law_autonomy_is_subordinate, conventional).
narrative_ontology:cs_axiom('420be44c-18de-44fa-be78-bf18c42a0b01', foundational, national_security_trumps_judicial_independence).
narrative_ontology:cs_axiom_status(national_security_trumps_judicial_independence, holdable).
narrative_ontology:cs_axiom_grounding('420be44c-18de-44fa-be78-bf18c42a0b01', national_security_trumps_judicial_independence, instrumental).
narrative_ontology:cs_reference_frame('420be44c-18de-44fa-be78-bf18c42a0b01', one_country_two_systems_as_transitional).
narrative_ontology:cs_drift_state('420be44c-18de-44fa-be78-bf18c42a0b01', post_nsl_enactment, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('420be44c-18de-44fa-be78-bf18c42a0b01', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, hong_kong_executive).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_civil_society).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directs the implementation of the NSL, establishing new security institutions in Hong Kong with broad powers. Benefits from the expansion of its jurisdiction and the ability to bypass Hong Kong's common law system in national security cases. Views the NSL as a necessary tool for stability and control.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains expanded powers to suppress dissent and enforce national security directives, aligning its legal framework more closely with mainland China. Benefits from increased political stability and reduced challenges to its authority, but operates under the ultimate direction of the mainland apparatus.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_executive, beneficiary,
    institutional, biographical, constrained, local).

% Experiences a direct erosion of its common law autonomy, with NSL cases often handled by specially designated judges and subject to mainland interpretation. Its independence is compromised, and its traditional role as a check on executive power is diminished. Exit is identity-locked due to professional commitment to common law principles.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary, payer,
    organized, generational, identity_locked, local).

% Faces new legal uncertainties, restrictions on free speech, and the risk of prosecution under the NSL. Its ability to defend clients in national security cases is severely curtailed, and the common law principles it upholds are undermined. Exit is identity-locked by professional ethics and commitment to rule of law.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession, payer,
    moderate, biographical, identity_locked, local).

% Suffers from the chilling effect of the NSL, leading to self-censorship, dissolution of organizations, and fear of arbitrary arrest. Its ability to advocate for human rights, democracy, and autonomy is severely restricted. Exit options are minimal, often involving emigration or direct confrontation with severe consequences.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_civil_society, payer,
    powerless, immediate, trapped, local).

% Monitor the implementation of the NSL and its impact on Hong Kong's legal system, issuing reports and condemnations. They provide an external analytical perspective but have limited direct power to alter the constraint's operation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, international_legal_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The NSL coordinates the legal and security frameworks of Hong Kong with that of mainland China, aiming to eliminate perceived 'gaps' in national security enforcement and ensure alignment with central government directives.
% TRANSFER_FUNCTION: Transfers legal and judicial autonomy from Hong Kong's common law system to the mainland's national security framework, effectively moving decision-making power and enforcement capacity to Beijing-aligned institutions.
% ABSENT_VOICES: Independent legal scholars, international human rights organizations, and former pro-democracy legislators are largely excluded from the official discourse. They would argue that the NSL fundamentally breaches the 'One Country, Two Systems' framework and undermines the rule of law in Hong Kong.
% DISAPPEARANCE_RATIONALE: If the NSL and its enforcement vanished overnight, Hong Kong's common law system would immediately reassert its full autonomy, the mainland security apparatus would lose its direct jurisdictional reach, and civil society would experience a rapid resurgence of activity and expression. The legal and political landscape of Hong Kong would fundamentally shift.
% FOUNDING_PROBLEM: The NSL was enacted to address perceived threats to national security in Hong Kong, particularly after the 2019 anti-government protests, which Beijing viewed as undermining national sovereignty and stability.
% FOUNDING_PROBLEM_CORROBORATION: The mainland government and the Hong Kong executive attest that the founding problem of national security threats is live and requires the NSL. However, international legal bodies, human rights organizations, and many former Hong Kong legal professionals (outside the benefiting parties) contend that the problem was exaggerated or has been used as a pretext for broader political control, and that the NSL's scope far exceeds any genuine security need.
narrative_ontology:disappearance_verdict(nsl_legal_text__jurisdictional_capture_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__jurisdictional_capture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__jurisdictional_capture_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nsl_legal_text__jurisdictional_capture_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.78) is high because the NSL fundamentally alters the legal landscape, transferring significant power from Hong Kong's independent judiciary to mainland-aligned security and legal bodies. Suppression (0.85) is also very high, as the law is actively enforced to prevent challenges to its authority and to silence dissent within the legal profession and civil society. The theater ratio (0.4) indicates that while some aspects of Hong Kong's legal system continue to operate, a substantial portion of the NSL's application is performative, demonstrating mainland control rather than addressing genuine, localized security threats through common law processes. The rising trend in all metrics reflects the progressive tightening of control since the NSL's enactment.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the mainland security apparatus, the NSL is a necessary and legitimate exercise of sovereign power to restore order and protect national interests. From the perspective of the Hong Kong judiciary and legal profession, it represents an illegitimate encroachment on established common law principles and judicial independence. This divergence is central to the constraint's operation, with the agenda-setter actively enforcing a narrative of necessity against a reality of jurisdictional capture.
 *
 * DIRECTIONALITY LOGIC:
 *   The mainland security apparatus and the Hong Kong executive are clear beneficiaries (d near 0.0-0.2), gaining expanded powers and control. The Hong Kong judiciary, legal profession, and civil society are the primary targets (d near 0.8-1.0), bearing the costs of eroded autonomy, restricted freedoms, and professional compromise. International legal observers maintain an analytical distance (d near 0.5).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    common_law_erosion_irreversibility,
    'To what extent is the erosion of Hong Kong''s common law autonomy under the NSL irreversible, or could it be restored if political conditions change?',
    'Analysis of legal precedents set, institutional changes made (e.g., establishment of new security bodies), and the extent of personnel changes within the judiciary. If fundamental structural changes are entrenched, reversibility is low.',
    'If irreversible, the constraint''s long-term extractiveness is higher, as the ''capture'' becomes permanent. If reversible, the constraint might be reclassified as a ''scaffold'' that overstayed its mandate, rather than a permanent ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(common_law_erosion_irreversibility, empirical, 'Assessing the permanence of common law erosion.').

omega_variable(
    legitimacy_narrative_vs_structural_impact,
    'Is the ''sovereignty restoration'' narrative (the ''sovereignty_restoration_reading'' sibling) genuinely believed by a significant portion of the Hong Kong population, or is it primarily a top-down imposition?',
    'Independent, anonymous public opinion surveys (if feasible), analysis of local media discourse, and observation of public behavior (e.g., participation in pro-government events vs. passive compliance).',
    'If the sovereignty narrative is widely accepted, the constraint''s effective suppression is lower, as it operates with more consent. If it''s a top-down imposition, the suppression is higher, relying more on coercion than legitimacy, reinforcing the ''snare'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_narrative_vs_structural_impact, empirical, 'The role of public belief in the NSL''s legitimacy.').

omega_variable(
    jurisdictional_capture_vs_democratic_enclosure,
    'Is the primary function of the NSL the capture of legal jurisdiction (this reading) or the permanent enclosure of democratic space (''democratic_enclosure_reading'' sibling)?',
    'Comparative analysis of NSL cases: proportion of cases targeting legal institutions/autonomy vs. those targeting political dissent/activism. If legal institutions are disproportionately affected, jurisdictional capture is primary.',
    'If jurisdictional capture is primary, the constraint is a ''snare'' extracting institutional independence. If democratic enclosure is primary, it''s a ''snare'' extracting political freedoms, potentially with different victim sets and long-term implications for civil society.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(jurisdictional_capture_vs_democratic_enclosure, conceptual, 'Distinguishing primary function: legal capture vs. political enclosure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__jurisdictional_capture_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nsl__tr_t1, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 1, 0.25).
narrative_ontology:measurement(nsl__tr_t2, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2, 0.3).
narrative_ontology:measurement(nsl__tr_t3, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 3, 0.35).
narrative_ontology:measurement(nsl__tr_t4, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 4, 0.4).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(nsl__be_t1, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 1, 0.7).
narrative_ontology:measurement(nsl__be_t2, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2, 0.74).
narrative_ontology:measurement(nsl__be_t3, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 3, 0.76).
narrative_ontology:measurement(nsl__be_t4, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 4, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(nsl__su_t1, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 1, 0.75).
narrative_ontology:measurement(nsl__su_t2, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2, 0.8).
narrative_ontology:measurement(nsl__su_t3, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 3, 0.83).
narrative_ontology:measurement(nsl__su_t4, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 4, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
