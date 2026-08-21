% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__democratic_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__democratic_enclosure_reading, []).

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
 *   constraint_id: nsl_legal_text__democratic_enclosure_reading
 *   human_readable: Hong Kong National Security Law: Democratic Enclosure Reading
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This constraint story analyzes the Hong Kong National Security Law (NSL)
 *   as a mechanism for the permanent closure of democratic space and the
 *   criminalization of dissent. It is one reading of the 'nsl_legal_text'
 *   kernel, focusing on the structural impact on civil liberties and
 *   political freedoms. The law, enacted by Beijing, has been used to target
 *   pro-democracy activists, independent media, and civil society
 *   organizations, fundamentally altering Hong Kong's political landscape.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, 0.9).
domain_priors:suppression_score(nsl_legal_text__democratic_enclosure_reading, 0.95).
domain_priors:theater_ratio(nsl_legal_text__democratic_enclosure_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__democratic_enclosure_reading, snare).
narrative_ontology:human_readable(nsl_legal_text__democratic_enclosure_reading, "Hong Kong National Security Law: Democratic Enclosure Reading").
narrative_ontology:topic_domain(nsl_legal_text__democratic_enclosure_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__democratic_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__democratic_enclosure_reading, 'f397205c-525a-4779-8faa-926a7c93560f').
narrative_ontology:cs_kernel_codification('f397205c-525a-4779-8faa-926a7c93560f', formalized).
narrative_ontology:cs_authority_grounding('f397205c-525a-4779-8faa-926a7c93560f', extraction).
narrative_ontology:cs_interpretation_layer_present('f397205c-525a-4779-8faa-926a7c93560f').
narrative_ontology:cs_reading_relation('f397205c-525a-4779-8faa-926a7c93560f', nsl_legal_text__sovereignty_restoration_reading, forecloses).
narrative_ontology:cs_reading_relation('f397205c-525a-4779-8faa-926a7c93560f', nsl_legal_text__jurisdictional_capture_reading, influences).
narrative_ontology:cs_axiom('f397205c-525a-4779-8faa-926a7c93560f', foundational, democratic_participation_as_right).
narrative_ontology:cs_axiom_status(democratic_participation_as_right, holdable).
narrative_ontology:cs_axiom_grounding('f397205c-525a-4779-8faa-926a7c93560f', democratic_participation_as_right, deontological).
narrative_ontology:cs_axiom('f397205c-525a-4779-8faa-926a7c93560f', foundational, dissent_as_legitimate_expression).
narrative_ontology:cs_axiom_status(dissent_as_legitimate_expression, holdable).
narrative_ontology:cs_axiom_grounding('f397205c-525a-4779-8faa-926a7c93560f', dissent_as_legitimate_expression, deontological).
narrative_ontology:cs_reference_frame('f397205c-525a-4779-8faa-926a7c93560f', hong_kong_basic_law_autonomy).
narrative_ontology:cs_drift_state('f397205c-525a-4779-8faa-926a7c93560f', post_nsl_implementation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f397205c-525a-4779-8faa-926a7c93560f', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, beijing_central_government).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, hong_kong_establishment).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, hong_kong_civil_society).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, pro_democracy_activists).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, independent_media).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, international_business_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary author and ultimate enforcer of the NSL, benefiting from increased political control over Hong Kong and the suppression of perceived threats to its authority. It frames the law as essential for national security and stability.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, beijing_central_government, agenda_setter,
    institutional, civilizational, arbitrage, global).

% The local government and aligned elites in Hong Kong who implement and enforce the NSL. They benefit from alignment with Beijing and the perceived stability that comes from suppressing dissent, but operate within the framework set by the central government.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hong_kong_establishment, agenda_setter,
    institutional, generational, constrained, national).

% The broad range of non-governmental organizations, community groups, and ordinary citizens who previously engaged in democratic participation. They bear the direct cost of shrinking civic space, fear of criminalization, and loss of fundamental freedoms.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hong_kong_civil_society, payer,
    powerless, biographical, trapped, local).

% Individuals who actively advocated for democratic reforms. They are direct targets of criminalization under the NSL, facing arrest, prosecution, and lengthy prison sentences for actions previously considered legitimate political expression.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, pro_democracy_activists, payer,
    powerless, immediate, trapped, local).

% Journalists and media outlets that provided critical reporting on the government. They face severe pressure through censorship, legal threats, and self-censorship, leading to a significant reduction in independent news coverage and public discourse.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, independent_media, payer,
    moderate, biographical, constrained, local).

% Organizations and individuals globally who monitor and report on human rights. They document the erosion of rights in Hong Kong and advocate for international pressure, but have limited direct power to alter the constraint's operation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, international_human_rights_advocates, observer,
    organized, generational, analytical, global).

% Multinational corporations and investors who prioritize stability and predictability. They benefit from the perceived reduction in political unrest and the enforcement of order, but face reputational risks and potential challenges to rule of law.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, international_business_community, beneficiary,
    powerful, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate national security efforts and prevent secession, subversion, terrorism, and collusion with foreign forces in Hong Kong, thereby ensuring political stability and control.
% TRANSFER_FUNCTION: Transfers political power, autonomy, and democratic freedoms from Hong Kong's civil society and institutions to the Beijing central government and its aligned local establishment, in exchange for perceived stability and control.
% ABSENT_VOICES: The voices of the Hong Kong public, particularly those who participated in pro-democracy movements, are actively suppressed and criminalized. Their perspectives are excluded from official discourse and legal processes, leading to a chilling effect on public expression.
% DISAPPEARANCE_RATIONALE: If the NSL vanished overnight, Hong Kong's political landscape would immediately shift. Pro-democracy movements would re-emerge, independent media would resume critical reporting, and the legal system would face pressure to reverse previous convictions. The relationship between Hong Kong and Beijing would undergo a profound re-evaluation, likely leading to renewed demands for autonomy.
% FOUNDING_PROBLEM: The perceived threat to national security and sovereignty posed by large-scale pro-democracy protests and perceived foreign interference in Hong Kong in 2019.
% FOUNDING_PROBLEM_CORROBORATION: The Beijing and Hong Kong governments assert the problem is still live and the law is essential for stability. International legal bodies, human rights organizations, and many former Hong Kong residents attest that the original problem was a pretext for political control, and the law has created new, severe problems of human rights and autonomy, supported by legislative-hearing testimony and independent analysis.
narrative_ontology:disappearance_verdict(nsl_legal_text__democratic_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__democratic_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__democratic_enclosure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(nsl_legal_text__democratic_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__democratic_enclosure_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__democratic_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__democratic_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.90) reflects the severe cost borne by Hong Kong's democratic infrastructure and civil society, as political freedoms and autonomy are systematically removed. Suppression (0.95) is extremely high due to the broad scope of the law, its retroactive application, and the active criminalization of previously legal activities, leading to a pervasive chilling effect. The low theater ratio (0.10) indicates that the enforcement is genuinely aimed at achieving its stated (and unstated) goals, rather than being merely performative. Resistance is moderate-low (0.40) as initial widespread protests were met with overwhelming state power, leading to a significant reduction in overt dissent.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Beijing and Hong Kong governments, the NSL is a legitimate and necessary instrument for restoring order and national security. From the perspective of Hong Kong civil society, pro-democracy activists, and international human rights advocates, it is a tool for political repression and the dismantling of democratic freedoms. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Beijing central government and the Hong Kong establishment are clear beneficiaries and agenda-setters, gaining control and stability. Hong Kong civil society, pro-democracy activists, and independent media are direct targets and payers, bearing the costs of lost freedoms and criminalization. The international business community is a beneficiary of perceived stability, though with potential reputational risks. International human rights advocates act as observers, documenting the impact.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nsl_kernel_reading_identification,
    'Is this constraint primarily a legitimate exercise of sovereign power to restore order, or a tool for suppressing fundamental rights and democratic space?',
    'Comparative analysis of similar national security laws in other jurisdictions, assessment of proportionality of measures to stated threats, and independent evaluation of human rights impacts.',
    'If primarily a legitimate exercise of sovereign power, the constraint would be reclassified closer to a Rope or even Mountain (from the sovereign''s perspective). If primarily a tool for suppression, its Snare classification is reinforced, highlighting the extractive nature of the ''security'' narrative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nsl_kernel_reading_identification, conceptual, 'Distinguishing the ''democratic_enclosure_reading'' from the ''sovereignty_restoration_reading'' of the NSL kernel.').

omega_variable(
    legal_erosion_vs_political_control,
    'Is the primary impact of the NSL the erosion of Hong Kong''s common law system, or the broader political enclosure of democratic space?',
    'Detailed legal analysis of judicial decisions and legislative changes versus sociological studies of civic participation, media freedom, and political expression.',
    'If legal erosion is primary, the ''jurisdictional_capture_reading'' would gain prominence. If political enclosure is primary, this ''democratic_enclosure_reading'' is reinforced. Both can coexist as distinct but related analyses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_erosion_vs_political_control, empirical, 'Distinguishing the ''democratic_enclosure_reading'' from the ''jurisdictional_capture_reading'' of the NSL kernel.').

omega_variable(
    internalized_suppression_extent,
    'To what extent has the NSL''s suppression led to internalized self-censorship and political apathy among the Hong Kong population, beyond overt enforcement?',
    'Longitudinal sociological surveys, qualitative interviews, and analysis of cultural production (e.g., art, literature) for shifts in expression and political engagement over time.',
    'If internalized suppression is substantial, the effective suppression of the constraint is higher than structural measures alone suggest, as individuals carry the suppression with them even in the absence of direct external threats. This would amplify the constraint''s overall extractive impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_extent, empirical, 'Assessing the degree of internalized suppression and its impact on democratic space.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__democratic_enclosure_reading, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(nsl__tr_t1, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 1, 0.11).
narrative_ontology:measurement(nsl__tr_t2, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2, 0.1).
narrative_ontology:measurement(nsl__tr_t3, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 3, 0.1).
narrative_ontology:measurement(nsl__tr_t4, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 4, 0.1).
narrative_ontology:measurement(nsl__tr_t5, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 5, 0.1).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(nsl__be_t1, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 1, 0.87).
narrative_ontology:measurement(nsl__be_t2, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2, 0.88).
narrative_ontology:measurement(nsl__be_t3, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 3, 0.89).
narrative_ontology:measurement(nsl__be_t4, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 4, 0.9).
narrative_ontology:measurement(nsl__be_t5, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 5, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(nsl__su_t1, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 1, 0.92).
narrative_ontology:measurement(nsl__su_t2, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2, 0.93).
narrative_ontology:measurement(nsl__su_t3, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 3, 0.94).
narrative_ontology:measurement(nsl__su_t4, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 4, 0.95).
narrative_ontology:measurement(nsl__su_t5, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 5, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__democratic_enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, hong_kong_common_law_autonomy).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, international_human_rights_norms).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, freedom_of_press_hong_kong).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Hong Kong National Security Law (nsl_legal_text), each focusing on a different structural impact. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
