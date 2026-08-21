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
 *   human_readable: National Security Law as Democratic Enclosure
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This constraint story analyzes the Hong Kong National Security Law (NSL)
 *   from the 'democratic enclosure' reading. In this reading, the NSL is
 *   understood as a mechanism designed to permanently close down democratic
 *   space, criminalize dissent, and dismantle civil society in Hong Kong. It
 *   is a snare, characterized by very high extraction and suppression,
 *   actively enforced by the Beijing central government and the Hong Kong
 *   establishment, with identifiable victims among civil society, independent
 *   media, and pro-democracy politicians. The claimed type is 'snare' because
 *   the coordination story (national security) is seen as a cover for pure
 *   extraction of political autonomy and suppression of opposition.
 *
 * KEY AGENTS:
 *   - beijing_central_government: Primary agenda-setter (institutional/arbitrage) — benefits from consolidation of power.
 *   - hong_kong_establishment: Secondary beneficiary (institutional/constrained) — administers the law locally.
 *   - hong_kong_civil_society: Primary target (powerless/trapped) — faces criminalization and suppression.
 *   - independent_media: Target (powerless/trapped) — subject to censorship and closure.
 *   - pro_democracy_politicians: Target (powerless/trapped) — disqualified, arrested, or imprisoned.
 *   - human_rights_lawyers: Target (moderate/constrained) — face pressure and surveillance.
 *   - international_human_rights_organizations: Analytical observer (organized/analytical) — monitors and advocates.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, 0.92).
domain_priors:suppression_score(nsl_legal_text__democratic_enclosure_reading, 0.95).
domain_priors:theater_ratio(nsl_legal_text__democratic_enclosure_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__democratic_enclosure_reading, snare).
narrative_ontology:human_readable(nsl_legal_text__democratic_enclosure_reading, "National Security Law as Democratic Enclosure").
narrative_ontology:topic_domain(nsl_legal_text__democratic_enclosure_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__democratic_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__democratic_enclosure_reading, '70909102-b350-4b97-ab9a-823f32ac8d6f').
narrative_ontology:cs_kernel_codification('70909102-b350-4b97-ab9a-823f32ac8d6f', formalized).
narrative_ontology:cs_authority_grounding('70909102-b350-4b97-ab9a-823f32ac8d6f', extraction).
narrative_ontology:cs_interpretation_layer_present('70909102-b350-4b97-ab9a-823f32ac8d6f').
narrative_ontology:cs_reading_relation('70909102-b350-4b97-ab9a-823f32ac8d6f', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('70909102-b350-4b97-ab9a-823f32ac8d6f', nsl_legal_text__jurisdictional_capture_reading, influences).
narrative_ontology:cs_axiom('70909102-b350-4b97-ab9a-823f32ac8d6f', foundational, democratic_expression_is_national_security_threat).
narrative_ontology:cs_axiom_status(democratic_expression_is_national_security_threat, holdable).
narrative_ontology:cs_axiom_grounding('70909102-b350-4b97-ab9a-823f32ac8d6f', democratic_expression_is_national_security_threat, conventional).
narrative_ontology:cs_axiom('70909102-b350-4b97-ab9a-823f32ac8d6f', foundational, autonomy_is_subordinate_to_central_authority).
narrative_ontology:cs_axiom_status(autonomy_is_subordinate_to_central_authority, holdable).
narrative_ontology:cs_axiom_grounding('70909102-b350-4b97-ab9a-823f32ac8d6f', autonomy_is_subordinate_to_central_authority, conventional).
narrative_ontology:cs_reference_frame('70909102-b350-4b97-ab9a-823f32ac8d6f', one_country_two_systems_as_subordinate_autonomy).
narrative_ontology:cs_drift_state('70909102-b350-4b97-ab9a-823f32ac8d6f', post_2019_protests_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('70909102-b350-4b97-ab9a-823f32ac8d6f', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, beijing_central_government).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, hong_kong_establishment).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, hong_kong_civil_society).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, independent_media).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, pro_democracy_politicians).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, human_rights_lawyers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary architect and enforcer of the NSL, using it to assert control over Hong Kong's political landscape and eliminate perceived threats to national security. Benefits from the consolidation of power and suppression of dissent.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, beijing_central_government, agenda_setter,
    institutional, generational, arbitrage, global).

% Local political and business elites who align with Beijing, benefiting from the stability and reduced opposition that the NSL provides. They administer the law locally, often with direct guidance from Beijing.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hong_kong_establishment, beneficiary,
    institutional, biographical, constrained, national).

% Organizations and individuals advocating for democratic values, human rights, and autonomy. They face severe restrictions on assembly, speech, and association, with many leaders arrested or forced into exile. Their activities are criminalized under the NSL.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hong_kong_civil_society, payer,
    powerless, immediate, trapped, local).

% Journalists and media outlets that previously provided critical reporting. They are targeted by the NSL, leading to self-censorship, arrests, and the closure of independent news organizations. Their ability to report freely is severely curtailed.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, independent_media, payer,
    powerless, immediate, trapped, local).

% Elected officials and activists who previously represented the democratic opposition. Many have been disqualified, arrested, or imprisoned under the NSL, effectively dismantling the political opposition within the legislative council.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, pro_democracy_politicians, payer,
    powerless, immediate, trapped, local).

% Legal professionals who defend those accused under the NSL or challenge its application. They face increasing pressure, surveillance, and potential disbarment, making it difficult to provide independent legal representation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, human_rights_lawyers, payer,
    moderate, biographical, constrained, local).

% Monitor the implementation of the NSL and document human rights abuses. They issue reports and advocate for international pressure, but have no direct enforcement power over the constraint.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, international_human_rights_organizations, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the perspective of the Beijing and Hong Kong establishment, the NSL coordinates the suppression of perceived secessionist, subversive, terrorist, and collusive activities, ensuring political stability and national security.
% TRANSFER_FUNCTION: Transfers political power and control over public discourse from Hong Kong's civil society and democratic institutions to the Beijing central government and its aligned Hong Kong establishment. It also transfers the cost of dissent (imprisonment, exile) to individuals and organizations.
% ABSENT_VOICES: The voices of exiled pro-democracy activists, international legal bodies, and independent human rights experts are largely absent from the official discourse within Hong Kong and mainland China. They would argue that the NSL is a tool of political repression, not security.
% DISAPPEARANCE_RATIONALE: If the NSL and its enforcement vanished overnight, Hong Kong's democratic space would immediately begin to reopen. Civil society organizations would re-emerge, independent media would resume critical reporting, and political opposition would re-mobilize. The political landscape would fundamentally shift.
% FOUNDING_PROBLEM: The NSL was ostensibly enacted to address the perceived threats to national security arising from the 2019 anti-government protests, which Beijing characterized as secessionist and subversive activities threatening China's sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: The Beijing central government and Hong Kong establishment maintain that the founding problem of national security threats remains live. However, international legal experts, human rights organizations, and exiled Hong Kong activists widely corroborate that the immediate threats from 2019 have been neutralized, and the NSL now serves to permanently close democratic space rather than address an ongoing security crisis.
narrative_ontology:disappearance_verdict(nsl_legal_text__democratic_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__democratic_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__democratic_enclosure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nsl_legal_text__democratic_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__democratic_enclosure_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.92) is extremely high because the NSL effectively removes fundamental freedoms and political rights from a significant portion of Hong Kong's population, transferring political control to Beijing. Suppression (0.95) is also very high, as the law is enforced through arrests, prosecutions, and the dismantling of institutions, with severe penalties for perceived violations. The theater ratio (0.15) is low because while 'national security' is the stated justification, the actual enforcement is directly aimed at eliminating political opposition, making the performative aspect minimal compared to the direct coercive function. Accessibility collapse (0.88) is high because alternatives for expressing dissent or participating in democratic processes have been systematically removed. Resistance (0.75) remains high, despite severe penalties, indicating ongoing, albeit suppressed, opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Beijing central government and the Hong Kong establishment, the NSL is a legitimate and necessary measure to restore order and protect national security. From the perspective of Hong Kong civil society and international observers, it is a tool of political repression that fundamentally alters Hong Kong's autonomy and democratic freedoms. The engine's classification will reflect this divergence, showing a snare for the victims and a perceived rope/scaffold for the beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   The Beijing central government and the Hong Kong establishment are clear beneficiaries, as the law consolidates their power and eliminates opposition. Hong Kong civil society, independent media, pro-democracy politicians, and human rights lawyers are direct victims, bearing the full cost of the law's enforcement through loss of freedom, livelihood, and political space. International human rights organizations act as observers, documenting the impact without direct involvement in the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading classifies the NSL as a snare, preventing its mislabeling as a legitimate security 'rope' or 'scaffold' by highlighting its high extraction and suppression of democratic space. The analysis focuses on the actual effects on civil society and political freedom, rather than accepting the stated national security mandate at face value. The high extractiveness and suppression, coupled with the dismantling of opposition, indicate that the constraint's primary function is not coordination but coercive control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nsl_true_intent,
    'Is the primary intent of the NSL genuinely national security, or is it political control and suppression of dissent?',
    'Analysis of enforcement patterns, judicial interpretations, and legislative history, particularly focusing on cases where ''national security'' charges are applied to non-violent political expression.',
    'If the primary intent is political control, the classification as a snare is strongly reinforced. If genuine national security threats are demonstrably the sole driver, the constraint might lean towards a tangled_rope or even a scaffold (if temporary and proportional).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nsl_true_intent, conceptual, 'Ambiguity regarding the true purpose of the National Security Law.').

omega_variable(
    international_response_efficacy,
    'To what extent can international pressure or sanctions alter the enforcement or scope of the NSL?',
    'Empirical observation of the impact of international diplomatic actions, sanctions, and legal challenges on Beijing''s and Hong Kong''s policy decisions regarding the NSL.',
    'If international pressure proves effective, it could introduce external constraints on the NSL''s enforcement, potentially reducing its effective suppression and extractiveness. If ineffective, the current high suppression and extractiveness are further entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_response_efficacy, empirical, 'Uncertainty about the efficacy of external interventions on the NSL''s operation.').

omega_variable(
    internalized_suppression_mechanism,
    'Is the measured suppression primarily structural (legal barriers, arrests) or internalized (self-censorship, fear) within Hong Kong civil society?',
    'Post-NSL enforcement trajectory: if suppression persists even after some legal mechanisms are removed or softened, it suggests a significant internalized component. Surveys and qualitative studies of public behavior and media practices.',
    'If internalized suppression is a major factor, the constraint''s effective suppression is higher than the structural measure suggests, as the targets carry the suppression with them. This would make the snare more resilient to external changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_mechanism, empirical, 'Structural vs. internalized suppression mechanism in Hong Kong.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__democratic_enclosure_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nsl__tr_t1, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 1, 0.18).
narrative_ontology:measurement(nsl__tr_t2, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 2, 0.16).
narrative_ontology:measurement(nsl__tr_t3, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 3, 0.15).
narrative_ontology:measurement(nsl__tr_t4, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 4, 0.15).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(nsl__be_t1, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 1, 0.88).
narrative_ontology:measurement(nsl__be_t2, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 2, 0.9).
narrative_ontology:measurement(nsl__be_t3, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 3, 0.91).
narrative_ontology:measurement(nsl__be_t4, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 4, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(nsl__su_t1, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 1, 0.91).
narrative_ontology:measurement(nsl__su_t2, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 2, 0.93).
narrative_ontology:measurement(nsl__su_t3, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 3, 0.94).
narrative_ontology:measurement(nsl__su_t4, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 4, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__democratic_enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, jurisdictional_capture_reading).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, sovereignty_restoration_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'nsl_legal_text' kernel. This 'democratic_enclosure_reading' focuses on the NSL's impact on civil liberties and political space, while 'jurisdictional_capture_reading' examines its effect on legal autonomy, and 'sovereignty_restoration_reading' presents the official justification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
