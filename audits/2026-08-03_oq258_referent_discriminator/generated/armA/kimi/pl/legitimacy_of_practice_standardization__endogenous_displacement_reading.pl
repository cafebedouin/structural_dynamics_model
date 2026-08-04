% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__endogenous_displacement_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Endogenous Displacement Reading of Practice Standardization Legitimacy
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This constraint captures the endogenous displacement reading of how
 *   practice standardization (calendar, dress, language, measurement) gains
 *   legitimacy during state-led modernization. The reading claims that change
 *   is legitimate when it emerges from voluntary adoption driven by perceived
 *   utility or cultural evolution. In historical cases from Ottoman/Turkish
 *   modernization to Meiji Japan and beyond, this narrative served as a
 *   soft-power tool allowing state elites to present top-down reforms as
 *   organic social development. The constraint is a contested kernel reading:
 *   it competes with exogenous override (state decree) and dual-practice
 *   equilibrium (domain partition) readings. The structural reality is that
 *   'voluntary adoption' was often manufactured through control of schools,
 *   administrative procedure, and economic incentive, making the endogenous
 *   frame a coordination mechanism that simultaneously extracted compliance
 *   from traditional populations.
 *
 * KEY AGENTS:
 *   - Modernizing state elites: agenda-setter with institutional power, constrained by their own modernization ideology
 *   - Urban commercial classes: beneficiary with mobile exit, gains from standardization
 *   - Cultural brokers: beneficiary with constrained exit, propagates the voluntariness narrative
 *   - Traditional religious authorities: payer with identity-locked exit, loses authority to endogenous legitimation
 *   - Rural communities: payer with trapped exit, adopts under structural pressure recorded as voluntary choice
 *   - Postcolonial historians: analytical observer documenting the coercion gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.48).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.52).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Endogenous Displacement Reading of Practice Standardization Legitimacy").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political_history/modernization_studies/institutional_change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__endogenous_displacement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, '5be7a9b9-3db3-4d80-9e3d-62d677ddbd7d').
narrative_ontology:cs_kernel_codification('5be7a9b9-3db3-4d80-9e3d-62d677ddbd7d', formalized).
narrative_ontology:cs_authority_grounding('5be7a9b9-3db3-4d80-9e3d-62d677ddbd7d', practice).
narrative_ontology:cs_interpretation_layer_present('5be7a9b9-3db3-4d80-9e3d-62d677ddbd7d').
narrative_ontology:cs_reading_relation('5be7a9b9-3db3-4d80-9e3d-62d677ddbd7d', legitimacy_of_practice_standardization__exogenous_override_reading, influences).
narrative_ontology:cs_reading_relation('5be7a9b9-3db3-4d80-9e3d-62d677ddbd7d', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('5be7a9b9-3db3-4d80-9e3d-62d677ddbd7d', foundational, legitimacy_from_voluntary_adoption).
narrative_ontology:cs_axiom_status(legitimacy_from_voluntary_adoption, holdable).
narrative_ontology:cs_axiom_grounding('5be7a9b9-3db3-4d80-9e3d-62d677ddbd7d', legitimacy_from_voluntary_adoption, conventional).
narrative_ontology:cs_axiom('5be7a9b9-3db3-4d80-9e3d-62d677ddbd7d', foundational, cultural_evolution_directs_progress).
narrative_ontology:cs_axiom_status(cultural_evolution_directs_progress, holdable).
narrative_ontology:cs_axiom_grounding('5be7a9b9-3db3-4d80-9e3d-62d677ddbd7d', cultural_evolution_directs_progress, empirically_contingent).
narrative_ontology:cs_reference_frame('5be7a9b9-3db3-4d80-9e3d-62d677ddbd7d', organic_modernization_framework).
narrative_ontology:cs_drift_state('5be7a9b9-3db3-4d80-9e3d-62d677ddbd7d', post_empirical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5be7a9b9-3db3-4d80-9e3d-62d677ddbd7d', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernizing_state_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, urban_commercial_classes).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_brokers).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_religious_authorities).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rural_communities).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernization_theory).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, social_evolutionism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and promote the narrative that practice change is legitimate only when it emerges from voluntary adoption. Control education, media, and state institutions to demonstrate 'perceived utility' of reforms such as calendar, dress, and language standardization. Benefit from the legitimacy conferred by the endogenous frame, which allows them to avoid the political costs of overt coercion while still driving uniform national modernization.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernizing_state_elites, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from standardized practices that reduce transaction costs and facilitate commerce across regions. They adopt new practices earlier and gain social status by aligning with the modernizing narrative. Their mobility allows them to operate within modernized institutional spaces, though they remain dependent on the state's continued enforcement of uniform standards.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, urban_commercial_classes, beneficiary,
    powerful, biographical, mobile, national).

% Intellectuals, educators, and journalists who translate modernization theory into local idioms of progress and utility. They propagate the frame that adoption is voluntary and culturally evolved, gaining professional status and institutional support from the state. Their careers are bound to the modernization project, making exit costly.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_brokers, beneficiary,
    moderate, biographical, constrained, national).

% Lose authority as traditional practices are delegitimized through the endogenous narrative, which recasts their domains as backward rather than as legitimate alternatives. They face social and institutional pressure to endorse or silently accept reforms. Their identity is fused with the traditional practices being displaced, making exit equivalent to self-dissolution.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_religious_authorities, payer,
    organized, generational, identity_locked, regional).

% Experience practice changes in dress, calendar, and language as practical necessities tied to market access and legal recognition, rather than as freely adopted utilities. Their adoption is often coerced by economic dependency and administrative requirements, yet is publicly recorded as voluntary cultural evolution. Geographically and economically isolated, they cannot easily access alternative institutional arrangements.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, rural_communities, payer,
    powerless, generational, trapped, regional).

% Document the gap between the 'voluntary adoption' narrative and the historical record of state imposition, social engineering, and economic coercion. They observe the constraint from outside the modernization project's benefit structure and provide empirical counter-narratives to the endogenous framing.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, postcolonial_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernizing_state_elites).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared legitimacy standard for institutional practice change that reduces the need for overt coercion by allowing reforms to be presented as emerging from popular utility and cultural evolution rather than state imposition.
% TRANSFER_FUNCTION: Moves authority and cultural capital from traditional religious and rural institutions to modernizing state elites and urban commercial classes, by delegitimizing top-down imposition while channeling compliance through the narrative of spontaneous adoption.
% ABSENT_VOICES: Traditional practitioners who experienced reform as economically and administratively imposed; rural communities whose adoption was driven by necessity and stigma rather than perceived utility; postcolonial historians who document the coercive underside of 'voluntary' modernization but are excluded from official historiography.
% DISAPPEARANCE_RATIONALE: If the endogenous legitimacy standard vanished, modernizing elites would lose the primary tool for legitimizing practice change without overt state violence. Reforms would require explicit coercion (shifting to exogenous override) or face persistent organized resistance. The boundary between modern and traditional domains would harden, and the pace of standardization would slow dramatically as compliance would need to be purchased or forced rather than narratively absorbed.
% FOUNDING_PROBLEM: How to transform institutional practices (calendar, dress, measurement, language) in traditional societies undergoing modernization without triggering permanent social fracture, violent rebellion, or persistent dual-institution inefficiency.
% FOUNDING_PROBLEM_CORROBORATION: Modernizing elites and nationalist historians attest the problem was urgent social backwardness requiring rapid standardization. Postcolonial historians and anthropologists from outside the benefiting parties attest that the 'problem' was constructed by modernizers and that traditional practices were functionally coherent. Traditional religious authorities dispute the characterization of pre-reform society as deficient. No outside corroboration of the urgency unfiltered by modernization ideology exists.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) reflects moderate but real ideological extraction: the endogenous frame harvests compliance from traditional actors by recasting structural pressure as free choice. Suppression (0.52) is driven by the delegitimization of traditional alternatives through backwardness stigma and educational narratives. Theater ratio (0.42) captures the significant performative labor required to maintain the appearance of voluntariness. Accessibility collapse (0.58) reflects how traditional practices become socially and economically inaccessible. Resistance (0.48) accounts for persistent traditional opposition that is publicly framed as temporary friction. Temporal measurements show a rise-then-fall pattern: extraction and suppression peak during intensive reform periods (t=30-50) as the voluntariness narrative is most actively enforced, then modestly decline as practices become routinized and genuine intergenerational habituation partially replaces manufactured consent.
 *
 * PERSPECTIVAL GAP:
 *   From the modernizing elite seat, the constraint appears as a rope: it coordinates society around progressive standards without the brutality of decree. From the traditional authority and rural community seats, it appears as extraction: the same structural pressures are renamed 'choice,' and their resistance is dismissed as transitional friction. The urban commercial seat sees moderate benefit with manageable cost. The engine computes this divergence from the structural data: agenda-setters with constrained exit experience low directionality, while identity-locked and trapped targets experience high directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Modernizing state elites are structural beneficiaries (d near 0.0): they collect political legitimacy and compliance without bearing the costs of overt coercion. Urban commercial classes and cultural brokers are secondary beneficiaries (d low-moderate). Traditional religious authorities and rural communities are targets (d near 1.0): they pay in cultural erasure, authority loss, and compliance costs, with severely limited exit options that amplify effective extraction. Postcolonial historians occupy the analytical seat with neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents the false summit of treating this as a pure coordination norm (rope) while also preventing the false snare classification that would ignore the genuine reduction in overt violence that the legitimacy standard sometimes provided. The rope function is real: in some cases, framing reform as endogenous did reduce bloodshed relative to abrupt imposition. However, the asymmetric extraction is equally real: the same frame allowed elites to bypass negotiation and compensation. The mandate has not fully atrophied because the narrative continues to serve nationalist historiography, though its founding problem (urgent backwardness) is contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_adoption_authenticity,
    'Is the ''voluntary adoption'' observed in historical modernization genuinely bottom-up, or manufactured through elite control of education, media, and economic incentives?',
    'Archival and oral-history research comparing official adoption curves with local administrative records, petitions, and economic coercion documentation.',
    'If manufactured, the constraint''s extractiveness is higher than the surface narrative suggests and its coordination function is largely theatrical; if genuine, the extraction metric should be revised downward toward pure rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_adoption_authenticity, empirical, 'Whether voluntary adoption is authentic or constructed').

omega_variable(
    coordination_extraction_boundary,
    'Does the endogenous legitimacy standard primarily coordinate collective expectations to reduce violent conflict, or primarily extract compliance by disguising imposition as choice?',
    'Comparative analysis across modernization cases: where the standard was used, did violent conflict decrease relative to cases of explicit exogenous imposition, holding other factors constant?',
    'A genuine coordination function would sustain tangled_rope classification; absence of coordination would shift classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of traditional alternatives structural (legal and economic barriers) or internalized (stigma, shame, backwardness narratives)?',
    'Post-reform ethnographic and interview data measuring persistence of traditional practice in private domains after structural barriers are removed.',
    'If internalized, effective suppression exceeds the structural measure because targets carry the constraint with them; this would raise the effective extraction for identity-locked victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(endog_disp_tr_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(endog_disp_tr_t15, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(endog_disp_tr_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(endog_disp_tr_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 50, 0.55).
narrative_ontology:measurement(endog_disp_tr_t75, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 75, 0.45).
narrative_ontology:measurement(endog_disp_tr_t100, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 100, 0.38).

% Extraction over time
narrative_ontology:measurement(endog_disp_be_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(endog_disp_be_t15, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(endog_disp_be_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(endog_disp_be_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(endog_disp_be_t75, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 75, 0.5).
narrative_ontology:measurement(endog_disp_be_t100, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(endog_disp_su_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(endog_disp_su_t15, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(endog_disp_su_t30, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(endog_disp_su_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement(endog_disp_su_t75, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 75, 0.55).
narrative_ontology:measurement(endog_disp_su_t100, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally distinct readings of the legitimacy_of_practice_standardization kernel. The endogenous_displacement_reading has a different ε, beneficiary/victim structure, and coordination function than its siblings. The kernel label 'legitimacy of practice standardization' conflates three different constraints; they are decomposed per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
