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
 *   constraint_id: nsl_legal_text__jurisdictional_capture_reading
 *   human_readable: NSL as Jurisdictional Capture of Hong Kong's Common Law Autonomy
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This constraint story analyzes the National Security Law (NSL) in Hong
 *   Kong from the 'jurisdictional capture' reading. In this reading, the NSL
 *   serves as a vehicle for transplanting elements of mainland China's legal
 *   system into Hong Kong, thereby eroding the autonomy and distinctiveness
 *   of its common law framework. The NSL is presented as a national security
 *   measure, but its operational effect, from this perspective, is to
 *   gradually subsume Hong Kong's independent judiciary and legal profession
 *   under mainland control. The metrics reflect a high degree of extraction
 *   and suppression, consistent with an institutional structure designed to
 *   centralize legal authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, 0.78).
domain_priors:suppression_score(nsl_legal_text__jurisdictional_capture_reading, 0.88).
domain_priors:theater_ratio(nsl_legal_text__jurisdictional_capture_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__jurisdictional_capture_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__jurisdictional_capture_reading, "NSL as Jurisdictional Capture of Hong Kong's Common Law Autonomy").
narrative_ontology:topic_domain(nsl_legal_text__jurisdictional_capture_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__jurisdictional_capture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__jurisdictional_capture_reading, '61fa37f6-0489-4b21-9ae4-0991283d8476').
narrative_ontology:cs_kernel_codification('61fa37f6-0489-4b21-9ae4-0991283d8476', fixed_text).
narrative_ontology:cs_authority_grounding('61fa37f6-0489-4b21-9ae4-0991283d8476', extraction).
narrative_ontology:cs_interpretation_layer_present('61fa37f6-0489-4b21-9ae4-0991283d8476').
narrative_ontology:cs_reading_relation('61fa37f6-0489-4b21-9ae4-0991283d8476', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('61fa37f6-0489-4b21-9ae4-0991283d8476', nsl_legal_text__democratic_enclosure_reading, influences).
narrative_ontology:cs_axiom('61fa37f6-0489-4b21-9ae4-0991283d8476', foundational, common_law_autonomy_is_sacrosanct).
narrative_ontology:cs_axiom_status(common_law_autonomy_is_sacrosanct, holdable).
narrative_ontology:cs_axiom_grounding('61fa37f6-0489-4b21-9ae4-0991283d8476', common_law_autonomy_is_sacrosanct, deontological).
narrative_ontology:cs_axiom('61fa37f6-0489-4b21-9ae4-0991283d8476', foundational, mainland_legal_system_is_distinct_and_separate).
narrative_ontology:cs_axiom_status(mainland_legal_system_is_distinct_and_separate, holdable).
narrative_ontology:cs_axiom_grounding('61fa37f6-0489-4b21-9ae4-0991283d8476', mainland_legal_system_is_distinct_and_separate, conventional).
narrative_ontology:cs_reference_frame('61fa37f6-0489-4b21-9ae4-0991283d8476', one_country_two_systems_common_law_autonomy).
narrative_ontology:cs_drift_state('61fa37f6-0489-4b21-9ae4-0991283d8476', post_nsl_implementation_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('61fa37f6-0489-4b21-9ae4-0991283d8476', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, hong_kong_executive_branch).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly benefits from the NSL's provisions, which allow it to operate within Hong Kong with broad powers, bypassing local legal processes. It views the NSL as a necessary tool for national security and stability, extending its jurisdiction and operational reach.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains enhanced powers to suppress dissent and enforce policies without significant judicial challenge. While nominally autonomous, its actions are increasingly aligned with mainland directives, benefiting from the NSL's erosion of checks and balances.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_executive_branch, beneficiary,
    institutional, biographical, constrained, local).

% Suffers a significant loss of autonomy and interpretive power. Its common law traditions are undermined by the NSL's supremacy and the mainland's interpretation. Judges are identity-locked by their professional commitment to the rule of law, making exit difficult despite the erosion of their independence.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary, payer,
    institutional, generational, identity_locked, local).

% Faces a shrinking scope for independent legal practice, increased political pressure, and the risk of prosecution under the NSL. Many are constrained by their careers and ties to Hong Kong, but some have chosen to emigrate.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession, payer,
    organized, biographical, constrained, local).

% Experience a direct loss of civil liberties, freedom of speech, and due process protections previously guaranteed under common law. They are trapped by the NSL's broad scope and severe penalties, with limited avenues for legal challenge or political expression.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_citizens, payer,
    powerless, immediate, trapped, local).

% Monitor the implementation of the NSL and its impact on Hong Kong's legal system, issuing reports and condemnations. Their influence is primarily diplomatic and reputational, with limited direct enforcement power.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The NSL coordinates the integration of Hong Kong's legal and political system with mainland China's national security framework, aiming to eliminate perceived legal ambiguities and ensure alignment with central government directives.
% TRANSFER_FUNCTION: Transfers legal and judicial authority from Hong Kong's common law institutions to mainland-aligned security and executive bodies, effectively moving interpretive and enforcement power.
% ABSENT_VOICES: Independent legal scholars and international human rights advocates, who would argue for the preservation of Hong Kong's judicial independence and common law traditions, are largely excluded from the official discourse and face risks if they operate within Hong Kong.
% DISAPPEARANCE_RATIONALE: If the NSL vanished overnight, Hong Kong's common law system would immediately reassert its previous autonomy, the judiciary would regain its interpretive power, and the mainland security apparatus would lose its legal basis for operation within the territory. This would lead to a significant reordering of legal and political power dynamics.
% FOUNDING_PROBLEM: The mainland government perceived a critical national security vulnerability in Hong Kong following large-scale pro-democracy protests in 2019, which it viewed as challenging its sovereignty and the 'One Country, Two Systems' framework.
% FOUNDING_PROBLEM_CORROBORATION: The mainland government and the Hong Kong executive branch attest that the founding problem of national security threats remains live. However, the Hong Kong legal profession and international observers argue that the NSL's scope far exceeds genuine security concerns, and that the 'problem' is now used to justify broader political control.
narrative_ontology:disappearance_verdict(nsl_legal_text__jurisdictional_capture_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__jurisdictional_capture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__jurisdictional_capture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nsl_legal_text__jurisdictional_capture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__jurisdictional_capture_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.78) is high because the NSL effectively reallocates significant legal authority and interpretive power from Hong Kong's independent judiciary to mainland-controlled bodies, imposing a substantial cost on the common law system. Suppression (0.88) is very high due to the NSL's broad scope, severe penalties, and the active enforcement by mainland security agencies, which effectively stifle dissent and legal challenges. The theater ratio (0.45) indicates that while some national security concerns may be genuine, a significant portion of the NSL's application serves to legitimize the expansion of mainland legal influence under the guise of security. Accessibility collapse (0.75) is high as legal avenues for challenging the NSL are severely curtailed, and resistance (0.60) is present but heavily suppressed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the mainland security apparatus, the NSL is a legitimate exercise of sovereign power to restore order and protect national security, thus appearing as a 'rope' or even 'mountain' of national interest. However, from the perspective of the Hong Kong judiciary and legal profession, it is a 'snare' or 'tangled rope' that systematically dismantles their institutional autonomy and extracts their common law heritage. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The mainland security apparatus and the Hong Kong executive branch are clear beneficiaries, gaining expanded powers and reduced legal constraints. The Hong Kong judiciary, legal profession, and citizens are victims, experiencing a direct loss of autonomy, professional independence, and civil liberties. The 'identity_locked' exit option for the judiciary reflects their deep professional commitment to common law principles, making it difficult to abandon their roles despite the erosion of their independence.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by focusing on the structural impact of the NSL on Hong Kong's legal system, rather than accepting its stated mandate at face value. While the NSL claims to address a 'live' national security problem, this reading argues that its primary function has drifted towards jurisdictional capture, making it a 'tangled rope' that coordinates the integration of legal systems while extracting autonomy. The high extractiveness and suppression, coupled with the 'contested' status of the founding problem, indicate that the constraint's mandate has been repurposed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_security_threat_vs_jurisdictional_ambition,
    'To what extent does the NSL genuinely address an existential national security threat, versus serving as a vehicle for broader jurisdictional and political integration?',
    'Independent, verifiable intelligence assessments of genuine security threats, compared against the actual scope and application of the NSL''s provisions and their impact on non-security-related legal areas.',
    'If the threat is minimal or manufactured, the NSL''s extractiveness is almost entirely due to jurisdictional capture; if the threat is substantial, a portion of the extractiveness could be reclassified as a legitimate (though still coercive) coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(true_security_threat_vs_jurisdictional_ambition, empirical, 'Distinguishing genuine security needs from political objectives in the NSL''s application.').

omega_variable(
    common_law_resilience_threshold,
    'At what point does the cumulative impact of the NSL render Hong Kong''s common law system functionally indistinguishable from mainland civil law, losing its distinct identity?',
    'Longitudinal comparative legal analysis of judicial decisions, legal education, and professional practice in Hong Kong versus mainland China, identifying a ''tipping point'' where core common law principles are no longer operative.',
    'If the threshold is crossed, the ''identity_locked'' exit option for the judiciary becomes ''trapped'' as their professional identity is dissolved, increasing effective extraction. The constraint would shift closer to a pure ''snare'' as the coordination function of maintaining a distinct legal system collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(common_law_resilience_threshold, conceptual, 'Assessing the point of no return for Hong Kong''s common law autonomy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (external barriers) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-exit suppression trajectory: if suppression persists (e.g., self-censorship, fear of reprisal) after the extractive mechanism is removed (e.g., emigration), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more insidious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in Hong Kong.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__jurisdictional_capture_reading, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(nsl__tr_t1, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 1, 0.35).
narrative_ontology:measurement(nsl__tr_t2, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2, 0.4).
narrative_ontology:measurement(nsl__tr_t3, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 3, 0.43).
narrative_ontology:measurement(nsl__tr_t4, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 4, 0.44).
narrative_ontology:measurement(nsl__tr_t5, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 5, 0.45).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(nsl__be_t1, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 1, 0.7).
narrative_ontology:measurement(nsl__be_t2, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2, 0.74).
narrative_ontology:measurement(nsl__be_t3, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 3, 0.76).
narrative_ontology:measurement(nsl__be_t4, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 4, 0.77).
narrative_ontology:measurement(nsl__be_t5, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 5, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(nsl__su_t1, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 1, 0.8).
narrative_ontology:measurement(nsl__su_t2, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2, 0.84).
narrative_ontology:measurement(nsl__su_t3, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 3, 0.86).
narrative_ontology:measurement(nsl__su_t4, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 4, 0.87).
narrative_ontology:measurement(nsl__su_t5, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 5, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__jurisdictional_capture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__democratic_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__sovereignty_restoration_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'nsl_legal_text' kernel. This 'jurisdictional_capture_reading' focuses on the erosion of common law autonomy, while 'democratic_enclosure_reading' focuses on the criminalization of dissent, and 'sovereignty_restoration_reading' frames the NSL as a legitimate security instrument. All three are distinct constraints with different ε values and stakeholder impacts, linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
