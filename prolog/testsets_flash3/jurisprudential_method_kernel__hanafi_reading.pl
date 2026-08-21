% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanafi_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanafi_reading
 *   human_readable: Hanafi Jurisprudential Method: Analogical Reasoning and Juristic Preference
 *   domain: islamic_jurisprudence/legal_philosophy/institutional_history
 *
 * SUMMARY:
 *   This constraint describes the Hanafi school's jurisprudential method,
 *   which emphasizes analogical reasoning (qiyas) and juristic preference
 *   (istihsan) as legitimate tools for extending divine intent to novel
 *   cases. It is one reading of the broader 'jurisprudential_method_kernel'
 *   which encompasses various schools of thought in Islamic law. The Hanafi
 *   reading, while providing adaptability, also concentrates interpretive
 *   authority among jurists skilled in these rationalist methods, leading to
 *   a 'tangled_rope' classification due to its coordination function
 *   (adaptability) and asymmetric extraction (from textualists and those
 *   seeking simpler guidance).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, 0.65).
domain_priors:suppression_score(jurisprudential_method_kernel__hanafi_reading, 0.45).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanafi_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanafi_reading, "Hanafi Jurisprudential Method: Analogical Reasoning and Juristic Preference").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanafi_reading, "islamic_jurisprudence/legal_philosophy/institutional_history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanafi_reading, '45965303-2a9c-49ba-8705-92032fd1f95b').
narrative_ontology:cs_kernel_codification('45965303-2a9c-49ba-8705-92032fd1f95b', formalized).
narrative_ontology:cs_authority_grounding('45965303-2a9c-49ba-8705-92032fd1f95b', lineage).
narrative_ontology:cs_interpretation_layer_present('45965303-2a9c-49ba-8705-92032fd1f95b').
narrative_ontology:cs_reading_relation('45965303-2a9c-49ba-8705-92032fd1f95b', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('45965303-2a9c-49ba-8705-92032fd1f95b', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('45965303-2a9c-49ba-8705-92032fd1f95b', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('45965303-2a9c-49ba-8705-92032fd1f95b', foundational, reason_extends_divine_intent).
narrative_ontology:cs_axiom_status(reason_extends_divine_intent, holdable).
narrative_ontology:cs_axiom_grounding('45965303-2a9c-49ba-8705-92032fd1f95b', reason_extends_divine_intent, deontological).
narrative_ontology:cs_axiom('45965303-2a9c-49ba-8705-92032fd1f95b', foundational, juristic_preference_avoids_hardship).
narrative_ontology:cs_axiom_status(juristic_preference_avoids_hardship, holdable).
narrative_ontology:cs_axiom_grounding('45965303-2a9c-49ba-8705-92032fd1f95b', juristic_preference_avoids_hardship, instrumental).
narrative_ontology:cs_reference_frame('45965303-2a9c-49ba-8705-92032fd1f95b', early_hanafi_rationalism).
narrative_ontology:cs_drift_state('45965303-2a9c-49ba-8705-92032fd1f95b', contemporary_global_islam, gap(stable, minor, true)).
narrative_ontology:cs_created_at('45965303-2a9c-49ba-8705-92032fd1f95b', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, hanafi_jurists).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, rationalist_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, textualist_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, lay_community_seeking_simple_guidance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These jurists interpret and extend divine law using qiyas (analogical reasoning) and istihsan (juristic preference), allowing for adaptation to novel cases. Their authority is grounded in their rationalist training and methodological expertise, which is central to their professional identity.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hanafi_jurists, agenda_setter,
    institutional, generational, identity_locked, global).

% Scholars who benefit from the intellectual space and legitimacy provided by the Hanafi method for employing reason in legal derivation. Their careers and influence are tied to the acceptance of these rationalist tools.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, rationalist_scholars, beneficiary,
    organized, biographical, constrained, global).

% Scholars who advocate for a strict adherence to the literal text of Qur'an and Hadith, viewing analogical reasoning and juristic preference as innovations (bid'ah) that dilute divine intent. They bear the cost of diminished authority and influence in legal discourse where Hanafi methods prevail.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, textualist_scholars, payer,
    powerful, generational, constrained, global).

% Members of the community who seek clear, unambiguous legal rulings directly from sacred texts. They find the complex, reasoning-based derivations of the Hanafi school difficult to access or understand, leading to a sense of alienation from legal authority.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, lay_community_seeking_simple_guidance, payer,
    powerless, immediate, trapped, local).

% Scholars of the Maliki school, who prioritize the living tradition of Medina ('amal ahl al-Madina) as a source of law. While not directly victimized, their distinct methodological approach is sidelined in contexts where Hanafi reasoning dominates, limiting their influence.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, maliki_scholars, excluded,
    institutional, generational, identity_locked, regional).

% Scholars of the Shafii school, who advocate for a strict hierarchical methodology of legal sources. Their emphasis on hadith transmission as the primary arbiter differs from Hanafi rationalism, leading to a distinct, often competing, jurisprudential landscape.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, shafii_scholars, excluded,
    institutional, generational, identity_locked, global).

% Scholars of the Hanbali school, known for their strict textualism and rejection of extensive analogical reasoning. They represent the most direct opposition to the Hanafi method's rationalist tendencies, often viewing it as illegitimate innovation.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hanbali_scholars, excluded,
    institutional, generational, identity_locked, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent and adaptable framework for deriving legal rulings in novel situations not explicitly covered by foundational texts, ensuring the continued relevance of Islamic law across diverse contexts and times.
% TRANSFER_FUNCTION: Transfers interpretive authority and the power to shape legal outcomes from strict textual literalists to jurists trained in rationalist methods, particularly in cases requiring analogical extension or juristic preference.
% ABSENT_VOICES: Strict textualist and traditionalist voices, particularly those from the Hanbali school, are often marginalized in Hanafi-dominated legal discourse. They would argue for a return to literal interpretation and a rejection of rationalist tools, but their methodological premises are often dismissed as overly rigid or impractical.
% DISAPPEARANCE_RATIONALE: If the Hanafi method of analogical reasoning and juristic preference vanished, the ability to address novel legal issues would be severely hampered. Other schools might fill the void, but the specific intellectual tradition and its extensive body of jurisprudence would be lost, leading to a significant reorganization of legal thought and practice, particularly in regions historically dominated by Hanafi fiqh.
% FOUNDING_PROBLEM: The early Islamic community faced new legal challenges in diverse regions not explicitly addressed by the Qur'an or Hadith, requiring a method to extend divine intent to these novel cases while maintaining consistency with foundational principles.
% FOUNDING_PROBLEM_CORROBORATION: Hanafi jurists attest that the problem of novel cases is perpetually live, requiring continuous application of rationalist tools. While textualists contest the legitimacy of the solution, the ongoing need for legal guidance in evolving societies is widely acknowledged by legal practitioners and observers across various schools, even if their preferred methods differ.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanafi_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanafi_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) arises from the power differential created by the methodological complexity: jurists trained in qiyas and istihsan gain significant authority in legal derivation, while those who prefer simpler, textualist approaches or the lay community seeking direct guidance bear the cost of this interpretive layer. Suppression (0.45) is moderate; while textualist critiques exist, the Hanafi method's institutional entrenchment and practical utility in diverse contexts ensure its persistence. Theater ratio is low (0.1) as the method is genuinely applied, not merely performed. The temporal measurements show a gradual increase in extractiveness and suppression as the school's methodology became more established and its interpretive authority solidified over centuries.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hanafi jurists, the method is a necessary and beneficial coordination mechanism for adapting divine law to changing circumstances. From the perspective of textualist scholars, it is an extractive imposition that deviates from the purity of divine revelation. The engine's classification as 'tangled_rope' captures this dual nature, acknowledging both the coordination function and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanafi jurists and rationalist scholars are beneficiaries, as the method legitimizes their intellectual tools and enhances their authority (low directionality). Textualist scholars and the lay community seeking simple guidance are victims, as their preferred modes of legal derivation are either suppressed or made inaccessible by the complexity of the Hanafi method (high directionality). Other schools (Maliki, Shafii, Hanbali) are excluded, as their distinct methodologies are not directly accommodated within the Hanafi framework, though they coexist as competing traditions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (adapting divine law to novel cases) remains live. The classification as 'tangled_rope' prevents mislabeling it as pure extraction by recognizing its genuine coordination function, while also highlighting the asymmetric benefits and costs associated with its operation. It is not a 'piton' because it actively serves a function, nor a 'snare' because it does provide a genuine, albeit costly, coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_rational_tools,
    'Is the use of qiyas and istihsan a legitimate extension of divine intent, or an innovation (bid''ah) that corrupts the kernel?',
    'Theological and jurisprudential consensus across major schools, or a definitive textual discovery that explicitly endorses or rejects these methods.',
    'If deemed illegitimate, the Hanafi method''s coordination function would collapse, and its extraction would be reclassified as pure usurpation (snare). If universally endorsed, its extractiveness would be seen as a legitimate cost of coordination (rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_rational_tools, conceptual, 'The fundamental theological and methodological dispute over rationalist tools in legal derivation.').

omega_variable(
    accessibility_of_legal_reasoning,
    'To what extent does the complexity of Hanafi legal reasoning genuinely exclude the lay community, versus merely requiring specialized training?',
    'Empirical studies on legal literacy and access to justice in Hanafi-dominated regions, comparing outcomes for those with and without specialized training.',
    'If exclusion is high and structural, the ''payer'' status of the lay community is reinforced, increasing effective extraction. If it''s primarily a training barrier, the ''payer'' status is mitigated, suggesting a coordination cost rather than pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accessibility_of_legal_reasoning, empirical, 'The degree to which methodological complexity creates an exclusionary barrier for non-specialists.').

omega_variable(
    mandatrophy_of_istihsan,
    'Has the original intent of istihsan (juristic preference to avoid hardship) atrophied into a tool for arbitrary legal maneuvering, or does it still genuinely serve its original purpose?',
    'Historical and contemporary case studies analyzing the application of istihsan, assessing whether its use consistently aligns with the principle of avoiding hardship or if it is used to justify outcomes based on other preferences.',
    'If atrophied, the ''theater_ratio'' would increase, and the ''extractiveness'' would be seen as less justified, potentially shifting the classification towards ''piton'' or ''snare'' if the coordination function is also compromised.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_istihsan, empirical, 'Whether juristic preference (istihsan) retains its original function or has become a tool for arbitrary legal outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanafi_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(juri_tr_t200, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 200, 0.07).
narrative_ontology:measurement(juri_tr_t400, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 400, 0.08).
narrative_ontology:measurement(juri_tr_t600, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 600, 0.1).
narrative_ontology:measurement(juri_tr_t800, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 800, 0.09).
narrative_ontology:measurement(juri_tr_t1000, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 1000, 0.09).
narrative_ontology:measurement(juri_tr_t1200, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 1200, 0.1).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(juri_be_t200, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 200, 0.55).
narrative_ontology:measurement(juri_be_t400, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 400, 0.6).
narrative_ontology:measurement(juri_be_t600, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 600, 0.65).
narrative_ontology:measurement(juri_be_t800, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 800, 0.63).
narrative_ontology:measurement(juri_be_t1000, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 1000, 0.64).
narrative_ontology:measurement(juri_be_t1200, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 1200, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(juri_su_t200, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 200, 0.38).
narrative_ontology:measurement(juri_su_t400, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 400, 0.4).
narrative_ontology:measurement(juri_su_t600, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 600, 0.45).
narrative_ontology:measurement(juri_su_t800, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 800, 0.43).
narrative_ontology:measurement(juri_su_t1000, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 1000, 0.44).
narrative_ontology:measurement(juri_su_t1200, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 1200, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanafi_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'jurisprudential_method_kernel,' each representing a major school of Islamic law. They are linked as a constraint family because their methodological differences directly influence each other's legitimacy and scope of application.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
