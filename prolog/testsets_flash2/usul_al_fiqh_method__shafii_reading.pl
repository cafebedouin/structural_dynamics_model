% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__shafii_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__shafii_reading
 *   human_readable: Shafi'i Reading of Usul al-Fiqh: Hadith-Centric Legal Derivation
 *   domain: islamic_jurisprudence/legal_theory
 *
 * SUMMARY:
 *   This constraint represents the Shafi'i reading of Usul al-Fiqh
 *   (principles of Islamic jurisprudence), which systematized the hierarchy
 *   of legal sources, prioritizing authenticated Hadith and subordinating
 *   analogical reasoning (qiyas) and other methods. It establishes a
 *   meta-discipline for legal derivation, granting significant gatekeeping
 *   authority to Hadith transmission specialists. This reading is one of
 *   several competing methodologies within Islamic legal theory, each with
 *   distinct structural implications for jurists and the application of law.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, 0.68).
domain_priors:suppression_score(usul_al_fiqh_method__shafii_reading, 0.75).
domain_priors:theater_ratio(usul_al_fiqh_method__shafii_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__shafii_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__shafii_reading, "Shafi'i Reading of Usul al-Fiqh: Hadith-Centric Legal Derivation").
narrative_ontology:topic_domain(usul_al_fiqh_method__shafii_reading, "islamic_jurisprudence/legal_theory").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__shafii_reading, '74d0fcc9-5c62-4a18-9551-204feb12c808').
narrative_ontology:cs_kernel_codification('74d0fcc9-5c62-4a18-9551-204feb12c808', formalized).
narrative_ontology:cs_authority_grounding('74d0fcc9-5c62-4a18-9551-204feb12c808', lineage).
narrative_ontology:cs_interpretation_layer_present('74d0fcc9-5c62-4a18-9551-204feb12c808').
narrative_ontology:cs_reading_relation('74d0fcc9-5c62-4a18-9551-204feb12c808', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('74d0fcc9-5c62-4a18-9551-204feb12c808', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('74d0fcc9-5c62-4a18-9551-204feb12c808', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('74d0fcc9-5c62-4a18-9551-204feb12c808', foundational, hadith_authentication_as_primary_gate).
narrative_ontology:cs_axiom_status(hadith_authentication_as_primary_gate, holdable).
narrative_ontology:cs_axiom_grounding('74d0fcc9-5c62-4a18-9551-204feb12c808', hadith_authentication_as_primary_gate, conventional).
narrative_ontology:cs_axiom('74d0fcc9-5c62-4a18-9551-204feb12c808', foundational, ijma_restricted_to_companions).
narrative_ontology:cs_axiom_status(ijma_restricted_to_companions, holdable).
narrative_ontology:cs_axiom_grounding('74d0fcc9-5c62-4a18-9551-204feb12c808', ijma_restricted_to_companions, conventional).
narrative_ontology:cs_reference_frame('74d0fcc9-5c62-4a18-9551-204feb12c808', systematized_textual_hierarchy).
narrative_ontology:cs_drift_state('74d0fcc9-5c62-4a18-9551-204feb12c808', contemporary_pluralistic_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('74d0fcc9-5c62-4a18-9551-204feb12c808', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, shafii_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, local_custom_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, lay_muslims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their expertise in authenticating Hadith becomes the primary gatekeeping mechanism for legal derivation. They define the corpus of permissible textual sources, thereby shaping the boundaries of legitimate legal reasoning. Their authority is deeply intertwined with their professional identity.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from a clear, systematized methodology that prioritizes authenticated Hadith, providing a robust framework for legal reasoning. Their school's methodology gains prominence and intellectual coherence, attracting adherents.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, shafii_jurists, beneficiary,
    organized, generational, constrained, global).

% Jurists who prioritize independent rational inquiry (ra'y) or expansive analogical reasoning (qiyas) find their methods subordinated and constrained by the strict hierarchy of sources. Their interpretive freedom is curtailed, and their authority diminished in favor of textual literalism.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, rationalist_jurists, payer,
    moderate, biographical, constrained, regional).

% Jurists who traditionally incorporated local custom ('urf) or unrestricted public interest (maslaha mursala) find these sources marginalized or entirely excluded from legal derivation, unless explicitly supported by authenticated texts. Their local authority is challenged by a universalizing textual methodology.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, local_custom_jurists, payer,
    moderate, biographical, constrained, local).

% Are subject to legal rulings derived from a methodology that may not always align with local practices or immediate public interest, as these are subordinated to textual authentication. They have little recourse to challenge the derived rulings.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, lay_muslims, payer,
    powerless, biographical, trapped, local).

% Jurists from the Hanafi school, who emphasize expansive qiyas and ra'y, are structurally excluded from the Shafi'i framework's internal legitimacy claims. They operate under a different interpretive paradigm, which the Shafi'i reading implicitly delegitimizes as less rigorous.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hanafi_jurists, excluded,
    organized, generational, identity_locked, global).

% Jurists from the Maliki school, who prioritize Medinan practice and maslaha mursala, are structurally excluded from the Shafi'i framework's internal legitimacy claims. Their sources of authority are not recognized as primary within this reading.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, maliki_jurists, excluded,
    organized, generational, identity_locked, global).

% Jurists from the Hanbali school, who emphasize strict textual adherence and minimize qiyas, are structurally excluded from the Shafi'i framework's internal legitimacy claims. While both are text-centric, the Hanbali reading's maximal restrictiveness differs in its application and scope.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hanbali_jurists, excluded,
    organized, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, universal hierarchy of legal sources and a systematic methodology for derivation, reducing interpretive anarchy and providing a consistent framework for Islamic law across diverse regions.
% TRANSFER_FUNCTION: Transfers interpretive authority and legitimacy from diverse local practices and expansive rationalist methods to a centralized, text-authenticated methodology, benefiting Hadith scholars and Shafi'i jurists.
% ABSENT_VOICES: Jurists from other schools (Hanafi, Maliki, Hanbali) and those advocating for broader rationalist or local custom-based approaches are structurally excluded from the Shafi'i framework's internal discourse, as their foundational premises are not accepted as primary. They would argue for a more inclusive or flexible methodology.
% DISAPPEARANCE_RATIONALE: If this systematized methodology vanished, the clarity and consistency of legal derivation would collapse. Hadith scholars would lose their gatekeeping authority, rationalist and local custom approaches would reassert themselves, and the entire structure of Islamic legal theory would fragment, leading to a profound reorganization of jurisprudential practice.
% FOUNDING_PROBLEM: The early Islamic legal landscape was characterized by diverse, often conflicting, methods of legal derivation, leading to inconsistency and potential chaos in applying Islamic law across a rapidly expanding empire.
% FOUNDING_PROBLEM_CORROBORATION: Shafi'i jurists and Hadith scholars attest that the problem of interpretive anarchy remains live, justifying the continued need for a rigorous, text-centric methodology. Historians of Islamic law corroborate the historical problem of early legal diversity, though they may contest the necessity or exclusivity of the Shafi'i solution.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__shafii_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__shafii_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(usul_al_fiqh_method__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__shafii_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) stems from the strict control over legitimate sources, which limits interpretive freedom and channels authority to specific scholarly groups. Suppression (0.75) is high because alternative methodologies (e.g., expansive qiyas, local custom) are actively marginalized or excluded from the accepted framework, requiring continuous intellectual and institutional defense of the Shafi'i method. Theater ratio (0.20) is low, as the system is genuinely functional in producing legal rulings, though some of its claims to universal applicability may be performative in the face of persistent alternative readings.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Shafi'i jurists, this system is a necessary coordination mechanism for legal consistency and rigor. From the perspective of rationalist or local custom jurists, it is an extractive system that suppresses alternative, equally valid, interpretive paths. The engine's classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith transmission specialists and Shafi'i jurists are clear beneficiaries, gaining authority and a coherent framework. Rationalist and local custom jurists are victims, as their methods are subordinated. Lay Muslims are also victims, subject to a legal system whose derivation methods may not always align with their local realities. Other schools of thought (Hanafi, Maliki, Hanbali) are structurally excluded, operating under different, competing frameworks.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universality_vs_local_relevance,
    'Does the systematized, Hadith-centric methodology genuinely provide universal legal relevance, or does it create a persistent gap with diverse local practices and needs?',
    'Comparative legal anthropology: empirical studies of how Shafi''i rulings are applied and adapted (or resisted) in various local contexts, especially where they conflict with established custom or public interest.',
    'If a persistent gap is found, the constraint''s effective extractiveness and suppression on local populations would be higher than currently measured, as it would be actively overriding local norms rather than coordinating them. This would push the classification closer to a Snare for local communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_vs_local_relevance, empirical, 'Assesses the tension between universal textual methodology and local legal realities.').

omega_variable(
    interpretive_authority_legitimacy,
    'Is the authority of Hadith transmission specialists and Shafi''i jurists derived from inherent textual rigor, or is it partly a function of institutional power and historical precedent that suppresses alternative interpretive claims?',
    'Historical-sociological analysis of the rise of the Shafi''i school: examining the institutional, political, and intellectual factors that led to its dominance, alongside its internal methodological arguments. This would distinguish between purely epistemic claims and power-laden historical contingencies.',
    'If institutional power is a significant factor, the constraint''s ''naturalness'' (as a purely epistemic system) would be undermined, and its classification would lean more strongly towards a Tangled Rope or even Snare, as the coordination story would be partly cover for power consolidation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_authority_legitimacy, conceptual, 'Examines the grounding of interpretive authority in the Shafi''i method.').

omega_variable(
    qiyas_subordination_necessity,
    'Is the subordination of qiyas (analogical reasoning) to authenticated Hadith a necessary condition for legal consistency, or does it unnecessarily limit the adaptability of Islamic law to novel situations?',
    'Comparative legal analysis across schools: examining how Hanafi jurists (with their more expansive qiyas) address novel legal problems, and whether their solutions are less consistent or more prone to error than Shafi''i approaches. This would be a conceptual comparison of methodological outcomes.',
    'If expansive qiyas proves equally consistent and adaptable, the Shafi''i reading''s suppression of qiyas would be seen as an unnecessary restriction, increasing its effective extractiveness from jurists seeking flexibility. This would strengthen the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qiyas_subordination_necessity, conceptual, 'Evaluates the necessity of qiyas subordination for legal consistency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__shafii_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__shafii_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(usul_tr_t300, usul_al_fiqh_method__shafii_reading, theater_ratio, 300, 0.12).
narrative_ontology:measurement(usul_tr_t600, usul_al_fiqh_method__shafii_reading, theater_ratio, 600, 0.15).
narrative_ontology:measurement(usul_tr_t900, usul_al_fiqh_method__shafii_reading, theater_ratio, 900, 0.18).
narrative_ontology:measurement(usul_tr_t1200, usul_al_fiqh_method__shafii_reading, theater_ratio, 1200, 0.2).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__shafii_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(usul_be_t300, usul_al_fiqh_method__shafii_reading, base_extractiveness, 300, 0.58).
narrative_ontology:measurement(usul_be_t600, usul_al_fiqh_method__shafii_reading, base_extractiveness, 600, 0.63).
narrative_ontology:measurement(usul_be_t900, usul_al_fiqh_method__shafii_reading, base_extractiveness, 900, 0.66).
narrative_ontology:measurement(usul_be_t1200, usul_al_fiqh_method__shafii_reading, base_extractiveness, 1200, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__shafii_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(usul_su_t300, usul_al_fiqh_method__shafii_reading, suppression_requirement, 300, 0.65).
narrative_ontology:measurement(usul_su_t600, usul_al_fiqh_method__shafii_reading, suppression_requirement, 600, 0.7).
narrative_ontology:measurement(usul_su_t900, usul_al_fiqh_method__shafii_reading, suppression_requirement, 900, 0.73).
narrative_ontology:measurement(usul_su_t1200, usul_al_fiqh_method__shafii_reading, suppression_requirement, 1200, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__shafii_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'usul_al_fiqh_method' kernel. Its systematized hierarchy of sources and Hadith-centric approach structurally influences (and competes with) other schools of thought, each representing a distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
