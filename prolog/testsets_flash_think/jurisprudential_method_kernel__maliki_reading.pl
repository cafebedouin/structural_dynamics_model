% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__maliki_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jurisprudential_method_kernel__maliki_reading
 *   human_readable: Maliki Jurisprudential Method: Medinan Practice as Source of Law
 *   domain: religious/legal/historical
 *
 * SUMMARY:
 *   This constraint describes the Maliki school of Islamic jurisprudence,
 *   which asserts that law derives not only from the Qur'an and Hadith but
 *   also from the living tradition ('amal ahl al-Madina) of the Medinan
 *   community, claiming it as the most faithful preservation of the Prophet's
 *   practice. This is one reading of the broader
 *   'jurisprudential_method_kernel', which encompasses various schools'
 *   approaches to legal derivation. The constraint is claimed as a Tangled
 *   Rope because it coordinates legal practice while simultaneously
 *   extracting interpretive authority from other regional and methodological
 *   claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, 0.55).
domain_priors:suppression_score(jurisprudential_method_kernel__maliki_reading, 0.6).
domain_priors:theater_ratio(jurisprudential_method_kernel__maliki_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__maliki_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__maliki_reading, "Maliki Jurisprudential Method: Medinan Practice as Source of Law").
narrative_ontology:topic_domain(jurisprudential_method_kernel__maliki_reading, "religious/legal/historical").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__maliki_reading, '88af95bd-33e5-482b-86f7-39de7dc79ca3').
narrative_ontology:cs_kernel_codification('88af95bd-33e5-482b-86f7-39de7dc79ca3', formalized).
narrative_ontology:cs_authority_grounding('88af95bd-33e5-482b-86f7-39de7dc79ca3', lineage).
narrative_ontology:cs_interpretation_layer_present('88af95bd-33e5-482b-86f7-39de7dc79ca3').
narrative_ontology:cs_reading_relation('88af95bd-33e5-482b-86f7-39de7dc79ca3', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('88af95bd-33e5-482b-86f7-39de7dc79ca3', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('88af95bd-33e5-482b-86f7-39de7dc79ca3', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('88af95bd-33e5-482b-86f7-39de7dc79ca3', foundational, amal_ahl_al_madina_is_prophetic_sunna).
narrative_ontology:cs_axiom_status(amal_ahl_al_madina_is_prophetic_sunna, holdable).
narrative_ontology:cs_axiom_grounding('88af95bd-33e5-482b-86f7-39de7dc79ca3', amal_ahl_al_madina_is_prophetic_sunna, theological).
narrative_ontology:cs_axiom('88af95bd-33e5-482b-86f7-39de7dc79ca3', foundational, medinan_practice_preserves_authenticity).
narrative_ontology:cs_axiom_status(medinan_practice_preserves_authenticity, holdable).
narrative_ontology:cs_axiom_grounding('88af95bd-33e5-482b-86f7-39de7dc79ca3', medinan_practice_preserves_authenticity, empirically_contingent).
narrative_ontology:cs_reference_frame('88af95bd-33e5-482b-86f7-39de7dc79ca3', prophetic_medinan_practice).
narrative_ontology:cs_drift_state('88af95bd-33e5-482b-86f7-39de7dc79ca3', contemporary_global_islamic_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('88af95bd-33e5-482b-86f7-39de7dc79ca3', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, maliki_jurists).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_community).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_claims).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, other_madhhab_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, muslim_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer and interpret Islamic law according to the Maliki method, deriving authority and legitimacy from its foundational claims. They benefit from the elevation of their school's methodology.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, maliki_jurists, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, maliki_jurists, beneficiary).

% The historical and ongoing scholarly tradition rooted in Medina, whose practices and interpretations are elevated to a primary source of law by this method. They are the symbolic and historical beneficiaries of this claim to authenticity.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage, beneficiary,
    organized, civilizational, identity_locked, regional).

% The historical community of Medina, whose 'living tradition' ('amal ahl al-Madina) is asserted as a uniquely authentic source of the Prophet's practice. They benefit from the elevation of their communal norms.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_community, beneficiary,
    organized, generational, constrained, local).

% Scholars from other schools of Islamic jurisprudence (e.g., Hanafi, Shafii, Hanbali) whose alternative methodologies and regional practices are implicitly devalued or challenged in their claim to equal authenticity by the Maliki method. They bear the cost of competing claims to interpretive authority.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, other_madhhab_scholars, payer,
    organized, biographical, constrained, global).

% The general Muslim population who are subject to the legal interpretations derived from this method. They bear the costs of any restrictions or specific rulings that arise from this particular jurisprudential approach, without direct input into its formulation.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, muslim_laity, payer,
    powerless, biographical, constrained, global).

% Alternative regional or methodological claims to interpretive authenticity that are sidelined or rejected by the Maliki emphasis on Medinan practice. These claims are structurally excluded from the primary sources of law within the Maliki framework.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_claims, excluded,
    powerless, civilizational, trapped, global).

% Academics and historians who analyze the development of Islamic jurisprudence, including the Maliki method, from a critical, external perspective. They neither benefit nor pay directly but observe its structural operation and historical impact.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, historical_scholars_of_islam, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__maliki_reading, maliki_jurists).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__maliki_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a consistent and authoritative method for deriving Islamic law, ensuring legal coherence and stability within the Maliki school by prioritizing the living tradition of Medina as a primary source.
% TRANSFER_FUNCTION: Transfers interpretive authority and legitimacy from diverse regional practices and individual juristic reasoning to the established communal practice of Medina, thereby centralizing a specific historical and geographical tradition as a legal source.
% ABSENT_VOICES: Scholars from other regions or schools (e.g., Hanafi, Hanbali) who would argue for the equal validity of their local traditions, different methodological approaches, or a more literal textual approach. Their claims to authenticity are implicitly challenged by the Maliki method's specific emphasis.
% DISAPPEARANCE_RATIONALE: If the Maliki jurisprudential method and its underlying claims vanished overnight, the legal landscape in regions where it is dominant (e.g., North Africa, West Africa) would be thrown into disarray. New methods of legal derivation would be required, potentially leading to fragmentation or the adoption of other schools, fundamentally reorganizing the legal and religious authority structures.
% FOUNDING_PROBLEM: The need to establish a reliable and authoritative source of Islamic law beyond the Qur'an and Hadith, particularly in light of diverse regional practices, potential innovations (bid'ah), and the desire to preserve the Prophet's authentic practice.
% FOUNDING_PROBLEM_CORROBORATION: Maliki scholars and adherents attest to its ongoing necessity for legal coherence and authenticity. Historians of Islamic law and comparative jurists acknowledge the historical problem of legal diversity but may contest the Maliki solution's universal applicability or its claim to unique authenticity, often citing the development of other schools as evidence of alternative valid approaches.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__maliki_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__maliki_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jurisprudential_method_kernel__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__maliki_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__maliki_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) stems from the Maliki method's elevation of Medinan practice, which implicitly devalues other legitimate regional traditions and interpretive approaches, thereby concentrating interpretive authority. Suppression (0.6) reflects the active scholarly and institutional efforts to establish and maintain this methodological hierarchy, pushing back against alternative claims to authenticity. Resistance (0.7) is high due to the historical competition and intellectual contestation from other major schools of thought. Theater ratio is low (0.2) as the method is genuinely applied and functional, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Maliki jurists, this method is a necessary coordination mechanism for preserving authentic Islamic law. From the perspective of other madhhab scholars, it represents an extractive claim to unique authenticity that suppresses alternative, equally valid interpretive paths. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Maliki jurists and the Medinan scholarly lineage are primary beneficiaries, as their authority and tradition are elevated. The Medinan community also benefits from the validation of its historical practices. Other madhhab scholars and non-Medinan interpretive claims are victims, as their methodologies or claims to authenticity are implicitly or explicitly challenged. The Muslim laity are payers, subject to the legal outcomes of this specific interpretive framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this method as a pure Rope (ignoring the extraction of authority from other schools) or a Snare (ignoring its genuine coordination function in legal derivation). The ongoing 'live' status of the founding problem, coupled with the 'world_rearranges' disappearance verdict, indicates that the constraint's function is still perceived as vital, even if its extractive aspects are contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medinan_practice_authenticity_empirical_basis,
    'Is the claim that ''amal ahl al-Madina uniquely preserved the Prophet''s practice more faithfully than other regions empirically verifiable through historical and textual analysis?',
    'Comparative historical-critical analysis of early Islamic legal traditions and Hadith transmission chains across different regions, assessing the empirical evidence for unique Medinan preservation.',
    'If the unique authenticity claim is empirically disproven, the Maliki method''s foundational axiom would be weakened, potentially reducing its perceived legitimacy and extractiveness over other interpretive claims. If strongly corroborated, its coordination function would be amplified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medinan_practice_authenticity_empirical_basis, empirical, 'Empirical basis for the unique authenticity of Medinan practice.').

omega_variable(
    coordination_vs_extraction_of_authority,
    'Is the elevation of ''amal ahl al-Madina primarily a necessary coordination mechanism for legal coherence, or an arbitrary extraction of interpretive authority from other equally valid regional traditions?',
    'Analysis of the historical development of Islamic law, comparing the practical outcomes and social benefits of the Maliki method against those of other schools, particularly in contexts where multiple schools coexisted or competed.',
    'If primarily coordination, the constraint''s effective extractiveness would be lower, emphasizing its role in legal stability. If primarily extraction, its effective extractiveness would be higher, highlighting the costs borne by alternative interpretive claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_of_authority, conceptual, 'Boundary between coordination and extraction in jurisprudential authority.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of non-Medinan interpretive claims structural (institutional backing of Maliki method) or internalized (scholarly deference to Medinan authority)?',
    'Sociological and historical study of scholarly discourse and institutional practices within and outside the Maliki school, examining how challenges to the Maliki method were received and whether alternative claims gained traction in different contexts.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as scholars carry the deference with them. If structural, changes in institutional backing could more readily alter the constraint''s suppressive force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of alternative interpretive claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__maliki_reading, 750, 1250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t750, jurisprudential_method_kernel__maliki_reading, theater_ratio, 750, 0.15).
narrative_ontology:measurement(juri_tr_t850, jurisprudential_method_kernel__maliki_reading, theater_ratio, 850, 0.18).
narrative_ontology:measurement(juri_tr_t950, jurisprudential_method_kernel__maliki_reading, theater_ratio, 950, 0.2).
narrative_ontology:measurement(juri_tr_t1050, jurisprudential_method_kernel__maliki_reading, theater_ratio, 1050, 0.2).
narrative_ontology:measurement(juri_tr_t1150, jurisprudential_method_kernel__maliki_reading, theater_ratio, 1150, 0.2).
narrative_ontology:measurement(juri_tr_t1250, jurisprudential_method_kernel__maliki_reading, theater_ratio, 1250, 0.2).

% Extraction over time
narrative_ontology:measurement(juri_be_t750, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 750, 0.45).
narrative_ontology:measurement(juri_be_t850, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 850, 0.5).
narrative_ontology:measurement(juri_be_t950, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 950, 0.55).
narrative_ontology:measurement(juri_be_t1050, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 1050, 0.58).
narrative_ontology:measurement(juri_be_t1150, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 1150, 0.6).
narrative_ontology:measurement(juri_be_t1250, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 1250, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t750, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 750, 0.5).
narrative_ontology:measurement(juri_su_t850, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 850, 0.55).
narrative_ontology:measurement(juri_su_t950, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 950, 0.6).
narrative_ontology:measurement(juri_su_t1050, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 1050, 0.65).
narrative_ontology:measurement(juri_su_t1150, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 1150, 0.68).
narrative_ontology:measurement(juri_su_t1250, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 1250, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__maliki_reading, identity_coordination).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'jurisprudential_method_kernel', each representing a major school of Islamic law. Each reading has a unique ε value and structural profile, reflecting its specific methodological claims and their impact on interpretive authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
