% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__maliki_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: usul_al_fiqh_method__maliki_reading
 *   human_readable: Maliki School's Jurisprudential Method
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   This constraint describes the Maliki school's methodology within Islamic
 *   jurisprudence, emphasizing 'amal ahl al-Madina (practice of the people of
 *   Medina), maslaha mursala (unrestricted public interest), and 'urf
 *   (custom) as valid sources of law. This reading elevates regional practice
 *   and public welfare alongside textual sources, distinguishing it from
 *   schools that prioritize strict textualism or analogical reasoning. It is
 *   one reading of the broader 'usul al-fiqh method' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, 0.3).
domain_priors:suppression_score(usul_al_fiqh_method__maliki_reading, 0.4).
domain_priors:theater_ratio(usul_al_fiqh_method__maliki_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__maliki_reading, rope).
narrative_ontology:human_readable(usul_al_fiqh_method__maliki_reading, "Maliki School's Jurisprudential Method").
narrative_ontology:topic_domain(usul_al_fiqh_method__maliki_reading, "islamic_jurisprudence/legal_theory/comparative_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__maliki_reading, 'b8eff015-1031-4b0d-a0d8-f45af11d040d').
narrative_ontology:cs_kernel_codification('b8eff015-1031-4b0d-a0d8-f45af11d040d', formalized).
narrative_ontology:cs_authority_grounding('b8eff015-1031-4b0d-a0d8-f45af11d040d', lineage).
narrative_ontology:cs_interpretation_layer_present('b8eff015-1031-4b0d-a0d8-f45af11d040d').
narrative_ontology:cs_reading_relation('b8eff015-1031-4b0d-a0d8-f45af11d040d', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8eff015-1031-4b0d-a0d8-f45af11d040d', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8eff015-1031-4b0d-a0d8-f45af11d040d', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('b8eff015-1031-4b0d-a0d8-f45af11d040d', foundational, medinan_practice_as_independent_source).
narrative_ontology:cs_axiom_status(medinan_practice_as_independent_source, holdable).
narrative_ontology:cs_axiom_grounding('b8eff015-1031-4b0d-a0d8-f45af11d040d', medinan_practice_as_independent_source, conventional).
narrative_ontology:cs_axiom('b8eff015-1031-4b0d-a0d8-f45af11d040d', foundational, maslaha_mursala_as_valid_source).
narrative_ontology:cs_axiom_status(maslaha_mursala_as_valid_source, holdable).
narrative_ontology:cs_axiom_grounding('b8eff015-1031-4b0d-a0d8-f45af11d040d', maslaha_mursala_as_valid_source, instrumental).
narrative_ontology:cs_reference_frame('b8eff015-1031-4b0d-a0d8-f45af11d040d', early_medinan_legal_practice).
narrative_ontology:cs_drift_state('b8eff015-1031-4b0d-a0d8-f45af11d040d', contemporary_globalized_islamic_law, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b8eff015-1031-4b0d-a0d8-f45af11d040d', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, maliki_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, local_medinan_communities).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, customary_law_practitioners).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, universalist_textualists).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, regional_legal_autonomy).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, public_interest_as_legal_source).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and promulgators of the Maliki methodology. Their professional identity and authority are deeply intertwined with this interpretive framework. They benefit from its flexibility and the weight given to their regional traditions.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, maliki_scholars, agenda_setter,
    institutional, generational, identity_locked, regional).

% Their historical practices ('amal ahl al-Madina) are given independent evidentiary weight, validating their traditions and providing a stable basis for local legal continuity. They benefit from legal rulings that reflect their established norms.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, local_medinan_communities, beneficiary,
    organized, generational, identity_locked, local).

% Benefit from the Maliki school's integration of 'urf (custom) as a valid legal source, allowing their local customs to be incorporated into formal legal rulings, provided they do not contradict textual sources. This reduces friction between formal law and lived practice.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, customary_law_practitioners, beneficiary,
    moderate, biographical, constrained, local).

% Adhere to a methodology that prioritizes strict textual interpretation (Quran and Hadith) above all else, often viewing regional practices or unrestricted public interest as secondary or potentially problematic sources. They bear the 'cost' of the Maliki method's deviation from their preferred hierarchy of sources, as it challenges the universality of their textualist approach.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, universalist_textualists, payer,
    powerful, generational, identity_locked, global).

% Represent a different school of thought that emphasizes qiyas (analogical reasoning) and istihsan (juristic preference) but does not grant the same independent weight to Medinan practice or unrestricted public interest. They are 'excluded' from the Maliki framework's internal logic, though their own school remains a parallel, valid system.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, hanafi_scholars, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__maliki_reading, maliki_scholars).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__maliki_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent and adaptable framework for legal derivation within the Maliki school, coordinating legal rulings with local practice and public welfare, and ensuring consistency across diverse cases.
% TRANSFER_FUNCTION: Transfers interpretive authority and legal validity to Medinan practice, public interest considerations, and local customs, away from a sole reliance on universal textual sources, thereby validating regional legal traditions.
% ABSENT_VOICES: Strict textualists and scholars from other schools (e.g., Hanbali, Shafii) who prioritize different hierarchies of sources would object to the independent evidentiary weight given to Medinan practice and unrestricted public interest, arguing for a more text-centric approach. They are absent from the internal methodological discourse of the Maliki school.
% DISAPPEARANCE_RATIONALE: If the Maliki method vanished, the legal landscape in regions historically governed by it would undergo significant upheaval. Legal rulings would lose a foundational interpretive framework, leading to inconsistency and a vacuum in addressing public interest and customary law, forcing a reorganization around other schools or secular legal systems.
% FOUNDING_PROBLEM: The need to reconcile universal Islamic textual sources with the specific, lived practices and public welfare concerns of the early Muslim community in Medina, and to provide a flexible framework for legal reasoning beyond strict textualism.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Islamic law and contemporary legal practitioners in Maliki-influenced regions attest that the challenge of applying universal texts to diverse local contexts and evolving public interests remains live. Independent legal scholars outside the Maliki school acknowledge the historical and ongoing relevance of these methodological questions, even if they disagree with the Maliki solutions.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__maliki_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__maliki_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(usul_al_fiqh_method__maliki_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__maliki_reading_tests).
:- end_tests(usul_al_fiqh_method__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Maliki method, while allowing for flexibility, still operates within a structured legal framework. Extractiveness is moderate (0.3) as it imposes a specific interpretive lens, but it also provides avenues for local adaptation, which can reduce burdens. Suppression is moderate (0.4) as it requires adherence to its methodology, but it is not coercively enforced against other schools. Theater ratio is low (0.1) as its principles are genuinely applied in legal derivation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Maliki scholars, this method is a robust and adaptable framework for Islamic law, genuinely serving the public interest. From a strict textualist perspective, it might be seen as an unwarranted departure from primary sources, potentially introducing arbitrary elements. The engine's classification will reflect the structural benefits and costs, independent of these internal framings.
 *
 * DIRECTIONALITY LOGIC:
 *   Maliki scholars and local Medinan communities are beneficiaries, as their practices and interests are given significant weight. Customary law practitioners also benefit from the integration of 'urf. Universalist textualists, who might prefer a more rigid, text-only approach, could be considered victims as their preferred methodology is challenged or overridden by the Maliki approach.
 *
 * MANDATROPHY ANALYSIS:
 *   The Maliki method's mandate remains live where its principles are actively applied. Its emphasis on 'maslaha mursala' (unrestricted public interest) prevents it from becoming a mere inertial structure, as it requires continuous assessment of public welfare. The integration of 'urf (custom) also ensures its relevance to evolving social contexts, preventing mandatrophy by design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maliki_vs_textualist_authority,
    'Is the Maliki elevation of Medinan practice and public interest a legitimate extension of Islamic law, or a deviation from textual primacy?',
    'Historical analysis of early Islamic legal development and comparative study of legal outcomes across schools, assessed by independent legal historians.',
    'If a legitimate extension, it reinforces the adaptability of Islamic law; if a deviation, it highlights a tension between regional practice and universal textual authority, potentially reclassifying the constraint as a Tangled Rope for textualists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maliki_vs_textualist_authority, conceptual, 'Ambiguity regarding the Maliki school''s methodological legitimacy in relation to other schools.').

omega_variable(
    kernel_reading_impact,
    'How does the Maliki reading''s emphasis on local practice and public interest structurally alter the application of Islamic law compared to more textualist schools?',
    'Comparative legal analysis of fatwas and judicial rulings on specific cases across Maliki, Hanbali, and Hanafi jurisdictions, focusing on the role of ''amal, maslaha, and ''urf in each.',
    'The Maliki reading allows for greater flexibility and contextual adaptation, potentially reducing extraction for local communities but increasing it for those who adhere strictly to universal textual interpretations. This would shift the balance of power in legal interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_impact, empirical, 'Structural delta of the Maliki reading on legal application and power dynamics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__maliki_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__maliki_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(usul_tr_t100, usul_al_fiqh_method__maliki_reading, theater_ratio, 100, 0.11).
narrative_ontology:measurement(usul_tr_t200, usul_al_fiqh_method__maliki_reading, theater_ratio, 200, 0.1).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__maliki_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(usul_be_t100, usul_al_fiqh_method__maliki_reading, base_extractiveness, 100, 0.28).
narrative_ontology:measurement(usul_be_t200, usul_al_fiqh_method__maliki_reading, base_extractiveness, 200, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__maliki_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(usul_su_t100, usul_al_fiqh_method__maliki_reading, suppression_requirement, 100, 0.38).
narrative_ontology:measurement(usul_su_t200, usul_al_fiqh_method__maliki_reading, suppression_requirement, 200, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__maliki_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'usul_al_fiqh_method' kernel, focusing on the Maliki school's unique methodological principles. Other readings (Hanafi, Hanbali, Shafii) are distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
