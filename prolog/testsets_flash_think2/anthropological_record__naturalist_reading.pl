% ============================================================================
% CONSTRAINT STORY: anthropological_record__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__naturalist_reading, []).

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
 *   constraint_id: anthropological_record__naturalist_reading
 *   human_readable: Naturalist Reading of Human Origins Record
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This constraint describes the dominant scientific framework for
 *   understanding human origins, which posits materialist explanations
 *   (evolution, migration) knowable through the scientific method. While it
 *   provides a powerful coordination function for scientific inquiry, it also
 *   operates with significant extraction and suppression by excluding
 *   non-scientific epistemologies and non-credentialed interpreters. The
 *   constraint is claimed as a 'rope' by its proponents (a neutral framework
 *   for knowledge coordination) but operates as a 'tangled_rope' due to its
 *   active enforcement and the asymmetric benefits it confers.
 *
 * KEY AGENTS:
 *   - Academic Anthropologists/Evolutionary Biologists: Primary beneficiaries and agenda-setters (institutional/analytical power, generational time horizon, analytical exit).
 *   - Scientific Institutions/Credentialing Bodies: Agenda-setters and beneficiaries (institutional power, generational time horizon, constrained exit).
 *   - Creationist Advocates/Indigenous Knowledge Holders/Non-Credentialed Interpreters: Primary targets and excluded parties (organized/moderate/powerless power, generational/civilizational time horizon, constrained/identity_locked exit).
 *   - General Public: Both beneficiary (access to coherent knowledge) and payer (indirect costs, epistemic exclusion) (moderate power, biographical time horizon, constrained exit).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__naturalist_reading, 0.7).
domain_priors:suppression_score(anthropological_record__naturalist_reading, 0.8).
domain_priors:theater_ratio(anthropological_record__naturalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__naturalist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__naturalist_reading, "Naturalist Reading of Human Origins Record").
narrative_ontology:topic_domain(anthropological_record__naturalist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__naturalist_reading, '8d1895a4-a781-4de3-8570-1ad41ec85b98').
narrative_ontology:cs_kernel_codification('8d1895a4-a781-4de3-8570-1ad41ec85b98', formalized).
narrative_ontology:cs_authority_grounding('8d1895a4-a781-4de3-8570-1ad41ec85b98', expertise).
narrative_ontology:cs_interpretation_layer_present('8d1895a4-a781-4de3-8570-1ad41ec85b98').
narrative_ontology:cs_reading_relation('8d1895a4-a781-4de3-8570-1ad41ec85b98', anthropological_record__creationist_reading, forecloses).
narrative_ontology:cs_reading_relation('8d1895a4-a781-4de3-8570-1ad41ec85b98', anthropological_record__indigenous_epistemology_reading, coexists_with).
narrative_ontology:cs_axiom('8d1895a4-a781-4de3-8570-1ad41ec85b98', foundational, methodological_naturalism).
narrative_ontology:cs_axiom_status(methodological_naturalism, holdable).
narrative_ontology:cs_axiom_grounding('8d1895a4-a781-4de3-8570-1ad41ec85b98', methodological_naturalism, empirically_contingent).
narrative_ontology:cs_axiom('8d1895a4-a781-4de3-8570-1ad41ec85b98', foundational, empirical_verifiability).
narrative_ontology:cs_axiom_status(empirical_verifiability, holdable).
narrative_ontology:cs_axiom_grounding('8d1895a4-a781-4de3-8570-1ad41ec85b98', empirical_verifiability, empirically_contingent).
narrative_ontology:cs_reference_frame('8d1895a4-a781-4de3-8570-1ad41ec85b98', enlightenment_scientific_paradigm).
narrative_ontology:cs_drift_state('8d1895a4-a781-4de3-8570-1ad41ec85b98', contemporary_postcolonial_critique, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8d1895a4-a781-4de3-8570-1ad41ec85b98', '').
narrative_ontology:cs_kernel_id(anthropological_record__naturalist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, academic_anthropologists).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, evolutionary_biologists).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, scientific_institutions).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, creationist_advocates).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, indigenous_knowledge_holders).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, non_credentialed_interpreters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, general_public).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the methodological naturalism that governs the study of human origins, benefiting from grants, publications, and professional recognition within the scientific paradigm. They are the primary interpreters and gatekeepers of the naturalist record.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, academic_anthropologists, agenda_setter,
    institutional, generational, analytical, global).

% Benefit from the established naturalist framework, which provides the foundational principles for their research into human evolution. They contribute to and reinforce the constraint through their scientific output and peer review.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, evolutionary_biologists, beneficiary,
    institutional, generational, analytical, global).

% Universities, research centers, and funding bodies that institutionalize and propagate the naturalist reading. They allocate resources, confer credentials, and maintain the infrastructure that supports this epistemic framework, benefiting from its authority and public trust.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, scientific_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Professional associations and academic departments that certify expertise in anthropology and related fields. They enforce the methodological and interpretive boundaries of the naturalist reading, ensuring only credentialed individuals gain access to positions of authority and influence.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, credentialing_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Bear the cost of their views being systematically excluded from mainstream scientific and educational discourse. They are denied academic legitimacy and public funding for their interpretations of human origins, often relegated to alternative institutions.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, creationist_advocates, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, creationist_advocates, excluded).

% Their ancestral narratives and oral traditions regarding human origins are often dismissed or marginalized by the dominant naturalist framework. They face challenges in having their epistemologies recognized as valid within academic or public spheres, despite their deep connection to place and long-term observation.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, indigenous_knowledge_holders, payer,
    moderate, civilizational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, indigenous_knowledge_holders, excluded).

% Individuals or groups who offer alternative interpretations of human origins without formal academic credentials. They are systematically excluded from publishing in peer-reviewed journals, presenting at major conferences, or holding academic positions, regardless of the merits of their arguments.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, non_credentialed_interpreters, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, non_credentialed_interpreters, excluded).

% Benefits from a coherent, evidence-based narrative of human origins taught in schools and disseminated through popular science. They indirectly pay for the maintenance of the scientific establishment through taxes and tuition, and may find their own non-scientific beliefs challenged or dismissed by the dominant narrative.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, general_public, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, general_public, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__naturalist_reading, scientific_institutions).
narrative_ontology:fixing_cost_class(anthropological_record__naturalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, evidence-based framework for understanding human origins, enabling cumulative knowledge building, intersubjective verification, and a common basis for scientific research across disciplines.
% TRANSFER_FUNCTION: Transfers epistemic authority, research funding, and educational influence from alternative explanations of human origins to the scientific establishment and its credentialed practitioners. It also transfers resources (grants, academic positions) to those who adhere to the naturalist framework.
% ABSENT_VOICES: Creationist theologians, indigenous elders, and other non-scientific interpreters of human origins are structurally excluded from the dominant academic and public discourse. They would argue for the validity of their own epistemologies and challenge the naturalist framework's claim to exclusive truth, but are kept out by the same credentialing and methodological rules the constraint rides on.
% DISAPPEARANCE_RATIONALE: If the naturalist framework and its enforcement vanished overnight, the shared understanding of human origins would fragment into competing, incommensurable narratives. Scientific institutions would lose their authority on this topic, public education would lack a coherent, evidence-based curriculum, and interdisciplinary research on human evolution would collapse without a common epistemic ground.
% FOUNDING_PROBLEM: To provide a systematic, verifiable, and non-supernatural explanation for human origins, replacing speculative, mythological, or faith-based accounts with empirically testable hypotheses and evidence-based conclusions.
% FOUNDING_PROBLEM_CORROBORATION: The scientific community attests the problem is still live, citing ongoing challenges from non-scientific explanations and the need for continuous empirical refinement. Creationist and indigenous communities, as well as some critical scholars, attest that the founding problem is either solved in other ways or that the naturalist framework itself creates new problems of exclusion; legislative hearings and independent philosophical critiques from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(anthropological_record__naturalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__naturalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__naturalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(anthropological_record__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__naturalist_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__naturalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__naturalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.7) is high because the framework's exclusivity allows the scientific establishment to capture significant resources (funding, positions, public trust) that would otherwise be distributed among alternative interpretive communities. Suppression (0.8) is high due to the active gatekeeping mechanisms (credentialing, peer review, funding allocation) that systematically exclude non-naturalist or non-credentialed interpretations. Theater ratio (0.4) is moderate; while genuine scientific work occurs, a substantial portion of institutional effort is dedicated to defending the boundaries of the naturalist paradigm against external challenges, rather than purely advancing knowledge. The increasing trend in metrics reflects the hardening of these boundaries and the accumulation of epistemic capital over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of academic scientists, the naturalist reading is a neutral, objective framework for discovering truth, operating as a 'rope' that coordinates inquiry. From the perspective of excluded groups (e.g., creationists, indigenous knowledge holders), the same structure operates as a 'snare' or 'tangled_rope,' actively suppressing alternative epistemologies and extracting authority and resources to maintain a scientific monopoly. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic anthropologists, evolutionary biologists, and scientific institutions are clear beneficiaries (low directionality) as they control the framework, receive funding, and gain professional recognition. Creationist advocates, indigenous knowledge holders, and non-credentialed interpreters are targets (high directionality) as they are excluded from mainstream discourse and denied legitimacy. The general public is mixed, benefiting from a coherent narrative but also bearing indirect costs and facing epistemic exclusion if their beliefs diverge.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as 'tangled_rope' prevents mislabeling this constraint as a pure 'rope' (which would ignore the significant extraction and suppression) or a pure 'snare' (which would ignore the genuine coordination function of the scientific method). It highlights that while the scientific method coordinates knowledge, its institutionalization and defense of exclusivity have layered on extractive dynamics. The founding problem (replacing speculative accounts) is still 'contested,' indicating that the mandate has not fully atrophied, but its current operation includes substantial rent-seeking and boundary maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_naturalist_reading,
    'Is this constraint a genuine, universally applicable epistemic framework, or one reading of a contested kernel (''anthropological_record'')?',
    'Analysis of cross-cultural and historical epistemologies: if other coherent, self-consistent frameworks exist that yield different ''truths'' about human origins, it supports the ''reading'' interpretation.',
    'If confirmed as a reading, it shifts the classification from a potentially universal ''mountain'' of knowledge to a ''tangled_rope'' or ''snare'' that actively defends its epistemic territory against alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_naturalist_reading, conceptual, 'This constraint is the naturalist reading of the ''anthropological_record'' kernel, with creationist and indigenous epistemology readings as siblings.').

omega_variable(
    epistemic_pluralism_vs_rigor,
    'Is the exclusion of non-scientific epistemologies a necessary condition for scientific rigor and cumulative knowledge, or an extractive gatekeeping mechanism that suppresses valid alternative forms of knowledge?',
    'Empirical study of knowledge systems that integrate scientific and indigenous epistemologies: if such systems demonstrate comparable rigor and predictive power, it suggests the exclusion is not strictly necessary for rigor.',
    'If exclusion is not necessary for rigor, the measured suppression is more purely extractive; if it is necessary, a portion of suppression is a legitimate cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_pluralism_vs_rigor, empirical, 'Whether epistemic exclusion is for rigor or extraction.').

omega_variable(
    credentialing_extraction_vs_quality,
    'Is the academic credentialing system primarily a quality control mechanism for scientific knowledge, or does it function to maintain an extractive monopoly on legitimate interpretation of human origins?',
    'Analysis of cases where non-credentialed individuals produce high-quality, peer-reviewable research that is initially dismissed due to lack of credentials, or where credentialed individuals produce low-quality work that is protected by their status.',
    'If credentialing primarily serves as an extractive monopoly, the measured extractiveness and suppression are amplified; if it is primarily quality control, they are damped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credentialing_extraction_vs_quality, empirical, 'Credentialing as quality control vs. extractive monopoly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__naturalist_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t1950, anthropological_record__naturalist_reading, theater_ratio, 1950, 0.25).
narrative_ontology:measurement(anth_tr_t1965, anthropological_record__naturalist_reading, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(anth_tr_t1980, anthropological_record__naturalist_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(anth_tr_t1995, anthropological_record__naturalist_reading, theater_ratio, 1995, 0.38).
narrative_ontology:measurement(anth_tr_t2010, anthropological_record__naturalist_reading, theater_ratio, 2010, 0.39).
narrative_ontology:measurement(anth_tr_t2024, anthropological_record__naturalist_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(anth_be_t1950, anthropological_record__naturalist_reading, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(anth_be_t1965, anthropological_record__naturalist_reading, base_extractiveness, 1965, 0.6).
narrative_ontology:measurement(anth_be_t1980, anthropological_record__naturalist_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(anth_be_t1995, anthropological_record__naturalist_reading, base_extractiveness, 1995, 0.68).
narrative_ontology:measurement(anth_be_t2010, anthropological_record__naturalist_reading, base_extractiveness, 2010, 0.69).
narrative_ontology:measurement(anth_be_t2024, anthropological_record__naturalist_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t1950, anthropological_record__naturalist_reading, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(anth_su_t1965, anthropological_record__naturalist_reading, suppression_requirement, 1965, 0.7).
narrative_ontology:measurement(anth_su_t1980, anthropological_record__naturalist_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(anth_su_t1995, anthropological_record__naturalist_reading, suppression_requirement, 1995, 0.78).
narrative_ontology:measurement(anth_su_t2010, anthropological_record__naturalist_reading, suppression_requirement, 2010, 0.79).
narrative_ontology:measurement(anth_su_t2024, anthropological_record__naturalist_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__naturalist_reading, information_standard).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, science_funding_allocation).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, public_education_curriculum).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__creationist_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'anthropological_record' kernel. Its ε value differs significantly from the creationist and indigenous epistemology readings due to its specific claims about knowability and its institutionalized enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
