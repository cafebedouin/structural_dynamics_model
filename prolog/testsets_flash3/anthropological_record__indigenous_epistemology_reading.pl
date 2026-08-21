% ============================================================================
% CONSTRAINT STORY: anthropological_record__indigenous_epistemology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__indigenous_epistemology_reading, []).

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
 *   constraint_id: anthropological_record__indigenous_epistemology_reading
 *   human_readable: Anthropological Record: Indigenous Epistemology Reading
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This constraint represents the 'indigenous epistemology' reading of the
 *   anthropological record, where relational continuity with ancestors and
 *   place, knowable via sustained oral tradition, is central. It asserts that
 *   material evidence is insufficient without oral tradition and subordinates
 *   both credentialed scientific and scriptural frameworks to community
 *   authority over ancestral remains. The constraint is claimed as a Rope,
 *   reflecting its function in coordinating ethical research and knowledge
 *   integration, but carries a moderate extractiveness and suppression as it
 *   requires established scientific practices to cede authority and adapt
 *   methodologies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__indigenous_epistemology_reading, 0.3).
domain_priors:suppression_score(anthropological_record__indigenous_epistemology_reading, 0.4).
domain_priors:theater_ratio(anthropological_record__indigenous_epistemology_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__indigenous_epistemology_reading, rope).
narrative_ontology:human_readable(anthropological_record__indigenous_epistemology_reading, "Anthropological Record: Indigenous Epistemology Reading").
narrative_ontology:topic_domain(anthropological_record__indigenous_epistemology_reading, "epistemology/philosophy_of_science/anthropology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__indigenous_epistemology_reading, '1c412964-e06e-493c-b78c-5b4061f99fb4').
narrative_ontology:cs_kernel_codification('1c412964-e06e-493c-b78c-5b4061f99fb4', distributed).
narrative_ontology:cs_authority_grounding('1c412964-e06e-493c-b78c-5b4061f99fb4', practice).
narrative_ontology:cs_interpretation_layer_present('1c412964-e06e-493c-b78c-5b4061f99fb4').
narrative_ontology:cs_reading_relation('1c412964-e06e-493c-b78c-5b4061f99fb4', anthropological_record__naturalist_reading, influences).
narrative_ontology:cs_reading_relation('1c412964-e06e-493c-b78c-5b4061f99fb4', anthropological_record__creationist_reading, forecloses).
narrative_ontology:cs_axiom('1c412964-e06e-493c-b78c-5b4061f99fb4', foundational, oral_tradition_as_primary_historical_source).
narrative_ontology:cs_axiom_status(oral_tradition_as_primary_historical_source, holdable).
narrative_ontology:cs_axiom_grounding('1c412964-e06e-493c-b78c-5b4061f99fb4', oral_tradition_as_primary_historical_source, conventional).
narrative_ontology:cs_axiom('1c412964-e06e-493c-b78c-5b4061f99fb4', foundational, relational_ontology_of_ancestors_and_place).
narrative_ontology:cs_axiom_status(relational_ontology_of_ancestors_and_place, holdable).
narrative_ontology:cs_axiom_grounding('1c412964-e06e-493c-b78c-5b4061f99fb4', relational_ontology_of_ancestors_and_place, deontological).
narrative_ontology:cs_reference_frame('1c412964-e06e-493c-b78c-5b4061f99fb4', indigenous_community_epistemic_sovereignty).
narrative_ontology:cs_drift_state('1c412964-e06e-493c-b78c-5b4061f99fb4', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('1c412964-e06e-493c-b78c-5b4061f99fb4', '').
narrative_ontology:cs_kernel_id(anthropological_record__indigenous_epistemology_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, indigenous_communities).
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, decolonizing_anthropologists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, mainstream_archaeologists).
narrative_ontology:constraint_vindicates(anthropological_record__indigenous_epistemology_reading, oral_tradition_as_valid_knowledge).
narrative_ontology:constraint_vindicates(anthropological_record__indigenous_epistemology_reading, relational_ontology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold and transmit oral traditions that define their relationship to ancestors and land. They assert authority over the interpretation of archaeological and genetic evidence pertaining to their heritage, often requiring material evidence to be contextualized by their narratives.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, indigenous_communities, agenda_setter,
    organized, generational, identity_locked, local).

% Advocate for the inclusion of indigenous epistemologies in the interpretation of the anthropological record. They benefit from new research avenues and ethical frameworks that prioritize community engagement and indigenous self-determination, but face resistance from mainstream academic institutions.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, decolonizing_anthropologists, beneficiary,
    moderate, biographical, constrained, global).

% Are challenged to integrate oral traditions and community authority into their scientific practices, which traditionally prioritize material evidence and peer-reviewed publications. This often requires re-evaluating established methodologies and relinquishing sole interpretive authority over ancestral remains and sites.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, mainstream_archaeologists, payer,
    institutional, biographical, constrained, global).

% Adhere to a materialist view of human origins and knowledge acquisition, often viewing oral traditions as cultural narratives rather than valid historical or scientific data. They are often excluded from collaborative projects that prioritize indigenous epistemologies, or their interpretations are subordinated.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, naturalist_scientists, excluded,
    institutional, generational, mobile, universal).

% Promote interpretations of human origins based on scriptural accounts, which are fundamentally incompatible with both indigenous oral traditions and mainstream scientific methods. They are excluded from the discourse of this reading due to irreconcilable foundational premises.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, creationist_advocates, excluded,
    organized, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the interpretation of human origins and ancestral heritage by integrating material evidence with sustained oral traditions, ensuring that scientific findings are contextualized within community-held knowledge and relational ontologies.
% TRANSFER_FUNCTION: Transfers interpretive authority over ancestral remains and cultural heritage from purely scientific institutions to indigenous communities, alongside the responsibility for ethical stewardship and knowledge transmission.
% ABSENT_VOICES: Naturalist scientists and creationist advocates are largely absent from the core interpretive process of this reading. Naturalists would argue for the primacy of empirical, material evidence; creationists would assert scriptural authority. Both are subordinated to community authority over ancestral remains in this framework.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the interpretation of the anthropological record would revert to a more purely materialist or scriptural framework, disempowering indigenous communities and severing the relational continuity with ancestors and place that oral traditions maintain. Research ethics, repatriation efforts, and community engagement would be fundamentally altered.
% FOUNDING_PROBLEM: The historical marginalization and misinterpretation of indigenous knowledge systems by colonial science, leading to the desecration of ancestral sites and the misrepresentation of indigenous histories.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous communities universally attest to the ongoing nature of this problem, citing continued struggles for repatriation, protection of sacred sites, and recognition of their knowledge. Decolonizing anthropologists corroborate this through their research and advocacy, acknowledging the historical injustices and ongoing power imbalances.
narrative_ontology:disappearance_verdict(anthropological_record__indigenous_epistemology_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__indigenous_epistemology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__indigenous_epistemology_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(anthropological_record__indigenous_epistemology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__indigenous_epistemology_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__indigenous_epistemology_reading_tests).
:- end_tests(anthropological_record__indigenous_epistemology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.3) is moderate because while it reallocates interpretive authority, it doesn't impose overwhelming costs on all parties; rather, it redefines the terms of engagement. Suppression (0.4) is also moderate, reflecting the active resistance from mainstream institutions and the need for indigenous communities to assert and defend their epistemological claims. Theater ratio (0.1) is low, as the efforts to integrate indigenous knowledge are generally genuine, though sometimes performative aspects exist in early stages of engagement. Accessibility collapse (0.6) is moderate, as it closes off purely materialist or scriptural interpretations as sole authorities, but opens new avenues for collaborative research. Resistance (0.3) is present from those whose traditional authority is challenged. The increasing extractiveness and suppression over time reflect the growing assertiveness of indigenous communities and the increasing pressure on mainstream science to comply.
 *
 * PERSPECTIVAL GAP:
 *   Indigenous communities experience this as a necessary and just rebalancing of power and knowledge, a genuine coordination of diverse epistemologies. Mainstream archaeologists may experience it as an imposition on scientific autonomy, a form of extraction of their traditional interpretive rights. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous communities are the primary agenda-setters and beneficiaries, gaining interpretive authority and control over their heritage. Decolonizing anthropologists benefit from new ethical frameworks and research opportunities. Mainstream archaeologists are payers, as they must adapt their methods and cede some authority. Naturalist scientists and creationist advocates are excluded, as their epistemological premises are either subordinated or deemed incompatible with this reading's foundational axioms.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemological_incommensurability,
    'To what extent are indigenous oral traditions and Western scientific methods truly incommensurable, or can they be genuinely integrated without one subordinating the other?',
    'Longitudinal studies of collaborative research projects: success in producing mutually enriching knowledge without epistemic compromise would suggest commensurability; persistent conflict or tokenism would suggest incommensurability.',
    'If incommensurable, the constraint''s coordination function is more performative (higher theater_ratio) and its suppression of alternative epistemologies is more fundamental. If commensurable, its coordination function is robust and its extractiveness is a transitional cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemological_incommensurability, conceptual, 'The degree to which different epistemologies can genuinely integrate.').

omega_variable(
    authority_transfer_legitimacy,
    'Is the transfer of interpretive authority to indigenous communities a legitimate act of decolonization, or an undue imposition on scientific autonomy?',
    'Analysis of international human rights law, indigenous rights declarations, and ethical guidelines for research with indigenous peoples. Examination of historical power imbalances and their ongoing effects.',
    'If legitimate decolonization, the constraint''s extractiveness from mainstream science is justified as restorative justice. If an undue imposition, the extractiveness is a form of reverse suppression, potentially reclassifying the constraint from a Rope to a Tangled Rope from the perspective of mainstream science.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_transfer_legitimacy, preference, 'Normative justification for the reallocation of interpretive authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__indigenous_epistemology_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t1970, anthropological_record__indigenous_epistemology_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(anth_tr_t1985, anthropological_record__indigenous_epistemology_reading, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(anth_tr_t2000, anthropological_record__indigenous_epistemology_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(anth_tr_t2010, anthropological_record__indigenous_epistemology_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(anth_tr_t2024, anthropological_record__indigenous_epistemology_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(anth_be_t1970, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement(anth_be_t1985, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 1985, 0.2).
narrative_ontology:measurement(anth_be_t2000, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(anth_be_t2010, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 2010, 0.28).
narrative_ontology:measurement(anth_be_t2024, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t1970, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(anth_su_t1985, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 1985, 0.3).
narrative_ontology:measurement(anth_su_t2000, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(anth_su_t2010, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 2010, 0.38).
narrative_ontology:measurement(anth_su_t2024, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__indigenous_epistemology_reading, identity_coordination).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, indigenous_land_rights_recognition).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, cultural_heritage_repatriation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
