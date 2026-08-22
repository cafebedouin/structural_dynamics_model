% ============================================================================
% CONSTRAINT STORY: anthropological_record__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Naturalist Reading of the Anthropological Record
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   The naturalist reading of the anthropological record asserts that human
 *   origins — evolution, migration, population history — are material
 *   processes knowable through scientific method (fossil evidence, genetics,
 *   archaeology, dating techniques). This reading achieved institutional
 *   dominance in the late 19th century and now structures funding, hiring,
 *   publication, and curricula across anthropology and biology. The
 *   constraint operates as a coordination mechanism: it provides a shared
 *   evidentiary framework that enables cumulative research. But it also
 *   functions extractively: credentialing gates (PhD programs, peer review,
 *   grant panels) concentrate resources and authority among practitioners who
 *   accept methodological naturalism, while excluding creationist researchers
 *   (who posit divine causation), indigenous knowledge holders (who ground
 *   knowledge in oral tradition and relational ontology), and independent
 *   scholars outside the academy. Suppression is not primarily legal — it is
 *   epistemic: non-naturalist frameworks are treated as 'not science' rather
 *   than 'false science,' which denies them access to the material
 *   infrastructure of knowledge production (journals, grants, museum
 *   collections, field permits). Theater ratio has risen as the coordination
 *   function matured: early on, the constraint solved a genuine fragmentation
 *   problem; now a growing share of enforcement energy maintains boundary
 *   purity against challenges (intelligent design, indigenous data
 *   sovereignty movements, decolonial critiques) rather than advancing the
 *   core research program.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__naturalist_reading, 0.68).
domain_priors:suppression_score(anthropological_record__naturalist_reading, 0.62).
domain_priors:theater_ratio(anthropological_record__naturalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__naturalist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__naturalist_reading, "Naturalist Reading of the Anthropological Record").
narrative_ontology:topic_domain(anthropological_record__naturalist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__naturalist_reading, '56ebd68e-262a-4268-a975-f6f1e63d7c52').
narrative_ontology:cs_kernel_codification('56ebd68e-262a-4268-a975-f6f1e63d7c52', formalized).
narrative_ontology:cs_authority_grounding('56ebd68e-262a-4268-a975-f6f1e63d7c52', expertise).
narrative_ontology:cs_interpretation_layer_present('56ebd68e-262a-4268-a975-f6f1e63d7c52').
narrative_ontology:cs_reading_relation('56ebd68e-262a-4268-a975-f6f1e63d7c52', anthropological_record__creationist_reading, forecloses).
narrative_ontology:cs_reading_relation('56ebd68e-262a-4268-a975-f6f1e63d7c52', anthropological_record__indigenous_epistemology_reading, influences).
narrative_ontology:cs_axiom('56ebd68e-262a-4268-a975-f6f1e63d7c52', foundational, methodological_naturalism).
narrative_ontology:cs_axiom_status(methodological_naturalism, holdable).
narrative_ontology:cs_axiom_grounding('56ebd68e-262a-4268-a975-f6f1e63d7c52', methodological_naturalism, empirically_contingent).
narrative_ontology:cs_axiom('56ebd68e-262a-4268-a975-f6f1e63d7c52', foundational, common_descent).
narrative_ontology:cs_axiom_status(common_descent, holdable).
narrative_ontology:cs_axiom_grounding('56ebd68e-262a-4268-a975-f6f1e63d7c52', common_descent, empirically_contingent).
narrative_ontology:cs_reference_frame('56ebd68e-262a-4268-a975-f6f1e63d7c52', modern_evolutionary_synthesis).
narrative_ontology:cs_drift_state('56ebd68e-262a-4268-a975-f6f1e63d7c52', contemporary_decolonial_critique_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('56ebd68e-262a-4268-a975-f6f1e63d7c52', '').
narrative_ontology:cs_kernel_id(anthropological_record__naturalist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, credentialed_paleoanthropologists).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, evolutionary_biologists).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, university_anthropology_departments).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, national_science_funding_agencies).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, peer_reviewed_journals).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, creationist_researchers).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, indigenous_knowledge_holders).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, independent_scholars_outside_academia).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, community_based_oral_historians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold PhDs and academic positions; their research careers depend on the naturalist framework's dominance. They compete for grants, publications, and tenure within the constraint's rules. Exit means leaving the discipline or moving to adjacent fields (anatomy, geology) where the same naturalist framework prevails. They benefit materially (salary, grants, prestige) and epistemically (their work counts as science).
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, credentialed_paleoanthropologists, beneficiary,
    organized, biographical, constrained, global).

% Use the anthropological record as testbed for evolutionary theory (population genetics, speciation, adaptation). Their theoretical framework is vindicated by the record; they benefit from the constraint's epistemic authority. Exit is constrained — evolutionary biology has no non-naturalist research program. They are not gatekeepers but their work reinforces the boundary.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, evolutionary_biologists, beneficiary,
    organized, biographical, constrained, global).

% Administer PhD programs, hiring, tenure, and curriculum. They set the credentialing standards that define who counts as a legitimate interpreter. They could change the rules (e.g., hire indigenous elders as faculty, create dual-epistemology tracks) but face institutional incentives not to: rankings, accreditation, donor expectations, and disciplinary identity all reward boundary maintenance. Their exit option is high — they could restructure — but they are the ones who would have to initiate it.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, university_anthropology_departments, agenda_setter,
    institutional, generational, arbitrage, global).

% Allocate research funding (NSF, NIH, ERC, etc.). Their peer-review panels enforce methodological naturalism as a funding criterion. They could fund alternative frameworks (indigenous co-production, creationist research) but treat such proposals as 'not science.' Exit is high — Congress or Parliament could mandate pluralism — but agencies actively resist such mandates as politicization.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, national_science_funding_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Control publication gateways (Nature, Science, JHE, AJPA, etc.). Editors and reviewers enforce the naturalist boundary: papers invoking supernatural causation or oral tradition as primary evidence are desk-rejected or sent to 'specialty' venues. Journals benefit from the constraint (submissions, citations, impact factor) but are also trapped by it — a journal that published creationist papers would lose its scientific standing. Their exit is constrained by the same epistemic economy they administer.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, peer_reviewed_journals, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, peer_reviewed_journals, beneficiary).

% Hold advanced degrees (often in relevant fields: geology, biology, paleontology) but interpret the record through a creationist framework. They are excluded from mainstream funding, publication, and academic positions. Their identity is fused to their framework — leaving it would mean abandoning their theological commitments and community. They operate parallel institutions (creationist journals, museums, conferences) but these lack the material infrastructure (collections, sequencing labs, dating facilities) that the naturalist constraint controls.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, creationist_researchers, payer,
    moderate, biographical, identity_locked, global).

% Hold oral traditions, place-based knowledge, and ancestral narratives that constitute a distinct epistemology of human origins. Their knowledge is often treated as 'data' for naturalist science (genetic sampling, archaeological consultation) while their interpretive authority is denied. They cannot exit their epistemology — it is constitutive of their identity, sovereignty, and relationship to land. They seek co-governance of collections and co-authorship of narratives, but the constraint's gatekeeping structure offers only 'consultation' roles.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, indigenous_knowledge_holders, payer,
    moderate, generational, identity_locked, continental).

% Researchers without institutional affiliation who work within or adjacent to naturalist frameworks. They lack access to grants, museum collections, sequencing labs, and field permits — all gated by the credentialing structure. Their exit is trapped: they cannot acquire the credentials without the institution, and the institution excludes them. Some publish in lower-tier journals or self-publish, but their work lacks epistemic standing in the constraint's economy.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, independent_scholars_outside_academia, payer,
    powerless, biographical, trapped, global).

% Practitioners of indigenous or local oral history traditions who document community narratives of origin and migration. They are structurally excluded from the constraint's epistemic infrastructure: their work is not citable in peer-reviewed journals, not fundable by science agencies, not recognized by university departments. Their identity is bound to the community's epistemic practice; exit means abandoning their role. They are the most suppressed seat — their knowledge is extracted (as 'ethnographic data') while their authority is denied.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, community_based_oral_historians, payer,
    powerless, biographical, identity_locked, local).

% Analyze the epistemic structure, boundary maintenance, and normative commitments of the naturalist reading from outside the research practice. They do not collect grants or publish in paleoanthropology journals. Their role is to map the constraint's logic, not to inhabit it. They see the full structure: the coordination function, the extraction, the suppressed alternatives.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, philosophers_of_science, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__naturalist_reading, university_anthropology_departments).
narrative_ontology:fixing_cost_class(anthropological_record__naturalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified evidentiary framework (fossils, genetics, dating, archaeology) that enables cumulative, interoperable research on human origins across disciplines and generations. Solves the problem of fragmented, incommensurable accounts by establishing methodological naturalism as the shared epistemic standard.
% TRANSFER_FUNCTION: Moves research funding, academic positions, publication access, museum collections, field permits, and epistemic authority from excluded frameworks (creationist, indigenous, independent) to credentialed naturalist practitioners and the institutions that employ them.
% ABSENT_VOICES: Indigenous elders and knowledge-keepers who hold origin narratives but are not credentialed; creationist scientists with relevant expertise who are filtered out at hiring and funding stages; community oral historians whose traditions are treated as raw material rather than interpretation. They are absent from the rooms where funding priorities, curation policies, and publication standards are set.
% DISAPPEARANCE_RATIONALE: If the naturalist constraint vanished overnight, the material infrastructure of paleoanthropology (collections, labs, field sites) would not disappear, but the gatekeeping that allocates access would collapse. Creationist and indigenous frameworks would immediately claim equal standing for funding, publication, and curation authority. The research economy would reorganize around pluralist or contested epistemic standards. The coordination function (shared method) would be lost, but the extraction function (credentialing rents) would also dissolve.
% FOUNDING_PROBLEM: In the mid-19th century, human origins research was fragmented among competing theories (polygenism, monogenism, biblical chronology, racial typology) with no shared evidentiary standard. The naturalist reading unified the field under evolutionary theory, deep time, and empirical evidence, enabling cumulative science.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as live by practitioners (the synthetic theory continues to generate novel predictions) and by philosophers of science (methodological naturalism remains the demarcation criterion for science). However, indigenous scholars and decolonial critics attest that the founding problem was *also* a colonial imposition that erased existing knowledge systems — a corroboration from outside the beneficiary set that the problem's status is contested, not settled.
narrative_ontology:disappearance_verdict(anthropological_record__naturalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__naturalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__naturalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(anthropological_record__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__naturalist_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the credentialing gatekeeping that channels funding, positions, and epistemic authority to naturalist practitioners while denying them to alternative frameworks. Suppression (0.62) captures the epistemic exclusion: non-naturalist interpretations are systematically filtered out at every gate (publication, funding, tenure, field access) not because they fail internal coherence tests but because they violate the boundary condition of methodological naturalism. Theater ratio (0.28) has grown over the interval as boundary maintenance consumes more institutional energy relative to novel discovery. Accessibility collapse (0.55) is moderate: alternatives (creationism, indigenous epistemology) persist outside the academy and even within it as critical discourse, but they cannot access the constraint's material infrastructure. Resistance (0.48) is substantial: creationist legal challenges, indigenous data sovereignty movements, and decolonial critiques actively contest the constraint's legitimacy, but they have not fractured the coordination core.
 *
 * PERSPECTIVAL GAP:
 *   From the credentialed practitioner seat (agenda_setter/beneficiary), the constraint is genuine coordination: a shared method that makes cumulative paleoanthropology possible. From the excluded seats (creationist researchers, indigenous knowledge holders), the same structure is a snare: it extracts their labor (specimens, site access, oral histories) while denying them epistemic standing. The engine computes this divergence from the structural data — the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed paleoanthropologists and evolutionary biologists are structural beneficiaries: they collect funding, career advancement, and epistemic authority from the constraint's operation (low directionality). University departments and funding agencies are agenda_setters who administer the gatekeeping apparatus (low directionality, high power). Creationist researchers, indigenous knowledge holders, and independent scholars are victims: they bear the cost of exclusion — denied grants, publication, field access, and institutional recognition — while their labor and data often feed the naturalist research program (high directionality). Peer-reviewed journals occupy a dual position: they benefit from the constraint's quality-control function but also enforce its boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — establishing a unified, evidence-based framework for human origins — was live in 1859-1950 when the constraint consolidated. By the late 20th century, the coordination function was mature: the synthetic theory, radiometric dating, and molecular phylogenetics had solved the core fragmentation. Yet the constraint intensified: credentialing tightened, funding concentrated, and boundary enforcement expanded against intelligent design (1990s-), decolonial critiques (2000s-), and indigenous data sovereignty (2010s-). This is mandatrophy: the arrangement persists and hardens after its founding coordination problem is substantially solved, because the institutional beneficiaries (departments, journals, funding agencies) have a material interest in maintaining the gatekeeping structure. The theater ratio rise tracks this transition from coordination to extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_naturalist,
    'Is the naturalist reading a genuine coordination standard for scientific inquiry, or an extractive gatekeeping mechanism that suppresses alternative epistemic frameworks?',
    'Compare resource allocation and career outcomes for credentialed practitioners vs. excluded interpreters; assess whether supernatural exclusion is a methodological necessity or a boundary-maintenance device.',
    'If coordination, the constraint is a functional rope with modest extraction; if gatekeeping, it is a tangled rope or snare with high asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_naturalist, conceptual, 'Whether the naturalist reading''s credentialing structure coordinates inquiry or extracts from excluded frameworks.').

omega_variable(
    supernatural_exclusion_mechanism,
    'Is the exclusion of supernatural causation a structural feature of scientific method (methodological naturalism as coordination) or a sociological boundary that could be relaxed without epistemic collapse?',
    'Historical analysis of sciences that once admitted teleological language; counterfactual assessment of whether evolutionary biology could function with formal agnosticism on ultimate causation.',
    'If structurally necessary, the exclusion is a coordination cost (lower effective extraction); if sociological, the suppression of creationist/indigenous frameworks is extractive overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supernatural_exclusion_mechanism, conceptual, 'Whether supernatural exclusion is epistemic necessity or sociological boundary.').

omega_variable(
    indigenous_epistemology_incommensurability,
    'Are indigenous oral traditions and naturalist science incommensurable frameworks, or can they be integrated under a pluralist epistemology?',
    'Case studies of successful co-production (e.g., Arctic climate records, Australian fire management); test whether integration requires surrendering naturalist methodological commitments.',
    'If incommensurable, the naturalist reading''s suppression of indigenous epistemology is structurally inevitable; if integrable, the current exclusion is extractive gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_epistemology_incommensurability, empirical, 'Whether indigenous and naturalist epistemologies can coexist without mutual exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__naturalist_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__naturalist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(anth_tr_t25, anthropological_record__naturalist_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(anth_tr_t50, anthropological_record__naturalist_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement(anth_tr_t75, anthropological_record__naturalist_reading, theater_ratio, 75, 0.22).
narrative_ontology:measurement(anth_tr_t100, anthropological_record__naturalist_reading, theater_ratio, 100, 0.25).
narrative_ontology:measurement(anth_tr_t125, anthropological_record__naturalist_reading, theater_ratio, 125, 0.27).
narrative_ontology:measurement(anth_tr_t150, anthropological_record__naturalist_reading, theater_ratio, 150, 0.28).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__naturalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(anth_be_t25, anthropological_record__naturalist_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(anth_be_t50, anthropological_record__naturalist_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(anth_be_t75, anthropological_record__naturalist_reading, base_extractiveness, 75, 0.55).
narrative_ontology:measurement(anth_be_t100, anthropological_record__naturalist_reading, base_extractiveness, 100, 0.61).
narrative_ontology:measurement(anth_be_t125, anthropological_record__naturalist_reading, base_extractiveness, 125, 0.65).
narrative_ontology:measurement(anth_be_t150, anthropological_record__naturalist_reading, base_extractiveness, 150, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__naturalist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(anth_su_t25, anthropological_record__naturalist_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(anth_su_t50, anthropological_record__naturalist_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement(anth_su_t75, anthropological_record__naturalist_reading, suppression_requirement, 75, 0.56).
narrative_ontology:measurement(anth_su_t100, anthropological_record__naturalist_reading, suppression_requirement, 100, 0.58).
narrative_ontology:measurement(anth_su_t125, anthropological_record__naturalist_reading, suppression_requirement, 125, 0.6).
narrative_ontology:measurement(anth_su_t150, anthropological_record__naturalist_reading, suppression_requirement, 150, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__naturalist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(anthropological_record__naturalist_reading, 0.03).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__creationist_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__indigenous_epistemology_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, scientific_funding_peer_review).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, museum_repatriation_policies).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, evolution_education_standards).

% DUAL FORMULATION NOTE:
% Part of the anthropological_record constraint family. This reading (naturalist) shares the referent (the material record of human origins) with creationist_reading and indigenous_epistemology_reading but instantiates a distinct constraint with distinct ε, beneficiaries, victims, and suppression profile. The naturalist reading's institutional dominance creates structural pressure on sibling readings by controlling the epistemic infrastructure they must negotiate with.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(anthropological_record__naturalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(anthropological_record__naturalist_reading, organized, 0.35).
constraint_indexing:directionality_override(anthropological_record__naturalist_reading, moderate, 0.75).
constraint_indexing:directionality_override(anthropological_record__naturalist_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
