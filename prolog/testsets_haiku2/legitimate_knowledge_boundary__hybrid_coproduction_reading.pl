% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__hybrid_coproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__hybrid_coproduction_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: legitimate_knowledge_boundary__hybrid_coproduction_reading
 *   human_readable: Hybrid Co-Production Epistemology: Methodological Rigor + Experiential Validity
 *   domain: epistemology/science-technology-studies/political-theory
 *
 * SUMMARY:
 *   The hybrid co-production reading asserts that legitimate knowledge
 *   requires both methodological rigor (systematic validation, documented
 *   procedures, reproducibility standards) AND experiential validity (lived
 *   knowledge, community-vetted understanding, situated expertise). This
 *   reading has emerged as a dominant legitimacy framework in social
 *   sciences, public health research, and participatory action research since
 *   ~2010. The constraint enforces this boundary by making co-production
 *   infrastructure—institutional partnerships with communities, formal
 *   co-investigator roles, community approval of findings—a requirement for
 *   funding, publication, and institutional legitimacy. This story models the
 *   hybrid reading's operation as a tangled_rope: it coordinates a genuine
 *   need (integrating historically siloed knowledge systems) while extracting
 *   labor from communities and displacing credentialed expertise, both
 *   asymmetrically. The claim/metric gap is intentional: the reading is
 *   CLAIMED as tangled_rope (recognizing both coordination and extraction)
 *   while metrics show moderate extractiveness (0.42) and substantial
 *   suppression (0.58)—the engine will measure whether the authored claim and
 *   metrics cohere or diverge.
 *
 * KEY AGENTS:
 *   - Co-production researchers and institutional adopters: agenda-setters who design and enforce the hybrid standard
 *   - Traditional credentialed experts: powerful but now constrained; exit is limited because funding and publication increasingly require co-production
 *   - Experiential communities: moderate power, identity-locked (participation framed as liberation); they pay research labor but also benefit from voice
 *   - Funding bodies: institutional agenda-setters enforcing the standard as condition of grants
 *   - Disciplinary gatekeepers (excluded): fields that maintain credentialist legitimacy pathways; their alternative standards are not recognized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.42).
domain_priors:suppression_score(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.58).
domain_priors:theater_ratio(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__hybrid_coproduction_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__hybrid_coproduction_reading, "Hybrid Co-Production Epistemology: Methodological Rigor + Experiential Validity").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__hybrid_coproduction_reading, "epistemology/science-technology-studies/political-theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__hybrid_coproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'b54b86da-67e0-40ed-ab5b-12bbab1c25f2').
narrative_ontology:cs_kernel_codification('b54b86da-67e0-40ed-ab5b-12bbab1c25f2', distributed).
narrative_ontology:cs_authority_grounding('b54b86da-67e0-40ed-ab5b-12bbab1c25f2', distributed).
narrative_ontology:cs_reading_relation('b54b86da-67e0-40ed-ab5b-12bbab1c25f2', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('b54b86da-67e0-40ed-ab5b-12bbab1c25f2', legitimate_knowledge_boundary__experiential_pluralism_reading, coexists_with).
narrative_ontology:cs_axiom('b54b86da-67e0-40ed-ab5b-12bbab1c25f2', foundational, methodological_and_experiential_both_necessary).
narrative_ontology:cs_axiom_status(methodological_and_experiential_both_necessary, holdable).
narrative_ontology:cs_axiom_grounding('b54b86da-67e0-40ed-ab5b-12bbab1c25f2', methodological_and_experiential_both_necessary, empirically_contingent).
narrative_ontology:cs_axiom('b54b86da-67e0-40ed-ab5b-12bbab1c25f2', foundational, legitimacy_requires_dual_validation).
narrative_ontology:cs_axiom_status(legitimacy_requires_dual_validation, holdable).
narrative_ontology:cs_axiom_grounding('b54b86da-67e0-40ed-ab5b-12bbab1c25f2', legitimacy_requires_dual_validation, deontological).
narrative_ontology:cs_reference_frame('b54b86da-67e0-40ed-ab5b-12bbab1c25f2', epistemic_justice_integration_framework).
narrative_ontology:cs_drift_state('b54b86da-67e0-40ed-ab5b-12bbab1c25f2', contemporary_institutionalization_phase, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b54b86da-67e0-40ed-ab5b-12bbab1c25f2', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_researchers).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, academic_institutions_adopting_hybrid_methods).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, traditional_credentialed_experts).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, experiential_communities_bearing_validation_labor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, experiential_communities).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, experiential_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Academic researchers and research institutions that have adopted hybrid co-production methodologies, integrating community expertise with academic rigor. They set standards for what counts as legitimate knowledge by mandating co-production process requirements, institutional review, and dual-validation infrastructure. They collect legitimacy gains (publications, funding, institutional prestige) from administering the hybrid framework.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_researchers, agenda_setter,
    institutional, generational, mobile, national).

% Disciplinary experts whose authority historically derived from credentialed peer review alone. Under the hybrid framework, their knowledge claims require co-production validation; their credentials alone no longer suffice. They must either integrate community partners into their research or have their work devalued in institutional assessment. Exit options are constrained: abandoning hybrid methodologies risks marginalization in funding and publication venues increasingly enforcing the standard.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, traditional_credentialed_experts, payer,
    powerful, biographical, constrained, national).

% Communities whose lived experience is the subject of research (patients, affected populations, social movements, marginalized groups). They gain legitimacy for their knowledge and voice in research design and validation. They also bear the labor cost of co-production: attending meetings, validating findings, teaching researchers about their experiential reality, negotiating research questions. Identity-locked: their participation is framed as essential to their own liberation/empowerment, making exit feel like abandonment of collective welfare even when co-production labor is extractive.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, experiential_communities, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, experiential_communities, beneficiary).

% Universities and research centers that adopt co-production frameworks gain reputation for social relevance, access to community-engaged funding streams, and institutional differentiation in a competitive market. They benefit from enforcing the hybrid standard because it creates a competitive moat: institutions with established community partnerships can produce legitimized research faster than competitors without those relationships.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, academic_institutions_adopting_hybrid_methods, beneficiary,
    institutional, generational, mobile, national).

% Government agencies and philanthropies that fund research increasingly mandate co-production as a condition of grants. They set the bar for what legitimacy looks like by refusing to fund research that does not meet co-production standards. They hold enforcement power but are themselves constrained by political pressure to demonstrate social impact and equity in research.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, funding_bodies_enforcing_coproduction, agenda_setter,
    institutional, generational, analytical, national).

% Journal editors, peer reviewers, and discipline leaders in fields slow to adopt co-production (physics, mathematics, classical philosophy). They are not seated at the hybrid framework's table because their disciplines have not yet mandated co-production. They would contest the framework's claim that methodological rigor requires community validation; their exclusion is structural—they are gatekeepers of alternative legitimacy pathways that the hybrid framework does not recognize.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, disciplinary_gatekeepers, excluded,
    powerful, biographical, constrained, global).

% Philosophers, STS scholars, and policy analysts who study legitimacy standards. They document how co-production claims are authored, who benefits, and whether the framework achieves its stated goals of equity and knowledge democratization or reproduces extraction under a new legitimacy cover.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, epistemology_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__hybrid_coproduction_reading, co_production_researchers).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__hybrid_coproduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine coordination problem: how to integrate methodological standards (which catch systematic bias, enable replication, surface hidden assumptions) with experiential knowledge (which is often more accurate about lived conditions than external credentialed research). Co-production coordinates between two knowledge systems that historically ignored each other, creating a shared validation language.
% TRANSFER_FUNCTION: Moves research authority, decision-making power, and legitimacy from credentialed experts alone to a hybrid seat that requires community presence. Also moves research labor from researchers alone to researchers-plus-community co-investigators. Moves funding and institutional prestige toward institutions and researchers who can form and maintain community partnerships.
% ABSENT_VOICES: Disciplinary experts in fields that have not adopted co-production are structurally excluded—their alternative legitimacy frameworks are not represented in discussions of what counts as rigorous knowledge. Communities exhausted by research participation (burnout, repeated extraction cycles) are seated as payers but their voice about co-production's costs is rarely integrated into standard-setting.
% DISAPPEARANCE_RATIONALE: If the hybrid co-production mandate disappeared, funding would flow back to traditional credentialed research; institutional incentives would reward disciplinary expertise without community partnership requirements; research would accelerate in traditional pathways but lose community input. The knowledge landscape would reorganize around the older legitimacy boundary (methodological rigor alone).
% FOUNDING_PROBLEM: Credentialed research historically ignored or misrepresented lived experience; communities lacked voice in defining research questions and validating findings about their own lives. Co-production emerged as a response to this power asymmetry and epistemic injustice.
% FOUNDING_PROBLEM_CORROBORATION: Community organizations, patient advocacy groups, and social movements independently attest that research without co-production remains disrespectful and inaccurate to their experience. This corroboration comes from outside the agenda-setter group and is documented in community research networks, participatory action research literature, and decolonial scholarship.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__hybrid_coproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__hybrid_coproduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).
:- end_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the framework genuinely solves a coordination problem—integrating two knowledge systems that were previously ignored each other—while simultaneously extracting labor from communities (attend meetings, validate findings, teach researchers) and displacing credentialed experts (their credentials alone no longer suffice). The trend upward (0.22→0.42 over 25 time units) reflects the accumulation of these extraction mechanisms as co-production becomes standardized: early adoption was voluntary and small-scale; as funding bodies mandate it and institutions invest infrastructure, the extraction mechanisms become compulsory and diffuse. Suppression (0.58) is high because the framework's persistence depends on actively excluding and marginalizing alternative legitimacy pathways (traditional peer review in classical disciplines, experiential knowledge without methodological framing). Theater (0.38) is moderate: the community-engagement and validation language is real and functional, but a growing share of enforcement activity defends the co-production requirement rather than improving knowledge integration. Accessibility_collapse (0.62) reflects that once understood, the framework substantially forecloses alternatives: researchers must either co-produce or accept institutional devaluation; communities must participate or see their knowledge unlegitimized. Resistance (0.68) is substantial: credentialed experts continue to work outside co-production where possible; traditional disciplines resist the mandate; communities organize against extractive co-production partnerships.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (co-production researchers, institutional adopters, funding bodies) and the payer seats (credentialed experts, experiential communities) compute structurally different classifications from the same constraint. From the agenda-setter position, the arrangement is genuine coordination they have built and are continuously improving through better partnership models and accountability standards. From the credentialed expert seat, the same structure operates as delegitimization and coerced participation. From the community seat, the structure offers voice and authority while demanding unpaid labor and identity-fusion (co-production becomes inseparable from their sense of empowerment). The engine derives these differences from the authored stakeholder positions (power, exit, roles) without requiring each to be pre-announced. Overrides are not necessary: the directionality chain (beneficiary/victim + exit + power) produces the right d values for each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Co-production researchers and institutional adopters are beneficiaries (they set the standard, collect legitimacy gains, mobile exit options). Traditional credentialed experts are targets (constrained by funding/publication requirements that now include co-production; their exit is limited because alternative credentialist pathways are being marginalized). Experiential communities are dual-positioned: they benefit from voice and validated knowledge (role: beneficiary) but pay through research labor and identity-lock (role: payer). Identity-lock is the key dynamic: communities' participation is framed as essential to their empowerment and justice, making exit feel like betrayal even when co-production labor is extractive. Funding bodies are also agenda-setters: they enforce through resource control. Disciplinary gatekeepers are excluded: their alternative legitimacy frameworks are not seated in the co-production deliberation.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid co-production reading avoids mandatrophy by explicitly recognizing that it COORDINATES (solves the genuine problem of integrating methodological and experiential knowledge) while EXTRACTING (displaces traditional expertise, extracts community labor, enforces participation through institutional requirement). This is exactly the tangled_rope structure: both functions are real and both must be named. A pure-extraction reading (snare) would miss the coordination problem the framework solves. A pure-coordination reading (rope) would miss the extraction mechanisms. The tangled_rope classification holds the structure: it is coordination that requires active enforcement (suppression of alternatives) to persist, and extraction that coordinates knowledge systems rather than pure coercion. The upward trend in extractiveness (0.22→0.42) shows the constraint becoming more entrenched as it's institutionalized; the theater_ratio rising (0.18→0.38) shows more of the enforcement activity devoted to defending the requirement rather than improving integration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_coordination_boundary,
    'Is the community labor required for co-production a necessary cost of integrating knowledge systems, or is it extractive overhead riding on a coordinated framework?',
    'Longitudinal study of community participants'' assessment of co-production relationships: track whether communities perceive labor as fair exchange (get validated voice + influence) or as uncompensated work (do research labor for academic legitimacy gains they don''t capture).',
    'If labor is perceived as fair exchange (proportional to voice gained), the tangled_rope classification holds but the extraction component is lower. If labor is perceived as uncompensated, the constraint moves toward snare—coordination framing covers extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Whether community co-production labor is equitable exchange or structured extraction.').

omega_variable(
    suppression_mechanism_identity_fusion,
    'Is the suppression of alternative legitimacy pathways structural (external barriers, funding rules, publication gates) or internalized (communities believe co-production is essential to their empowerment, making exit feel like betrayal)?',
    'Post-exit analysis: communities that withdraw from co-production relationships report whether their sense of empowerment persists or collapses; if it persists, suppression was mostly structural; if it collapses, identity-fusion has internalized suppression.',
    'If suppression is mostly structural, alternative pathways could be restored by funding policy change. If suppression is internalized through identity-fusion, alternatives would need to dissolve the learned belief that co-production is liberation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_fusion, empirical, 'Structural vs. internalized suppression in community participation.').

omega_variable(
    methodological_experiential_separability,
    'Are methodological rigor and experiential validity structurally separable, or does each inherently require the other?',
    'Philosophical and empirical analysis: can research be methodologically rigorous without experiential validation? Can experiential knowledge be validated without methodological standards? Examine cases in each tradition.',
    'If separable, the co-production mandate is a policy choice (legitimacy boundary chosen by agenda-setters) rather than a natural law. If inseparable, co-production reflects real epistemic dependencies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(methodological_experiential_separability, conceptual, 'Whether methodological and experiential knowledge systems are conceptually independent.').

omega_variable(
    reading_contest_kernel_identity,
    'Is the legitimate_knowledge_boundary kernel a genuine fissure in epistemology (real parties holding incommensurable standards) or a STS construction (scholars have created the three readings as a framework)?',
    'Historical and discourse analysis: what did credentialed experts, experiential communities, and hybrid researchers believe BEFORE the readings were articulated? Did the readings discover existing divisions or create them?',
    'If genuine fissure, the readings map pre-existing epistemic controversy. If constructed, the kernel is partly an artifact of scholarly attention and framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_kernel_identity, conceptual, 'Whether the kernel represents discovered epistemic divisions or scholarly construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement_basis(legi_tr_t5, observed).
narrative_ontology:measurement(legi_tr_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(legi_tr_t10, observed).
narrative_ontology:measurement(legi_tr_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(legi_tr_t15, observed).
narrative_ontology:measurement(legi_tr_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement_basis(legi_tr_t20, observed).
narrative_ontology:measurement(legi_tr_t25, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement_basis(legi_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement_basis(legi_be_t5, observed).
narrative_ontology:measurement(legi_be_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement_basis(legi_be_t10, observed).
narrative_ontology:measurement(legi_be_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement_basis(legi_be_t15, observed).
narrative_ontology:measurement(legi_be_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement_basis(legi_be_t20, observed).
narrative_ontology:measurement(legi_be_t25, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement_basis(legi_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 5, 0.44).
narrative_ontology:measurement_basis(legi_su_t5, observed).
narrative_ontology:measurement(legi_su_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 10, 0.51).
narrative_ontology:measurement_basis(legi_su_t10, observed).
narrative_ontology:measurement(legi_su_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement_basis(legi_su_t15, observed).
narrative_ontology:measurement(legi_su_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement_basis(legi_su_t20, observed).
narrative_ontology:measurement(legi_su_t25, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(legi_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__hybrid_coproduction_reading, information_standard).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.12).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary__credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary__experiential_pluralism_reading).

% DUAL FORMULATION NOTE:
% The legitimate_knowledge_boundary kernel has three distinct readings, each with different extractiveness profiles and structural dependencies. The credentialed_expertise_reading treats co-production as methodological excess (extraction rising as co-production is mandated). The experiential_pluralism_reading treats methodological standards as tool colonialism (extraction of indigenous/community knowledge into academic frameworks). This reading (hybrid_coproduction) treats both as necessary and modeling the coordination/extraction mix explicitly. All three are linked by network.affects_constraints because the legitimacy standard-setting in one reading directly constrains the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_knowledge_boundary__hybrid_coproduction_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
