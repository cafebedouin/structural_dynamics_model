% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__hybrid_coproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: legitimate_knowledge_boundary__hybrid_coproduction_reading
 *   human_readable: Hybrid Co-Production Legitimacy Standard
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   The hybrid co-production reading asserts that legitimate knowledge
 *   requires BOTH methodological rigor AND experiential validity, integrated
 *   through structured co-production processes (participatory research,
 *   community-based participatory research, patient and public involvement,
 *   transdisciplinary synthesis). This constraint emerged from
 *   late-20th-century legitimacy crises in both credentialed expertise and
 *   experiential knowledge movements. It is enforced through funding
 *   mandates, journal requirements, ethics review expansions, and policy
 *   frameworks. The constraint has genuine coordination value — it integrates
 *   epistemic traditions that were historically segregated — but also
 *   extracts translation labor from both communities and concentrates
 *   agenda-setting power in infrastructure funders. The claimed type is
 *   tangled_rope because both coordination and asymmetric extraction are
 *   structurally present and require active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.52).
domain_priors:suppression_score(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.45).
domain_priors:theater_ratio(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__hybrid_coproduction_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__hybrid_coproduction_reading, "Hybrid Co-Production Legitimacy Standard").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__hybrid_coproduction_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__hybrid_coproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'a8d0801c-b94c-409f-9a25-ef069912ebba').
narrative_ontology:cs_kernel_codification('a8d0801c-b94c-409f-9a25-ef069912ebba', distributed).
narrative_ontology:cs_authority_grounding('a8d0801c-b94c-409f-9a25-ef069912ebba', practice).
narrative_ontology:cs_interpretation_layer_present('a8d0801c-b94c-409f-9a25-ef069912ebba').
narrative_ontology:cs_reading_relation('a8d0801c-b94c-409f-9a25-ef069912ebba', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('a8d0801c-b94c-409f-9a25-ef069912ebba', legitimate_knowledge_boundary__experiential_pluralism_reading, coexists_with).
narrative_ontology:cs_axiom('a8d0801c-b94c-409f-9a25-ef069912ebba', foundational, dual_validation_necessary_for_legitimacy).
narrative_ontology:cs_axiom_status(dual_validation_necessary_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a8d0801c-b94c-409f-9a25-ef069912ebba', dual_validation_necessary_for_legitimacy, instrumental).
narrative_ontology:cs_axiom('a8d0801c-b94c-409f-9a25-ef069912ebba', foundational, coproduction_infrastructure_is_epistemic_prerequisite).
narrative_ontology:cs_axiom_status(coproduction_infrastructure_is_epistemic_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('a8d0801c-b94c-409f-9a25-ef069912ebba', coproduction_infrastructure_is_epistemic_prerequisite, conventional).
narrative_ontology:cs_reference_frame('a8d0801c-b94c-409f-9a25-ef069912ebba', separate_epistemic_spheres).
narrative_ontology:cs_drift_state('a8d0801c-b94c-409f-9a25-ef069912ebba', contemporary_coproduction_mandate_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a8d0801c-b94c-409f-9a25-ef069912ebba', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_infrastructure_funders).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_researchers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, experiential_knowledge_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_researchers).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, experiential_knowledge_holders).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__hybrid_coproduction_reading, dual_validation_necessary_for_legitimacy).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_infrastructure_is_epistemic_prerequisite).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Must integrate experiential validity into their methodological work to maintain institutional legitimacy; co-production requires translation effort, ceding epistemic authority, and learning community engagement protocols that fall outside traditional training; career advancement still largely runs through credentialed venues, creating dual-track pressure
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_researchers, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, credentialed_researchers, beneficiary).

% Must formalize lived experience through methodological co-production frameworks to gain institutional recognition; translation labor (documentation, protocol compliance, data formatting) falls disproportionately on marginalized communities; community validation remains primary but is insufficient for resource access without methodological legitimation
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, experiential_knowledge_holders, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__hybrid_coproduction_reading, experiential_knowledge_holders, beneficiary).

% Design and fund the platforms, training programs, ethics review boards, and validation protocols that make co-production operational; set legitimacy criteria through grant requirements, journal mandates, and policy frameworks; capture influence over what counts as legitimate knowledge across domains; can redirect funding if co-production fails to deliver promised robustness
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_infrastructure_funders, agenda_setter,
    institutional, generational, arbitrage, global).

% Maintain that methodological rigor alone suffices for legitimacy; view experiential validity requirements as diluting standards and introducing subjectivity; retain traditional peer-review venues and funding streams that do not require co-production; excluded from hybrid legitimacy but not materially harmed by it
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, pure_credentialists, excluded,
    organized, biographical, mobile, global).

% Maintain that lived experience and community validation are self-sufficient; view methodological standards as colonial/oppressive impositions; retain community-based knowledge ecosystems that operate without institutional legitimation; excluded from hybrid legitimacy but not materially harmed by it
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, pure_experientialists, excluded,
    moderate, biographical, mobile, regional).

% Analyze the hybrid constraint's operation, legitimacy effects, power dynamics, and epistemic outcomes from outside the production process; provide meta-level critique of whether co-production delivers more robust knowledge or merely legitimates existing power structures; no material stake in the constraint's persistence
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, sts_epistemology_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates two historically separated epistemic traditions — credentialed methodological rigor and community-validated experiential knowledge — into a single legitimacy standard, solving the problem of knowledge claims that are either methodologically sound but experientially blind, or experientially rich but methodologically unvetted.
% TRANSFER_FUNCTION: Moves translation labor (formalization, documentation, protocol compliance) from experiential knowledge holders and methodological adaptation effort from credentialed researchers to the co-production infrastructure; moves epistemic authority and resource allocation toward funders who design the integration frameworks.
% ABSENT_VOICES: Indigenous knowledge keepers who reject any methodological formalization as ontological violation; Global South communities where co-production infrastructure is imposed by Northern funders; radical epistemic pluralists who deny any universal legitimacy standard can be legitimate.
% DISAPPEARANCE_RATIONALE: If the hybrid standard vanished overnight, funding mandates requiring co-production would lapse, journals would revert to single-track review, credentialed researchers would stop engaging experiential validation, experiential knowledge holders would lose institutional access pathways, and the co-production infrastructure (training programs, ethics boards, platforms) would dissolve — the epistemic landscape would revert to parallel separate spheres.
% FOUNDING_PROBLEM: Late 20th-century crises of legitimacy in both credentialed science (blind spots, irreproducibility, public distrust) and experiential knowledge movements (marginalization, lack of resource access, epistemic injustice) created demand for a legitimacy standard that could honor both methodological robustness and lived-experience validity without reducing either to the other.
% FOUNDING_PROBLEM_CORROBORATION: STS scholars (Jasanoff, Wynne, Collins) document the credibility crises in credentialed expertise; decolonial theorists (Santos, de Sousa Santos) document the marginalization of experiential knowledge; funding agencies (NIH, ESRC, EU Horizon) explicitly cite both crises in co-production mandate justifications; however, credentialed researchers contest whether the experiential crisis was ever epistemic rather than political, and experiential communities contest whether the credibility crisis in science justifies imposing methodological norms on their knowledge.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__hybrid_coproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__hybrid_coproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.52, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects real but moderate extraction: translation labor costs are substantial but not totalizing; both communities gain expanded legitimacy in exchange. Suppression (0.45) reflects dual validation excluding pure credentialist and pure experiential claims from hybrid legitimacy, but both excluded groups retain their own venues. Theater ratio (0.32) reflects growing performative compliance (box-ticking community engagement) alongside genuine integration. Accessibility collapse (0.55) reflects partial foreclosure of single-track alternatives in funded/published venues. Resistance (0.48) reflects pushback from both credentialed researchers (methodological purity) and experiential communities (refusal of formalization).
 *
 * PERSPECTIVAL GAP:
 *   From the funder/agenda-setter seat, the constraint is genuine coordination solving a real epistemic fragmentation problem. From the researcher/knowledge-holder payer seats, the same structure operates as mandated translation labor with uncertain returns. From the excluded seats, it is an illegitimate imposition of alien standards. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed researchers and experiential knowledge holders are dual-positioned: they pay translation costs (payer) but gain hybrid legitimacy (beneficiary) — net directionality near symmetric. Coproduction infrastructure funders are primary beneficiaries (collect agenda-setting power, legitimacy rents) with arbitrage exit — d near beneficiary end. Pure credentialists and pure experientialists are excluded but mobile — not targets of extraction, d near 0.5. Observers are analytical — d=0.5 by definition. The engine derives these from beneficiary/victim declarations plus exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (dual legitimacy crises) remains contested — credentialed researchers dispute the experiential crisis was epistemic; experiential communities dispute the science crisis justifies methodological imposition. The constraint persists not because the founding problem is resolved, but because the co-production infrastructure has become self-sustaining (funding streams, career paths, institutional mandates). This is mandatrophy risk: the arrangement may persist as extraction even if the coordination function atrophies. Theater ratio rise (0.15→0.32) signals this risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the hybrid co-production reading a structurally distinct constraint from its sibling readings, or a pragmatic compromise that collapses under scrutiny?',
    'Test ε-invariance: if measuring extraction via credentialed-researcher labor costs yields ε≈0.5 but measuring via experiential-community translation costs yields ε≈0.7, the readings may be conflating distinct constraints. Decompose if ε varies by observable.',
    'If ε is not invariant, the kernel label ''legitimate_knowledge_boundary'' covers multiple constraints; each reading should be a separate story with its own ε, linked via network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel label conceals ε-variant constraints across readings').

omega_variable(
    structural_delta_ambiguity,
    'Does the ''moderate barriers; dual validation required'' structural delta describe a stable coordination-extraction hybrid, or a transitional scaffold toward full experiential pluralism?',
    'Track theater_ratio and suppression_requirement trajectories: if theater rises while suppression falls, the constraint is a scaffold shedding coordination; if both rise, it is a ratcheting tangled_rope.',
    'If scaffold, the constraint has implicit sunset logic; if tangled_rope, it is a stable hybrid requiring permanent enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_delta_ambiguity, empirical, 'Whether the hybrid standard is a stable equilibrium or transitional formation').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.45) structural (funding mandates, journal gatekeeping) or internalized (researchers/communities self-censor to anticipate co-production requirements)?',
    'Post-mandate relaxation study: if suppression persists in venues that drop co-production requirements, reclassify as partially internalized.',
    'If internalized, effective suppression is higher than structural measure suggests — agents carry the constraint after formal enforcement lifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in epistemic gatekeeping').

omega_variable(
    coproduction_infrastructure_capture,
    'Does the co-production infrastructure primarily serve epistemic integration, or has it been captured by funder agendas (e.g., ''participation washing'' for predetermined outcomes)?',
    'Compare protocol compliance rates vs. substantive influence metrics: high compliance + low influence = capture.',
    'If captured, the coordination function is theater; the constraint reclassifies toward snare. If genuine, tangled_rope holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coproduction_infrastructure_capture, empirical, 'Whether co-production infrastructure serves integration or funder capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lkb_hcr_tr_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(lkb_hcr_tr_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(lkb_hcr_tr_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(lkb_hcr_tr_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(lkb_hcr_tr_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 20, 0.32).

% Extraction over time
narrative_ontology:measurement(lkb_hcr_be_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(lkb_hcr_be_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(lkb_hcr_be_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(lkb_hcr_be_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(lkb_hcr_be_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(lkb_hcr_su_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(lkb_hcr_su_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(lkb_hcr_su_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(lkb_hcr_su_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 15, 0.43).
narrative_ontology:measurement(lkb_hcr_su_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 20, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__hybrid_coproduction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.08).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary__credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary__experiential_pluralism_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'legitimate knowledge boundary' kernel into three structurally distinct readings with divergent ε: credentialed_expertise_reading (low extraction, mountain-like), experiential_pluralism_reading (moderate extraction, rope/tangled_rope), hybrid_coproduction_reading (this story, moderate-high extraction, tangled_rope). The hybrid reading cites the other two as evidence of the crises it resolves, creating upstream→downstream influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_knowledge_boundary__hybrid_coproduction_reading, organized, 0.35).
constraint_indexing:directionality_override(legitimate_knowledge_boundary__hybrid_coproduction_reading, moderate, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
