% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__experiential_pluralism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__experiential_pluralism_reading, []).

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
 *   constraint_id: legitimate_knowledge_boundary__experiential_pluralism_reading
 *   human_readable: Experiential Pluralism Knowledge Boundary
 *   domain: epistemology/science_and_technology_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the experiential pluralism reading of
 *   the legitimate knowledge boundary kernel. The reading declares that
 *   legitimate knowledge arises from lived experience and community
 *   validation, with methodological standards as one tool among many rather
 *   than the exclusive arbiter. The constraint operates within epistemology
 *   and STS but has direct material effects: it determines whose knowledge
 *   shapes policy on environmental management, public health, education, and
 *   territorial governance. This reading is contested by credentialed
 *   expertise and hybrid coproduction readings, which assert that
 *   methodological rigor and external validation are necessary. The
 *   claim/metric divergence is intentional: the reading claims tangled_rope
 *   (coordination of validation without methodological gatekeeping +
 *   extraction from credentialed institutions) while the metrics describe
 *   substantial extraction, rising theater, and persistent suppression
 *   requirements, reflecting the institutional resistance to this knowledge
 *   boundary and the performative assertion of community validation alongside
 *   continued credentialed control.
 *
 * KEY AGENTS:
 *   - Marginalized knowledge communities: Beneficiaries whose lived experience is elevated to legitimate knowledge status
 *   - Indigenous practitioners: Custodians of place-based knowledge with decision-making authority under this reading
 *   - Community organizers: Generators of participatory action knowledge validated through material outcomes
 *   - Credentialed researchers: Payers whose methodological gatekeeping loses exclusive legitimacy
 *   - Institutional gatekeepers: Universities, funding bodies, peer-review systems bearing reduced authority
 *   - Policy makers: Observers witnessing competing frameworks and required to credit community voices equally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.62).
domain_priors:suppression_score(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.58).
domain_priors:theater_ratio(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__experiential_pluralism_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__experiential_pluralism_reading, "Experiential Pluralism Knowledge Boundary").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__experiential_pluralism_reading, "epistemology/science_and_technology_studies").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__experiential_pluralism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__experiential_pluralism_reading, '1c295ffb-5eca-4f14-b295-93a1d1f1bdb5').
narrative_ontology:cs_kernel_codification('1c295ffb-5eca-4f14-b295-93a1d1f1bdb5', distributed).
narrative_ontology:cs_authority_grounding('1c295ffb-5eca-4f14-b295-93a1d1f1bdb5', distributed).
narrative_ontology:cs_reading_relation('1c295ffb-5eca-4f14-b295-93a1d1f1bdb5', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c295ffb-5eca-4f14-b295-93a1d1f1bdb5', legitimate_knowledge_boundary__hybrid_coproduction_reading, coexists_with).
narrative_ontology:cs_axiom('1c295ffb-5eca-4f14-b295-93a1d1f1bdb5', foundational, epistemic_authority_distributed_to_lived_experience).
narrative_ontology:cs_axiom_status(epistemic_authority_distributed_to_lived_experience, holdable).
narrative_ontology:cs_axiom_grounding('1c295ffb-5eca-4f14-b295-93a1d1f1bdb5', epistemic_authority_distributed_to_lived_experience, deontological).
narrative_ontology:cs_axiom('1c295ffb-5eca-4f14-b295-93a1d1f1bdb5', foundational, methodological_standards_are_contextual_tools_not_universal_arbiters).
narrative_ontology:cs_axiom_status(methodological_standards_are_contextual_tools_not_universal_arbiters, holdable).
narrative_ontology:cs_axiom_grounding('1c295ffb-5eca-4f14-b295-93a1d1f1bdb5', methodological_standards_are_contextual_tools_not_universal_arbiters, deontological).
narrative_ontology:cs_axiom('1c295ffb-5eca-4f14-b295-93a1d1f1bdb5', secondary, community_validation_sufficient_for_knowledge_legitimacy).
narrative_ontology:cs_axiom_status(community_validation_sufficient_for_knowledge_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('1c295ffb-5eca-4f14-b295-93a1d1f1bdb5', community_validation_sufficient_for_knowledge_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('1c295ffb-5eca-4f14-b295-93a1d1f1bdb5', epistemic_pluralism_framework).
narrative_ontology:cs_drift_state('1c295ffb-5eca-4f14-b295-93a1d1f1bdb5', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1c295ffb-5eca-4f14-b295-93a1d1f1bdb5', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_knowledge_communities).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, indigenous_practitioners).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, community_organizers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_researchers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, institutional_gatekeepers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their lived experience—accumulated over generations, embedded in practice and oral tradition—is elevated to the status of legitimate knowledge without requiring translation into methodological frameworks. They validate knowledge claims through community deliberation, testing against lived outcomes, and cultural continuity. They gain recognition and resource allocation on the strength of their own evidentiary standards without needing credentialing.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_knowledge_communities, beneficiary,
    powerless, generational, identity_locked, global).

% Custodians of place-based ecological, medical, and social knowledge. Under this reading, their knowledge is validated by demonstrated long-term stewardship and community endorsement, not by peer review of methodology. They hold decision-making authority over knowledge claims about their territories and practices.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, indigenous_practitioners, beneficiary,
    moderate, civilizational, identity_locked, regional).

% Practitioners who generate knowledge through participatory action: mapping power structures, testing interventions in real communities, learning through organized resistance. Their knowledge—about what works, what fails, what communities actually need—is validated through participatory deliberation and material outcomes, not through publication in credentialed journals.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, community_organizers, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, community_organizers, agenda_setter).

% Hold institutional authority over knowledge validation through peer review, methodology, and disciplinary standards. Under this reading, their methodological gatekeeping loses exclusive legitimacy; they must justify their standards as one tool among many rather than as the arbiter of knowledge validity. They experience loss of institutional authority and access to funding when their work is devalued against community-validated knowledge.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_researchers, payer,
    powerful, biographical, constrained, global).

% Universities, research councils, funding bodies, peer-review systems that control access to research credibility and resources. Under this reading, they must legitimize knowledge claims that bypass their validation apparatus. They bear the cost of reduced institutional gatekeeping power and the operational burden of integrating non-standardized, community-validated knowledge into decision-making.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, institutional_gatekeepers, payer,
    institutional, generational, mobile, global).

% Tasked with implementing policy on contested domains (environmental management, public health, education). They witness competing knowledge frameworks and must decide which voices to credit. Under this reading, they must weigh community experience and evidence equally against credentialed expert testimony.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, policy_makers, observer,
    institutional, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__experiential_pluralism_reading, institutional_gatekeepers).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__experiential_pluralism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of whose knowledge counts in decision-making. Without it: marginalized communities' centuries of tested practice are overwritten by external experts; indigenous stewardship is dismissed as anecdotal; community organizers' field-tested solutions are rejected as unscientific. The arrangement coordinates validation without requiring translation into credentialed methodologies.
% TRANSFER_FUNCTION: Transfers epistemic authority from credentialed institutions to community-embedded validators. The constraint redistributes who gets to decide which knowledge claims are legitimate, and therefore who gets listened to in policy, medicine, environmental management, and social practice.
% ABSENT_VOICES: Commercial interests hiding behind 'community validation' rhetorics (pharmaceutical companies running community studies to legitimate profitable interventions). Also: credentialed experts whose criticisms of the reading are excluded from deliberation when they are framed as defensive gatekeeping rather than substantive epistemological positions.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared, institutional gatekeeping would reassert exclusive epistemic authority. Indigenous land management would revert to external expert determination. Community-organized solutions would be overwritten by top-down policy based on credentialed research. The entire knowledge landscape shifts from distributed validation back to credentialed hierarchy.
% FOUNDING_PROBLEM: Centuries of institutional knowledge systems dismissing, overwriting, and rendering invisible the tested knowledge of marginalized communities, indigenous peoples, and organized communities. Epistemic injustice: the systematic exclusion of certain people and communities from the status of knowers.
% FOUNDING_PROBLEM_CORROBORATION: Documented in ethnographic research on indigenous land management showing superior ecological outcomes to expert-designed conservation (Agrawal, Boyd, Donahue); in public health literature on community health worker efficacy (WHO documentation); in participatory action research showing community-generated solutions outperforming top-down interventions in development contexts (Chambers, Reason & Bradbury). Corroboration comes from researchers sympathetic to the reading but also from non-aligned institutional sources (CDC adoption of community health worker models; World Bank integration of indigenous resource management).
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__experiential_pluralism_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__experiential_pluralism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__experiential_pluralism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.15 (1980) to 0.62 (2026) because the reading's assertion of community validation as legitimate requires that credentialed researchers and institutional gatekeepers surrender epistemic authority they previously held exclusively. This is extraction: taking gatekeeping power from those who held it. Suppression rises but plateaus (0.35→0.58) because the constraint requires active institutional work to suppress dismissal of community knowledge and to maintain the fiction that methodological standards are 'just one tool' while credentialed systems continue to dominate funding, publication, and policy. Theater ratio (0.08→0.41) indicates rising performativity: institutions increasingly adopt 'community engagement' and 'participatory' language while maintaining substantive credentialed gatekeeping, and marginalized communities perform validation in forms legible to institutional bodies. The measurement series tracks one shared time grid across 46 years so institutional capture and epistemic injustice literature can be consulted at each point.
 *
 * PERSPECTIVAL GAP:
 *   From the marginalized knowledge communities' seat: this reading removes barriers to legitimate knowledge claims and redistributes authority. From the credentialed researchers' and institutional gatekeepers' seat: this reading imposes costs (loss of gatekeeping, requirement to legitimate non-standardized knowledge, operational uncertainty about what counts as valid). From policy makers' seat: the reading creates obligation to weigh incommensurable knowledge claims without clear adjudication rules. From the reading's own internal perspective: the constraint achieves genuine coordination by preventing epistemic erasure. From external critiques (credentialed expertise reading): the constraint dissolves methodological rigor and opens space for motivated reasoning disguised as community wisdom. These divergences are structural, not merely evaluative, and the engine should compute them from power atoms and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized communities: beneficiaries with low power and identity-locked exit yield d near 0.0 (full beneficiary benefit, no escape regardless of extraction direction). Credentialed researchers: payers with high power and constrained exit yield d near 0.8–0.9 (they bear extraction cost, cannot leave credentialed system without losing professional identity). Institutional gatekeepers: payers with institutional power and mobile exit yield d near 0.5–0.7 (they bear gatekeeping loss but can reorganize). Community organizers: beneficiary-agenda-setters with organized power and mobile exit yield d near 0.3–0.4 (they benefit from elevated status, have some agency in setting validation terms, but remain dependent on institutional recognition for scaling). Policy makers: observers with institutional power and mobile exit yield d near 0.5 (symmetric: they gain better information from distributed validation but lose clarity in decision-making).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (epistemic injustice, institutional dismissal of marginalized knowledge) is live and well-documented. The constraint's stated coordination function (enabling validation without methodological gatekeeping) is genuinely functional for some domains (indigenous land management, community health, participatory development). However, the theater ratio (0.41) and rising suppression requirement (0.58 stable since 2015) suggest the constraint increasingly operates as performance: institutions adopt participatory language while maintaining credentialed control, and 'community validation' becomes a rhetorical move to legitimize predetermined decisions. The measurement data shows extractiveness climbing while theater plateaus—the divergence indicates that the extraction is becoming more visible and less masked by coordination narrative. This is a tangled rope that is drifting toward snare characteristics as institutional capture tightens and participatory forms hollow out. Mandatrophy does not yet apply (the founding problem is live, the reading is still contested and defended), but the trajectory suggests approaching terminal obsolescence: if institutions succeed in maintaining credentialed gatekeeping while performing community engagement, the constraint's functional coordination dissolves and only extraction remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    community_validation_operationalization,
    'What constitutes ''community validation'' such that it cannot be captured or simulated by institutions performing community engagement while maintaining credentialed gatekeeping?',
    'Ethnographic observation of communities claiming epistemic authority and tracking whether their knowledge claims alter institutional decision-making (policy, funding, resource allocation) or merely provide legitimizing testimony for pre-made decisions. Measure institutional responsiveness to knowledge claims that contradict credentialed expertise.',
    'If institutional decision-making genuinely reallocates authority based on community validation, the constraint achieves its coordination function and theater is justified. If institutions use community testimony to legitimize credentialed decisions, the constraint is substantially snare (extraction masked by performative coordination). This is the mandatrophy boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(community_validation_operationalization, empirical, 'Whether community validation constitutes genuine epistemic authority or institutional performance.').

omega_variable(
    experiential_pluralism_vs_motivated_reasoning,
    'How does the experiential pluralism reading distinguish legitimate lived experience and community knowledge from motivated reasoning, confirmation bias, or factual error dressed in community language?',
    'Philosophical analysis of epistemological standards internal to the reading (community deliberation under what conditions? Testing against what outcomes? Over what time horizon?). Empirical observation of cases where community-validated knowledge failed predictively or materially and how the reading accounts for failure without invoking credentialed methodology as the error-detection mechanism.',
    'If the reading supplies internal error-correction mechanisms, it is a complete epistemic system and extraction (cost to credentialed systems) may be justified as payment for epistemic humility. If the reading depends on credentialed methodology to detect failure cases (while denying methodology legitimacy in success cases), the constraint is incoherent and the reading is unsustainable. This frames the theological/motivational question: is this a genuine epistemology or a political position masquerading as epistemology?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(experiential_pluralism_vs_motivated_reasoning, conceptual, 'Whether experiential pluralism has independent error-correction or presupposes credentialed methodology while denying it legitimacy.').

omega_variable(
    reading_vs_commercial_capture,
    'How does the experiential pluralism reading distinguish community-validated knowledge from commercial interests marketing products as community-endorsed wisdom?',
    'Examine cases where pharmaceutical, agricultural, or technology companies mobilize ''community voices'' to validate products. Determine whether the reading''s criteria exclude these cases and how exclusion is operationalized without invoking credentialed scientific methodology.',
    'If commercial capture can be excluded by reading-internal criteria, the constraint protects against this distortion. If only credentialed methodology can identify commercial capture, the reading presupposes methodology while denying it—a performative contradiction. This frames the commodification vulnerability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_commercial_capture, empirical, 'Whether the reading can exclude commercial capture without invoking credentialed methodology.').

omega_variable(
    sibling_reading_coexistence,
    'Can experiential pluralism and credentialed expertise coexist as legitimate frameworks within a single institutional context, or does assertion of experiential legitimacy necessarily displace credentialed gatekeeping?',
    'Comparative analysis of institutional sites (universities, policy agencies, funding bodies) that claim to integrate both frameworks. Measure whether credentialed standards retain gatekeeping power (do credentialed researchers still determine what gets funded/published/adopted?) or whether authority genuinely distributes to community validators.',
    'If genuine coexistence is possible, both readings can hold and the constraint operates as coordination. If assertion of experiential legitimacy necessarily displace credentialed gatekeeping in practice, the readings foreclose each other (despite the engine classifying as coexists_with) and one reading''s assertion is inherently antagonistic to the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, empirical, 'Whether the experiential pluralism and credentialed expertise readings can genuinely coexist or foreclose each other in practice.').

omega_variable(
    incommensurability_vs_pluralism,
    'Is this reading genuinely pluralist (multiple valid frameworks with trade-offs but no ultimate hierarchy) or does it assert incommensurability (frameworks are so different they cannot be compared or weighed against each other)?',
    'Analysis of the reading''s own rhetoric: when credentialed expertise and community validation conflict, does the reading propose adjudication procedures or declare them incommensurable and insist both be credited? If incommensurable, the constraint may actually increase uncertainty in decision-making rather than achieving coordination.',
    'If pluralist: the reading solves a coordination problem (multiple perspectives, weighted trade-off). If incommensurable: the reading shifts the problem from gatekeeping conflict to irresolvable epistemic conflict, and institutional decision-making becomes paralyzed or arbitrary. This frames whether the reading improves or degrades institutional functioning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incommensurability_vs_pluralism, conceptual, 'Whether the reading is genuinely pluralist or asserts incommensurability without adjudication.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__experiential_pluralism_reading, 1980, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1980, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(legi_tr_t1995, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 1995, 0.14).
narrative_ontology:measurement(legi_tr_t2005, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 2005, 0.24).
narrative_ontology:measurement(legi_tr_t2015, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(legi_tr_t2020, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(legi_tr_t2026, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 2026, 0.41).

% Extraction over time
narrative_ontology:measurement(legi_be_t1980, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(legi_be_t1995, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement(legi_be_t2005, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(legi_be_t2015, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 2015, 0.54).
narrative_ontology:measurement(legi_be_t2020, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(legi_be_t2026, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1980, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(legi_su_t1995, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 1995, 0.42).
narrative_ontology:measurement(legi_su_t2005, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement(legi_su_t2015, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(legi_su_t2020, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 2020, 0.58).
narrative_ontology:measurement(legi_su_t2026, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 2026, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__experiential_pluralism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.12).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading).

% DUAL FORMULATION NOTE:
% The legitimate_knowledge_boundary kernel decomposes into three structurally distinct constraints instantiating different readings. This story (experiential_pluralism_reading) asserts community validation and lived experience as primary legitimacy criteria, with methodological standards demoted to optional tool status. The credentialed_expertise_reading (sibling) asserts methodological rigor and peer review as necessary and sufficient. The hybrid_coproduction_reading (sibling) asserts that neither alone is sufficient and co-produced integration is required. Each reading generates different ε values (this reading: ε=0.62 extractive, shifting from coordination); different victim/beneficiary structures; and different measured types. They are not alternative measurements of a single constraint but three readings of a single contested kernel, each with its own functional and extractive properties. Network edges allow contamination analysis: if one reading's credibility declines, does it affect the others' institutional purchase?

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
