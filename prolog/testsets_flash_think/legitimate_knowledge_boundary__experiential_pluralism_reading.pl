% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__experiential_pluralism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   human_readable: Legitimate Knowledge from Lived Experience and Community Validation (Experiential Pluralism Reading)
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'experiential pluralism' reading of the
 *   'legitimate_knowledge_boundary' kernel. It posits that legitimate
 *   knowledge arises from lived experience and community validation, with
 *   methodological standards serving as one tool among many, rather than the
 *   sole arbiter of truth. This reading aims to broaden epistemic inclusion
 *   and challenge the exclusive authority of credentialed expertise. The
 *   metrics reflect its aspirational nature as a 'rope' that genuinely
 *   coordinates diverse knowledge forms, with relatively low extraction and
 *   suppression, but acknowledges the significant resistance it faces from
 *   established systems.
 *
 * KEY AGENTS:
 *   - experiential_knowledge_holders: Primary beneficiary (powerless/constrained)
 *   - marginalized_communities: Primary beneficiary (organized/constrained)
 *   - traditional_epistemic_authorities: Primary payer (institutional/constrained)
 *   - science_and_technology_studies_scholars: Agenda setter (analytical/analytical)
 *   - policy_makers: Payer (institutional/constrained)
 *   - public_discourse: Observer (moderate/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.35).
domain_priors:suppression_score(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.25).
domain_priors:theater_ratio(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__experiential_pluralism_reading, rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__experiential_pluralism_reading, "Legitimate Knowledge from Lived Experience and Community Validation (Experiential Pluralism Reading)").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__experiential_pluralism_reading, "epistemology/science_and_technology_studies/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__experiential_pluralism_reading, '59aa9ba8-2f18-47ba-b9ac-7949792f2723').
narrative_ontology:cs_kernel_codification('59aa9ba8-2f18-47ba-b9ac-7949792f2723', distributed).
narrative_ontology:cs_authority_grounding('59aa9ba8-2f18-47ba-b9ac-7949792f2723', diffuse_epistemic).
narrative_ontology:cs_reading_relation('59aa9ba8-2f18-47ba-b9ac-7949792f2723', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('59aa9ba8-2f18-47ba-b9ac-7949792f2723', legitimate_knowledge_boundary__hybrid_coproduction_reading, influences).
narrative_ontology:cs_axiom('59aa9ba8-2f18-47ba-b9ac-7949792f2723', foundational, experiential_primacy).
narrative_ontology:cs_axiom_status(experiential_primacy, holdable).
narrative_ontology:cs_axiom_grounding('59aa9ba8-2f18-47ba-b9ac-7949792f2723', experiential_primacy, empirically_contingent).
narrative_ontology:cs_axiom('59aa9ba8-2f18-47ba-b9ac-7949792f2723', foundational, contextual_validation).
narrative_ontology:cs_axiom_status(contextual_validation, holdable).
narrative_ontology:cs_axiom_grounding('59aa9ba8-2f18-47ba-b9ac-7949792f2723', contextual_validation, conventional).
narrative_ontology:cs_reference_frame('59aa9ba8-2f18-47ba-b9ac-7949792f2723', situated_knowledge_paradigm).
narrative_ontology:cs_drift_state('59aa9ba8-2f18-47ba-b9ac-7949792f2723', contemporary_discourse, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('59aa9ba8-2f18-47ba-b9ac-7949792f2723', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, experiential_knowledge_holders).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, traditional_epistemic_authorities).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, policy_makers).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, situated_knowledge_theory).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, standpoint_epistemology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals whose knowledge is primarily derived from direct lived experience, often marginalized by traditional epistemic frameworks. This constraint elevates their knowledge claims to a position of legitimacy.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, experiential_knowledge_holders, beneficiary,
    powerless, biographical, constrained, local).

% Groups whose collective knowledge and validation practices were historically excluded or devalued. This reading provides a framework for their epistemic contributions to be recognized and integrated.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_communities, beneficiary,
    organized, generational, constrained, regional).

% Institutions and individuals (e.g., universities, credentialed experts) whose authority was historically based on methodological rigor and peer review. Under this reading, their claims are re-contextualized as 'one tool among many,' requiring them to engage with diverse forms of knowledge and potentially cede exclusive epistemic gatekeeping power.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, traditional_epistemic_authorities, payer,
    institutional, generational, constrained, global).

% Academics who theorize and advocate for pluralistic epistemologies, often articulating the principles of experiential and community-based knowledge validation. They actively shape the discourse around this constraint.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, science_and_technology_studies_scholars, agenda_setter,
    analytical, biographical, analytical, global).

% Government officials and agencies who must decide which forms of knowledge to incorporate into policy decisions. Adopting this reading requires them to broaden their evidence base beyond conventional scientific expertise, incurring costs in terms of new processes and potential political friction.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, policy_makers, payer,
    institutional, immediate, constrained, national).

% The broader arena of public debate where different claims to knowledge legitimacy are contested. This stakeholder observes the unfolding dynamics of epistemic re-evaluation without directly enforcing or benefiting from the constraint's operation.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, public_discourse, observer,
    moderate, immediate, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the integration and validation of diverse forms of knowledge, particularly those arising from lived experience and community contexts, alongside traditional methodological standards, to achieve a more comprehensive understanding of complex problems.
% TRANSFER_FUNCTION: Transfers epistemic authority and legitimacy from exclusively credentialed expertise to a broader array of experiential knowledge holders and community validation processes. It also transfers the burden of integration and contextualization to all participants.
% ABSENT_VOICES: Those who insist on a singular, universal standard of methodological rigor as the *only* legitimate source of knowledge are structurally de-prioritized or excluded from setting the terms of validation. They would argue that this pluralism risks epistemic relativism or a loss of objectivity.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the epistemic landscape would revert to a more hierarchical structure, devaluing experiential and community-based knowledge. Many marginalized voices would lose their claim to legitimacy, and policy decisions would likely rely solely on conventional scientific expertise, leading to different outcomes and potentially exacerbating existing inequalities.
% FOUNDING_PROBLEM: The historical exclusion and devaluation of knowledge derived from lived experience and community contexts, particularly from marginalized populations, leading to incomplete or biased understandings of social and environmental issues.
% FOUNDING_PROBLEM_CORROBORATION: Scholars in Science and Technology Studies, critical race theory, and indigenous studies consistently attest to the ongoing problem of epistemic injustice. Activist movements and community organizers also corroborate the need for broader knowledge legitimation, providing evidence from outside the immediate beneficiaries of this reading.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__experiential_pluralism_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__experiential_pluralism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legitimate_knowledge_boundary__experiential_pluralism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).
:- end_tests(legitimate_knowledge_boundary__experiential_pluralism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is set at a moderate-low 0.35, reflecting that while this reading aims for epistemic justice, any system of knowledge validation inherently imposes some costs on claims that don't fit its framework. However, it's significantly lower than readings that enforce strict, exclusive criteria. `suppression` is low (0.25) because the core aim is to *reduce* the suppression of previously marginalized knowledge forms. `theater_ratio` is low (0.15) as this is a genuine intellectual and social movement, not primarily performative. `accessibility_collapse` is low (0.30) as it actively seeks to open up access to knowledge production and validation. `resistance` is high (0.70) because this reading directly challenges deeply entrenched epistemic hierarchies and institutional power structures.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of experiential knowledge holders and marginalized communities, this constraint is a vital mechanism for epistemic liberation and recognition. For traditional epistemic authorities, it is perceived as a threat to established standards and a source of increased complexity and contestation, requiring them to 'pay' in terms of adapting their practices and ceding exclusive authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Experiential knowledge holders and marginalized communities are clear beneficiaries, as their knowledge claims gain legitimacy and recognition. Traditional epistemic authorities and policy makers act as payers, as they must adapt to new epistemic demands and integrate diverse knowledge forms, which entails costs to their established practices and authority. Science and Technology Studies scholars act as agenda-setters, actively shaping and promoting this epistemic framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a 'rope' prevents mislabeling by focusing on its genuine coordination function: integrating diverse knowledge forms for more robust understanding. It avoids being a 'snare' because it aims to reduce, not create, asymmetric extraction of epistemic authority, and it does not suppress alternatives (methodological standards are 'one tool among many'). It is not a 'piton' because it is an active, contested, and evolving framework with clear beneficiaries and ongoing resistance, not an atrophied function maintained by inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_relativism_risk,
    'Does the emphasis on experiential and community validation, without clear overarching methodological standards, lead to epistemic relativism or a loss of shared criteria for truth?',
    'Longitudinal studies of knowledge integration projects: if diverse knowledge forms can be effectively synthesized and applied to solve problems without undermining shared understanding, the risk is mitigated. If coherence breaks down, the risk is real.',
    'If the risk is realized, the constraint''s coordination function is undermined, and it may be reclassified as a ''piton'' (failed coordination) or even a ''snare'' (if new forms of exclusion emerge from the fragmentation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_relativism_risk, empirical, 'Whether pluralistic validation leads to epistemic fragmentation.').

omega_variable(
    redefinition_of_expertise_inclusivity,
    'Is the redefinition of expertise as context-specific and distributed truly inclusive, or does it inadvertently create new forms of exclusion or gatekeeping based on different criteria?',
    'Qualitative sociological studies examining power dynamics within ''pluralistic'' knowledge communities: if new hierarchies emerge that marginalize certain experiential claims, the inclusivity claim is weakened.',
    'If new exclusions are found, the constraint''s effective suppression and extractiveness would be higher than currently measured, potentially shifting its classification towards a ''tangled_rope'' or ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redefinition_of_expertise_inclusivity, empirical, 'Whether new forms of exclusion emerge from re-defined expertise.').

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the ''experiential_pluralism_reading'' of the ''legitimate_knowledge_boundary'' kernel. Sibling readings include ''credentialed_expertise_reading'' and ''hybrid_coproduction_reading''. What is the precise structural delta this reading introduces compared to its siblings?',
    'Comparative analysis of knowledge governance frameworks adopted under each reading: specifically, how each reading defines validation criteria, allocates epistemic authority, and manages conflicts between different knowledge claims.',
    'This reading structurally changes the weighting of knowledge claims, prioritizing lived experience and community validation, which differs from the methodological rigor focus of ''credentialed_expertise_reading'' and influences the integration approach of ''hybrid_coproduction_reading''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Documents the structural differences of this kernel reading from its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__experiential_pluralism_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1980, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(legi_tr_t1990, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(legi_tr_t2000, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 2000, 0.16).
narrative_ontology:measurement(legi_tr_t2010, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(legi_tr_t2024, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(legi_be_t1980, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(legi_be_t1990, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(legi_be_t2000, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(legi_be_t2010, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 2010, 0.36).
narrative_ontology:measurement(legi_be_t2024, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1980, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(legi_su_t1990, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(legi_su_t2000, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(legi_su_t2010, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 2010, 0.26).
narrative_ontology:measurement(legi_su_t2024, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__experiential_pluralism_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'legitimate_knowledge_boundary' kernel. Each reading represents a distinct structural claim about how knowledge is legitimized, with different ε values and stakeholder dynamics. They are linked to enable comparative analysis of their epistemic consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
