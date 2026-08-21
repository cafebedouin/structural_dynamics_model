% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__credentialed_expertise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__credentialed_expertise_reading, []).

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
 *   constraint_id: legitimate_knowledge_boundary__credentialed_expertise_reading
 *   human_readable: Legitimate Knowledge Boundary: Credentialed Expertise Reading
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This constraint describes the 'credentialed expertise' reading of the
 *   legitimate knowledge boundary, where knowledge is deemed legitimate
 *   primarily through methodologically rigorous inquiry validated by
 *   credentialed peer review. This reading emphasizes formal training,
 *   institutional affiliation, and adherence to established scientific
 *   protocols as the primary determinants of epistemic authority. It
 *   functions as a gatekeeping mechanism, coordinating epistemic standards
 *   while simultaneously extracting from and suppressing alternative
 *   knowledge forms. The metrics reflect a system that has become
 *   increasingly extractive and suppressive over time, as the power of
 *   credentialed institutions has solidified.
 *
 * KEY AGENTS:
 *   - Credentialed Experts: Primary agenda-setters and beneficiaries (institutional/constrained)
 *   - Academic Institutions: Beneficiaries (institutional/constrained)
 *   - Funding Bodies: Beneficiaries (institutional/mobile)
 *   - Non-Credentialed Knowledge Producers: Primary payers and victims (powerless/identity_locked)
 *   - Marginalized Communities: Victims (powerless/trapped)
 *   - Interdisciplinary Researchers: Payers (moderate/constrained)
 *   - Public Policy Makers: Observers (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.65).
domain_priors:suppression_score(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.78).
domain_priors:theater_ratio(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__credentialed_expertise_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__credentialed_expertise_reading, "Legitimate Knowledge Boundary: Credentialed Expertise Reading").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__credentialed_expertise_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__credentialed_expertise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__credentialed_expertise_reading, 'c2b56e0d-1c02-4059-8a33-edfbda7c1c55').
narrative_ontology:cs_kernel_codification('c2b56e0d-1c02-4059-8a33-edfbda7c1c55', formalized).
narrative_ontology:cs_authority_grounding('c2b56e0d-1c02-4059-8a33-edfbda7c1c55', lineage).
narrative_ontology:cs_interpretation_layer_present('c2b56e0d-1c02-4059-8a33-edfbda7c1c55').
narrative_ontology:cs_reading_relation('c2b56e0d-1c02-4059-8a33-edfbda7c1c55', legitimate_knowledge_boundary__experiential_pluralism_reading, coexists_with).
narrative_ontology:cs_reading_relation('c2b56e0d-1c02-4059-8a33-edfbda7c1c55', legitimate_knowledge_boundary__hybrid_coproduction_reading, coexists_with).
narrative_ontology:cs_axiom('c2b56e0d-1c02-4059-8a33-edfbda7c1c55', foundational, epistemic_validity_requires_credentialed_peer_review).
narrative_ontology:cs_axiom_status(epistemic_validity_requires_credentialed_peer_review, holdable).
narrative_ontology:cs_axiom_grounding('c2b56e0d-1c02-4059-8a33-edfbda7c1c55', epistemic_validity_requires_credentialed_peer_review, conventional).
narrative_ontology:cs_axiom('c2b56e0d-1c02-4059-8a33-edfbda7c1c55', foundational, methodological_rigor_is_universal_and_objective).
narrative_ontology:cs_axiom_status(methodological_rigor_is_universal_and_objective, holdable).
narrative_ontology:cs_axiom_grounding('c2b56e0d-1c02-4059-8a33-edfbda7c1c55', methodological_rigor_is_universal_and_objective, empirically_contingent).
narrative_ontology:cs_reference_frame('c2b56e0d-1c02-4059-8a33-edfbda7c1c55', post_wwii_scientific_consensus).
narrative_ontology:cs_drift_state('c2b56e0d-1c02-4059-8a33-edfbda7c1c55', contemporary_post_truth_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c2b56e0d-1c02-4059-8a33-edfbda7c1c55', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_institutions).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, funding_bodies).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, non_credentialed_knowledge_producers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, marginalized_communities).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, interdisciplinary_researchers).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, scientific_method_supremacy).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, epistemic_authority_of_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are individuals with advanced degrees and institutional affiliations who define methodological rigor, conduct peer review, and largely control access to publication and funding. They benefit from the system's prestige and resource allocation.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts, agenda_setter,
    institutional, generational, constrained, global).

% Universities and research centers whose legitimacy and funding depend on housing credentialed experts and producing 'legitimate' knowledge. They benefit from the system's gatekeeping function, which reinforces their central role.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_institutions, beneficiary,
    institutional, generational, constrained, national).

% Government agencies and private foundations that allocate research grants based on peer-reviewed proposals. They benefit from a clear, if narrow, definition of legitimate knowledge, which simplifies their decision-making and legitimizes their allocations.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, funding_bodies, beneficiary,
    institutional, biographical, mobile, national).

% Individuals or groups (e.g., citizen scientists, indigenous knowledge holders, community organizers) who produce valuable knowledge but lack formal academic credentials. Their knowledge is often dismissed or devalued, making it difficult to gain recognition or influence policy.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, non_credentialed_knowledge_producers, payer,
    powerless, biographical, identity_locked, local).

% Communities whose lived experiences generate critical insights, but whose knowledge is systematically excluded from 'legitimate' discourse due to lack of credentialed validation. They bear the cost of having their perspectives ignored in policy and research.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, marginalized_communities, payer,
    powerless, generational, trapped, local).

% Academics working across traditional disciplinary boundaries, whose methodologies may not fit neatly into established peer-review frameworks. They face challenges in publishing, funding, and career progression due to the rigid enforcement of disciplinary norms.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, interdisciplinary_researchers, payer,
    moderate, biographical, constrained, global).

% Rely on 'legitimate' knowledge to inform policy decisions, often prioritizing peer-reviewed scientific consensus. They observe the debates but are often constrained by institutional norms to privilege credentialed sources, even when other forms of knowledge might be relevant.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, public_policy_makers, observer,
    institutional, immediate, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__credentialed_expertise_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common standard for evaluating knowledge claims, ensuring a baseline of rigor and reproducibility, and coordinating the allocation of epistemic authority within a complex information ecosystem.
% TRANSFER_FUNCTION: Transfers epistemic authority, prestige, and resources (funding, publication access) from non-credentialed or non-peer-reviewed knowledge producers to credentialed experts and academic institutions.
% ABSENT_VOICES: Knowledge producers from marginalized communities, indigenous knowledge systems, and citizen science initiatives are largely absent from the formal validation process. They would argue for broader epistemic inclusion and recognition of diverse forms of evidence.
% DISAPPEARANCE_RATIONALE: If the credentialed peer-review system vanished overnight, there would be an initial period of epistemic chaos, followed by the emergence of diverse, decentralized, and potentially more inclusive knowledge validation mechanisms. The current hierarchy of knowledge would collapse, forcing a re-evaluation of what counts as 'truth'.
% FOUNDING_PROBLEM: To establish a reliable and trustworthy method for distinguishing valid knowledge from speculation, dogma, or error, particularly in an era of increasing scientific specialization and public discourse.
% FOUNDING_PROBLEM_CORROBORATION: Credentialed experts and academic institutions attest that the problem of distinguishing valid knowledge is still live and more critical than ever, citing misinformation and disinformation. Non-credentialed knowledge producers and critical scholars attest that while the problem is live, the current system exacerbates it by excluding valuable insights, and that the system's function has shifted to maintaining epistemic power structures.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__credentialed_expertise_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__credentialed_expertise_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimate_knowledge_boundary__credentialed_expertise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the significant resources, time, and social capital required to gain credentials and navigate the peer-review system, which often disproportionately benefits those already within the system. Suppression (0.78) is high due to the active exclusion of non-credentialed knowledge and the gatekeeping power of peer review, which can reject valid insights that don't conform to established methodologies or paradigms. The theater ratio (0.20) is relatively low, as the core functions of methodological rigor and peer review are genuinely performed, though their application can be performative in defending existing power structures. Accessibility collapse (0.70) is high because alternative paths to 'legitimate' knowledge are severely limited once the credentialed system is understood as the primary arbiter. Resistance (0.45) is moderate, coming from marginalized groups and critical scholars, but not strong enough to fundamentally challenge the system's dominance.
 *
 * PERSPECTIVAL GAP:
 *   Credentialed experts and academic institutions perceive this as a necessary 'rope' for quality control and coordination, ensuring reliable knowledge. Non-credentialed producers and marginalized communities experience it as a 'snare' that systematically devalues their knowledge and excludes them from epistemic authority. The engine's classification as a 'tangled_rope' captures this dual function of coordination and asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed experts, academic institutions, and funding bodies are clear beneficiaries, as the system channels prestige, resources, and authority to them. Non-credentialed knowledge producers, marginalized communities, and interdisciplinary researchers are victims/payers, as their knowledge is devalued, and they face significant barriers to entry and recognition. Public policy makers are observers, often constrained to rely on the 'legitimate' knowledge produced by the system.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to ensure reliable knowledge is still live, but its function has drifted. While it still coordinates, it increasingly extracts from and suppresses alternative knowledge forms. The classification as a tangled_rope prevents mislabeling it as a pure rope (as beneficiaries claim) or a pure snare (as victims claim), acknowledging both its coordination function and its asymmetric extraction. The rising extractiveness and suppression over time indicate a drift towards greater rent-seeking and gatekeeping, even as the core mandate persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_exclusion_cost,
    'What is the societal cost of systematically excluding non-credentialed and experiential knowledge from ''legitimate'' discourse, particularly in areas like public health, environmental justice, and social policy?',
    'Longitudinal studies comparing policy outcomes in contexts that integrate diverse knowledge forms versus those that rely solely on credentialed expertise, or economic modeling of ''lost'' innovation and social capital.',
    'If the cost is high, it would strongly challenge the efficiency and ethical justification of the current boundary, pushing for reclassification towards a snare or a more extractive tangled_rope. If low, it would reinforce the current system''s perceived utility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_exclusion_cost, empirical, 'Quantifying the impact of epistemic exclusion on societal well-being and problem-solving.').

omega_variable(
    legitimacy_grounding_ambiguity,
    'Is the legitimacy of credentialed expertise primarily grounded in its demonstrated methodological rigor and predictive power, or in its institutional power and historical precedent?',
    'Comparative analysis of knowledge systems: examining cases where methodological rigor is present without institutional credentialing, or where institutional power persists despite declining methodological quality.',
    'If legitimacy is primarily institutional, the constraint is more of a snare, as its persistence relies on power rather than epistemic merit. If primarily methodological, it leans more towards a rope, with extraction being a necessary cost of coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_grounding_ambiguity, conceptual, 'Understanding the true source of epistemic authority in the credentialed system.').

omega_variable(
    kernel_reading_difference,
    'This constraint is one reading of the ''legitimate_knowledge_boundary'' kernel. What would change structurally if the ''experiential_pluralism_reading'' or ''hybrid_coproduction_reading'' were adopted?',
    'Analysis of policy and funding shifts in jurisdictions or institutions that explicitly adopt alternative epistemic frameworks.',
    'Adopting the ''experiential_pluralism_reading'' would significantly lower extractiveness and suppression for non-credentialed producers, reclassifying this constraint closer to a rope or even a scaffold. The ''hybrid_coproduction_reading'' would lead to a more balanced distribution of authority and resources, likely resulting in a less extractive tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'Impact of alternative kernel readings on the constraint''s structure and classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__credentialed_expertise_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1950, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(legi_tr_t1970, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(legi_tr_t1990, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(legi_tr_t2010, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(legi_tr_t2024, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(legi_be_t1950, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(legi_be_t1970, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(legi_be_t1990, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(legi_be_t2010, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(legi_be_t2024, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1950, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(legi_su_t1970, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(legi_su_t1990, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 1990, 0.72).
narrative_ontology:measurement(legi_su_t2010, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 2010, 0.76).
narrative_ontology:measurement(legi_su_t2024, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__credentialed_expertise_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, scientific_funding_allocation).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, public_health_policy_formation).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, environmental_impact_assessment).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
