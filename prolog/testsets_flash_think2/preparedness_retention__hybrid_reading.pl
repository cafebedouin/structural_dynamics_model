% ============================================================================
% CONSTRAINT STORY: preparedness_retention__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__hybrid_reading, []).

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
 *   constraint_id: preparedness_retention__hybrid_reading
 *   human_readable: Stratified Disaster Preparedness (Hybrid Reading)
 *   domain: disaster_preparedness/institutional_memory/governance
 *
 * SUMMARY:
 *   This constraint describes a stratified system of disaster preparedness,
 *   where deep technical competence is concentrated within specialized
 *   institutions (e.g., Rijkswaterstaat, water boards), while broader
 *   societal engagement in preparedness becomes largely ceremonial. This is
 *   the 'hybrid_reading' of the 'preparedness_retention' kernel,
 *   acknowledging both genuine competence and performative aspects. The
 *   system coordinates complex risk management but extracts distributed
 *   resilience from the general public.
 *
 * KEY AGENTS:
 *   - specialized_institutions: Primary beneficiary/agenda_setter (institutional/arbitrage) — retains competence and authority
 *   - broader_society: Primary payer/victim (powerless/constrained) — loses distributed resilience, participates ceremonially
 *   - local_communities: Payer/victim (moderate/constrained) — dependent on central institutions, limited local agency
 *   - emergency_management_agencies: Beneficiary/agenda_setter (institutional/constrained) — relies on and reinforces centralized expertise
 *   - policy_makers: Agenda_setter/observer (powerful/mobile) — funds and oversees, influenced by visible performance
 *   - advocates_for_distributed_resilience: Excluded (organized/constrained) — would object to centralization, but marginalized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, 0.65).
domain_priors:suppression_score(preparedness_retention__hybrid_reading, 0.55).
domain_priors:theater_ratio(preparedness_retention__hybrid_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_retention__hybrid_reading, "Stratified Disaster Preparedness (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_retention__hybrid_reading, "disaster_preparedness/institutional_memory/governance").

domain_priors:requires_active_enforcement(preparedness_retention__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__hybrid_reading, '08cf3fb0-cbc7-4307-b720-9e490b014c94').
narrative_ontology:cs_kernel_codification('08cf3fb0-cbc7-4307-b720-9e490b014c94', formalized).
narrative_ontology:cs_authority_grounding('08cf3fb0-cbc7-4307-b720-9e490b014c94', expertise).
narrative_ontology:cs_interpretation_layer_present('08cf3fb0-cbc7-4307-b720-9e490b014c94').
narrative_ontology:cs_reading_relation('08cf3fb0-cbc7-4307-b720-9e490b014c94', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('08cf3fb0-cbc7-4307-b720-9e490b014c94', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_axiom('08cf3fb0-cbc7-4307-b720-9e490b014c94', foundational, centralized_technical_competence_is_necessary).
narrative_ontology:cs_axiom_status(centralized_technical_competence_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('08cf3fb0-cbc7-4307-b720-9e490b014c94', centralized_technical_competence_is_necessary, empirically_contingent).
narrative_ontology:cs_axiom('08cf3fb0-cbc7-4307-b720-9e490b014c94', secondary, public_engagement_as_symbolic_reassurance).
narrative_ontology:cs_axiom_status(public_engagement_as_symbolic_reassurance, holdable).
narrative_ontology:cs_axiom_grounding('08cf3fb0-cbc7-4307-b720-9e490b014c94', public_engagement_as_symbolic_reassurance, empirically_contingent).
narrative_ontology:cs_reference_frame('08cf3fb0-cbc7-4307-b720-9e490b014c94', efficient_centralized_risk_management).
narrative_ontology:cs_drift_state('08cf3fb0-cbc7-4307-b720-9e490b014c94', contemporary_climate_change_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('08cf3fb0-cbc7-4307-b720-9e490b014c94', '').
narrative_ontology:cs_kernel_id(preparedness_retention__hybrid_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, specialized_institutions).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, institutional_continuity).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, emergency_management_agencies).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, broader_society).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, distributed_resilience).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, local_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions (e.g., Rijkswaterstaat, water boards) retain deep technical competence, manage critical infrastructure, and benefit from their indispensable role in complex disaster management. They set the standards and protocols for preparedness.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, specialized_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Relies on specialized institutions for safety and security, but loses direct competence and agency in disaster preparedness. Participates in public-facing drills and campaigns that are often more ceremonial than competence-building, bearing the cost of lost distributed resilience.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, broader_society, payer,
    powerless, biographical, constrained, national).

% Depends heavily on central institutions for major disaster response, often lacking comprehensive local knowledge or resources for self-organization beyond basic measures. Their local initiatives may be overlooked or underfunded in favor of centralized approaches.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, local_communities, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, local_communities, excluded).

% Coordinate disaster responses and rely on the technical expertise of specialized institutions. They benefit from clear lines of authority and a centralized knowledge base, but may also contribute to the ceremonial aspects of public preparedness.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, emergency_management_agencies, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, emergency_management_agencies, agenda_setter).

% Fund and oversee the disaster preparedness system. They are often influenced by visible performance and institutional claims of competence, potentially reinforcing the stratified model without fully assessing the costs to distributed resilience.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, policy_makers, agenda_setter,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, policy_makers, observer).

% Argue for empowering communities with direct competence and local knowledge networks, rather than relying solely on centralized expertise. Their proposals often struggle for funding and institutional recognition within the existing stratified system.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, advocates_for_distributed_resilience, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__hybrid_reading, specialized_institutions).
narrative_ontology:fixing_cost_class(preparedness_retention__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes complex technical knowledge and operational capacity for large-scale disaster management (e.g., water control, infrastructure protection), ensuring a consistent, expert-led response to high-impact events.
% TRANSFER_FUNCTION: Transfers the burden of deep technical competence and continuous operational readiness from broader society to specialized institutions, in exchange for a perceived sense of security and institutional continuity. It also transfers resources (funding, authority) to these central bodies.
% ABSENT_VOICES: Advocates for distributed, bottom-up resilience and local knowledge networks are often marginalized; they would argue for empowering communities with direct competence rather than ceremonial roles, but are kept out of core decision-making by the centralized structure.
% DISAPPEARANCE_RATIONALE: If this stratified system vanished overnight, the immediate technical capacity for managing complex, large-scale environmental risks would collapse, leading to catastrophic failures (e.g., widespread flooding, infrastructure breakdown). Society would have to rapidly re-learn or rebuild distributed competence, a process that would take decades and incur immense costs.
% FOUNDING_PROBLEM: Managing complex, large-scale environmental risks (such as water management in low-lying areas or national infrastructure protection) that require specialized, continuous technical expertise and long-term planning beyond general public knowledge or local capacity.
% FOUNDING_PROBLEM_CORROBORATION: Specialized institutions and many policymakers attest the problem is still live, citing ongoing climate change, evolving threats, and the need for continuous infrastructure maintenance. Some community resilience advocates contest the *solution* (centralization), arguing for more distributed models, but not the underlying problem of managing complex risks; their testimony supports the problem's persistence.
narrative_ontology:disappearance_verdict(preparedness_retention__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(preparedness_retention__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__hybrid_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `tangled_rope` classification reflects the dual nature: a genuine coordination function (managing complex risks with specialized expertise) combined with asymmetric extraction (the cost to broader society of losing distributed resilience and agency). Extractiveness is moderate-high (0.65) due to this loss of societal capacity. Suppression (0.55) is present as the centralized model actively discourages or defunds alternative, more distributed approaches. Theater ratio (0.45) is significant because public-facing preparedness activities often serve more to reassure and legitimize the central institutions than to build widespread, actionable competence. The metrics show a gradual increase in extraction and theatricality over time as the system entrenches.
 *
 * PERSPECTIVAL GAP:
 *   Specialized institutions and emergency management agencies perceive this system as an efficient and necessary coordination mechanism for complex risks, leveraging expert knowledge. Broader society and local communities, however, experience it as a disempowering structure that reduces their agency and capacity for self-reliance, even while providing a sense of security. Policy makers may oscillate between these views, prioritizing efficiency or public engagement depending on political pressures.
 *
 * DIRECTIONALITY LOGIC:
 *   Specialized institutions and emergency management agencies are beneficiaries, gaining authority, resources, and institutional continuity from their indispensable role (low directionality). Broader society and local communities are targets, bearing the cost of lost distributed resilience and agency (high directionality). Policy makers are agenda-setters who benefit from a seemingly stable system but also bear the political cost of any failures. Advocates for distributed resilience are excluded, their alternatives suppressed by the existing structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure 'rope' (ignoring the extraction of distributed resilience) or a pure 'snare' (ignoring the genuine technical competence retained). It highlights that while the founding problem (complex risk management) remains live, the *solution* has evolved to create an extractive dynamic where societal memory becomes ceremonial, indicating a potential for mandatrophy in the broader societal function, even as the core technical function persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''hybrid_reading'' of the ''preparedness_retention'' kernel?',
    'Comparative analysis with ''competence_reading'' and ''husk_reading'' constraints, assessing the balance of retained competence versus ceremonial performance in empirical case studies.',
    'If the balance shifts, the constraint might reclassify towards ''competence_reading'' (lower extraction, less theater) or ''husk_reading'' (higher extraction, more theater, potentially a snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms this constraint''s identity as a specific reading of the preparedness kernel.').

omega_variable(
    ceremonial_function_ambiguity,
    'Does the ''ceremonial'' aspect of broader societal preparedness serve any latent, unacknowledged coordination function (e.g., social cohesion, trust-building, basic awareness) beyond mere performance?',
    'Sociological studies on the latent functions of ritual in disaster response, or comparative analysis with societies that lack such ceremonial aspects.',
    'If latent functions are significant, the `theater_ratio` might be slightly lower, and the `extractiveness` from broader society might be marginally reduced, as some diffuse benefit is present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremonial_function_ambiguity, empirical, 'Assesses the true functional value of ceremonial preparedness activities.').

omega_variable(
    cost_of_distributed_resilience,
    'What is the true cost (economic, social, political) of building and maintaining a genuinely distributed, high-competence societal resilience system compared to the current stratified model?',
    'Comprehensive economic modeling and social impact assessments of alternative preparedness models, including pilot programs for distributed competence.',
    'If the cost of distributed resilience is prohibitively high, the current system''s `extractiveness` might be re-evaluated as a necessary trade-off for efficiency. If the cost is manageable, the current system''s `extractiveness` is confirmed as a choice, not an inevitability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cost_of_distributed_resilience, empirical, 'Evaluates the feasibility and cost-effectiveness of alternative preparedness models.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__hybrid_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(prep_tr_t10, preparedness_retention__hybrid_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(prep_tr_t20, preparedness_retention__hybrid_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(prep_tr_t30, preparedness_retention__hybrid_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__hybrid_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement(prep_tr_t50, preparedness_retention__hybrid_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__hybrid_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(prep_be_t10, preparedness_retention__hybrid_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(prep_be_t20, preparedness_retention__hybrid_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(prep_be_t30, preparedness_retention__hybrid_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__hybrid_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(prep_be_t50, preparedness_retention__hybrid_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__hybrid_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(prep_su_t10, preparedness_retention__hybrid_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(prep_su_t20, preparedness_retention__hybrid_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(prep_su_t30, preparedness_retention__hybrid_reading, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(prep_su_t40, preparedness_retention__hybrid_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(prep_su_t50, preparedness_retention__hybrid_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__hybrid_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
