% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__experiential_pluralism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: legitimate_knowledge_boundary__experiential_pluralism_reading
 *   human_readable: Experiential Pluralism Reading of Legitimate Knowledge Boundary
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the experiential pluralism reading of
 *   a contested kernel about legitimate knowledge boundaries. The reading
 *   asserts that legitimate knowledge arises from lived experience and
 *   community validation, with methodological standards as one tool among
 *   many rather than the sole gate to legitimacy. This reading emerged in
 *   response to historical exclusion of indigenous, women, and non-Western
 *   knowledge systems by institutions that weaponized methodological rigor to
 *   devalue alternative epistemologies. The constraint operates as a tangled
 *   rope: it coordinates genuine recognition of excluded knowledge traditions
 *   (coordination function) while simultaneously extracting costs from
 *   credentialed experts and institutions whose monopoly on validation is
 *   eroded (extraction). Active enforcement is required because credentialed
 *   institutions resist loss of gatekeeping power, and because communities
 *   must assert their validation authority against institutional pressure to
 *   translate their knowledge into methodological frameworks. The
 *   constraint's theater ratio (0.44) reflects rising institutional
 *   incorporation of experiential frameworks as 'decolonization' or
 *   'community partnership' while actual power over validation remains
 *   contested—much of the machinery is performative compliance.
 *
 * KEY AGENTS:
 *   - experiential_communities: Set the terms of their own validation; coordinate around lived experience and community recognition. Benefit from institutional legitimacy without credentialing.
 *   - marginalized_knowledge_holders: Historical victims of methodological gatekeeping; gain authority and resources under this reading.
 *   - indigenous_and_local_practitioners: Identity-locked in their knowledge traditions; exit from this reading means exit from cultural identity.
 *   - academic_researchers: Bear cost of eroded methodological monopoly; must negotiate validation with communities.
 *   - credentialed_experts_with_methodological_monopoly: Institutional gatekeepers whose authority is challenged; resist loss of validation control.
 *   - knowledge_systems_dependent_on_institutional_validation: Medical licensing, agricultural regulation, psychology standards face pressure to accommodate alternative validation.
 *   - funding_bodies_and_institutions: Navigate between pressure to co-produce knowledge and traditional hierarchical control.
 *   - excluded_technical_experts: Cannot participate in this reading's framework; their knowledge domains require methodological validation for safety.
 *   - observer_epistemologists: Analytical seat documenting the epistemic contest and its institutional consequences.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.62).
domain_priors:suppression_score(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.71).
domain_priors:theater_ratio(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__experiential_pluralism_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__experiential_pluralism_reading, "Experiential Pluralism Reading of Legitimate Knowledge Boundary").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__experiential_pluralism_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__experiential_pluralism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__experiential_pluralism_reading, '94132659-881c-4319-9ab7-cf64b1a978b0').
narrative_ontology:cs_kernel_codification('94132659-881c-4319-9ab7-cf64b1a978b0', distributed).
narrative_ontology:cs_authority_grounding('94132659-881c-4319-9ab7-cf64b1a978b0', extraction).
narrative_ontology:cs_interpretation_layer_present('94132659-881c-4319-9ab7-cf64b1a978b0').
narrative_ontology:cs_reading_relation('94132659-881c-4319-9ab7-cf64b1a978b0', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('94132659-881c-4319-9ab7-cf64b1a978b0', legitimate_knowledge_boundary__hybrid_coproduction_reading, coexists_with).
narrative_ontology:cs_axiom('94132659-881c-4319-9ab7-cf64b1a978b0', foundational, experiential_knowledge_is_independently_valid).
narrative_ontology:cs_axiom_status(experiential_knowledge_is_independently_valid, holdable).
narrative_ontology:cs_axiom_grounding('94132659-881c-4319-9ab7-cf64b1a978b0', experiential_knowledge_is_independently_valid, deontological).
narrative_ontology:cs_axiom('94132659-881c-4319-9ab7-cf64b1a978b0', foundational, community_validation_suffices_without_methodological_translation).
narrative_ontology:cs_axiom_status(community_validation_suffices_without_methodological_translation, holdable).
narrative_ontology:cs_axiom_grounding('94132659-881c-4319-9ab7-cf64b1a978b0', community_validation_suffices_without_methodological_translation, conventional).
narrative_ontology:cs_axiom('94132659-881c-4319-9ab7-cf64b1a978b0', secondary, methodological_rigor_is_tool_not_gatekeeper).
narrative_ontology:cs_axiom_status(methodological_rigor_is_tool_not_gatekeeper, holdable).
narrative_ontology:cs_axiom_grounding('94132659-881c-4319-9ab7-cf64b1a978b0', methodological_rigor_is_tool_not_gatekeeper, instrumental).
narrative_ontology:cs_reference_frame('94132659-881c-4319-9ab7-cf64b1a978b0', epistemic_justice_through_autonomy).
narrative_ontology:cs_drift_state('94132659-881c-4319-9ab7-cf64b1a978b0', contemporary_institutional_capture_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('94132659-881c-4319-9ab7-cf64b1a978b0', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, experiential_communities).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_knowledge_holders).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, indigenous_and_local_practitioners).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_experts_with_methodological_monopoly).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, knowledge_systems_dependent_on_institutional_validation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, academic_researchers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, funding_bodies_and_institutions).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, epistemic_pluralism).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, decolonization_of_knowledge).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, community_sovereignty_over_validation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts that knowledge grounded in lived experience—environmental stewardship, healing practices, agricultural techniques, social navigation—constitutes legitimate knowledge without requiring external methodological certification. They set the terms of their own validation: peer recognition within the community, demonstrated practical efficacy, and continuity of the knowledge tradition. They benefit by having their knowledge recognized as authoritative within their context without subordination to external credentialing systems.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, experiential_communities, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, experiential_communities, beneficiary).

% Their knowledge systems—traditional medicine, ecological management, social technologies—have been historically devalued as unscientific by institutions that control credentialing. This reading legitimates their knowledge on its own terms without requiring translation into methodological frameworks designed by outsiders. Benefits accrue as institutional recognition and resource access based on experiential validity rather than methodological compliance.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_knowledge_holders, beneficiary,
    moderate, generational, constrained, regional).

% Hold knowledge systems embedded in place, relationship, and ancestral lineage. This reading asserts their knowledge is legitimate precisely because it emerges from deep, embodied engagement with a specific ecological and social context. Exit from this framing means exit from cultural identity itself. They bear costs when institutional systems still demand methodological translation; they gain when their knowledge is recognized as authoritative in its own right.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, indigenous_and_local_practitioners, beneficiary,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, indigenous_and_local_practitioners, agenda_setter).

% Face institutional pressure to incorporate experiential validation and community partnership into research design, publication, and credibility assessment. Methodological monopoly on legitimacy is eroded—their methods are now 'one tool among many' rather than the gate to legitimate knowledge. They must negotiate validation with communities whose epistemological authority they previously controlled unilaterally. Career advancement, funding, and publication prestige become partially hostage to community acceptance.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, academic_researchers, payer,
    institutional, biographical, constrained, global).

% Specialists in fields (medicine, agriculture, psychology) whose authority rested on exclusive command of validated methods now compete with experiential knowledge holders for authority. They bear the cost of reduced monopoly power—their methods can no longer unilaterally adjudicate what counts as legitimate knowledge. Regulatory and institutional deference that once flowed automatically to credentialed expertise must now be negotiated with experiential voices.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_experts_with_methodological_monopoly, payer,
    institutional, generational, constrained, global).

% Institutional structures (medical licensing, agricultural regulation, psychological practice standards) built on methodological validation must now accommodate or compete with alternative validation regimes. They face pressure to recognize experiential knowledge in policy, licensing, and practice standards—costs include regulatory complexity, legal liability if experiential approaches cause harm without methodological safeguards, and erosion of the institutional legitimacy foundation.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, knowledge_systems_dependent_on_institutional_validation, payer,
    institutional, generational, trapped, global).

% Control resources but increasingly face pressure to fund research that co-produces knowledge with communities rather than extracting data from them. They must negotiate funding allocation with non-credentialed knowledge holders, manage reputational risk from extraction or exploitation, and justify why experiential communities should not lead or control research agendas.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, funding_bodies_and_institutions, payer,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, funding_bodies_and_institutions, agenda_setter).

% Specialists in technical domains (nuclear engineering, pharmaceutical chemistry, epidemiology) where experiential knowledge alone cannot safely validate practice are locked out of the epistemic conversation during policy decisions. They would argue that some knowledge domains require methodological rigor because the cost of unvalidated claims is catastrophic failure. They cannot effectively participate in this reading's framework.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, excluded_technical_experts, excluded,
    institutional, biographical, trapped, global).

% May be harmed when experiential knowledge claims that lack methodological validation are treated as equally legitimate—e.g., a community adopting an untested healing practice that has community validation but no efficacy, or agricultural techniques that work in one microclimate but fail when applied elsewhere. Their exclusion from the conversation is structural: harm from false positive claims is not part of this reading's validation regime.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, communities_harmed_by_ineffective_experiential_claims, excluded,
    powerless, immediate, trapped, local).

% Analyze how legitimacy claims are constructed, what work different validation regimes perform, and what happens when competing epistemologies collide in practice. They do not advocate for either regime but document the structural consequences of this reading's ascendance.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, observer_epistemologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__experiential_pluralism_reading, experiential_communities).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__experiential_pluralism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of whose knowledge counts and how validation happens when credentialing systems have historically excluded certain communities and knowledge traditions. Coordinates around the principle that legitimacy can arise from lived experience and community recognition without requiring external methodological certification. Enables knowledge that was previously treated as folklore, tradition, or opinion to enter policy, practice, and institutional conversations on equal footing.
% TRANSFER_FUNCTION: Transfers epistemic authority from credentialed institutional experts to experiential communities and their own validation mechanisms. Moves the work of justification and proof from methodological rigor alone to include demonstrated efficacy, community endorsement, contextual embeddedness, and continuity of tradition. Redistributes power to define what counts as knowledge from institutions that controlled methodological standards to communities that live the knowledge.
% ABSENT_VOICES: Technical experts in domains where experiential knowledge alone cannot validate safety (nuclear engineers, pharmaceutical chemists, structural engineers) would argue this reading creates catastrophic risk by elevating non-validated claims to equal authority in contexts where failure is lethal. Communities historically harmed by ineffective experiential claims—patients given untested treatments, farmers whose traditional techniques fail outside their original context—are not in the conversation. Credentialed experts whose institutional authority is eroded by this reading cannot effectively challenge its premises without being read as defending exclusion.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, institutional control over methodological validation would reconsolidate; indigenous knowledge systems would lose institutional legitimacy and resource access; communities would lose the authority to validate their own knowledge; the power dynamic between credentialed experts and experiential communities would revert to institutional asymmetry. Research funding, medical licensing, policy-making, and educational institutions would realign around methodological gatekeeping as the sole legitimacy criterion.
% FOUNDING_PROBLEM: Historically credentialed, institutional methodologies were used to devalue and exclude knowledge systems held by indigenous peoples, communities of color, women, and the Global South. Methodological rigor became a proxy for truth that systematically disadvantaged knowledge traditions embedded in lived experience, place, and community practice. Legitimate knowledge was restricted to what could be documented in the disciplinary frameworks designed by institutions in the Global North.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science, postcolonial theorists, and indigenous scholars outside credentialed epistemology establish that methodology was historically weaponized to exclude. Communities whose knowledge systems were devalued attest to ongoing institutional dismissal of their knowledge when it is not translated into methodological frameworks. However, public health officials, pharmaceutical regulators, and structural engineers attest that the founding problem of HISTORICAL EXCLUSION is distinct from the claim that methodological standards no longer serve a gatekeeping function—they argue exclusion was the social problem, not the methodological standards themselves.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__experiential_pluralism_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__experiential_pluralism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__experiential_pluralism_reading, 'none', 1).

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
 *   Extractiveness is 0.62 because the reading transfers epistemic authority from credentialed institutions to experiential communities—a real extraction of power and institutional resources from one seat to another. It is not higher (0.8+) because genuine coordination benefits accrue: excluded knowledge traditions gain legitimacy, and communities gain autonomy over their own knowledge validation. Suppression is 0.71 because active institutional enforcement is required to prevent reversion to credential monopoly—the reading must suppress credentialed experts' claims to exclusive validation authority, and it must suppress internalized epistemic hierarchies in communities that have internalized the belief that they need credentialing to be heard. Theater ratio is 0.44 (moderate) because much contemporary institutional incorporation of experiential frameworks is performative—research partnerships that maintain hierarchical control, decolonization language that masks extraction, community consultation that is non-binding. The measurement series show extractiveness and theater rising over the interval as the reading gains institutional visibility; suppression stabilizes, suggesting the enforcement burden becomes routinized rather than escalating. Accessibility collapse is 0.48 because alternatives do not fully disappear: credentialed expertise remains available and powerful, hybrid approaches exist, and the methodological option persists even as it is decentered. Resistance is 0.73 because significant institutional and expert pushback opposes this reading; credentialed institutions defend methodological gatekeeping; technical experts argue some domains require methodological validation for safety.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (credentialed experts, institutional validators) and the beneficiary seats (experiential communities, marginalized knowledge holders) compute different types from identical structural conditions. For institutional validators, this reading operates as a snare: extracted monopoly, suppressed authority, active enforcement required to maintain the constraint. For experiential communities, it operates as rope or even mountain-adjacent: genuine coordination solving real exclusion, minimal enforcement burden because it aligns with their existing practices. The engine computes this divergence from power atoms, exit options, and directionality: institutional payers sit at high d (full targets), experiential beneficiaries sit at low d (beneficiaries). The authored claim (tangled rope) is the reading's own framing; the metrics describe extractiveness from institutional seats, which is real.
 *
 * DIRECTIONALITY LOGIC:
 *   Experiential communities and marginalized knowledge holders derive directionality near the beneficiary end (d ~0.2-0.3) because the constraint benefits them (recognition, authority, resources) and their exit options are mobile or identity-locked in ways that support participation. Credentialed experts derive directionality near the target end (d ~0.7-0.8) because the constraint extracts their monopoly power, their exit options are constrained (they cannot opt out of the epistemic conversation), and they bear costs. Institutional validators occupy intermediate ground (d ~0.6) because they lose gatekeeping authority but gain co-production partnerships and potentially new legitimacy. The reading's own framing (tangled rope) treats it as genuine coordination of excluded voices with extraction from credentialed monopoly. An alternative framing (the hybrid_coproduction_reading) would distribute benefits and costs differently, creating a constraint family where each reading has a different ε and different beneficiary/victim structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—historical exclusion of non-Western and experiential knowledge from institutional legitimacy—is live for marginalized communities and dead for institutions that have incorporated decolonization frameworks. The disappearance verdict is world_rearranges because institutional structures, funding flows, and policy legitimacy depend on this reading, and dismantling it would reconsolidate credentialing monopoly. However, the gap between founding problem (exclusion) and current operation (distributed validation with institutional suppression) creates risk of mandatrophy: the reading was justified by a justice claim (inclusion of excluded voices) but increasingly operates as a new form of extraction and theater (institutions co-opt the language of experiential validation while maintaining hierarchical control). The measurement series show theater rising while suppression plateaus, which is consistent with theatrical maintenance of the constraint's legitimacy language without actual power transfer. This does not yet constitute mandatrophy_resolved, but the trajectory suggests the founding problem may be obsoleted by institutional capture of the reading's rhetoric while its substance remains contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foundational_axiom_contest,
    'Is legitimacy of knowledge fundamentally a property of its origin (lived experience, community validation) or a property of its correspondence to reality and reliability of its predictions?',
    'This is a conceptual divide, not an empirical one. Resolution would require commitment to an epistemological theory about what ''legitimacy'' means—whether it is about power/recognition or about truth/reliability. Different philosophical traditions give different answers.',
    'If legitimacy is primarily about recognition and social position, this reading is coherent and the founding problem of exclusion is real. If legitimacy is primarily about correspondence to reality, then experiential validation without methodological testing may produce false positives with real-world harms. The classification depends on which axiom is accepted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foundational_axiom_contest, conceptual, 'Whether epistemic legitimacy is about social recognition or about truth-correspondence.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.71) primarily structural—institutional gatekeeping, funding control, regulatory bars that prevent experiential knowledge from entering policy—or internalized—the internalized belief among credentialed experts that their methods are the only legitimate way to know, and among some communities that they need credentialing to be heard?',
    'Post-institutional-shift suppression trajectory: if suppression persists after gatekeeping barriers are removed (e.g., communities are given authority and resources), reclassify as partially internalized. If suppression declines sharply when barriers fall, it was primarily structural.',
    'If suppression is primarily structural, removing barriers should enable the reading to operate with low ongoing enforcement cost. If internalized, the reading must maintain active enforcement of its validation criteria to prevent reversion to credentialism—which would make it a higher-cost snare than a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of methodological monopoly is structural gatekeeping or internalized epistemic hierarchy.').

omega_variable(
    practical_harm_from_invalid_experiential_claims,
    'What is the rate of harmful outcomes when experiential claims that are community-validated but lack methodological testing are treated as equally legitimate in domains where false positives have material consequences (medicine, public health, engineering, agriculture)?',
    'Comparative outcome studies: track health, safety, and efficacy outcomes in communities where experiential knowledge is implemented with and without methodological validation. Establish baseline harm rates from credentialed-expert-only regimes for comparison.',
    'High harm rates from experiential-only validation in safety-critical domains would establish constraints on which domains this reading can coherently apply. It might degrade from tangled rope (real coordination benefit + extraction cost) to snare (the coordination benefit is illusory; victims are harmed). Alternatively, it would show the reading requires methodological backup in specific domains while maintaining autonomy in others.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(practical_harm_from_invalid_experiential_claims, empirical, 'Whether removing methodological validation from safety-critical knowledge domains produces unacceptable harm rates.').

omega_variable(
    kernel_reading_contest,
    'This reading is one instantiation of a contested kernel about legitimate knowledge boundaries. The sibling readings—credentialed_expertise_reading and hybrid_coproduction_reading—instantiate different axioms about what validates knowledge. Which reading''s axioms should structure how communities and institutions decide what counts as knowledge?',
    'This is a normative and political question, not an empirical one. Resolution occurs through political process, institutional power struggles, and the lived consequences of each reading''s implementation. The three readings coexist in contemporary practice across different institutional contexts.',
    'The classification of this constraint as tangled rope (vs. snare or rope) depends partly on whether the reader accepts its foundational axioms about equality of epistemic authority. An institution that privileges credentialed expertise would classify this as a snare. An institution that privileges experiential validation would classify it as a rope enabling justice. The kernel context is that all three readings are live, and the contest itself is what defines the contemporary epistemic landscape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Inter-kernel reading contest: which epistemological reading should govern knowledge legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__experiential_pluralism_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t5, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(legi_tr_t5, observed).
narrative_ontology:measurement(legi_tr_t10, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement_basis(legi_tr_t10, observed).
narrative_ontology:measurement(legi_tr_t15, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(legi_tr_t15, observed).
narrative_ontology:measurement(legi_tr_t25, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(legi_tr_t25, observed).
narrative_ontology:measurement(legi_tr_t40, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement_basis(legi_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t5, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(legi_be_t5, observed).
narrative_ontology:measurement(legi_be_t10, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(legi_be_t10, observed).
narrative_ontology:measurement(legi_be_t15, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(legi_be_t15, observed).
narrative_ontology:measurement(legi_be_t25, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement_basis(legi_be_t25, observed).
narrative_ontology:measurement(legi_be_t40, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(legi_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t5, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 5, 0.66).
narrative_ontology:measurement_basis(legi_su_t5, observed).
narrative_ontology:measurement(legi_su_t10, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(legi_su_t10, observed).
narrative_ontology:measurement(legi_su_t15, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(legi_su_t15, observed).
narrative_ontology:measurement(legi_su_t25, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(legi_su_t25, observed).
narrative_ontology:measurement(legi_su_t40, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(legi_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__experiential_pluralism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.12).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, indigenous_knowledge_institutional_validation).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, scientific_consensus_authority).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the legitimate_knowledge_boundary kernel. Its sibling readings (credentialed_expertise_reading and hybrid_coproduction_reading) are separate constraint stories with different ε values, beneficiary/victim structures, and classifications. All three stories should be generated and linked via network.affects_constraints to represent the kernel contest. Decomposition follows the ε-invariance principle: each reading instantiates a different constraint because the structural relationship between experiential validation and institutional authority differs radically across readings. The kernel itself (the question 'what counts as legitimate knowledge?') is the commitment system; the readings are different interpretations of that kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_knowledge_boundary__experiential_pluralism_reading, institutional, 0.75).
constraint_indexing:directionality_override(legitimate_knowledge_boundary__experiential_pluralism_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
