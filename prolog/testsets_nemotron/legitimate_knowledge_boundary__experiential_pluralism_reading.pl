% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__experiential_pluralism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: legitimate_knowledge_boundary__experiential_pluralism_reading
 *   human_readable: Experiential Pluralism Boundary on Legitimate Knowledge
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the experiential pluralism reading of
 *   the legitimate_knowledge_boundary kernel. It asserts that legitimate
 *   knowledge arises from lived experience and community validation, with
 *   methodological standards as one tool among many — not as a gatekeeping
 *   prerequisite. The reading emerged from feminist standpoint theory,
 *   postcolonial epistemology, indigenous knowledge movements, and
 *   participatory action research (1970s–present). Its boundary lowers
 *   barriers to epistemic participation, distributes validation authority to
 *   affected communities, and redefines expertise as context-specific rather
 *   than credential-conferred. The credentialed_expertise_reading and
 *   hybrid_coproduction_reading are sibling constraints in the same kernel
 *   family, not perspectives within this constraint.
 *
 * KEY AGENTS:
 *   - marginalized_epistemic_communities: Primary beneficiary (powerless/identity_locked) — gains epistemic authority and validation access
 *   - community_knowledge_holders: Beneficiary (moderate/identity_locked) — traditional, indigenous, and experiential knowledge recognized as legitimate
 *   - participatory_research_practitioners: Beneficiary (organized/mobile) — institutional foothold for co-production methodologies
 *   - credentialed_experts: Payer/Excluded (powerful/arbitrage) — loses monopoly on epistemic authority, faces competition from experiential claims
 *   - institutional_science_funders: Agenda_setter (institutional/arbitrage) — allocates resources across competing epistemic frameworks
 *   - policy_makers: Observer (institutional/analytical) — navigates competing knowledge claims for decision-making
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.28).
domain_priors:suppression_score(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.15).
domain_priors:theater_ratio(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__experiential_pluralism_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__experiential_pluralism_reading, rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__experiential_pluralism_reading, "Experiential Pluralism Boundary on Legitimate Knowledge").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__experiential_pluralism_reading, "epistemology/science_and_technology_studies/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__experiential_pluralism_reading, '9c4f8ab7-d6a7-4422-8dc5-3959cf6ce045').
narrative_ontology:cs_kernel_codification('9c4f8ab7-d6a7-4422-8dc5-3959cf6ce045', distributed).
narrative_ontology:cs_authority_grounding('9c4f8ab7-d6a7-4422-8dc5-3959cf6ce045', distributed).
narrative_ontology:cs_reading_relation('9c4f8ab7-d6a7-4422-8dc5-3959cf6ce045', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('9c4f8ab7-d6a7-4422-8dc5-3959cf6ce045', legitimate_knowledge_boundary__hybrid_coproduction_reading, influences).
narrative_ontology:cs_axiom('9c4f8ab7-d6a7-4422-8dc5-3959cf6ce045', foundational, lived_experience_as_primary_epistemic_resource).
narrative_ontology:cs_axiom_status(lived_experience_as_primary_epistemic_resource, holdable).
narrative_ontology:cs_axiom_grounding('9c4f8ab7-d6a7-4422-8dc5-3959cf6ce045', lived_experience_as_primary_epistemic_resource, deontological).
narrative_ontology:cs_axiom('9c4f8ab7-d6a7-4422-8dc5-3959cf6ce045', foundational, community_validation_suffices_for_legitimacy).
narrative_ontology:cs_axiom_status(community_validation_suffices_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('9c4f8ab7-d6a7-4422-8dc5-3959cf6ce045', community_validation_suffices_for_legitimacy, conventional).
narrative_ontology:cs_axiom('9c4f8ab7-d6a7-4422-8dc5-3959cf6ce045', secondary, methodological_standards_are_tools_not_gates).
narrative_ontology:cs_axiom_status(methodological_standards_are_tools_not_gates, holdable).
narrative_ontology:cs_axiom_grounding('9c4f8ab7-d6a7-4422-8dc5-3959cf6ce045', methodological_standards_are_tools_not_gates, instrumental).
narrative_ontology:cs_reference_frame('9c4f8ab7-d6a7-4422-8dc5-3959cf6ce045', epistemic_justice_framework).
narrative_ontology:cs_drift_state('9c4f8ab7-d6a7-4422-8dc5-3959cf6ce045', contemporary_institutional_uptake, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9c4f8ab7-d6a7-4422-8dc5-3959cf6ce045', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_epistemic_communities).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, community_knowledge_holders).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, participatory_research_practitioners).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__experiential_pluralism_reading, social_movement_epistemologists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_experts).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, situated_knowledge_thesis).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, epistemic_injustice_framework).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, participatory_paradigm).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__experiential_pluralism_reading, strong_objectivity_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities historically excluded from epistemic authority (indigenous peoples, disabled communities, racialized groups, gender/sexual minorities, Global South knowledge holders). They gain recognition of their situated knowledge as legitimate without requiring methodological translation. Their exit from this constraint is identity-locked: their epistemic standing is constitutive of their collective self-understanding and political claims. They cannot 'leave' the need for epistemic recognition without abandoning their struggle for justice.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_epistemic_communities, beneficiary,
    powerless, generational, identity_locked, global).

% Elders, healers, land-based practitioners, oral tradition keepers, and craft practitioners whose knowledge is validated through community practice rather than institutional credentials. They set local validation agendas but remain dependent on broader institutional recognition for resource access. Exit is identity-locked: their knowledge-holding role is inseparable from their community identity and relational obligations.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, community_knowledge_holders, beneficiary,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, community_knowledge_holders, agenda_setter).

% Academic and non-academic researchers using participatory action research, community-based participatory research, citizen science, and co-production methodologies. They gain institutional legitimacy, funding pathways, and publication venues. Their exit is mobile: they can shift to conventional research paradigms, though with career costs. They are not identity-locked to the methodology.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, participatory_research_practitioners, beneficiary,
    organized, biographical, mobile, national).

% Activist-scholars and movement intellectuals who articulate epistemic justice frameworks (standpoint theory, epistemic injustice, decolonial epistemology). They set the theoretical agenda for the constraint's validation standards. Exit is constrained: their intellectual project is tied to the movements they serve, but they could pivot to other frameworks.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, social_movement_epistemologists, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, social_movement_epistemologists, agenda_setter).

% Scientists, scholars, and professionals whose epistemic authority derives from disciplinary credentials, peer review, and methodological rigor. They experience the constraint as loss of monopoly: their validation standards are no longer the sole gate to legitimacy. They retain high exit options (arbitrage) — they can operate within their disciplines, ignore the pluralism discourse, or engage selectively. Their power cushions the extraction; they are not trapped.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_experts, payer,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__experiential_pluralism_reading, credentialed_experts, excluded).

% Government agencies, foundations, and international bodies allocating research funding. They now navigate competing validity claims and face pressure to fund experiential-pluralism-aligned work. They set agendas through funding priorities but are not bound to any single epistemic framework. Exit is arbitrage: they can adjust portfolio allocation across epistemic paradigms.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, institutional_science_funders, agenda_setter,
    institutional, generational, arbitrage, national).

% Legislators, regulators, and administrators who must adjudicate between competing knowledge claims for decision-making (public health, environmental regulation, education, Indigenous rights). They do not produce or validate knowledge but depend on legitimate knowledge boundaries for authority. Their seat is analytical: they observe the constraint's operation from the decision-making periphery.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__experiential_pluralism_reading, policy_makers, observer,
    institutional, immediate, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__experiential_pluralism_reading, marginalized_epistemic_communities).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__experiential_pluralism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of epistemic exclusion: enables communities to validate their own knowledge without requiring translation into dominant methodological frameworks, creating distributed epistemic authority that matches the diversity of human experience.
% TRANSFER_FUNCTION: Moves epistemic authority, funding access, publication venues, and policy influence from credentialed-expertise institutions toward community-validated knowledge holders and participatory research practitioners. The transfer is not zero-sum — the constraint claims to expand the total epistemic pie — but resource competition makes it functionally redistributive.
% ABSENT_VOICES: Communities that reject both credentialed expertise and experiential pluralism in favor of other epistemic frameworks (e.g., certain religious epistemologies, traditional authorities that are patriarchal or hierarchical, anti-science movements that weaponize 'lived experience' for denialism). These voices are excluded because the constraint's validation logic requires a commitment to epistemic justice and power analysis that they do not share.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, marginalized communities would lose the primary institutional lever for epistemic recognition gained in the last 50 years. Funding programs, publication venues, policy consultation mandates, and legal standards (e.g., UNDRIP free prior informed consent) that operationalize experiential pluralism would lose their normative foundation. Credentialed expertise would reconsolidate its monopoly. The world of epistemic authority would rearrange significantly.
% FOUNDING_PROBLEM: Epistemic injustice: credentialed scientific institutions systematically excluded, marginalized, and devalued the knowledge of colonized peoples, women, disabled people, racialized communities, and Global South knowers — treating their lived experience as anecdote, bias, or 'traditional belief' rather than legitimate knowledge. This exclusion enabled epistemic violence, policy harm, and the theft of intellectual heritage.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by: (1) the credentialed_expertise_reading's own historical record (e.g., histories of scientific racism, medical experimentation on marginalized groups, dismissal of indigenous ecological knowledge) — the 'perpetrator' institutions document the exclusion; (2) international human rights bodies (UN Permanent Forum on Indigenous Issues, CRPD Committee) recognizing epistemic exclusion as a rights violation; (3) independent historiography of science (Harding, Tuhiwai Smith, Medina, Fricker) from outside the benefiting communities. No major epistemic authority denies the historical exclusion; the contest is whether it persists and whether this constraint solves it.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__experiential_pluralism_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__experiential_pluralism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(legitimate_knowledge_boundary__experiential_pluralism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.28) because the constraint's primary operation is inclusionary — it expands who counts as a knower rather than extracting from a defined target. Suppression is low (0.15) because the constraint does not actively prevent credentialed inquiry; it contests the credentialed framework's monopoly on legitimacy. Theater ratio (0.22) reflects performative inclusion in some institutional settings (diversity statements without structural power-sharing). Accessibility collapse (0.35) is moderate: the constraint opens epistemic space but alternative validation structures are still building capacity. Resistance (0.42) comes from credentialed institutions defending epistemic monopoly and from internal debates about relativism.
 *
 * PERSPECTIVAL GAP:
 *   From the marginalized_epistemic_communities seat, this is a rope — genuine coordination solving the problem of epistemic exclusion. From the credentialed_experts seat, the same constraint feels like extraction — their monopoly on legitimacy is dissolved without compensation. The engine computes this divergence from the structural data: beneficiaries get low d (subsidy), the excluded credentialed seat gets higher d (extraction). The claimed rope type reflects the authoring seat (participatory epistemology); the engine may compute tangled_rope for the credentialed seat if redistribution is confirmed.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are marginalized_epistemic_communities, community_knowledge_holders, participatory_research_practitioners, and social_movement_epistemologists — they gain epistemic authority, funding access, and validation pathways. The credentialed_expertise seat is not listed as a victim because the constraint does not actively extract from them; it contests their exclusive claim. However, the credentialed seat experiences effective extraction via redistributed epistemic resources (funding, publication, policy influence). This redistributive dynamic is the core structural ambiguity captured in the extraction_mechanism_ambiguity omega.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (epistemic exclusion of marginalized knowers) remains live (contested status). The constraint has not resolved its mandatropy because credentialed institutions still dominate resource allocation, and the experiential pluralism reading has not achieved structural parity. The constraint is not a piton — it is actively maintained by social movements and has growing institutional uptake. The theater ratio rise (0.12→0.22) signals performative adoption without power transfer, a mandatrophy risk if institutional capture outpaces genuine redistribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is the experiential pluralism reading a distinct constraint with its own ε, or a perspectival lens on the credentialed expertise reading?',
    'Apply the ε-invariance test: if changing the observable (lived experience vs. methodological rigor) changes ε, they are distinct constraints. The decomposition into three readings with different beneficiary/victim structures and different extractiveness profiles confirms distinctness.',
    'If distinct, each reading gets its own classification and metrics; the kernel is the contested label, not a single constraint. The three readings form a constraint family linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether the kernel''s readings are structurally distinct constraints or observer-relative framings of one constraint.').

omega_variable(
    extraction_mechanism_ambiguity,
    'Does the low extractiveness score reflect genuine low extraction, or does the constraint extract by excluding methodological rigor as a gatekeeping mechanism?',
    'Trace resource flows: who gains funding, publication venues, policy influence, and epistemic authority under this boundary? If experiential-pluralism-aligned institutions capture resources previously held by credentialed-expertise institutions, extraction is redistributive, not absent.',
    'If redistributive, the constraint is a tangled_rope (coordination + asymmetric extraction) rather than a rope. The current low ε scores coordination; redistribution would raise ε for the credentialed expertise seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_ambiguity, empirical, 'Whether experiential pluralism''s boundary operates as pure coordination or as extraction from the credentialed expertise seat.').

omega_variable(
    internalized_suppression_in_communities,
    'Is the measured suppression (0.15) structural, or do marginalized communities internalize exclusion from credentialed epistemology as epistemic inferiority?',
    'Post-exit trajectory: if communities that establish their own validation structures still experience epistemic devaluation from external institutions, suppression is partially internalized and the structural measure understates effective suppression.',
    'If internalized suppression is substantial, the constraint''s effective suppression for the marginalized_epistemic_communities seat is higher than 0.15, potentially shifting seat classification toward snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_in_communities, empirical, 'Structural vs. internalized suppression mechanism for marginalized epistemic communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__experiential_pluralism_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lkn_exp_plur_tr_t1970, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(lkn_exp_plur_tr_t1990, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(lkn_exp_plur_tr_t2005, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(lkn_exp_plur_tr_t2015, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(lkn_exp_plur_tr_t2025, legitimate_knowledge_boundary__experiential_pluralism_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(lkn_exp_plur_be_t1970, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(lkn_exp_plur_be_t1990, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(lkn_exp_plur_be_t2005, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 2005, 0.32).
narrative_ontology:measurement(lkn_exp_plur_be_t2015, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 2015, 0.28).
narrative_ontology:measurement(lkn_exp_plur_be_t2025, legitimate_knowledge_boundary__experiential_pluralism_reading, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(lkn_exp_plur_su_t1970, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 1970, 0.25).
narrative_ontology:measurement(lkn_exp_plur_su_t1990, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(lkn_exp_plur_su_t2005, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 2005, 0.18).
narrative_ontology:measurement(lkn_exp_plur_su_t2015, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 2015, 0.15).
narrative_ontology:measurement(lkn_exp_plur_su_t2025, legitimate_knowledge_boundary__experiential_pluralism_reading, suppression_requirement, 2025, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__experiential_pluralism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__experiential_pluralism_reading, 0.08).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__credentialed_expertise_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading).

% DUAL FORMULATION NOTE:
% This reading and its siblings decompose the natural-language label 'legitimate knowledge boundary' into three structurally distinct constraints with different ε values (experiential_plurism: 0.28, credentialed_expertise: ~0.35, hybrid_coproduction: ~0.42), different coordination types (identity_coordination, enforcement_mechanism, resource_allocation), and different beneficiary/victim structures. They are linked as a constraint family via network.affects_constraints. The ε-invariance principle requires this decomposition: the label 'legitimate knowledge' conflates distinct claims with different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_knowledge_boundary__experiential_pluralism_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
