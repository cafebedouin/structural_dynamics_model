% ============================================================================
% CONSTRAINT STORY: preparedness_retention__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: preparedness_retention__hybrid_reading
 *   human_readable: Stratified Preparedness: Technical Competence vs. Ceremonial Memory
 *   domain: governance/disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   The Netherlands' preparedness system is built on centralized technical
 *   expertise housed in Rijkswaterstaat and regional water boards. This
 *   hybrid reading claims the system maintains genuine competence in its
 *   specialized core (engineers, hydrologists, infrastructure managers) while
 *   peripheral actors—municipalities, communities, and local
 *   knowledge-keepers—participate in preparedness through drills and
 *   protocols designed centrally. The hybrid reading accepts that both tracks
 *   are necessary: technical competence requires continuous specialized
 *   training and authority. But it claims that the arrangement extracts
 *   authority from distributed communities in exchange for coordination
 *   services, and that the result is brittle: if specialized staff are lost
 *   (retirement, attrition, skill transfer failure), the system has no
 *   fallback because local preparedness knowledge has been ceremonially
 *   performed out of existence. The system's beneficiaries are the central
 *   institutions and their embedded specialists; the victims are distributed
 *   populations whose competence and authority have atrophied through non-use
 *   and non-validation.
 *
 * KEY AGENTS:
 *   - Rijkswaterstaat hierarchy: central authority setting technical standards and controlling knowledge definitions
 *   - Water board bureaucracy: regional administrators operationalizing central protocols
 *   - Specialized technical staff: career hydrologists and engineers maintaining live competence
 *   - Municipal civil protection: local responders executing central plans
 *   - Local populations: subjects of drills, excluded from design authority
 *   - Collective memory keepers: residents with intergenerational flood knowledge, formally excluded
 *   - Policy observers: parliamentary and audit institutions with observation but not intervention authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__hybrid_reading, 0.68).
domain_priors:suppression_score(preparedness_retention__hybrid_reading, 0.72).
domain_priors:theater_ratio(preparedness_retention__hybrid_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(preparedness_retention__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_retention__hybrid_reading, "Stratified Preparedness: Technical Competence vs. Ceremonial Memory").
narrative_ontology:topic_domain(preparedness_retention__hybrid_reading, "governance/disaster_preparedness/institutional_memory").

domain_priors:requires_active_enforcement(preparedness_retention__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__hybrid_reading, '758f0fe7-1335-4bd7-8f84-c6903591a358').
narrative_ontology:cs_kernel_codification('758f0fe7-1335-4bd7-8f84-c6903591a358', distributed).
narrative_ontology:cs_authority_grounding('758f0fe7-1335-4bd7-8f84-c6903591a358', extraction).
narrative_ontology:cs_interpretation_layer_present('758f0fe7-1335-4bd7-8f84-c6903591a358').
narrative_ontology:cs_reading_relation('758f0fe7-1335-4bd7-8f84-c6903591a358', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('758f0fe7-1335-4bd7-8f84-c6903591a358', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_axiom('758f0fe7-1335-4bd7-8f84-c6903591a358', foundational, preparedness_stratification_necessary).
narrative_ontology:cs_axiom_status(preparedness_stratification_necessary, holdable).
narrative_ontology:cs_axiom_grounding('758f0fe7-1335-4bd7-8f84-c6903591a358', preparedness_stratification_necessary, instrumental).
narrative_ontology:cs_axiom('758f0fe7-1335-4bd7-8f84-c6903591a358', secondary, expertise_centralization_legitimacy).
narrative_ontology:cs_axiom_status(expertise_centralization_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('758f0fe7-1335-4bd7-8f84-c6903591a358', expertise_centralization_legitimacy, conventional).
narrative_ontology:cs_reference_frame('758f0fe7-1335-4bd7-8f84-c6903591a358', centralized_expert_authority).
narrative_ontology:cs_drift_state('758f0fe7-1335-4bd7-8f84-c6903591a358', climate_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('758f0fe7-1335-4bd7-8f84-c6903591a358', '').
narrative_ontology:cs_kernel_id(preparedness_retention__hybrid_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, rijkswaterstaat_hierarchy).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, water_board_bureaucracy).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, distributed_communities).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, non_specialist_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, specialized_technical_staff).
narrative_ontology:constraint_beneficiary(preparedness_retention__hybrid_reading, municipal_civil_protection).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, municipal_civil_protection).
narrative_ontology:constraint_victim(preparedness_retention__hybrid_reading, local_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Central water management authority that sets preparedness standards, defines what counts as competence, and controls specialized training pathways. Retains technical knowledge through continuous cycles of inspection, simulation, and staff rotation. Benefits from the arrangement by consolidating authority over crisis response and maintaining budgetary capture justified by expertise claims.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, rijkswaterstaat_hierarchy, agenda_setter,
    institutional, generational, arbitrage, national).

% Regional water boards operationalize Rijkswaterstaat protocols through local personnel. They conduct annual drills, maintain infrastructure inspection schedules, and train a rotation of technicians. They benefit from the system's legitimacy and operational coherence while remaining subordinate to the central authority structure.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, water_board_bureaucracy, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, water_board_bureaucracy, beneficiary).

% Career hydrologists, engineers, and flood-response specialists embedded in Rijkswaterstaat and water boards who exercise actual technical knowledge. They maintain competence through practice and credential-renewal systems. They benefit from job security, professional status, and insider authority over crisis decisions, but their expertise becomes inseparable from the institution—exit means loss of technical identity.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, specialized_technical_staff, beneficiary,
    moderate, biographical, identity_locked, regional).

% Municipal crisis managers and emergency coordinators required to participate in drills designed by and evaluated by the central authority. They execute preparedness rituals (community evacuation drills, shelter protocols) that follow Rijkswaterstaat templates but lack authority to modify them or contest their relevance. They benefit from having a defined role in crisis response but pay the cost of deference to central expertise.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, municipal_civil_protection, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__hybrid_reading, municipal_civil_protection, beneficiary).

% Communities subject to mandatory evacuations, drills, and preparedness protocols set by distant authorities. They participate in ceremonial drills that often lack coherence with local geography, social structure, or communication networks. Their knowledge of local flood history, ground conditions, and social capacity is treated as non-technical and subordinated to the centralized expertise model. They cannot exit the jurisdiction or opt out of drills, and their local preparedness knowledge atrophies because it is not valued by the formal system.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, local_populations, payer,
    powerless, biographical, trapped, local).

% Older residents, local historians, and community elders who hold intergenerational knowledge about past flood events, successful adaptation strategies, and social networks that functioned during crises. This knowledge is formally excluded from preparedness planning as anecdotal or unscientific. They are invited to attend drills but not consulted on their design, creating a lived experience of their knowledge being ceremonially performed but practically erased.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, collective_memory_keepers, excluded,
    powerless, biographical, trapped, local).

% Academic and private-sector water scientists whose research or experience might inform preparedness could contribute alternative approaches but are structurally excluded from the planning process because Rijkswaterstaat monopolizes the definition of legitimate technical knowledge. They see drills and protocols that contradict contemporary hydrology but cannot intervene without challenging the central authority structure.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, independent_hydrologists, excluded,
    moderate, biographical, mobile, national).

% Parliamentary oversight committees, audit agencies, and independent research institutions tasked with assessing whether preparedness systems function. They observe the dual track—central competence vs. peripheral ceremony—but lack authority to mandate restructuring, and the system's complexity shields it from easy accountability.
narrative_ontology:constraint_stakeholder(preparedness_retention__hybrid_reading, policy_observers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__hybrid_reading, rijkswaterstaat_hierarchy).
narrative_ontology:fixing_cost_class(preparedness_retention__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes hydrological expertise and crisis-response decision-making to achieve coordinated, scientifically-grounded flood management across a vulnerable delta nation, replacing localized, uncoordinated responses with standardized protocols.
% TRANSFER_FUNCTION: Transfers legitimate authority over preparedness from distributed communities and local knowledge-keepers to centralized technical institutions; transfers operational control from municipalities and local responders to Rijkswaterstaat-designed protocols; transfers career opportunities and social status to credentialed technicians while local knowledge-holders are demoted to audience.
% ABSENT_VOICES: Collective memory keepers and older residents whose intergenerational flood knowledge is excluded from formal preparedness; independent hydrologists and academics whose research might challenge Rijkswaterstaat's technical monopoly; populations in flood-prone areas who experience drills as ceremonial but are not consulted on their design.
% DISAPPEARANCE_RATIONALE: If the hybrid system disappeared, the Netherlands would not revert to pure localism: specialized technical capacity is genuinely necessary for delta management. But the shift would depend on whether distributed local knowledge could re-integrate into professional preparedness. Proponents of the hybrid reading argue the current system would be irreplaceable; critics argue preparedness would reorganize around mixed teams of specialists and local knowledge-holders, maintaining technical capacity while recovering resilience.
% FOUNDING_PROBLEM: Post-1953 Delta Works: after the Great Flood, the Netherlands needed centralized, scientifically-managed water systems to replace ad-hoc local responses that had failed catastrophically. Specialized expertise in hydrology, engineering, and systems management had to be concentrated and coordinated nationally.
% FOUNDING_PROBLEM_CORROBORATION: Rijkswaterstaat and water boards attest the founding problem is live: climate change, sea-level rise, and complex delta hydrology require continuous expert management. Independent hydrologists and preparedness researchers attest that while the technical problem is live, the current solution has created new vulnerabilities (centralized single point of failure, atrophied local knowledge, brittle social networks). Communities experiencing drills as theater (not corroborated by the central authority) attest that their actual preparedness has declined even as ceremonial participation has increased.
narrative_ontology:disappearance_verdict(preparedness_retention__hybrid_reading, contested).
narrative_ontology:founding_problem_status(preparedness_retention__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_retention__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__hybrid_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68) because the system transfers authority over preparedness decisions from distributed communities and local institutions to centralized experts, and the measurement is stable across the interval—the arrangement is persistent, not improving. Theater is substantial and rising (0.38→0.58): drills and public participation rituals increase in frequency while decision authority remains centralized, indicating that public-facing performance is growing faster than functional integration. Suppression is high and stable (0.72): the system actively excludes alternative knowledge (independent hydrologists, local memory-keepers) and controls which voices count as expert, requiring active maintenance of the monopoly. Accessibility collapse is moderate (0.62): distributed populations cannot easily exit the jurisdiction or opt out of preparedness, but they can and do bypass formal systems (informal community networks, unregistered shelters, family-based strategies) when they do not trust the centralized protocols. Resistance is moderate-high (0.55): municipalities and some academic institutions push back against centralized authority, communities ignore drills they find irrelevant, but resistance is fragmented and lacks institutional power.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces radically different experiences from different seats. From Rijkswaterstaat's perspective, the system IS genuine competence-preserving coordination—they invest in training, conduct rigorous simulations, and their protocols reflect current hydrological science. From the perspective of a specialized engineer embedded in Rijkswaterstaat, the arrangement is professional identity and secure expertise. From a municipal civil protection coordinator's position, it is constrained deference: they execute protocols they did not design, cannot modify, and sometimes find locally incoherent. From a local population's perspective, it is ceremony: they participate in drills designed elsewhere, their local knowledge is ignored, and the drills do not teach them how to actually prepare in their community's terms. The engine computes per-seat classifications from these structural positions; the hybrid reading claims all these perspectives are TRUE simultaneously—the system IS technically competent at its core AND ceremonial at its periphery.
 *
 * DIRECTIONALITY LOGIC:
 *   Rijkswaterstaat (institutional beneficiary, d near 0.1): benefits from centralized authority capture, budgetary justification through expertise claims, and control over crisis response. Specialized technical staff (moderate beneficiary, d near 0.3): benefits from career security, professional status, and insider authority, but their exit is costly because their expertise is institution-specific. Water boards (organized payer/beneficiary mix, d near 0.4): execute protocols but lack authority to modify them; they benefit from institutional legitimacy but pay through constraint. Municipal civil protection (moderate payer, d near 0.65): constrained to execute central protocols, cannot contest them, but retain some operational adaptation authority. Local populations (powerless payers, d near 0.85): excluded from design, participate in ceremonies, cannot opt out, cannot exit. The directionality overrides are minimal here because the beneficiary/victim structure is clear from the structural positions: beneficiaries have institutional power and authority over rule definition; victims are constrained and excluded. The only override consideration is whether specialized technicians should be classified nearer the beneficiary end despite their identity lock—they genuinely benefit from the system even as they reproduce it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-1953 Delta Works: need for centralized, scientifically-managed systems after catastrophic local failure) is formally live according to Rijkswaterstaat and water boards—they argue climate change and complex hydrology demand continuous expert management. But independent hydrologists and preparedness researchers attest the founding problem has SHIFTED: the contemporary problem is not 'how to coordinate scientific response' but 'how to integrate expert systems with local knowledge and community capacity.' If this reframing is accurate, the current arrangement persists not because it solves a live founding problem but because it solves an institutional problem (how to maintain Rijkswaterstaat's centrality). This is the mandatrophy signature: the founding mandate has outlived its functional necessity, but the institution persists through legitimacy claims and by ceremonially involving populations who have learned not to trust it. The constraint bridges this gap by converting real coordination (technical expertise is necessary) into institutional extraction (expertise claims justify centralized authority). This is precisely the tangled-rope structure: genuine coordination function (yes, you need hydrological expertise) and genuine extraction (yes, distributed authority is transferred to centralized institutions), both operating through the same constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_ceremony_boundary,
    'This reading claims a dual track: specialized staff maintain genuine technical competence while peripheral actors perform ceremony. But where is the boundary? Do municipal civil protection officials exercise real competence or ceremony? Do local populations have latent competence that atrophies through exclusion, or do they lack technical capacity altogether?',
    'Post-crisis analysis: examine actual decision-making during floods (e.g., 2021 European floods in Dutch communities). Did Rijkswaterstaat protocols fail? Did excluded local knowledge prove necessary? Did municipal coordinators improvise beyond drills? Did communities'' ceremonial participation turn into coordinated response or breakdown?',
    'If the boundary is permeable and local actors exercise real competence, the constraint is less extractive than measured—it is tangled-rope-as-claimed, with genuine coordination riding on unequal authority. If the boundary is hard and ceremony is all that remains outside the center, the constraint approaches snare: central extraction of authority disguised as necessary specialization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_ceremony_boundary, empirical, 'Whether the competence/ceremony divide is a structural necessity or an enforced hierarchy.').

omega_variable(
    hydrological_necessity_vs_institutional_capture,
    'Does the centralization of preparedness in Rijkswaterstaat reflect genuine hydrological and technical necessity (the competence reading), or does it reflect institutional path dependence and bureaucratic self-preservation (the husk reading)? The hybrid reading accepts both are true, but in what proportion?',
    'Comparative institutional analysis: examine preparedness systems in other delta nations (Belgium, Germany, Denmark) that face similar hydrology. Do they achieve comparable safety outcomes with decentralized or mixed-authority models? Does Rijkswaterstaat''s monopoly outperform alternatives on technical metrics, or does it outperform on budget capture and institutional stability?',
    'If decentralized or mixed systems achieve comparable safety, the constraint''s centralization is revealed as institutional preference rather than technical necessity—shifting classification toward snare (pure extraction under expertise cover). If centralization measurably improves safety outcomes, the extraction is justified as the price of coordination—confirming tangled-rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hydrological_necessity_vs_institutional_capture, empirical, 'Whether centralized preparedness is technically necessary or institutionally preferred.').

omega_variable(
    reading_boundary_ambiguity,
    'This constraint instantiates the hybrid reading of the preparedness_retention kernel. But the hybrid reading''s definition (''dual-track system: core technical staff maintain competence, peripheral actors perform ceremony'') depends on what counts as ceremony vs. competence. The competence reading and husk reading contest this exact boundary—is ceremony ceremonial because it lacks real knowledge, or is it ceremonial because it is not given real authority despite competent execution?',
    'Ethnographic and textual analysis: trace the intentionality in the design of drills, protocols, and exclusions. Are they designed to partition knowledge (hybrid/husk reading) or to concentrate authority (husk reading''s interpretation of hybrid)? Are local populations invited to participate so they are included or so they can be blamed for non-compliance?',
    'If the design intentionally partitions competence and ceremony, the hybrid reading''s dual-track account is accurate. If the design intentionally excludes competent outsiders from authority regardless of their knowledge, the reading collapses toward the husk reading (ceremony as cover for extraction). The boundary is not discovered in the world; it depends on the reading''s interpretation of institutional intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'The hybrid reading''s core premise depends on a reading-specific interpretation of ceremony vs. competence.').

omega_variable(
    sibling_reading_distinction,
    'What distinguishes the hybrid reading from its siblings—the competence reading and husk reading? The competence reading claims all drills and inspections maintain operational capacity (no ceremony, just competence-building). The husk reading claims all drills are ritual performance with atrophied competence. The hybrid reading claims BOTH are true at different levels. But if both are true, is the constraint one reading or three?',
    'Structural analysis of the kernel''s definition: the kernel is ''preparedness retention''—the claim that some arrangement persists and retains something. The three readings contest what is retained: competence (competence reading), ceremony (husk reading), or both-at-different-levels (hybrid reading). These are logically distinct. The hybrid reading''s axiom must claim that STRATIFICATION itself is the essential feature—that a system can be simultaneously competent and theatrical at different organizational levels. If stratification is not the essential feature, the reading collapses into either competence or husk.',
    'The hybrid reading is sustainable only if stratification as a system property is ontologically distinct from the competence-or-husk dyad. If stratification is just ''competence for insiders, ceremony for outsiders,'' it is a description of how competence-based systems fail, not a third reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_distinction, conceptual, 'The coherence of the hybrid reading as a distinct third reading depends on stratification being an ontological property, not a description of insider/outsider access to a single underlying reality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__hybrid_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(prep_tr_t5, preparedness_retention__hybrid_reading, theater_ratio, 5, 0.43).
narrative_ontology:measurement(prep_tr_t10, preparedness_retention__hybrid_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(prep_tr_t15, preparedness_retention__hybrid_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement(prep_tr_t25, preparedness_retention__hybrid_reading, theater_ratio, 25, 0.57).
narrative_ontology:measurement(prep_tr_t40, preparedness_retention__hybrid_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__hybrid_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(prep_be_t5, preparedness_retention__hybrid_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(prep_be_t10, preparedness_retention__hybrid_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(prep_be_t15, preparedness_retention__hybrid_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(prep_be_t25, preparedness_retention__hybrid_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(prep_be_t40, preparedness_retention__hybrid_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__hybrid_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement(prep_su_t5, preparedness_retention__hybrid_reading, suppression_requirement, 5, 0.66).
narrative_ontology:measurement(prep_su_t10, preparedness_retention__hybrid_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(prep_su_t15, preparedness_retention__hybrid_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(prep_su_t25, preparedness_retention__hybrid_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(prep_su_t40, preparedness_retention__hybrid_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__competence_reading).
narrative_ontology:affects_constraint(preparedness_retention__hybrid_reading, preparedness_retention__husk_reading).

% DUAL FORMULATION NOTE:
% The preparedness_retention kernel decomposes into three constraint stories distinguished by what they claim is retained through preparedness practices. competence_reading claims all drills and inspections are competence-preserving (low extraction, high accessibility collapse, emergent natural law of practice). husk_reading claims all drills are ritual performance with atrophied competence (high extraction, high theater_ratio, snare structure). hybrid_reading (this story) claims both are true at different organizational levels—a tangled rope where coordination function (technical expertise coordination) and extraction function (centralized authority capture) operate through the same institutional structure. The three readings share the kernel 'preparedness retention' and contest what is retained: competence, ceremony, or stratified both. They are linked via network.affects_constraints in all three files and distinguished by cs_structure axioms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_retention__hybrid_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
