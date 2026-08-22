% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus_universal_heritage_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__universal_heritage_reading
 *   human_readable: Universal Heritage Reading of Cultural Property Law
 *   domain: international_law/cultural_property/post_colonial
 *
 * SUMMARY:
 *   The universal heritage reading of cultural property law presents the
 *   global museum system as a coordination mechanism preserving humanity's
 *   shared culture. Its authored metrics reveal a structure that actively
 *   enforces retention of colonial acquisitions while extracting diplomatic,
 *   legal, and existential costs from claimant states and communities. The
 *   constraint is claimed as a tangled_rope — it genuinely coordinates
 *   conservation and circulation (beneficiaries exist) but simultaneously
 *   extracts asymmetrically from source nations and indigenous communities
 *   (victims exist) through legal asymmetry, professional gatekeeping, and
 *   ontological dismissal. The engine computes per-seat types from this
 *   structural data; the divergence between the agenda-setter seat
 *   (coordination as lived experience) and payer seats (extraction as lived
 *   experience) is the measurement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, 0.72).
domain_priors:suppression_score(cultural_property_legal_corpus__universal_heritage_reading, 0.68).
domain_priors:theater_ratio(cultural_property_legal_corpus__universal_heritage_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__universal_heritage_reading, "Universal Heritage Reading of Cultural Property Law").
narrative_ontology:topic_domain(cultural_property_legal_corpus__universal_heritage_reading, "international_law/cultural_property/post_colonial").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__universal_heritage_reading, '569b4e1b-aaf7-47b7-8e9b-eeb4e845c261').
narrative_ontology:cs_kernel_codification('569b4e1b-aaf7-47b7-8e9b-eeb4e845c261', formalized).
narrative_ontology:cs_authority_grounding('569b4e1b-aaf7-47b7-8e9b-eeb4e845c261', extraction).
narrative_ontology:cs_interpretation_layer_present('569b4e1b-aaf7-47b7-8e9b-eeb4e845c261').
narrative_ontology:cs_reading_relation('569b4e1b-aaf7-47b7-8e9b-eeb4e845c261', cultural_property_legal_corpus__indigenous_stewardship_reading, forecloses).
narrative_ontology:cs_reading_relation('569b4e1b-aaf7-47b7-8e9b-eeb4e845c261', cultural_property_legal_corpus__sovereign_repatriation_reading, influences).
narrative_ontology:cs_axiom('569b4e1b-aaf7-47b7-8e9b-eeb4e845c261', foundational, universal_access_primacy_over_particularist_claims).
narrative_ontology:cs_axiom_status(universal_access_primacy_over_particularist_claims, holdable).
narrative_ontology:cs_axiom_grounding('569b4e1b-aaf7-47b7-8e9b-eeb4e845c261', universal_access_primacy_over_particularist_claims, empirically_contingent).
narrative_ontology:cs_axiom('569b4e1b-aaf7-47b7-8e9b-eeb4e845c261', secondary, scientific_conservation_as_neutral_stewardship).
narrative_ontology:cs_axiom_status(scientific_conservation_as_neutral_stewardship, holdable).
narrative_ontology:cs_axiom_grounding('569b4e1b-aaf7-47b7-8e9b-eeb4e845c261', scientific_conservation_as_neutral_stewardship, conventional).
narrative_ontology:cs_reference_frame('569b4e1b-aaf7-47b7-8e9b-eeb4e845c261', postwar_anti_nationalist_cultural_protection).
narrative_ontology:cs_drift_state('569b4e1b-aaf7-47b7-8e9b-eeb4e845c261', post_decolonization_repurposing, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('569b4e1b-aaf7-47b7-8e9b-eeb4e845c261', '2026-08-07T14:30:00Z').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, holding_institutions).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, international_museum_network).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, conservation_professionals).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, source_nation_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, indigenous_communities).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, diaspora_communities).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, universal_cultural_heritage_doctrine).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, encyclopedic_museum_model).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, scientific_conservation_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major encyclopedic museums (British Museum, Louvre, Met, Berlin State Museums) hold contested collections acquired during colonial periods. They set accession, display, and loan policies; control provenance research agendas; and dominate the International Council of Museums (ICOM) governance. They frame retention as stewardship for humanity while benefiting from visitor revenue, philanthropic funding tied to collection breadth, and scholarly prestige. Exit from the constraint would mean restructuring their core institutional identity and revenue model.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, holding_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% The professional network of curators, registrars, conservators, and directors whose careers, conferences, publications, and loan circuits depend on the free circulation of objects within the holding-institution ecosystem. They benefit from the universal heritage framing as it legitimizes their professional authority and cross-institutional mobility. Exit means leaving the profession or accepting marginalization.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, international_museum_network, beneficiary,
    organized, biographical, constrained, global).

% Specialists whose technical authority is invoked to justify retention (objects are too fragile to move, require specialized climate control, need Western conservation science). They benefit from the constraint by controlling access to objects and defining preservation standards. Some genuinely prioritize object survival; others have career interests aligned with institutional retention. Exit is professionally feasible but carries reputational cost.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, conservation_professionals, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, conservation_professionals, observer).

% Post-colonial states (Nigeria, Greece, Egypt, Peru, China, India, etc.) pursuing repatriation through diplomatic, legal, and moral channels. They bear legal costs (litigation in foreign jurisdictions), diplomatic friction (strained bilateral relations), and identity harm (cultural dislocation, severed transmission chains). Their exit options are constrained by the asymmetry of international law: they must prove illegitimacy of acquisition under standards set by the holding institutions' legal frameworks.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, source_nation_states, payer,
    powerful, generational, constrained, national).

% Communities (Māori, Native American nations, Aboriginal Australian groups, First Nations, etc.) for whom specific objects are ancestors, ceremonial necessities, or living entities — not 'artifacts.' They bear spiritual harm, epistemic violence (their ontologies dismissed as 'belief'), and exclusion from decision-making. Identity-locked because the objects constitute their cultural continuity; exit from the relationship is culturally impossible. The universal heritage reading treats their claims as particularist obstacles to the universal good.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, indigenous_communities, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, indigenous_communities, excluded).

% Communities displaced from source regions (African diaspora, Palestinian diaspora, Armenian diaspora, etc.) who experience objects in holding institutions as both accessible heritage and painful evidence of displacement. They pay through the psychological cost of access on the holder's terms, and through exclusion from stewardship. Exit is constrained by geographic and legal distance from both source and holding locations.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, diaspora_communities, payer,
    moderate, biographical, constrained, regional).

% Scholars of cultural property law, post-colonial legal theory, and museum ethics who analyze the regime's structural dynamics. They neither collect nor pay directly but their work shapes the interpretive framework. Some advocate for the universal heritage model; others document its extractive operation. Their analytical seat is the engine's reference for 'what the structure computes.'
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the global circulation, preservation, and scholarly study of cultural objects through a shared professional infrastructure (conservation standards, loan networks, provenance databases, exhibition practices) that would be fragmented if each nation asserted exclusive control over its heritage.
% TRANSFER_FUNCTION: Moves cultural objects, interpretive authority, tourism revenue, scholarly prestige, and philanthropic capital from source communities and nations to holding institutions and the international museum network, justified as stewardship for humanity but operating as a rent extracted from the colonial provenance of collections.
% ABSENT_VOICES: Indigenous communities are structurally excluded from the ICOM governance and UNESCO committee structures that codify the universal heritage doctrine; their ontologies of objecthood (objects as ancestors, living beings) are translated into 'cultural significance' — a category the framework can absorb without ceding authority. Pre-colonial provenance holders (pre-state polities, displaced peoples) have no legal personality in the international system.
% DISAPPEARANCE_RATIONALE: If the universal heritage reading vanished overnight, holding institutions would lose their primary legitimizing doctrine; repatriation claims would accelerate under sovereign and indigenous readings; the global loan network would contract; conservation funding tied to 'universal access' would be redirected; the legal architecture of cultural property (1970 UNESCO Convention, 1995 UNIDROIT Convention) would be reinterpreted toward restitution. The world of objects would rearrange toward source communities.
% FOUNDING_PROBLEM: Post-WWII, the universal heritage doctrine was built to prevent nationalist destruction of cultural property (Nazi looting, ideological iconoclasm) by establishing that culture transcends borders and belongs to all humanity — a protective frame against state instrumentalization of heritage.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (protecting culture from state destruction) is attested by the 1954 Hague Convention preparatory works and UNESCO's own institutional history. However, source nations and indigenous communities attest that the doctrine was repurposed post-decolonization to protect colonial holdings from restitution — a shift documented in the 1970 UNESCO Convention negotiations where European states resisted retroactivity clauses. Independent legal historians (e.g., Meskell, Merryman, Toman) corroborate the dual genealogy.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__universal_heritage_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(cultural_property_legal_corpus__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__universal_heritage_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint's operation transfers value (objects, authority, revenue, epistemic control) from claimants to holders under a legitimacy framework the holders authored. Suppression (0.68) is substantial because the regime deploys legal technicalities (statutes of limitations, sovereign immunity, burden of proof on claimants), professional standards (conservation requirements only holders can meet), and ontological translation (indigenous claims reduced to 'cultural significance') to suppress alternatives. Theater (0.45) is significant: the conservation/circulation function is real but increasingly performative relative to the retention function. Accessibility collapse (0.35) is moderate — alternatives (repatriation, shared stewardship, digital return) exist but are structurally impeded. Resistance (0.62) is high and rising — claimant states, indigenous communities, and critical scholars actively contest the regime.
 *
 * PERSPECTIVAL GAP:
 *   From the holding institution seat, the constraint is a functioning coordination infrastructure they built and maintain — the conservation labs, loan networks, and scholarly access are real and costly. From the indigenous community seat, the same infrastructure is a machine that translates their ancestors into 'specimens,' their ceremonies into 'cultural practices,' and their sovereignty into 'stakeholder consultation.' The engine computes this divergence from the declared power/exit/role structure; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Holding institutions are structural beneficiaries (d ≈ 0.15): they collect revenue, prestige, and authority; their exit is arbitrage-grade (they could restructure but choose not to). International museum network professionals are beneficiaries (d ≈ 0.25): coordinated careers, constrained exit. Conservation professionals are near-symmetric (d ≈ 0.45): genuine expertise, mobile exit. Source nations are targets (d ≈ 0.85): bear legal/diplomatic costs, constrained exit (asymmetric law). Indigenous communities are identity-locked targets (d ≈ 0.95): spiritual/existential harm, culturally impossible exit. Diaspora communities are constrained targets (d ≈ 0.75): psychological costs, geographic/legal distance. Observers are analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting culture from state destruction) is live in new forms (ISIS iconoclasm, Yemen war damage, Ukraine museum targeting) but the universal heritage reading has been repurposed to defend colonial holdings against the very communities the doctrine's anti-nationalist logic should protect. The constraint exhibits mandatrophy: its mandate (universal protection) has been captured by its administrators (holding institutions) to serve their institutional perpetuity. The theater ratio rise (0.15→0.45) tracks this capture — more performance of stewardship, less function for claimants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_access_empirical_claim,
    'Do encyclopedic museums in Western capitals actually provide greater global access to cultural objects than source-community stewardship with digital sharing and rotating loans would?',
    'Comparative access metrics: visitor demographics, digital reach, scholarly access rates, community access for source populations under different governance models.',
    'If source-community models provide equal or greater access (especially for source communities), the universal_access_primacy axiom is empirically falsified and the reading''s coordination claim collapses — reclassification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_access_empirical_claim, empirical, 'Whether the universal access justification is empirically grounded or a cover story.').

omega_variable(
    ontological_translation_as_suppression,
    'Is the translation of indigenous ontologies (objects as ancestors/living beings) into the universal heritage category of ''cultural significance'' a structural suppression mechanism or a necessary bridging concept for cross-cultural law?',
    'Analyze whether the translation preserves or erases the claimant community''s decision-making authority over the object''s fate. Track legal outcomes: when indigenous ontology is ''translated,'' who decides the object''s disposition?',
    'If translation systematically removes decision authority from communities, it is suppression — the constraint''s effective suppression is higher than the structural measure suggests. If translation enables co-governance, it is coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ontological_translation_as_suppression, conceptual, 'Whether ontological translation is a coordination bridge or an epistemic extraction mechanism.').

omega_variable(
    reading_foreclosure_structure,
    'Does the universal heritage reading''s core premise (humanity''s shared heritage transcends particularist claims) logically foreclose the indigenous stewardship reading''s core premise (objects are specific communities'' living relatives), or do they coexist as competing frameworks in a plural legal order?',
    'Examine whether any legal framework has successfully implemented both readings simultaneously for the same object. If mutual exclusivity is structural, foreclosure is the correct relation; if parallel governance exists (e.g., co-management agreements), coexistence is correct.',
    'Foreclosure means the readings cannot be reconciled within one commitment system — the kernel is constitutively fractured. Coexistence means the kernel supports plural legitimate readings, changing the mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Structural relationship between universal_heritage_reading and indigenous_stewardship_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__universal_heritage_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cultural_property_uh_tr_t1945, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(cultural_property_uh_tr_t1970, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 1970, 0.28).
narrative_ontology:measurement(cultural_property_uh_tr_t1990, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 1990, 0.36).
narrative_ontology:measurement(cultural_property_uh_tr_t2007, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 2007, 0.41).
narrative_ontology:measurement(cultural_property_uh_tr_t2018, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 2018, 0.43).
narrative_ontology:measurement(cultural_property_uh_tr_t2025, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(cultural_property_uh_be_t1945, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(cultural_property_uh_be_t1970, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 1970, 0.52).
narrative_ontology:measurement(cultural_property_uh_be_t1990, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 1990, 0.61).
narrative_ontology:measurement(cultural_property_uh_be_t2007, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 2007, 0.68).
narrative_ontology:measurement(cultural_property_uh_be_t2018, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 2018, 0.7).
narrative_ontology:measurement(cultural_property_uh_be_t2025, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(cultural_property_uh_su_t1945, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 1945, 0.25).
narrative_ontology:measurement(cultural_property_uh_su_t1970, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(cultural_property_uh_su_t1990, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(cultural_property_uh_su_t2007, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 2007, 0.62).
narrative_ontology:measurement(cultural_property_uh_su_t2018, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 2018, 0.66).
narrative_ontology:measurement(cultural_property_uh_su_t2025, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__universal_heritage_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__universal_heritage_reading, 0.08).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% The cultural_property_legal_corpus kernel decomposes into three constraint stories: universal_heritage_reading (this file, tangled_rope, ε=0.72), sovereign_repatriation_reading (tangled_rope or rope, ε≈0.45 — coordinates state-level restitution but extracts from indigenous communities excluded from state representation), and indigenous_stewardship_reading (rope or mountain, ε≈0.15 — coordinates community continuity with minimal extraction). The universal heritage reading's high extraction from claimants structurally enables the other readings' claims by making the regime's asymmetry visible. Network edges: this reading influences both siblings by setting the legal-burden baseline they must overcome.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cultural_property_legal_corpus__universal_heritage_reading, powerful, 0.85).
constraint_indexing:directionality_override(cultural_property_legal_corpus__universal_heritage_reading, powerless, 0.95).
constraint_indexing:directionality_override(cultural_property_legal_corpus__universal_heritage_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
