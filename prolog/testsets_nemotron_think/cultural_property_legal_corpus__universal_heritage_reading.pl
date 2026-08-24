% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__universal_heritage_reading, []).

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
 *   human_readable: Universal Heritage Doctrine in Cultural Property Law
 *   domain: international_law/cultural_property/post_colonial
 *
 * SUMMARY:
 *   The universal heritage reading of the cultural property legal corpus
 *   presents itself as a coordination mechanism protecting humanity's shared
 *   past. Structurally, it operates as a tangled rope: it delivers genuine
 *   preservation and access coordination (UNESCO conventions, conservation
 *   standards, anti-looting norms) while simultaneously extracting from
 *   successor states and indigenous communities through asymmetrical legal
 *   burdens, standing rules that exclude non-state peoples, and a doctrinal
 *   presumption favoring current possessors. Holding institutions (major
 *   Western museums) are the primary beneficiaries — they collect prestige,
 *   revenue, and epistemic authority while externalizing the costs of
 *   contested provenance. The constraint requires active enforcement
 *   (litigation defense, diplomatic pressure, institutional policy) to
 *   maintain the possession presumption against mounting repatriation claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, 0.72).
domain_priors:suppression_score(cultural_property_legal_corpus__universal_heritage_reading, 0.65).
domain_priors:theater_ratio(cultural_property_legal_corpus__universal_heritage_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__universal_heritage_reading, "Universal Heritage Doctrine in Cultural Property Law").
narrative_ontology:topic_domain(cultural_property_legal_corpus__universal_heritage_reading, "international_law/cultural_property/post_colonial").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__universal_heritage_reading, '17c93f51-31f3-4b51-91ae-133b2c88c00b').
narrative_ontology:cs_kernel_codification('17c93f51-31f3-4b51-91ae-133b2c88c00b', formalized).
narrative_ontology:cs_authority_grounding('17c93f51-31f3-4b51-91ae-133b2c88c00b', extraction).
narrative_ontology:cs_interpretation_layer_present('17c93f51-31f3-4b51-91ae-133b2c88c00b').
narrative_ontology:cs_reading_relation('17c93f51-31f3-4b51-91ae-133b2c88c00b', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('17c93f51-31f3-4b51-91ae-133b2c88c00b', cultural_property_legal_corpus__indigenous_stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('17c93f51-31f3-4b51-91ae-133b2c88c00b', foundational, universal_heritage_primacy).
narrative_ontology:cs_axiom_status(universal_heritage_primacy, holdable).
narrative_ontology:cs_axiom_grounding('17c93f51-31f3-4b51-91ae-133b2c88c00b', universal_heritage_primacy, conventional).
narrative_ontology:cs_axiom('17c93f51-31f3-4b51-91ae-133b2c88c00b', secondary, institutional_competence_preservation).
narrative_ontology:cs_axiom_status(institutional_competence_preservation, holdable).
narrative_ontology:cs_axiom_grounding('17c93f51-31f3-4b51-91ae-133b2c88c00b', institutional_competence_preservation, empirically_contingent).
narrative_ontology:cs_reference_frame('17c93f51-31f3-4b51-91ae-133b2c88c00b', universal_museum_framework).
narrative_ontology:cs_drift_state('17c93f51-31f3-4b51-91ae-133b2c88c00b', post_colonial_repatriation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('17c93f51-31f3-4b51-91ae-133b2c88c00b', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, holding_institutions).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, universal_public).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, successor_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, indigenous_communities).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, cultural_heritage_belongs_to_humanity).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, institutional_preservation_maximizes_access).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major museums and cultural institutions (British Museum, Louvre, Met, etc.) hold contested artifacts acquired during colonial eras. They set acquisition, display, and repatriation policies; control conservation expertise and exhibition narratives; collect prestige, tourism revenue, and research access. Their exit from the constraint is near-arbitrage: they can resist claims indefinitely, leverage legal frameworks they helped shape, and treat repatriation as exceptional discretion rather than obligation.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, holding_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, holding_institutions, beneficiary).

% Post-colonial states (Nigeria, Greece, Egypt, Mexico, etc.) claiming artifacts as sovereign patrimony. They bear legal costs of litigation, diplomatic friction with holding states, and identity harm from seeing cultural patrimony displayed abroad. Their exit is constrained: they can pursue bilateral agreements, UNESCO mediation, or domestic litigation, but face asymmetrical burden of proof, statutes of limitation, and institutional inertia. Coalition power exists but is fragmented by geopolitical alignment.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, successor_states, payer,
    organized, biographical, constrained, national).

% First Nations, Aboriginal, Māori, Native American, and other indigenous groups for whom artifacts are sacred, communal, or genealogical. They bear cultural dislocation, spiritual harm, and epistemic erasure when artifacts are framed as 'universal heritage.' Their exit is identity-locked: the artifacts constitute their cultural continuity; leaving the relationship is not an option. They are structurally excluded from the legal corpus's standing rules, which recognize states not peoples.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, indigenous_communities, payer,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, indigenous_communities, excluded).

% The abstract 'humanity' invoked by the doctrine — in practice, museum-going publics, researchers, and digital audiences primarily in the Global North. They receive access to consolidated collections, educational programming, and digital surrogates. Their exit is mobile: they can visit other museums, access digital collections, or disengage without structural penalty. The benefit is real but diffuse and asymmetrically distributed.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, universal_public, beneficiary,
    organized, biographical, mobile, global).

% International lawyers, cultural property scholars, UNESCO officials, and courts who administer the legal corpus. They interpret treaties (1954 Hague, 1970 UNESCO, 1995 UNIDROIT), adjudicate restitution claims, and produce the doctrinal framework. Their seat is analytical: they neither collect rents nor bear extraction directly, but their interpretive authority shapes which claims succeed. They can exit analytically by shifting interpretive frameworks.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, legal_profession, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a transnational legal framework for protecting cultural artifacts from destruction, looting, and nationalist fragmentation; centralizes conservation expertise; enables physical and digital access across borders through standardized protocols.
% TRANSFER_FUNCTION: Moves physical custody, narrative authority, and decision-making control over artifacts from source communities and successor states to holding institutions; moves legal costs, diplomatic burden, and identity harm to claimant states and indigenous communities; moves prestige, tourism revenue, and epistemic authority to holding institutions.
% ABSENT_VOICES: Indigenous communities directly connected to artifacts (excluded by state-centric standing rules); source communities in the Global South lacking legal capacity; diaspora communities whose cultural continuity is severed; future generations in source regions who lose developmental cultural resources.
% DISAPPEARANCE_RATIONALE: If the universal heritage doctrine vanished overnight, the legal presumption favoring current possession would collapse. Repatriation claims would shift to national courts and bilateral negotiations where source states have stronger standing. Artifacts would disperse to origin countries and communities; new preservation models (community-led, digital repatriation, shared stewardship) would compete. The global museum ecosystem would lose its coordinating legal logic.
% FOUNDING_PROBLEM: Mid-20th century response to WWII cultural destruction and accelerating illicit trade: need for universal standards to protect heritage from war, looting, and nationalist fragmentation; creation of a transnational legal regime treating cultural property as a shared human concern rather than national spoils.
% FOUNDING_PROBLEM_CORROBORATION: UNESCO and ICOM attest the preservation/looting problem remains live (Syria, Iraq, Yemen, Ukraine). Successor states (Nigeria Benin Bronzes, Greece Parthenon Marbles, Egypt Rosetta Stone) and indigenous advocates (Native American NAGPRA, Māori taonga) attest the founding problem was never neutral — the regime was built on colonial possession and legitimates retention. Independent scholars (e.g., Savoy, Sarr, Prott) corroborate the colonial acquisition legacy undermines the regime's universalist claim.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__universal_heritage_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
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
 *   Extractiveness (0.72) is high because the legal framework systematically advantages possessors: burden of proof on claimants, statutes of limitation, non-retroactivity of treaties, and standing rules excluding indigenous communities. Suppression (0.65) reflects legal barriers (immunity from suit, act of state doctrine), narrative control (universal museum discourse), and resource asymmetry (claimants lack conservation infrastructure). Theater ratio (0.42) captures that preservation/access functions are real but increasingly serve as cover: digitization enables access without possession, yet physical retention persists. Accessibility collapse (0.58) shows alternatives (repatriation, shared stewardship, digital return) exist but are legally and institutionally suppressed. Resistance (0.71) is high: claimant states pursue litigation, diplomacy, and public pressure; indigenous communities assert cultural rights; even some institutions now practice restitution.
 *
 * PERSPECTIVAL GAP:
 *   From the holding institution seat, the constraint appears as rope: they built the conservation infrastructure, they maintain access, they solve the coordination problem of global preservation. From successor state and indigenous community seats, the same structure appears as snare: the coordination story is cover for retaining colonial plunder; the legal machinery exists to suppress their claims. The engine computes this divergence from the structural data — the declared beneficiaries and victims with their exit options generate the seat-specific effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Holding institutions are structural beneficiaries (d near 0.15): they collect rents, control rules, have arbitrage-grade exit. Successor states are targets (d near 0.85): they bear legal costs, face asymmetrical proof burdens, have constrained exit. Indigenous communities are identity-locked targets (d near 0.95): the constraint extracts cultural continuity itself; exit is existentially impossible. Universal public sits near symmetric (d ~0.5): genuine access benefit but diffuse and unequal. Legal profession is analytical (d=0.5 by definition). The derivation chain produces these from beneficiary/victim declarations plus exit options; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (wartime destruction, illicit trade) remains live but has mutated: the regime now primarily manages colonial-era acquisitions, not wartime looting. The mandate has partially atrophied — the preservation function is real but the universalist framing extracts from the very peoples whose heritage was expropriated to build the collections. This is not pure mandatrophy (the coordination function hasn't vanished) but mandatrophy-adjacent: the original justification has been stretched to cover a structurally extractive arrangement. The theater ratio rise tracks this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Does the universal heritage reading represent a genuine coordination function (preservation/access) with extractive overdraft, or is the coordination function itself a cover story for retention of colonial acquisitions?',
    'Counterfactual analysis: if all contested artifacts were repatriated tomorrow, would the legal corpus''s preservation/access coordination collapse, or would new coordination mechanisms (digital sharing, rotating loans, community-led conservation) emerge? Track post-repatriation outcomes (e.g., Benin Bronzes returns) for preservation and access metrics.',
    'If coordination survives repatriation, the reading is tangled_rope with separable functions; if coordination collapses, the reading is snare with coordination as cover. Determines whether reform (separating functions) or abolition (rejecting the reading) is the structural remedy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the reading''s coordination and extraction components are structurally separable or fused.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, resource asymmetry, standing rules) or internalized (source communities accept universal heritage framing as legitimate, epistemic capture)?',
    'Post-exit suppression trajectory: track claimant communities that achieve repatriation — does suppression persist as internalized devaluation of their own stewardship capacity? Compare communities with successful returns vs. those still litigating.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint''s harm continues after legal victory. Would require reclassification toward snare for identity-locked seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for indigenous and successor state seats.').

omega_variable(
    universal_public_coherence,
    'Is ''universal public'' a coherent beneficiary seat, or a rhetorical construct that masks the actual beneficiary (holding institutions and their primary audiences in the Global North)?',
    'Demographic analysis of actual museum visitors, digital collection users, and research beneficiaries vs. global population. Measure access asymmetry: what percentage of ''universal'' access accrues to populations in holding-institution countries?',
    'If universal public is incoherent, the beneficiary declaration is structurally false — the constraint has only holding_institutions as concentrated beneficiary, making it more snare-like. Would affect Boltzmann coupling analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_public_coherence, empirical, 'Whether the declared universal beneficiary is a real actor or a legitimating fiction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__universal_heritage_reading, 0, 74).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cplc_uhr_tr_t0, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cplc_uhr_tr_t15, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(cplc_uhr_tr_t30, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(cplc_uhr_tr_t45, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 45, 0.34).
narrative_ontology:measurement(cplc_uhr_tr_t60, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(cplc_uhr_tr_t74, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 74, 0.42).

% Extraction over time
narrative_ontology:measurement(cplc_uhr_be_t0, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cplc_uhr_be_t15, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(cplc_uhr_be_t30, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(cplc_uhr_be_t45, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 45, 0.65).
narrative_ontology:measurement(cplc_uhr_be_t60, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 60, 0.69).
narrative_ontology:measurement(cplc_uhr_be_t74, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 74, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(cplc_uhr_su_t0, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cplc_uhr_su_t15, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(cplc_uhr_su_t30, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(cplc_uhr_su_t45, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 45, 0.6).
narrative_ontology:measurement(cplc_uhr_su_t60, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 60, 0.63).
narrative_ontology:measurement(cplc_uhr_su_t74, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 74, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__universal_heritage_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__universal_heritage_reading, 0.12).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% This reading and its siblings form a constraint family decomposing the cultural_property_legal_corpus kernel. The universal_heritage_reading dominates the current legal framework (treaty interpretation, court precedents, museum policy), creating downstream pressure on sibling readings by setting the burden-of-proof baseline and standing rules. The sovereign_repatriation_reading pushes back through state practice (bilateral returns, national legislation). The indigenous_stewardship_reading pushes back through UNDRIP, NAGPRA, and community-led protocols. All three readings share the same kernel (the legal corpus) but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
